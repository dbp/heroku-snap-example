{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Main where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Exception     (SomeException, try)
import qualified Data.Attoparsec.Text  as A
import qualified Data.ByteString.Char8 as BS
import           Data.Maybe
import qualified Data.Text             as T
import           Site
import           Snap.Core
import           Snap.Http.Server
import           Snap.Snaplet
import           Snap.Snaplet.Config
import           Snap.Util.Proxy
import           System.Environment
import           System.IO

#ifdef DEVELOPMENT
import           Snap.Loader.Dynamic
#else
import           Snap.Loader.Static
#endif

main :: IO ()
main = do
    mdb <- lookupEnv "DATABASE_URL"
    case mdb of
      Nothing -> return ()
      Just db -> do -- [database type]://[username]:[password]@[host]:[port]/[database name]
                    let r = A.parseOnly pgurlparser (T.pack db)
                    case r of
                      Left _ -> return ()
                      Right (u, p, h, t, n) -> do
                        setEnv "SNAPLET_POSTGRES_USER" (T.unpack u)
                        setEnv "SNAPLET_POSTGRES_PASSWORD" (T.unpack p)
                        setEnv "SNAPLET_POSTGRES_HOST" (T.unpack h)
                        setEnv "SNAPLET_POSTGRES_PORT" (T.unpack t)
                        setEnv "SNAPLET_POSTGRES_DBNAME" (T.unpack n)
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup
  where pgurlparser = do A.string "postgres://"
                         user <- A.takeWhile (/= ':')
                         A.char ':'
                         pass <- A.takeWhile (/= '@')
                         A.char '@'
                         host <- A.takeWhile (/= ':')
                         A.char ':'
                         port <- A.takeWhile (/= '/')
                         A.char '/'
                         name <- A.takeText
                         return (user, pass, host, port, name)

getConf :: IO (Config Snap AppConfig)
getConf = do
  port <- fromMaybe "8000" <$> lookupEnv "PORT"
  commandLineAppConfig $ setPort (read port)
                       . setAccessLog (ConfigIoLog BS.putStrLn)
                       . setErrorLog (ConfigIoLog BS.putStrLn)
                       . setProxyType X_Forwarded_For
                       $ emptyConfig

getActions :: Config Snap AppConfig -> IO (Snap (), IO ())
getActions conf = do
    (msgs, site, cleanup) <- runSnaplet
        (appEnvironment =<< getOther conf) app
    hPutStrLn stderr $ T.unpack msgs
    return (site, cleanup)
