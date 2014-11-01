{-# LANGUAGE CPP             #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

------------------------------------------------------------------------------
import           Control.Exception     (SomeException, try)
import qualified Data.ByteString.Char8 as BS
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
    (conf, site, cleanup) <- $(loadSnapTH [| getConf |]
                                          'getActions
                                          ["snaplets/heist/templates"])

    _ <- try $ httpServe conf site :: IO (Either SomeException ())
    cleanup

getConf :: IO (Config Snap AppConfig)
getConf = do
  port <- getEnv "PORT"
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
