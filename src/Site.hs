{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Control.Lens
import           Data.ByteString               (ByteString)
import           Data.Monoid                   (mempty)
import qualified Data.Text                     as T
import           Heist
import           Heist.Interpreted
import           Snap                          (lift, liftIO, void)
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.FileServe
import qualified Text.XmlHtml                  as X
------------------------------------------------------------------------------
import           Application


mkTable :: AppHandler ()
mkTable =
  do [n :: Int] <- head <$> query_ "select count(*) from pg_tables where tablename = 'count'"
     if n == 0
        then do void $ execute_ "CREATE TABLE count (val INTEGER NOT NULL)"
                void $ execute_ "INSERT INTO COUNT (val) VALUES (0)"
        else return ()

viewCountSplice :: Splice AppHandler
viewCountSplice =
  do lift mkTable
     (Only c) <- lift $ head <$> (query_ "update count set val = val + 1 returning val")
     return [X.TextNode (T.pack (show (c :: Int)))]


routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         ]


app :: SnapletInit App App
app = makeSnaplet "app" "A heroku example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    d <- nestSnaplet "db" db pgsInit
    addRoutes routes
    addConfig h (set scInterpretedSplices ("view-count" ## viewCountSplice) mempty)
    addTemplates h ""
    return $ App h d
