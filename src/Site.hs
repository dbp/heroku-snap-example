{-# LANGUAGE OverloadedStrings #-}

module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Control.Applicative
import           Data.ByteString     (ByteString)
import qualified Data.Text           as T
import           Heist
import qualified Heist.Interpreted   as I
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Heist
import           Snap.Util.FileServe
------------------------------------------------------------------------------
import           Application



routes :: [(ByteString, Handler App App ())]
routes = [ ("",          serveDirectory "static")
         ]


app :: SnapletInit App App
app = makeSnaplet "app" "A heroku example application." Nothing $ do
    h <- nestSnaplet "" heist $ heistInit "templates"
    addRoutes routes
    addTemplates h ""
    return $ App h
