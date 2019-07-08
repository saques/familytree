{-# LANGUAGE OverloadedStrings #-}

------------------------------------------------------------------------------
-- | This module is where all the routes and handlers are defined for your
-- site. The 'app' function is the initializer that combines everything
-- together and is exported by this module.
module Site
  ( app
  ) where

------------------------------------------------------------------------------
import           Api.Core (Api(Api), apiInit)
import           Control.Applicative
import           Data.ByteString (ByteString)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.PostgresqlSimple
import           Snap.Util.CORS
------------------------------------------------------------------------------
import           Application


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = []


------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "FamilyTree Rest Api" Nothing $ do
    d <- nestSnaplet "db" db pgsInit
    a <- nestSnaplet "api" api $ apiInit d
    addRoutes routes
    wrapSite $ applyCORS defaultOptions
    return $ App a d