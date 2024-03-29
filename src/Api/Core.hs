{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Core where

import           Api.Controllers.UserController
import           Api.Controllers.FamilyTreeController
import           Snap.Core
import           Control.Lens
import           Snap.Snaplet
import           Control.Applicative
import           Snap.Snaplet.PostgresqlSimple
import qualified Data.ByteString.Char8 as B

data Api = Api
    {
       _db   :: Snaplet Postgres ,
       _users :: Snaplet UserController,
       _familyTree :: Snaplet FamilyTreeController
    }

makeLenses ''Api

apiRoutes :: [(B.ByteString, Handler b Api ())]
apiRoutes = [("status", method GET respondOk)]

respondOk :: Handler b Api ()
respondOk = modifyResponse $ setResponseCode 200

apiInit :: Snaplet Postgres -> SnapletInit b Api
apiInit d = makeSnaplet "api" "Core Api" Nothing $ do
    u <- nestSnaplet "users" users $ userControllerInit d
    ft <- nestSnaplet "family-tree" familyTree $ familyTreeControllerInit d
    addRoutes apiRoutes
    return $ Api d u ft