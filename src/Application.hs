{-# LANGUAGE TemplateHaskell #-}

------------------------------------------------------------------------------
-- | This module defines our application's state type and an alias for its
-- handler monad.
module Application where

------------------------------------------------------------------------------
import Api.Core (Api(Api))
import Control.Lens
import Snap.Snaplet
import Snap.Snaplet.PostgresqlSimple
------------------------------------------------------------------------------
data App = App { 
                 _api :: Snaplet Api , 
                 _db   :: Snaplet Postgres          
               }

makeLenses ''App

------------------------------------------------------------------------------
type AppHandler = Handler App App
