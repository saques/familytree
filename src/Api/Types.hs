{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Api.Types where

import           GHC.Generics
import           Control.Applicative
import           Data.Text
import           Data.Aeson
import           Snap.Snaplet.PostgresqlSimple

data User = User
    { id :: Int
    , name :: Text
    , password :: Text
    } deriving Generic

instance FromRow User where
    fromRow = User <$> field
                    <*> field
                    <*> field

instance ToJSON User where
    toJSON (User id name _) = object [ "id" .= id, "name" .= name ]

instance FromJSON User where
    parseJSON = genericParseJSON defaultOptions