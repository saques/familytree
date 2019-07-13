{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Api.Types where

import           GHC.Generics
import           Control.Applicative
import           Data.Text
import           Data.Aeson
import           Snap.Snaplet.PostgresqlSimple
import			 FamilyTree

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



data DBPerson = DBPerson
    { pId :: Int
    , familyTreeId :: Int
    , level :: Int
    , firstname :: Text
    , lastName :: Text
    , birthDate :: Text
    , hairColor :: Text
    , eyeColor :: Text
    , skinColor :: Text
    , deathDate :: Maybe Text
    , deathPlace :: Maybe Text
    , profession :: Maybe Text
    , deseases :: Maybe Text
   } deriving Generic 

instance FromRow DBPerson where
    fromRow = DBPerson <$> field
					   <*> field
					   <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field
                       <*> field

instance ToJSON DBPerson where
    toJSON (DBPerson id ftId level name lastname bd hc ec sc dp dd  pr dss) = object [ "id" .= id, "name" .= name ,"lastname".= lastname, "birthDate" .= bd,  "hairColor" .= hc,  "eyeColor" .= ec, "skinColor" .= sc]

instance FromJSON DBPerson where
    parseJSON = genericParseJSON defaultOptions

instance ToJSON FamilyTree where
    toJSON (Ft lvls) = object [ "lvls" .= lvls]

instance FromJSON FamilyTree where
    parseJSON = genericParseJSON defaultOptions  


instance ToJSON LevelWithPersons where
    toJSON (Lwp (lvl,ps)) = object [ "level" .= lvl , "persons" .= ps]

instance FromJSON LevelWithPersons where
    parseJSON = genericParseJSON defaultOptions   


instance ToJSON Person where
    toJSON (Ps id name lastname bd a hc ec sc d dp dd  pr dss) = object [ "id" .= id, "name" .= name ,"lastname".= lastname, "birthDate" .= bd, "age" .= a, "hairColor" .= hc,  "eyeColor" .= ec, "skinColor" .= sc]

instance FromJSON Person where
    parseJSON = genericParseJSON defaultOptions


instance ToJSON PersonWithParents where
    toJSON (PsP p parents) = object [ "person" .= p, "parents" .= parents ]

instance FromJSON PersonWithParents where
    parseJSON = genericParseJSON defaultOptions   



instance ToJSON HairColor where
    toJSON (a) = "Rubio"

instance FromJSON HairColor where
    parseJSON = genericParseJSON defaultOptions   

instance ToJSON EyeColor where
    toJSON (a) = "Rubio"

instance FromJSON EyeColor where
    parseJSON = genericParseJSON defaultOptions   

instance ToJSON SkinColor where
    toJSON (a) = "Rubio"

instance FromJSON SkinColor where
    parseJSON = genericParseJSON defaultOptions   


instance ToJSON Deseases where
    toJSON (a) = "Rubio"

instance FromJSON Deseases where
    parseJSON = genericParseJSON defaultOptions   

data FamilyTreeData = FamilyTreeData 
	{ ftid :: Int
	, ftname :: Text
	} deriving Generic

instance FromRow FamilyTreeData where
    fromRow = FamilyTreeData <$> field
					<*> field
    
instance ToJSON FamilyTreeData where
    toJSON (FamilyTreeData id name) = object [ "name" .= name, "id" .= id ]

instance FromJSON FamilyTreeData where
    parseJSON = genericParseJSON defaultOptions  

data ResponseId = ResponseId 
	{ rId :: Int
	} deriving Generic

instance FromRow ResponseId where
    fromRow = ResponseId <$> field
    
instance ToJSON ResponseId where
    toJSON (ResponseId id ) = object [ "id" .= id ]

instance FromJSON ResponseId where
    parseJSON = genericParseJSON defaultOptions  

data ParentRelation = ParentRelation 
	{ prid :: Int
	, descendantid :: Int
	, parentId :: Int
	} deriving Generic

instance FromRow ParentRelation where
    fromRow = ParentRelation <$> field
							<*> field
   							 <*> field
    
instance ToJSON ParentRelation where
    toJSON (ParentRelation id descendant parent) = object [ "descendant" .= descendant, "parent" .= parent ]

instance FromJSON ParentRelation where
    parseJSON = genericParseJSON defaultOptions



