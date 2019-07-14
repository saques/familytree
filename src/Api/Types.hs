{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module Api.Types where

import           GHC.Generics
import           Control.Applicative
import           Data.Text
import           Data.Aeson
import           Snap.Snaplet.PostgresqlSimple
import			     FamilyTree

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
    , age :: Int
   } deriving (Generic )

instance Eq DBPerson where
    (DBPerson id ftId l fn ln bd hc ec sc dd dp pr dss age) == (DBPerson id2 ftId2 l2 fn2 ln2 bd2 hc2 ec2 sc2 dd2 dp2 pr2 dss2 age2) =  (ftId == ftId2 && id == id2 && ftId == ftId2 && l == l2 && fn==fn2  && ln == ln2 && bd == bd2  && hc==hc2 && ec==ec2 && sc == sc2 && dd==dd2 && dp==dp2  && pr==pr2 && dss==dss2 && age==age2) || (fn == fn2 && ln == ln2 && bd == bd2) 
 

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
                       <*> field
instance ToJSON DBPerson where
    toJSON (DBPerson id ftId level name lastname bd hc ec sc dp dd  pr dss age) = object [ "id" .= id, "name" .= name ,"lastname".= lastname, "birthDate" .= bd,  "hairColor" .= hc,  "eyeColor" .= ec, "skinColor" .= sc , "age" .= age, "deathPlace" .= dp, "deathDate" .= dd , "profession" .= pr, "deseases" .= dss]

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
    toJSON (Ps id name lastname bd a hc ec sc d dp dd  pr dss) = object [ "id" .= id, "name" .= name ,"lastname".= lastname, "birthDate" .= bd, "age" .= a, "hairColor" .= hc,  "eyeColor" .= ec, "skinColor" .= sc , "deathPlace" .= dp, "deathDate" .= dd , "profession" .= pr ,"deseases" .= dss]

instance FromJSON Person where
    parseJSON = genericParseJSON defaultOptions


instance ToJSON PersonWithParents where
    toJSON (PsP p parents) = object [ "person" .= p, "parents" .= parents ]

instance FromJSON PersonWithParents where
    parseJSON = genericParseJSON defaultOptions   



instance ToJSON HairColor where
    toJSON (Blonde) = "Rubio"
    toJSON (BrownHair) = "Castaño"
    toJSON (BlackHair) = "Morocho"
    toJSON (Red) = "Colorado"
    toJSON (White) = "Blanco"
    toJSON (IncorrectHairColor) = ""

instance FromJSON HairColor where
    parseJSON = genericParseJSON defaultOptions   

instance ToJSON EyeColor where
    toJSON (Green) = "Verdes"
    toJSON (Blue) = "Azules"
    toJSON (Brown) = "Marrones"
    toJSON (Black) = "Negros"
    toJSON (IncorrectEyeColor) = ""

instance FromJSON EyeColor where
    parseJSON = genericParseJSON defaultOptions   

instance ToJSON SkinColor where
    toJSON (LightSkin) = "Clara"
    toJSON (DarkSkin) = "Oscura"
    toJSON ( IncorrectSkinColor ) = ""

instance FromJSON SkinColor where
    parseJSON = genericParseJSON defaultOptions   


instance ToJSON Deseases where
    toJSON (Cancer) = "Cancer"
    toJSON (Diabetes) = "Diabetes"
    toJSON (Leukemia) = "Leucemia"
    toJSON ( IncorrectDesease ) = ""

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


data FamilyTreeIdAndLevel = FamilyTreeIdAndLevel 
  { personFtId :: Int
    , personLvl :: Int
  } deriving Generic

instance FromRow FamilyTreeIdAndLevel where
    fromRow = FamilyTreeIdAndLevel <$> field
                                    <*> field
    
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


data FilteredPerson = FilteredPerson 
    { pIdFilter :: Int
    , familyTreeIdFilter :: Int
    , levelFilter :: Int
    , firstnameFilter :: Text
    , lastNameFilter :: Text
    , birthDateFilter :: Text
    , hairColorFilter :: Text
    , eyeColorFilter :: Text
    , skinColorFilter :: Text
    , deathDateFilter :: Maybe Text
    , deathPlaceFilter :: Maybe Text
    , professionFilter :: Maybe Text
    , deseasesFilter :: [Deseases]
    , ageFilter :: Int
   } deriving (Generic , Eq)

instance ToJSON FilteredPerson where
    toJSON (FilteredPerson id ftId level name lastname bd hc ec sc dp dd  pr dss age) = object [ "id" .= id, "name" .= name ,"lastname".= lastname, "birthDate" .= bd,  "hairColor" .= hc,  "eyeColor" .= ec, "skinColor" .= sc , "age" .= age, "deathPlace" .= dp, "deathDate" .= dd , "profession" .= pr, "deseases" .= dss]

instance FromJSON FilteredPerson where
    parseJSON = genericParseJSON defaultOptions


data IdAndFullNamePerson = IdAndFullNamePerson 
    { pId2 :: Int
    , firstname2 :: Text
    , lastName2 :: Text
   } deriving (Generic , Eq)

instance ToJSON IdAndFullNamePerson where
    toJSON (IdAndFullNamePerson id name lastname) = object [ "id" .= id, "name" .= name ,"lastname".= lastname]

instance FromJSON IdAndFullNamePerson where
    parseJSON = genericParseJSON defaultOptions

instance FromRow IdAndFullNamePerson where
fromRow = IdAndFullNamePerson <$> field
                   <*> field
                   <*> field