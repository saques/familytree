{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Controllers.FamilyTreeController (
    familyTreeControllerInit, 
    FamilyTreeController
    ) where

import Api.Types (DBPerson(DBPerson))
import Api.Types (FamilyTreeData(FamilyTreeData))
import Api.Types (ResponseId(ResponseId))
import Api.Types (ParentRelation(ParentRelation))
import Api.Utils
import Control.Lens (makeLenses)
import Snap.Core
import Snap.Snaplet
import Control.Applicative
import Snap.Snaplet.PostgresqlSimple
import Control.Monad.State.Class (get)
import Data.Aeson (encode)
import FamilyTree
import Database.PostgreSQL.Simple.Types

import Data.Maybe
import Data.ByteString.Lazy
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B

data FamilyTreeController = FamilyTreeController 
    {
        _db   :: Snaplet Postgres
    }

makeLenses ''FamilyTreeController



apiRoutes :: [(B.ByteString, Handler b FamilyTreeController ())]
apiRoutes = Prelude.map (mapSecond (authenticate >>))
            [("/", method GET getFamilyTrees),
                ("/:id", method GET getFamilyTreeById),
                ("/parent-relations", method GET getParentRelations),
                ("/:id/parent/:descendantId", method POST addPersonAsParent),
                ("/:id/descendant/:parentId", method POST addPersonAsDescendant),
                ("/:id/level", method POST addPersonToLevel),
                ("/:name", method POST createFamilyTree)]

generateFromPersons :: [DBPerson] -> FamilyTree -> FamilyTree
generateFromPersons [] ft = ft
generateFromPersons ( (DBPerson id ftId l fn ln bd hc ec sc dd dp pr dss) :ps) ft =  generateFromPersons ps (addToLevel ft (Ps id fn ln bd 12 Blonde Brown DarkSkin False dd dp pr (Cancer:[])) l) 

getIds :: [DBPerson] -> [Int]
getIds [] = []
getIds ( (DBPerson id _ _ _ _ _ _ _ _ _ _ _ _) :ps) =  id : (getIds ps)

addRelations :: [ParentRelation] -> FamilyTree -> FamilyTree
addRelations [] ft = ft
addRelations ( (ParentRelation _ dId pId) : ps) ft =  addRelations ps (markAsParentInTree ft pId dId) 


getFamilyTreeById :: Handler b FamilyTreeController ()
getFamilyTreeById = do 
    maybeId <- getParam "id"
    let id = fromMaybe "" maybeId
    persons <- query "SELECT * FROM persons WHERE family_tree_id = ?" (Only id)
    if Prelude.null $ (persons :: [DBPerson])
        then do
            modifyResponse $ setResponseCode 404
            writeLBS "Not found"
        else do
            parentRelations <- query "SELECT * FROM parent_relation where descendant_id in (SELECT unnest(?))"  (Only ( PGArray {fromPGArray =  (getIds persons)}))
            modifyResponse $ setHeader "Content-Type" "application/json"
            writeLBS . encode $ ( (addRelations parentRelations (generateFromPersons persons (Ft []))) :: FamilyTree) 


getFamilyTrees :: Handler b FamilyTreeController ()
getFamilyTrees = do 
    ids <- query_ "SELECT * FROM family_trees" 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ ( ids :: [FamilyTreeData])

getParentRelations :: Handler b FamilyTreeController ()
getParentRelations = do 
    parentRelations <- query_ "SELECT * FROM parent_relation" 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ ( parentRelations :: [ParentRelation])


getParentIdFromQuery :: [ResponseId] -> Int
getParentIdFromQuery [] = -1
getParentIdFromQuery ((ResponseId id) :ps) = id


addPersonAsParent :: Handler b FamilyTreeController ()
addPersonAsParent = do 
    maybeFamilyTreeId <- getParam "id"
    maybeLevel <- getParam "level"
    maybeDescendantId <- getParam "descendantId"
    maybeName <- getPostParam "name"
    maybeLastName <- getPostParam "lastName"
    maybeBirthDate <- getPostParam "birthDate"
    maybeHairColor <- getPostParam "hairColor"
    maybeEyeColor <- getPostParam "eyeColor"
    maybeSkinColor <- getPostParam "skinColor"
    maybeDeathDate <- getPostParam "deathDate"
    maybeDeathPlace <- getPostParam "deathPlace"
    maybeProfession <- getPostParam "profession"
    maybeDeseases <- getPostParam "deseases"

    let familyTreeId = fromMaybe "" maybeFamilyTreeId
        level = fromMaybe "" maybeLevel
        descendantId = fromMaybe "" maybeDescendantId
        name = fromMaybe "" maybeName
        lastName = fromMaybe "" maybeLastName
        birthDate = fromMaybe "" maybeBirthDate
        hairColor = fromMaybe "" maybeHairColor
        eyeColor = fromMaybe "" maybeEyeColor
        skinColor = fromMaybe "" maybeSkinColor
        deathDate = fromMaybe "" maybeDeathDate
        deathPlace = fromMaybe "" maybeDeathPlace
        profession = fromMaybe "" maybeProfession
        deseases = fromMaybe "" maybeDeseases
    do
    parentId <- query "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id" (familyTreeId , level, name, lastName, birthDate, hairColor, eyeColor, skinColor, deathDate, deathPlace)
    execute "INSERT INTO parent_relation (descendant_id, parent_id) VALUES (?, ?) " [descendantId, B.pack (show (getParentIdFromQuery (parentId:: [ResponseId])))]
    modifyResponse $ setResponseCode 201
    writeLBS . encode $ ( parentId :: [ResponseId] )

addPersonAsDescendant :: Handler b FamilyTreeController ()
addPersonAsDescendant = do 
    maybeFamilyTreeId <- getParam "id"
    maybeLevel <- getParam "level"
    maybeParentId <- getParam "parentId"
    maybeName <- getPostParam "name"
    maybeLastName <- getPostParam "lastName"
    maybeBirthDate <- getPostParam "birthDate"
    maybeHairColor <- getPostParam "hairColor"
    maybeEyeColor <- getPostParam "eyeColor"
    maybeSkinColor <- getPostParam "skinColor"
    maybeDeathDate <- getPostParam "deathDate"
    maybeDeathPlace <- getPostParam "deathPlace"
    maybeProfession <- getPostParam "profession"
    maybeDeseases <- getPostParam "deseases"

    let familyTreeId = fromMaybe "" maybeFamilyTreeId
        level = fromMaybe "" maybeLevel
        parentId = fromMaybe "" maybeParentId
        name = fromMaybe "" maybeName
        lastName = fromMaybe "" maybeLastName
        birthDate = fromMaybe "" maybeBirthDate
        hairColor = fromMaybe "" maybeHairColor
        eyeColor = fromMaybe "" maybeEyeColor
        skinColor = fromMaybe "" maybeSkinColor
        deathDate = fromMaybe "" maybeDeathDate
        deathPlace = fromMaybe "" maybeDeathPlace
        profession = fromMaybe "" maybeProfession
        deseases = fromMaybe "" maybeDeseases
    do
    descendantId <- query "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id" (familyTreeId , level, name, lastName, birthDate, hairColor, eyeColor, skinColor, deathDate, deathPlace)
    execute "INSERT INTO parent_relation (descendant_id, parent_id) VALUES (?, ?) " [B.pack (show (getParentIdFromQuery (descendantId:: [ResponseId]))), parentId]
    modifyResponse $ setResponseCode 201
    writeLBS . encode $ ( descendantId :: [ResponseId] )


addPersonToLevel :: Handler b FamilyTreeController ()
addPersonToLevel = do 
    maybeFamilyTreeId <- getParam "id"
    maybeLevel <- getParam "level"
    maybeDescendantId <- getParam "descendantId"
    maybeName <- getPostParam "name"
    maybeLastName <- getPostParam "lastName"
    maybeBirthDate <- getPostParam "birthDate"
    maybeHairColor <- getPostParam "hairColor"
    maybeEyeColor <- getPostParam "eyeColor"
    maybeSkinColor <- getPostParam "skinColor"
    maybeDeathDate <- getPostParam "deathDate"
    maybeDeathPlace <- getPostParam "deathPlace"
    maybeProfession <- getPostParam "profession"
    maybeDeseases <- getPostParam "deseases"

    let familyTreeId = fromMaybe "" maybeFamilyTreeId
        level = fromMaybe "" maybeLevel
        descendantId = fromMaybe "" maybeDescendantId
        name = fromMaybe "" maybeName
        lastName = fromMaybe "" maybeLastName
        birthDate = fromMaybe "" maybeBirthDate
        hairColor = fromMaybe "" maybeHairColor
        eyeColor = fromMaybe "" maybeEyeColor
        skinColor = fromMaybe "" maybeSkinColor
        deathDate = fromMaybe "" maybeDeathDate
        deathPlace = fromMaybe "" maybeDeathPlace
        profession = fromMaybe "" maybeProfession
        deseases = fromMaybe "" maybeDeseases
    do
    parentId <- query "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id" (familyTreeId , level, name, lastName, birthDate, hairColor, eyeColor, skinColor, deathDate, deathPlace)
    modifyResponse $ setResponseCode 201
    writeLBS . encode $ ( parentId :: [ResponseId] )       

createFamilyTree :: Handler b FamilyTreeController ()
createFamilyTree = do 
    maybeName <- getParam "name"
    let  name = fromMaybe "" maybeName
    ids <- query "SELECT id FROM family_trees WHERE name = ?" (Only name)

    if Prelude.null $ (ids :: [ResponseId])
        then do
            familyTreeId <- query "INSERT INTO family_trees (name) VALUES (?) returning id" (Only name)
            modifyResponse $ setResponseCode 201
            writeLBS . encode $ (familyTreeId :: [ResponseId])
        else do 
            modifyResponse $ setResponseCode 403
            writeLBS . encode $ (ids :: [ResponseId])


familyTreeControllerInit :: Snaplet Postgres -> SnapletInit b FamilyTreeController
familyTreeControllerInit db = makeSnaplet "persons" "Family Tree Controller" Nothing $ do
    addRoutes apiRoutes
    return $ FamilyTreeController db


instance HasPostgres (Handler b FamilyTreeController) where
    getPostgresState = with db get