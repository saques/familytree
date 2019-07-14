{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

module Api.Controllers.FamilyTreeController (
    familyTreeControllerInit, 
    FamilyTreeController
    ) where

import Api.Types (DBPerson(DBPerson))
import Api.Types (FilteredPerson(FilteredPerson))
import Api.Types (IdAndFullNamePerson(IdAndFullNamePerson))
import Api.Types (FamilyTreeData(FamilyTreeData))
import Api.Types (ResponseId(ResponseId))
import Api.Types (ParentRelation(ParentRelation))
import Api.Types (FamilyTreeIdAndLevel(FamilyTreeIdAndLevel))
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
import Data.List
import Data.Text
import Data.Char

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
                ("/name/:name", method GET getFamilyTreeByName),
                ("/parent-relations", method GET getParentRelations),
                ("/:id/filter", method GET getPersonsWithFilter),
                ("/:id/persons", method GET getPersonsOfFamilyTree),
                ("/common", method GET getPersonsInCommon),
                ("/marriage", method POST mergeInMarriage),
                ("/:id/parent/:descendantId", method POST addPersonAsParent),
                ("/:id/descendant/:parentId", method POST addPersonAsDescendant),
                ("/:id/level", method POST addPersonToLevel),
                ("/:name", method POST createFamilyTree)]

generateFromPersons :: [DBPerson] -> FamilyTree -> FamilyTree
generateFromPersons [] ft = ft
generateFromPersons ( (DBPerson id ftId l fn ln bd hc ec sc dd dp pr dss age) :ps) ft =  generateFromPersons ps (addToLevel ft (Ps id fn ln bd age (getHairColor hc) (getEyeColor ec)  (getSkinColor sc)  False dd dp pr (getDeseases (show dss))) l) 

getDeseases :: [Char] -> [Deseases]
getDeseases "}" = []
getDeseases (x:xs) = if ((isDigit x) == False) then (getDeseases xs) else (getDesease (digitToInt  x)) : (getDeseases xs)
getDeseases a = []


getHairColor :: Text -> HairColor
getHairColor "Rubio" = Blonde
getHairColor "Castaño" = BrownHair
getHairColor "Morocho" = BlackHair
getHairColor "Colorado" = Red
getHairColor "Blanco" = White
getHairColor a = IncorrectHairColor

getEyeColor :: Text -> EyeColor
getEyeColor "Verdes" = Green
getEyeColor "Azules" = Blue
getEyeColor "Marrones" = Brown
getEyeColor "Negros" = Black
getEyeColor a = IncorrectEyeColor

getSkinColor :: Text -> SkinColor
getSkinColor "Clara" = LightSkin
getSkinColor "Oscura" = DarkSkin
getSkinColor a = IncorrectSkinColor


getDesease :: Int -> Deseases
getDesease 0 = Cancer
getDesease 1 = Diabetes
getDesease 2 = Leukemia
getDesease a = IncorrectDesease



getIds :: [DBPerson] -> [Int]
getIds [] = []
getIds ( (DBPerson id _ _ _ _ _ _ _ _ _ _ _ _ _ ) :ps) =  id : (getIds ps)

addRelations :: [ParentRelation] -> FamilyTree -> FamilyTree
addRelations [] ft = ft
addRelations ( (ParentRelation _ dId pId) : ps) ft =  addRelations ps (markAsParentInTree ft pId dId) 


getFamilyTreeById :: Handler b FamilyTreeController ()
getFamilyTreeById = do 
    maybeId <- getParam "id"
    let id = fromMaybe "" maybeId
    fts <- query "SELECT id FROM family_trees WHERE id = ?" (Only id)
    if Prelude.null $ (fts :: [ResponseId])
        then do
            modifyResponse $ setResponseCode 404
            writeLBS "Not found"
        else do
            modifyResponse $ setHeader "Content-Type" "application/json"
            persons <- query "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int FROM persons WHERE family_tree_id = ?" (Only id)    
            if Prelude.null $ (persons :: [DBPerson])
                then do
                    writeLBS . encode $ ( (addRelations [] (generateFromPersons persons (Ft []))) :: FamilyTree) 
                else do 
                    parentRelations <- query "SELECT * FROM parent_relation where descendant_id in (SELECT unnest(?))"  (Only ( PGArray {fromPGArray =  (getIds persons)}))
                    writeLBS . encode $ ( (addRelations parentRelations (generateFromPersons persons (Ft []))) :: FamilyTree) 


getPersonsWithFilter :: Handler b FamilyTreeController ()
getPersonsWithFilter = do 
    maybeFamilyTreeId <- getParam "id"
    maybeName <- getQueryParam "name"
    maybeLastName <- getQueryParam "lastName"
    maybeHairColor <- getQueryParam "hairColor"
    maybeEyeColor <- getQueryParam "eyeColor"
    maybeSkinColor <- getQueryParam "skinColor"
    maybeDeathPlace <- getQueryParam "deathPlace"
    maybeProfession <- getQueryParam "profession"
    maybeDesease <- getQueryParam "desease"
    maybeAge <- getQueryParam "age"


    let familyTreeId = fromMaybe "" maybeFamilyTreeId
        name = fromMaybe "" maybeName
        lastName = fromMaybe "" maybeLastName
        hairColor = fromMaybe "" maybeHairColor
        eyeColor = fromMaybe "" maybeEyeColor
        skinColor = fromMaybe "" maybeSkinColor
        deathPlace = fromMaybe "" maybeDeathPlace
        profession = fromMaybe "" maybeProfession
        desease = fromMaybe "" maybeDesease
        age = fromMaybe "" maybeAge

    persons <- query   "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text ,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int  FROM persons WHERE family_tree_id = ? and (? = '' or initcap(name) = initcap(?)) and (? = '' or initcap(last_name) = initcap(?)) and (? = '' or initcap(hair_color) = initcap(?) ) and (? = '' or initcap(eye_color) = initcap(?) ) " (familyTreeId, name, name, lastName, lastName, hairColor, hairColor, eyeColor, eyeColor) 
    persons2 <- query   "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int  FROM persons WHERE family_tree_id = ? and (? = '' or initcap(skin_color) = initcap(?) ) and (? = '' or initcap(death_place) = initcap(?) ) and (? = '' or initcap(profession) = initcap(?) ) and (? = '' or cast(coalesce(nullif(?,''),'-1') as float) in (select unnest(deseases))) " (familyTreeId, skinColor, skinColor, deathPlace, deathPlace, profession, profession, desease, desease) 
    persons3 <- query   "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text ,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int  FROM persons WHERE family_tree_id = ? and (? = '' or cast(coalesce(nullif(?,''),'-1') as float) = (select extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY'))))) " (familyTreeId, age, age) 

    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ ( dbPersonToFilteredPerson ((persons `intersect` (persons2 `intersect` persons3)) :: [DBPerson]) )

dbPersonToFilteredPerson  :: [DBPerson] -> [FilteredPerson]
dbPersonToFilteredPerson [] = []
dbPersonToFilteredPerson ( (DBPerson id ftId l fn ln bd hc ec sc dd dp pr dss age) : ps) = (FilteredPerson id ftId l fn ln bd hc ec sc dd dp pr (getDeseases (show dss)) age)  : (dbPersonToFilteredPerson ps)

getPersonsOfFamilyTree :: Handler b FamilyTreeController ()
getPersonsOfFamilyTree = do 
    maybeFamilyTreeId <- getParam "id"

    let familyTreeId = fromMaybe "" maybeFamilyTreeId
       

    persons <- query   "SELECT id,name,last_name FROM persons WHERE family_tree_id = ? " (Only familyTreeId) 
    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ (persons :: [IdAndFullNamePerson] )


getPersonsInCommon :: Handler b FamilyTreeController ()
getPersonsInCommon = do 
    maybeFamilyTree1Id <- getQueryParam "id1"
    maybeFamilyTree2Id <- getQueryParam "id2"

    let familyTree1Id = fromMaybe "" maybeFamilyTree1Id
        familyTree2Id = fromMaybe "" maybeFamilyTree2Id

    persons <- query   "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text ,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int  FROM persons WHERE family_tree_id = cast(coalesce(nullif(?,''),'-1') as float)" (Only familyTree1Id) 
    persons2 <- query   "SELECT id, family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place, profession, deseases::text ,extract (year from AGE(current_date,TO_DATE(birth_date, 'DD/MM/YYYY')))::int  FROM persons WHERE family_tree_id = cast(coalesce(nullif(?,''),'-1') as float)" (Only familyTree2Id) 

    modifyResponse $ setHeader "Content-Type" "application/json"
    writeLBS . encode $ ( dbPersonToFilteredPerson ((persons `intersect` persons2 ) :: [DBPerson]) )

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
    execute "UPDATE  persons SET deseases = ? , profession = ? where id = ? " [deseases,profession, B.pack (show (getParentIdFromQuery (parentId:: [ResponseId])))]
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
    execute "UPDATE  persons SET deseases = ? , profession = ? where id = ? " [deseases,profession, B.pack (show (getParentIdFromQuery (descendantId:: [ResponseId])))]
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
    personId <- query "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?) returning id" (familyTreeId , level, name, lastName, birthDate, hairColor, eyeColor, skinColor, deathDate, deathPlace)
    execute "UPDATE  persons SET deseases = ? , profession = ? where id = ? " [deseases,profession, B.pack (show (getParentIdFromQuery (personId:: [ResponseId])))]
    modifyResponse $ setResponseCode 201
    writeLBS . encode $ ( personId :: [ResponseId] )       

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

getFamilyTreeByName :: Handler b FamilyTreeController ()
getFamilyTreeByName = do
    maybeName <- getParam "name"
    let  name = fromMaybe "" maybeName
    ids <- query "SELECT id FROM family_trees WHERE name = ?" (Only name)

    if Prelude.null $ (ids :: [ResponseId])
        then do
            modifyResponse $ setResponseCode 404
            writeLBS "Not found"
        else do 
            modifyResponse $ setResponseCode 200
            writeLBS . encode $ (ids :: [ResponseId])


getLevelDifference :: [FamilyTreeIdAndLevel] -> [FamilyTreeIdAndLevel] -> Int
getLevelDifference [] a = -1
getLevelDifference a [] = -1
getLevelDifference ((FamilyTreeIdAndLevel _ lvl):ftIds)  ((FamilyTreeIdAndLevel _ lvl2):ftIds2) = lvl - lvl2

getFtId :: [FamilyTreeIdAndLevel]  -> Int
getFtId [] = -1
getFtId  ((FamilyTreeIdAndLevel ftid _):ftIds)   = ftid

getFtIdFromRespId :: [ResponseId] -> Int
getFtIdFromRespId [] = -1
getFtIdFromRespId ((ResponseId id):rs) = id

mergeInMarriage :: Handler b FamilyTreeController ()
mergeInMarriage = do 
    maybePerson1Id <- getPostParam "id1"
    maybePerson2Id <- getPostParam "id2"
    maybeFamilyTreeName <- getPostParam "familyTreeName"


    let person1Id = fromMaybe "" maybePerson1Id
        person2Id = fromMaybe "" maybePerson2Id
        familyTreeName = fromMaybe "" maybeFamilyTreeName

    ids <- query "SELECT id FROM family_trees WHERE name = ?" (Only familyTreeName)

    if Prelude.null $ (ids :: [ResponseId])
        then do
            familyTreeId <- query "INSERT INTO family_trees (name) VALUES (?) returning id" (Only familyTreeName)
            person1 <- query   "SELECT  family_tree_id,level from persons where id = ? limit 1" (Only person1Id) 
            person2 <- query   "SELECT  family_tree_id,level from persons where id = ? limit 1" (Only person2Id) 
            let levelDifference = getLevelDifference person1 person2
                ft1id = getFtId person1 
                ft2id = getFtId person2
            do
            execute "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place,profession,deseases)  (SELECT ?,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place,profession,deseases FROM persons where family_tree_id = ?);" [(getFtIdFromRespId familyTreeId),ft1id]
            execute "INSERT INTO persons (family_tree_id,level,name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place,profession,deseases)  (SELECT ?, (level+?),name,last_name,birth_date,hair_color,eye_color,skin_color, death_date, death_place,profession,deseases FROM persons where family_tree_id = ?);" [ (getFtIdFromRespId familyTreeId),levelDifference, ft2id]
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






