{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}


module FamilyTree where


import qualified Data.Set as Set
import Prelude
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.List
import GHC.Generics
import Data.Text



data Person = Ps Id Name LastName BirthDate Age HairColor EyeColor SkinColor Dead DeathDate DeathPlace  Profession [Deseases] deriving (Show , Eq, Ord, Generic)
data PersonWithParents = PsP Person [Int] deriving (Show , Eq, Ord, Generic)

type Id = Int
type Name = Text
type LastName = Text
type BirthDate = Text
type Age = Int
type DeathDate = Maybe Text
type Dead = Bool
type DeathPlace = Maybe Text
type Profession = Maybe Text
data HairColor = Blonde | BrownHair | BlackHair | Red | White | IncorrectHairColor deriving (Show , Eq, Ord, Generic)
data EyeColor = Green | Blue | Brown | Black | IncorrectEyeColor deriving (Show , Eq, Ord, Generic)
data SkinColor = LightSkin | DarkSkin |  IncorrectSkinColor deriving (Show , Eq, Ord, Generic)
data Deseases = Cancer | Diabetes | Leukemia | IncorrectDesease deriving (Show , Eq, Ord, Generic)



data FamilyTree = Ft [LevelWithPersons] deriving (Show , Eq, Ord, Generic)

data LevelWithPersons = Lwp (Int,[PersonWithParents]) deriving (Show , Eq, Ord, Generic)


filterByName name (Ps _ n _ _ _  _ _ _ _ _ _ _ _ ) = n == name
filterByLastName lastname (Ps _ _ l _ _ _ _ _ _ _ _ _ _) = l == lastname
filterByBirthDate birthDate (Ps _ _ _ bd _ _ _ _ _ _ _ _ _ ) = bd == birthDate
filterByHairColor hairColor (Ps _ _ _ _ _ hc _ _ _ _ _ _ _ ) = hc == hairColor
filterByEyeColor eyeColor (Ps _ _ _ _ _ _ ec _ _ _ _ _ _ ) = ec == eyeColor
filterBySkinColor skinColor (Ps _ _ _ _ _ _ _ sc _ _ _ _ _ ) = sc == skinColor
filterByDead dead (Ps _ _ _ _ _ _ _ _ d _ _ _ _ ) = d == dead
filterByDeathDate deathDate (Ps _ _ _ _ _ _ _ _ _ dd _ _ _ ) = dd == deathDate
filterByDeathPlace deathPlace ( Ps _ _ _ _ _ _ _ _ _ _ dp _ _ ) = dp == deathPlace
filterByProfession profession (Ps _ _ _ _ _ _ _ _ _ _ _ pr _ ) = pr == profession
filterByDesease desease (Ps _ _ _ _ _ _ _ _ _ _ _ _ dss ) = desease `elem` dss
filterByAge age (Ps _ _ _ _ a _ _ _ _ _ _ _ _ )  = a == age

noFilter p = True

composeFilters [] p = True
composeFilters [f] p = f p
composeFilters (f:fs) p = f p && (composeFilters fs p)

checkIfBelongs (PsP p rs) condition = 
     let belongs = condition p
           in if belongs then p:[] else []

showPersonsForLevel [] condition =  []
showPersonsForLevel (pP : psP) condition = checkIfBelongs (pP) condition ++ showPersonsForLevel  psP condition


showPersons (Ft []) condition = []
showPersons (Ft ( (Lwp (l,(p:ps))) : lvls)) condition = (showPersonsForLevel (p:ps) condition) ++ (showPersons (Ft lvls) condition)

showPersonsInCommon ft ft2 =  (showPersons ft noFilter) `intersect` (showPersons ft2 noFilter)

isInFamilyTreeForLevel [] person =  False
isInFamilyTreeForLevel ((PsP p r) : psP) person = p == person || isInFamilyTreeForLevel  psP person

isInFamilyTree (Ft []) person = False
isInFamilyTree (Ft ( Lwp (l,(p:ps) ) : lvls)) person = (isInFamilyTreeForLevel (p:ps) person) || (isInFamilyTree (Ft lvls) person)




filterFamilyTreeNameSergioAge57 ft = showPersons ft (composeFilters ((filterByName "Sergio"):(filterByAge 57):[]))


addToLevelWrap [] person level =  ([(Lwp ((level) , (PsP person []):[] ))])
addToLevelWrap (( (Lwp (l,ps)) : lvls)) person level = 
    let shouldAdd = level  ==  l 
        in if shouldAdd then ( (Lwp (l,((PsP person []):ps))) : lvls)   else ( ( (Lwp (l, ps )) : (addToLevelWrap lvls person level) ) ) 

addToLevel (Ft lvls) person level = (Ft (addToLevelWrap lvls person level))


getId (PsP (Ps id _ _ _ _ _ _ _ _ _ _ _ _ ) _ ) = id
addParentId (PsP person parents) parentId = (PsP person (parentId:parents))

markAsParentAtLevel [] parentId descendantId = Nothing
markAsParentAtLevel  (p:ps) parentId descendantId = 
             let isThePerson =  (getId p) == descendantId 
                  in if isThePerson then Just ( (addParentId p parentId ):ps)                     
                        else let ans = (markAsParentAtLevel ps parentId descendantId)
                          in case (ans) of 
                              Nothing  -> Nothing
                              Just newPs -> (Just (p:newPs))

markAsParentInTree (Ft lvls) parentId descendantId = Ft (markAsParent lvls parentId descendantId)


markAsParent [] parentId descendantId = []
markAsParent  ( (Lwp (l, ps)) : lvls) parentId descendantId =    
     let ans = (markAsParentAtLevel ps parentId descendantId)
        in case (ans) of 
              Nothing  -> ( (Lwp (l,ps)) : (markAsParent lvls parentId descendantId))
              Just newPs -> ( (Lwp (l,newPs)) : lvls)

nicolas = (Ps 0 "Nicolas" "Marcantonio" "14/06/1996" 23 BlackHair Brown DarkSkin False Nothing Nothing (Just "Engineer") [])
patricia = (Ps 1 "Patricia" "Ruiz" "10/03/1956" 63 BrownHair Brown LightSkin False Nothing Nothing (Just "Accountant") (Cancer:[]))
sergio = (Ps 2 "Sergio" "Marcantonio" "09/11/1961" 57 BlackHair Green LightSkin False Nothing Nothing (Just "Veterinarian") [])
joaquin = (Ps 3 "Joaquin" "Marcantonio" "19/12/1997" 21 BlackHair Brown DarkSkin False Nothing Nothing (Just "Student") [])
antonia = (Ps 4 "Antonia" "Perez" "12/06/1926" 93 BlackHair Brown LightSkin False Nothing Nothing (Just "Teacher") [])

luis = (Ps 5 "Luis" "Marcantonio" "06/08/1930" 88 BlackHair Brown LightSkin False Nothing Nothing (Just "Banker") [])

nicolasFamilyTreeDB = markAsParentInTree (addToLevel (addToLevel (Ft []) nicolas 0) sergio (-1)) 2 0