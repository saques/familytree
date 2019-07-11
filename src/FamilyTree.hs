import qualified Data.Set as Set
import Prelude
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.List



data Person = Ps Id Name LastName BirthDate Age HairColor EyeColor SkinColor Dead DeathDate DeathPlace  Profession [Deseases] deriving (Show , Eq, Ord)
data PersonWithParents = PsP Person [Int] deriving (Show , Eq, Ord)

type Id = Int
type Name = String
type LastName = String
type BirthDate = String
type Age = Int
type DeathDate = String
type Dead = Bool
type DeathPlace = String
type Profession = String
data HairColor = Blonde | BrownHair | BlackHair | Red deriving (Show , Eq, Ord)
data EyeColor = Green | Blue | Brown | Black deriving (Show , Eq, Ord)
data SkinColor = LightSkin | DarkSkin deriving (Show , Eq, Ord)
data Deseases = Cancer | Diabetes | Celiac deriving (Show , Eq, Ord)



data FamilyTree = Ft [(Int,[PersonWithParents])] deriving (Show , Eq, Ord)

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
showPersons (Ft ( (l,(p:ps)) : lvls)) condition = (showPersonsForLevel (p:ps) condition) ++ (showPersons (Ft lvls) condition)

showPersonsInCommon ft ft2 =  (showPersons ft noFilter) `intersect` (showPersons ft2 noFilter)

isInFamilyTreeForLevel [] person =  False
isInFamilyTreeForLevel ((PsP p r) : psP) person = p == person || isInFamilyTreeForLevel  psP person

isInFamilyTree (Ft []) person = False
isInFamilyTree (Ft ( (l,(p:ps)) : lvls)) person = (isInFamilyTreeForLevel (p:ps) person) || (isInFamilyTree (Ft lvls) person)




filterFamilyTreeNameSergioAge57 ft = showPersons ft (composeFilters ((filterByName "Sergio"):(filterByAge 57):[]))

getLevelOfPerson [] person =  Nothing
getLevelOfPerson ([(l, ps)]) person =  
    let isInLevel = person `elem` (showPersonsForLevel (ps) noFilter)
        in if isInLevel then Just l else Nothing
getLevelOfPerson (( (l, ps) : lvls)) person = 
    let isInLevel = person `elem` (showPersonsForLevel (ps) noFilter)
        in if isInLevel then Just l else  (getLevelOfPerson lvls person)



addToLevelWrap [] person level =  ([((level) , (PsP person []):[] )])
addToLevelWrap (( (l,ps) : lvls)) person level = 
    let shouldAdd = level  ==  l 
        in if shouldAdd then ( (l,((PsP person []):ps)) : lvls)   else ( ( (l, ps ) : (addToLevelWrap lvls person level) ) ) 

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
markAsParent  ((l, ps) : lvls) parentId descendantId =    
     let ans = (markAsParentAtLevel ps parentId descendantId)
        in case (ans) of 
              Nothing  -> ((l,ps) : (markAsParent lvls parentId descendantId))
              Just newPs -> ((l,newPs) : lvls)

nicolas = (Ps 0 "Nicolas" "Marcantonio" "14/06/1996" 23 BlackHair Brown DarkSkin False "" "" "Engineer" [])
patricia = (Ps 1 "Patricia" "Ruiz" "10/03/1956" 63 BrownHair Brown LightSkin False "" "" "Accountant" (Cancer:[]))
sergio = (Ps 2 "Sergio" "Marcantonio" "09/11/1961" 57 BlackHair Green LightSkin False "" "" "Veterinarian" [])
joaquin = (Ps 3 "Joaquin" "Marcantonio" "19/12/1997" 21 BlackHair Brown DarkSkin False "" "" "Student" [])
antonia = (Ps 4 "Antonia" "Perez" "12/06/1926" 93 BlackHair Brown LightSkin False "" "" "Teacher" [])

luis = (Ps 5 "Luis" "Marcantonio" "06/08/1930" 88 BlackHair Brown LightSkin False "" "" "Banker" [])

nicolasFamilyTreeDB = markAsParentInTree (addToLevel (addToLevel (Ft []) nicolas 0) sergio (-1)) 2 0