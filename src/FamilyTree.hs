import qualified Data.Set as Set

data Person = Ps Name LastName Age  HairColor EyeColor SkinColor Dead DeadPlace Profession [Deseases] deriving (Show , Eq, Ord)
type Name = String
type LastName = String
type Age = Int
type Dead = Bool
type DeadPlace = String
type Profession = String
data HairColor = Blonde | BrownHair | BlackHair | Red deriving (Show , Eq, Ord)
data EyeColor = Green | Blue | Brown | Black deriving (Show , Eq, Ord)
data SkinColor = LightSkin | DarkSkin deriving (Show , Eq, Ord)
data Deseases = Cancer | Diabetes | Celiac deriving (Show , Eq, Ord)



data FamilyTree =  Ft [FamilyTree] | WithChildren Person [FamilyTree] | Single Person  deriving (Show, Eq, Ord)

nicolas =  (Ps "Nicolas" "Marcantonio" 23 BlackHair Black DarkSkin False "" "Engineer" [])
patricia =  (Ps "Patricia" "Ruiz" 63 BrownHair Brown LightSkin False "" "Accountant" (Cancer:[]) ) 
sergio = (Ps "Sergio" "Marcantonio" 57 BlackHair Green LightSkin False "" "Veterinarian" [])
joaquin = (Ps "Joaquin" "Marcantonio" 21 BlackHair Black DarkSkin False "" "Publicist" [])

nicolasFamilyTree = Ft [WithChildren patricia [Single nicolas  , Single joaquin] , WithChildren sergio  [Single nicolas, Single joaquin]]


haveChildrenInCommon (Ft [(WithChildren p ft) , (WithChildren p2 ft2)]) =   
   let a = Set.fromList ft  
       b = Set.fromList ft2 
            in  Set.size (a `Set.intersection` b) > 0

filterByName name (Ps n _ _ _ _ _ _ _ _ _ ) = n == name
filterByLastName lastname (Ps _ l _ _ _ _ _ _ _ _ ) = l == lastname
filterByAge age (Ps _ _ a _ _ _ _ _ _ _ ) = a == age
filterByHairColor hairColor (Ps _ _ _ hc _ _ _ _ _ _ ) = hc == hairColor
filterByEyeColor eyeColor (Ps _ _ _ _ ec _ _ _ _ _ ) = ec == eyeColor
filterBySkinColor skinColor (Ps _ _ _ _ _ sc _ _ _ _ ) = sc == skinColor
filterByDead dead (Ps _ _ _ _ _ _ d _ _ _ ) = d == dead
filterByDeadPlace deadPlace (Ps _ _ _ _ _ _ _ dp _ _ ) = dp == deadPlace
filterByProfession profession (Ps _ _ _ _ _ _ _ _ pr _ ) = pr == profession
filterByDesease desease (Ps _ _ _ _ _ _ _ _ _ dss ) = desease `elem` dss


composeFilters [] p = True
composeFilters [f] p = f p
composeFilters (f:fs) p = f p && (composeFilters fs p)

showPersons ft condition = Set.fromList (showPersonsWithDuplicates ft condition)

showPersonsWithDuplicates (Single p) condition = 
     let belongs = condition p
           in if belongs then p:[] else []
showPersonsWithDuplicates (WithChildren p ft) condition =
     let belongs = condition p
         predecessors = showPersonsWithDuplicates (Ft ft) condition
         in if belongs then p:predecessors else predecessors
showPersonsWithDuplicates (Ft [ft]) condition = (showPersonsWithDuplicates ft condition) 
showPersonsWithDuplicates (Ft (ft:fts)) condition = (showPersonsWithDuplicates ft condition) ++ (showPersonsWithDuplicates (Ft fts) condition)


filterFamilyTreeNameSergioAge57 ft = showPersons ft (composeFilters ((filterByName "Sergio"):(filterByAge 57):[]))

getAge (Ps _ _ a _ _ _ _ _ _ _ ) = a

showOlder (Single p) = p
showOlder (WithChildren p ft) = 
     let p2 = (showOlder (Ft ft))
         age = getAge p
         age2 = getAge p2 
             in if age > age2 then p else p2
showOlder (Ft [ft]) =  (showOlder ft)
showOlder (Ft (ft:fts)) =
    let p1 = (showOlder ft)
        p2 = (showOlder (Ft fts))
        age = getAge p1 
        age2 = getAge p2 
             in if age > age2 then p1 else p2



