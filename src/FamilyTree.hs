import qualified Data.Set as Set

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

data Attributes = Attr Name LastName Age  HairColor EyeColor SkinColor Dead DeadPlace Profession [Deseases] deriving (Show , Eq, Ord)
data Person = Ps Attributes deriving (Show , Eq, Ord)
data PersonWithRelations = PsR Attributes [Person] deriving (Show , Eq, Ord)


data FamilyTree = Ft [(Int,[PersonWithRelations])] deriving (Show , Eq, Ord)

nicolas =  (PsR (Attr "Nicolas" "Marcantonio" 23 BlackHair Brown DarkSkin False "" "Engineer" [])  [Ps (Attr "Patricia" "Ruiz" 63 BrownHair Brown LightSkin False "" "Accountant" (Cancer:[])) ,  (Ps (Attr "Sergio" "Marcantonio" 57 BlackHair Green LightSkin False "" "Veterinarian" []))])
patricia =  (PsR (Attr "Patricia" "Ruiz" 63 BrownHair Brown LightSkin False "" "Accountant" (Cancer:[]) ) []) 
sergio = (PsR (Attr "Sergio" "Marcantonio" 57 BlackHair Green LightSkin False "" "Veterinarian" []) [] )

nicolasFamilyTree2 = Ft [ (0, [patricia,sergio]),(1,[nicolas]) ]

filterByName name (Attr n _ _ _ _ _ _ _ _ _ ) = n == name
filterByLastName lastname (Attr _ l _ _ _ _ _ _ _ _ ) = l == lastname
filterByAge age (Attr _ _ a _ _ _ _ _ _ _ ) = a == age
filterByHairColor hairColor (Attr _ _ _ hc _ _ _ _ _ _ ) = hc == hairColor
filterByEyeColor eyeColor (Attr _ _ _ _ ec _ _ _ _ _ ) = ec == eyeColor
filterBySkinColor skinColor (Attr _ _ _ _ _ sc _ _ _ _ ) = sc == skinColor
filterByDead dead (Attr _ _ _ _ _ _ d _ _ _ ) = d == dead
filterByDeadPlace deadPlace (Attr _ _ _ _ _ _ _ dp _ _ ) = dp == deadPlace
filterByProfession profession (Attr _ _ _ _ _ _ _ _ pr _ ) = pr == profession
filterByDesease desease (Attr _ _ _ _ _ _ _ _ _ dss ) = desease `elem` dss
noFilter p = True

composeFilters [] p = True
composeFilters [f] p = f p
composeFilters (f:fs) p = f p && (composeFilters fs p)

checkIfBelongs (PsR a p) condition = 
     let belongs = condition a
           in if belongs then a:[] else []

showPersonsForLevel [] condition =  []
showPersonsForLevel (pR : psR) condition = checkIfBelongs (pR) condition ++ showPersonsForLevel  psR condition


showPersons (Ft [(l,(p:ps))]) condition = (showPersonsForLevel (p:ps) condition)
showPersons (Ft ( (l,(p:ps)) : lvls)) condition = (showPersonsForLevel (p:ps) condition) ++ (showPersons (Ft lvls) condition)

filterFamilyTreeNameSergioAge57 ft = showPersons ft (composeFilters ((filterByName "Sergio"):(filterByAge 57):[]))





