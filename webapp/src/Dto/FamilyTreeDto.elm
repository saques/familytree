module Dto.FamilyTreeDto exposing (..)



import Http exposing (..)
import HttpHelper exposing (..)
import Dict
import List exposing (..)
import Json.Decode as D

type alias ResponseId = 
    { id : Int
    }

responseIdListDecoder : D.Decoder (List ResponseId)
responseIdListDecoder =
  D.list responseIdDecoder

responseIdDecoder : D.Decoder ResponseId
responseIdDecoder =
    D.map
        ResponseId
        (D.field "id" D.int)


type alias FamilyTreeData = 
    {
        name : String,
        id : Int
    }

familyTreeDataListDecoder : D.Decoder (List FamilyTreeData)
familyTreeDataListDecoder =
  D.list familyTreeDataDecoder

familyTreeDataDecoder : D.Decoder FamilyTreeData
familyTreeDataDecoder =
    D.map2
        FamilyTreeData
        (D.field "name" D.string)
        (D.field "id" D.int)


type alias FamilyTree =
    { lvls : List ByLevel
    }

ftMinLevel : FamilyTree -> Int
ftMinLevel ft = 
    case List.minimum (List.map (\x -> x.level) ft.lvls) of
        Nothing -> -1
        Just i -> i

ftMaxLevel : FamilyTree -> Int
ftMaxLevel ft = 
    case List.maximum (List.map (\x -> x.level) ft.lvls) of
        Nothing -> -1
        Just i -> i

ftGetByLevel : Int -> FamilyTree -> Maybe ByLevel
ftGetByLevel i f = List.head (List.filter (\x -> x.level == i) f.lvls)

ftGetByLevelMaybe : Int -> Maybe FamilyTree -> Maybe ByLevel
ftGetByLevelMaybe i f = 
    case f of
        Nothing -> Nothing
        Just ft ->
            List.head (List.filter (\x -> x.level == i) ft.lvls)


getPersonByIdByLevel : Int -> Int -> Maybe FamilyTree -> List Person
getPersonByIdByLevel id level f =
    case f of
        Nothing -> []
        Just ft ->
            let
                maybeLevel = ftGetByLevel level ft
            in case maybeLevel of
                Nothing -> []
                Just byLevel -> 
                    List.map (\x -> x.person) (List.filter (\x -> x.person.id == id) byLevel.persons)
            

personWithParentsToListOfParents : PersonWithParents -> Maybe FamilyTree -> Int -> List Person
personWithParentsToListOfParents pWp ft searchLevel =
    List.concat(List.foldl (::) [] (List.map (\x -> getPersonByIdByLevel x searchLevel ft) pWp.parents) )


concatenateStringsWithSpace : String -> String -> String
concatenateStringsWithSpace s1 s2 = s1 ++ " " ++ s2

stringOfParents : PersonWithParents -> Maybe FamilyTree -> Int -> String
stringOfParents pWp ft i = 
    List.foldl  concatenateStringsWithSpace "" 
        (List.map (\x -> concatenateStringsWithSpace x.name x.lastname) 
                   (personWithParentsToListOfParents pWp ft i))



familyTreeDecoder : D.Decoder FamilyTree
familyTreeDecoder =
    D.map
        FamilyTree
        (D.field "lvls" (D.list byLevelDecoder))

type alias ByLevel =
    { persons : List PersonWithParents
    , level : Int
    }

byLevelDecoder : D.Decoder ByLevel
byLevelDecoder = 
    D.map2
        ByLevel
        (D.field "persons" (D.list personWithParentsDecoder))
        (D.field "level" D.int)

type alias PersonWithParents =
    { person : Person
    , parents : List Int
    }

personWithParentsDecoder : D.Decoder PersonWithParents
personWithParentsDecoder =
    D.map2
        PersonWithParents
        (D.field "person" personDecoder)
        (D.field "parents" (D.list D.int))

type alias Person =
    { hairColor : String
    , skinColor : String
    , lastname : String
    , age : Int
    , eyeColor : String
    , name : String
    , id : Int
    , birthDate : String
    }

--Todo: max is 8, enlargement needed
personDecoder : D.Decoder Person
personDecoder = 
    D.map8
        Person
        (D.field "hairColor" D.string)
        (D.field "skinColor" D.string)
        (D.field "lastname" D.string)
        (D.field "age" D.int)
        (D.field "eyeColor" D.string)
        (D.field "name" D.string)
        (D.field "id" D.int)
        (D.field "birthDate" D.string)


