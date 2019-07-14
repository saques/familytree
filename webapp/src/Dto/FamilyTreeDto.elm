module Dto.FamilyTreeDto exposing (..)



import Http exposing (..)
import HttpHelper exposing (..)
import Dict
import List exposing (foldl)
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


