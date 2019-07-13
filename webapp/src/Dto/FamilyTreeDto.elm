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
