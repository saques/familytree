module Requests.FamilyTree.Crud exposing (..)


import Http exposing (..)
import Model exposing (..)
import HttpHelper exposing (..)
import Dict
import List exposing (foldl)
import Dto.FamilyTreeDto exposing (..)



createFamilyTree : Model -> Cmd Msg
createFamilyTree model =
  Http.request
    { method = "POST"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ model.ftName
    , body = Http.emptyBody
    , expect = Http.expectJson (ResponseGetFTId "Could not create family tree: ") responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

getFamilyTreeById : Int -> Model -> Cmd Msg
getFamilyTreeById id model =
  Http.request
    { method = "GET"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ String.fromInt (id)
    , body = Http.emptyBody
    , expect = Http.expectJson ResponseGetFamilyTreeById familyTreeDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

getFamilyTreeByName :  Model -> Cmd Msg
getFamilyTreeByName model =
  Http.request
    { method = "GET"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/name/" ++ model.ftName
    , body = Http.emptyBody
    , expect = Http.expectJson (ResponseGetFTId "Tree does not exist: ") responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }



  

