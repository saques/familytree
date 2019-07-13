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
    , expect = Http.expectJson ResponseCreateFamilyTree responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

