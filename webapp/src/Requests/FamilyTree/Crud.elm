module Requests.FamilyTree.Crud exposing (..)


import Http exposing (..)
import Model exposing (..)
import HttpHelper exposing (..)
import Dict
import List exposing (..)
import Dto.FamilyTreeDto exposing (..)
import String



createFamilyTree : Model -> Cmd Msg
createFamilyTree model =
  Http.request
    { method = "POST"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ model.ftData.name
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
    , url = api ++ "family-tree/name/" ++ model.ftData.name
    , body = Http.emptyBody
    , expect = Http.expectJson (ResponseGetFTId "Tree does not exist: ") responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


dateToCorrectDate : String -> String
dateToCorrectDate s =
  if String.isEmpty s 
    then ""
    else String.join "/" (List.reverse (String.split "-" s))


mapDeseaseToNumber : String -> String
mapDeseaseToNumber s = 
  case s of
      "Cancer" -> "0"
      "Diabetes" -> "1"
      "Leukemia" -> "2"
      _ -> ""
          

diseasesToString : List String -> String
diseasesToString l = "{" ++ (String.join "," (List.map mapDeseaseToNumber l)) ++ "}"
  


addPersonToLevel : Model -> Int -> Cmd Msg
addPersonToLevel model i = 
  Http.request
    { method = "POST"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ (String.fromInt model.ftData.id) ++ "/level"
    , body = formBody 
              [
                ("name", model.personForm.name),
                ("lastName", model.personForm.lastname),
                ("birthDate", dateToCorrectDate model.personForm.birthDate),
                ("deathDate", dateToCorrectDate model.personForm.deathDate),
                ("deathPlace", model.personForm.deathPlace),
                ("hairColor", model.personForm.hairColor),
                ("eyeColor", model.personForm.eyeColor),
                ("skinColor", model.personForm.skinColor),
                ("level", (String.fromInt i)),
                ("profession", model.personForm.profession),
                ("deseases", diseasesToString model.personForm.diseases)
              ]
    , expect = Http.expectJson (ResponseAddToLevel) responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }

addPersonAsParent : Model -> Int -> Int -> Cmd Msg
addPersonAsParent model level childId = 
  Http.request
    { method = "POST"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ (String.fromInt model.ftData.id) ++ "/parent/" ++ (String.fromInt childId)
    , body = formBody 
              [
                ("name", model.personForm.name),
                ("lastName", model.personForm.lastname),
                ("birthDate", dateToCorrectDate model.personForm.birthDate),
                ("deathDate", dateToCorrectDate model.personForm.deathDate),
                ("deathPlace", model.personForm.deathPlace),
                ("hairColor", model.personForm.hairColor),
                ("eyeColor", model.personForm.eyeColor),
                ("skinColor", model.personForm.skinColor),
                ("level", (String.fromInt level)),
                ("profession", model.personForm.profession),
                ("deseases", diseasesToString model.personForm.diseases)
              ]
    , expect = Http.expectJson (ResponseAddToLevel) responseIdListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }


buildQueryParams : List (String, String) -> String
buildQueryParams l = 
  let
      listOfKeyVal = List.map (\x -> (Tuple.first x) ++ "=" ++ (Tuple.second x)) (List.filter (\x -> not (String.isEmpty (Tuple.second x))) l)
  in String.join "&" listOfKeyVal
  

ifAge0Empty : String -> String
ifAge0Empty a = 
  if a == "0"
    then ""
    else a

queryPersons : Model -> Cmd Msg
queryPersons model = 
  Http.request
    { method = "GET"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/" ++ (String.fromInt model.ftData.id) ++ "/filter?" ++ buildQueryParams 
                                        [
                                          ("name", model.personForm.name),
                                          ("lastName", model.personForm.lastname),
                                          ("deathPlace", model.personForm.deathPlace),
                                          ("hairColor", model.personForm.hairColor),
                                          ("eyeColor", model.personForm.eyeColor),
                                          ("skinColor", model.personForm.skinColor),
                                          ("age", ifAge0Empty (String.fromInt model.personForm.age)),
                                          ("profession", model.personForm.profession),
                                          ("desease", mapDeseaseToNumber model.disease)
                                        ]
    , body = Http.emptyBody
    , expect = Http.expectJson (ResponseQueryPersons) personListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }



  
getRelativesInCommon : Model -> Cmd Msg
getRelativesInCommon model =
  Http.request
    { method = "Get"
    , headers = [(Http.header "Token" model.userLogin.token)]
    , url = api ++ "family-tree/common?"  ++ buildQueryParams 
                                        [
                                          ("name1", model.relativesInCommonData.name1),
                                          ("name2", model.relativesInCommonData.name2)
                                        ]
    , body = Http.emptyBody
    , expect = Http.expectJson (ResponseQueryPersons) personListDecoder
    , timeout = Nothing
    , tracker = Nothing
    }