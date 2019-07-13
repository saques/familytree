module Requests.LoginAndRegister exposing (..) 


import Http exposing (..)
import Model exposing (..)
import HttpHelper exposing (..)
import Dict
import List exposing (foldl)

expectJWT : (Result Http.Error String -> msg) -> Http.Expect msg
expectJWT toMsg =
    Http.expectStringResponse toMsg <|
        \response ->
            case response of
                Http.BadUrl_ url ->
                    Err (Http.BadUrl url)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Ok body


createUser : Model -> Cmd Msg
createUser model = 
    Http.post
        {
            url = (api ++ "users/" ++ model.userLogin.username),
            body = formBody [("password", model.userLogin.password)],
            expect = expectJWT (ResponseLoginRegister "Error creating user")
        }

loginUser : Model -> Cmd Msg
loginUser model = 
    Http.post
        {
            url = (api ++ "users/login/" ++ model.userLogin.username),
            body = formBody [("password", model.userLogin.password)],
            expect = expectJWT (ResponseLoginRegister "Invalid username or password")
        }