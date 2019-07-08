module Snippets.LoginModal exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.small
        |> Modal.h4 [] [ text "Sign in" ]
        |> Modal.body []
            [ Grid.containerFluid []
                [
                    Form.form []
                        [ Form.group []
                            [ Form.label [for "username"] [ text "User name"]
                            , Input.text [ Input.id "username", Input.onInput SetUsername ]
                            , Form.help [] [ text model.userLogin.userError ]
                            ]
                        , Form.group []
                            [ Form.label [for "mypwd"] [ text "Password"]
                            , Input.password [ Input.id "mypwd", Input.onInput SetPassword ]
                            ]
                        , Form.row []
                            [
                                Form.col []
                                [
                                    Button.button [ Button.secondary, Button.attrs [onClick CreateUser] ][ text "Sign in" ]
                                ],
                                Form.col []
                                [
                                    Button.button [ Button.primary, Button.attrs [onClick Login] ][ text "Log in" ]
                                ]
                            ]
                        ]       
                ] 
            
            ]
        |> Modal.view model.modalVisibility
