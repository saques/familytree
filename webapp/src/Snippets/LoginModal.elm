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

import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text

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
                            , Form.help [] [ text model.globalError ]
                            ]
                        , Form.group []
                            [ Form.label [for "mypwd"] [ text "Password"]
                            , Input.password [ Input.id "mypwd", Input.onInput SetPassword ]
                            ]
                        , Form.row []
                            [
                                Form.col [Col.textAlign Text.alignSmRight]
                                [
                                    Button.button [ Button.secondary, Button.attrs [onClick CreateUser] ][ text "Sign in" ]
                                ],
                                Form.col [Col.textAlign Text.alignSmLeft]
                                [
                                    Button.button [ Button.primary, Button.attrs [onClick Login] ][ text "Log in" ]
                                ]
                            ]
                        ]       
                ] 
            
            ]
        |> Modal.view model.modalVisibility
