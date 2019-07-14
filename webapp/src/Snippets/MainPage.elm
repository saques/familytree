module Snippets.MainPage exposing (..)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Html.Events exposing (onClick)
import Utils exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

pageMain : Model -> List (Html Msg)
pageMain model =
    [ h3 [] [ text "Welcome to Familytree!" ]
    , Grid.containerFluid[]
        [
            h4 [] [ text model.globalError ],
            Grid.row []
            [
                Grid.col[]
                [
                    Form.form []
                    [ Form.group []
                        [ Form.label [for "ftName"] [ text "Family tree name"]
                        , Input.text [ Input.id "ftName", Input.onInput SetFtName ]
                        ]
                    , Button.button [ 
                        Button.primary, 
                        Button.attrs [onClick CreateFamilyTree] ]
                        [ text "Create family tree" ]
                    ] 
                ] ,
                Grid.col[]
                [
                    Form.form []
                    [ Form.group []
                        [ Form.label [for "ftName"] [ text "Family tree name"]
                        , Input.text [ Input.id "ftName", Input.onInput SetFtName ]
                        ]
                    , Button.button [ 
                        Button.primary, 
                        Button.attrs [onClick LoadFamilyTree] ]
                        [ text "Load family tree" ]
                    ]
                ]
            ]
        ]
    ]