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
import Bootstrap.Text as Text
import Bootstrap.Utilities.Spacing as Spacing

pageMain : Model -> List (Html Msg)
pageMain model =
    [Grid.containerFluid []
        [
            Grid.row[]
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
                [
                    h4 [] [ text "Welcome to FamilyTree!" ]
                ]
            ],
            Grid.row []
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
                [
                    h6 [] [ text model.globalError ]
                ]
            ],
            Grid.row []
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
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
                Grid.col[Col.textAlign Text.alignMdCenter]
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
            ],
              Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
                [
                   Button.button [ Button.secondary, 
                                Button.attrs [onClick (Goto RelativesInCommon)] ]
                                [ text "See relatives in common" ]
                ] 
            ]


        ]
    ]
    