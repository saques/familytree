module Snippets.FamilyTree exposing (..)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button


ftView : Model -> List (Html Msg)
ftView model =
    [ h1 [] [ text model.ftName ]
    , Grid.containerFluid []
        [
            Grid.row []
            [
                Grid.col []
                [

                ]
            ]
        ]
    ]