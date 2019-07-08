module Snippets.Home exposing (..)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Welcome" ]
                |> Card.block []
                    [ Block.text [] [ text "Welcome to FamilyTree, the fully functional app. Please log in." ]]
                |> Card.view
            ]
        ]
    ]