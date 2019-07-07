module Snippets.Navbar exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar

navbar : Model -> Html Msg
navbar model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#" ] [ text "Elm Bootstrap" ]
        |> Navbar.items
            [ Navbar.itemLink [ href "#getting-started" ] [ text "Getting started" ]
            ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ 
                  Button.button
                    [ Button.success
                    , Button.attrs [onClick ShowModal]
                    ]
                    [ text "Log in"]
                ]
            ]
        |> Navbar.view model.navState