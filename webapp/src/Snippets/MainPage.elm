module Snippets.MainPage exposing (..)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Button as Button
import Html.Events exposing (onClick)
import Utils exposing (..)

pageMain : Model -> List (Html Msg)
pageMain model =
    [ h3 [] [ text "Welcome to Familytree!" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick GetAuthSample ]
        ]
        [ text "Verify you are authenticated!"]
    , h4 [ class "text-center" ] [ text model.authenticatedMessage ]
    ]