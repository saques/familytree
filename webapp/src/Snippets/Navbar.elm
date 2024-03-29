module Snippets.Navbar exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button
import Bootstrap.Navbar as Navbar


gotoMainIfLoggedIn : Model -> Page
gotoMainIfLoggedIn model = 
    if isLoggedIn model then        
        MainPage
    else
        Home

navbar : Model -> Html Msg
navbar model =
    Navbar.config NavMsg
        |> Navbar.withAnimation
        |> Navbar.container
        |> Navbar.brand [ href "#", onClick (Goto (gotoMainIfLoggedIn model)) ] [ text "Elm Bootstrap" ]
        |> Navbar.customItems
            [ Navbar.formItem []
                [ if not (isLoggedIn model) then
                    Button.button
                        [ Button.success
                        , Button.attrs [onClick (ShowModal LoginType)]
                        ]
                        [ text "Log in"]
                  else
                    Button.button
                        [ Button.danger
                        , Button.attrs [onClick Logout]
                        ]
                        [ text "Log out"] 
                ]
            ]
        |> Navbar.view model.navState