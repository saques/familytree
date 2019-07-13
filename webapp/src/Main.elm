module Main exposing (main)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.Modal as Modal

import Http exposing (..)
import Json.Decode exposing (Decoder, map2, list, field, string, int)

import Model exposing (..)
import HttpHelper exposing (..)
import Utils exposing (..)

import Requests.LoginAndRegister exposing (..)
import Requests.FamilyTree.Crud exposing (..)

import Snippets.LoginModal exposing (..)
import Snippets.Navbar exposing (..)
import Snippets.Home exposing (..)
import Snippets.MainPage exposing (..)
import Snippets.FamilyTree exposing (..)


-------------------------------------------------------------------


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = ClickedLink
        , onUrlChange = UrlChange
        }

init : Flags -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        ( navState, navCmd ) =
            Navbar.initialState NavMsg

        ( model, urlCmd ) =
            urlUpdate url { navKey = key, 
                            navState = navState, 
                            page = Home, 
                            modalVisibility = Modal.hidden, 
                            userLogin = UserLogin "" "" "" False,
                            ftName = "",
                            globalError = "" }
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
    Navbar.subscriptions model.navState NavMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )
                 Browser.External href ->
                     ( model, Navigation.load href )

        UrlChange url ->
            --urlUpdate url model
            (model, Cmd.none)

        Goto page -> ({model | page = page}, Cmd.none)

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal ->
            ( { model | modalVisibility = Modal.shown }
            , Cmd.none
            )

        SetUsername e ->
            ( { model | userLogin = setUsername e model.userLogin }, Cmd.none)

        SetPassword e ->
            ( { model | userLogin = setPassword e model.userLogin }, Cmd.none)

        CreateUser -> (model, createUser model)

        ResponseLoginRegister message result ->
            case result of
                Ok t -> (
                      {model | userLogin = setToken t model.userLogin, 
                               modalVisibility = Modal.hidden,
                               globalError = "",
                               --Go to main page
                               page = MainPage }, Cmd.none)

                Err e -> (
                    { model | globalError = message }
                    , Cmd.none )

        Login -> (model, loginUser model)

        Logout -> ({model | userLogin = logOut model.userLogin, 
                            page = Home,
                            globalError = "" }, Cmd.none)

        SetFtName name -> ( { model | ftName = name }, Cmd.none)

        CreateFamilyTree -> (model, createFamilyTree model)

        ResponseCreateFamilyTree result ->
            case result of
                Ok id -> ( { model | page = FamilyTree,
                                     globalError = "" }, Cmd.none)
                Err _ -> ( { model | globalError = "Error creating family tree " ++ model.ftName }
                            , Cmd.none )
                    


        



urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url model of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Model-> Maybe Page
decode url model =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse (routeParser model)


routeParser : Model -> Parser (Page -> a) a
routeParser model =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map MainPage (s "main-page")
        ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Bootstrap"
    , body =
        [ div []
            [ navbar model
            , mainContent model
            , modal model
            ]
        ]
    }


mainContent : Model -> Html Msg
mainContent model =
    Grid.container [] <|
        case model.page of
            Home ->
                pageHome model

            MainPage ->
                pageMain model

            NotFound ->
                pageNotFound

            FamilyTree ->
                ftView model


pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]