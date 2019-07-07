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
import Snippets.LoginModal exposing (..)
import Snippets.Navbar exposing (..)
import HttpHelper exposing (..)
import Utils exposing (..)




userListDecoder : Decoder (List User)
userListDecoder =
  list userDecoder


userDecoder : Decoder User
userDecoder = 
    map2 User
        (field "name" string)
        (field "id" int)


getUsers : Cmd Msg
getUsers =
  Http.get
    { url = api ++ "users"
    , expect = Http.expectJson GotUser userListDecoder
    }

createUser : Model -> Cmd Msg
createUser model = 
    Http.post
        {
            url = (api ++ "users/" ++ model.userLogin.username),
            body = formBody [("password", model.userLogin.password)],
            expect = Http.expectWhatever ResponseCreateUser
        }



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
                            counter = 0, 
                            user = Nothing, 
                            userLogin = UserLogin "" "" "" }
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
            urlUpdate url model

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

        IncrementCounter ->
            ( { model | counter = model.counter + 1 }
            , Cmd.none
            )

        GetUser -> (model, getUsers)

        GotUser result ->
            case result of
                Ok newUser ->
                    ( { model | user = Just newUser }
                    , Cmd.none
                    )

                Err _ ->
                    (model, Cmd.none)

        SetUsername e ->
            ( { model | userLogin = UserLogin e model.userLogin.password "" }, Cmd.none)

        SetPassword e ->
            ( { model | userLogin = UserLogin model.userLogin.username e "" }, Cmd.none)

        CreateUser -> (model, createUser model)

        ResponseCreateUser result ->
            case result of
                Ok _ -> (model, Cmd.none)
                Err _ -> ({ model | userLogin = UserLogin model.userLogin.username model.userLogin.password "Error creating user"}, Cmd.none)
                    


        



urlUpdate : Url -> Model -> ( Model, Cmd Msg )
urlUpdate url model =
    case decode url of
        Nothing ->
            ( { model | page = NotFound }, Cmd.none )

        Just route ->
            ( { model | page = route }, Cmd.none )


decode : Url -> Maybe Page
decode url =
    { url | path = Maybe.withDefault "" url.fragment, fragment = Nothing }
    |> UrlParser.parse routeParser


routeParser : Parser (Page -> a) a
routeParser =
    UrlParser.oneOf
        [ UrlParser.map Home top
        , UrlParser.map GettingStarted (s "getting-started")
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

            GettingStarted ->
                pageGettingStarted model

            NotFound ->
                pageNotFound


pageHome : Model -> List (Html Msg)
pageHome model =
    [ h1 [] [ text "Home" ]
    , Grid.row []
        [ Grid.col []
            [ Card.config [ Card.outlinePrimary ]
                |> Card.headerH4 [] [ text "Getting started" ]
                |> Card.block []
                    [ Block.text [] [ text "Getting started is real easy. Just click the start button." ]
                    , Block.custom <|
                        Button.linkButton
                            [ Button.primary, Button.attrs [ href "#getting-started" ] ]
                            [ text "Start" ]
                    ]
                |> Card.view
            ]
        ]
    ]


pageGettingStarted : Model -> List (Html Msg)
pageGettingStarted model =
    [ h2 [] [ text "Getting started" ]
    , Button.button
        [ Button.success
        , Button.large
        , Button.block
        , Button.attrs [ onClick GetUser ]
        ]
        [ text "Increment the counter!"]
    , h3 [ class "text-center" ] [ text (showNames model.user) ]
    ]

pageNotFound : List (Html Msg)
pageNotFound =
    [ h1 [] [ text "Not found" ]
    , text "Sorry couldn't find that page"
    ]