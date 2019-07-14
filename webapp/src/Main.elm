module Main exposing (main)

import List
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

import Snippets.Modal exposing (..)
import Snippets.Navbar exposing (..)
import Snippets.Home exposing (..)
import Snippets.MainPage exposing (..)
import Snippets.FamilyTree exposing (..)

import Bootstrap.Accordion as Accordion


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
            urlUpdate url { accordionState = Accordion.initialState,
                            navKey = key, 
                            navState = navState, 
                            page = Home, 
                            modalVisibility = Modal.hidden,
                            modalType = LoginType,
                            userLogin = UserLogin "" "" "" False,
                            globalError = "",
                            ftData = ftDataInit,
                            personForm = emptyPerson}
    in
        ( model, Cmd.batch [ urlCmd, navCmd ] )


subscriptions : Model -> Sub Msg
subscriptions model =
        Sub.batch 
        [
            Navbar.subscriptions model.navState NavMsg,
            Accordion.subscriptions model.accordionState AccordionMsg
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of

        AccordionMsg state ->
            ( { model | accordionState = state } , Cmd.none )

        ClickedLink req ->
             case req of
                 Browser.Internal url ->
                     ( model, Navigation.pushUrl model.navKey <| Url.toString url )
                 Browser.External href ->
                     ( model, Navigation.load href )

        UrlChange url ->
            --urlUpdate url model
            (model, Cmd.none)

        Goto page -> ({model | ftData = ftDataInit, 
                               page = page, 
                               accordionState = Accordion.initialState}, Cmd.none)

        NavMsg state ->
            ( { model | navState = state }
            , Cmd.none
            )

        CloseModal ->
            ( { model | modalVisibility = Modal.hidden }
            , Cmd.none
            )

        ShowModal t ->
            ( { model | modalVisibility = Modal.shown, modalType = t }
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
                            accordionState = Accordion.initialState,
                            ftData = ftDataInit,
                            page = Home,
                            globalError = "" }, Cmd.none)

        SetFtName name -> ( { model | ftData = ftDataSetName name model.ftData }, Cmd.none)

        CreateFamilyTree -> (model, createFamilyTree model)

        ResponseGetFTId m result ->
            case result of
                Err e -> ( {model | globalError = m ++ model.ftData.name} , Cmd.none )
                Ok id -> 
                    case List.head id of
                        Nothing -> ( { model | globalError = "API returned nothing" }
                                    , Cmd.none )
                        Just responseId -> ( {model | ftData = ftDataSetId responseId.id model.ftData}, 
                            getFamilyTreeById responseId.id model)
                    
            

        ResponseGetFamilyTreeById result ->
            case result of
                --Should never reach first case
                Err e -> ( model, Cmd.none )
                Ok ft -> ( { model | ftData = (ftDataSetFT ft model.ftData),
                                               modalVisibility = Modal.hidden,
                                               page = FamilyTree,
                                               globalError = "" }, Cmd.none)

        LoadFamilyTree -> (model, getFamilyTreeByName model)

        OffsetLevel i -> ({model | ftData = offsetLevel i model.ftData}, Cmd.none)

        SetDeathPlace v -> ({model | personForm = setDeathPlace v model.personForm}, Cmd.none)

        SetHairColor v -> ({model | personForm = setHairColor v model.personForm}, Cmd.none)

        SetSkinColor v -> ({model | personForm = setSkinColor v model.personForm}, Cmd.none)
        
        SetLastname v -> ({model | personForm = setLastName v model.personForm}, Cmd.none)
        
        SetDeathdate v -> ({model | personForm = setDeathDate v model.personForm}, Cmd.none)
        
        SetEyeColor v -> ({model | personForm = setEyeColor v model.personForm}, Cmd.none)
        
        SetName v -> ({model | personForm = setName v model.personForm}, Cmd.none)
        
        SetProfession v -> ({model | personForm = setProfession v model.personForm}, Cmd.none)
        
        SetBirthDate v -> ({model | personForm = setBirthDate v model.personForm}, Cmd.none)

        SetDisease s b -> ({model | personForm = toggleDisease s b model.personForm}, Cmd.none)

        AddPersonToLevel level -> (model, addPersonToLevel model level) 

        ResponseAddToLevel result ->
            case result of
                Err e -> ( {model | globalError = "Error adding person to level"} , Cmd.none )
                Ok id -> 
                    case List.head id of
                        Nothing -> ( { model | globalError = "API returned nothing" }
                                    , Cmd.none )
                        Just responseId -> ( model, getFamilyTreeById model.ftData.id model) 




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