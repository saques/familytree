module Model exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Browser.Navigation as Navigation
import Browser exposing (UrlRequest)
import Url exposing (Url)
import Url.Parser as UrlParser exposing ((</>), Parser, s, top)
import Bootstrap.Navbar as Navbar
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as Listgroup
import Bootstrap.Modal as Modal
import Bootstrap.Utilities.Spacing as Spacing
import Bootstrap.Utilities.Flex as Flex
import Http exposing (..)

api : String
api =
    "http://localhost:9000/api/"



type alias User = 
    {
        username : String ,
        id : Int
    }

type alias UserLogin = 
    {
        username : String ,
        password : String ,
        userError : String ,
        token : String , 
        isLoggedIn : Bool
    }

setUsername : String -> UserLogin -> UserLogin
setUsername s u = {u | username = s}

setPassword : String -> UserLogin -> UserLogin
setPassword s u = {u | password = s}

setUserError : String -> UserLogin -> UserLogin
setUserError s u = {u | userError = s}

setToken : String -> UserLogin -> UserLogin
setToken s u = {u | token = s, isLoggedIn = True, userError = ""}

isLoggedIn : Model -> Bool 
isLoggedIn model = model.userLogin.isLoggedIn

type Page
    = Home
    | GettingStarted
    | NotFound


type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , counter : Int
    , user : Maybe (List User)
    , userLogin : UserLogin
    }

type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal
    | IncrementCounter
    | GetUser
    | GotUser (Result Http.Error (List User))
    | SetUsername String
    | SetPassword String
    | CreateUser
    | Login
    | ResponseLoginRegister String (Result Http.Error (String))