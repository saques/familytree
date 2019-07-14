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

import Dto.FamilyTreeDto exposing (..)

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
        token : String , 
        isLoggedIn : Bool
    }

setUsername : String -> UserLogin -> UserLogin
setUsername s u = {u | username = s}

setPassword : String -> UserLogin -> UserLogin
setPassword s u = {u | password = s}

setToken : String -> UserLogin -> UserLogin
setToken s u = {u | token = s, isLoggedIn = True}

isLoggedIn : Model -> Bool 
isLoggedIn model = model.userLogin.isLoggedIn

logOut : UserLogin -> UserLogin
logOut u = UserLogin u.username u.password "" False

type Page
    = Home
    | MainPage
    | NotFound
    | FamilyTree


type alias Flags =
    {}

type alias Model =
    { navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , userLogin : UserLogin
    , ftName : String
    , globalError : String
    , currentFamilyTree : Maybe FamilyTree
    }

type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal
    | SetUsername String
    | SetPassword String
    | CreateUser
    | Login
    | Logout
    | ResponseLoginRegister String (Result Http.Error (String))
    | SetFtName String
    | CreateFamilyTree
    | ResponseGetFTId String (Result Http.Error (List ResponseId))
    | ResponseGetFamilyTreeById (Result Http.Error FamilyTree)
    | Goto Page
    | LoadFamilyTree
