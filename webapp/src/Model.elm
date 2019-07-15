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
import Bootstrap.Accordion as Accordion
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

type alias FTData =
    {
        name : String,
        id : Int,
        ft : Maybe FamilyTree,
        currLevel : Int,
        left : Maybe ByLevel,
        mid : Maybe ByLevel,
        right : Maybe ByLevel
    }

ftDataInit : FTData
ftDataInit = FTData "" 0 Nothing 0 Nothing Nothing Nothing

ftDataSetName : String -> FTData -> FTData
ftDataSetName s d = {d | name = s}

ftDataSetId : Int -> FTData -> FTData
ftDataSetId i d = {d | id = i}

ftDataSetFT : FamilyTree -> FTData -> FTData
ftDataSetFT f d = {d | ft = Just f,
                       left = ftGetByLevelMaybe d.currLevel (Just f),
                       mid = ftGetByLevelMaybe (d.currLevel-1) (Just f),
                       right = ftGetByLevelMaybe (d.currLevel-2) (Just f)}

offsetLevel : Int -> FTData -> FTData
offsetLevel i d = {d | currLevel = d.currLevel + i,
                       left = ftGetByLevelMaybe (d.currLevel+i) d.ft,
                       mid = ftGetByLevelMaybe (d.currLevel+i-1) d.ft,
                       right = ftGetByLevelMaybe (d.currLevel+i-2) d.ft}



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

emptyPerson = Person "" "" "" "" "" [] 0 "" "" 0 "" ""


type ModalType 
    = LoginType
    | CreateUserType Int
    | AddPersonAsParentType Int Int


type Page
    = Home
    | MainPage
    | NotFound
    | FamilyTreeView
    | FamilyTreeQuery


type alias Flags =
    {}

type alias Model =
    { accordionState : Accordion.State
    , navKey : Navigation.Key
    , page : Page
    , navState : Navbar.State
    , modalVisibility : Modal.Visibility
    , modalType : ModalType
    , userLogin : UserLogin
    , globalError : String
    , ftData : FTData
    , personForm : Person
    , disease : String
    , queryPersons : List Person
    }

type Msg
    = UrlChange Url
    | ClickedLink UrlRequest
    | AccordionMsg Accordion.State
    | NavMsg Navbar.State
    | CloseModal
    | ShowModal ModalType
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
    | OffsetLevel Int
    | SetDeathPlace String
    | SetHairColor String
    | SetSkinColor String
    | SetLastname String
    | SetDeathdate String
    | SetDisease String Bool
    | SetEyeColor String
    | SetName String
    | SetProfession String
    | SetBirthDate String
    | AddPersonToLevel Int
    | ResponseAddToLevel (Result Http.Error (List ResponseId))
    | AddPersonAsParent Int Int
    | SetOneDisease String
    | SetAge String
    | QueryPersons
    | ResponseQueryPersons (Result Http.Error (List Person))

toggleDisease : String -> Bool -> Person -> Person
toggleDisease s b p = 
    if not (List.member s p.diseases) then
        {p | diseases = (s :: p.diseases)}
    else 
        {p | diseases = List.filter (\x -> not (x == s)) p.diseases}

setDeathPlace : String -> Person -> Person
setDeathPlace s p = {p | deathPlace = s}

setHairColor : String -> Person -> Person
setHairColor s p = {p | hairColor = s}

setSkinColor : String -> Person -> Person
setSkinColor s p = {p | skinColor = s}

setLastName : String -> Person -> Person
setLastName s p = {p | lastname = s}

setDeathDate : String -> Person -> Person
setDeathDate s p = {p | deathDate = s}

setEyeColor : String -> Person -> Person
setEyeColor s p = {p | eyeColor = s}

setName : String -> Person -> Person
setName s p = {p | name = s}

setAge : String -> Person -> Person
setAge s p = {p | age = Maybe.withDefault (-1) (String.toInt s)}

setProfession : String -> Person -> Person
setProfession s p = {p | profession = s}

setBirthDate : String -> Person -> Person
setBirthDate s p = {p | birthDate = s}

