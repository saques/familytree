module Snippets.Modal exposing (..)

import Model exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Bootstrap.Grid as Grid
import Bootstrap.Modal as Modal

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Bootstrap.Form.Checkbox as Checkbox

modalMessage : Model -> String
modalMessage m =
    case m.modalType of
        LoginType -> "Sign in or log in"
        CreateUserType level -> "Create user in " ++ String.fromInt level
        AddPersonAsParentType level childId -> "Add parent"

modal : Model -> Html Msg
modal model =
    Modal.config CloseModal
        |> Modal.large
        |> Modal.h5 [] [ text (modalMessage model) ]
        |> Modal.body []
            [ 
                case model.modalType of
                    LoginType -> loginForm model 
                    CreateUserType level -> createUserForm model level 
                    AddPersonAsParentType level childId -> addPersonAsParentForm model level childId
            ]
        |> Modal.view model.modalVisibility



loginForm : Model ->  Html Msg
loginForm model = 
    Grid.containerFluid []
    [
        Form.form []
        [ Form.group []
            [ Form.label [for "username"] [ text "User name"]
            , Input.text [ Input.id "username", Input.onInput SetUsername ]
            , Form.help [] [ text model.globalError ]
            ]
        , Form.group []
            [ Form.label [for "mypwd"] [ text "Password"]
            , Input.password [ Input.id "mypwd", Input.onInput SetPassword ]
            ]
        , Form.row []
            [
                Form.col [Col.textAlign Text.alignSmRight]
                [
                    Button.button [ Button.secondary, Button.attrs [onClick CreateUser] ][ text "Sign in" ]
                ],
                Form.col [Col.textAlign Text.alignSmLeft]
                [
                    Button.button [ Button.primary, Button.attrs [onClick Login] ][ text "Log in" ]
                ]
            ]
        ]
    ] 


personForm : Html Msg
personForm = 
    Form.row []
    [
        Form.col []
        [
            Form.group []
            [ Form.label [for "name"] [ text "Name"]
            , Input.text [ Input.id "name" , Input.onInput SetName ]
            ], 
            Form.group []
            [ Form.label [for "lastname"] [ text "Last name"]
            , Input.text [ Input.id "lastname", Input.onInput SetLastname]
            ], 
            Form.group []
            [ Form.label [for "birthDate"] [ text "Birth date"]
            , Input.date [ Input.id "birthDate" , Input.onInput SetBirthDate ]
            ],
            Form.group []
            [ Form.label [for "deathDate"] [ text "Death date"]
            , Input.date [ Input.id "deathDate", Input.onInput SetDeathdate ]
            ], 
            Form.group []
            [ Form.label [for "profession"] [ text "Profession"]
            , Input.text [ Input.id "profession", Input.onInput SetProfession ]
            ]
        ],
        Form.col []
        [
            Form.group []
            [ Form.label [for "eyeColor"] [ text "Eye color"]
            , Input.text [ Input.id "eyeColor", Input.onInput SetEyeColor]
            ], 
            Form.group []
            [ Form.label [for "hairColor"] [ text "Hair color"]
            , Input.text [ Input.id "hairColor", Input.onInput SetHairColor ]
            ], 
            Form.group []
            [ Form.label [for "skinColor"] [ text "Skin color"]
            , Input.text [ Input.id "skinColor", Input.onInput SetSkinColor ]
            ],
            Form.group []
            [ Form.label [for "deathPlace"] [ text "Death place"]
            , Input.text [ Input.id "deathPlace", Input.onInput SetDeathPlace  ]
            ],
            Form.group []
            [
                Checkbox.checkbox [ Checkbox.id "0", Checkbox.onCheck (SetDisease "Cancer") ] "Cancer"
                ,Checkbox.checkbox [ Checkbox.id "1", Checkbox.onCheck (SetDisease "Diabetes")  ] "Diabetes"
                ,Checkbox.checkbox [ Checkbox.id "2", Checkbox.onCheck (SetDisease "Leukemia")  ] "Leukemia"
            ]
        ]
    ]

createUserForm : Model -> Int -> Html Msg
createUserForm model level = 
    Grid.containerFluid []
    [
        Form.form []
        [

            personForm

            , Form.row []
            [
                Form.col [Col.textAlign Text.alignSmRight]
                [
                    Form.help [] [ text model.globalError ],
                    Button.button [ Button.secondary, Button.attrs [onClick (AddPersonToLevel level)] ][ text "Add person" ]
                ]
            ]
        ] 
    ] 

addPersonAsParentForm : Model -> Int -> Int -> Html Msg
addPersonAsParentForm model level childId = 
    Grid.containerFluid []
    [
        Form.form []
        [

            personForm

            , Form.row []
            [
                Form.col [Col.textAlign Text.alignSmRight]
                [
                    Form.help [] [ text model.globalError ],
                    Button.button [ Button.secondary, Button.attrs [onClick (AddPersonAsParent level childId)] ][ text "Add parent" ]
                ]
            ]
        ] 
    ] 
