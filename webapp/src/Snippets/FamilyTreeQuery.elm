module Snippets.FamilyTreeQuery exposing (..)

import Html exposing (..)
import Model exposing (..)
import Html.Attributes exposing (..)
import Bootstrap.Grid as Grid
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Button as Button
import Bootstrap.ListGroup as ListGroup
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Text as Text
import Html.Events exposing (onClick)
import Bootstrap.Card as Card
import Bootstrap.Card.Block as Block
import Bootstrap.Utilities.Spacing as Spacing
import Dto.FamilyTreeDto exposing (..)

import Bootstrap.Form as Form
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button

import Bootstrap.Form.Checkbox as Checkbox

import Bootstrap.Accordion as Accordion


ftQuery : Model -> List (Html Msg)
ftQuery model =
    [Grid.containerFluid []
        [
            Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdLeft]
                [
                  h1 [] [ text ("Query on " ++ model.ftData.name) ]  
                ],
                Grid.col [Col.textAlign Text.alignMdRight]
                [
                    Button.button [ Button.secondary, 
                                Button.attrs [onClick (Goto FamilyTreeView)] ]
                                [ text "Back to view" ]
                ]
            ],
            Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdCenter]
                [
                    queryForm model
                ]
            ]
        ]
    ]


queryPersonForm : Html Msg
queryPersonForm = 
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
            [ Form.label [for "profession"] [ text "Profession"]
            , Input.text [ Input.id "profession", Input.onInput SetProfession ]
            ],
            Form.group []
            [ Form.label [for "deathPlace"] [ text "Death place"]
            , Input.text [ Input.id "deathPlace", Input.onInput SetDeathPlace  ]
            ],
            Form.group []
            [ Form.label [for "age"] [ text "Age"]
            , Input.number [ Input.id "age", Input.onInput SetAge  ]
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
            [ Form.label [for "disease"] [ text "Disease"]
            , Input.text [ Input.id "disease", Input.onInput SetOneDisease ]
            ]
        ]
    ]

queryForm : Model -> Html Msg
queryForm model = 
    Grid.containerFluid []
    [
        Form.form []
        [
            queryPersonForm
            , Form.row []
            [
                Form.col [Col.textAlign Text.alignSmRight]
                [
                    Form.help [] [ text model.globalError ],
                    Button.button [ Button.secondary, Button.attrs [onClick QueryPersons] ][ text "Query" ]
                ]
            ],
            Form.row []
            [
                Form.col [Col.textAlign Text.alignXlCenter]
                [
                    ListGroup.ul (List.map (renderPersonAccordion model) model.queryPersons)
                ]
            ]
        ] 
    ] 


renderPersonAccordion : Model -> Person -> ListGroup.Item Msg
renderPersonAccordion model p =
            ListGroup.li [] [
            Accordion.config AccordionMsg
                |> Accordion.withAnimation
                |> Accordion.cards
                    [ Accordion.card
                        { id = "personindivcard" ++ String.fromInt (p.id)
                        , options = []
                        , header =
                            Accordion.header [] <| Accordion.toggle [] [ text (p.name ++ " " ++ p.lastname) ]
                        , blocks =
                            [ Accordion.block []
                                [ Block.text [] 
                                    [
                                        ListGroup.ul
                                        [ 
                                            ListGroup.li [] 
                                            [
                                                text ("Age: " ++ (String.fromInt p.age))
                                            ]
                                            , ListGroup.li []
                                            [   
                                                text ("Birthday: " ++ p.birthDate)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Eye color: " ++ p.eyeColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Skin color: " ++ p.skinColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Hair color: " ++ p.hairColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Profession: " ++ p.profession)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Death place and date: " ++ p.deathPlace ++ ", " ++ p.deathDate)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Known diseases: " ++ (List.foldl (concatenateStringsWith ", ") "" p.diseases))
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        }
                    ]
                |> Accordion.view model.accordionState ]