module Snippets.RelativesInCommon exposing (..)

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


relativesInCommon : Model -> List (Html Msg)
relativesInCommon model =
    [Grid.containerFluid []
        [
            Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdLeft]
                [
                  h1 [] [ text ("Select 2 family trees " ++ model.ftData.name) ]  
                ]
            ],
            Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
                [
                    Form.form []
                    [Form.row []
                        [
                            Form.col []
                            [
                                Form.group []
                                [ Form.label [for "ft1Name"] [ text "Family Tree 1 Name "]
                                , Input.text [ Input.id "ft1Name" , Input.onInput SetFt1Name ]
                                ]
                            ],
                            Form.col []
                            [
                                Form.group []
                                [ Form.label [for "ft2Name"] [ text "Family Tree 2 Name"]
                                , Input.text [ Input.id "ft2Name", Input.onInput SetFt2Name]
                                ]
                            ]
                        ]
                    ]
                ]       
            ],
              Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdCenter]
                [
                     Button.button [ Button.secondary, Button.attrs [onClick GetRelativesInCommon] ][ text "Search" ]
                ]
            ],

              Grid.row []
            [
                Grid.col[Col.textAlign Text.alignMdCenter]
                [
                    h6 [] [ text model.globalError ]
                ]
            ],

              Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdCenter]
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