module Snippets.FamilyTree exposing (..)

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

import Bootstrap.Accordion as Accordion
import Dto.FamilyTreeDto exposing (..)


ftView : Model -> List (Html Msg)
ftView model =
    [Grid.containerFluid []
        [
            Grid.row [Row.attrs [Spacing.p3]]
            [
                Grid.col [Col.textAlign Text.alignMdLeft]
                [
                  h1 [] [ text model.ftData.name ]  
                ],
                Grid.col [Col.textAlign Text.alignMdRight]
                [
                    Button.button [ Button.secondary, 
                                Button.attrs [onClick (Goto FamilyTreeQuery)] ]
                                [ text "Query this tree" ]
                ]
            ],
            Grid.row [Row.centerMd]
            [
                Grid.col [Col.textAlign Text.alignMdRight]
                [
                    Button.button [ Button.primary, 
                                    Button.attrs [onClick (OffsetLevel 1)] ]
                                    [ text "Go down" ]
                ],
                Grid.col [Col.textAlign Text.alignMdLeft]
                [
                    Button.button [ Button.primary, 
                                    Button.attrs [onClick (OffsetLevel -1)] ]
                                    [ text "Go up" ]
                ]
            ],
            Grid.row [Row.attrs [Spacing.p3]]
            [
                --left
                Grid.col [Col.textAlign Text.alignLgCenter]
                [
                    addToLvlButton model.ftData.currLevel,
                    lvlMsg model.ftData.left,
                    renderLevel model model.ftData.left
                ],
                --mid
                Grid.col [Col.textAlign Text.alignLgCenter]
                [
                    addToLvlButton (model.ftData.currLevel - 1),
                    lvlMsg model.ftData.mid,
                    renderLevel model model.ftData.mid
                ],
                --right
                Grid.col [Col.textAlign Text.alignLgCenter]
                [
                    addToLvlButton (model.ftData.currLevel - 2),
                    lvlMsg model.ftData.right,
                    renderLevel model model.ftData.right
                ]
            ]
        ]
    ]

addToLvlButton : Int -> Html Msg
addToLvlButton i = Button.button [ Button.primary, Button.attrs [onClick (ShowModal (CreateUserType i))] ] [ text "Add" ]

lvlMsg : Maybe ByLevel -> Html Msg
lvlMsg b = 
    case b of
        Nothing -> h5 [] [ text "Empty" ]
        Just bl -> h5 [] [ text ("Level " ++ String.fromInt bl.level)]

renderLevel : Model -> Maybe ByLevel -> Html Msg
renderLevel model b =
    ListGroup.ul
        (
            [
                case b of 
                    Nothing -> ListGroup.li [] []
                    Just bl -> ListGroup.li [] (List.map (\x -> renderPersonAccordion model x bl.level) bl.persons)
            ]
        )


renderPersonAccordion : Model -> PersonWithParents -> Int -> Html Msg
renderPersonAccordion model pWp i =
    ListGroup.ul 
        [
            ListGroup.li [] [
            Accordion.config AccordionMsg
                |> Accordion.withAnimation
                |> Accordion.cards
                    [ Accordion.card
                        { id = "personcard" ++ String.fromInt (pWp.person.id)
                        , options = []
                        , header =
                            Accordion.header [] <| Accordion.toggle [] [ text (pWp.person.name ++ " " ++ pWp.person.lastname) ]
                        , blocks =
                            [ Accordion.block []
                                [ Block.text [] 
                                    [
                                        ListGroup.ul
                                        [ 
                                            ListGroup.li [] 
                                            [
                                                text ("Age: " ++ (String.fromInt pWp.person.age))
                                            ]
                                            , ListGroup.li []
                                            [   
                                                text ("Birthday: " ++ pWp.person.birthDate)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Eye color: " ++ pWp.person.eyeColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Skin color: " ++ pWp.person.skinColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Hair color: " ++ pWp.person.hairColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Profession: " ++ pWp.person.profession)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Death place and date: " ++ pWp.person.deathPlace ++ ", " ++ pWp.person.deathDate)
                                            ]
                                            , ListGroup.li []
                                            [
                                                text ("Known diseases: " ++ (List.foldl (concatenateStringsWith ", ") "" pWp.person.diseases))
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        }
                    ]
                |> Accordion.view model.accordionState ]

            , ListGroup.li [] [text ("Parents: " ++ (stringOfParents pWp model.ftData.ft (i-1)))]
            , 
            (if List.length pWp.parents < 2 
                then ListGroup.li[] [Button.button [ Button.primary, Button.attrs [onClick (ShowModal (AddPersonAsParentType (i-1) pWp.person.id))] ] [ text "Add parent" ]]
                else ListGroup.li[] [])
        ]
    