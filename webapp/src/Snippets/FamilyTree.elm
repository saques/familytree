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

import Bootstrap.Accordion as Accordion
import Dto.FamilyTreeDto exposing (..)


ftView : Model -> List (Html Msg)
ftView model =
    [ h1 [] [ text model.ftData.name ]
    , Grid.containerFluid []
        [
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
            Grid.row []
            [
                --left
                Grid.col []
                [
                    lvlMsg model.ftData.left,
                    renderLevel model model.ftData.left
                ],
                --mid
                Grid.col []
                [
                    lvlMsg model.ftData.mid,
                    renderLevel model model.ftData.mid
                ],
                --right
                Grid.col []
                [
                    lvlMsg model.ftData.right,
                    renderLevel model model.ftData.right
                ]
            ]
        ]
    ]

lvlMsg : Maybe ByLevel -> Html Msg
lvlMsg b = 
    case b of
        Nothing -> h4 [] [ text "Empty" ]
        Just bl -> h4 [] [ text ("Showing level " ++ String.fromInt bl.level)]

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
                                                h6 [] [ text "Age:" ],
                                                text (String.fromInt pWp.person.age)
                                            ]
                                            , ListGroup.li []
                                            [   
                                                h6 [] [ text "Birthday:"],
                                                text (pWp.person.birthDate)
                                            ]
                                            , ListGroup.li []
                                            [
                                                h6 [] [ text "Eye color:"],
                                                text (pWp.person.eyeColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                h6 [] [ text "Skin color:"],
                                                text (pWp.person.skinColor)
                                            ]
                                            , ListGroup.li []
                                            [
                                                h6 [] [ text "Hair color:"],
                                                text (pWp.person.hairColor)
                                            ]
                                        ]
                                    ]
                                ]
                            ]
                        }
                    ]
                |> Accordion.view model.accordionState ]

            , ListGroup.li [] [text ("Parents: " ++ (stringOfParents pWp model.ftData.ft (i-1)))]
        ]
    