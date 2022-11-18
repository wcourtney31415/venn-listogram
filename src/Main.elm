module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html exposing (Html)
import List.Extra as ListE exposing (..)


main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


type alias Model =
    { textboxA : String
    , textboxB : String
    }


init : Model
init =
    { textboxA = ""
    , textboxB = ""
    }


type Msg
    = UpdateTextboxA String
    | UpdateTextboxB String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateTextboxA newText ->
            { model | textboxA = newText }

        UpdateTextboxB newText ->
            { model | textboxB = newText }


grey : Int -> Color
grey x =
    rgb255 x x x


view : Model -> Html Msg
view model =
    let
        listA =
            ListE.unique <| List.filter (\y -> not <| y == "") <| List.map String.trim <| List.map String.toUpper <| String.split "\n" model.textboxA

        listB =
            ListE.unique <| List.filter (\y -> not <| y == "") <| List.map String.trim <| List.map String.toUpper <| String.split "\n" model.textboxB

        part =
            List.partition (\x -> List.member x listB) listA

        common =
            ListE.unique <| List.reverse <| Tuple.first part

        uncommonA =
            ListE.unique <| List.reverse <| Tuple.second part

        uncommonB =
            ListE.unique <| List.reverse <| Tuple.second <| List.partition (\x -> List.member x listA) listB
    in
    Element.layoutWith
        { options =
            [ focusStyle
                { borderColor = Maybe.Nothing
                , backgroundColor = Maybe.Nothing
                , shadow = Maybe.Nothing
                }
            ]
        }
        [ Background.color <| grey 255, padding 35 ]
    <|
        Element.column
            [ centerX
            , centerY
            , spacing 40
            , width fill
            , height fill
            ]
            [ Element.el [ Font.size 30, Font.bold, centerX ] <| text "Venn Listogram"
            , Element.row [ spacing 50, width fill, height fill ]
                [ textBox UpdateTextboxA model.textboxA "List A" (rgb255 204 255 204) (List.length listA)
                , textBox UpdateTextboxB model.textboxB "List B" (rgb255 204 255 204) (List.length listB)
                , listBox uncommonA "Only in List A" (rgb255 255 204 204)
                , listBox common "In Both Lists" (rgb255 204 204 255)
                , listBox uncommonB "Only in List B" (rgb255 204 230 255)
                ]
            ]


listBox : List String -> String -> Color -> Element Msg
listBox myList myTitle myColor =
    let
        listWithLineBreaks =
            List.intersperse "\n" myList

        listAsString =
            List.foldl (++) "" listWithLineBreaks
        listLength = List.length myList
    in
    textBox
        (\_ -> UpdateTextboxA "")
        listAsString
        myTitle
        myColor
        listLength


textBox : (String -> msg) -> String -> String -> Color -> Int -> Element msg
textBox onChange value label myColor len =
    Input.multiline
        [ height fill
        , scrollbarY
        , Background.color myColor
        , Border.shadow
            { blur = 10
            , size = 1
            , offset = ( 4, 2 )
            , color = grey 150
            }
        , Border.rounded 20
        , alpha 0.9
        ]
        { onChange = onChange
        , text = value
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [ Font.bold
                , paddingEach
                    { top = 0
                    , right = 0
                    , left = 0
                    , bottom = 10
                    }
                , centerX
                ]
            <|
                text <|
                    label
                        ++ "     Qty: "
                        ++ String.fromInt len
        , spellcheck = False
        }


red : Color
red =
    rgb 1 0 0


green : Color
green =
    rgb 0 1 0


blue : Color
blue =
    rgb 0 0 1


purple : Color
purple =
    rgb 1 0 1
