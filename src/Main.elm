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
    { myNum : Int
    , textboxA : String
    , textboxB : String
    }


init : Model
init =
    { myNum = 42
    , textboxA = ""
    , textboxB = ""
    }


type Msg
    = UpdateNum Int
    | UpdateTextboxA String
    | UpdateTextboxB String


update msg model =
    case msg of
        UpdateNum newNumber ->
            { model | myNum = newNumber }

        UpdateTextboxA newText ->
            { model | textboxA = newText }

        UpdateTextboxB newText ->
            { model | textboxB = newText }


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
            [ focusStyle { borderColor = Maybe.Nothing, backgroundColor = Maybe.Nothing, shadow = Maybe.Nothing } ]
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
                , textBox (\_ -> UpdateNum 4) (List.foldl (++) "" (List.intersperse "\n" uncommonA)) "Only in List A" (rgb255 255 204 204) (List.length uncommonA)
                , textBox (\_ -> UpdateNum 4) (List.foldl (++) "" (List.intersperse "\n" common)) "In Both Lists" (rgb255 204 204 255) (List.length common)
                , textBox (\_ -> UpdateNum 4) (List.foldl (++) "" (List.intersperse "\n" uncommonB)) "Only in List B" (rgb255 204 230 255) (List.length uncommonB)
                ]
            ]


textBox onChange value label myColor len =
    Input.multiline
        [ height fill
        , scrollbarY
        , Background.color myColor
        , Border.shadow { blur = 10, size = 1, offset = ( 4, 2 ), color = grey 150 }
        , Border.rounded 20
        , alpha 0.9
        ]
        { onChange = onChange
        , text = value
        , placeholder = Nothing
        , label =
            Input.labelAbove
                [Font.bold, paddingEach{top = 0, right = 0, left = 0, bottom = 10 }, centerX]
            <|
                text <|
                    label
                        ++ "     Qty: "
                        ++ String.fromInt len
        , spellcheck = False
        }


red =
    rgb 1 0 0


green =
    rgb 0 1 0


blue =
    rgb 0 0 1


purple =
    rgb 1 0 1


doubleIt x =
    x * 2


incrementIt x =
    x + 1


decrementIt x =
    x - 1


customButton attr fun myNum txt =
    Input.button
        ([ Background.color <| rgb 0 0 1
         , centerX
         , centerY
         , width <| px 140
         , paddingEach { bottom = 10, left = 10, right = 10, top = 10 }
         ]
            ++ attr
        )
        { onPress = Maybe.Just <| UpdateNum <| fun myNum
        , label = Element.el [ centerX, centerY ] <| text txt
        }
