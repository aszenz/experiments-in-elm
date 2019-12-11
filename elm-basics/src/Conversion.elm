module Conversion exposing (main)

import Browser
import Html exposing (Html, br, div, input, span, text)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Converter =
    { input : String
    , converterFunc : Float -> Float
    , changeMessager : String -> Msg
    , units : { from : String, to : String }
    }


type alias Model =
    { celciusConverter : Converter
    , cmConverter : Converter
    }


init : Model
init =
    { celciusConverter = Converter "" convertToFaherniet ChangeTemp { from = "C", to = "F" }
    , cmConverter = Converter "" convertToMeter ChangeLength { from = "CM", to = "M" }
    }



-- UPDATE


setNewInputValue : Converter -> String -> Converter
setNewInputValue converterType value =
    { converterType | input = value }


type Msg
    = ChangeTemp String
    | ChangeLength String


update : Msg -> Model -> Model
update msg model =
    case msg of
        ChangeTemp newInputTemp ->
            { model | celciusConverter = setNewInputValue model.celciusConverter newInputTemp }

        ChangeLength newInputLength ->
            { model | cmConverter = setNewInputValue model.cmConverter newInputLength }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ renderConvertedValue model.celciusConverter
        , br [] []
        , br [] []
        , renderConvertedValue model.cmConverter
        ]



-- LOGIC


convertToFaherniet : Float -> Float
convertToFaherniet inputCelsius =
    inputCelsius * 1.8 + 32


convertToMeter : Float -> Float
convertToMeter lengthCm =
    lengthCm / 100


renderConvertedValue : Converter -> Html Msg
renderConvertedValue converter =
    case String.toFloat converter.input of
        Just v ->
            viewConverter
                converter.input
                converter.changeMessager
                "blue"
                (v
                    |> converter.converterFunc
                    |> String.fromFloat
                )
                converter.units.from
                converter.units.to

        Nothing ->
            viewConverter converter.input converter.changeMessager "red" "???" converter.units.from converter.units.to


viewConverter : String -> (String -> Msg) -> String -> String -> String -> String -> Html Msg
viewConverter userInput onChange color convertedValue v1 v2 =
    span []
        [ input
            [ value userInput
            , onInput onChange
            , style "width" "40px"
            , style "border-color" color
            , style "border-width" "4px"
            ]
            []
        , text (v1 ++ "= ")
        , span [ style "color" color ] [ text convertedValue ]
        , text v2
        ]
