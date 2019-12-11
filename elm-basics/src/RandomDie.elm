module RandomDie exposing (main)

import Browser
import Html
import Html.Events
import Process
import Random
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { dieFace1 : Int
    , dieFace2 : Int
    , rolling : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 0 0 False, Cmd.none )


type Msg
    = Roll
    | GotRandomDigits ( Int, Int )
    | SetNewDieFaces Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( Model 0 0 True, rollDices )

        GotRandomDigits ( r1, r2 ) ->
            ( model
            , Process.sleep 1000
                |> Task.perform (always (SetNewDieFaces r1 r2))
            )

        SetNewDieFaces f1 f2 ->
            ( { model | dieFace1 = f1, dieFace2 = f2, rolling = False }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html.Html Msg
view model =
    Html.div []
        [ Html.h1 [] [ text "Random Die" ]
        , generateDieFace model.dieFace1 model.rolling "blue"
        , generateDieFace model.dieFace2 model.rolling "green"
        , Html.br [] []
        , Html.button [ Html.Events.onClick Roll ] [ Html.text "Roll anew" ]
        ]


rollDices : Cmd Msg
rollDices =
    Random.generate GotRandomDigits (Random.pair diceRoller diceRoller)


diceRoller : Random.Generator Int
diceRoller =
    Random.weighted ( 30, 1 )
        [ ( 20, 2 ), ( 5, 3 ), ( 20, 4 ), ( 10, 5 ), ( 30, 6 ) ]


generateDiagnolCircles : Int -> Int -> Int -> Int -> String -> List (Svg Msg)
generateDiagnolCircles start radius dist no color =
    List.map
        (\i ->
            circle
                [ cx (String.fromInt (start + dist * i))
                , cy (String.fromInt (start + dist * i))
                , r (String.fromInt radius)
                , fill color
                ]
                []
        )
        (List.range
            1
            no
        )


generateDieFace : Int -> Bool -> String -> Html.Html Msg
generateDieFace dieNumber rolling color =
    svg
        [ width "250"
        , height "250"
        , viewBox "-100 0 300 200"
        ]
        (rect
            [ x "10"
            , y "10"
            , width "100"
            , height "100"
            , rx "15"
            , ry "15"
            , fill color
            ]
            (showTransformation
                rolling
            )
            :: generateDiagnolCircles 20 6 11 dieNumber "red"
        )


showTransformation : Bool -> List (Svg Msg)
showTransformation isRolling =
    if isRolling then
        [ animateTransform
            [ attributeName "transform"
            , attributeType "XML"
            , type_ "rotate"
            , from "0 60 70"
            , to "360 60 70"
            , dur "1s"
            , repeatCount "indefinite"
            ]
            []
        ]

    else
        []
