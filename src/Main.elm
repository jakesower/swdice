module Main exposing (main)

import Browser
import Html exposing (div, input, label, span, text)
import Html.Attributes exposing (for, style)
import Html.Events exposing (onInput)
import Probabilities exposing (..)


main =
    Browser.sandbox { init = { d6s = 0, d10s = 0 }, update = update, view = view }


type Msg
    = SetD6s String
    | SetD10s String


update msg model =
    case msg of
        SetD6s s ->
            { model | d6s = Maybe.withDefault 0 <| String.toInt s }

        SetD10s s ->
            { model | d10s = Maybe.withDefault 0 <| String.toInt s }


view model =
    div []
        ([ div []
            [ label [ for "d6s" ] [ text "d6s" ]
            , input [ onInput SetD6s ] []
            , label [ for "d10s" ] [ text "d10s" ]
            , input [ onInput SetD10s ] []
            ]
         ]
            ++ successView model.d6s model.d10s
        )


successView d6s d10s =
    let
        row =
            \succs prob ->
                div []
                    [ span [ style "padding-right" "40px" ] [ text (String.fromInt succs) ]
                    , span [] [ text (toPercent prob) ]
                    ]
    in
    List.indexedMap
        row
        (poolSuccesses d6s d10s)


toPercent : Float -> String
toPercent n =
    (n * 1000) |> round |> toFloat |> (\m -> m / 10) |> String.fromFloat |> (\s -> s ++ "%")
