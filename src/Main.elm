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
            ++ [ div [] [ text "--- Successes"] ]
            ++ successView model.d6s model.d10s
            ++ [ div [] [ text "--- Tuples"] ]
            ++ tuplesView model.d6s model.d10s
        )


successView d6s d10s =
    let
        row =
            \succs prob ->
                div []
                    [ span
                        [ style "display" "inline-block", style "width" "150px" ]
                        [ text (String.fromInt succs) ]
                    , span
                        [ style "display" "inline-block", style "width" "3em", style "text-align" "right" ]
                        [ text (toPercent prob) ]
                    ]
    in
    List.indexedMap
        row
        (poolSuccesses d6s d10s)


tupleTests =
    [ ( "Pairs", \x -> x.a >= 2 )
    , ( "Triples" , \x -> x.a >= 3 )
    , ( "Quadruples" , \x -> x.a >= 4 )
    , ( "Quintuples" , \x -> x.a >= 5 )
    , ( "Sextuples" , \x -> x.a >= 6 )
    , ( "Septuples" , \x -> x.a >= 7 )
    ]


tuplesView d6s d10s =
    (tuplePercentages d6s d10s
        |> testPercentages
        |> List.map (\tuple ->
            [ div []
                [ span
                    [ style "display" "inline-block", style "width" "150px" ]
                    [ text (Tuple.first tuple) ]
                , span
                    [ style "display" "inline-block", style "width" "3em", style "text-align" "right" ]
                    [ text (toPercent (Tuple.second tuple)) ]
                ]
            ])
        |> List.concat)

testPercentages : List Outcome -> List ( String, Float )
testPercentages outcomes =
    List.map
        (\tup ->
            let (n, test) = tup
            in
            List.map (\o -> if test(o) then o.prob else 0) outcomes
                |> List.sum
                |> \x -> (n, x))
        tupleTests



tuplePercentages d6s d10s =
    poolSuccesses d6s d10s
        |> List.indexedMap (\succs prob -> tupleConfigs succs |> weight prob)
        |> List.concat


weight prob outcomes =
    List.map (\outcome -> { outcome | prob = outcome.prob * prob }) outcomes


toPercent : Float -> String
toPercent n =
    if (n >= 0.995) && n /= 1 then
        ">99%"

    else if ( n < 0.005 && n /= 0) then
        "<1%"

    else
        (n * 100) |> round |> String.fromInt |> (\s -> s ++ "%")
