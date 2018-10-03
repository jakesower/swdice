module Probabilities exposing (poolSuccesses, tupleConfigs, Outcome)

import List exposing (head, tail)
import List.Extra exposing (last, zip)


type alias Outcome =
    { prob : Float
    , a : Int
    , b : Int
    , c : Int
    , d : Int
    }


poolSuccesses : Int -> Int -> List Float
poolSuccesses d6s d10s =
    if d6s == 0 && d10s == 0 then
        [ 1.0 ]

    else if d6s == 0 then
        applySuccessStep (poolSuccesses 0 (d10s - 1)) (4 / 10)

    else
        applySuccessStep (poolSuccesses (d6s - 1) d10s) (4 / 6)


applySuccessStep list succ =
    let
        pairs =
            zip list (Maybe.withDefault [] (tail list))

        fail =
            1 - succ
    in
    [ Maybe.withDefault 0.0 (head list) * fail ]
        ++ List.map (\( fst, snd ) -> (fst * succ) + (snd * fail)) pairs
        ++ [ Maybe.withDefault 0.0 (last list) * succ ]



tupleConfigs succs =
    if succs == 0 then
        [ { prob = 1, a = 0, b = 0, c = 0, d = 0 } ]
    else
        tupleConfigs (succs - 1)
            |> List.map weightedOutcomes
            |> List.foldr mergeEquivalents []


weightedOutcomes : Outcome -> List Outcome
weightedOutcomes outcome =
    let
            aWeight =
                if outcome.a == outcome.d then
                    1

                else if outcome.a == outcome.c then
                    0.75

                else if outcome.a == outcome.b then
                    0.5

                else
                    0.25

            bWeight =
                if outcome.b == outcome.d then
                    0.75

                else if outcome.b == outcome.c then
                    0.5

                else
                    0.25

            cWeight =
                if outcome.c == outcome.d then
                    0.5

                else
                    0.25
        in
        List.concat
            [ [{ outcome | prob = outcome.prob * aWeight, a = outcome.a + 1 }]
            , if outcome.a == outcome.b then [] else [{ outcome | prob = outcome.prob * bWeight, b = outcome.b + 1 }]
            , if outcome.b == outcome.c then [] else [{ outcome | prob = outcome.prob * cWeight, c = outcome.c + 1 }]
            , if outcome.c == outcome.d then [] else [{ outcome | prob = outcome.prob * 0.25, d = outcome.d + 1 }]
            ]

-- x and y are already sorted
mergeEquivalents xs ys =
    case (xs, ys) of
        ([], []) ->
            []
        (x::_, []) ->
            xs
        ([], y::_) ->
            ys
        (x::xxs, y::yys) ->
            case (compareOutcomes x y) of
                GT ->
                    x :: mergeEquivalents xxs ys
                LT ->
                    y :: mergeEquivalents xs yys
                EQ ->
                    { x | prob = x.prob + y.prob } :: (mergeEquivalents xxs yys)


compareOutcomes x y =
    if x.a /= y.a then (compare x.a y.a)
        else if x.b /= y.b then (compare x.b y.b)
        else if x.c /= y.c then (compare x.c y.c)
        else if x.d /= y.d then (compare x.d y.d)
        else EQ
