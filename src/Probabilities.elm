module Probabilities exposing (poolSuccesses)

import List exposing (head, tail)
import List.Extra exposing (last, zip)


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
