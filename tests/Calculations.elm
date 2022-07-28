module Calculations exposing (calcSuite)

import Expect
import Fuzz exposing (bool, int, intRange)
import Main exposing (bonus, totalBonus)
import Random exposing (maxInt)
import Test exposing (..)


calcSuite : Test
calcSuite =
    describe "bonus"
        [ describe "per creature"
            [ fuzz (intRange 0 maxInt) "is the number of creatures attacking" <|
                \a -> bonus a |> Expect.equal a
            ]
        , describe "total"
            [ fuzz (intRange 0 (round <| sqrt <| toFloat maxInt)) "Total bonuses between all attacking creatures" <|
                \a -> totalBonus a |> Expect.equal (a * a)
            ]
        ]
