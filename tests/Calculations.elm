module Calculations exposing (calcSuite, justMerten, killSuite)

import Expect
import Fuzz exposing (intRange)
import Main exposing (CanKill(..), bonus, canKill, totalBonus)
import Random exposing (maxInt)
import Test exposing (..)


justMerten : { attackingCount : number, basePower : number }
justMerten =
    { attackingCount = 0
    , basePower = 0
    }


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


killSuite : Test
killSuite =
    describe "can kill"
        [ test "weakest possible enemy" <|
            \_ ->
                canKill
                    [ { life = 1, blockers = 0 } ]
                    justMerten
                    |> Expect.equal Everyone
        , test "cant kill more enemy with more life than can do" <|
            \_ ->
                canKill
                    [ { life = 2, blockers = 0 } ]
                    justMerten
                    |> Expect.equal NoEnemies
        , test "can can only kill some enemies if haven't enough totes power as they have lifes" <|
            \_ ->
                canKill
                    [ { life = 1, blockers = 0 }
                    , { life = 1, blockers = 0 }
                    , { life = 1, blockers = 0 }
                    ]
                    { justMerten | attackingCount = 1 }
                    |> Expect.equal SomeEnemies
        , test "can kill if have s'many monsters can kill outright 1 mob for 1 enemy" <|
            \_ ->
                canKill
                    [ { life = 3, blockers = 0 }
                    , { life = 3, blockers = 0 }
                    , { life = 3, blockers = 0 }
                    ]
                    { justMerten | attackingCount = 3 }
                    |> Expect.equal Everyone
        ]
