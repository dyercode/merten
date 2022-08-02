module Calculations exposing (calcSuite, justMerten, killSuite)

import Expect
import Fuzz exposing (Fuzzer, intRange)
import Main exposing (CanKill(..), Enemy, bonus, canKill, totalBonus)
import Random exposing (maxInt)
import Shrink
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
        , fuzz2
            (nonEmptyList (boundedLifeFuzzer maxInt))
            positiveInt
            ("can kill as many enemies as there are creatures,"
                ++ "provided no life exceeds the number of attacking creatures"
            )
          <|
            \l i ->
                let
                    size =
                        List.length l
                in
                canKill
                    (l |> List.map (\r -> { r | life = min r.life size }))
                    { justMerten | attackingCount = max i size }
                    |> Expect.equal Everyone
        , fuzz2 (nonEmptyList (boundedEnemyFuzzer 1 maxInt 2 maxInt))
            positiveInt
            ("cannot kill anyone if does not have more attackers than "
                ++ "any individuals blockers."
                ++ "(when they can also survive merten)"
            )
          <|
            \l aa ->
                canKill
                    l
                    { justMerten | attackingCount = min aa (List.length l) }
                    |> Expect.equal NoEnemies
        , fuzz3 positiveInt
            (intRange 1 maxInt)
            (intRange 1 (round <| sqrt <| toFloat maxInt))
            ("can kill an enemy (without merten) if have more "
                ++ "attackers than blockers and more remaining attack "
                ++ "than their life"
            )
          <|
            \blockers life attackers ->
                let
                    remainingPower =
                        bonus attackers * max 0 (attackers - blockers)
                in
                canKill
                    [ { blockers = blockers, life = life } ]
                    { justMerten | attackingCount = attackers }
                    |> Expect.equal
                        (if remainingPower >= life then
                            Everyone

                         else
                            NoEnemies
                        )
        ]


positiveInt : Fuzzer Int
positiveInt =
    Fuzz.intRange 0 maxInt


boundedLifeFuzzer : Int -> Fuzzer { blockers : Int, life : Int }
boundedLifeFuzzer maxLife =
    Fuzz.custom
        (Random.map2 Enemy (Random.int 0 0) (Random.int 0 maxLife))
        (\{ blockers, life } -> Shrink.map Enemy (Shrink.noShrink blockers) |> Shrink.andMap (Shrink.int life))


boundedEnemyFuzzer : Int -> Int -> Int -> Int -> Fuzzer { blockers : Int, life : Int }
boundedEnemyFuzzer minBlockers maxBlockers minLife maxLife =
    Fuzz.custom
        (Random.map2 Enemy (Random.int minBlockers maxBlockers) (Random.int minLife maxLife))
        (\{ blockers, life } -> Shrink.map Enemy (Shrink.int blockers) |> Shrink.andMap (Shrink.int life))


nonEmptyList : Fuzzer a -> Fuzzer (List a)
nonEmptyList fuzzer =
    Fuzz.map2 (::) fuzzer (Fuzz.list fuzzer)
