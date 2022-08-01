module Main exposing (CanKill(..), Enemy, bonus, canKill, main, totalBonus)

import Browser
import Html exposing (Attribute, Html, button, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (for, id, pattern, style, type_, value)
import Html.Events exposing (onClick, onInput)


main : Program () Model Msg
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias Model =
    { attackingCount : Int
    , totalPower : Int
    , basePower : Int
    , enemies : List Enemy
    }


type alias Enemy =
    { blockers : Int
    , life : Int
    }


init : Model
init =
    { attackingCount = 0
    , totalPower = 0
    , basePower = 0
    , enemies = []
    }


defaultEnemy : Enemy
defaultEnemy =
    { blockers = 0, life = 40 }


type Msg
    = Increment
    | Decrement
    | UpdateBasePower String
    | AddEnemy
    | UpdateLife Int String
    | UpdateBlockers Int String


type CanKill
    = Everyone
    | SomeEnemies
    | NoEnemies


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | attackingCount = model.attackingCount + 1 }

        Decrement ->
            { model | attackingCount = model.attackingCount - 1 }

        UpdateBasePower input ->
            case String.toInt input of
                Just newPower ->
                    { model | basePower = newPower }

                Nothing ->
                    model

        AddEnemy ->
            { model | enemies = defaultEnemy :: model.enemies }

        UpdateLife i l ->
            case String.toInt l of
                Just newLife ->
                    { model | enemies = updateFieldAtIndex i newLife updateLife model.enemies }

                Nothing ->
                    model

        UpdateBlockers i b ->
            case String.toInt b of
                Just newBlockers ->
                    { model | enemies = updateFieldAtIndex i newBlockers updateBlockers model.enemies }

                Nothing ->
                    model


updateLife : Enemy -> Int -> Enemy
updateLife e l =
    { e | life = l }


updateBlockers : Enemy -> Int -> Enemy
updateBlockers e b =
    { e | blockers = b }


updateFieldAtIndex : Int -> b -> (a -> b -> a) -> List a -> List a
updateFieldAtIndex i val updateField =
    List.indexedMap
        (\j e ->
            if i /= j then
                e

            else
                updateField e val
        )


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.attackingCount) ]
        , button [ onClick Increment ] [ text "+" ]
        , chart model
        ]


enemies : { a | enemies : List Enemy } -> Html Msg
enemies model =
    model.enemies
        |> List.indexedMap enemy
        |> div []


enemy : Int -> Enemy -> Html Msg
enemy n e =
    let
        i =
            String.fromInt n
    in
    div []
        [ label [ for ("life-" ++ i) ] [ text "life" ]
        , input
            [ id ("life-" ++ i)
            , value (String.fromInt e.life)
            , onInput (UpdateLife n)
            ]
            []
        , label [ for ("blocker-" ++ i) ] [ text "blockers" ]
        , input
            [ id ("blocker-" ++ i)
            , value (String.fromInt e.blockers)
            , onInput (UpdateBlockers n)
            ]
            []
        ]


chart : Model -> Html Msg
chart model =
    div []
        [ label [ for "total-base-power" ] [ text "Total power of attacking w/o Merten' bonus" ]
        , input
            [ type_ "number"
            , id "total-base-power"
            , Html.Attributes.min "0"
            , pattern "[0-9]+"
            , value (String.fromInt model.basePower)
            , Html.Events.onInput UpdateBasePower
            ]
            []
        , button
            [ id "add-enemy"
            , onClick AddEnemy
            ]
            [ text "Add Enemy" ]
        , enemies model
        , table []
            [ thead []
                [ tr []
                    [ th [] [ text "Attacking/Blocking with" ]
                    , th [] [ text "Bonus +1/+1 per creature" ]
                    , th [] [ text "Total bonus +x/+x" ]
                    , th [] [ text "Total unblocked damage" ]
                    , th [] [ text "Can kill players" ]
                    ]
                ]
            , tbody []
                ((List.range 0 5
                    |> List.map
                        (\a ->
                            let
                                highlight =
                                    if a == model.attackingCount then
                                        [ style "background" "yellow" ]

                                    else
                                        []
                            in
                            row highlight a model
                        )
                 )
                    ++ (if model.attackingCount > 5 then
                            [ row
                                [ style "background" "yellow" ]
                                model.attackingCount
                                model
                            ]

                        else
                            []
                       )
                )
            ]
        ]


row : List (Attribute msg) -> Int -> Model -> Html msg
row highlight a model =
    tr highlight
        [ td [] [ text <| String.fromInt a ]
        , td [] [ text <| String.fromInt a ]
        , td [] [ text <| String.fromInt <| totalBonus a ]
        , td [] [ text <| String.fromInt <| totalPower a model.basePower ]
        , td []
            [ text <|
                case canKill model.enemies model of
                    SomeEnemies ->
                        "Some"

                    Everyone ->
                        "All"

                    NoEnemies ->
                        "None"
            ]
        ]


totalPower : Int -> Int -> Int
totalPower attackingCount basePower =
    totalBonus attackingCount + basePower


bonus : Int -> Int
bonus c =
    c


{-| calculate total bonus given to all other attackers.

    totalBonus 5 == 25

-}
totalBonus : Int -> Int
totalBonus c =
    c * c


canKill :
    List Enemy
    ->
        { r
            | attackingCount : Int
            , basePower : Int
        }
    -> CanKill
canKill es model =
    if
        let
            attackBonus =
                bonus model.attackingCount

            mertenCounts =
                if es |> List.any (.life >> (==) 1) then
                    1

                else
                    0
        in
        (es
            |> List.all
                (\e -> e.life <= max attackBonus mertenCounts)
        )
            && (((es
                    |> List.map .blockers
                    |> List.sum
                 )
                    + List.length es
                )
                    <= (model.attackingCount + mertenCounts)
               )
    then
        Everyone

    else if
        let
            lifes =
                es |> List.map .life
        in
        ((List.sum lifes
            > totalPower model.attackingCount model.basePower
         )
            || (List.length es > (model.attackingCount + 1))
        )
            && (lifes
                    |> List.any (\l -> l <= bonus model.attackingCount)
               )
    then
        SomeEnemies

    else
        NoEnemies
