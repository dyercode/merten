module Main exposing (bonus, main, totalBonus)

import Browser
import Debug exposing (todo)
import Html exposing (Attribute, Html, button, div, input, label, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (for, id, pattern, style, type_, value)
import Html.Events exposing (onClick, onSubmit)


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
        |> List.map enemy
        |> div []


enemy : Enemy -> Html Msg
enemy e =
    div []
        [ label [ for "life-id" ] [ text "life" ]
        , input [ id "life-id", value (String.fromInt e.life) ] []
        , label [ for "blocker-id" ] [ text "blockers" ]
        , input [ id "blocker-id", value (String.fromInt e.blockers) ] []
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
