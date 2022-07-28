module Main exposing (bonus, main, totalBonus)

import Browser
import Debug exposing (todo)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
    Browser.sandbox { init = 0, update = update, view = view }


type Msg
    = Increment
    | Decrement


update msg model =
    case msg of
        Increment ->
            model + 1

        Decrement ->
            model - 1


view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model) ]
        , button [ onClick Increment ] [ text "+" ]
        ]


bonus : Int -> Int
bonus c =
    c


{-| calculate total bonus given to all other attackers.

    totalBonus 5 == 25

-}
totalBonus : Int -> Int
totalBonus c =
    c * c
