module Rummy exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import List exposing (..)

import Msg exposing (..)

type Card = Blue Int
          | Green Int
          | Red Int
          | Yellow Int

type alias Cards = List Card

type Clump = Group Cards
          | Run Cards

type alias Clumps = List Clump

type alias Hand = Cards
type alias Table = Clumps

{- length functions-}


printCard : Card -> Html Msg
printCard card =
    let
        val = case card of
            Blue n -> "B" ++ (toString n)
            Green n -> "G" ++ (toString n)
            Red n -> "R" ++ (toString n)
            Yellow n -> "Y" ++ (toString n)
    in
        span [class "cardd"] [text val]

printCards : Hand -> Html Msg
printCards hand =
    let
        cards = List.map printCard hand
    in
        div [] cards


printClump : Clump -> Html Msg
printClump clump =
    let
        res = case clump of
            Group cards -> (List.map printCard cards, "group")
            Run cards -> (List.map printCard cards, "run")
        results = Tuple.first res
        className = Tuple.second res
    in
        div [class className] results

printClumps : Clumps -> Html Msg
printClumps clumps =
    div [] (List.map printClump clumps)

printBoth : Hand -> Table -> Html Msg
printBoth hand table =
    let
        tableHtml = printClumps table
        handHtml = printCards hand
    in
        div []
        [ h1 [] [text "Table"]
        , tableHtml
        , h3 [] [text "Hand"]
        , handHtml
        ]
