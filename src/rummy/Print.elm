module Print exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Msg exposing (..)
import Objects exposing (..)

printCard : Card -> Html Msg
printCard card =
    let
        val = case card of
            Blue n -> ("blue", "B" ++ (toString n))
            Green n -> ("green", "G" ++ (toString n))
            Red n -> ("red", "R" ++ (toString n))
            Yellow n -> ("yellow", "Y" ++ (toString n))
        colour = Tuple.first val
        label = Tuple.second val
    in
        span [class "cardd", class colour] [text label]

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
        , h3 [] [text "Controls"]
        ]
