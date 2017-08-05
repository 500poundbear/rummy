module ObjectsTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)

import List exposing (..)
import Objects exposing (..)

suite : Test
suite =
    describe "The Objects module"
        [ describe "Card"
            [ test "can return card's value" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        res = List.map cardValue cards
                        expectedResults = [100, 1, 10, 5]
                    in
                        Expect.equal res expectedResults
            , test "returns whether two cards are of the same suit" <|
                \_ ->
                    let
                        cardsA =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        cardsB =
                            [ Blue 1
                            , Red 10
                            , Green 1
                            , Yellow 3
                            ]
                        res = List.map2 sameSuit cardsA cardsB
                        expectedResults = [True, True, False, False]
                    in
                        Expect.equal res expectedResults
            ]
        , describe "Cards"
            [ test "can return first card" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        res = firstCard cards
                        expectedResults = Just <| Blue 100
                    in
                        Expect.equal res expectedResults
            , test "can return last card" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        res = lastCard cards
                        expectedResults = Just <| Green 5
                    in
                        Expect.equal res expectedResults


            , test "can return nthed card" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            , Yellow 15
                            ]
                        res = nthCard 3 cards
                        expectedResults = Just <| Yellow 5
                    in
                        Expect.notEqual res expectedResults

            ]
        ]
