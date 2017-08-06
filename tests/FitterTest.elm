module FitterTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, list, int, string)
import Test exposing (..)

import List exposing (..)
import Fitter exposing (..)
import Objects exposing (..)

suite : Test
suite =
    describe "The Fitter module"
        [ describe "formFromHand"
            [ test "can return best option [run] #1" <|
                \_ ->
                    let
                        card = Blue 7
                        hand = [Red 5]
                        table = [Run [Blue 4, Blue 5, Blue 6]]

                        res = (formFromHand card hand table)
                        expectedResults = Just
                            (( [Red 5]
                             , [Run [Blue 4, Blue 5, Blue 6, Blue 7]]
                             ), 7)
                    in
                        Expect.equal res expectedResults
            ]
        ]
