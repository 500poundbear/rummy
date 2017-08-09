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
        [ describe "formClump works"
            [ test "forms the right run from draft of 2 (from hand)" <|
                \_ ->
                    let
                        clump = Run [Red 5, Red 6]
                        hand = [Blue 5, Red 7, Yellow 10]
                        table = []
                        res = formClump clump hand table 11
                        expectedResults =
                            [
                            ((Run [Red 5, Red 6, Red 7], ([Blue 5, Yellow 10], [])), 18)
                            ]
                    in
                        Expect.equal res expectedResults
            , test "forms the right run from draft of 1 (from hand)" <|
                \_ ->
                    let
                        clump = Run [Red 5]
                        hand = [Blue 5, Red 6, Red 7, Yellow 10]
                        table = []
                        res = formClump clump hand table 5
                        expectedResults =
                            [
                            ((Run [Red 5, Red 6, Red 7], ([Blue 5, Yellow 10], [])), 18)
                            ]
                    in
                        Expect.equal res expectedResults
            , test "doesn't form any run appropriately (from hand)" <|
                \_ ->
                    let
                        clump = Run [Red 5]
                        hand = [Blue 5, Yellow 10]
                        table = []
                        res = formClump clump hand table 5
                        expectedResults = []
                    in
                        Expect.equal res expectedResults
            , test "forms runs appropriately (from table)" <|
                \_ ->
                    let
                        clump = Run [Red 5]
                        hand = [Blue 5, Yellow 10]
                        table =
                            [ Group [Green 6, Blue 6, Yellow 6, Red 6]
                            , Group [Green 7, Blue 7, Yellow 7, Red 7]
                            ]
                        res = formClump clump hand table 5
                        expectedResults =
                            [ (((Run [Red 5, Red 6, Red 7]
                                , ([Blue 5, Yellow 10],
                                    [ Group [Green 6, Blue 6, Yellow 6]
                                    , Group [Green 7, Blue 7, Yellow 7]
                                    ]
                                )),
                                5))
                            ]
                    in
                        Expect.equal res expectedResults
            , test "forms runs appropriately (from both)" <|
                \_ ->
                    let
                        clump = Run [Red 5]
                        hand = [Blue 5, Yellow 10]
                        table = []
                        res = formClump clump hand table 5
                        expectedResults = []
                    in
                        Expect.equal res expectedResults
            ]
        , describe "fitGroup & fitGroupRunner"
            [ test "fitGroup won't add card if group is full" <|
                \_ ->
                    let
                        card = Red 5
                        clump = Group [Red 5, Blue 5, Yellow 5, Green 5]
                        res = fitGroup card clump
                        expectedResults = Nothing
                    in
                        Expect.equal res expectedResults
            , test "fitGroup can add card" <|
                \_ ->
                    let
                        card = Red 5
                        clump = Group [Blue 5, Yellow 5, Green 5]
                        res = fitGroup card clump
                        expectedResults = Just <| ((Group [Blue 5, Yellow 5, Green 5, Red 5], []), 5)
                    in
                        Expect.equal res expectedResults

            ]
        , describe "fitRun & fitRunner"
            [ test "fitRunner can return possibility" <|
                \_ ->
                    let
                        card = Red 1
                        hand =
                            [ Red 10
                            , Blue 6
                            ]
                        table =
                            [ Run [Red 2, Red 3, Red 4]
                            ]
                        res = fitRunRunner card hand table
                        expectedTable =
                            [ Run [Red 1, Red 2, Red 3, Red 4]
                            ]
                        expectedResults = Just ((hand, expectedTable), 1)
                    in
                        Expect.equal res expectedResults
            , test "fitRunner can return possibility w multiple results #1" <|
                \_ ->
                    let
                        card = Red 9
                        hand =
                            [ Red 10
                            , Blue 6
                            ]
                        table =
                            [ Run [Red 10, Red 11, Red 12]
                            , Run [Red 6, Red 7, Red 8]
                            ]
                        res = fitRunRunner card hand table
                        expectedTable =
                            [ Run [Red 9, Red 10, Red 11, Red 12]
                            , Run [Red 6, Red 7, Red 8]
                            ]
                        expectedResults = Just ((hand, expectedTable), 9)
                    in
                        Expect.equal res expectedResults
            , test "fitRunner can return possibility w multiple results #2" <|
                \_ ->
                    let
                        card = Red 9
                        hand =
                            [ Red 10
                            , Blue 6
                            ]
                        table =
                            [ Run [Red 6, Red 7, Red 8]
                            , Run [Red 10, Red 11, Red 12]
                            ]
                        res = fitRunRunner card hand table
                        expectedTable =
                            [ Run [Red 6, Red 7, Red 8, Red 9]
                            , Run [Red 10, Red 11, Red 12]
                            ]
                        expectedResults = Just ((hand, expectedTable), 9)
                    in
                        Expect.equal res expectedResults
            , test "can add card to end of run" <|
                \_ ->
                    let
                        card = Blue 7
                        clump = Run [Blue 4, Blue 5, Blue 6]

                        res = fitRun card clump
                        expectedResults = Just ((Run [Blue 4, Blue 5, Blue 6, Blue 7], []), 7)
                    in
                        Expect.equal res expectedResults
            , test "can add card to front of run" <|
                \_ ->
                    let
                        card = Blue 3
                        clump = Run [Blue 4, Blue 5, Blue 6]

                        res = fitRun card clump
                        expectedResults = Just ((Run [Blue 3, Blue 4, Blue 5, Blue 6], []), 3)
                    in
                        Expect.equal res expectedResults

            ]
        , describe "formFromHand"
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
            , test "can return best option [run] #2" <|
                \_ ->
                    let
                        card = Blue 7
                        hand = [Red 5]
                        table =
                            [ Group [Blue 4, Red 4, Yellow 4]
                            , Run [Yellow 10, Blue 10, Red 10]
                            , Run [Blue 4, Blue 5, Blue 6]
                            ]

                        res = (formFromHand card hand table)
                        expectedResults = Just
                            (( [Red 5]
                             , [ Group [Blue 4, Red 4, Yellow 4]
                               , Run [Yellow 10, Blue 10, Red 10]
                               , Run [Blue 4, Blue 5, Blue 6, Blue 7]
                               ]
                             ), 7)
                    in
                        Expect.equal res expectedResults
            , test "can return best option [group] #3" <|
                \_ ->
                    let
                        card = Green 7
                        hand = [Red 5]
                        table =
                            [ Group [Blue 7, Red 7, Yellow 7]
                            , Run [Yellow 10, Blue 10, Red 10]
                            , Run [Blue 3, Blue 4, Blue 5]
                            ]

                        res = (formFromHand card hand table)
                        expectedResults = Just
                            (( [Red 5]
                             , [ Group [Blue 7, Red 7, Yellow 7, Green 7]
                               , Run [Yellow 10, Blue 10, Red 10]
                               , Run [Blue 3, Blue 4, Blue 5]
                               ]
                             ), 7)
                    in
                        Expect.equal res expectedResults
            , test "can return best option [group] #4" <|
                \_ ->
                    let
                        card = Blue 7
                        hand = [Red 5]
                        table =
                            [ Run [Yellow 10, Blue 10, Red 10]
                            , Run [Blue 3, Blue 4, Blue 5, Blue 6]
                            , Group [Blue 7, Red 7, Yellow 7]
                            ]

                        res = (formFromHand card hand table)
                        expectedResults = Just
                            (( [Red 5]
                             , [ Run [Yellow 10, Blue 10, Red 10]
                               , Run [Blue 3, Blue 4, Blue 5, Blue 6, Blue 7]
                               , Group [Blue 7, Red 7, Yellow 7]
                               ]
                             ), 7)
                    in
                        Expect.equal res expectedResults

            {-}, test "can return best option [group] #1" <|
                \_ ->
                    let
                        card = Yellow 5
                        hand = [Red 5, Blue 5]
                        table = [Run [Blue 5, Red 5, Green 5]]

                        res = (formFromHand card hand table)
                        expectedResults = Just
                            (( [Red 5, Blue 5]
                             , [Run [Blue 5, Red 5, Green 5, Yellow 5]]
                             ), 7)
                    in
                        Expect.equal res expectedResults
            -}]
        ]
