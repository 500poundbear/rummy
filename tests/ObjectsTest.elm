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
            , test "can return card's suit" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        res = List.map cardSuit cards
                        expectedResults = [0, 2, 3, 1]
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
            , test "returns whether we can create cards" <|
                \_ ->
                    let
                        cardsSuit =
                            [ 0
                            , 2
                            , 1
                            , 3
                            ]
                        cardsValue =
                            [ 1
                            , 10
                            , 1
                            , 3
                            ]
                        res = List.map2 convertToCard cardsSuit cardsValue
                        expectedResults =
                            [ Blue 1
                            , Red 10
                            , Green 1
                            , Yellow 3
                            ]
                    in
                        Expect.equal res expectedResults
            , test "returns whether two cards are identical" <|
                \_ ->
                    let
                        cardsA =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        cardsB =
                            [ Blue 100
                            , Red 10
                            , Yellow 10
                            , Green 5
                            ]
                        res = List.map2 sameCard cardsA cardsB
                        expectedResults = [True, False, True, True]
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
        , describe "Hands"
            [ test "card present in hand" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        hand =
                            [ Blue 100
                            , Red 3
                            , Yellow 10
                            , Green 5
                            ]
                        res = List.map (cardPresentHand hand) cards
                        expectedResults = [True, False, True, True]
                    in
                        Expect.equal res expectedResults
             , test "remove card from hand if possible" <|
                \_ ->
                    let
                        cards =
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        hand =
                            [ Blue 100
                            , Red 3
                            , Yellow 10
                            , Green 5
                            ]
                        res = List.map (removeCardHand hand) cards
                        expectedResults =
                            [ Just [Red 3, Yellow 10, Green 5]
                            , Nothing
                            , Just [Blue 100, Red 3, Green 5]
                            , Just [Blue 100, Red 3, Yellow 10]
                            ]
                    in
                        Expect.equal res expectedResults
            ]
        , describe "Clumps"
            [ test "can add card to clump" <|
                \_ ->
                    let
                        card = Green 10
                        clumpRun = Run
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        clumpRunAfter = Run
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            , Green 10
                            ]
                    in
                        Expect.equal clumpRunAfter (addCard card clumpRun)
            , test "can remove card from clump" <|
                \_ ->
                    let
                        card = Yellow 10
                        clumpRun = Run
                            [ Blue 100
                            , Red 1
                            , Yellow 10
                            , Green 5
                            ]
                        clumpRunAfter = Run
                            [ Blue 100
                            , Red 1
                            , Green 5
                            ]
                    in
                        Expect.equal clumpRunAfter (removeCard card clumpRun)
            , test "can remove only one card at a time from clump" <|
                \_ ->
                    let
                        card = Yellow 10
                        clumpRun = Run
                            [ Yellow 7
                            , Yellow 8
                            , Yellow 9
                            , Yellow 10
                            ]
                        clumpRunAfter = Run
                            [ Yellow 7
                            , Yellow 8
                            , Yellow 9
                            ]
                    in
                        Expect.equal clumpRunAfter (removeCard card clumpRun)
            , test "can sort cards in a clump" <|
                \_ ->
                    let
                        clumpGroup = Run
                            [ Red 4
                            , Red 2
                            , Red 1
                            ]
                        clumpGroupAfter = Run
                            [ Red 1
                            , Red 2
                            , Red 4
                            ]
                    in
                        Expect.equal clumpGroupAfter (sortCards clumpGroup)
            , test "can add and reorder cards in a clump" <|
                \_ ->
                    let
                        add = Red 3
                        clumpGroup = Run
                            [ Red 4
                            , Red 2
                            , Red 1
                            ]
                        clumpGroupAfter = Run
                            [ Red 1
                            , Red 2
                            , Red 3
                            , Red 4
                            ]
                    in
                        Expect.equal clumpGroupAfter (addAndReorderCards add clumpGroup)
            , test "can remove and reorder cards in a clump" <|
                \_ ->
                    let
                        rm = Red 4
                        clumpGroup = Run
                            [ Red 4
                            , Red 2
                            , Red 1
                            , Red 5
                            ]
                        clumpGroupAfter = Run
                            [ Red 1
                            , Red 2
                            , Red 5
                            ]
                    in
                        Expect.equal clumpGroupAfter (removeAndReorderCards rm clumpGroup)
            , test "can patch clump" <|
                \_ ->
                    let
                        newCard = Red 4
                        oldCard = Blue 4
                        clumpGroup = Group
                            [ Yellow 4
                            , Red 4
                            , Blue 4
                            ]
                        clumpGroupAfter = Group
                            [ Red 4
                            , Red 4
                            , Yellow 4
                            ]
                    in
                        Expect.equal clumpGroupAfter (patchClump newCard oldCard clumpGroup)
            , test "can replace clump" <|
                \_ ->
                    let
                        clumpOld = Group
                            [ Yellow 4
                            , Red 4
                            , Blue 4
                            ]
                        clumpNew = Group
                            [ Yellow 4
                            , Red 4
                            , Red 4
                            ]
                        table =
                            [ Group [Blue 3, Red 3, Yellow 3]
                            , Group [Yellow 10, Green 10, Blue 10]
                            , Run [Yellow 1, Yellow 2, Yellow 3]
                            , Group [Yellow 4, Red 4, Blue 4]
                            ]
                        expected =
                            [ Group [Blue 3, Red 3, Yellow 3]
                            , Group [Yellow 10, Green 10, Blue 10]
                            , Run [Yellow 1, Yellow 2, Yellow 3]
                            , Group [Yellow 4, Red 4, Red 4]
                            ]
                    in
                        Expect.equal expected (replaceClump table clumpOld clumpNew)
            , test "detect if identical clump" <|
                \_ ->
                    let
                        clump1 = Group
                            [ Yellow 4
                            , Red 4
                            , Blue 4
                            , Green 4
                            ]
                        clump2 = Group
                            [ Yellow 4
                            , Red 4
                            , Green 4
                            , Blue 4
                            ]
                        clump3 = Run
                            [ Red 3
                            , Red 4
                            , Red 5
                            , Red 6
                            ]
                        clump4 = Run
                            [ Red 5
                            , Red 4
                            , Red 3
                            , Red 6
                            ]
                        clump5 = Run
                            [ Red 3
                            , Red 4
                            , Red 5
                            , Red 6
                            ]
                        clump6 = Run
                            [ Red 5
                            , Red 4
                            , Red 3
                            ]
                        clump7 = Group
                            [ Yellow 4
                            , Red 4
                            , Blue 4
                            ]
                        clump8 = Group
                            [ Yellow 4
                            , Red 4
                            , Green 4
                            ]

                        leftSide =
                            [ clump1
                            , clump3
                            , clump5
                            , clump7
                            ]
                        rightSide =
                            [ clump2
                            , clump4
                            , clump6
                            , clump8
                            ]
                        expected = [ True, True, False, False]
                    in
                        Expect.equal expected
                            (List.map2 sameClump leftSide rightSide)
            ]
        , describe "Possibilities"
            [ test "can be generated" <|
                \_ ->
                    let
                        lisLen = 3
                        cards =
                            [ Blue 4
                            , Red 5
                            , Yellow 1
                            , Green 10
                            , Red 13
                            ]
                        res = List.map (generatePossibilities 3) cards
                        expectedResults =
                            [ [ [Blue 2, Blue 3, Blue 4]
                              , [Blue 3, Blue 4, Blue 5]
                              , [Blue 4, Blue 5, Blue 6]
                              ]
                            , [ [Red 3, Red 4, Red 5]
                              , [Red 4, Red 5, Red 6]
                              , [Red 5, Red 6, Red 7]
                              ]
                            , [ [Yellow 1, Yellow 2, Yellow 3]
                              ]
                            , [ [Green 8, Green 9, Green 10]
                              , [Green 9, Green 10, Green 11]
                              , [Green 10, Green 11, Green 12]
                              ]
                            , [ [Red 11, Red 12, Red 13]]
                            ]
                    in
                        Expect.equal res expectedResults
            ]
            , describe "Possibility Object"
                [ test "can determine maximum with getBestPossibility" <|
                    \_ ->
                        let
                            possibilities =
                                [ Nothing
                                , Just (([], [Group [Red 5, Green 5, Blue 5]]), 15)
                                , Just (([], [Run [Red 4, Red 5, Red 6, Red 7 ]]), 4)
                                , Just (([Red 5], [Group [Red 8, Blue 8, Green 8, Yellow 8]]), 8)
                                , Nothing
                                ]
                            expectedResult = Just (([], [Group [Red 5, Green 5, Blue 5]]), 15)
                        in
                            Expect.equal (getBestPossibility possibilities) expectedResult
                , test "getBestPossibility can return Nothing appropriately" <|
                    \_ ->
                        let
                            possibilities =
                                [ Nothing
                                , Nothing
                                ]
                            expectedResult = Nothing
                        in
                            Expect.equal (getBestPossibility possibilities) expectedResult
                ]
        ]
