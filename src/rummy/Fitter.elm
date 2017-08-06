module Fitter exposing (..)

import Objects exposing (..)

pickGroups : Table -> List Clump
pickGroups table =
    let
        isGroup = \n ->
            case n of
                Group v -> True
                Run v -> False
    in
    List.filter isGroup table


pickRuns : Table -> List Clump
pickRuns table =
    let
        isRun = \n ->
            case n of
                Group v -> False
                Run v -> True
    in
    List.filter isRun table

{- Since it is already a group, this is the 4++ card -}
fitGroup : Card -> Clump -> Maybe ((Clump, Cards), Int)
fitGroup card group =
    let
        cardVal = cardValue card
        cards = case group of
            Group v -> v
            _ -> []
        cardHVal = case (List.head cards) of
            Nothing -> -1
            Just v -> cardValue v
    in
        if cardVal == cardHVal then
            Just <| ((Group (cards ++ [card]), []), cardHVal)
        else
            Nothing

{- The aim is to select a list of clumps, and just pick the first-}
{- Hand is redundant, but here for the functions to require the same params -}
fitRunRunner : Card -> Hand -> Table -> Possibility
fitRunRunner card hand table =
    let
        runs = pickRuns table
        oldClump = case (List.head runs) of
            Just v -> v
            Nothing -> Group []
    in
        case (List.head<|List.map (fitRun card) runs) of
            Just v ->
                let
                    ((newClump, _), score) = case v of
                        Just v -> v
                        Nothing -> ((Group [], []), -1)

                    newTable = replaceClump table oldClump newClump
                in
                    Just ((hand, newTable), score)
            Nothing -> Nothing

{- Since it is already a run, this is the 4++ card -}
fitRun : Card -> Clump -> Maybe ((Clump, Cards), Int)
fitRun card run =
    let
        cardVal = cardValue card
        cards = case run of
            Run v -> v
            _ -> []
        cardsLen = List.length cards
        fstCard = case (firstCard cards) of
            Nothing -> Blue -1
            Just v -> v
        fstCardValue = cardValue fstCard
        lstCardValue = case (lastCard cards) of
            Nothing -> -1
            Just v -> cardValue v
    in
        if sameSuit card fstCard then
            if cardVal == (fstCardValue) - 1 then
                Just ((Run ([card] ++ cards), []), cardVal)
            else if cardVal == (lstCardValue) + 1 then
                Just ((Run (cards ++ [card]), []), cardVal)
            else
                {- Return the rest if a breakup is possible -}
                if cardsLen >= 4 then
                    if cardsLen % 2 == 0 then
                        Nothing {- TODO -}
                    else
                        Nothing {- TODO -}
                else
                    Nothing
        else
            Nothing



{- Given a card that we MUST use, see what score we get.
   There are a few things that can be done:
        (1) fitGroup
            - this card can extend a group
        (2) fitRun
            - this card can extend a run
        (3) fitRunCenter
            - this card can cause two existing runs to be concatenated
            - How this works? We pair every run w every other run.
            - See if we can connect any two pairs together
        (4) formFromHand
            - a general function that can take a parameter
            that determines how many other cards they are looking
            from hand.

            For example, if I need Blue 7 and Blue 7 is in the table
            in a group of 3 and I have Green 7, my move will be to
            put Green 7 there before I use Blue 7 elsewhere.

            But note that this does not mean that the utmost priority
            is to ensure that we entertain groups, because we don't want
            to feed opponents unnecessarily too?

            When plucking from table, not

            Meaning if I am running (formFromHand 3) it means that I'll
            try to pattern match using 3 from my hand (2 + one forced)

            If it is (formFromhand 2) it means that I


    If a new run or group can be formed, this will results in more points
    then simply extending a clump.

    The interesting portion is the bias between forming a new run vs forming
    a new group.

    fitTile will force the use of card, using whatever is available on
    the table. It will recursively call itself

    Maybe is used as a signal for whether the card can be used.

    returns the first value in
-}
formFromHand : Card -> Hand -> Table -> Possibility
formFromHand card hand table =
    let
        res =
            [ fitRunRunner card hand table
            , fitRunRunner card hand table
            ]
        maxFn : Possibility -> Possibility -> Possibility
        maxFn newPossibility currentRecord =
            case newPossibility of
                Just newV->
                    case currentRecord of
                        Nothing -> newPossibility
                        Just v ->
                            let
                                {- Compare which possibility has a larger value -}
                                (_, newInt) = newV
                                (_, recordInt) = v
                            in
                                if newInt > recordInt then
                                    Just newV
                                else
                                    Just v
                Nothing ->
                    currentRecord
    in
        List.foldl maxFn Nothing res


{- Go through every card in hand, running formFromHand -}
fitter : Hand -> Table -> Maybe (Hand, Table)
fitter hand table =
    let
        handLen = List.length hand

        helper = \ind ->
            let
                theCard = case (nthCard ind hand) of
                    Just v -> v
                    Nothing -> Blue -1
                theHand = case (removeCardHand hand theCard) of
                    Just v -> v
                    Nothing -> []
            in
                formFromHand theCard theHand table

        combis = List.map helper <| List.range 0 (handLen - 1)

        getBest a b=
            case a of
                Just v ->
                    let
                        ((hand, table), score) = v
                        newb = case b of
                            Just v ->
                                let
                                    ((_, _), bScore) = v
                                in
                                    if bScore < score then
                                        a
                                    else
                                        b
                            Nothing ->
                                a
                    in
                        newb

                Nothing -> b

        {- a fold function that gets the max value-}
        best = List.foldl getBest Nothing combis
    in
        case best of
            Just v -> Just <| Tuple.first v
            Nothing -> Nothing

fitterRunner : Hand -> Table -> (Hand, Table)
fitterRunner hand table =
    let
        results = fitter hand table
    in
        case results of
            Just v ->
                let
                    (nhand, ntable) = v
                in
                    fitterRunner nhand ntable
            Nothing -> (hand, table)
