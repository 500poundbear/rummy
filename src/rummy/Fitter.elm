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

fitGroupRunner : Card -> Hand -> Table -> Possibility
fitGroupRunner card hand table =
    let
        groups = pickGroups table
        highestYieldingClump = getPositiveResult <|List.map (fitGroup card) groups {- Abit futile -}
    in
        case highestYieldingClump of
            Just v ->
                let
                    ((newClump, newCards), val) = v
                    oldClump = removeCard card newClump
                    newTable = replaceClump table oldClump newClump
                in
                    Just ((hand, newTable), val)
            Nothing -> Nothing


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
        cardSuitValue = cardSuit card
        otherCardsSuitValues = List.map cardSuit cards
        cardSuitExists = \n -> n == cardSuitValue
    in
        if cardVal == cardHVal && List.isEmpty (List.filter cardSuitExists otherCardsSuitValues) then
            Just <| ((Group (cards ++ [card]), []), cardHVal)
        else
            Nothing

getPositiveResult : List (Maybe ((Clump, Cards), Int)) -> Maybe ((Clump, Cards), Int)
getPositiveResult lis =
    let
        fn : Maybe ((Clump, Cards), Int) -> Maybe ((Clump, Cards), Int) -> Maybe ((Clump, Cards), Int)
        fn = \a b ->
            case a of
                Nothing ->
                    b
                Just v ->
                    case b of
                        Nothing -> Just v
                        Just w ->
                            let
                                (vComb, vVal) = v
                                (wComb, wVal) = w
                            in
                                if vVal > wVal then
                                    Just v
                                else
                                    Just w
    in
        List.foldl fn (Nothing) lis

{- The aim is to select a list of clumps, and just pick the first-}
{- Hand is redundant, but here for the functions to require the same params -}
fitRunRunner : Card -> Hand -> Table -> Possibility
fitRunRunner card hand table =
    let
        runs = pickRuns table
        highestYieldingClump = getPositiveResult <|List.map (fitRun card) runs
    in
        case highestYieldingClump of
            Just v ->
                let
                    ((newClump, newCards), val) = v
                    oldClump = removeCard card newClump
                    newTable = replaceClump table oldClump newClump
                in
                    Just ((hand, newTable), val)
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
            , fitGroupRunner card hand table
            , formClumpRunner card hand table
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

{- Given a draft, attempt to lengthen it until you get 3 -}
{- formClump doesn't look for cards in both directions, it only looks
   for cards 1 larger than current card (to reduce search space) -}
{- Draft: ((Clump, (Hand, Table)), Int) -}

formClump : Clump -> Hand -> Table -> Int -> List Draft
formClump draft hand table currentScore =
    let
        draftLen = List.length <| case draft of
            Group v -> v
            Run v -> v
    in
        if draftLen >= 3 then
            [((draft, (hand, table)), currentScore)]
        else if draftLen == 0 then
            {- Means an empty clump, don't bother -}
            []
        else
            let

                sortedDraft =
                    case (sortCards draft) of
                        Group v -> Group v
                        Run v -> Run v

                {- Process RUN -}
                runLastCard = lastClumpCard sortedDraft
                runLastCardValue = cardValue runLastCard

                nextTargetValue = runLastCardValue + 1
                nextTargetCard = case runLastCard of
                    Blue v -> Blue nextTargetValue
                    Green v -> Green nextTargetValue
                    Red v -> Red nextTargetValue
                    Yellow v -> Yellow nextTargetValue

                findRunFromHand =
                    if nextTargetValue < 13 && cardPresentHand hand nextTargetCard then
                        case (removeCardHand hand nextTargetCard) of
                            Just v ->
                                [(nextTargetCard
                                 , v
                                 , table
                                 , nextTargetValue + currentScore)]
                            Nothing ->
                                []
                    else
                        []

                findRunFromTable =
                    if nextTargetValue < 13 then
                        let
                            (found, oldClump) = case (cardPresentTable table nextTargetCard) of
                                Just clump -> (True, clump)
                                Nothing -> (False, Run [])


                            {- Check if this will result in a clump being torn down -}
                            clumpBefore = takeCardUntil oldClump nextTargetCard
                            clumpAfter = takeCardAfter oldClump nextTargetCard

                            newClump = removeAndReorderCards nextTargetCard oldClump

                            newHand = hand
                            newTable = replaceClump table oldClump newClump
                            newScore = currentScore
                         in
                            if found then
                                [ (nextTargetCard
                                , newHand
                                , newTable
                                , newScore)]
                            else
                                []
                    else
                        []


                mapFormClump = \n ->
                    let
                        (nextTargetCard, updatedHand, updatedTable, updatedScore) = n
                        nDraft = addCard nextTargetCard draft
                    in
                        (formClump nDraft updatedHand updatedTable updatedScore)

                shortlist
                    = findRunFromHand ++ findRunFromTable

                {- FOR GROUP PROCESS SEPARATELY -}
            in
                List.foldl (++) [] (List.map mapFormClump shortlist)

{- calls formClump -}
formClumpRunner : Card -> Hand -> Table -> Possibility
formClumpRunner card hand table =
    let
        resRun = formClump (Run [card]) hand table (cardValue card)
        resGroup = formClump (Group [card]) hand table (cardValue card)

        consolidated = resRun ++ resGroup

    in
        case (getBestDraft consolidated) of
            Just v ->
                let
                    ((bestDraftClump, (bestDraftHand, bestDraftTable)), bestDraftScore) = v

                    newTable = addClump bestDraftClump bestDraftTable
                    newHand = bestDraftHand
                    newScore = bestDraftScore
                in
                    Just ((newHand, newTable), newScore)
            Nothing ->
                Nothing
