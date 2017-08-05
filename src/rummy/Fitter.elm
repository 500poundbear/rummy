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
fitGroup : Card -> Clump -> Maybe (Clump, Int)
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
            Just <| (Group (cards ++ [card]), cardHVal)
        else
            Nothing

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


    If a new run or group can be formed, this will results in more points
    then simply extending a clump.

    The interesting portion is the bias between forming a new run vs forming
    a new group. 
-}
fitTile : Card -> Hand -> Table -> Maybe <| ((Hand, Table), Int)
fitTile card hand table =
