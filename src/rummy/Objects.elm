module Objects exposing (..)

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

cardValue : Card -> Int
cardValue card =
    case card of
        Blue v -> v
        Green v -> v
        Red v -> v
        Yellow v -> v


nthCard : Int -> Cards -> Maybe Card
nthCard n cards =
    if n < 0 then
        Nothing
    else if n == 0 then
        case cards of
            x::_ -> Just x
            _ -> Nothing
    else
        let
            rest = case (List.tail cards) of
                Nothing -> []
                Just v -> v
        in
        nthCard (n - 1) rest

firstCard : Cards -> Maybe Card
firstCard = nthCard 0

lastCard : Cards -> Maybe Card
lastCard cards = nthCard ((List.length cards) - 1) cards

sameSuit : Card -> Card -> Bool
sameSuit card1 card2 =
    let
        card1V = case card1 of
            Blue v -> 0
            Green v -> 1
            Red v -> 2
            Yellow v -> 3
        card2V = case card2 of
            Blue v -> 0
            Green v -> 1
            Red v -> 2
            Yellow v -> 3
    in
        card1V == card2V

{- Definition of card equality: same suit and value -}
sameCard : Card -> Card -> Bool
sameCard card1 card2 =
    let
        card1V = case card1 of
            Blue v -> 0
            Green v -> 1
            Red v -> 2
            Yellow v -> 3
        card2V = case card2 of
            Blue v -> 0
            Green v -> 1
            Red v -> 2
            Yellow v -> 3
    in
        (sameSuit card1 card2) && (cardValue card1 == cardValue card2)

cardPresentHand : Hand -> Card -> Bool
cardPresentHand hand card =
    let
        removeNots = \n -> sameCard card n
    in
        List.length (List.filter removeNots hand) > 0

{- Remove 1 instance of the card if it exists-}
removeCardHand : Hand -> Card -> Maybe Hand
removeCardHand hand card =
    let
        cardPresent = cardPresentHand hand card
        removeDriver = \fnt bck ->
            if List.isEmpty bck then
                fnt
            else
                {- Get the head and check-}
                let
                    headCard = case (List.head bck) of
                        Nothing -> Blue -1
                        Just v -> v
                    restCards = case (List.tail bck) of
                        Nothing -> []
                        Just v -> v
                in
                    if sameCard headCard card then
                        fnt ++ restCards
                    else
                        removeDriver (fnt ++ [headCard]) restCards
    in
        if cardPresent then
            Just (removeDriver [] hand)
        else
            Nothing


addCard : Card -> Clump -> Clump
addCard card clump =
    case clump of
        Group v -> Group <| v ++ [card]
        Run v -> Run <| v ++ [card]

{- Removes a single instance of the card. Fails silently if card not found -}
removeCard : Card -> Clump -> Clump
removeCard card clump =
    let
        driver = \fst snd ->
            let
                currentCard = case (List.head snd) of
                    Just v -> v
                    Nothing -> Blue -1
                restCards = case (List.tail snd) of
                    Just v -> v
                    Nothing -> []
            in
                if List.isEmpty snd then
                    fst
                else if sameCard card currentCard then
                    fst ++ restCards
                else
                    driver (fst ++ [currentCard]) restCards
    in
        case clump of
            Group v -> Group <| driver [] v
            Run v -> Run <| driver [] v

sortCards : Clump -> Clump
sortCards x = x

addAndReorderCards : Card -> Clump -> Clump
addAndReorderCards card clump = sortCards <| addCard card clump

removeAndReorderCards : Card -> Clump -> Clump
removeAndReorderCards card clump = sortCards <| removeCard card clump

{- Swaps a card for another card in the clump -}
{-
patchFromHand : Card -> Card -> Clump -> Clump
patchFromhand card oldcard clump =
    case clump of
            Group v ->

            Run v ->
-}
