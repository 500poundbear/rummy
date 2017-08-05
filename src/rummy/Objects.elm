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
