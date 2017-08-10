module Objects exposing (..)

type Card = Blue Int
          | Green Int
          | Red Int
          | Yellow Int

type alias Cards = List Card

type Clump = Group Cards
          | Run Cards

type alias Clumps = List Clump

type alias DraftCards = Cards
type alias Hand = Cards
type alias Table = Clumps

type alias Possibility = Maybe ((Hand, Table), Int)

type alias Draft = ((Clump, (Hand, Table)), Int)

cardValue : Card -> Int
cardValue card =
    case card of
        Blue v -> v
        Green v -> v
        Red v -> v
        Yellow v -> v

cardSuit : Card -> Int
cardSuit card =
    case card of
        Blue v -> 0
        Green v -> 1
        Red v -> 2
        Yellow v -> 3

convertToCard : Int -> Int -> Card
convertToCard suit value =
    let
        validSuit = suit >= 0 && suit <= 3
        validValue = value > 0 && value <= 13
    in
        if validSuit && validValue then
            case suit of
                0 -> Blue value
                1 -> Green value
                2 -> Red value
                3 -> Yellow value
                _ -> Blue -1
        else
            Blue -1 {- In the future create an invalid card instead of throwing a maybe -}

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
        card1V = cardSuit card1
        card2V = cardSuit card2
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

compareListInt : List Int -> List Int -> Bool
compareListInt a b =
    let
        ahead = case (List.head a) of
            Just v -> v
            Nothing -> -999
        bhead = case (List.head b) of
            Just v -> v
            Nothing -> -999
        arest = case (List.tail a) of
            Just v -> v
            Nothing -> []
        brest = case (List.tail b) of
            Just v -> v
            Nothing -> []
    in
        if List.isEmpty a && List.isEmpty b then
            True
        else if List.isEmpty a || List.isEmpty b then
            False
        else if ahead == bhead then
            True && compareListInt arest brest
        else
            False


{- Define proper group to be [3,4] in length, having colours from each suit-}
properClump : Clump -> Bool
properClump clump =
    case clump of
        Group v ->
            True
        Run v ->
            let
                firstCard = firstClumpCard clump
                lastCard = lastClumpCard clump
                values = List.map cardValue v
                leng = List.length v
            in
                leng >= 3 &&
                    compareListInt values (List.range (cardValue firstCard) (cardValue lastCard))

firstClumpCard : Clump -> Card
firstClumpCard clump =
    case clump of
        Group v ->
            case List.head v of
                Just crd -> crd
                Nothing -> Blue -2
        Run v ->
            case List.head v of
                Just crd -> crd
                Nothing -> Blue -2

lastClumpCard : Clump -> Card
lastClumpCard clump =
    case clump of
        Group v ->
            if List.length v <= 1 then
                case List.head v of
                    Just crd -> crd
                    Nothing -> Blue -2
            else
                case List.tail v of
                    Just rst -> lastClumpCard (Group rst)
                    Nothing -> Blue -3
        Run v ->
            if List.length v <= 1 then
                case List.head v of
                    Just crd -> crd
                    Nothing -> Blue -2
            else
                case List.tail v of
                    Just rst -> lastClumpCard (Run rst)
                    Nothing -> Blue -3

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
sortCards clump =
    let
        comparafn = \a b ->
            compare (cardValue a) (cardValue b)

        comparGrp = \a b ->
            let
                valA = cardSuit a
                valB = cardSuit b
            in
                if valA < valB then
                    LT
                else if valA == valB then
                    EQ
                else
                    GT

        results = case clump of
            Group v -> List.sortWith comparGrp v
            Run v -> List.sortWith comparafn v


    in
        case clump of
            Group v -> Group results
            Run v -> Run results

addAndReorderCards : Card -> Clump -> Clump
addAndReorderCards card clump = sortCards <| addCard card clump

removeAndReorderCards : Card -> Clump -> Clump
removeAndReorderCards card clump = sortCards <| removeCard card clump

{- Swaps a card for another card in the clump -}

patchClump : Card -> Card -> Clump -> Clump
patchClump card oldcard clump =
    case clump of
        Group v -> sortCards <| addAndReorderCards card <|removeCard oldcard clump
        Run v -> sortCards <| addAndReorderCards card <| removeCard oldcard clump

takeCardUntil : Clump -> Card -> Clump
takeCardUntil clump card =
    let
        cards = case (sortCards clump) of
            Group v -> v
            Run v -> v
        fn = \cl cd ->
            let
                headC = case (List.head cl) of
                    Just v -> v
                    Nothing -> Yellow -1
                restC = case (List.tail cl) of
                    Just v -> v
                    Nothing -> []
            in
                if List.isEmpty cl then
                    []
                else if sameCard cd headC then
                    []
                else
                    [headC] ++ fn restC cd
        res = fn cards card
    in
        case clump of
            Group v -> Group res
            Run v -> Run res


takeCardAfter : Clump -> Card -> Clump
takeCardAfter clump card =
    let
        cards = case (sortCards clump) of
            Group v -> v
            Run v -> v
        fn = \cl cd ->
            let
                headC = case (List.head cl) of
                    Just v -> v
                    Nothing -> Yellow -1
                restC = case (List.tail cl) of
                    Just v -> v
                    Nothing -> []
            in
                if List.isEmpty cl then
                    []
                else if sameCard cd headC then
                    restC
                else
                    fn restC cd
        res = fn cards card
    in
        case clump of
            Group v -> Group res
            Run v -> Run res

cardPresentClump : Clump -> Card -> Bool
cardPresentClump clump card =
    let
        lis = case clump of
            Group v -> v
            Run v -> v

        rst = case (List.tail lis) of
            Just v -> v
            Nothing -> []

        rstClump = case clump of
            Group v -> Group rst
            Run v -> Run rst
    in
        case (List.head lis) of
            Just x ->
                sameCard card x || cardPresentClump rstClump card
            Nothing -> False

cardPresentTable : Table -> Card -> Maybe Clump
cardPresentTable table card =
    let
        headV = case (List.head table) of
            Just v -> v
            Nothing -> Run []

        restV = case (List.tail table) of
            Just v -> v
            Nothing -> []
    in
        if List.isEmpty table then
            Nothing
        else
            if (cardPresentClump headV card) then
                Just headV
            else
                cardPresentTable restV card

replaceClump : Table -> Clump -> Clump -> Table
replaceClump table oldClump newClump =
    let
        clumpHead = case (List.head table) of
            Just v -> v
            Nothing -> Group []
        clumpTail = case (List.tail table) of
            Just v -> v
            Nothing -> []
    in
        if List.isEmpty table then
            []
        else
            if sameClump clumpHead oldClump then
                [newClump] ++ clumpTail
            else
                [clumpHead] ++ (replaceClump clumpTail oldClump newClump)

sameClump : Clump -> Clump -> Bool
sameClump clump1 clump2 =
    let
        equalCards = \a b ->
            let
                headA = case (List.head a) of
                    Just v -> v
                    Nothing -> Blue -1
                headB = case (List.head b) of
                    Just v -> v
                    Nothing -> Red -1
                restA = case (List.tail a) of
                    Just v -> v
                    Nothing -> []
                restB = case (List.tail b) of
                    Just v -> v
                    Nothing -> []
            in
                if List.isEmpty a && List.isEmpty b then
                    True
                else if sameCard headA headB then
                    equalCards restA restB
                else
                    False
    in
        case (sortCards clump1) of
            Group v ->
                case (sortCards clump2) of
                    Group w -> equalCards v w
                    Run w -> False
            Run v ->
                case (sortCards clump2) of
                    Group w -> False
                    Run w -> equalCards v w

removeClump : Clump -> Table -> Table
removeClump clump table = table

addClump : Clump -> Table -> Table
addClump clump table = table ++ [clump]

{- Generate a list of Cards, given the chain length we want (3)-}
{- Exclude the current card -}
generatePossibilities : Int -> Card -> List Cards
generatePossibilities n card =
    let
        suit = cardSuit card
        value = cardValue card

        rangeStartInt = max 1 (value - (n - 1))
        rangeEndInt = min 13 (value + (n - 1)) {- To make this adjustable, based on number of cards-}
        rangeEndStartingInt = rangeEndInt - (n - 1)

        generateRange = \len n ->
            List.map (convertToCard suit) <| List.range n (n + len - 1)
    in
        if (rangeEndStartingInt >= rangeStartInt) then
            List.map (generateRange n) (List.range rangeStartInt rangeEndStartingInt)
        else
            []

getBestPossibility : List Possibility -> Possibility
getBestPossibility possibilities =
    let
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
        List.foldl maxFn Nothing possibilities

{- draft is in the shape of ((Clump, (Hand, Table)), Int) -}
getBestDraft : List Draft -> Maybe Draft
getBestDraft drafts =
    let
        fn a b =
            let
                ((ac, (ah, at)), ai) = a
                ((bc, (bh, bt)), bi) = b
            in
                if ai > bi then
                    a
                else
                    b
    in
    if List.isEmpty drafts then
        Nothing
    else
        Just <| List.foldl fn ((Run [], ([], [])), 0) drafts
