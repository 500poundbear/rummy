module Rummy exposing (..)

{- Elm Modules -}
import List exposing (..)

{- Rummy Modules -}
import Print exposing (..)
import Objects exposing (..)
import Fitter exposing (..)

{- Structure -}
import Msg exposing (..)

initialHand = [Green 5, Red 5, Blue 5] ++ List.map Blue (List.range 4 10) ++ List.map Green (List.range 1 5)
    ++ List.map Yellow (List.range 4 10)
initialTable =
    [ Group [Blue 4, Red 4, Yellow 4]
    , Run [Green 3, Green 4, Green 5, Green 6]
    , Group [Yellow 10, Blue 10, Red 10]
    , Group [Red 1, Red 1, Red 1, Red 1]]

type alias RummyModel =
    { table : Table
    , hand : Hand
    }

viewRummyModel model = printBoth (.hand model) (.table model)
