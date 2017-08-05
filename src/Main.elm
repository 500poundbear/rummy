module Main exposing (..)

import Html exposing (Html, div, text, program)
import Navigation exposing (Location)

import Array exposing (..)
import Basics exposing (..)
import Bitwise exposing (..)
import Char exposing (..)
import Debug exposing (..)
import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Msg exposing (..)

import Rummy exposing (..)

type alias Model = RummyModel


-- MODEL

init : Location -> ( Model, Cmd Msg )
init location =
    (
    { table = initialTable
    , hand = initialHand
    }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model = viewRummyModel model



update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            (model, Cmd.none)
        _ -> (model, Cmd.none )


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- MAIN


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
