module Routing exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)

type Route
    = MainRoute
    | NotFoundRoute

matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map MainRoute top
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFoundRoute
