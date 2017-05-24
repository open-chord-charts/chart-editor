module Route exposing (Route(..), fromLocation, href)

import Html exposing (Attribute)
import Html.Attributes
import Navigation exposing (Location)
import UrlParser as Url exposing ((</>), parseHash, string, top)


-- ROUTING --


type alias Slug =
    String


type Route
    = Home
    | Chart Slug


route : Url.Parser (Route -> a) a
route =
    Url.oneOf
        [ Url.map Home top
        , Url.map Chart string
        ]



-- INTERNAL --


routeToString : Route -> String
routeToString page =
    let
        pieces =
            case page of
                Home ->
                    []

                Chart slug ->
                    [ slug ]
    in
        "#/" ++ (String.join "/" pieces)



-- PUBLIC HELPERS --


href : Route -> Attribute msg
href route =
    Html.Attributes.href (routeToString route)


fromLocation : Location -> Maybe Route
fromLocation location =
    if String.isEmpty location.hash then
        Just Home
    else
        parseHash route location
