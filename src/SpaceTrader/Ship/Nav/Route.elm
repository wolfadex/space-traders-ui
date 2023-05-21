module SpaceTrader.Ship.Nav.Route exposing (Route, decode)

import Iso8601
import Json.Decode
import SpaceTrader.Ship.Nav.Route.Waypoint exposing (Waypoint)
import Time


type alias Route =
    { destination : Waypoint
    , departure : Waypoint
    , departureTime : Time.Posix
    , arrival : Time.Posix
    }


decode : Json.Decode.Decoder Route
decode =
    Json.Decode.map4 Route
        (Json.Decode.field "destination" SpaceTrader.Ship.Nav.Route.Waypoint.decode)
        (Json.Decode.field "departure" SpaceTrader.Ship.Nav.Route.Waypoint.decode)
        (Json.Decode.field "departureTime" Iso8601.decoder)
        (Json.Decode.field "arrival" Iso8601.decoder)
