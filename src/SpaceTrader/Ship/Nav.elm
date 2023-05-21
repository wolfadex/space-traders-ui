module SpaceTrader.Ship.Nav exposing (Nav, decode)

import Json.Decode
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship.Nav.FlightMode exposing (FlightMode)
import SpaceTrader.Ship.Nav.Route exposing (Route)
import SpaceTrader.Ship.Nav.Status exposing (Status)


type alias Nav =
    { system : SpaceTrader.Point.System.System
    , waypoint : SpaceTrader.Point.Waypoint.Waypoint
    , route : Route
    , status : Status
    , flightMode : FlightMode
    }


decode : Json.Decode.Decoder Nav
decode =
    Json.Decode.map5 Nav
        (Json.Decode.field "systemSymbol" SpaceTrader.Point.System.decode)
        (Json.Decode.field "waypointSymbol" SpaceTrader.Point.Waypoint.decode)
        (Json.Decode.field "route" SpaceTrader.Ship.Nav.Route.decode)
        (Json.Decode.field "status" SpaceTrader.Ship.Nav.Status.decode)
        (Json.Decode.field "flightMode" SpaceTrader.Ship.Nav.FlightMode.decode)
