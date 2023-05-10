module SpaceTrader.Ship.Nav exposing (..)

import Json.Decode
import SpaceTrader.Ship.Nav.FlightMode exposing (FlightMode)
import SpaceTrader.Ship.Nav.Route exposing (Route)
import SpaceTrader.Ship.Nav.Status exposing (Status)


type alias Nav =
    { system : String
    , waypoint : String
    , route : Route
    , status : Status
    , flightMode : FlightMode
    }


decode : Json.Decode.Decoder Nav
decode =
    Json.Decode.map5 Nav
        (Json.Decode.field "system" Json.Decode.string)
        (Json.Decode.field "waypoint" Json.Decode.string)
        (Json.Decode.field "route" SpaceTrader.Ship.Nav.Route.decode)
        (Json.Decode.field "status" SpaceTrader.Ship.Nav.Status.decode)
        (Json.Decode.field "flightMode" SpaceTrader.Ship.Nav.FlightMode.decode)
