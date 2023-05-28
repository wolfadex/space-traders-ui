module SpaceTrader.Ship.Nav.Route.Waypoint exposing (Waypoint, decode)

import Json.Decode
import SpaceTrader.Point.Waypoint
import SpaceTrader.Waypoint.Type


type alias Waypoint =
    { symbol : SpaceTrader.Point.Waypoint.Waypoint
    , type_ : SpaceTrader.Waypoint.Type.Type
    , system : String
    , x : Int
    , y : Int
    }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.map5 Waypoint
        (Json.Decode.field "symbol" SpaceTrader.Point.Waypoint.decode)
        (Json.Decode.field "type" SpaceTrader.Waypoint.Type.decode)
        (Json.Decode.field "systemSymbol" Json.Decode.string)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
