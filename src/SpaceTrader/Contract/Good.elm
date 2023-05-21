module SpaceTrader.Contract.Good exposing (Good, decode)

import Json.Decode
import SpaceTrader.Point.Waypoint


type alias Good =
    { tradeSymbol : String
    , destinationSymbol : SpaceTrader.Point.Waypoint.Waypoint
    , unitsRequired : Int
    , unitsFulfilled : Int
    }


decode : Json.Decode.Decoder Good
decode =
    Json.Decode.map4 Good
        (Json.Decode.field "tradeSymbol" Json.Decode.string)
        (Json.Decode.field "destinationSymbol" SpaceTrader.Point.Waypoint.decode)
        (Json.Decode.field "unitsRequired" Json.Decode.int)
        (Json.Decode.field "unitsFulfilled" Json.Decode.int)
