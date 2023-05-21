module SpaceTrader.System.Waypoint exposing (Waypoint, decode, encode)

import Json.Decode
import Json.Encode
import SpaceTrader.Point.Waypoint
import SpaceTrader.Waypoint.Type


type alias Waypoint =
    { symbol : SpaceTrader.Point.Waypoint.Waypoint
    , type_ : SpaceTrader.Waypoint.Type.Type
    , x : Int
    , y : Int
    }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.map4 Waypoint
        (Json.Decode.field "symbol" SpaceTrader.Point.Waypoint.decode)
        (Json.Decode.field "type" SpaceTrader.Waypoint.Type.decode)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


encode : Waypoint -> Json.Encode.Value
encode waypoint =
    Json.Encode.object
        [ ( "symbol", SpaceTrader.Point.Waypoint.encode waypoint.symbol )
        , ( "type", SpaceTrader.Waypoint.Type.encode waypoint.type_ )
        , ( "x", Json.Encode.int waypoint.x )
        , ( "y", Json.Encode.int waypoint.y )
        ]
