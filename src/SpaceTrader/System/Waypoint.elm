module SpaceTrader.System.Waypoint exposing (Waypoint, decode, encode)

import Json.Decode
import Json.Encode
import SpaceTrader.Waypoint.Type


type alias Waypoint =
    { symbol : String
    , type_ : SpaceTrader.Waypoint.Type.Type
    , x : Int
    , y : Int
    }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.map4 Waypoint
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "type" SpaceTrader.Waypoint.Type.decode)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


encode : Waypoint -> Json.Encode.Value
encode waypoint =
    Json.Encode.object
        [ ( "symbol", Json.Encode.string waypoint.symbol )
        , ( "type", SpaceTrader.Waypoint.Type.encode waypoint.type_ )
        , ( "x", Json.Encode.int waypoint.x )
        , ( "y", Json.Encode.int waypoint.y )
        ]
