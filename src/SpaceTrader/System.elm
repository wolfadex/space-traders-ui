module SpaceTrader.System exposing (..)

import Json.Decode
import Json.Encode
import SpaceTrader.Faction
import SpaceTrader.System.Type
import SpaceTrader.System.Waypoint


type alias System =
    { id : String
    , sector : String
    , type_ : SpaceTrader.System.Type.Type
    , x : Int
    , y : Int
    , waypoints : List SpaceTrader.System.Waypoint.Waypoint
    , factions : List String
    }


decode : Json.Decode.Decoder System
decode =
    Json.Decode.map7 System
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "sectorSymbol" Json.Decode.string)
        (Json.Decode.field "type" SpaceTrader.System.Type.decode)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)
        (Json.Decode.field "waypoints" (Json.Decode.list SpaceTrader.System.Waypoint.decode))
        (Json.Decode.field "factions" (Json.Decode.list (Json.Decode.field "symbol" Json.Decode.string)))


encode : System -> Json.Encode.Value
encode system =
    Json.Encode.object
        [ ( "symbol", Json.Encode.string system.id )
        , ( "sectorSymbol", Json.Encode.string system.sector )
        , ( "type", SpaceTrader.System.Type.encode system.type_ )
        , ( "x", Json.Encode.int system.x )
        , ( "y", Json.Encode.int system.y )
        , ( "waypoints", Json.Encode.list SpaceTrader.System.Waypoint.encode system.waypoints )
        , ( "factions"
          , Json.Encode.list
                (\faction -> Json.Encode.object [( "symbol", Json.Encode.string faction )])
                system.factions
          )
        ]