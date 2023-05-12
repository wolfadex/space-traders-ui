module SpaceTrader.Agent exposing (..)

import Json.Decode
import SpaceTrader.Point


type alias Agent =
    { accountId : String
    , callsign : String
    , headquarters : SpaceTrader.Point.Point
    , credits : Int
    }


decode : Json.Decode.Decoder Agent
decode =
    Json.Decode.map4 Agent
        (Json.Decode.field "accountId" Json.Decode.string)
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "headquarters" SpaceTrader.Point.decode)
        (Json.Decode.field "credits" Json.Decode.int)
