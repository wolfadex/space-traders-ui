module SpaceTrader.Agent exposing (Agent, decode)

import Json.Decode


type alias Agent =
    { accountId : String
    , callsign : String
    , headquarters : String
    , credits : Int
    }


decode : Json.Decode.Decoder Agent
decode =
    Json.Decode.map4 Agent
        (Json.Decode.field "accountId" Json.Decode.string)
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "headquarters" Json.Decode.string)
        (Json.Decode.field "credits" Json.Decode.int)
