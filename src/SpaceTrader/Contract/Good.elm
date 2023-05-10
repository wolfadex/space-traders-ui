module SpaceTrader.Contract.Good exposing (..)

import Json.Decode


type alias Good =
    { tradeSymbol : String
    , destinationSymbol : String
    , unitsRequired : Int
    , unitsFulfilled : Int
    }


decode : Json.Decode.Decoder Good
decode =
    Json.Decode.map4 Good
        (Json.Decode.field "tradeSymbol" Json.Decode.string)
        (Json.Decode.field "destinationSymbol" Json.Decode.string)
        (Json.Decode.field "unitsRequired" Json.Decode.int)
        (Json.Decode.field "unitsFulfilled" Json.Decode.int)
