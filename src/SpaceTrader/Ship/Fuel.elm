module SpaceTrader.Ship.Fuel exposing (Fuel, decode)

import Iso8601
import Json.Decode
import Time


type alias Fuel =
    { current : Int
    , capacity : Int
    , consumed :
        { amount : Int
        , timestamp : Time.Posix
        }
    }


decode : Json.Decode.Decoder Fuel
decode =
    Json.Decode.map3 Fuel
        (Json.Decode.field "current" Json.Decode.int)
        (Json.Decode.field "capacity" Json.Decode.int)
        (Json.Decode.field "consumed" consumedDecoder)


consumedDecoder :
    Json.Decode.Decoder
        { amount : Int
        , timestamp : Time.Posix
        }
consumedDecoder =
    Json.Decode.map2
        (\amount timestamp ->
            { amount = amount
            , timestamp = timestamp
            }
        )
        (Json.Decode.field "amount" Json.Decode.int)
        (Json.Decode.field "timestamp" Iso8601.decoder)
