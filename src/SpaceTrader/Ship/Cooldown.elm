module SpaceTrader.Ship.Cooldown exposing (Cooldown, decode)

import Iso8601
import Json.Decode
import Time


type alias Cooldown =
    { shipSymbol : String
    , totalSeconds : Int
    , remainingSeconds : Int
    , expiration : Time.Posix
    }


decode : Json.Decode.Decoder Cooldown
decode =
    Json.Decode.map4 Cooldown
        (Json.Decode.field "shipSymbol" Json.Decode.string)
        (Json.Decode.field "totalSeconds" Json.Decode.int)
        (Json.Decode.field "remainingSeconds" Json.Decode.int)
        (Json.Decode.field "expiration" Iso8601.decoder)
