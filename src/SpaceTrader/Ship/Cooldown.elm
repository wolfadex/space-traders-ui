module SpaceTrader.Ship.Cooldown exposing (Cooldown, decode)

import Id
import Iso8601
import Json.Decode
import SpaceTrader.Id exposing (ShipId)
import Time


type alias Cooldown =
    { shipSymbol : ShipId
    , totalSeconds : Int
    , remainingSeconds : Int
    , expiration : Time.Posix
    }


decode : Json.Decode.Decoder Cooldown
decode =
    Json.Decode.map4 Cooldown
        (Json.Decode.field "shipSymbol" Id.decode)
        (Json.Decode.field "totalSeconds" Json.Decode.int)
        (Json.Decode.field "remainingSeconds" Json.Decode.int)
        (Json.Decode.field "expiration" Iso8601.decoder)
