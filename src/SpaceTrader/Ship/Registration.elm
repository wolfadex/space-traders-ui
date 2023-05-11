module SpaceTrader.Ship.Registration exposing (..)

import Json.Decode
import SpaceTrader.Ship.Role exposing (Role)


type alias Registration =
    { name : String
    , faction : String
    , role : Role
    }


decode : Json.Decode.Decoder Registration
decode =
    Json.Decode.map3 Registration
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "factionSymbol" Json.Decode.string)
        (Json.Decode.field "role" SpaceTrader.Ship.Role.decode)
