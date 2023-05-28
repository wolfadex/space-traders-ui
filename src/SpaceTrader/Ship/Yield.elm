module SpaceTrader.Ship.Yield exposing (Yield, decode)

import Json.Decode


type alias Yield =
    { symbol : String
    , units : Int
    }


decode : Json.Decode.Decoder Yield
decode =
    Json.Decode.map2 Yield
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "units" Json.Decode.int)

