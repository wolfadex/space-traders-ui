module SpaceTrader.Ship.Extraction exposing (Extraction, decode)

import Json.Decode
import SpaceTrader.Ship.Yield


type alias Extraction =
    { shipId : String
    , yield : SpaceTrader.Ship.Yield.Yield
    }


decode : Json.Decode.Decoder Extraction
decode =
    Json.Decode.map2 Extraction
        (Json.Decode.field "shipSymbol" Json.Decode.string)
        (Json.Decode.field "yield" SpaceTrader.Ship.Yield.decode)
