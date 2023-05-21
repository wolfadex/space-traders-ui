module SpaceTrader.Ship.Condition exposing (Condition(..), decode)

import Json.Decode


type Condition
    = Condition Int


decode : Json.Decode.Decoder Condition
decode =
    Json.Decode.map (max 0 >> min 100 >> Condition)
        Json.Decode.int
