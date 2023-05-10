module SpaceTrader.Contract.Payment exposing (..)

import Json.Decode


type alias Payment =
    { onAccepted : Int
    , onFulfilled : Int
    }


decode : Json.Decode.Decoder Payment
decode =
    Json.Decode.map2 Payment
        (Json.Decode.field "onAccepted" Json.Decode.int)
        (Json.Decode.field "onFulfilled" Json.Decode.int)
