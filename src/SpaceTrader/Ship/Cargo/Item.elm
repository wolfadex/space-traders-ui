module SpaceTrader.Ship.Cargo.Item exposing (..)

import Json.Decode


type alias Item =
    { symbol : String
    , name : String
    , description : String
    , units : Int
    }


decode : Json.Decode.Decoder Item
decode =
    Json.Decode.map4 Item
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "units" Json.Decode.int)
