module SpaceTrader.Ship.Requirements exposing (Requirements, decode)

import Json.Decode


type alias Requirements =
    { power : Maybe Int
    , crew : Maybe Int
    , slots : Maybe Int
    }


decode : Json.Decode.Decoder Requirements
decode =
    Json.Decode.map3 Requirements
        (Json.Decode.maybe (Json.Decode.field "power" Json.Decode.int))
        (Json.Decode.maybe (Json.Decode.field "crew" Json.Decode.int))
        (Json.Decode.maybe (Json.Decode.field "slots" Json.Decode.int))
