module SpaceTrader.Ship.Cargo.Item exposing
    ( Item
    , decode
    , toLabel
    )

import Json.Decode
import String.Extra


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


toLabel : Item -> String
toLabel item =
    item.name
        |> String.replace "_" " "
        |> String.toLower
        |> String.Extra.toSentenceCase
