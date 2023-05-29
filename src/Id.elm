module Id exposing
    ( Id
    , decode
    , fromString
    , toLabel
    , toString
    )

import Json.Decode


type Id a
    = Id String


fromString : String -> Id a
fromString =
    Id


toLabel : Id a -> String
toLabel (Id s) =
    s


toString : Id a -> String
toString (Id s) =
    s


decode : Json.Decode.Decoder (Id a)
decode =
    Json.Decode.map fromString
        Json.Decode.string
