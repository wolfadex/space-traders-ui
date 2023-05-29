module Id exposing
    ( Id
    , decode
    , encode
    , fromString
    , toLabel
    , toString
    )

import Json.Decode
import Json.Encode


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


encode : Id a -> Json.Encode.Value
encode (Id s) =
    Json.Encode.string s
