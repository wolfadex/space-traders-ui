module SpaceTrader.Survey exposing (..)

import Iso8601
import Json.Decode
import Time


type alias Survey =
    { signature : String
    , symbol : String
    , deposits : List String
    , expiration : Time.Posix
    , size : Size
    }


decode : Json.Decode.Decoder Survey
decode =
    Json.Decode.map5 Survey
        (Json.Decode.field "signature" Json.Decode.string)
        (Json.Decode.field "symbol" Json.Decode.string)
        (Json.Decode.field "deposits" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "expiration" Iso8601.decoder)
        (Json.Decode.field "size" decodeSize)


type Size
    = Small
    | Moderate
    | Large


decodeSize : Json.Decode.Decoder Size
decodeSize =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "SMALL" ->
                        Json.Decode.succeed Small

                    "MODERATE" ->
                        Json.Decode.succeed Moderate

                    "LARGE" ->
                        Json.Decode.succeed Large

                    _ ->
                        Json.Decode.fail "Invalid size"
            )
