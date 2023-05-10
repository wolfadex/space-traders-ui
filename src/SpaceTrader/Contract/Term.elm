module SpaceTrader.Contract.Term exposing (..)

import Iso8601
import Json.Decode
import SpaceTrader.Contract.Good exposing (Good)
import SpaceTrader.Contract.Payment exposing (Payment)
import Time


type alias Term =
    { deadline : Time.Posix
    , payment : Payment
    , deliver : List Good
    }


decode : Json.Decode.Decoder Term
decode =
    Json.Decode.map3 Term
        (Json.Decode.field "deadline" Iso8601.decoder)
        (Json.Decode.field "payment" SpaceTrader.Contract.Payment.decode)
        (Json.Decode.maybe
            (Json.Decode.field "deliver" (Json.Decode.list SpaceTrader.Contract.Good.decode))
            |> Json.Decode.map (Maybe.withDefault [])
        )
