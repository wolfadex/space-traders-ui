module SpaceTrader.Chart exposing (Chart, decode)

import Iso8601
import Json.Decode
import Time


type alias Chart =
    { waypoint : Maybe String
    , submittedBy : Maybe String
    , submittedOn : Maybe Time.Posix
    }


decode : Json.Decode.Decoder Chart
decode =
    Json.Decode.map3 Chart
        (Json.Decode.maybe (Json.Decode.field "waypointSymbol" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "submittedBy" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "submittedOn" Iso8601.decoder))
