module SpaceTrader.Point.Waypoint exposing (..)

import Json.Decode
import Json.Encode
import SpaceTrader.Point.System


toLabel : Waypoint -> String
toLabel (Waypoint waypoint) =
    waypoint.sector ++ "-" ++ waypoint.system ++ "-" ++ waypoint.waypoint


toShortLabel : Waypoint -> String
toShortLabel (Waypoint waypoint) =
    waypoint.waypoint


toSystem : Waypoint -> SpaceTrader.Point.System.System
toSystem (Waypoint waypoint) =
    SpaceTrader.Point.System.System
        { sector = waypoint.sector
        , system = waypoint.system
        }


toKey : Waypoint -> String
toKey (Waypoint waypoint) =
    waypoint.sector ++ "-" ++ waypoint.system ++ "-" ++ waypoint.waypoint


type Waypoint
    = Waypoint
        { sector : String
        , system : String
        , waypoint : String
        }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case parse str of
                    Just waypoint ->
                        Json.Decode.succeed waypoint

                    Nothing ->
                        Json.Decode.fail "Invalid waypoint point"
            )


parse : String -> Maybe Waypoint
parse str =
    case String.split "-" str of
        [ sector, system, waypoint ] ->
            Just
                (Waypoint
                    { sector = sector
                    , system = system
                    , waypoint = waypoint
                    }
                )

        _ ->
            Nothing


encode : Waypoint -> Json.Encode.Value
encode waypoint =
    Json.Encode.string (toKey waypoint)
