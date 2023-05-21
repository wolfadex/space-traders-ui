module SpaceTrader.Point.System exposing (System(..), decode, encode, parse, toKey, toLabel)

import Json.Decode
import Json.Encode


toLabel : System -> String
toLabel (System system) =
    system.sector ++ "-" ++ system.system


toKey : System -> String
toKey (System system) =
    system.sector ++ "-" ++ system.system


type System
    = System
        { sector : String
        , system : String
        }


decode : Json.Decode.Decoder System
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case parse str of
                    Just system ->
                        Json.Decode.succeed system

                    Nothing ->
                        Json.Decode.fail "Invalid system point"
            )


parse : String -> Maybe System
parse str =
    case String.split "-" str of
        [ sector, system ] ->
            Just (System { sector = sector, system = system })

        [ sector, system, _ ] ->
            Just (System { sector = sector, system = system })

        _ ->
            Nothing


encode : System -> Json.Encode.Value
encode system =
    Json.Encode.string (toKey system)
