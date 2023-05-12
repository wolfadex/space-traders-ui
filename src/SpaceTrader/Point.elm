module SpaceTrader.Point exposing (..)

import Json.Decode


type alias Point =
    { sector : String
    , system : String
    , point : Maybe String
    }


decode : Json.Decode.Decoder Point
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case String.split "-" str of
                    [ sector, system, point ] ->
                        Json.Decode.succeed
                            { sector = sector, system = system, point = Just point }

                    [ sector, system ] ->
                        Json.Decode.succeed
                            { sector = sector, system = system, point = Nothing }

                    _ ->
                        Json.Decode.fail ("Unknown waypoint symbol: " ++ str)
            )
