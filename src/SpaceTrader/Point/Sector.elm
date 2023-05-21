module SpaceTrader.Point.Sector exposing (Sector(..), decode, toLabel)

import Json.Decode


toLabel : Sector -> String
toLabel (Sector sector) =
    sector


type Sector
    = Sector String


decode : Json.Decode.Decoder Sector
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case String.split "-" str of
                    sector :: _ ->
                        Json.Decode.succeed (Sector sector)

                    _ ->
                        Json.Decode.fail "Invalid sector point"
            )
