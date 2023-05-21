module SpaceTrader.Ship.Nav.FlightMode exposing (FlightMode(..), decode, default)

import Json.Decode


type FlightMode
    = Drift
    | Stealth
    | Cruise
    | Burn


default : FlightMode
default =
    Cruise


decode : Json.Decode.Decoder FlightMode
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "DRIFT" ->
                        Json.Decode.succeed Drift

                    "STEALTH" ->
                        Json.Decode.succeed Stealth

                    "CRUISE" ->
                        Json.Decode.succeed Cruise

                    "BURN" ->
                        Json.Decode.succeed Burn

                    _ ->
                        Json.Decode.fail ("Unknown flight mode: " ++ str)
            )
