module SpaceTrader.Ship.Nav.FlightMode exposing
    ( FlightMode(..)
    , allModes
    , decode
    , default
    , encode
    , toLabel
    )

import Json.Decode
import Json.Encode


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


encode : FlightMode -> Json.Encode.Value
encode flightMode =
    Json.Encode.string <|
        case flightMode of
            Drift ->
                "DRIFT"

            Stealth ->
                "STEALTH"

            Cruise ->
                "CRUISE"

            Burn ->
                "BURN"


toLabel : FlightMode -> String
toLabel flightMode =
    case flightMode of
        Drift ->
            "Drift"

        Stealth ->
            "Stealth"

        Cruise ->
            "Cruise"

        Burn ->
            "Burn"


allModes : List FlightMode
allModes =
    [ Drift, Stealth, Cruise, Burn ]
