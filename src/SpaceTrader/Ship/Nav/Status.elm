module SpaceTrader.Ship.Nav.Status exposing (..)

import Json.Decode


type Status
    = InTransit
    | InOrbit
    | Docked


decode : Json.Decode.Decoder Status
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\status ->
                case status of
                    "IN_TRANSIT" ->
                        Json.Decode.succeed InTransit

                    "IN_ORBIT" ->
                        Json.Decode.succeed InOrbit

                    "DOCKED" ->
                        Json.Decode.succeed Docked

                    _ ->
                        Json.Decode.fail "Invalid status"
            )
