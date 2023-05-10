module SpaceTrader.Ship.Role exposing (..)

import Json.Decode


type Role
    = Fabricator
    | Harvester
    | Hauler
    | Interceptor
    | Excavator
    | Transport
    | Repair
    | Surveyor
    | Command
    | Carrier
    | Patrol
    | Satellite
    | Explorer
    | Refinery


decode : Json.Decode.Decoder Role
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\role ->
                case role of
                    "FABRICATOR" ->
                        Json.Decode.succeed Fabricator

                    "HARVESTER" ->
                        Json.Decode.succeed Harvester

                    "HAULER" ->
                        Json.Decode.succeed Hauler

                    "INTERCEPTOR" ->
                        Json.Decode.succeed Interceptor

                    "EXCAVATOR" ->
                        Json.Decode.succeed Excavator

                    "TRANSPORT" ->
                        Json.Decode.succeed Transport

                    "REPAIR" ->
                        Json.Decode.succeed Repair

                    "SURVEYOR" ->
                        Json.Decode.succeed Surveyor

                    "COMMAND" ->
                        Json.Decode.succeed Command

                    "CARRIER" ->
                        Json.Decode.succeed Carrier

                    "PATROL" ->
                        Json.Decode.succeed Patrol

                    "SATELLITE" ->
                        Json.Decode.succeed Satellite

                    "EXPLORER" ->
                        Json.Decode.succeed Explorer

                    "REFINERY" ->
                        Json.Decode.succeed Refinery

                    _ ->
                        Json.Decode.fail <| "Unknown ship role: " ++ role
            )
