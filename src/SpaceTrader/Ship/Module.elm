module SpaceTrader.Ship.Module exposing (..)

import Json.Decode
import SpaceTrader.Ship.Requirements exposing (Requirements)


type alias Module =
    { style : Style
    , name : String
    , requirements : Requirements
    , capacity : Maybe Int
    , range : Maybe Int
    }


decode : Json.Decode.Decoder Module
decode =
    Json.Decode.map5 Module
        (Json.Decode.field "symbol" decodeStyle)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "requirements" SpaceTrader.Ship.Requirements.decode)
        (Json.Decode.maybe (Json.Decode.field "capacity" Json.Decode.int))
        (Json.Decode.maybe (Json.Decode.field "range" Json.Decode.int))


type Style
    = MINERAL_PROCESSOR_I
    | CARGO_HOLD_I
    | CREW_QUARTERS_I
    | ENVOY_QUARTERS_I
    | PASSENGER_CABIN_I
    | MICRO_REFINERY_I
    | ORE_REFINERY_I
    | FUEL_REFINERY_I
    | SCIENCE_LAB_I
    | JUMP_DRIVE_I
    | JUMP_DRIVE_II
    | JUMP_DRIVE_III
    | WARP_DRIVE_I
    | WARP_DRIVE_II
    | WARP_DRIVE_III
    | SHIELD_GENERATOR_I
    | SHIELD_GENERATOR_II


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\style ->
                case style of
                    "MODULE_MINERAL_PROCESSOR_I" ->
                        Json.Decode.succeed MINERAL_PROCESSOR_I

                    "MODULE_CARGO_HOLD_I" ->
                        Json.Decode.succeed CARGO_HOLD_I

                    "MODULE_CREW_QUARTERS_I" ->
                        Json.Decode.succeed CREW_QUARTERS_I

                    "MODULE_ENVOY_QUARTERS_I" ->
                        Json.Decode.succeed ENVOY_QUARTERS_I

                    "MODULE_PASSENGER_CABIN_I" ->
                        Json.Decode.succeed PASSENGER_CABIN_I

                    "MODULE_MICRO_REFINERY_I" ->
                        Json.Decode.succeed MICRO_REFINERY_I

                    "MODULE_ORE_REFINERY_I" ->
                        Json.Decode.succeed ORE_REFINERY_I

                    "MODULE_FUEL_REFINERY_I" ->
                        Json.Decode.succeed FUEL_REFINERY_I

                    "MODULE_SCIENCE_LAB_I" ->
                        Json.Decode.succeed SCIENCE_LAB_I

                    "MODULE_JUMP_DRIVE_I" ->
                        Json.Decode.succeed JUMP_DRIVE_I

                    "MODULE_JUMP_DRIVE_II" ->
                        Json.Decode.succeed JUMP_DRIVE_II

                    "MODULE_JUMP_DRIVE_III" ->
                        Json.Decode.succeed JUMP_DRIVE_III

                    "MODULE_WARP_DRIVE_I" ->
                        Json.Decode.succeed WARP_DRIVE_I

                    "MODULE_WARP_DRIVE_II" ->
                        Json.Decode.succeed WARP_DRIVE_II

                    "MODULE_WARP_DRIVE_III" ->
                        Json.Decode.succeed WARP_DRIVE_III

                    "MODULE_SHIELD_GENERATOR_I" ->
                        Json.Decode.succeed SHIELD_GENERATOR_I

                    "MODULE_SHIELD_GENERATOR_II" ->
                        Json.Decode.succeed SHIELD_GENERATOR_II

                    _ ->
                        Json.Decode.fail <| "Unknown module style: " ++ style
            )
