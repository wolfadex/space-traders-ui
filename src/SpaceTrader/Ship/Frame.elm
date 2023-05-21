module SpaceTrader.Ship.Frame exposing (Frame, Style(..), decode)

import Json.Decode
import SpaceTrader.Ship.Condition exposing (Condition)
import SpaceTrader.Ship.Requirements exposing (Requirements)


type alias Frame =
    { style : Style
    , name : String
    , description : String
    , condition : Condition
    , moduleSlots : Int
    , mountingPoints : Int
    , fuelCapacity : Int
    , requirements : Requirements
    }


decode : Json.Decode.Decoder Frame
decode =
    Json.Decode.map8 Frame
        (Json.Decode.field "symbol" decodeStyle)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "condition" SpaceTrader.Ship.Condition.decode)
        (Json.Decode.field "moduleSlots" Json.Decode.int)
        (Json.Decode.field "mountingPoints" Json.Decode.int)
        (Json.Decode.field "fuelCapacity" Json.Decode.int)
        (Json.Decode.field "requirements" SpaceTrader.Ship.Requirements.decode)


type Style
    = Probe
    | Drone
    | Interceptor
    | Racer
    | Fighter
    | Frigate
    | Shuttle
    | Explorer
    | Miner
    | LightFreighter
    | HeavyFreighter
    | Transport
    | Destroyer
    | Cruiser
    | Carrier


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\style ->
                case style of
                    "FRAME_PROBE" ->
                        Json.Decode.succeed Probe

                    "FRAME_DRONE" ->
                        Json.Decode.succeed Drone

                    "FRAME_INTERCEPTOR" ->
                        Json.Decode.succeed Interceptor

                    "FRAME_RACER" ->
                        Json.Decode.succeed Racer

                    "FRAME_FIGHTER" ->
                        Json.Decode.succeed Fighter

                    "FRAME_FRIGATE" ->
                        Json.Decode.succeed Frigate

                    "FRAME_SHUTTLE" ->
                        Json.Decode.succeed Shuttle

                    "FRAME_EXPLORER" ->
                        Json.Decode.succeed Explorer

                    "FRAME_MINER" ->
                        Json.Decode.succeed Miner

                    "FRAME_LIGHT_FREIGHTER" ->
                        Json.Decode.succeed LightFreighter

                    "FRAME_HEAVY_FREIGHTER" ->
                        Json.Decode.succeed HeavyFreighter

                    "FRAME_TRANSPORT" ->
                        Json.Decode.succeed Transport

                    "FRAME_DESTROYER" ->
                        Json.Decode.succeed Destroyer

                    "FRAME_CRUISER" ->
                        Json.Decode.succeed Cruiser

                    "FRAME_CARRIER" ->
                        Json.Decode.succeed Carrier

                    _ ->
                        Json.Decode.fail <| "Unknown ship frame: " ++ style
            )
