module SpaceTrader.Ship.Engine exposing (..)

import Json.Decode
import SpaceTrader.Ship.Condition exposing (Condition)
import SpaceTrader.Ship.Requirements exposing (Requirements)


type alias Engine =
    { style : Style
    , name : String
    , description : String
    , condition : Condition
    , speed : Float
    , requirements : Requirements
    }


type Style
    = ImpulseDriveI
    | IonDriveI
    | IonDriveII
    | HyperDriveI


decode : Json.Decode.Decoder Engine
decode =
    Json.Decode.map6 Engine
        (Json.Decode.field "symbol" decodeStyle)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "condition" SpaceTrader.Ship.Condition.decode)
        (Json.Decode.field "speed" Json.Decode.float)
        (Json.Decode.field "requirements" SpaceTrader.Ship.Requirements.decode)


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\style ->
                case style of
                    "ENGINE_IMPULSE_DRIVE_I" ->
                        Json.Decode.succeed ImpulseDriveI

                    "ENGINE_ION_DRIVE_I" ->
                        Json.Decode.succeed IonDriveI

                    "ENGINE_ION_DRIVE_II" ->
                        Json.Decode.succeed IonDriveII

                    "ENGINE_HYPER_DRIVE_I" ->
                        Json.Decode.succeed HyperDriveI

                    _ ->
                        Json.Decode.fail <| "Unknown engine style: " ++ style
            )
