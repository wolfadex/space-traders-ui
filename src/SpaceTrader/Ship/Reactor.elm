module SpaceTrader.Ship.Reactor exposing (Reactor, Style(..), decode)

import Json.Decode
import SpaceTrader.Ship.Condition exposing (Condition)
import SpaceTrader.Ship.Requirements exposing (Requirements)


type alias Reactor =
    { style : Style
    , name : String
    , description : String
    , condition : Condition
    , powerOutput : Int
    , requirements : Requirements
    }


decode : Json.Decode.Decoder Reactor
decode =
    Json.Decode.map6 Reactor
        (Json.Decode.field "symbol" decodeStyle)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "condition" SpaceTrader.Ship.Condition.decode)
        (Json.Decode.field "powerOutput" Json.Decode.int)
        (Json.Decode.field "requirements" SpaceTrader.Ship.Requirements.decode)


type Style
    = SolarI
    | FutionI
    | FissionI
    | ChemicalI
    | AntimatterI


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "REACTOR_SOLAR_I" ->
                        Json.Decode.succeed SolarI

                    "REACTOR_FUSION_I" ->
                        Json.Decode.succeed FutionI

                    "REACTOR_FISSION_I" ->
                        Json.Decode.succeed FissionI

                    "REACTOR_CHEMICAL_I" ->
                        Json.Decode.succeed ChemicalI

                    "REACTOR_ANTIMATTER_I" ->
                        Json.Decode.succeed AntimatterI

                    _ ->
                        Json.Decode.fail "Unknown reactor style"
            )
