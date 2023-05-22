module SpaceTrader.Waypoint.Type exposing
    ( Type(..)
    , decode
    , encode
    , toLabel
    )

import Json.Decode
import Json.Encode


type Type
    = Planet
    | GasGiant
    | Moon
    | OrbitalStation
    | JumpGate
    | AsteroidField
    | Nebula
    | DebrisField
    | GravityWell


toLabel : Type -> String
toLabel waypointType =
    case waypointType of
        Planet ->
            "Planet"

        GasGiant ->
            "Gas Giant"

        Moon ->
            "Moon"

        OrbitalStation ->
            "Orbital Station"

        JumpGate ->
            "Jump Gate"

        AsteroidField ->
            "Asteroid Field"

        Nebula ->
            "Nebula"

        DebrisField ->
            "Debris Field"

        GravityWell ->
            "Gravity Well"


decode : Json.Decode.Decoder Type
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "PLANET" ->
                        Json.Decode.succeed Planet

                    "GAS_GIANT" ->
                        Json.Decode.succeed GasGiant

                    "MOON" ->
                        Json.Decode.succeed Moon

                    "ORBITAL_STATION" ->
                        Json.Decode.succeed OrbitalStation

                    "JUMP_GATE" ->
                        Json.Decode.succeed JumpGate

                    "ASTEROID_FIELD" ->
                        Json.Decode.succeed AsteroidField

                    "NEBULA" ->
                        Json.Decode.succeed Nebula

                    "DEBRIS_FIELD" ->
                        Json.Decode.succeed DebrisField

                    "GRAVITY_WELL" ->
                        Json.Decode.succeed GravityWell

                    _ ->
                        Json.Decode.fail ("Unknown waypoint type: " ++ str)
            )


encode : Type -> Json.Encode.Value
encode waypointType =
    case waypointType of
        Planet ->
            Json.Encode.string "PLANET"

        GasGiant ->
            Json.Encode.string "GAS_GIANT"

        Moon ->
            Json.Encode.string "MOON"

        OrbitalStation ->
            Json.Encode.string "ORBITAL_STATION"

        JumpGate ->
            Json.Encode.string "JUMP_GATE"

        AsteroidField ->
            Json.Encode.string "ASTEROID_FIELD"

        Nebula ->
            Json.Encode.string "NEBULA"

        DebrisField ->
            Json.Encode.string "DEBRIS_FIELD"

        GravityWell ->
            Json.Encode.string "GRAVITY_WELL"
