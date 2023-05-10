module SpaceTrader.Waypoint exposing (..)

import Json.Decode
import Json.Decode.Extra
import SpaceTrader.Chart exposing (Chart)
import SpaceTrader.Waypoint.Trait exposing (Trait)


type alias Waypoint =
    { symbol : String
    , type_ : Type
    , system : String
    , x : Int
    , y : Int
    , orbitals : List String
    , traits : List Trait
    , faction : Maybe String
    , chart : Maybe Chart
    }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.succeed Waypoint
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "symbol" Json.Decode.string)
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "type" deoceType)
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "systemSymbol" Json.Decode.string)
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "x" Json.Decode.int)
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "y" Json.Decode.int)
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "orbitals"
                (Json.Decode.list
                    (Json.Decode.field "symbol" Json.Decode.string)
                )
            )
        |> Json.Decode.Extra.andMap
            (Json.Decode.field "traits"
                (Json.Decode.list SpaceTrader.Waypoint.Trait.decode)
            )
        |> Json.Decode.Extra.andMap
            (Json.Decode.maybe
                (Json.Decode.at [ "faction", "symbol" ] Json.Decode.string)
            )
        |> Json.Decode.Extra.andMap
            (Json.Decode.maybe
                (Json.Decode.field "chart" SpaceTrader.Chart.decode)
            )


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


deoceType : Json.Decode.Decoder Type
deoceType =
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
