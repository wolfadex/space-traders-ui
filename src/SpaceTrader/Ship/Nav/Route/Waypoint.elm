module SpaceTrader.Ship.Nav.Route.Waypoint exposing (..)

import Json.Decode
import SpaceTrader.Point


type alias Waypoint =
    { symbol : SpaceTrader.Point.Point
    , type_ : Type
    , system : String
    , x : Int
    , y : Int
    }


decode : Json.Decode.Decoder Waypoint
decode =
    Json.Decode.map5 Waypoint
        (Json.Decode.field "symbol" SpaceTrader.Point.decode)
        (Json.Decode.field "type" decodeType)
        (Json.Decode.field "systemSymbol" Json.Decode.string)
        (Json.Decode.field "x" Json.Decode.int)
        (Json.Decode.field "y" Json.Decode.int)


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


decodeType : Json.Decode.Decoder Type
decodeType =
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
