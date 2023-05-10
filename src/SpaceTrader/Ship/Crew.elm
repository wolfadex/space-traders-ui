module SpaceTrader.Ship.Crew exposing (..)

import Json.Decode


type alias Crew =
    { current : Int
    , required : Int
    , capacity : Int
    , rotation : Rotation
    , morale : Int
    , wages : Int
    }


type Rotation
    = Strict
    | Relaxed


decode : Json.Decode.Decoder Crew
decode =
    Json.Decode.map6 Crew
        (Json.Decode.field "current" Json.Decode.int)
        (Json.Decode.field "required" Json.Decode.int)
        (Json.Decode.field "capacity" Json.Decode.int)
        (Json.Decode.field "rotation" decodeRotation)
        (Json.Decode.field "morale" Json.Decode.int)
        (Json.Decode.field "wages" Json.Decode.int)


decodeRotation : Json.Decode.Decoder Rotation
decodeRotation =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "STRICT" ->
                        Json.Decode.succeed Strict

                    "RELAXED" ->
                        Json.Decode.succeed Relaxed

                    _ ->
                        Json.Decode.fail "Invalid rotation"
            )
