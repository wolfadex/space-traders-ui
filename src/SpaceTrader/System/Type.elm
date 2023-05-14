module SpaceTrader.System.Type exposing (..)

import Json.Decode
import Json.Encode

type Type
    = NeutronStar
    | RedStar
    | OrangeStar
    | BlueStar
    | YoungStar
    | WhiteDwarf
    | BlackHole
    | Hypergiant
    | Nebula
    | Unstable


decode : Json.Decode.Decoder Type
decode =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "NEUTRON_STAR" ->
                        Json.Decode.succeed NeutronStar

                    "RED_STAR" ->
                        Json.Decode.succeed RedStar

                    "ORANGE_STAR" ->
                        Json.Decode.succeed OrangeStar

                    "BLUE_STAR" ->
                        Json.Decode.succeed BlueStar

                    "YOUNG_STAR" ->
                        Json.Decode.succeed YoungStar

                    "WHITE_DWARF" ->
                        Json.Decode.succeed WhiteDwarf

                    "BLACK_HOLE" ->
                        Json.Decode.succeed BlackHole

                    "HYPERGIANT" ->
                        Json.Decode.succeed Hypergiant

                    "NEBULA" ->
                        Json.Decode.succeed Nebula

                    "UNSTABLE" ->
                        Json.Decode.succeed Unstable

                    _ ->
                        Json.Decode.fail ("Unknown system type: " ++ str)
            )


encode : Type -> Json.Encode.Value
encode systemType =
    case systemType of
        NeutronStar ->
            Json.Encode.string "NEUTRON_STAR"

        RedStar ->
            Json.Encode.string "RED_STAR"

        OrangeStar ->
            Json.Encode.string "ORANGE_STAR"

        BlueStar ->
            Json.Encode.string "BLUE_STAR"

        YoungStar ->
            Json.Encode.string "YOUNG_STAR"

        WhiteDwarf ->
            Json.Encode.string "WHITE_DWARF"

        BlackHole ->
            Json.Encode.string "BLACK_HOLE"

        Hypergiant ->
            Json.Encode.string "HYPERGIANT"

        Nebula ->
            Json.Encode.string "NEBULA"

        Unstable ->
            Json.Encode.string "UNSTABLE"