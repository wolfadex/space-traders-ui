module SpaceTrader.Waypoint exposing (Waypoint, decode)

import Json.Decode
import Json.Decode.Extra
import SpaceTrader.Chart exposing (Chart)
import SpaceTrader.Waypoint.Trait exposing (Trait)
import SpaceTrader.Waypoint.Type


type alias Waypoint =
    { symbol : String
    , type_ : SpaceTrader.Waypoint.Type.Type
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
            (Json.Decode.field "type" SpaceTrader.Waypoint.Type.decode)
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
