module SpaceTrader.Faction exposing (..)

import Json.Decode
import SpaceTrader.Faction.Trait exposing (Trait)


type Group
    = Cosmic
    | Void
    | Galactic
    | Quantum
    | Dominion


groups : List Group
groups =
    [ Cosmic
    , Void
    , Galactic
    , Quantum
    , Dominion
    ]


groupToPrettyString : Group -> String
groupToPrettyString group =
    case group of
        Cosmic ->
            "Cosmic"

        Void ->
            "Void"

        Galactic ->
            "Galactic"

        Quantum ->
            "Quantum"

        Dominion ->
            "Dominion"


groupToString : Group -> String
groupToString group =
    case group of
        Cosmic ->
            "COSMIC"

        Void ->
            "VOID"

        Galactic ->
            "GALACTIC"

        Quantum ->
            "QUANTUM"

        Dominion ->
            "DOMINION"


groupFromString : String -> Maybe Group
groupFromString str =
    case str of
        "COSMIC" ->
            Just Cosmic

        "VOID" ->
            Just Void

        "GALACTIC" ->
            Just Galactic

        "QUANTUM" ->
            Just Quantum

        "DOMINION" ->
            Just Dominion

        _ ->
            Nothing


type alias Faction =
    { group : Group
    , name : String
    , description : String
    , headquarters : String
    , traits : List Trait
    }


decode : Json.Decode.Decoder Faction
decode =
    Json.Decode.map5
        (\group name description headquarters traits ->
            { group = group
            , name = name
            , description = description
            , headquarters = headquarters
            , traits = traits
            }
        )
        (Json.Decode.field "group" decodeGroup)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "headquarters" Json.Decode.string)
        (Json.Decode.field "traits" (Json.Decode.list SpaceTrader.Faction.Trait.decode))


decodeGroup : Json.Decode.Decoder Group
decodeGroup =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case groupFromString str of
                    Just group ->
                        Json.Decode.succeed group

                    _ ->
                        Json.Decode.fail ("Unknown faction group: " ++ str)
            )
