module SpaceTrader.Ship.Mount exposing (Deposit(..), Mount, Style(..), decode)

import Json.Decode
import SpaceTrader.Ship.Requirements exposing (Requirements)


type alias Mount =
    { style : Style
    , name : String
    , requirements : Requirements
    , description : Maybe String
    , strength : Int
    , deposits : List Deposit
    }


decode : Json.Decode.Decoder Mount
decode =
    Json.Decode.map6 Mount
        (Json.Decode.field "symbol" decodeStyle)
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "requirements" SpaceTrader.Ship.Requirements.decode)
        (Json.Decode.maybe (Json.Decode.field "description" Json.Decode.string))
        (Json.Decode.maybe (Json.Decode.field "strength" Json.Decode.int)
            |> Json.Decode.map (Maybe.withDefault 0)
        )
        (Json.Decode.maybe (Json.Decode.field "deposits" (Json.Decode.list decodeDeposit))
            |> Json.Decode.map (Maybe.withDefault [])
        )


type Deposit
    = QuartzSand
    | SiliconCrystals
    | PreciousStones
    | IceWater
    | AmmoniaIce
    | IronOre
    | CopperOre
    | SilverOre
    | AluminumOre
    | GoldOre
    | PlatinumOre
    | Diamonds
    | UraniteOre
    | MeritiumOre


decodeDeposit : Json.Decode.Decoder Deposit
decodeDeposit =
    Json.Decode.string
        |> Json.Decode.andThen
            (\deposit ->
                case deposit of
                    "QUARTZ_SAND" ->
                        Json.Decode.succeed QuartzSand

                    "SILICON_CRYSTALS" ->
                        Json.Decode.succeed SiliconCrystals

                    "PRECIOUS_STONES" ->
                        Json.Decode.succeed PreciousStones

                    "ICE_WATER" ->
                        Json.Decode.succeed IceWater

                    "AMMONIA_ICE" ->
                        Json.Decode.succeed AmmoniaIce

                    "IRON_ORE" ->
                        Json.Decode.succeed IronOre

                    "COPPER_ORE" ->
                        Json.Decode.succeed CopperOre

                    "SILVER_ORE" ->
                        Json.Decode.succeed SilverOre

                    "ALUMINUM_ORE" ->
                        Json.Decode.succeed AluminumOre

                    "GOLD_ORE" ->
                        Json.Decode.succeed GoldOre

                    "PLATINUM_ORE" ->
                        Json.Decode.succeed PlatinumOre

                    "DIAMONDS" ->
                        Json.Decode.succeed Diamonds

                    "URANITE_ORE" ->
                        Json.Decode.succeed UraniteOre

                    "MERITIUM_ORE" ->
                        Json.Decode.succeed MeritiumOre

                    _ ->
                        Json.Decode.fail <| "Unknown deposit: " ++ deposit
            )


type Style
    = GAS_SIPHON_I
    | GAS_SIPHON_II
    | GAS_SIPHON_III
    | SURVEYOR_I
    | SURVEYOR_II
    | SURVEYOR_III
    | SENSOR_ARRAY_I
    | SENSOR_ARRAY_II
    | SENSOR_ARRAY_III
    | MINING_LASER_I
    | MINING_LASER_II
    | MINING_LASER_III
    | LASER_CANNON_I
    | MISSILE_LAUNCHER_I
    | TURRET_I


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\style ->
                case style of
                    "MOUNT_GAS_SIPHON_I" ->
                        Json.Decode.succeed GAS_SIPHON_I

                    "MOUNT_GAS_SIPHON_II" ->
                        Json.Decode.succeed GAS_SIPHON_II

                    "MOUNT_GAS_SIPHON_III" ->
                        Json.Decode.succeed GAS_SIPHON_III

                    "MOUNT_SURVEYOR_I" ->
                        Json.Decode.succeed SURVEYOR_I

                    "MOUNT_SURVEYOR_II" ->
                        Json.Decode.succeed SURVEYOR_II

                    "MOUNT_SURVEYOR_III" ->
                        Json.Decode.succeed SURVEYOR_III

                    "MOUNT_SENSOR_ARRAY_I" ->
                        Json.Decode.succeed SENSOR_ARRAY_I

                    "MOUNT_SENSOR_ARRAY_II" ->
                        Json.Decode.succeed SENSOR_ARRAY_II

                    "MOUNT_SENSOR_ARRAY_III" ->
                        Json.Decode.succeed SENSOR_ARRAY_III

                    "MOUNT_MINING_LASER_I" ->
                        Json.Decode.succeed MINING_LASER_I

                    "MOUNT_MINING_LASER_II" ->
                        Json.Decode.succeed MINING_LASER_II

                    "MOUNT_MINING_LASER_III" ->
                        Json.Decode.succeed MINING_LASER_III

                    "MOUNT_LASER_CANNON_I" ->
                        Json.Decode.succeed LASER_CANNON_I

                    "MOUNT_MISSILE_LAUNCHER_I" ->
                        Json.Decode.succeed MISSILE_LAUNCHER_I

                    "MOUNT_TURRET_I" ->
                        Json.Decode.succeed TURRET_I

                    _ ->
                        Json.Decode.fail <| "Unknown mount style: " ++ style
            )
