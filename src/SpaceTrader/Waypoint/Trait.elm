module SpaceTrader.Waypoint.Trait exposing
    ( Style(..)
    , Trait
    , decode
    , styleToLabel
    )

import Json.Decode


type alias Trait =
    { name : String
    , description : String
    , style : Style
    }


decode : Json.Decode.Decoder Trait
decode =
    Json.Decode.map3 Trait
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "symbol" decodeStyle)


type Style
    = Uncharted
    | Marketplace
    | Shipyard
    | Outpost
    | ScatteredSettlements
    | SprawlingCities
    | MegaStructures
    | Overcrowded
    | HighTech
    | Corrupt
    | Bureaucratic
    | TradingHub
    | Industrial
    | BlackMarket
    | ResearchFacility
    | MilitaryBase
    | SurveillanceOutpost
    | ExplorationOutpost
    | MineralDeposits
    | CommonMetalDeposits
    | PreciousMetalDeposits
    | RareMetalDeposits
    | MethanePools
    | IceCrystals
    | ExplosiveGases
    | StrongMagnetosphere
    | VibrantAuroras
    | SaltFlats
    | Canyons
    | PerpetualDaylight
    | PerpetualOvercast
    | DrySeabeds
    | MagmaSeas
    | Supervolcanoes
    | AshClouds
    | VastRuins
    | MutatedFlora
    | Terraformed
    | ExtremeTemperatures
    | ExtremePressure
    | DiverseLife
    | ScarceLife
    | Fossils
    | WeakGravity
    | StrongGravity
    | CrushingGravity
    | ToxicAtmosphere
    | CorrosiveAtmosphere
    | BreathableAtmosphere
    | Jovian
    | Rocky
    | Volcanic
    | Frozen
    | Swamp
    | Barren
    | Temperate
    | Jungle
    | Ocean
    | Stripped


styleToLabel : Style -> String
styleToLabel style =
    case style of
        Uncharted ->
            "Uncharted"

        Marketplace ->
            "Marketplace"

        Shipyard ->
            "Shipyard"

        Outpost ->
            "Outpost"

        ScatteredSettlements ->
            "Scattered Settlements"

        SprawlingCities ->
            "Sprawling Cities"

        MegaStructures ->
            "Mega Structures"

        Overcrowded ->
            "Overcrowded"

        HighTech ->
            "High Tech"

        Corrupt ->
            "Corrupt"

        Bureaucratic ->
            "Bureaucratic"

        TradingHub ->
            "Trading Hub"

        Industrial ->
            "Industrial"

        BlackMarket ->
            "Black Market"

        ResearchFacility ->
            "Research Facility"

        MilitaryBase ->
            "Military Base"

        SurveillanceOutpost ->
            "Surveillance Outpost"

        ExplorationOutpost ->
            "Exploration Outpost"

        MineralDeposits ->
            "Mineral Deposits"

        CommonMetalDeposits ->
            "Common Metal Deposits"

        PreciousMetalDeposits ->
            "Precious Metal Deposits"

        RareMetalDeposits ->
            "Rare Metal Deposits"

        MethanePools ->
            "Methane Pools"

        IceCrystals ->
            "Ice Crystals"

        ExplosiveGases ->
            "Explosive Gases"

        StrongMagnetosphere ->
            "Strong Magnetosphere"

        VibrantAuroras ->
            "Vibrant Auroras"

        SaltFlats ->
            "Salt Flats"

        Canyons ->
            "Canyons"

        PerpetualDaylight ->
            "Perpetual Daylight"

        PerpetualOvercast ->
            "Perpetual Overcast"

        DrySeabeds ->
            "Dry Seabeds"

        MagmaSeas ->
            "Magma Seas"

        Supervolcanoes ->
            "Supervolcanoes"

        AshClouds ->
            "Ash Clouds"

        VastRuins ->
            "Vast Ruins"

        MutatedFlora ->
            "Mutated Flora"

        Terraformed ->
            "Terraformed"

        ExtremeTemperatures ->
            "Extreme Temperatures"

        ExtremePressure ->
            "Extreme Pressure"

        DiverseLife ->
            "Diverse Life"

        ScarceLife ->
            "Scarce Life"

        Fossils ->
            "Fossils"

        WeakGravity ->
            "Weak Gravity"

        StrongGravity ->
            "Strong Gravity"

        CrushingGravity ->
            "Crushing Gravity"

        ToxicAtmosphere ->
            "Toxic Atmosphere"

        CorrosiveAtmosphere ->
            "Corrosive Atmosphere"

        BreathableAtmosphere ->
            "Breathable Atmosphere"

        Jovian ->
            "Jovian"

        Rocky ->
            "Rocky"

        Volcanic ->
            "Volcanic"

        Frozen ->
            "Frozen"

        Swamp ->
            "Swamp"

        Barren ->
            "Barren"

        Temperate ->
            "Temperate"

        Jungle ->
            "Jungle"

        Ocean ->
            "Ocean"

        Stripped ->
            "Stripped"


decodeStyle : Json.Decode.Decoder Style
decodeStyle =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "UNCHARTED" ->
                        Json.Decode.succeed Uncharted

                    "MARKETPLACE" ->
                        Json.Decode.succeed Marketplace

                    "SHIPYARD" ->
                        Json.Decode.succeed Shipyard

                    "OUTPOST" ->
                        Json.Decode.succeed Outpost

                    "SCATTERED_SETTLEMENTS" ->
                        Json.Decode.succeed ScatteredSettlements

                    "SPRAWLING_CITIES" ->
                        Json.Decode.succeed SprawlingCities

                    "MEGA_STRUCTURES" ->
                        Json.Decode.succeed MegaStructures

                    "OVERCROWDED" ->
                        Json.Decode.succeed Overcrowded

                    "HIGH_TECH" ->
                        Json.Decode.succeed HighTech

                    "CORRUPT" ->
                        Json.Decode.succeed Corrupt

                    "BUREAUCRATIC" ->
                        Json.Decode.succeed Bureaucratic

                    "TRADING_HUB" ->
                        Json.Decode.succeed TradingHub

                    "INDUSTRIAL" ->
                        Json.Decode.succeed Industrial

                    "BLACK_MARKET" ->
                        Json.Decode.succeed BlackMarket

                    "RESEARCH_FACILITY" ->
                        Json.Decode.succeed ResearchFacility

                    "MILITARY_BASE" ->
                        Json.Decode.succeed MilitaryBase

                    "SURVEILLANCE_OUTPOST" ->
                        Json.Decode.succeed SurveillanceOutpost

                    "EXPLORATION_OUTPOST" ->
                        Json.Decode.succeed ExplorationOutpost

                    "MINERAL_DEPOSITS" ->
                        Json.Decode.succeed MineralDeposits

                    "COMMON_METAL_DEPOSITS" ->
                        Json.Decode.succeed CommonMetalDeposits

                    "PRECIOUS_METAL_DEPOSITS" ->
                        Json.Decode.succeed PreciousMetalDeposits

                    "RARE_METAL_DEPOSITS" ->
                        Json.Decode.succeed RareMetalDeposits

                    "METHANE_POOLS" ->
                        Json.Decode.succeed MethanePools

                    "ICE_CRYSTALS" ->
                        Json.Decode.succeed IceCrystals

                    "EXPLOSIVE_GASES" ->
                        Json.Decode.succeed ExplosiveGases

                    "STRONG_MAGNETOSPHERE" ->
                        Json.Decode.succeed StrongMagnetosphere

                    "VIBRANT_AURORAS" ->
                        Json.Decode.succeed VibrantAuroras

                    "SALTF_LATS" ->
                        Json.Decode.succeed SaltFlats

                    "CANYONS" ->
                        Json.Decode.succeed Canyons

                    "PERPETUAL_DAYLIGHT" ->
                        Json.Decode.succeed PerpetualDaylight

                    "PERPETUAL_OVERCAST" ->
                        Json.Decode.succeed PerpetualOvercast

                    "DRY_SEABEDS" ->
                        Json.Decode.succeed DrySeabeds

                    "MAGMA_SEAS" ->
                        Json.Decode.succeed MagmaSeas

                    "SUPERVOLCANOES" ->
                        Json.Decode.succeed Supervolcanoes

                    "ASH_CLOUDS" ->
                        Json.Decode.succeed AshClouds

                    "VAST_RUINS" ->
                        Json.Decode.succeed VastRuins

                    "MUTATED_FLORA" ->
                        Json.Decode.succeed MutatedFlora

                    "TERRAFORMED" ->
                        Json.Decode.succeed Terraformed

                    "EXTREME_TEMPERATURES" ->
                        Json.Decode.succeed ExtremeTemperatures

                    "EXTREME_PRESSURE" ->
                        Json.Decode.succeed ExtremePressure

                    "DIVERSE_LIFE" ->
                        Json.Decode.succeed DiverseLife

                    "SCARCE_LIFE" ->
                        Json.Decode.succeed ScarceLife

                    "FOSSILS" ->
                        Json.Decode.succeed Fossils

                    "WEAK_GRAVITY" ->
                        Json.Decode.succeed WeakGravity

                    "STRONG_GRAVITY" ->
                        Json.Decode.succeed StrongGravity

                    "CRUSHING_GRAVITY" ->
                        Json.Decode.succeed CrushingGravity

                    "TOXIC_ATMOSPHERE" ->
                        Json.Decode.succeed ToxicAtmosphere

                    "CORROSIVE_ATMOSPHERE" ->
                        Json.Decode.succeed CorrosiveAtmosphere

                    "BREATHABLE_ATMOSPHERE" ->
                        Json.Decode.succeed BreathableAtmosphere

                    "JOVIAN" ->
                        Json.Decode.succeed Jovian

                    "ROCKY" ->
                        Json.Decode.succeed Rocky

                    "VOLCANIC" ->
                        Json.Decode.succeed Volcanic

                    "FROZEN" ->
                        Json.Decode.succeed Frozen

                    "SWAMP" ->
                        Json.Decode.succeed Swamp

                    "BARREN" ->
                        Json.Decode.succeed Barren

                    "TEMPERATE" ->
                        Json.Decode.succeed Temperate

                    "JUNGLE" ->
                        Json.Decode.succeed Jungle

                    "OCEAN" ->
                        Json.Decode.succeed Ocean

                    "STRIPPED" ->
                        Json.Decode.succeed Stripped

                    _ ->
                        Json.Decode.fail ("Unknown Style: " ++ str)
            )
