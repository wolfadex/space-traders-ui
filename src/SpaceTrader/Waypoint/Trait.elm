module SpaceTrader.Waypoint.Trait exposing (Style(..), Trait, decode)

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

                    "SCATTEREDSETTLEMENTS" ->
                        Json.Decode.succeed ScatteredSettlements

                    "SPRAWLINGCITIES" ->
                        Json.Decode.succeed SprawlingCities

                    "MEGASTRUCTURES" ->
                        Json.Decode.succeed MegaStructures

                    "OVERCROWDED" ->
                        Json.Decode.succeed Overcrowded

                    "HIGHTECH" ->
                        Json.Decode.succeed HighTech

                    "CORRUPT" ->
                        Json.Decode.succeed Corrupt

                    "BUREAUCRATIC" ->
                        Json.Decode.succeed Bureaucratic

                    "TRADINGHUB" ->
                        Json.Decode.succeed TradingHub

                    "INDUSTRIAL" ->
                        Json.Decode.succeed Industrial

                    "BLACKMARKET" ->
                        Json.Decode.succeed BlackMarket

                    "RESEARCHFACILITY" ->
                        Json.Decode.succeed ResearchFacility

                    "MILITARYBASE" ->
                        Json.Decode.succeed MilitaryBase

                    "SURVEILLANCEOUTPOST" ->
                        Json.Decode.succeed SurveillanceOutpost

                    "EXPLORATIONOUTPOST" ->
                        Json.Decode.succeed ExplorationOutpost

                    "MINERALDEPOSITS" ->
                        Json.Decode.succeed MineralDeposits

                    "COMMONMETALDEPOSITS" ->
                        Json.Decode.succeed CommonMetalDeposits

                    "PRECIOUSMETALDEPOSITS" ->
                        Json.Decode.succeed PreciousMetalDeposits

                    "RAREMETALDEPOSITS" ->
                        Json.Decode.succeed RareMetalDeposits

                    "METHANEPOOLS" ->
                        Json.Decode.succeed MethanePools

                    "ICECRYSTALS" ->
                        Json.Decode.succeed IceCrystals

                    "EXPLOSIVEGASES" ->
                        Json.Decode.succeed ExplosiveGases

                    "STRONGMAGNETOSPHERE" ->
                        Json.Decode.succeed StrongMagnetosphere

                    "VIBRANTAURORAS" ->
                        Json.Decode.succeed VibrantAuroras

                    "SALTFLATS" ->
                        Json.Decode.succeed SaltFlats

                    "CANYONS" ->
                        Json.Decode.succeed Canyons

                    "PERPETUALDAYLIGHT" ->
                        Json.Decode.succeed PerpetualDaylight

                    "PERPETUALOVERCAST" ->
                        Json.Decode.succeed PerpetualOvercast

                    "DRYSEABEDS" ->
                        Json.Decode.succeed DrySeabeds

                    "MAGMASEAS" ->
                        Json.Decode.succeed MagmaSeas

                    "SUPERVOLCANOES" ->
                        Json.Decode.succeed Supervolcanoes

                    "ASHCLOUDS" ->
                        Json.Decode.succeed AshClouds

                    "VASTRUINS" ->
                        Json.Decode.succeed VastRuins

                    "MUTATEDFLORA" ->
                        Json.Decode.succeed MutatedFlora

                    "TERRAFORMED" ->
                        Json.Decode.succeed Terraformed

                    "EXTREMETEMPERATURES" ->
                        Json.Decode.succeed ExtremeTemperatures

                    "EXTREMEPRESSURE" ->
                        Json.Decode.succeed ExtremePressure

                    "DIVERSELIFE" ->
                        Json.Decode.succeed DiverseLife

                    "SCARCELIFE" ->
                        Json.Decode.succeed ScarceLife

                    "FOSSILS" ->
                        Json.Decode.succeed Fossils

                    "WEAKGRAVITY" ->
                        Json.Decode.succeed WeakGravity

                    "STRONGGRAVITY" ->
                        Json.Decode.succeed StrongGravity

                    "CRUSHINGGRAVITY" ->
                        Json.Decode.succeed CrushingGravity

                    "TOXICATMOSPHERE" ->
                        Json.Decode.succeed ToxicAtmosphere

                    "CORROSIVEATMOSPHERE" ->
                        Json.Decode.succeed CorrosiveAtmosphere

                    "BREATHABLEATMOSPHERE" ->
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
