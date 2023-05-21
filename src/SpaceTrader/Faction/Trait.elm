module SpaceTrader.Faction.Trait exposing (Style(..), Trait, decode)

import Json.Decode


type alias Trait =
    { name : String
    , description : String
    , style : Style
    }


type Style
    = Bureaucratic
    | Secretive
    | Capitalistic
    | Industrious
    | Peaceful
    | Distrustful
    | Welcoming
    | Anarchist
    | Conflicted
    | Authoritarian
    | Oligarchical
    | Dynastic
    | Democcractic
    | Decentralized
    | Smugglers
    | Scavengers
    | Rebellious
    | Exiles
    | Pirates
    | Raiders
    | Clan
    | Guild
    | Dominion
    | Fringe
    | Forsaken
    | Isolated
    | Localized
    | Established
    | Notable
    | Dominant
    | Inescapable
    | Innovative
    | Bold
    | Visionary
    | Curious
    | Daring
    | Exploratory
    | Resourceful
    | Flexible
    | Cooperative
    | United
    | Strategic
    | Intelligent
    | ResearchFocused
    | Collaborative
    | Progressive
    | Militaristic
    | TechnologicallyAdvanced
    | Aggressive
    | Imperialistic
    | TreasureHunters
    | Dextrous
    | Unpredictable
    | Brutal
    | Fleeting
    | Adaptable
    | SelfSufficient
    | Defensive
    | Proud
    | Diverse
    | Independent
    | SelfInterested
    | Fragmented
    | Commercial
    | FreeMarkets
    | Entrepreneurial


styleToString : Style -> String
styleToString style =
    case style of
        Bureaucratic ->
            "BUREAUCRATIC"

        Secretive ->
            "SECRETIVE"

        Capitalistic ->
            "CAPITALISTIC"

        Industrious ->
            "INDUSTRIOUS"

        Peaceful ->
            "PEACEFUL"

        Distrustful ->
            "DISTRUSTFUL"

        Welcoming ->
            "WELCOMING"

        Anarchist ->
            "ANARCHIST"

        Conflicted ->
            "CONFLICTED"

        Authoritarian ->
            "AUTHORITARIAN"

        Oligarchical ->
            "OLIGARCHICAL"

        Dynastic ->
            "DYNASTIC"

        Democcractic ->
            "DEMOCRACTIC"

        Decentralized ->
            "DECENTRALIZED"

        Smugglers ->
            "SMUGGLERS"

        Scavengers ->
            "SCAVENGERS"

        Rebellious ->
            "REBELLIOUS"

        Exiles ->
            "EXILES"

        Pirates ->
            "PIRATES"

        Raiders ->
            "RAIDERS"

        Clan ->
            "CLAN"

        Guild ->
            "GUILD"

        Dominion ->
            "DOMINION"

        Fringe ->
            "FRINGE"

        Forsaken ->
            "FORSAKEN"

        Bold ->
            "BOLD"

        Visionary ->
            "VISIONARY"

        Curious ->
            "CURIOUS"

        Daring ->
            "DARING"

        Exploratory ->
            "EXPLORATORY"

        Resourceful ->
            "RESOURCEFUL"

        Flexible ->
            "FLEXIBLE"

        Cooperative ->
            "COOPERATIVE"

        United ->
            "UNITED"

        Strategic ->
            "STRATEGIC"

        Intelligent ->
            "INTELLIGENT"

        ResearchFocused ->
            "RESEARCH_FOCUSED"

        Collaborative ->
            "COLLABORATIVE"

        Progressive ->
            "PROGRESSIVE"

        Militaristic ->
            "MILITARISTIC"

        TechnologicallyAdvanced ->
            "TECHNOLOGICALLY_ADVANCED"

        Aggressive ->
            "AGGRESSIVE"

        Imperialistic ->
            "IMPERIALISTIC"

        TreasureHunters ->
            "TREASURE_HUNTERS"

        Dextrous ->
            "DEXTEROUS"

        Unpredictable ->
            "UNPREDICTABLE"

        Brutal ->
            "BRUTAL"

        Fleeting ->
            "FLEETING"

        Adaptable ->
            "ADAPTABLE"

        SelfSufficient ->
            "SELF_SUFFICIENT"

        Defensive ->
            "DEFENSIVE"

        Proud ->
            "PROUD"

        Diverse ->
            "DIVERSE"

        Independent ->
            "INDEPENDENT"

        SelfInterested ->
            "SELF_INTERESTED"

        Fragmented ->
            "FRAGMENTED"

        Commercial ->
            "COMMERCIAL"

        FreeMarkets ->
            "FREE_MARKETS"

        Entrepreneurial ->
            "ENTREPRENEURIAL"

        Isolated ->
            "ISOLATED"

        Localized ->
            "LOCALIZED"

        Established ->
            "ESTABLISHED"

        Notable ->
            "NOTABLE"

        Dominant ->
            "DOMINANT"

        Inescapable ->
            "INESCAPABLE"

        Innovative ->
            "INNOVATIVE"


{-| Attempts to convert a `String` into a `Style`. E.g.

    "SELF_INTERESTED" -> Just SelfInterested
    "INESCAPABLE" -> Just Inescapable
    "NoT_A_StYlE" -> Nothing

-}
styleFromString : String -> Maybe Style
styleFromString str =
    case str of
        "BUREAUCRATIC" ->
            Just Bureaucratic

        "SECRETIVE" ->
            Just Secretive

        "CAPITALISTIC" ->
            Just Capitalistic

        "INDUSTRIOUS" ->
            Just Industrious

        "PEACEFUL" ->
            Just Peaceful

        "DISTRUSTFUL" ->
            Just Distrustful

        "WELCOMING" ->
            Just Welcoming

        "ANARCHIST" ->
            Just Anarchist

        "CONFLICTED" ->
            Just Conflicted

        "AUTHORITARIAN" ->
            Just Authoritarian

        "OLIGARCHICAL" ->
            Just Oligarchical

        "DYNASTIC" ->
            Just Dynastic

        "DEMOCRACTIC" ->
            Just Democcractic

        "DECENTRALIZED" ->
            Just Decentralized

        "SMUGGLERS" ->
            Just Smugglers

        "SCAVENGERS" ->
            Just Scavengers

        "REBELLIOUS" ->
            Just Rebellious

        "EXILES" ->
            Just Exiles

        "PIRATES" ->
            Just Pirates

        "RAIDERS" ->
            Just Raiders

        "CLAN" ->
            Just Clan

        "GUILD" ->
            Just Guild

        "DOMINION" ->
            Just Dominion

        "FRINGE" ->
            Just Fringe

        "FORSAKEN" ->
            Just Forsaken

        "BOLD" ->
            Just Bold

        "VISIONARY" ->
            Just Visionary

        "CURIOUS" ->
            Just Curious

        "DARING" ->
            Just Daring

        "EXPLORATORY" ->
            Just Exploratory

        "RESOURCEFUL" ->
            Just Resourceful

        "FLEXIBLE" ->
            Just Flexible

        "COOPERATIVE" ->
            Just Cooperative

        "UNITED" ->
            Just United

        "STRATEGIC" ->
            Just Strategic

        "INTELLIGENT" ->
            Just Intelligent

        "RESEARCH_FOCUSED" ->
            Just ResearchFocused

        "COLLABORATIVE" ->
            Just Collaborative

        "PROGRESSIVE" ->
            Just Progressive

        "MILITARISTIC" ->
            Just Militaristic

        "TECHNOLOGICALLY_ADVANCED" ->
            Just TechnologicallyAdvanced

        "AGGRESSIVE" ->
            Just Aggressive

        "IMPERIALISTIC" ->
            Just Imperialistic

        "TREASURE_HUNTERS" ->
            Just TreasureHunters

        "DEXTEROUS" ->
            Just Dextrous

        "UNPREDICTABLE" ->
            Just Unpredictable

        "BRUTAL" ->
            Just Brutal

        "FLEETING" ->
            Just Fleeting

        "ADAPTABLE" ->
            Just Adaptable

        "SELF_SUFFICIENT" ->
            Just SelfSufficient

        "DEFENSIVE" ->
            Just Defensive

        "PROUD" ->
            Just Proud

        "DIVERSE" ->
            Just Diverse

        "INDEPENDENT" ->
            Just Independent

        "SELF_INTERESTED" ->
            Just SelfInterested

        "FRAGMENTED" ->
            Just Fragmented

        "COMMERCIAL" ->
            Just Commercial

        "FREE_MARKETS" ->
            Just FreeMarkets

        "ENTREPRENEURIAL" ->
            Just Entrepreneurial

        "ISOLATED" ->
            Just Isolated

        "LOCALIZED" ->
            Just Localized

        "ESTABLISHED" ->
            Just Established

        "NOTABLE" ->
            Just Notable

        "DOMINANT" ->
            Just Dominant

        "INESCAPABLE" ->
            Just Inescapable

        "INNOVATIVE" ->
            Just Innovative

        _ ->
            Nothing


decode : Json.Decode.Decoder Trait
decode =
    Json.Decode.map3
        (\name description symbol ->
            { name = name
            , description = description
            , style = symbol
            }
        )
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "description" Json.Decode.string)
        (Json.Decode.field "symbol" styleDecoder)


styleDecoder : Json.Decode.Decoder Style
styleDecoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case styleFromString str of
                    Just style ->
                        Json.Decode.succeed style

                    Nothing ->
                        Json.Decode.fail <| "Unknown style: " ++ str
            )
