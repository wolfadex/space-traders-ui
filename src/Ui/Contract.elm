module Ui.Contract exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Contract
import SpaceTrader.Contract.Good
import SpaceTrader.Contract.Term
import SpaceTrader.Faction
import Time
import Ui


view : Time.Zone -> SpaceTrader.Contract.Contract -> Html msg
view timeZone contract =
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid black"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        [ case contract.type_ of
            SpaceTrader.Contract.Porcurement ->
                Html.p []
                    [ Html.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Html.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)
                    , Html.br [] []
                    , Html.text "you must "
                    , important "procure"
                    , Html.text " the following goods:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Html.text " units of "
                                    , important good.tradeSymbol
                                    , Html.br [] []
                                    , Html.text "and deliver them to "
                                    , important good.destinationSymbol
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]

            SpaceTrader.Contract.Transport ->
                Html.p []
                    [ Html.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Html.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)
                    , Html.br [] []
                    , Html.text "you must "
                    , important "transport"
                    , Html.text " the following:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Html.text " units of "
                                    , important good.tradeSymbol
                                    , Html.br [] []
                                    , Html.text "to "
                                    , important good.destinationSymbol
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]

            SpaceTrader.Contract.Shuttle ->
                Html.p []
                    [ Html.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Html.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)
                    , Html.br [] []
                    , Html.text "you must "
                    , important "shuttle"
                    , Html.text " the following:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Html.text " units of "
                                    , important good.tradeSymbol
                                    , Html.br [] []
                                    , Html.text "to "
                                    , important good.destinationSymbol
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]
        ]


important : String -> Html msg
important text =
    Html.span [ Html.Attributes.style "font-weight" "bold" ]
        [ Html.text text ]
