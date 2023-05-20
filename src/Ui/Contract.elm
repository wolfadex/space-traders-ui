module Ui.Contract exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Contract
import SpaceTrader.Contract.Good
import SpaceTrader.Contract.Term
import SpaceTrader.Faction
import Time
import Time.Distance
import Ui
import Ui.Button


view : { timeZone : Time.Zone, currentTime : Time.Posix, onDestinationClicked : String -> msg } -> SpaceTrader.Contract.Contract -> Html msg
view { timeZone, currentTime, onDestinationClicked } contract =
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        [ case contract.type_ of
            SpaceTrader.Contract.Porcurement ->
                Html.p []
                    [ let
                        isInFuture : Bool
                        isInFuture =
                            Time.posixToMillis currentTime < Time.posixToMillis contract.terms.deadline
                      in
                      Html.div
                        [ Html.Attributes.style "float" "right"
                        , Html.Attributes.style "margin-top" "-1rem"
                        , Html.Attributes.style "color" <|
                            if isInFuture then
                                ""

                            else
                                "red"
                        ]
                        [ Html.text <|
                            (if isInFuture then
                                "Expires "

                             else
                                "Expired"
                            )
                                ++ Time.Distance.inWords contract.terms.deadline currentTime
                        ]
                    , Html.text "For "
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
                                    , Ui.Button.link [ Html.Attributes.style "font-weight" "bold" ]
                                        { label = Html.text good.destinationSymbol
                                        , onClick =
                                            good.destinationSymbol
                                                |> String.split "-"
                                                |> List.take 2
                                                |> String.join "-"
                                                |> onDestinationClicked
                                                |> Just
                                        }
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

                    -- , important (Time.Distance.inWords contract.terms.deadline currentTime)
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

                    -- , important (Time.Distance.inWords contract.terms.deadline currentTime)
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
