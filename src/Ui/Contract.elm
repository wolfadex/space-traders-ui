module Ui.Contract exposing (view)

import Html exposing (Html)
import Html.Attributes
import Route
import SpaceTrader.Contract
import SpaceTrader.Faction
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import String.Extra
import Time
import Time.Distance
import Ui


view : { timeZone : Time.Zone, currentTime : Time.Posix } -> SpaceTrader.Contract.Contract -> Html msg
view { timeZone, currentTime } contract =
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
                        [ Ui.text <|
                            (if isInFuture then
                                "Expires "

                             else
                                "Expired"
                            )
                                ++ Time.Distance.inWords contract.terms.deadline currentTime
                        ]
                    , Ui.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Ui.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)
                    , Html.br [] []
                    , Ui.text "you must "
                    , important "procure"
                    , Ui.text " the following goods:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Ui.text " units of "
                                    , good.tradeSymbol
                                        |> String.replace "_" " "
                                        |> String.toLower
                                        |> String.Extra.toTitleCase
                                        |> important
                                    , Html.br [] []
                                    , Ui.text "and deliver them to "
                                    , let
                                        systemId : SpaceTrader.Point.System.System
                                        systemId =
                                            good.destinationSymbol
                                                |> SpaceTrader.Point.Waypoint.toSystem
                                      in
                                      Ui.link []
                                        { label =
                                            systemId
                                                |> SpaceTrader.Point.System.toLabel
                                                |> Ui.text
                                        , route = Route.fromSystem systemId
                                        }
                                    , Ui.text "-"
                                    , Ui.link []
                                        { label =
                                            good.destinationSymbol
                                                |> SpaceTrader.Point.Waypoint.toShortLabel
                                                |> Ui.text
                                        , route = Route.fromWaypoint good.destinationSymbol
                                        }
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]

            SpaceTrader.Contract.Transport ->
                Html.p []
                    [ Ui.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Ui.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)

                    -- , important (Time.Distance.inWords contract.terms.deadline currentTime)
                    , Html.br [] []
                    , Ui.text "you must "
                    , important "transport"
                    , Ui.text " the following:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Ui.text " units of "
                                    , important good.tradeSymbol
                                    , Html.br [] []
                                    , Ui.text "to "
                                    , important (SpaceTrader.Point.Waypoint.toLabel good.destinationSymbol)
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]

            SpaceTrader.Contract.Shuttle ->
                Html.p []
                    [ Ui.text "For "
                    , important (SpaceTrader.Faction.groupToPrettyString contract.factionGroup)
                    , Html.br [] []
                    , Ui.text " by "
                    , important (Ui.dateTime timeZone contract.terms.deadline)

                    -- , important (Time.Distance.inWords contract.terms.deadline currentTime)
                    , Html.br [] []
                    , Ui.text "you must "
                    , important "shuttle"
                    , Ui.text " the following:"
                    , Html.ul []
                        (List.map
                            (\good ->
                                Html.li []
                                    [ important (String.fromInt good.unitsRequired)
                                    , Ui.text " units of "
                                    , important good.tradeSymbol
                                    , Html.br [] []
                                    , Ui.text "to "
                                    , important (SpaceTrader.Point.Waypoint.toLabel good.destinationSymbol)
                                    ]
                            )
                            contract.terms.deliver
                        )
                    ]
        ]


important : String -> Html msg
important text =
    Html.span [ Html.Attributes.style "font-weight" "bold" ]
        [ Ui.text text ]
