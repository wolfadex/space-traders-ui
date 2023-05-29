module Ui.Ship exposing
    ( TransitForm
    , view
    , viewBrief
    )

import Form
import Form.Field
import Form.Validation
import Html exposing (Html)
import Html.Attributes
import Id
import Route
import SpaceTrader.Id exposing (ShipId)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship
import SpaceTrader.Ship.Nav.Status
import Time
import Time.Distance
import Ui
import Ui.Button
import Ui.Form
import Ui.Form.Field
import Ui.Ship.Cargo
import Ui.Ship.Nav.Status
import Util.Time


viewBrief : SpaceTrader.Ship.Ship -> Html msg
viewBrief ship =
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        , Ui.grid
        , Ui.gap 0.5
        , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "ID:" ]
        , Ui.link []
            { label = Ui.text (Id.toLabel ship.id)
            , route = Route.fromShip ship.id
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "Waypoint:" ]
        , Html.span []
            [ Ui.link []
                { label =
                    ship.nav.system
                        |> SpaceTrader.Point.System.toLabel
                        |> Ui.text
                , route = Route.fromSystem ship.nav.system
                }
            , Ui.text "-"
            , Ui.link []
                { label =
                    ship.nav.waypoint
                        |> SpaceTrader.Point.Waypoint.toShortLabel
                        |> Ui.text
                , route = Route.fromWaypoint ship.nav.waypoint
                }
            ]
        , Html.span
            [ Html.Attributes.style "font-weight" "bold"
            ]
            [ Ui.text "Fuel:" ]
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "Cargo:" ]
        , Html.span [] [ Ui.Ship.Cargo.viewBrief ship.cargo ]
        ]


view :
    { onDock : ShipId -> msg
    , onOrbit : ShipId -> msg
    , onMove : ShipId -> Ui.Form.Submission String TransitForm -> msg
    , onExtract : ShipId -> msg
    , onRefresh : ShipId -> msg
    , onRefreshCooldown : ShipId -> msg
    , currentTime : Time.Posix
    , transitForm : Form.Model
    , onTransitFormMsg : ShipId -> Form.Msg msg -> msg
    , transitableWaypoints : List SpaceTrader.Point.Waypoint.Waypoint
    }
    -> SpaceTrader.Ship.Ship
    -> Html msg
view opts ship =
    Html.div
        [ Ui.gap 0.5
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        ]
        [ Html.div
            [ Ui.grid
            , Ui.gap 1
            , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
            ]
            [ Ui.Button.small
                [ Html.Attributes.style "padding" "0 0.5rem"
                ]
                { label = Ui.text "Refresh"
                , onClick = Just (opts.onRefresh ship.id)
                }
            ]
        , labeled "Fuel" <|
            Ui.progress [ Html.Attributes.style "max-width" "20rem" ]
                { max = toFloat ship.fuel.capacity
                , current = toFloat ship.fuel.current
                }
        , labeled "Waypoint" <|
            Html.span []
                [ Ui.link []
                    { label =
                        ship.nav.system
                            |> SpaceTrader.Point.System.toLabel
                            |> Ui.text
                    , route = Route.fromSystem ship.nav.system
                    }
                , Ui.text "-"
                , Ui.link []
                    { label =
                        ship.nav.waypoint
                            |> SpaceTrader.Point.Waypoint.toShortLabel
                            |> Ui.text
                    , route = Route.fromWaypoint ship.nav.waypoint
                    }
                ]
        , ship.nav.status
            |> SpaceTrader.Ship.Nav.Status.prettyPrint
            |> Ui.text
            |> labeled "Status"
        , labeled "Actions" <|
            Html.div [ Ui.grid, Ui.gap 0.5, Html.Attributes.style "max-width" "30rem" ]
                (case ship.nav.status of
                    SpaceTrader.Ship.Nav.Status.InOrbit ->
                        [ Ui.Button.default []
                            { label = Ui.text "Dock"
                            , onClick = Just (opts.onDock ship.id)
                            }
                        , Ui.Button.default []
                            { label =
                                Ui.text <|
                                    case ship.cooldown of
                                        Just cooldown ->
                                            if cooldown.expiration |> Util.Time.isAfter opts.currentTime then
                                                "Cooldown complete " ++ Time.Distance.inWords cooldown.expiration opts.currentTime ++ "..."

                                            else
                                                "Extract"

                                        Nothing ->
                                            "Extract"
                            , onClick =
                                case ship.cooldown of
                                    Just cooldown ->
                                        if cooldown.expiration |> Util.Time.isAfter opts.currentTime then
                                            Nothing

                                        else
                                            Just <| opts.onExtract ship.id

                                    Nothing ->
                                        Just <| opts.onExtract ship.id
                            }
                        , Form.renderHtml
                            { submitting = False
                            , state = opts.transitForm
                            , toMsg = opts.onTransitFormMsg ship.id
                            }
                            (Form.options ("ship-transit-form-" ++ Id.toString ship.id)
                                |> Form.withOnSubmit (opts.onMove ship.id)
                            )
                            [ Ui.grid
                            , Ui.gap 1
                            ]
                            (transitForm opts.transitableWaypoints)
                        ]

                    SpaceTrader.Ship.Nav.Status.InTransit ->
                        [ Ui.text
                            ("Arriving at "
                                ++ SpaceTrader.Point.Waypoint.toShortLabel ship.nav.route.destination.symbol
                                ++ " in "
                                ++ Time.Distance.inWords ship.nav.route.arrival opts.currentTime
                            )
                        ]

                    SpaceTrader.Ship.Nav.Status.Docked ->
                        []
                )
        , labeled "Cargo" <|
            Html.span [ Html.Attributes.style "max-width" "30rem" ] [ Ui.Ship.Cargo.view ship.cargo ]
        ]


labeled : String -> Html msg -> Html msg
labeled label content =
    Html.div
        [ Ui.grid
        , Ui.gap 1
        , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text (label ++ ":") ]
        , content
        ]


type alias TransitForm =
    { destination : SpaceTrader.Point.Waypoint.Waypoint
    }


transitForm : List SpaceTrader.Point.Waypoint.Waypoint -> Form.HtmlForm String TransitForm input msg
transitForm transitableWaypoints =
    (\destination ->
        { combine =
            Form.Validation.succeed TransitForm
                |> Form.Validation.andMap destination
        , view =
            \formState ->
                List.concat
                    [ Ui.Form.Field.select { toString = SpaceTrader.Point.Waypoint.toShortLabel }
                        formState
                        "Travel to"
                        destination
                    , Ui.Form.Field.submit
                        { label =
                            if formState.submitting then
                                "In transit..."

                            else
                                "Travel"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "destination"
            (Form.Field.select
                (List.map (\waypoint -> ( SpaceTrader.Point.Waypoint.toKey waypoint, waypoint ))
                    transitableWaypoints
                )
                (\_ -> "Invalid")
                |> Form.Field.required "Select a destination"
            )
