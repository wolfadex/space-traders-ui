module Ui.Ship exposing (TransitForm, view)

import Form
import Form.Field
import Form.Validation
import Html exposing (Html)
import Html.Attributes
import Route
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


view :
    { onDock : String -> msg
    , onOrbit : String -> msg
    , onMove : String -> Ui.Form.Submission String TransitForm -> msg
    , onExtract : String -> msg
    , onRefresh : String -> msg
    , onRefreshCooldown : String -> msg
    , currentTime : Time.Posix
    , transitForm : Form.Model
    , onTransitFormMsg : String -> Form.Msg msg -> msg
    , transitableWaypoints : List SpaceTrader.Point.Waypoint.Waypoint
    }
    -> SpaceTrader.Ship.Ship
    -> Html msg
view opts ship =
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        , Ui.grid
        , Ui.gap 0.5
        , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "ID:" ]
        , Html.div
            [ Ui.grid
            , Ui.gap 0.5
            , Html.Attributes.style "grid-template-columns" "3fr 1fr"
            ]
            [ Html.span [] [ Html.text ship.id ]
            , Ui.Button.small [ Html.Attributes.style "padding" "0 0.5rem" ]
                { label = Html.text "Refresh"
                , onClick = Just (opts.onRefresh ship.id)
                }
            ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Fuel:" ]
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Waypoint:" ]
        , Html.span []
            [ Ui.link []
                { label =
                    ship.nav.system
                        |> SpaceTrader.Point.System.toLabel
                        |> Html.text
                , route = Route.fromSystem ship.nav.system
                }
            , Html.text "-"
            , Ui.link []
                { label =
                    ship.nav.waypoint
                        |> SpaceTrader.Point.Waypoint.toShortLabel
                        |> Html.text
                , route = Route.fromWaypoint ship.nav.waypoint
                }
            ]
        , Html.div
            [ Html.Attributes.style "grid-column" "1 / 3" ]
            [ Ui.Ship.Nav.Status.view
                { onDock = opts.onDock ship.id
                , onOrbit = opts.onOrbit ship.id

                -- , onMove = opts.onMove ship.id
                }
                ship.nav.status
            ]
        , Html.span
            [ Html.Attributes.style "font-weight" "bold" ]
            [ Html.text "Actions:" ]
        , Html.div [ Ui.grid, Ui.gap 0.5 ]
            [ case ship.nav.status of
                SpaceTrader.Ship.Nav.Status.InOrbit ->
                    Ui.Button.default []
                        { label =
                            Html.text <|
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

                _ ->
                    Ui.none
            , case ship.nav.status of
                SpaceTrader.Ship.Nav.Status.InOrbit ->
                    Form.renderHtml
                        { submitting = False
                        , state = opts.transitForm
                        , toMsg = opts.onTransitFormMsg ship.id
                        }
                        (Form.options ("ship-transit-form-" ++ ship.id)
                            |> Form.withOnSubmit (opts.onMove ship.id)
                        )
                        [ Ui.grid
                        , Ui.gap 1
                        ]
                        (transitForm opts.transitableWaypoints)

                _ ->
                    Ui.none
            ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Cargo:" ]
        , Html.span [] [ Ui.Ship.Cargo.view ship.cargo ]
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
