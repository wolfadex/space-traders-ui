module Ui.Ship exposing (view)

import Html exposing (Html)
import Html.Attributes
import Route
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship
import Ui
import Ui.Ship.Nav.Status


view :
    { onDock : String -> msg
    , onOrbit : String -> msg
    , onMove : String -> msg
    }
    -> SpaceTrader.Ship.Ship
    -> Html msg
view opts ship =
    -- { id : String
    -- , registration : Registration
    -- , nav : Nav
    -- , crew : Crew
    -- , frame : Frame
    -- , reactor : Reactor
    -- , engine : Engine
    -- , modules : List Module
    -- , mounts : List Mount
    -- , cargo : Cargo
    -- , fuel : Fuel
    -- }
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        , Ui.grid
        , Ui.gap 0.5
        , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "ID:" ]
        , Html.span [] [ Html.text ship.id ]
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
                , onMove = opts.onMove ship.id
                }
                ship.nav.status
            ]
        ]
