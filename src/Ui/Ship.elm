module Ui.Ship exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Faction
import SpaceTrader.Ship
import SpaceTrader.Ship.Nav.Status
import Time
import Time.Distance
import Ui
import Ui.Ship.Nav
import Ui.Ship.Nav.Status


view :
    { onDock : String -> msg
    , onOrbit : String -> msg
    , onMove : String -> msg
    , onSystemClicked : String -> msg
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
        , Html.Attributes.style "grid-template-columns" "6rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "ID:" ]
        , Html.span [] [ Html.text ship.id ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Fuel:" ]
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "System:" ]

        -- , Ui.Button.link [ Html.Attributes.style "width" "fit-content" ]
        --     { label = Html.text nav.system
        --     , onClick =
        --         nav.system
        --             |> opts.onSystemClicked
        --             |> Just
        --     }
        , Html.span [] [ Html.text ship.nav.system ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Waypoint:" ]
        , Html.span [] [ Html.text ship.nav.waypoint ]
        , Html.div
            [ Html.Attributes.style "grid-column" "1 / 3" ]
            [ let
                carl : Html msg
                carl =
                    Ui.Ship.Nav.Status.view
                        { onDock = opts.onDock ship.id
                        , onOrbit = opts.onOrbit ship.id
                        , onMove = opts.onMove ship.id
                        }
                        ship.nav.status
              in
              carl
            ]
        ]
