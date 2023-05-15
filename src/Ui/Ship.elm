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
    Ui.column
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        [ Html.div
            [ Html.Attributes.style "display" "grid" ]
            (Ui.viewLabeled { label = "Name", value = Html.text <| ship.id })
        , Html.h3 [] [ Html.text "Fuel" ]
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        , Ui.Ship.Nav.view
            { onDock = opts.onDock ship.id
            , onOrbit = opts.onOrbit ship.id
            , onMove = opts.onMove ship.id
            , onSystemClicked = opts.onSystemClicked
            }
            ship.nav
        ]
