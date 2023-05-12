module Ui.Ship exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Faction
import SpaceTrader.Ship
import Time
import Time.Distance
import Ui


view : SpaceTrader.Ship.Ship -> Html msg
view ship =
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
        [ Html.text ship.id
        , Html.br [] []
        , Html.text "Fuel"
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        ]
