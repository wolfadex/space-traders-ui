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
    Html.text ship.id
