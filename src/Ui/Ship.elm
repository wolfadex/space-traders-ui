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
    Html.text ship.id
