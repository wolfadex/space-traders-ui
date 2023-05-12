module Ui.Ship.Nav exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import SpaceTrader.Ship.Nav
import SpaceTrader.Ship.Nav.Status
import Ui
import Ui.Button
import Ui.Ship.Nav.Status


view : { onDock : msg, onOrbit : msg, onMove : msg } -> SpaceTrader.Ship.Nav.Nav -> Html msg
view opts nav =
    Ui.column
        []
        (Html.h3 [] [ Html.text "Nav" ]
            :: Ui.viewLabeled
                { label = "System"
                , value = nav.system
                }
            ++ Ui.viewLabeled
                { label = "Waypoint"
                , value = nav.waypoint
                }
            ++ [ Ui.Ship.Nav.Status.view opts
                    nav.status
               ]
        )
