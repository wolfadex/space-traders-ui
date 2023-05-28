module Ui.Ship.Nav.Status exposing (view)

import Html exposing (Html)
import SpaceTrader.Ship.Nav.Status exposing (Status(..))
import Ui
import Ui.Button


view : { a | onDock : msg, onOrbit : msg } -> SpaceTrader.Ship.Nav.Status.Status -> Html msg
view otps status =
    case status of
        Docked ->
            Ui.Button.multi []
                [ { label = Html.text "Docked"
                  , onClick = otps.onDock
                  , selected = True
                  }
                , { label = Html.text "Orbit"
                  , onClick = otps.onOrbit
                  , selected = False
                  }
                ]

        InOrbit ->
            Ui.Button.multi []
                [ { label = Html.text "Dock"
                  , onClick = otps.onDock
                  , selected = False
                  }
                , { label = Html.text "Orbiting"
                  , onClick = otps.onOrbit
                  , selected = True
                  }
                ]

        InTransit ->
            Ui.none
