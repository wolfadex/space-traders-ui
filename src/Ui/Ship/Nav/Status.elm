module Ui.Ship.Nav.Status exposing (..)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Ship.Nav.Status exposing (Status(..))
import Ui
import Ui.Button


view : { onDock : msg, onOrbit : msg, onMove : msg } -> SpaceTrader.Ship.Nav.Status.Status -> Html msg
view otps status =
    Ui.Button.multi [] <|
        case status of
            Docked ->
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
                [ { label = Html.text "Dock"
                  , onClick = otps.onDock
                  , selected = False
                  }
                , { label = Html.text "Orbiting"
                  , onClick = otps.onOrbit
                  , selected = True
                  }
                , { label = Html.text "Move"
                  , onClick = otps.onMove
                  , selected = False
                  }
                ]

            InTransit ->
                [ { label = Html.text "Orbit"
                  , onClick = otps.onOrbit
                  , selected = False
                  }
                , { label = Html.text "In Transit"
                  , onClick = otps.onMove
                  , selected = True
                  }
                ]
