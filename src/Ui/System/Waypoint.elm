module Ui.System.Waypoint exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import SpaceTrader.System.Waypoint
import Ui
import Ui.Waypoint.Type


view : SpaceTrader.System.Waypoint.Waypoint -> Html msg
view waypoint =
    Ui.row []
        [ waypoint.symbol
            |> String.split "-"
            |> List.drop 2
            |> String.join "-"
            |> Html.text
        , Html.text ": "
        , Html.text <| Ui.Waypoint.Type.view waypoint.type_
        ]
