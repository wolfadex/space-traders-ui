module Ui.System exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Id exposing (ShipId)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship
import SpaceTrader.System
import Ui
import Ui.System.Type
import Ui.System.Waypoint


view :
    { myShips : List SpaceTrader.Ship.Ship
    , onCreateSurveyClicked : { waypointId : SpaceTrader.Point.Waypoint.Waypoint, shipId : ShipId } -> msg
    }
    -> SpaceTrader.System.System
    -> Html msg
view opts system =
    Html.div
        [ Ui.grid
        , Ui.gap 1
        , Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        [ Ui.header.three []
            [ system.id
                |> SpaceTrader.Point.System.toLabel
                |> Ui.text
            ]
        , Html.div
            [ Ui.grid
            , Html.Attributes.style "grid-template-columns" "4rem 1fr"
            ]
            [ Html.span [ Html.Attributes.style "font-weight" "bold" ]
                [ Ui.text "Type:" ]
            , Html.span [] [ Ui.text <| Ui.System.Type.view system.type_ ]
            ]
        , Ui.header.four [] [ Ui.text "Waypoints:" ]
        , Ui.column [ Ui.gap 0.5 ]
            (List.map
                (Ui.System.Waypoint.view opts)
                system.waypoints
            )
        ]
