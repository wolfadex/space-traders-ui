module Ui.System exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Ship
import SpaceTrader.System
import Ui
import Ui.System.Type
import Ui.System.Waypoint


view :
    { myShips : List SpaceTrader.Ship.Ship
    , onCreateSurveyClicked : { waypointId : String, shipId : String } -> msg
    }
    -> SpaceTrader.System.System
    -> Html msg
view opts system =
    -- { id : String
    -- , sector : String
    -- , type_ : SpaceTrader.System.Type.Type
    -- , x : Int
    -- , y : Int
    -- , waypoints : List SpaceTrader.System.Waypoint.Waypoint
    -- , factions : List String
    -- }
    Ui.column
        [ Ui.gap 1
        , Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        ]
        [ Ui.header.three [] [ Html.text system.id ]
        , Html.div [] <|
            Ui.viewLabeled
                { label = "Type"
                , value = Html.text <| Ui.System.Type.view system.type_
                }
        , Ui.header.four [] [ Html.text "Waypoints:" ]
        , Ui.column [ Ui.gap 0.5 ]
            (List.map
                (Ui.System.Waypoint.view opts)
                system.waypoints
            )
        ]
