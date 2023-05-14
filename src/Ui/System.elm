module Ui.System exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import SpaceTrader.System
import Ui
import Ui.System.Type
import Ui.System.Waypoint


view : SpaceTrader.System.System -> Html msg
view system =
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
                , value = Ui.System.Type.view system.type_
                }
        , Ui.header.four [] [ Html.text "Waypoints:" ]
        , Ui.column [] (List.map Ui.System.Waypoint.view system.waypoints)
        ]
