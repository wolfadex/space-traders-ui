module Ui.System.Waypoint exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import SpaceTrader.Ship
import SpaceTrader.System.Waypoint
import Ui
import Ui.Waypoint.Type


view : { myShips : List SpaceTrader.Ship.Ship } -> SpaceTrader.System.Waypoint.Waypoint -> Html msg
view opts waypoint =
    -- { symbol : String
    -- , type_ : SpaceTrader.Waypoint.Type.Type
    -- , x : Int
    -- , y : Int
    -- }
    let
        shipsHere =
            opts.myShips
                |> List.filterMap
                    (\ship ->
                        if ship.nav.waypoint == waypoint.symbol then
                            Just ship.id

                        else
                            Nothing
                    )
    in
    Ui.row []
        [ waypoint.symbol
            |> String.split "-"
            |> List.drop 2
            |> String.join "-"
            |> Html.text
        , Html.text ": "
        , Html.text <| Ui.Waypoint.Type.view waypoint.type_
        , case shipsHere of
            [] ->
                Html.text ""

            _ ->
                shipsHere
                    |> String.join ", "
                    |> (\s -> " [ " ++ s ++ " ]")
                    |> Html.text
        ]
