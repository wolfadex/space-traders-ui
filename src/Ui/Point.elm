module Ui.Point exposing (view)

import SpaceTrader.Point


view : SpaceTrader.Point.Point -> String
view point =
    List.filterMap identity
        [ Just point.sector
        , Just point.system
        , point.point
        ]
        |> String.join "-"
