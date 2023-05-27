module Ui.Ship.Cargo exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Ship.Cargo
import SpaceTrader.Ship.Cargo.Item
import Ui
import Ui.Ship.Cargo.Item


view :
    {}
    -> SpaceTrader.Ship.Cargo.Cargo
    -> Html msg
view opts cargo =
    -- type alias Cargo =
    --     { capcity : Int
    --     , units : Int
    --     , inventory : List Item
    --     }
    Html.div []
        [ Html.div []
            [ Ui.progress []
                { max = toFloat cargo.capcity
                , current = toFloat cargo.units
                }
            ]
        , cargo.inventory
            |> List.map (Ui.Ship.Cargo.Item.view {})
            |> Html.div
                [ Ui.grid
                , Html.Attributes.style "grid-template-columns" "1fr 1fr"
                , Ui.gap 1
                ]
        ]
