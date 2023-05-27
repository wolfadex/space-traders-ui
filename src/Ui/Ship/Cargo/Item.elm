module Ui.Ship.Cargo.Item exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Ship.Cargo.Item


view :
    SpaceTrader.Ship.Cargo.Item.Item
    -> Html msg
view item =
    -- type alias Item =
    --     { symbol : String
    --     , name : String
    --     , description : String
    --     , units : Int
    --     }
    Html.div [ Html.Attributes.title item.description ]
        [ Html.span []
            [ Html.text item.symbol
            , Html.text " "
            , Html.text "x"
            , Html.text <| String.fromInt item.units
            ]
        ]
