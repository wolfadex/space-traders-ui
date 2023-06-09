module Ui.Modal exposing (view)

import Html exposing (Html)
import Html.Attributes


view :
    String
    -> List (Html.Attribute msg)
    -> List (Html msg)
    -> Html msg
view id attributes children =
    Html.node "dialog"
        (attributes
            ++ [ Html.Attributes.id id
               ]
        )
        children
