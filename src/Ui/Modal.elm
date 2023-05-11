module Ui.Modal exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode
import Ui


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
