module Ui.Button exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events


view : List (Html.Attribute msg) -> { label : Html msg, onClick : Maybe msg } -> Html msg
view attr { label, onClick } =
    Html.button
        (attr
            ++ [ Html.Attributes.style "cursor" "pointer"
               , case onClick of
                    Nothing ->
                        Html.Attributes.disabled True

                    Just msg ->
                        Html.Events.onClick msg
               ]
        )
        [ label ]
