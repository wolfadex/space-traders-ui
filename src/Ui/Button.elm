module Ui.Button exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events


default : List (Html.Attribute msg) -> { label : Html msg, onClick : Maybe msg } -> Html msg
default attr { label, onClick } =
    Html.button
        (attr
            ++ [ Html.Attributes.style "cursor" "pointer"
               , Html.Attributes.style "border-radius" "3rem"
               , Html.Attributes.style "border-style" "solid"
               , Html.Attributes.style "padding" "0.5rem 1.5rem"
               , case onClick of
                    Nothing ->
                        Html.Attributes.disabled True

                    Just msg ->
                        Html.Events.onClick msg
               ]
        )
        [ label ]
