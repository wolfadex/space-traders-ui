module Ui.Button exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Ui


default : List (Html.Attribute msg) -> { label : Html msg, onClick : Maybe msg } -> Html msg
default attr { label, onClick } =
    Html.button
        (attr
            ++ [ case onClick of
                    Nothing ->
                        Html.Attributes.disabled True

                    Just msg ->
                        Html.Events.onClick msg
               ]
        )
        [ label ]


primary : List (Html.Attribute msg) -> { label : Html msg, onClick : Maybe msg } -> Html msg
primary attr { label, onClick } =
    Html.button
        (attr
            ++ [ case onClick of
                    Nothing ->
                        Html.Attributes.disabled True

                    Just msg ->
                        Html.Events.onClick msg
               , Html.Attributes.style "background-color" "var(--blue-light)"
               , Html.Attributes.style "border-color" "var(--blur-dar)"
               ]
        )
        [ label ]


link : List (Html.Attribute msg) -> { label : Html msg, onClick : Maybe msg } -> Html msg
link attr { label, onClick } =
    Html.button
        (attr
            ++ [ Html.Attributes.class "button-link"
               , Html.Attributes.style "border" "none"
               , Html.Attributes.style "text-decoration" "underline"
               , case onClick of
                    Nothing ->
                        Html.Attributes.disabled True

                    Just msg ->
                        Html.Events.onClick msg
               ]
        )
        [ label ]


multi : List (Html.Attribute msg) -> List { label : Html msg, onClick : msg, selected : Bool } -> Html msg
multi attr btns =
    let
        lastBtnIndex =
            List.length btns - 1
    in
    Ui.row
        (attr ++ [])
        (List.indexedMap
            (\i { label, onClick, selected } ->
                Html.button
                    (attr
                        ++ [ Html.Attributes.style "grid-column" (String.fromInt (i + 1))

                           --    , Html.Attributes.style "border-radius" "3rem"
                           , Html.Attributes.style "border-top-left-radius" <|
                                if i == 0 then
                                    "3rem"

                                else
                                    "0"
                           , Html.Attributes.style "border-bottom-left-radius" <|
                                if i == 0 then
                                    "3rem"

                                else
                                    "0"
                           , Html.Attributes.style "border-top-right-radius" <|
                                if i == lastBtnIndex then
                                    "3rem"

                                else
                                    "0"
                           , Html.Attributes.style "border-bottom-right-radius" <|
                                if i == lastBtnIndex then
                                    "3rem"

                                else
                                    "0"
                           , Html.Attributes.style "border-style" "solid"
                           , Html.Attributes.style "background-color"
                                (if selected then
                                    "var(--primary-color)"

                                 else
                                    "var(--secondary-color)"
                                )
                           , Html.Attributes.style "color"
                                (if selected then
                                    "var(--secondary-color)"

                                 else
                                    "var(--primary-color)"
                                )
                           , Html.Attributes.style "padding" "0.5rem 1.5rem"
                           , Html.Events.onClick onClick
                           ]
                    )
                    [ label ]
            )
            btns
        )
