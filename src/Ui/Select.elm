module Ui.Select exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import Ui


view :
    List (Html.Attribute msg)
    ->
        { options : List a
        , toString : a -> String
        , value : a
        , onChange : a -> msg
        }
    -> Html msg
view attr opts =
    Html.select
        (attr
            ++ [ Html.Attributes.value (opts.toString opts.value)
               , Html.Events.onInput
                    (\newValue ->
                        opts.options
                            |> find (opts.toString >> (==) newValue)
                            |> Maybe.withDefault opts.value
                            |> opts.onChange
                    )
               , Html.Attributes.style "border-radius" "0.1rem"
               , Html.Attributes.style "border-style" "solid"
               , Html.Attributes.style "padding" "0.25rem 1rem"
               ]
        )
        (List.map
            (\option ->
                Html.option
                    [ Html.Attributes.value (opts.toString option) ]
                    [ Ui.text (opts.toString option) ]
            )
            opts.options
        )


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        h :: t ->
            if predicate h then
                Just h

            else
                find predicate t
