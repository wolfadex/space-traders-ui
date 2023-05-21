module Util.Maybe exposing (..)


onNothing : Maybe a -> Maybe a -> Maybe a
onNothing fallback maybe =
    case maybe of
        Just value ->
            Just value

        Nothing ->
            fallback
