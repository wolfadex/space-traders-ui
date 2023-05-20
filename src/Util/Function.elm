module Util.Function exposing (..)


applyMaybe : Maybe a -> (a -> b -> b) -> b -> b
applyMaybe maybeVal fn b =
    case maybeVal of
        Just a ->
            fn a b

        Nothing ->
            b
