module Util.Result exposing (..)


onError : (x -> Result y a) -> Result x a -> Result y a
onError f result =
    case result of
        Ok a ->
            Ok a

        Err x ->
            f x
