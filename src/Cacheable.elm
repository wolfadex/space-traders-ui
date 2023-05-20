module Cacheable exposing (..)

import Dict exposing (Dict)


type Cacheable a
    = Uncached
    | Caching { data : Dict String a, current : Int, max : Int }
    | Cached (Dict String a)


getData : Cacheable a -> Dict String a
getData cacheable =
    case cacheable of
        Uncached ->
            Dict.empty

        Caching { data } ->
            data

        Cached data ->
            data
