module Cacheable exposing
    ( Cacheable(..)
    , get
    , getData
    , insert
    , update
    )

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


get : String -> Cacheable a -> Maybe a
get key cacheable =
    case cacheable of
        Uncached ->
            Nothing

        Caching { data } ->
            Dict.get key data

        Cached data ->
            Dict.get key data


insert : String -> a -> Cacheable a -> Cacheable a
insert key value cacheable =
    case cacheable of
        Uncached ->
            Uncached

        Caching details ->
            Caching { details | data = Dict.insert key value details.data }

        Cached data ->
            Cached (Dict.insert key value data)


update : String -> (Maybe a -> Maybe a) -> Cacheable a -> Cacheable a
update key fn cacheable =
    case cacheable of
        Uncached ->
            Uncached

        Caching details ->
            Caching { details | data = Dict.update key fn details.data }

        Cached data ->
            Cached (Dict.update key fn data)
