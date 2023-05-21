module Cacheable exposing
    ( Cacheable(..)
    , get
    , getData
    , insert
    , update
    )


type Cacheable data
    = Uncached
    | Caching { data : data, current : Int, max : Int }
    | Cached data


getData : Cacheable data -> Maybe data
getData cacheable =
    case cacheable of
        Uncached ->
            Nothing

        Caching { data } ->
            Just data

        Cached data ->
            Just data


get : (key -> data -> Maybe a) -> key -> Cacheable data -> Maybe a
get fn key cacheable =
    case cacheable of
        Uncached ->
            Nothing

        Caching { data } ->
            fn key data

        Cached data ->
            fn key data


insert : (key -> value -> data -> data) -> key -> value -> Cacheable data -> Cacheable data
insert fn key value cacheable =
    case cacheable of
        Uncached ->
            Uncached

        Caching details ->
            Caching { details | data = fn key value details.data }

        Cached data ->
            Cached (fn key value data)


update : (key -> (Maybe value -> Maybe value) -> data -> data) -> key -> (Maybe value -> Maybe value) -> Cacheable data -> Cacheable data
update fn_ key fn cacheable =
    case cacheable of
        Uncached ->
            Uncached

        Caching details ->
            Caching { details | data = fn_ key fn details.data }

        Cached data ->
            Cached (fn_ key fn data)
