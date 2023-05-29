module RemoteData exposing
    ( RemoteData(..)
    , fromResult
    , map
    , refresh
    , toMaybe
    )


type RemoteData a
    = Loading
    | Failure String
    | Loaded a
    | Refreshing a


fromResult : Result String a -> RemoteData a
fromResult result =
    case result of
        Ok a ->
            Loaded a

        Err e ->
            Failure e


toMaybe : RemoteData a -> Maybe a
toMaybe remoteData =
    case remoteData of
        Loaded a ->
            Just a

        _ ->
            Nothing


map : (a -> b) -> RemoteData a -> RemoteData b
map f remoteData =
    case remoteData of
        Loading ->
            Loading

        Failure e ->
            Failure e

        Loaded a ->
            Loaded (f a)

        Refreshing a ->
            Refreshing (f a)


refresh : RemoteData a -> RemoteData a
refresh remoteData =
    case remoteData of
        Loading ->
            Loading

        Failure _ ->
            Loading

        Loaded a ->
            Refreshing a

        Refreshing a ->
            Refreshing a
