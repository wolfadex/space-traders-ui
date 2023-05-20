module RemoteData exposing (..)


type RemoteData a
    = Loading
    | Failure String
    | Loaded a


fromResult : Result String a -> RemoteData a
fromResult result =
    case result of
        Ok a ->
            Loaded a

        Err e ->
            Failure e


mapToString : (a -> String) -> RemoteData a -> String
mapToString fn remoteData =
    case remoteData of
        Loading ->
            "Loading"

        Failure e ->
            e

        Loaded a ->
            fn a
