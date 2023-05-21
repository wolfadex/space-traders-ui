module RemoteData exposing (RemoteData(..), fromResult)


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
