module List.NonEmpty exposing (..)


type NonEmpty a
    = NonEmpty a (List a)


init : a -> List a -> NonEmpty a
init h t =
    NonEmpty h t


toList : NonEmpty a -> List a
toList (NonEmpty h t) =
    h :: t


head : NonEmpty a -> a
head (NonEmpty h _) =
    h


find : (a -> Bool) -> NonEmpty a -> Maybe a
find predicate (NonEmpty h t) =
    if predicate h then
        Just h

    else
        findHelper predicate t


findHelper : (a -> Bool) -> List a -> Maybe a
findHelper predicate list =
    case list of
        [] ->
            Nothing

        h :: t ->
            if predicate h then
                Just h

            else
                findHelper predicate t
