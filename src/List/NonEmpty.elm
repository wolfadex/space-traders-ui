module List.NonEmpty exposing (NonEmpty(..), head, init, toList)


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
