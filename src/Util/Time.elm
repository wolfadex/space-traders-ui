module Util.Time exposing (isAfter)

import Time


{-| time1 |> Util.Time.isAfter time2
-}
isAfter : Time.Posix -> Time.Posix -> Bool
isAfter t2 t1 =
    Time.posixToMillis t1 > Time.posixToMillis t2
