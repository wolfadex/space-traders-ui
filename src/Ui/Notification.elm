module Ui.Notification exposing (Notification(..), Style(..), TimedNotification(..), isUnexpired, new, toTimed, view, withAlert)

import Html exposing (Html)
import Html.Attributes
import Time


type Notification
    = Notification { message : String, style : Style }


type TimedNotification
    = TimedNotification { message : String, style : Style, expiration : Time.Posix }


type Style
    = Alert
    | Success
    | Warning
    | Info


new : String -> Notification
new message =
    Notification { message = message, style = Info }


withAlert : Notification -> Notification
withAlert (Notification notification) =
    Notification { notification | style = Alert }


toTimed : Notification -> Time.Posix -> TimedNotification
toTimed (Notification notification) currentTime =
    TimedNotification
        { message = notification.message
        , style = notification.style
        , expiration =
            currentTime
                |> Time.posixToMillis
                |> (+) (1000 * 3)
                |> Time.millisToPosix
        }


view : TimedNotification -> Html msg
view (TimedNotification notification) =
    Html.div
        [ Html.Attributes.class
            ("notification-"
                ++ (case notification.style of
                        Alert ->
                            "alert"

                        Success ->
                            "success"

                        Warning ->
                            "warning"

                        Info ->
                            "info"
                   )
            )
        ]
        [ Html.text notification.message ]


isUnexpired : Time.Posix -> TimedNotification -> Bool
isUnexpired now (TimedNotification notification) =
    Time.posixToMillis now < Time.posixToMillis notification.expiration
