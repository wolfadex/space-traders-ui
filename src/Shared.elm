module Shared exposing (LightYear(..), Model, Msg(..), ScaledViewPoint(..), SpaceFocus(..), init, pushNotification, scalePointInLightYearsToOne, subscriptions, update, viewModals, viewNotifications)

import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes
import Json.Encode
import Length exposing (Meters)
import List.NonEmpty
import Point3d exposing (Point3d)
import Port
import SpaceTrader.Point.SystemDict as SystemDict exposing (SystemDict)
import SpaceTrader.System
import Task
import Time
import Ui
import Ui.Button
import Ui.Modal
import Ui.Notification
import Ui.Select
import Ui.Theme
import Update exposing (Update)


type alias Model =
    { timeZone : Time.Zone
    , currentTime : Time.Posix
    , theme : Ui.Theme.Theme
    , notifications : List Ui.Notification.TimedNotification
    }


type SpaceFocus
    = FGalaxy
    | FSystem String
    | FWaypoint String


type ScaledViewPoint
    = ScaledViewPoint Never


type LightYear
    = LightYear Never


init :
    { theme : Maybe Ui.Theme.Theme
    , systems : Maybe (SystemDict SpaceTrader.System.System)
    }
    -> ( Model, Cmd Msg )
init opts =
    ( { timeZone = Time.utc
      , currentTime = Time.millisToPosix 0
      , theme =
            case opts.theme of
                Nothing ->
                    List.NonEmpty.head Ui.Theme.themes

                Just theme ->
                    theme
      , notifications = []
      }
    , Task.map2 CurrentTimeAndZoneReceived
        Time.now
        Time.here
        |> Task.perform identity
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every
        (case model.notifications of
            [] ->
                -- 5 minutes
                5 * 1000 * 60

            _ ->
                -- 3 seconds
                3 * 1000
        )
        CurrentTimeReceived


type
    Msg
    -- time
    = CurrentTimeReceived Time.Posix
    | CurrentTimeAndZoneReceived Time.Posix Time.Zone
    | TimedNotificationCreated ( Time.Posix, Ui.Notification.TimedNotification )
      -- settings
    | OpenSettingsClicked
    | CloseSettingsClicked
    | ThemeSelected Ui.Theme.Theme


update :
    { msg : Msg
    , model : Model
    , toMsg : Msg -> msg
    , toModel : Model -> model
    }
    -> Update model msg
update ({ model } as opts) =
    Update.mapModel opts.toModel <|
        Update.mapMsg opts.toMsg <|
            case opts.msg of
                -- time
                CurrentTimeReceived currentTime ->
                    { model
                        | currentTime = currentTime
                        , notifications = List.filter (Ui.Notification.isUnexpired currentTime) model.notifications
                    }
                        |> Update.succeeed

                CurrentTimeAndZoneReceived time zone ->
                    { model
                        | currentTime = time
                        , timeZone = zone
                    }
                        |> Update.succeeed

                TimedNotificationCreated ( currentTime, notification ) ->
                    { model
                        | notifications = notification :: List.filter (Ui.Notification.isUnexpired currentTime) model.notifications
                        , currentTime = currentTime
                    }
                        |> Update.succeeed

                -- settings
                OpenSettingsClicked ->
                    model
                        |> Update.succeeed
                        |> Update.withCmd
                            (Port.openModal modalIds.settings)

                CloseSettingsClicked ->
                    model
                        |> Update.succeeed
                        |> Update.withCmd
                            (Cmd.batch
                                [ Port.closeModal modalIds.settings
                                , saveSettings model
                                ]
                            )

                ThemeSelected theme ->
                    { model | theme = theme }
                        |> Update.succeeed


pushNotification : { model : Model, notification : Ui.Notification.Notification, toMsg : Msg -> msg, toModel : Model -> model } -> Update model msg
pushNotification opts =
    opts.model
        |> Update.succeeed
        |> Update.withCmd
            (Time.now
                |> Task.map (\now -> ( now, Ui.Notification.toTimed opts.notification now ))
                |> Task.perform TimedNotificationCreated
            )
        |> Update.mapMsg opts.toMsg
        |> Update.mapModel opts.toModel


saveSettings : Model -> Cmd msg
saveSettings model =
    Port.storeSettings
        (Json.Encode.object
            [ ( "theme", Ui.Theme.encode model.theme )
            ]
        )


scalePointInLightYearsToOne : Point3d Meters LightYear -> Point3d Meters ScaledViewPoint
scalePointInLightYearsToOne point =
    Point3d.fromMeters
        { x = Length.inMeters (Point3d.xCoordinate point)
        , y = Length.inMeters (Point3d.yCoordinate point)
        , z = Length.inMeters (Point3d.zCoordinate point)
        }


modalIds : { settings : String }
modalIds =
    { settings = "settings" }


viewModals : Model -> List (Html Msg)
viewModals model =
    [ Ui.Modal.view modalIds.settings
        [ Html.Attributes.class model.theme.class
        ]
        [ Ui.column
            [ Ui.gap 1 ]
            [ Html.label
                []
                [ Html.text "Theme: "
                , Ui.Select.view
                    []
                    { options = List.NonEmpty.toList Ui.Theme.themes
                    , toString = .label
                    , value = model.theme
                    , onChange = ThemeSelected
                    }
                ]
            , Ui.Button.default
                [ Ui.justify.end
                , Ui.align.end
                ]
                { label = Html.text "Save Settings"
                , onClick = Just CloseSettingsClicked
                }
            ]
        ]
    ]


viewNotifications : Model -> Html Msg
viewNotifications model =
    Html.div [ Html.Attributes.class "notifications" ]
        (List.map Ui.Notification.view model.notifications)
