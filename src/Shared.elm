module Shared exposing
    ( LightYear(..)
    , Model
    , Msg(..)
    , ScaledViewPoint(..)
    , Settings
    , SpaceFocus(..)
    , decodeSettings
    , init
    , modalIds
    , pushNotification
    , scalePointInLightYearsToOne
    , subscriptions
    , update
    , viewModals
    , viewNotifications
    )

import Dict exposing (Dict)
import Form
import Form.Field
import Form.Validation
import Html exposing (Html)
import Html.Attributes
import Json.Decode
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
import Ui.Form
import Ui.Form.Field
import Ui.Modal
import Ui.Notification
import Ui.Select
import Update exposing (Update)


type alias Model =
    { timeZone : Time.Zone
    , currentTime : Time.Posix
    , notifications : List Ui.Notification.TimedNotification
    , settings : Settings
    , settingsFormModel : Form.Model
    }


type SpaceFocus
    = FGalaxy
    | FSystem String
    | FWaypoint String


type ScaledViewPoint
    = ScaledViewPoint Never


type LightYear
    = LightYear Never


type alias Settings =
    { systemLimit : Int }


defaultSettings : Settings
defaultSettings =
    { systemLimit = 1000
    }


decodeSettings : Json.Decode.Decoder Settings
decodeSettings =
    Json.Decode.map
        (\systemLimit ->
            { systemLimit = systemLimit
            }
        )
        (Json.Decode.maybe (Json.Decode.field "systemLimit" Json.Decode.int)
            |> Json.Decode.map (Maybe.withDefault 1000)
        )


saveSettings : Settings -> Cmd msg
saveSettings settings =
    Port.storeSettings
        (Json.Encode.object
            [ ( "systemLimit", Json.Encode.int settings.systemLimit )
            ]
        )


init :
    { systems : Maybe (SystemDict SpaceTrader.System.System)
    , settings : Maybe Settings
    }
    -> ( Model, Cmd Msg )
init opts =
    ( { timeZone = Time.utc
      , currentTime = Time.millisToPosix 0
      , notifications = []
      , settings = Maybe.withDefault defaultSettings opts.settings
      , settingsFormModel = Form.init
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
    | SettingsFormMsg (Form.Msg Msg)
    | SettingsFormSubmitted (Ui.Form.Submission String Settings)


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
                        |> Update.succeed

                CurrentTimeAndZoneReceived time zone ->
                    { model
                        | currentTime = time
                        , timeZone = zone
                    }
                        |> Update.succeed

                TimedNotificationCreated ( currentTime, notification ) ->
                    { model
                        | notifications = notification :: List.filter (Ui.Notification.isUnexpired currentTime) model.notifications
                        , currentTime = currentTime
                    }
                        |> Update.succeed

                -- settings
                OpenSettingsClicked ->
                    model
                        |> Update.succeed
                        |> Update.withCmd
                            (Port.openModal modalIds.settings)

                SettingsFormMsg msg_ ->
                    let
                        ( settingsFormModel, formCmd ) =
                            Form.update msg_ model.settingsFormModel
                    in
                    { model
                        | settingsFormModel = settingsFormModel
                    }
                        |> Update.succeed
                        |> Update.withCmd formCmd

                SettingsFormSubmitted { parsed } ->
                    case parsed of
                        Form.Valid newSettings ->
                            { model | settings = newSettings }
                                |> Update.succeed
                                |> Update.withCmd
                                    (Cmd.batch
                                        [ Port.closeModal modalIds.settings
                                        , saveSettings newSettings
                                        ]
                                    )

                        Form.Invalid _ _ ->
                            model
                                |> Update.succeed


pushNotification : { model : Model, notification : Ui.Notification.Notification, toMsg : Msg -> msg, toModel : Model -> model } -> Update model msg
pushNotification opts =
    opts.model
        |> Update.succeed
        |> Update.withCmd
            (Time.now
                |> Task.map (\now -> ( now, Ui.Notification.toTimed opts.notification now ))
                |> Task.perform TimedNotificationCreated
            )
        |> Update.mapMsg opts.toMsg
        |> Update.mapModel opts.toModel


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
        []
        [ Ui.column
            [ Ui.gap 1 ]
            [ Ui.column
                [ Html.Attributes.style "border" "0.125rem solid "
                , Html.Attributes.style "border-radius" "0.5rem"
                , Html.Attributes.style "max-width" "50rem"
                , Html.Attributes.style "padding" "1rem"
                , Html.Attributes.style "background-color" "var(--blue)"
                , Ui.gap 1
                ]
                [ Ui.header.two
                    [ Html.Attributes.style "border-bottom" "0.125rem solid "
                    ]
                    [ Html.text "Settings" ]
                , Form.renderHtml
                    { submitting = False
                    , state = model.settingsFormModel
                    , toMsg = SettingsFormMsg
                    }
                    (Form.options "settings-form"
                        |> Form.withInput model.settings
                        |> Form.withOnSubmit SettingsFormSubmitted
                    )
                    [ Html.Attributes.style "display" "grid"
                    , Html.Attributes.style "gap" "1rem"
                    ]
                    settingsForm
                ]
            ]
        ]
    ]


settingsForm : Form.HtmlForm String Settings Settings Msg
settingsForm =
    (\systemLimit ->
        { combine =
            Form.Validation.succeed Settings
                |> Form.Validation.andMap systemLimit
        , view =
            \formState ->
                List.concat
                    [ [ Ui.header.three [] [ Html.text "Performance" ] ]
                    , Ui.Form.Field.text
                        { formState = formState
                        , hint = Just "The maximum number of systems to render in 3D."
                        , label = "System limit"
                        , field = systemLimit
                        }
                    , Ui.Form.Field.submit
                        { label =
                            -- if formState.submitting then
                            --     "Saving..."
                            -- else
                            "Save settings"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "systemLimit"
            (Form.Field.int { invalid = \value -> "Must be a positive integter" }
                |> Form.Field.required "Required"
                |> Form.Field.withInitialValue .systemLimit
            )


viewNotifications : Model -> Html Msg
viewNotifications model =
    Html.div [ Html.Attributes.class "notifications" ]
        (List.map Ui.Notification.view model.notifications)
