module Update exposing
    ( Effect(..)
    , Update(..)
    , mapModel
    , mapMsg
    , succeeed
    , toTuple
    , withCmd
    , withEffect
    , withMsg
    , withRequest
    , withResponse
    )

import Dict exposing (Dict)
import Route exposing (Route)
import SpaceTrader.Agent
import SpaceTrader.Api
import SpaceTrader.Point.SystemDict as SystemDict exposing (SystemDict)
import SpaceTrader.System
import Task exposing (Task)
import Ui.Notification exposing (Notification)


type Effect
    = Authenticated
        { accessToken : String
        , agent : Maybe SpaceTrader.Agent.Agent
        , systems : Maybe (SystemDict SpaceTrader.System.System)
        }
    | RouteChangeRequested Route
    | RouteModifyRequested Route
    | PushNotification Notification


type Update model msg
    = Update
        { model : model
        , cmd : Cmd msg
        , effects : List Effect
        }


succeeed : model -> Update model msg
succeeed model =
    Update { model = model, cmd = Cmd.none, effects = [] }



-- ADD TO


withCmd : Cmd msg -> Update model msg -> Update model msg
withCmd cmd (Update update) =
    Update { update | cmd = Cmd.batch [ update.cmd, cmd ] }


withMsg : msg -> Update model msg -> Update model msg
withMsg msg update =
    update


withEffect : Effect -> Update model msg -> Update model msg
withEffect eff (Update update) =
    Update { update | effects = eff :: update.effects }


withRequest : (Result SpaceTrader.Api.Error a -> msg) -> Task SpaceTrader.Api.Error a -> Update model msg -> Update model msg
withRequest toMsg request update =
    update
        |> withCmd (request |> Task.attempt toMsg)


withResponse : Result SpaceTrader.Api.Error a -> (a -> Update model msg) -> model -> Update model msg
withResponse response onOk update =
    case response of
        Ok a ->
            onOk a

        Err error ->
            update
                |> succeeed
                |> withEffect
                    (PushNotification
                        (let
                            errMessage =
                                case error of
                                    SpaceTrader.Api.ApiErrorDecodeError err ->
                                        "API JSON mismatch: " ++ err

                                    SpaceTrader.Api.ApiError err ->
                                        err.message

                                    SpaceTrader.Api.HttpError _ ->
                                        "Server error"
                         in
                         Ui.Notification.new errMessage
                            |> Ui.Notification.withAlert
                        )
                    )



-- MAP


mapModel : (model1 -> model2) -> Update model1 msg -> Update model2 msg
mapModel fn (Update update) =
    Update
        { model = fn update.model
        , cmd = update.cmd
        , effects = update.effects
        }


mapMsg : (msg1 -> msg2) -> Update model msg1 -> Update model msg2
mapMsg fn (Update update) =
    Update
        { model = update.model
        , cmd = Cmd.map fn update.cmd
        , effects = update.effects
        }



-- DECONSTRUCT


toTuple : { fromEffect : Effect -> msg } -> Update model msg -> ( model, Cmd msg )
toTuple opts (Update update) =
    ( update.model
    , Cmd.batch
        (update.cmd
            :: List.map
                (\eff ->
                    eff
                        |> opts.fromEffect
                        |> Task.succeed
                        |> Task.perform identity
                )
                update.effects
        )
    )
