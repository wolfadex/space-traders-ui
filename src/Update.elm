module Update exposing (..)

import Dict exposing (Dict)
import Route exposing (Route)
import SpaceTrader.Agent
import SpaceTrader.System
import Task


type Effect
    = Authenticated
        { accessToken : String
        , agent : Maybe SpaceTrader.Agent.Agent
        , systems : Maybe (Dict String SpaceTrader.System.System)
        }
    | RouteChangeRequested Route


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


type Msg
    = Auth
        { accessToken : String
        , agent : SpaceTrader.Agent.Agent
        }
