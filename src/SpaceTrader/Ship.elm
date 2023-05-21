module SpaceTrader.Ship exposing (Ship, decode)

import Json.Decode
import Json.Decode.Extra
import SpaceTrader.Ship.Cargo exposing (Cargo)
import SpaceTrader.Ship.Cooldown
import SpaceTrader.Ship.Crew exposing (Crew)
import SpaceTrader.Ship.Engine exposing (Engine)
import SpaceTrader.Ship.Frame exposing (Frame)
import SpaceTrader.Ship.Fuel exposing (Fuel)
import SpaceTrader.Ship.Module exposing (Module)
import SpaceTrader.Ship.Mount exposing (Mount)
import SpaceTrader.Ship.Nav exposing (Nav)
import SpaceTrader.Ship.Reactor exposing (Reactor)
import SpaceTrader.Ship.Registration exposing (Registration)


type alias Ship =
    { id : String
    , registration : Registration
    , nav : Nav
    , crew : Crew
    , frame : Frame
    , reactor : Reactor
    , engine : Engine
    , modules : List Module
    , mounts : List Mount
    , cargo : Cargo
    , fuel : Fuel
    , cooldown : Maybe SpaceTrader.Ship.Cooldown.Cooldown
    }


decode : Json.Decode.Decoder Ship
decode =
    Json.Decode.succeed Ship
        |> Json.Decode.Extra.andMap (Json.Decode.field "symbol" Json.Decode.string)
        |> Json.Decode.Extra.andMap (Json.Decode.field "registration" SpaceTrader.Ship.Registration.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "crew" SpaceTrader.Ship.Crew.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "frame" SpaceTrader.Ship.Frame.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "reactor" SpaceTrader.Ship.Reactor.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "engine" SpaceTrader.Ship.Engine.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "modules" (Json.Decode.list SpaceTrader.Ship.Module.decode))
        |> Json.Decode.Extra.andMap (Json.Decode.field "mounts" (Json.Decode.list SpaceTrader.Ship.Mount.decode))
        |> Json.Decode.Extra.andMap (Json.Decode.field "cargo" SpaceTrader.Ship.Cargo.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.field "fuel" SpaceTrader.Ship.Fuel.decode)
        |> Json.Decode.Extra.andMap (Json.Decode.succeed Nothing)
