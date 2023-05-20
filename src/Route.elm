module Route exposing (..)

import AppUrl exposing (AppUrl)


type Route
    = Login
    | Game
    | NotFound


fromAppUrl : AppUrl -> Route
fromAppUrl url =
    case url.path of
        [] ->
            Game

        [ "login" ] ->
            Login

        _ ->
            NotFound


toUrlString : Route -> String
toUrlString route =
    case route of
        Login ->
            "/login"

        Game ->
            "/"

        NotFound ->
            "/404"
