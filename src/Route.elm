module Route exposing (..)

import AppUrl exposing (AppUrl)


type Route
    = Login
    | Game (Maybe GameTab)
    | NotFound


type GameTab
    = Ships
    | Contracts
    | Waypoints


fromAppUrl : AppUrl -> Route
fromAppUrl url =
    case url.path of
        [] ->
            Login

        [ "game", "ships" ] ->
            Game (Just Ships)

        [ "game", "contracts" ] ->
            Game (Just Contracts)

        [ "game", "waypoints" ] ->
            Game (Just Waypoints)

        "game" :: _ ->
            Game Nothing

        _ ->
            NotFound


toUrlString : Route -> String
toUrlString route =
    case route of
        Login ->
            ""

        Game tab ->
            String.join "/"
                [ "game"
                , case tab of
                    Nothing ->
                        "ships"

                    Just Ships ->
                        "ships"

                    Just Contracts ->
                        "contracts"

                    Just Waypoints ->
                        "waypoints"
                ]

        NotFound ->
            "404"
