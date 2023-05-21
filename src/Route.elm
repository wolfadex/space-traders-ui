module Route exposing (..)

import AppUrl exposing (AppUrl)


type Route
    = Login
    | Game { tab : Maybe GameTab }
    | NotFound


type GameTab
    = Ships
    | Contracts
    | Waypoints { systemId : Maybe String }


fromAppUrl : AppUrl -> Route
fromAppUrl url =
    case url.path of
        [] ->
            Login

        "game" :: rest ->
            Game
                { tab =
                    case rest of
                        [ "ships" ] ->
                            Just Ships

                        [ "contracts" ] ->
                            Just Contracts

                        "waypoints" :: rest_ ->
                            Waypoints
                                { systemId =
                                    case rest_ of
                                        [ systemId ] ->
                                            Just systemId

                                        _ ->
                                            Nothing
                                }
                                |> Just

                        _ ->
                            Nothing
                }

        _ ->
            NotFound


toUrlString : Route -> String
toUrlString route =
    case route of
        Login ->
            "/"

        Game { tab } ->
            "/"
                ++ ([ Just [ "game" ]
                    , case tab of
                        Nothing ->
                            Nothing

                        Just Ships ->
                            Just [ "ships" ]

                        Just Contracts ->
                            Just [ "contracts" ]

                        Just (Waypoints { systemId }) ->
                            [ Just "waypoints"
                            , systemId
                            ]
                                |> List.filterMap identity
                                |> Just
                    ]
                        |> List.filterMap identity
                        |> List.concat
                        |> String.join "/"
                   )

        NotFound ->
            "/"
