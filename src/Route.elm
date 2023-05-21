module Route exposing
    ( GameTab(..)
    , Route(..)
    , WaypointDetail(..)
    , fromAppUrl
    , fromSystem
    , fromWaypoint
    , toUrlString
    )

import AppUrl exposing (AppUrl)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import Util.Maybe


type Route
    = Login
    | Game { tab : Maybe GameTab }
    | NotFound


type GameTab
    = Ships
    | Contracts
    | Waypoints { id : Maybe WaypointDetail }


type WaypointDetail
    = ViewSystem SpaceTrader.Point.System.System
    | ViewWaypoint SpaceTrader.Point.Waypoint.Waypoint


fromSystem : SpaceTrader.Point.System.System -> Route
fromSystem system =
    Game { tab = Just (Waypoints { id = Just (ViewSystem system) }) }


fromWaypoint : SpaceTrader.Point.Waypoint.Waypoint -> Route
fromWaypoint waypoint =
    Game { tab = Just (Waypoints { id = Just (ViewWaypoint waypoint) }) }


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
                                { id =
                                    case rest_ of
                                        [ id ] ->
                                            id
                                                |> SpaceTrader.Point.Waypoint.parse
                                                |> Maybe.map ViewWaypoint
                                                |> Util.Maybe.onNothing
                                                    (SpaceTrader.Point.System.parse id
                                                        |> Maybe.map ViewSystem
                                                    )

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

                        Just (Waypoints { id }) ->
                            [ Just "waypoints"
                            , case id of
                                Nothing ->
                                    Nothing

                                Just (ViewSystem systemId) ->
                                    Just (SpaceTrader.Point.System.toKey systemId)

                                Just (ViewWaypoint waypointId) ->
                                    Just (SpaceTrader.Point.Waypoint.toKey waypointId)
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
