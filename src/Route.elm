module Route exposing
    ( GameTab(..)
    , Route(..)
    , WaypointDetail(..)
    , fromAppUrl
    , fromNoShip
    , fromShip
    , fromSystem
    , fromWaypoint
    , toUrlString
    )

import AppUrl exposing (AppUrl)
import Id
import SpaceTrader.Id exposing (ShipId)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import Util.Maybe


type Route
    = Login
    | Game { tab : Maybe GameTab }
    | NotFound


type GameTab
    = Ships { id : Maybe ShipId }
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


fromShip : ShipId -> Route
fromShip shipId =
    Game { tab = Just (Ships { id = Just shipId }) }


fromNoShip : Route
fromNoShip =
    Game { tab = Just (Ships { id = Nothing }) }


fromAppUrl : AppUrl -> Route
fromAppUrl url =
    case url.path of
        [] ->
            Login

        "game" :: rest ->
            Game
                { tab =
                    case rest of
                        "ships" :: rest_ ->
                            Ships
                                { id =
                                    case rest_ of
                                        [ id ] ->
                                            id
                                                |> Id.fromString
                                                |> Just

                                        _ ->
                                            Nothing
                                }
                                |> Just

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

                        Just (Ships { id }) ->
                            [ Just "ships"
                            , case id of
                                Nothing ->
                                    Nothing

                                Just shipId ->
                                    Just (Id.toString shipId)
                            ]
                                |> List.filterMap identity
                                |> Just

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
