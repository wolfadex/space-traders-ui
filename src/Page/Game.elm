module Page.Game exposing (..)

import Cacheable exposing (Cacheable(..))
import Color
import Dict exposing (Dict)
import Form
import Form.Field
import Form.FieldView
import Form.Handler
import Form.Validation
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Length exposing (Meters)
import List.NonEmpty
import Point3d exposing (Point3d)
import Port
import RemoteData exposing (RemoteData(..))
import Route
import Scene3d
import Scene3d.Material
import Shared
import SpaceTrader.Agent
import SpaceTrader.Api
import SpaceTrader.Contract
import SpaceTrader.Faction
import SpaceTrader.Ship
import SpaceTrader.Ship.Nav
import SpaceTrader.System
import SpaceTrader.Waypoint
import Sphere3d
import Task
import Time
import Ui
import Ui.Button
import Ui.Contract
import Ui.Form
import Ui.Form.Field
import Ui.Galaxy3d
import Ui.Modal
import Ui.Select
import Ui.Ship
import Ui.System
import Ui.Theme
import Update exposing (Update)
import Util.Function


type alias Model =
    { accessToken : String
    , agent : RemoteData SpaceTrader.Agent.Agent
    , waypoints : Dict String SpaceTrader.Waypoint.Waypoint
    , myContracts : Dict String SpaceTrader.Contract.Contract
    , myShips : Dict String SpaceTrader.Ship.Ship
    , selectedSystem : Maybe (RemoteData SpaceTrader.System.System)

    -- cached data
    , systems : Cacheable SpaceTrader.System.System

    -- game stuffs
    , spaceFocus : Shared.SpaceFocus
    , zoom : Float
    , viewRotation : Float
    , systems3d : Dict String ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint )
    }


init :
    { accessToken : String
    , agent : Maybe SpaceTrader.Agent.Agent
    , systems : Maybe (Dict String SpaceTrader.System.System)
    }
    -> ( Model, Cmd Msg )
init opts =
    let
        sys =
            initSystems opts.systems
    in
    ( { accessToken = opts.accessToken
      , agent =
            case opts.agent of
                Nothing ->
                    Loading

                Just agent ->
                    Loaded agent
      , waypoints = Dict.empty
      , myContracts = Dict.empty
      , myShips = Dict.empty
      , selectedSystem = Nothing
      , systems =
            case opts.systems of
                Nothing ->
                    Uncached

                Just systems ->
                    Cached systems

      -- 3d stuffs
      , spaceFocus = Shared.FGalaxy
      , viewRotation = 0
      , zoom = sys.zoom
      , systems3d = sys.systems3d
      }
    , Cmd.batch
        [ SpaceTrader.Api.myContracts MyContractsResponded
            { token = opts.accessToken }
        , SpaceTrader.Api.myShips MyShipsResponded
            { token = opts.accessToken }
        , case opts.systems of
            Nothing ->
                SpaceTrader.Api.getAllSystemsInit SystemsLongRequestMsg { token = opts.accessToken }

            Just _ ->
                Cmd.none
        , Port.setToken opts.accessToken
        , case opts.agent of
            Nothing ->
                SpaceTrader.Api.myAgent AgentResponded
                    { token = opts.accessToken }

            Just _ ->
                Cmd.none
        ]
    )


initSystems :
    Maybe (Dict String SpaceTrader.System.System)
    ->
        { zoom : Float
        , systems3d : Dict String ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint )
        }
initSystems maybeSystems =
    case maybeSystems of
        Nothing ->
            { -- a nice default
              zoom = 6621539845261203 * 10000
            , systems3d = Dict.empty
            }

        Just systems ->
            { zoom =
                systems
                    |> Dict.values
                    |> List.map
                        (\system ->
                            Length.inMeters
                                (Point3d.distanceFrom Point3d.origin
                                    (Point3d.xyz
                                        (system.x |> toFloat |> Length.lightYears)
                                        (system.y |> toFloat |> Length.lightYears)
                                        (Length.lightYears 0)
                                    )
                                )
                        )
                    |> List.sort
                    |> List.reverse
                    |> List.head
                    |> Maybe.map (\a -> a / 2)
                    |> Maybe.withDefault (25000 + (100 * 9460730000000000))
            , systems3d =
                systems
                    |> Dict.map
                        (\_ system ->
                            let
                                point =
                                    Point3d.xyz
                                        (system.x |> toFloat |> Length.lightYears)
                                        (system.y |> toFloat |> Length.lightYears)
                                        (Length.lightYears 0)
                            in
                            ( point
                            , renderSystem point
                            )
                        )
            }


type Msg
    = LogoutClicked
    | SystemClicked String
    | Zoomed Json.Encode.Value
    | ZoomPressed Float
    | RotationPressed Float
    | AgentResponded (Result Http.Error SpaceTrader.Agent.Agent)
      -- game
    | MyContractsResponded (Result Http.Error (List SpaceTrader.Contract.Contract))
    | MyShipsResponded (Result Http.Error (List SpaceTrader.Ship.Ship))
    | WaypointResponded String (Result Http.Error SpaceTrader.Waypoint.Waypoint)
    | ShipDockRequested String
    | ShipDockResponded String (Result Http.Error SpaceTrader.Ship.Nav.Nav)
    | ShipOrbitRequested String
    | ShipOrbitResponded String (Result Http.Error SpaceTrader.Ship.Nav.Nav)
    | ShipMoveRequested String
    | SystemsLoadRequested
    | SystemsLongRequestMsg (Result Http.Error (SpaceTrader.Api.Msg SpaceTrader.System.System))
    | SystemResponded (Result Http.Error SpaceTrader.System.System)


update :
    { shared : Shared.Model
    , msg : Msg
    , model : Model
    , toMsg : Msg -> msg
    , toModel : Model -> model
    }
    -> Update model msg
update ({ model } as opts) =
    Update.mapModel opts.toModel <|
        Update.mapMsg opts.toMsg <|
            case opts.msg of
                LogoutClicked ->
                    model
                        |> Update.succeeed
                        |> Update.withCmd Port.clearToken
                        |> Update.withEffect (Update.RouteChangeRequested Route.Login)

                AgentResponded result ->
                    { model
                        | agent =
                            result
                                |> Result.mapError (\_ -> "Failed to load agent")
                                |> RemoteData.fromResult
                    }
                        |> Update.succeeed

                WaypointResponded _ (Err err) ->
                    Debug.todo (Debug.toString err)

                WaypointResponded id (Ok waypoint) ->
                    { model
                        | waypoints = Dict.insert id waypoint model.waypoints
                    }
                        |> Update.succeeed

                MyContractsResponded (Err err) ->
                    Debug.todo (Debug.toString err)

                MyContractsResponded (Ok contracts) ->
                    { model
                        | myContracts =
                            List.foldl
                                (\contract dict ->
                                    Dict.insert contract.id contract dict
                                )
                                Dict.empty
                                contracts
                    }
                        |> Update.succeeed

                ShipDockRequested id ->
                    model
                        |> Update.succeeed
                        |> Update.withCmd
                            (SpaceTrader.Api.dockShip (ShipDockResponded id)
                                { token = model.accessToken
                                , shipId = id
                                }
                            )

                ShipDockResponded _ (Err err) ->
                    Debug.todo (Debug.toString err)

                ShipDockResponded id (Ok nav) ->
                    { model
                        | myShips =
                            Dict.update id
                                (Maybe.map
                                    (\ship ->
                                        { ship
                                            | nav = nav
                                        }
                                    )
                                )
                                model.myShips
                    }
                        |> Update.succeeed

                ShipOrbitRequested id ->
                    model
                        |> Update.succeeed
                        |> Update.withCmd
                            (SpaceTrader.Api.moveToOrbit (ShipOrbitResponded id)
                                { token = model.accessToken
                                , shipId = id
                                }
                            )

                ShipOrbitResponded _ (Err err) ->
                    Debug.todo (Debug.toString err)

                ShipOrbitResponded id (Ok nav) ->
                    { model
                        | myShips =
                            Dict.update id
                                (Maybe.map
                                    (\ship ->
                                        { ship
                                            | nav = nav
                                        }
                                    )
                                )
                                model.myShips
                    }
                        |> Update.succeeed

                ShipMoveRequested id ->
                    Debug.todo ""

                MyShipsResponded (Err err) ->
                    Debug.todo (Debug.toString err)

                MyShipsResponded (Ok ships) ->
                    { model
                        | myShips =
                            List.foldl
                                (\ship dict ->
                                    Dict.insert ship.id ship dict
                                )
                                Dict.empty
                                ships
                    }
                        |> Update.succeeed

                SystemClicked systemId ->
                    let
                        systemFound : Maybe SpaceTrader.System.System
                        systemFound =
                            model.systems
                                |> Cacheable.getData
                                |> Dict.get systemId
                    in
                    { model
                        | selectedSystem =
                            Just <|
                                case systemFound of
                                    Just system ->
                                        Loaded system

                                    Nothing ->
                                        Loading
                    }
                        |> Update.succeeed
                        |> Update.withCmd
                            (case systemFound of
                                Just _ ->
                                    Cmd.none

                                Nothing ->
                                    SpaceTrader.Api.getSystem SystemResponded
                                        { token = model.accessToken
                                        , systemId = systemId
                                        }
                            )

                SystemsLoadRequested ->
                    { model
                        | systems =
                            Caching
                                { data = Cacheable.getData model.systems
                                , current = 0
                                , max = 1
                                }
                    }
                        |> Update.succeeed

                SystemsLongRequestMsg (Err err) ->
                    Debug.todo (Debug.toString err)

                SystemsLongRequestMsg (Ok msg_) ->
                    case msg_ of
                        SpaceTrader.Api.Complete systems ->
                            let
                                updatedSystems =
                                    List.foldl
                                        (\system dict ->
                                            Dict.insert system.id system dict
                                        )
                                        (Cacheable.getData model.systems)
                                        systems
                            in
                            { model
                                | systems = Cached updatedSystems
                                , systems3d =
                                    List.foldl
                                        (\system dict ->
                                            Dict.insert system.id
                                                (let
                                                    point =
                                                        Point3d.xyz
                                                            (system.x |> toFloat |> Length.lightYears)
                                                            (system.y |> toFloat |> Length.lightYears)
                                                            (Length.lightYears 0)
                                                 in
                                                 ( point
                                                 , renderSystem point
                                                 )
                                                )
                                                dict
                                        )
                                        model.systems3d
                                        systems
                            }
                                |> Update.succeeed
                                |> Update.withCmd
                                    (updatedSystems
                                        |> Dict.values
                                        |> Json.Encode.list SpaceTrader.System.encode
                                        |> Port.cacheSystems
                                    )

                        SpaceTrader.Api.NeedsMore data ->
                            let
                                updatedSystems =
                                    List.foldl
                                        (\system dict ->
                                            Dict.insert system.id system dict
                                        )
                                        (Cacheable.getData model.systems)
                                        data.data
                            in
                            { model
                                | systems = Caching { data = updatedSystems, current = data.current, max = data.max }
                                , systems3d =
                                    List.foldl
                                        (\system dict ->
                                            Dict.insert system.id
                                                (let
                                                    point =
                                                        Point3d.xyz
                                                            (system.x |> toFloat |> Length.lightYears)
                                                            (system.y |> toFloat |> Length.lightYears)
                                                            (Length.lightYears 0)
                                                 in
                                                 ( point
                                                 , renderSystem point
                                                 )
                                                )
                                                dict
                                        )
                                        model.systems3d
                                        data.data
                            }
                                |> Update.succeeed
                                |> Update.withCmd
                                    (Cmd.batch
                                        [ SpaceTrader.Api.getAllSystemsUpdate SystemsLongRequestMsg data
                                        , updatedSystems
                                            |> Dict.values
                                            |> Json.Encode.list SpaceTrader.System.encode
                                            |> Port.cacheSystems
                                        ]
                                    )

                SystemResponded (Err err) ->
                    Debug.todo (Debug.toString err)

                SystemResponded (Ok system) ->
                    { model
                        | systems = Cached (Dict.insert system.id system (Cacheable.getData model.systems))
                        , systems3d =
                            Dict.insert system.id
                                (let
                                    point =
                                        Point3d.xyz
                                            (system.x |> toFloat |> Length.lightYears)
                                            (system.y |> toFloat |> Length.lightYears)
                                            (Length.lightYears 0)
                                 in
                                 ( point
                                 , renderSystem point
                                 )
                                )
                                model.systems3d
                    }
                        |> Update.succeeed

                -- 3d stuff
                Zoomed value ->
                    case Json.Decode.decodeValue decodeZoomEvent value of
                        Ok delta ->
                            setZoom model (delta * zoomMultiplier model.spaceFocus)

                        Err _ ->
                            model |> Update.succeeed

                ZoomPressed change ->
                    setZoom model (change * zoomMultiplier model.spaceFocus)

                RotationPressed change ->
                    { model
                        | viewRotation =
                            toFloat (remainderBy 360 (floor (model.viewRotation + change)))
                    }
                        |> Update.succeeed


setZoom : Model -> Float -> Update Model Msg
setZoom model delta =
    { model | zoom = max 5000000 (model.zoom + delta) }
        |> Update.succeeed


decodeZoomEvent : Json.Decode.Decoder Float
decodeZoomEvent =
    Json.Decode.field "deltaY" Json.Decode.float


zoomMultiplier : Shared.SpaceFocus -> Float
zoomMultiplier focus =
    case focus of
        Shared.FGalaxy ->
            -- One light year is 9460730000000000, this is about 10 light years
            94607300000000000

        Shared.FSystem _ ->
            10000000000

        Shared.FWaypoint _ ->
            1


renderSystem : Point3d Meters Shared.LightYear -> Scene3d.Entity Shared.ScaledViewPoint
renderSystem position =
    Scene3d.sphere
        (Scene3d.Material.color Color.gray)
        (Sphere3d.atPoint (scalePointInLightYearsToOne position) (Length.lightYears 60))


scalePointInLightYearsToOne : Point3d Meters Shared.LightYear -> Point3d Meters Shared.ScaledViewPoint
scalePointInLightYearsToOne point =
    Point3d.fromMeters
        { x = Length.inMeters (Point3d.xCoordinate point)
        , y = Length.inMeters (Point3d.yCoordinate point)
        , z = Length.inMeters (Point3d.zCoordinate point)
        }


view : Shared.Model -> Model -> Html Msg
view shared model =
    Ui.row []
        [ Ui.column
            [ Html.Attributes.style "justify-self" "start"
            , Html.Attributes.style "padding" "1rem"
            ]
            [ Ui.viewLabelGroup
                (Html.div []
                    [ Html.text "Agent"
                    , Ui.Button.default
                        [ Html.Attributes.style "float" "right" ]
                        { label = Html.text "Logout"
                        , onClick = Just LogoutClicked
                        }
                    ]
                )
                [ { label = "Callsign"
                  , value =
                        model.agent
                            |> RemoteData.mapToString .callsign
                            |> Html.text
                  }
                , { label = "Headquarters"
                  , value =
                        Ui.Button.link []
                            { label =
                                model.agent
                                    |> RemoteData.mapToString .headquarters
                                    |> Html.text
                            , onClick =
                                case model.agent of
                                    Loaded { headquarters } ->
                                        headquarters
                                            |> String.split "-"
                                            |> List.take 2
                                            |> String.join "-"
                                            |> SystemClicked
                                            |> Just

                                    _ ->
                                        Nothing
                            }
                  }
                , { label = "Credits"
                  , value =
                        model.agent
                            |> RemoteData.mapToString (.credits >> String.fromInt)
                            |> Html.text
                  }
                ]
            , model.myContracts
                |> Dict.values
                |> List.map
                    (Ui.Contract.view
                        { timeZone = shared.timeZone
                        , currentTime = shared.currentTime
                        , onDestinationClicked = SystemClicked
                        }
                    )
                |> (::) (Html.h3 [] [ Html.text "My Contracts" ])
                |> Html.div []
            , model.myShips
                |> Dict.values
                |> List.map
                    (Ui.Ship.view
                        { onDock = ShipDockRequested
                        , onOrbit = ShipOrbitRequested
                        , onMove = ShipMoveRequested
                        , onSystemClicked = SystemClicked
                        }
                    )
                |> (::) (Ui.header.three [] [ Html.text "My Ships" ])
                |> Html.div []
            ]
        , Ui.column []
            [ Ui.Galaxy3d.viewSystems
                { onSystemClick = SystemClicked
                , onZoom = Zoomed
                , onZoomPress = ZoomPressed
                , onRotationPress = RotationPressed
                , selected =
                    case model.selectedSystem of
                        Just (Loaded system) ->
                            Just system.id

                        _ ->
                            Nothing
                }
                { galaxyViewSize = { width = 750, height = 500 }
                , zoom = model.zoom
                , viewRotation = model.viewRotation
                , systems = Dict.toList model.systems3d
                }
            , case model.systems of
                Uncached ->
                    Ui.Button.default
                        []
                        { label = Html.text "Load Systems"
                        , onClick = Just SystemsLoadRequested
                        }

                Caching { current, max } ->
                    Ui.row []
                        [ Html.text "Loading Systems..."
                        , Ui.progress []
                            { max = toFloat max
                            , current = toFloat current
                            }
                        ]

                Cached _ ->
                    Ui.row []
                        [ Html.text "Systems Loaded & Cached"
                        , Ui.Button.default
                            []
                            { label = Html.text "Reload Systems"
                            , onClick = Just SystemsLoadRequested
                            }
                        ]
            , case model.selectedSystem of
                Nothing ->
                    Html.text ""

                Just Loading ->
                    Html.text "Loading System..."

                Just (Failure error) ->
                    Html.text ("Failed to load system: " ++ error)

                Just (Loaded system) ->
                    Ui.System.view
                        { myShips = Dict.values model.myShips }
                        system
            ]
        ]
