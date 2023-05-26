module Page.Game exposing (Model, Msg(..), init, update, view, withTab)

import Cacheable exposing (Cacheable(..))
import Color
import Dict exposing (Dict)
import FormatNumber
import FormatNumber.Locales
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode
import Json.Encode
import Length exposing (Meters)
import Point2d exposing (Point2d)
import Point3d exposing (Point3d)
import Port
import Quantity
import Random
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Scene3d
import Scene3d.Material
import Shared
import SpaceTrader.Agent
import SpaceTrader.Api
import SpaceTrader.Contract
import SpaceTrader.Point.System
import SpaceTrader.Point.SystemDict as SystemDict exposing (SystemDict)
import SpaceTrader.Point.Waypoint
import SpaceTrader.Point.WaypointDict as WaypointDict exposing (WaypointDict)
import SpaceTrader.Ship
import SpaceTrader.Ship.Cooldown
import SpaceTrader.Ship.Nav
import SpaceTrader.Survey
import SpaceTrader.System
import SpaceTrader.Waypoint
import SpaceTrader.Waypoint.Trait
import SpaceTrader.Waypoint.Type
import Sphere3d
import Time
import Ui
import Ui.Button
import Ui.Contract
import Ui.Galaxy3d
import Ui.Ship
import Ui.System
import Update exposing (Update)


type alias Model =
    { accessToken : String
    , tab : Route.GameTab
    , agent : RemoteData SpaceTrader.Agent.Agent
    , waypoints : WaypointDict (RemoteData SpaceTrader.Waypoint.Waypoint)
    , myContracts : Dict String SpaceTrader.Contract.Contract
    , myShips : Dict String SpaceTrader.Ship.Ship
    , surveys : WaypointDict (List SpaceTrader.Survey.Survey)

    -- cached data
    , systems : CacheableSystems

    -- game stuffs
    , spaceFocus : Shared.SpaceFocus
    , zoom : Float
    , viewRotation : Float
    , eyeHeight : Float
    , seed : Random.Seed
    , systems3d : SystemDict ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint )
    }


type alias CacheableSystems =
    Cacheable (SystemDict (RemoteData SpaceTrader.System.System))


init :
    { accessToken : String
    , tab : Maybe Route.GameTab
    , agent : Maybe SpaceTrader.Agent.Agent
    , systems : Maybe (SystemDict SpaceTrader.System.System)
    , toMsg : Msg -> msg
    , toModel : Model -> model
    }
    -> Update model msg
init opts =
    let
        sys :
            { zoom : Float
            , systems3d : SystemDict ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint )
            , seed : Random.Seed
            }
        sys =
            initSystems (Random.initialSeed 0) opts.systems
    in
    { accessToken = opts.accessToken
    , tab = Maybe.withDefault Route.Ships opts.tab
    , agent =
        case opts.agent of
            Nothing ->
                Loading

            Just agent ->
                Loaded agent
    , waypoints = WaypointDict.empty
    , myContracts = Dict.empty
    , myShips = Dict.empty
    , surveys = WaypointDict.empty
    , systems =
        case opts.systems of
            Nothing ->
                Uncached

            Just systems ->
                Cached (SystemDict.map (\_ v -> RemoteData.Loaded v) systems)

    -- 3d stuffs
    , spaceFocus = Shared.FGalaxy
    , viewRotation = 0
    , zoom = sys.zoom
    , eyeHeight = 3
    , systems3d = sys.systems3d
    , seed = sys.seed
    }
        |> Update.succeed
        |> Update.withRequest MyContractsResponded
            (SpaceTrader.Api.myContracts { token = opts.accessToken })
        |> Update.withCmd
            (Cmd.batch
                [ SpaceTrader.Api.myShips MyShipsResponded
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
        |> (\updatedModel ->
                case opts.tab of
                    Nothing ->
                        Update.withEffect (Update.RouteModifyRequested (Route.Game { tab = Just Route.Ships })) updatedModel

                    Just _ ->
                        updatedModel
           )
        |> Update.mapMsg opts.toMsg
        |> Update.mapModel opts.toModel


initSystems :
    Random.Seed
    -> Maybe (SystemDict SpaceTrader.System.System)
    ->
        { zoom : Float
        , systems3d : SystemDict ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint )
        , seed : Random.Seed
        }
initSystems seed maybeSystems =
    case maybeSystems of
        Nothing ->
            { -- a nice default
              zoom = 6621539845261203 * 10000
            , systems3d = SystemDict.empty
            , seed = seed
            }

        Just systems ->
            let
                ( systems3d, finalSeed ) =
                    systems
                        |> SystemDict.toList
                        |> List.foldl
                            (\( systemId, system ) ( dict, sd ) ->
                                let
                                    ( val, s ) =
                                        buildSystem3d sd system
                                in
                                ( SystemDict.insert systemId val dict, s )
                            )
                            ( SystemDict.empty, seed )
            in
            { zoom =
                systems
                    |> SystemDict.values
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
            , systems3d = systems3d
            , seed = finalSeed
            }


type Msg
    = LogoutClicked
    | Zoomed Json.Encode.Value
    | ZoomPressed Float
    | RotationPressed Float
    | PitchPressed Float
    | SystemClicked SpaceTrader.Point.System.System
      -- game
    | CreateSurveyRequested { waypointId : SpaceTrader.Point.Waypoint.Waypoint, shipId : String }
    | SurveyResponded SpaceTrader.Point.Waypoint.Waypoint (Result SpaceTrader.Api.Error ( List SpaceTrader.Survey.Survey, SpaceTrader.Ship.Cooldown.Cooldown ))
    | AgentResponded (Result Http.Error SpaceTrader.Agent.Agent)
    | MyContractsResponded (Result SpaceTrader.Api.Error (List SpaceTrader.Contract.Contract))
    | MyShipsResponded (Result Http.Error (List SpaceTrader.Ship.Ship))
    | WaypointResponded SpaceTrader.Point.Waypoint.Waypoint (Result SpaceTrader.Api.Error SpaceTrader.Waypoint.Waypoint)
    | ShipDockRequested String
    | ShipDockResponded String (Result SpaceTrader.Api.Error SpaceTrader.Ship.Nav.Nav)
    | ShipOrbitRequested String
    | ShipOrbitResponded String (Result SpaceTrader.Api.Error SpaceTrader.Ship.Nav.Nav)
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
                        |> Update.succeed
                        |> Update.withCmd Port.clearToken
                        |> Update.withEffect (Update.RouteChangeRequested Route.Login)

                AgentResponded result ->
                    { model
                        | agent =
                            result
                                |> Result.mapError (\_ -> "Failed to load agent")
                                |> RemoteData.fromResult
                    }
                        |> Update.succeed

                CreateSurveyRequested { waypointId, shipId } ->
                    model
                        |> Update.succeed
                        |> Update.withRequest (SurveyResponded waypointId)
                            (SpaceTrader.Api.createSurvey
                                { token = model.accessToken
                                , shipId = shipId
                                }
                            )

                SurveyResponded waypointId response ->
                    model
                        |> Update.withResponse response
                            (\( surveys, cooldown ) ->
                                { model
                                    | surveys =
                                        WaypointDict.update waypointId
                                            (\waypointSurveys ->
                                                case waypointSurveys of
                                                    Nothing ->
                                                        Just surveys

                                                    Just existingSurveys ->
                                                        Just
                                                            (List.filter
                                                                (\existingSurvey ->
                                                                    Time.posixToMillis existingSurvey.expiration > Time.posixToMillis opts.shared.currentTime
                                                                )
                                                                existingSurveys
                                                                ++ surveys
                                                            )
                                            )
                                            model.surveys
                                    , myShips =
                                        Dict.update cooldown.shipSymbol
                                            (Maybe.map
                                                (\ship ->
                                                    { ship | cooldown = Just cooldown }
                                                )
                                            )
                                            model.myShips
                                }
                                    |> Update.succeed
                            )

                WaypointResponded id response ->
                    model
                        |> Update.withResponse response
                            (\waypoint ->
                                { model
                                    | waypoints = WaypointDict.insert id (Loaded waypoint) model.waypoints
                                }
                                    |> Update.succeed
                            )

                MyContractsResponded response ->
                    model
                        |> Update.withResponse response
                            (\contracts ->
                                { model
                                    | myContracts =
                                        List.foldl
                                            (\contract dict ->
                                                Dict.insert contract.id contract dict
                                            )
                                            Dict.empty
                                            contracts
                                }
                                    |> Update.succeed
                            )

                ShipDockRequested id ->
                    model
                        |> Update.succeed
                        |> Update.withRequest (ShipDockResponded id)
                            (SpaceTrader.Api.dockShip
                                { token = model.accessToken
                                , shipId = id
                                }
                            )

                ShipDockResponded id response ->
                    model
                        |> Update.withResponse response
                            (\nav ->
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
                                    |> Update.succeed
                            )

                ShipOrbitRequested id ->
                    model
                        |> Update.succeed
                        |> Update.withRequest (ShipOrbitResponded id)
                            (SpaceTrader.Api.moveToOrbit
                                { token = model.accessToken
                                , shipId = id
                                }
                            )

                ShipOrbitResponded id response ->
                    model
                        |> Update.withResponse response
                            (\nav ->
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
                                    |> Update.succeed
                            )

                ShipMoveRequested id ->
                    -- Debug.todo ""
                    model
                        |> Update.succeed

                MyShipsResponded (Err err) ->
                    -- Debug.todo (Debug.toString err)
                    model
                        |> Update.succeed

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
                        |> Update.succeed

                SystemClicked systemId ->
                    model
                        |> Update.succeed
                        |> Update.withEffect (Update.RouteChangeRequested (Route.fromSystem systemId))

                SystemsLoadRequested ->
                    { model
                        | systems =
                            Caching
                                { data =
                                    Cacheable.getData model.systems
                                        |> Maybe.withDefault SystemDict.empty
                                , current = 0
                                , max = 1
                                }
                    }
                        |> Update.succeed

                SystemsLongRequestMsg (Err err) ->
                    -- Debug.todo (Debug.toString err)
                    model
                        |> Update.succeed

                SystemsLongRequestMsg (Ok msg_) ->
                    case msg_ of
                        SpaceTrader.Api.Complete systems ->
                            let
                                updatedSystems : SystemDict (RemoteData SpaceTrader.System.System)
                                updatedSystems =
                                    List.foldl
                                        (\system dict ->
                                            SystemDict.insert system.id (RemoteData.Loaded system) dict
                                        )
                                        (Cacheable.getData model.systems
                                            |> Maybe.withDefault SystemDict.empty
                                        )
                                        systems

                                ( systems3d, finalSeed ) =
                                    List.foldl
                                        (\system ( dict, seed ) ->
                                            let
                                                ( sys, s ) =
                                                    buildSystem3d seed system
                                            in
                                            ( SystemDict.insert system.id
                                                sys
                                                dict
                                            , s
                                            )
                                        )
                                        ( model.systems3d, model.seed )
                                        systems
                            in
                            { model
                                | systems = Cached updatedSystems
                                , systems3d = systems3d
                                , seed = finalSeed
                            }
                                |> Update.succeed
                                |> Update.withCmd
                                    (updatedSystems
                                        |> SystemDict.values
                                        |> List.filterMap RemoteData.toMaybe
                                        |> Json.Encode.list SpaceTrader.System.encode
                                        |> Port.cacheSystems
                                    )

                        SpaceTrader.Api.NeedsMore data ->
                            let
                                updatedSystems : SystemDict (RemoteData SpaceTrader.System.System)
                                updatedSystems =
                                    List.foldl
                                        (\system dict ->
                                            SystemDict.insert system.id (RemoteData.Loaded system) dict
                                        )
                                        (Cacheable.getData model.systems
                                            |> Maybe.withDefault SystemDict.empty
                                        )
                                        data.data

                                ( systems3d, finalSeed ) =
                                    List.foldl
                                        (\system ( dict, seed ) ->
                                            let
                                                ( sys, s ) =
                                                    buildSystem3d seed system
                                            in
                                            ( SystemDict.insert system.id
                                                sys
                                                dict
                                            , s
                                            )
                                        )
                                        ( model.systems3d, model.seed )
                                        data.data
                            in
                            { model
                                | systems = Caching { data = updatedSystems, current = data.current, max = data.max }
                                , systems3d = systems3d
                                , seed = finalSeed
                            }
                                |> Update.succeed
                                |> Update.withCmd
                                    (Cmd.batch
                                        [ SpaceTrader.Api.getAllSystemsUpdate SystemsLongRequestMsg data
                                        , updatedSystems
                                            |> SystemDict.values
                                            |> List.filterMap RemoteData.toMaybe
                                            |> Json.Encode.list SpaceTrader.System.encode
                                            |> Port.cacheSystems
                                        ]
                                    )

                SystemResponded (Err err) ->
                    -- Debug.todo (Debug.toString err)
                    model
                        |> Update.succeed

                SystemResponded (Ok system) ->
                    let
                        ( sys, seed ) =
                            buildSystem3d model.seed system
                    in
                    { model
                        | systems = Cacheable.insert SystemDict.insert system.id (RemoteData.Loaded system) model.systems
                        , systems3d = SystemDict.insert system.id sys model.systems3d
                        , seed = seed
                    }
                        |> Update.succeed

                -- 3d stuff
                Zoomed value ->
                    case Json.Decode.decodeValue decodeZoomEvent value of
                        Ok delta ->
                            setZoom model (delta * zoomMultiplier model.spaceFocus)

                        Err _ ->
                            model |> Update.succeed

                ZoomPressed change ->
                    setZoom model (change * zoomMultiplier model.spaceFocus)

                RotationPressed change ->
                    { model
                        | viewRotation =
                            toFloat (remainderBy 360 (floor (model.viewRotation + change)))
                    }
                        |> Update.succeed

                PitchPressed change ->
                    { model
                        | eyeHeight = model.eyeHeight + change
                    }
                        |> Update.succeed


withTab : { tab : Maybe Route.GameTab, model : Model, toMsg : Msg -> msg, toModel : Model -> model } -> Update model msg
withTab ({ model } as opts) =
    Update.mapMsg opts.toMsg <|
        Update.mapModel opts.toModel <|
            case opts.tab of
                Just tab ->
                    { model | tab = tab }
                        |> (case tab of
                                Route.Waypoints details ->
                                    case details.id of
                                        Nothing ->
                                            Update.succeed

                                        Just (Route.ViewSystem systemId) ->
                                            systemSelected systemId

                                        Just (Route.ViewWaypoint waypointId) ->
                                            waytpointSelected waypointId

                                _ ->
                                    Update.succeed
                           )

                Nothing ->
                    model
                        |> Update.succeed


systemSelected : SpaceTrader.Point.System.System -> Model -> Update Model Msg
systemSelected systemId model =
    { model
        | systems =
            Cacheable.update SystemDict.update
                systemId
                (\maybeSystem ->
                    case maybeSystem of
                        Just (Loaded system) ->
                            Just (Loaded system)

                        _ ->
                            Just Loading
                )
                model.systems
    }
        |> Update.succeed
        |> (case Cacheable.get SystemDict.get systemId model.systems of
                Just (Loaded _) ->
                    identity

                Just Loading ->
                    identity

                _ ->
                    Update.withCmd
                        (SpaceTrader.Api.getSystem SystemResponded
                            { token = model.accessToken
                            , systemId = systemId
                            }
                        )
           )


waytpointSelected : SpaceTrader.Point.Waypoint.Waypoint -> Model -> Update Model Msg
waytpointSelected waypointId model =
    { model
        | waypoints =
            WaypointDict.update waypointId
                (\maybeWaypoint ->
                    case maybeWaypoint of
                        Just (Loaded waypoint) ->
                            Just (Loaded waypoint)

                        _ ->
                            Just Loading
                )
                model.waypoints
    }
        |> Update.succeed
        |> (case WaypointDict.get waypointId model.waypoints of
                Just (Loaded _) ->
                    identity

                Just Loading ->
                    identity

                _ ->
                    Update.withRequest (WaypointResponded waypointId)
                        (SpaceTrader.Api.getWaypoint
                            { token = model.accessToken
                            , waypointId = waypointId
                            }
                        )
           )


setZoom : Model -> Float -> Update Model Msg
setZoom model delta =
    { model | zoom = max 5000000 (model.zoom + delta) }
        |> Update.succeed


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


buildSystem3d : Random.Seed -> { a | x : Int, y : Int } -> ( ( Point3d Meters Shared.LightYear, Scene3d.Entity Shared.ScaledViewPoint ), Random.Seed )
buildSystem3d seed system =
    let
        zMax =
            Point2d.unitless (toFloat system.x) (toFloat system.y)
                |> Point2d.distanceFrom Point2d.origin
                |> Quantity.toFloat
                |> normalize 0 45000
                |> (\a -> 1 - a)
                |> zScale
                |> Tuple.second

        ( z, nextSeed ) =
            Random.step
                (Random.float -zMax zMax)
                seed

        point : Point3d Meters Shared.LightYear
        point =
            Point3d.xyz
                (system.x |> toFloat |> Length.lightYears)
                (system.y |> toFloat |> Length.lightYears)
                (Length.lightYears z)
    in
    ( ( point
      , renderSystem point
      )
    , nextSeed
    )


normalize : Float -> Float -> Float -> Float
normalize min max value =
    (value - min) / (max - min)


{-| <https://www.desmos.com/calculator/vxq5jjmlyu>
-}
zScale : Float -> ( Float, Float )
zScale t =
    let
        -- the max distance from the center of the galaxy
        x_0 =
            45000

        -- magic number
        x_1 =
            5500

        -- magic number
        x_2 =
            21000

        -- magic number
        x_3 =
            0

        -- max height, magic
        y_0 =
            0

        -- magic number
        y_1 =
            600

        -- magic number
        y_2 =
            4800

        -- magic number
        y_3 =
            4400
    in
    ( (1 - t) * ((1 - t) * ((1 - t) * x_0 + t * x_1) + t * ((1 - t) * x_1 + t * x_2)) + t * ((1 - t) * ((1 - t) * x_1 + t * x_2) + t * ((1 - t) * x_2 + t * x_3)), (1 - t) * ((1 - t) * ((1 - t) * y_0 + t * y_1) + t * ((1 - t) * y_1 + t * y_2)) + t * ((1 - t) * ((1 - t) * y_1 + t * y_2) + t * ((1 - t) * y_2 + t * y_3)) )


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


navLink : { label : String, route : Route } -> Bool -> Html Msg
navLink opts focused =
    Html.a
        [ Html.Attributes.classList
            [ ( "nav-button", True )
            , ( "nav-button-focused", focused )
            ]
        , Html.Attributes.href (Route.toUrlString opts.route)
        , Ui.grid
        ]
        [ Html.div [ Html.Attributes.class "nav-button-top", Ui.justify.end ] []
        , Html.div
            [ Html.Attributes.class "nav-button-middle"
            , Ui.grid
            ]
            [ Html.span
                [ Html.Attributes.style "font-size" "1rem" ]
                [ Html.text opts.label ]
            ]
        , Html.div [ Html.Attributes.class "nav-button-bottom", Ui.justify.end ] []
        ]


view : Shared.Model -> Model -> Html Msg
view shared model =
    Html.div
        [ Html.Attributes.style "display" "grid"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "grid-template-columns" "15rem 1fr"
        , Html.Attributes.style "grid-template-areas" """
            "sidebar content"
        """
        ]
        [ Html.nav
            [ Html.Attributes.class "sidebar"
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "display" "flex"
            , Html.Attributes.style "flex-direction" "column"
            , Html.Attributes.style "align-content" "start"
            , Html.Attributes.style "background-color" "var(--blue-dark)"
            ]
            [ Ui.header.one
                [ Ui.justify.center
                , Html.Attributes.style "padding" "1rem"
                , Html.Attributes.style "background-color" "var(--blue-dark)"
                , Html.Attributes.style "white-space" "nowrap"
                , Html.Attributes.style "color" "var(--red)"
                , Html.Attributes.style "font-weight" "bold"
                , Html.Attributes.style "text-align" "center"
                , Html.Attributes.style "-webkit-text-stroke" "0.1rem var(--yellow)"
                ]
                [ Html.text "Space Trader" ]
            , Html.div
                [ Ui.align.center
                , Html.Attributes.style "padding" "1rem"
                , Html.Attributes.style "border" "0.125rem solid"
                , Html.Attributes.style "border-radius" "0.5rem"
                , Html.Attributes.style "color" "var(--blue-light)"
                ]
                [ case model.agent of
                    Loading ->
                        Html.text "Loading agent..."

                    Failure _ ->
                        Html.text "Failed to load agent"

                    Loaded agent ->
                        Html.div
                            [ Ui.grid
                            ]
                            [ Html.span [ Html.Attributes.style "color" "var(--blue-light)" ]
                                [ Html.text agent.callsign ]
                            , Html.div
                                []
                                [ Html.span
                                    [ Html.Attributes.style "color" "var(--blue-light)"
                                    , Html.Attributes.style "font-weight" "bold"
                                    ]
                                    [ Html.text "Credits: " ]
                                , Html.span
                                    [ Html.Attributes.style "color" "var(--blue-light)"
                                    ]
                                    [ agent.credits
                                        |> toFloat
                                        |> FormatNumber.format FormatNumber.Locales.usLocale
                                        |> (++) "₩"
                                        |> Html.text
                                    ]
                                ]
                            ]
                ]
            , navLink
                { label = "Ships"
                , route = Route.Game { tab = Just Route.Ships }
                }
                (model.tab == Route.Ships)
            , navLink
                { label = "Contracts"
                , route = Route.Game { tab = Just Route.Contracts }
                }
                (model.tab == Route.Contracts)
            , navLink
                { label = "Waypoints"
                , route = Route.Game { tab = Just (Route.Waypoints { id = Nothing }) }
                }
                (case model.tab of
                    Route.Waypoints _ ->
                        True

                    _ ->
                        False
                )

            -- , Ui.Button.default []
            --     { label = Html.text "⚙️"
            --     , onClick = Nothing -- Just OpenSettingsClicked
            --     }
            , Html.div [ Html.Attributes.style "height" "100%" ] []
            , Ui.Button.default
                [ Html.Attributes.style "float" "right"
                , Html.Attributes.style "color" "var(--blue-light)"
                , Html.Attributes.style "margin" "0 1rem 1rem 1rem"
                ]
                { label = Html.text "Logout"
                , onClick = Just LogoutClicked
                }
            ]
        , Html.main_
            [ Html.Attributes.class "content"
            , Html.Attributes.style "padding" "1rem"
            , Html.Attributes.style "background-color" "var(--blue)"
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "overflow-y" "auto"
            ]
            [ case model.tab of
                Route.Ships ->
                    model.myShips
                        |> Dict.values
                        |> List.map
                            (Ui.Ship.view
                                { onDock = ShipDockRequested
                                , onOrbit = ShipOrbitRequested
                                , onMove = ShipMoveRequested
                                }
                            )
                        |> Html.div
                            [ Ui.grid
                            , Html.Attributes.style "grid-template-columns" "1fr 1fr"
                            , Ui.gap 1
                            ]
                        |> viewContent Nothing "My Ships"

                Route.Contracts ->
                    model.myContracts
                        |> Dict.values
                        |> List.map
                            (Ui.Contract.view
                                { timeZone = shared.timeZone
                                , currentTime = shared.currentTime
                                }
                            )
                        |> Html.div
                            [ Ui.grid
                            , Html.Attributes.style "grid-template-columns" "1fr 1fr 1fr"
                            , Ui.gap 1
                            ]
                        |> viewContent Nothing "My Contracts"

                Route.Waypoints details ->
                    case details.id of
                        Nothing ->
                            viewSystem model Nothing

                        Just (Route.ViewSystem systemId) ->
                            viewSystem model <| Just systemId

                        Just (Route.ViewWaypoint waypointId) ->
                            viewWaypoint model waypointId
            ]
        ]


viewSystem : Model -> Maybe SpaceTrader.Point.System.System -> Html Msg
viewSystem model maybeSystemId =
    Html.div [ Ui.grid, Ui.gap 0.5 ]
        [ Ui.Galaxy3d.viewSystems
            { onSystemClick = SystemClicked
            , onZoom = Zoomed
            , onZoomPress = ZoomPressed
            , onRotationPress = RotationPressed
            , onPitchPress = PitchPressed
            , selected =
                case maybeSystemId of
                    Nothing ->
                        Nothing

                    Just systemId ->
                        model.systems
                            |> Cacheable.get SystemDict.get systemId
                            |> Maybe.andThen RemoteData.toMaybe
                            |> Maybe.map .id
            }
            { galaxyViewSize = { width = 750, height = 500 }
            , zoom = model.zoom
            , viewRotation = model.viewRotation
            , systems = SystemDict.toList model.systems3d
            , eyeHeight = model.eyeHeight
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
        , let
            selectedSystem : Maybe (RemoteData.RemoteData SpaceTrader.System.System)
            selectedSystem =
                case maybeSystemId of
                    Nothing ->
                        Nothing

                    Just systemId ->
                        model.systems
                            |> Cacheable.get SystemDict.get systemId
          in
          case selectedSystem of
            Nothing ->
                Html.text ""

            Just Loading ->
                Html.text "Loading System..."

            Just (Failure error) ->
                Html.text ("Failed to load system: " ++ error)

            Just (Loaded system) ->
                Ui.System.view
                    { myShips = Dict.values model.myShips
                    , onCreateSurveyClicked = CreateSurveyRequested
                    }
                    system
        ]


viewWaypoint : Model -> SpaceTrader.Point.Waypoint.Waypoint -> Html Msg
viewWaypoint model waypointId =
    viewContent
        (Just
            (Route.Game
                { tab =
                    Just
                        (Route.Waypoints
                            { id =
                                waypointId
                                    |> SpaceTrader.Point.Waypoint.toSystem
                                    |> Route.ViewSystem
                                    |> Just
                            }
                        )
                }
            )
        )
        ("Waypoint: " ++ SpaceTrader.Point.Waypoint.toShortLabel waypointId)
        (case WaypointDict.get waypointId model.waypoints of
            Nothing ->
                Html.text "Waypoint not found"

            Just Loading ->
                Html.text "Gathering waypoints data..."

            Just (Failure _) ->
                Html.text "Failed to load waypoint data"

            Just (Loaded waypoint) ->
                -- { id : SpaceTrader.Point.Waypoint.Waypoint
                -- , type_ : SpaceTrader.Waypoint.Type.Type
                -- , system : SpaceTrader.Point.System.System
                -- , x : Int
                -- , y : Int
                -- , orbitals : List String
                -- , traits : List Trait
                -- , faction : Maybe String
                -- , chart : Maybe Chart
                -- }
                let
                    shipsHere : List SpaceTrader.Ship.Ship
                    shipsHere =
                        model.myShips
                            |> Dict.values
                            |> List.filterMap
                                (\ship ->
                                    if ship.nav.waypoint == waypoint.id then
                                        Just ship

                                    else
                                        Nothing
                                )
                in
                Html.div
                    [ Ui.grid
                    , Ui.gap 1
                    ]
                    [ Html.span []
                        [ Html.text <| SpaceTrader.Waypoint.Type.toLabel waypoint.type_ ]
                    , waypoint.traits
                        |> List.map
                            (\trait ->
                                Html.li [ Html.Attributes.style "max-width" "35rem" ]
                                    [ Html.dl []
                                        [ Html.dt [ Html.Attributes.style "font-weight" "bold" ]
                                            [ Html.text (trait.name ++ ": ") ]
                                        , Html.dd [] [ Html.text trait.description ]
                                        ]
                                    ]
                            )
                        |> Html.ul []
                    , Html.div []
                        [ Html.span [] [ Html.text "My ships here:" ]
                        , case shipsHere of
                            [] ->
                                Html.text " None"

                            _ ->
                                shipsHere
                                    |> List.map
                                        (\ship ->
                                            Html.li []
                                                [ Html.text ship.id ]
                                        )
                                    |> Html.ul []
                        ]
                    , Html.span []
                        [ Html.text
                            ("Faction: "
                                ++ (case waypoint.faction of
                                        Nothing ->
                                            "None"

                                        Just faction ->
                                            faction
                                   )
                            )
                        ]
                    , Html.span []
                        [ Html.text "Orbitals:" ]
                    , waypoint.orbitals
                        |> List.map
                            (\orbital ->
                                Html.li
                                    []
                                    [ Ui.link []
                                        { label =
                                            orbital
                                                |> SpaceTrader.Point.Waypoint.toLabel
                                                |> Html.text
                                        , route = Route.fromWaypoint orbital
                                        }
                                    ]
                            )
                        |> Html.ul []
                    ]
        )


viewContent : Maybe Route -> String -> Html msg -> Html msg
viewContent backRoute title content =
    Html.div []
        [ Ui.header.two
            [ Html.Attributes.style "margin-bottom" "1rem"
            ]
            [ backRoute
                |> Maybe.map
                    (\route ->
                        Ui.link [ Html.Attributes.style "margin-right" "1rem" ]
                            { label = Html.text "<"
                            , route = route
                            }
                    )
                |> Maybe.withDefault Ui.none
            , Html.text title
            ]
        , content
        ]
