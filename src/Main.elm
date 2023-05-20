module Main exposing (main)

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Cacheable exposing (Cacheable(..))
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
import Page.Game
import Page.Login
import Point3d exposing (Point3d)
import Port
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Scene3d
import Shared
import SpaceTrader.Agent
import SpaceTrader.Api
import SpaceTrader.Contract
import SpaceTrader.Faction
import SpaceTrader.Ship
import SpaceTrader.Ship.Nav
import SpaceTrader.System
import SpaceTrader.Waypoint
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
import Update
import Url exposing (Url)
import Util.Function


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }


type alias Model =
    { navKey : Browser.Navigation.Key
    , shared : Shared.Model
    , page : Page
    }


type Page
    = Login Page.Login.Model
    | Game Page.Game.Model


init : Json.Encode.Value -> Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url navKey =
    let
        appUrl =
            AppUrl.fromUrl url

        route =
            Route.fromAppUrl appUrl

        initialState =
            case Json.Decode.decodeValue decodeFlags flags of
                Err _ ->
                    { accessToken = Nothing
                    , cachedSystemd = Nothing
                    , shared =
                        Shared.init
                            { theme = Nothing
                            , systems = Nothing
                            }
                    }

                Ok { accessToken, settings, cached } ->
                    { accessToken = accessToken
                    , cachedSystemd = Just cached.systems
                    , shared =
                        Shared.init
                            { theme = Just settings.theme
                            , systems = Just cached.systems
                            }
                    }

        ( sharedModel, sharedCmd ) =
            initialState.shared

        ( pageModel, pageCmd ) =
            case route of
                Route.Login ->
                    ( Login <| Page.Login.init { systems = initialState.cachedSystemd }
                    , Cmd.none
                    )

                Route.Game ->
                    case initialState.accessToken of
                        Nothing ->
                            ( Login <| Page.Login.init { systems = initialState.cachedSystemd }
                            , Cmd.none
                            )

                        Just accessToken ->
                            Page.Game.init
                                { accessToken = accessToken
                                , agent = Nothing
                                , systems = initialState.cachedSystemd
                                }
                                |> Tuple.mapBoth Game (Cmd.map GameMsg)

                Route.NotFound ->
                    ( Login <| Page.Login.init { systems = initialState.cachedSystemd }
                    , Cmd.none
                    )
    in
    ( { navKey = navKey
      , shared = sharedModel
      , page = pageModel
      }
    , Cmd.batch
        [ Cmd.map SharedMsg sharedCmd
        , pageCmd
        ]
    )


decodeFlags :
    Json.Decode.Decoder
        { accessToken : Maybe String
        , settings : { theme : Ui.Theme.Theme }
        , cached : { systems : Dict String SpaceTrader.System.System }
        }
decodeFlags =
    Json.Decode.map3
        (\accessToken settings cached ->
            { accessToken = accessToken
            , settings = settings
            , cached = cached
            }
        )
        (Json.Decode.maybe (Json.Decode.field "accessToken" Json.Decode.string))
        (Json.Decode.field "settings" decodeSettings)
        (Json.Decode.field "cached" decodeCached)


decodeSettings : Json.Decode.Decoder { theme : Ui.Theme.Theme }
decodeSettings =
    Json.Decode.map
        (\theme ->
            { theme = theme
            }
        )
        (Json.Decode.maybe (Json.Decode.field "theme" Ui.Theme.decode)
            |> Json.Decode.map (Maybe.withDefault (List.NonEmpty.head Ui.Theme.themes))
        )


decodeCached : Json.Decode.Decoder { systems : Dict String SpaceTrader.System.System }
decodeCached =
    Json.Decode.map
        (\systems ->
            { systems = systems
            }
        )
        (Json.Decode.maybe (Json.Decode.field "systems" (Json.Decode.list SpaceTrader.System.decode))
            |> Json.Decode.map
                (Maybe.map (List.foldl (\system systems -> Dict.insert system.id system systems) Dict.empty)
                    >> Maybe.withDefault Dict.empty
                )
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SharedMsg (Shared.subscriptions model.shared)


type Msg
    = SharedMsg Shared.Msg
      -- common
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | FromEffect Update.Effect
      -- page
    | LoginMsg Page.Login.Msg
    | GameMsg Page.Game.Msg


updateShared : (Shared.Model -> Shared.Model) -> Model -> Model
updateShared fn model =
    { model | shared = fn model.shared }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SharedMsg msg_ ->
            Shared.update
                { msg = msg_
                , model = model.shared
                , toMsg = SharedMsg
                , toModel = \shared -> { model | shared = shared }
                }
                |> Update.toTuple { fromEffect = FromEffect }

        -- common
        OnUrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model
                    , Browser.Navigation.pushUrl model.navKey (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Browser.Navigation.load url
                    )

        OnUrlChange url ->
            let
                appUrl =
                    AppUrl.fromUrl url

                route =
                    Route.fromAppUrl appUrl
            in
            case route of
                Route.Login ->
                    -- TODO: Should we redirect here?
                    ( model, Cmd.none )

                Route.Game ->
                    case model.page of
                        Game _ ->
                            ( model, Cmd.none )

                        Login loginModel ->
                            case Dict.get "token" appUrl.queryParameters of
                                Just [ accessToken ] ->
                                    Page.Game.init
                                        { accessToken = accessToken
                                        , agent = Nothing
                                        , systems = loginModel.systems
                                        }
                                        |> Tuple.mapBoth
                                            (\gameModel -> { model | page = Game gameModel })
                                            (\cmd ->
                                                Cmd.batch
                                                    [ Cmd.map GameMsg cmd
                                                    , Browser.Navigation.replaceUrl model.navKey (Route.toUrlString Route.Game)
                                                    , Port.setToken accessToken
                                                    ]
                                            )

                                _ ->
                                    ( model, Cmd.none )

                Route.NotFound ->
                    ( model
                    , Cmd.none
                    )

        FromEffect (Update.Authenticated { accessToken, systems, agent }) ->
            case model.page of
                Login _ ->
                    Page.Game.init
                        { accessToken = accessToken
                        , agent = agent
                        , systems = systems
                        }
                        |> Tuple.mapBoth
                            (\gameModel -> { model | page = Game gameModel })
                            (\cmd ->
                                Cmd.batch
                                    [ Cmd.map GameMsg cmd
                                    , Browser.Navigation.replaceUrl model.navKey (Route.toUrlString Route.Game)
                                    ]
                            )

                Game _ ->
                    ( model, Cmd.none )

        FromEffect (Update.RouteChangeRequested route) ->
            ( model
            , Browser.Navigation.pushUrl model.navKey (Route.toUrlString route)
            )

        -- page
        LoginMsg msg_ ->
            case model.page of
                Game _ ->
                    ( model, Cmd.none )

                Login loginModel ->
                    Page.Login.update
                        { shared = model.shared
                        , msg = msg_
                        , model = loginModel
                        , toMsg = LoginMsg
                        , toModel = \newLoginModel -> { model | page = Login newLoginModel }
                        }
                        |> Update.toTuple { fromEffect = FromEffect }

        GameMsg msg_ ->
            case model.page of
                Login _ ->
                    ( model, Cmd.none )

                Game gameModel ->
                    Page.Game.update
                        { shared = model.shared
                        , msg = msg_
                        , model = gameModel
                        , toMsg = GameMsg
                        , toModel = \newGameModel -> { model | page = Game newGameModel }
                        }
                        |> Update.toTuple { fromEffect = FromEffect }


view : Model -> Browser.Document Msg
view model =
    { title = "SpaceTrader"
    , body =
        [ Shared.viewHeader model.shared
            |> Html.map SharedMsg
        , Ui.column
            [ Html.Attributes.class model.shared.theme.class
            , Html.Attributes.style "height" "100vh"
            , Html.Attributes.style "overflow-y" "auto"
            , Html.Attributes.style "padding-right" "1rem"
            , Html.Attributes.style "padding-bottom" "1rem"
            ]
            [ case model.page of
                Login m ->
                    Page.Login.view m
                        |> Html.map LoginMsg

                Game m ->
                    Page.Game.view model.shared m
                        |> Html.map GameMsg
            ]
        ]
            ++ (Shared.viewModals model.shared
                    |> List.map (Html.map SharedMsg)
               )
    }
