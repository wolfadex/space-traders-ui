module Main exposing (Model, Msg, Page, main)

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Cacheable
import Dict exposing (Dict)
import Html
import Json.Decode
import Json.Encode
import List.NonEmpty
import Page.Game
import Page.Login
import Port
import RemoteData exposing (RemoteData(..))
import Route exposing (Route)
import Shared
import SpaceTrader.Point.SystemDict as SystemDict exposing (SystemDict)
import SpaceTrader.System
import Ui.Theme
import Update
import Url exposing (Url)


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
        appUrl : AppUrl
        appUrl =
            AppUrl.fromUrl url

        route : Route
        route =
            Route.fromAppUrl appUrl

        initialState : { accessToken : Maybe String, cachedSystemd : Maybe (SystemDict SpaceTrader.System.System), shared : ( Shared.Model, Cmd Shared.Msg ) }
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
                    , cachedSystemd = cached.systems
                    , shared =
                        Shared.init
                            { theme = Just settings.theme
                            , systems = cached.systems
                            }
                    }

        ( sharedModel, sharedCmd ) =
            initialState.shared

        ( pageModel, pageCmd ) =
            case route of
                Route.Login ->
                    Page.Login.init
                        { systems = initialState.cachedSystemd
                        , toMsg = LoginMsg
                        , toModel = \m -> Login m
                        }
                        |> Update.toTuple { fromEffect = FromEffect }

                Route.Game { tab } ->
                    case initialState.accessToken of
                        Nothing ->
                            Page.Login.init
                                { systems = initialState.cachedSystemd
                                , toMsg = LoginMsg
                                , toModel = \m -> Login m
                                }
                                |> Update.toTuple { fromEffect = FromEffect }

                        Just accessToken ->
                            Page.Game.init
                                { accessToken = accessToken
                                , agent = Nothing
                                , systems = initialState.cachedSystemd
                                , tab = tab
                                , toMsg = GameMsg
                                , toModel = \m -> Game m
                                }
                                |> Update.toTuple { fromEffect = FromEffect }

                Route.NotFound ->
                    Page.Login.init
                        { systems = initialState.cachedSystemd
                        , toMsg = LoginMsg
                        , toModel = \m -> Login m
                        }
                        |> Update.toTuple { fromEffect = FromEffect }
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
        , cached : { systems : Maybe (SystemDict SpaceTrader.System.System) }
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


decodeCached : Json.Decode.Decoder { systems : Maybe (SystemDict SpaceTrader.System.System) }
decodeCached =
    Json.Decode.map
        (\systems ->
            { systems = systems
            }
        )
        (Json.Decode.maybe (Json.Decode.field "systems" (Json.Decode.list SpaceTrader.System.decode))
            |> Json.Decode.map
                (Maybe.map
                    (List.foldl (\system systems -> SystemDict.insert system.id system systems)
                        SystemDict.empty
                    )
                )
        )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map SharedMsg (Shared.subscriptions model.shared)
        , case model.page of
            Login pageModel ->
                Sub.map LoginMsg (Page.Login.subscriptions pageModel)

            Game _ ->
                Sub.none
        ]


type Msg
    = SharedMsg Shared.Msg
      -- common
    | OnUrlRequest Browser.UrlRequest
    | OnUrlChange Url
    | FromEffect Update.Effect
      -- page
    | LoginMsg Page.Login.Msg
    | GameMsg Page.Game.Msg


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
                appUrl : AppUrl
                appUrl =
                    AppUrl.fromUrl url

                route : Route
                route =
                    Route.fromAppUrl appUrl
            in
            case route of
                Route.Login ->
                    case model.page of
                        Login loginModel ->
                            ( model, Cmd.none )

                        Game gameModel ->
                            Page.Login.init
                                { systems =
                                    gameModel.systems
                                        |> Cacheable.getData
                                        |> Maybe.map
                                            (SystemDict.toList
                                                >> List.filterMap
                                                    (\( id, val ) ->
                                                        case val of
                                                            Loaded system ->
                                                                Just ( id, system )

                                                            _ ->
                                                                Nothing
                                                    )
                                                >> SystemDict.fromList
                                            )
                                , toMsg = LoginMsg
                                , toModel = \m -> { model | page = Login m }
                                }
                                |> Update.toTuple { fromEffect = FromEffect }

                Route.Game { tab } ->
                    case model.page of
                        Game gameModel ->
                            Page.Game.withTab
                                { tab = tab
                                , model = gameModel
                                , toMsg = GameMsg
                                , toModel = \m -> { model | page = Game m }
                                }
                                |> Update.toTuple { fromEffect = FromEffect }

                        Login loginModel ->
                            case Dict.get "token" appUrl.queryParameters of
                                Just [ accessToken ] ->
                                    Page.Game.init
                                        { accessToken = accessToken
                                        , agent = Nothing
                                        , systems = loginModel.systems
                                        , tab = tab
                                        , toMsg = GameMsg
                                        , toModel = \m -> { model | page = Game m }
                                        }
                                        |> Update.toTuple { fromEffect = FromEffect }
                                        |> Tuple.mapSecond
                                            (\cmd ->
                                                Cmd.batch
                                                    [ cmd
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
                        , tab = Nothing
                        , toMsg = GameMsg
                        , toModel = \m -> { model | page = Game m }
                        }
                        |> Update.toTuple { fromEffect = FromEffect }

                Game _ ->
                    ( model, Cmd.none )

        FromEffect (Update.RouteChangeRequested route) ->
            ( model
            , Browser.Navigation.pushUrl model.navKey (Route.toUrlString route)
            )

        FromEffect (Update.RouteModifyRequested route) ->
            ( model
            , Browser.Navigation.replaceUrl model.navKey (Route.toUrlString route)
            )

        FromEffect (Update.PushNotification notification) ->
            Shared.pushNotification
                { model = model.shared
                , notification = notification
                , toMsg = SharedMsg
                , toModel = \shared -> { model | shared = shared }
                }
                |> Update.toTuple { fromEffect = FromEffect }

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
        [ --     Shared.viewHeader model.shared
          --     |> Html.map SharedMsg
          -- ,
          case model.page of
            Login m ->
                Page.Login.view m
                    |> Html.map LoginMsg

            Game m ->
                Page.Game.view model.shared m
                    |> Html.map GameMsg
        , Shared.viewNotifications model.shared
            |> Html.map SharedMsg
        ]
            ++ (Shared.viewModals model.shared
                    |> List.map (Html.map SharedMsg)
               )
    }
