module Main exposing (Model, Msg, Page, main)

import AppUrl exposing (AppUrl)
import Browser
import Browser.Navigation
import Cacheable
import Dict exposing (Dict)
import Html
import Html.Attributes
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
import Svg
import Svg.Attributes
import Ui
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
                            { systems = Nothing
                            , settings = Nothing
                            }
                    }

                Ok { accessToken, settings, cached } ->
                    { accessToken = accessToken
                    , cachedSystemd = cached.systems
                    , shared =
                        Shared.init
                            { systems = cached.systems
                            , settings = Just settings
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
        , settings : { systemLimit : Int }
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
        (Json.Decode.field "settings" Shared.decodeSettings)
        (Json.Decode.field "cached" decodeCached)


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
                        Login _ ->
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
                                    case loginModel.registrationToken of
                                        Nothing ->
                                            ( model, Cmd.none )

                                        Just accessToken ->
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
        , Html.div
            [ Html.Attributes.style "background-color" "rgb(0 0 0 / 0.6)"
            , Ui.grid
            , Html.Attributes.style "grid-template-rows" "1fr 1fr"
            , Ui.gap 0.25
            , Html.Attributes.style "padding" "0.5rem 1rem"
            , Html.Attributes.style "border-radius" "0.5rem"
            , Html.Attributes.style "color" "var(--blue)"
            , Html.Attributes.style "position" "fixed"
            , Html.Attributes.style "bottom" "0"
            , Html.Attributes.style "right" "0"
            ]
            [ Html.span [ Html.Attributes.style "color" "var(--blue)" ]
                [ Html.text "A UI for "
                , Ui.externalLink []
                    { label = Html.text "SpaceTraders"
                    , link = "https://spacetraders.io/"
                    }
                ]
            , Ui.externalLink
                [ Ui.align.center
                ]
                { label =
                    Html.span [ Html.Attributes.style "color" "var(--blue-light)" ]
                        [ Html.text "Frontend repo"
                        , Svg.svg
                            [ Svg.Attributes.width "18"
                            , Svg.Attributes.height "16"
                            , Svg.Attributes.viewBox "0 0 98 96"
                            , Html.Attributes.style "margin-left" "0.5rem"
                            ]
                            [ Svg.path
                                [ Svg.Attributes.fillRule "evenodd"
                                , Svg.Attributes.clipRule "evenodd"
                                , Svg.Attributes.d "M48.854 0C21.839 0 0 22 0 49.217c0 21.756 13.993 40.172 33.405 46.69 2.427.49 3.316-1.059 3.316-2.362 0-1.141-.08-5.052-.08-9.127-13.59 2.934-16.42-5.867-16.42-5.867-2.184-5.704-5.42-7.17-5.42-7.17-4.448-3.015.324-3.015.324-3.015 4.934.326 7.523 5.052 7.523 5.052 4.367 7.496 11.404 5.378 14.235 4.074.404-3.178 1.699-5.378 3.074-6.6-10.839-1.141-22.243-5.378-22.243-24.283 0-5.378 1.94-9.778 5.014-13.2-.485-1.222-2.184-6.275.486-13.038 0 0 4.125-1.304 13.426 5.052a46.97 46.97 0 0 1 12.214-1.63c4.125 0 8.33.571 12.213 1.63 9.302-6.356 13.427-5.052 13.427-5.052 2.67 6.763.97 11.816.485 13.038 3.155 3.422 5.015 7.822 5.015 13.2 0 18.905-11.404 23.06-22.324 24.283 1.78 1.548 3.316 4.481 3.316 9.126 0 6.6-.08 11.897-.08 13.526 0 1.304.89 2.853 3.316 2.364 19.412-6.52 33.405-24.935 33.405-46.691C97.707 22 75.788 0 48.854 0z"
                                , Svg.Attributes.fill "var(--blue)"
                                ]
                                []
                            ]
                        ]
                , link = "https://github.com/wolfadex/space-traders-ui"
                }
            ]
        ]
            ++ (Shared.viewModals model.shared
                    |> List.map (Html.map SharedMsg)
               )
            ++ (case model.page of
                    Login m ->
                        Page.Login.viewModals model.shared m
                            |> List.map (Html.map LoginMsg)

                    Game _ ->
                        -- Page.Game.viewModals m
                        --     |> List.map (Html.map GameMsg)
                        []
               )
    }
