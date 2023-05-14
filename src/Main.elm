port module Main exposing (main)

import Browser
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
import Length
import List.NonEmpty
import Point3d
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
import Ui.Theme


main : Program Json.Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Cacheable a
    = Uncached
    | Loading { data : Dict String a, current : Int, max : Int }
    | Cached (Dict String a)


cachedData : Cacheable a -> Dict String a
cachedData cacheable =
    case cacheable of
        Uncached ->
            Dict.empty

        Loading { data } ->
            data

        Cached data ->
            data


type alias Model =
    { timeZone : Time.Zone
    , currentTime : Time.Posix
    , theme : Ui.Theme.Theme
    , authed : Authed

    -- cached data
    , systems : Cacheable SpaceTrader.System.System

    -- game stuffs
    , spaceFocus : SpaceFocus
    , zoom : Float
    , viewRotation : Float
    }


type Authed
    = Unregistered UnregisteredModel
    | Registered RegisteredModel


type alias UnregisteredModel =
    { registerFormModel : Form.Model
    , submittingRegistration : Bool
    , registrationServerError : Maybe String
    , loginFormModel : Form.Model
    , submittingLogin : Bool
    , loginServerError : Maybe String
    }


type alias RegisteredModel =
    { accessToken : String
    , agent : SpaceTrader.Agent.Agent
    , waypoints : Dict String SpaceTrader.Waypoint.Waypoint
    , myContracts : Dict String SpaceTrader.Contract.Contract
    , myShips : Dict String SpaceTrader.Ship.Ship
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeFlags flags of
        Err _ ->
            ( { timeZone = Time.utc
              , currentTime = Time.millisToPosix 0
              , theme = List.NonEmpty.head Ui.Theme.themes
              , authed =
                    Unregistered
                        { registerFormModel = Form.init
                        , submittingRegistration = False
                        , registrationServerError = Nothing
                        , loginFormModel = Form.init
                        , submittingLogin = False
                        , loginServerError = Nothing
                        }
              , systems = Uncached
              , spaceFocus = FGalaxy
              , viewRotation = 0

              -- a nice default
              , zoom = 6621539845261203 * 10000
              }
            , Task.map2 CurrentTimeAndZoneReceived
                Time.now
                Time.here
                |> Task.perform identity
            )

        Ok { accessToken, settings, cached } ->
            ( { timeZone = Time.utc
              , currentTime = Time.millisToPosix 0
              , theme = settings.theme
              , authed =
                    Unregistered
                        { registerFormModel = Form.init
                        , submittingRegistration = False
                        , registrationServerError = Nothing
                        , loginFormModel = Form.init
                        , submittingLogin =
                            case accessToken of
                                Nothing ->
                                    False

                                Just _ ->
                                    True
                        , loginServerError = Nothing
                        }
              , systems = Cached cached.systems
              , spaceFocus = FGalaxy
              , zoom =
                    cached.systems
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
              , viewRotation = 0
              }
            , Cmd.batch
                [ case accessToken of
                    Nothing ->
                        Cmd.none

                    Just token ->
                        SpaceTrader.Api.myAgent (LoginResponded token)
                            { token = token }
                , Task.map2 CurrentTimeAndZoneReceived
                    Time.now
                    Time.here
                    |> Task.perform identity
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
    Time.every (5 * 1000 * 60) CurrentTimeReceived


port setToken : String -> Cmd msg


port clearToken : () -> Cmd msg


port openModal : String -> Cmd msg


port closeModal : String -> Cmd msg


port storeSettings : Json.Encode.Value -> Cmd msg


port cacheSystems : Json.Encode.Value -> Cmd msg


saveSettings : Model -> Cmd msg
saveSettings model =
    storeSettings
        (Json.Encode.object
            [ ( "theme", Ui.Theme.encode model.theme )
            ]
        )


type
    Msg
    -- time
    = CurrentTimeReceived Time.Posix
    | CurrentTimeAndZoneReceived Time.Posix Time.Zone
      -- settings
    | OpenSettingsClicked
    | CloseSettingsClicked
    | ThemeSelected Ui.Theme.Theme
      -- auth
    | LogoutClicked
    | RegistrationFormMsg (Form.Msg Msg)
    | RegistrationFormSubmitted (Ui.Form.Submission String RegisterForm)
    | RegistrationResponded
        (Result
            Http.Error
            { agent : SpaceTrader.Agent.Agent
            , contract : SpaceTrader.Contract.Contract
            , faction : SpaceTrader.Faction.Faction
            , ship : SpaceTrader.Ship.Ship
            , token : String
            }
        )
    | LoginFormMsg (Form.Msg Msg)
    | LoginFormSubmitted (Ui.Form.Submission String LoginForm)
    | LoginResponded String (Result Http.Error SpaceTrader.Agent.Agent)
      -- game
    | WaypointResponded String (Result Http.Error SpaceTrader.Waypoint.Waypoint)
    | MyContractsResponded (Result Http.Error (List SpaceTrader.Contract.Contract))
    | MyShipsResponded (Result Http.Error (List SpaceTrader.Ship.Ship))
    | ShipDockRequested String
    | ShipDockResponded String (Result Http.Error SpaceTrader.Ship.Nav.Nav)
    | ShipOrbitRequested String
    | ShipOrbitResponded String (Result Http.Error SpaceTrader.Ship.Nav.Nav)
    | ShipMoveRequested String
    | SystemsLoadRequested
    | SystemsLongRequestMsg (Result Http.Error (SpaceTrader.Api.Msg SpaceTrader.System.System))
      -- 3d view
    | SystemClicked String
    | Zoomed Json.Encode.Value
    | ZoomPressed Float
    | RotationPressed Float


updateWithUnregistered : (UnregisteredModel -> ( UnregisteredModel, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
updateWithUnregistered f model =
    case model.authed of
        Unregistered m ->
            let
                ( newAuthedModel, cmd ) =
                    f m
            in
            ( { model | authed = Unregistered newAuthedModel }, cmd )

        Registered _ ->
            ( model, Cmd.none )


updateWithRegistered : (RegisteredModel -> ( RegisteredModel, Cmd Msg )) -> Model -> ( Model, Cmd Msg )
updateWithRegistered f model =
    case model.authed of
        Unregistered _ ->
            ( model, Cmd.none )

        Registered m ->
            let
                ( newAuthedModel, cmd ) =
                    f m
            in
            ( { model | authed = Registered newAuthedModel }, cmd )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- time
        CurrentTimeReceived time ->
            ( { model
                | currentTime = time
              }
            , Cmd.none
            )

        CurrentTimeAndZoneReceived time zone ->
            ( { model
                | currentTime = time
                , timeZone = zone
              }
            , Cmd.none
            )

        -- settings
        OpenSettingsClicked ->
            ( model
            , openModal modalIds.settings
            )

        CloseSettingsClicked ->
            ( model
            , Cmd.batch
                [ closeModal modalIds.settings
                , saveSettings model
                ]
            )

        ThemeSelected theme ->
            ( { model | theme = theme }
            , Cmd.none
            )

        ---- auth
        LogoutClicked ->
            ( { model
                | authed =
                    Unregistered
                        { registerFormModel = Form.init
                        , submittingRegistration = False
                        , registrationServerError = Nothing
                        , loginFormModel = Form.init
                        , submittingLogin = False
                        , loginServerError = Nothing
                        }
              }
            , clearToken ()
            )

        -- registration
        RegistrationFormMsg msg_ ->
            updateWithUnregistered
                (\unregisteredModel ->
                    let
                        ( registerFormModel, formCmd ) =
                            Form.update msg_ unregisteredModel.registerFormModel
                    in
                    ( { unregisteredModel
                        | registerFormModel = registerFormModel
                      }
                    , formCmd
                    )
                )
                model

        RegistrationFormSubmitted { parsed } ->
            updateWithUnregistered
                (\unregisteredModel ->
                    case parsed of
                        Form.Valid registerData ->
                            ( { unregisteredModel
                                | submittingRegistration = True
                              }
                            , SpaceTrader.Api.register RegistrationResponded
                                registerData
                            )

                        Form.Invalid _ _ ->
                            ( unregisteredModel, Cmd.none )
                )
                model

        RegistrationResponded (Err err) ->
            updateWithUnregistered
                (\unregisteredModel ->
                    ( { unregisteredModel
                        | submittingRegistration = False
                        , registrationServerError = Just (Debug.toString err)
                      }
                    , Cmd.none
                    )
                )
                model

        RegistrationResponded (Ok data) ->
            ( initRegistered
                { accessToken = data.token
                , agent = data.agent
                }
                model
            , setToken data.token
            )

        -- login
        LoginFormMsg msg_ ->
            updateWithUnregistered
                (\unregisteredModel ->
                    let
                        ( loginFormModel, formCmd ) =
                            Form.update msg_ unregisteredModel.loginFormModel
                    in
                    ( { unregisteredModel
                        | loginFormModel = loginFormModel
                      }
                    , formCmd
                    )
                )
                model

        LoginFormSubmitted { parsed } ->
            updateWithUnregistered
                (\unregisteredModel ->
                    case parsed of
                        Form.Valid loginData ->
                            ( { unregisteredModel | submittingLogin = True }
                            , SpaceTrader.Api.myAgent (LoginResponded loginData.accessToken)
                                { token = loginData.accessToken
                                }
                            )

                        Form.Invalid _ _ ->
                            ( unregisteredModel, Cmd.none )
                )
                model

        LoginResponded _ (Err err) ->
            updateWithUnregistered
                (\unregisteredModel ->
                    ( { unregisteredModel
                        | submittingLogin = False
                        , loginServerError = Just (Debug.toString err)
                      }
                    , Cmd.none
                    )
                )
                model

        LoginResponded accessToken (Ok agent) ->
            ( initRegistered
                { accessToken = accessToken
                , agent = agent
                }
                model
            , Cmd.batch
                [ SpaceTrader.Api.myContracts MyContractsResponded
                    { token = accessToken }
                , SpaceTrader.Api.myShips MyShipsResponded
                    { token = accessToken }
                , case model.systems of
                    Uncached ->
                        SpaceTrader.Api.getAllSystemsInit SystemsLongRequestMsg { token = accessToken }

                    _ ->
                        Cmd.none
                , setToken accessToken
                ]
            )

        WaypointResponded _ (Err err) ->
            Debug.todo (Debug.toString err)

        WaypointResponded id (Ok waypoint) ->
            updateWithRegistered
                (\registeredModel ->
                    ( { registeredModel
                        | waypoints = Dict.insert id waypoint registeredModel.waypoints
                      }
                    , Cmd.none
                    )
                )
                model

        MyContractsResponded (Err err) ->
            Debug.todo (Debug.toString err)

        MyContractsResponded (Ok contracts) ->
            updateWithRegistered
                (\registeredModel ->
                    ( { registeredModel
                        | myContracts =
                            List.foldl
                                (\contract dict ->
                                    Dict.insert contract.id contract dict
                                )
                                Dict.empty
                                contracts
                      }
                    , Cmd.none
                    )
                )
                model

        MyShipsResponded (Err err) ->
            Debug.todo (Debug.toString err)

        MyShipsResponded (Ok ships) ->
            updateWithRegistered
                (\registeredModel ->
                    ( { registeredModel
                        | myShips =
                            List.foldl
                                (\ship dict ->
                                    Dict.insert ship.id ship dict
                                )
                                Dict.empty
                                ships
                      }
                    , Cmd.none
                    )
                )
                model

        ShipDockRequested id ->
            updateWithRegistered
                (\registeredModel ->
                    ( registeredModel
                    , SpaceTrader.Api.dockShip (ShipDockResponded id)
                        { token = registeredModel.accessToken
                        , shipId = id
                        }
                    )
                )
                model

        ShipDockResponded _ (Err err) ->
            Debug.todo (Debug.toString err)

        ShipDockResponded id (Ok nav) ->
            updateWithRegistered
                (\registeredModel ->
                    ( { registeredModel
                        | myShips =
                            Dict.update id
                                (Maybe.map
                                    (\ship ->
                                        { ship
                                            | nav = nav
                                        }
                                    )
                                )
                                registeredModel.myShips
                      }
                    , Cmd.none
                    )
                )
                model

        ShipOrbitRequested id ->
            updateWithRegistered
                (\registeredModel ->
                    ( registeredModel
                    , SpaceTrader.Api.moveToOrbit (ShipOrbitResponded id)
                        { token = registeredModel.accessToken
                        , shipId = id
                        }
                    )
                )
                model

        ShipOrbitResponded _ (Err err) ->
            Debug.todo (Debug.toString err)

        ShipOrbitResponded id (Ok nav) ->
            updateWithRegistered
                (\registeredModel ->
                    ( { registeredModel
                        | myShips =
                            Dict.update id
                                (Maybe.map
                                    (\ship ->
                                        { ship
                                            | nav = nav
                                        }
                                    )
                                )
                                registeredModel.myShips
                      }
                    , Cmd.none
                    )
                )
                model

        ShipMoveRequested id ->
            Debug.todo ""

        SystemsLoadRequested ->
            updateWithRegistered
                (\registeredModel ->
                    ( registeredModel
                    , SpaceTrader.Api.getAllSystemsInit SystemsLongRequestMsg
                        { token = registeredModel.accessToken }
                    )
                )
                { model
                    | systems =
                        Loading
                            { data = cachedData model.systems
                            , current = 0
                            , max = 1
                            }
                }

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
                                (cachedData model.systems)
                                systems
                    in
                    ( { model | systems = Cached updatedSystems }
                    , updatedSystems
                        |> Dict.values
                        |> Json.Encode.list SpaceTrader.System.encode
                        |> cacheSystems
                    )

                SpaceTrader.Api.NeedsMore data ->
                    let
                        updatedSystems =
                            List.foldl
                                (\system dict ->
                                    Dict.insert system.id system dict
                                )
                                (cachedData model.systems)
                                data.data
                    in
                    ( { model | systems = Loading { data = updatedSystems, current = data.current, max = data.max } }
                    , Cmd.batch
                        [ SpaceTrader.Api.getAllSystemsUpdate SystemsLongRequestMsg data
                        , updatedSystems
                            |> Dict.values
                            |> Json.Encode.list SpaceTrader.System.encode
                            |> cacheSystems
                        ]
                    )

        -- 3d view
        SystemClicked systemId ->
            Debug.todo ""

        Zoomed value ->
            case Json.Decode.decodeValue decodeZoomEvent value of
                Ok delta ->
                    setZoom model (delta * zoomMultiplier model.spaceFocus)

                Err _ ->
                    ( model, Cmd.none )

        ZoomPressed change ->
            setZoom model (change * zoomMultiplier model.spaceFocus)

        RotationPressed change ->
            ( { model | viewRotation = toFloat (remainderBy 360 (floor (model.viewRotation + change))) }
            , Cmd.none
            )


setZoom : Model -> Float -> ( Model, Cmd Msg )
setZoom model delta =
    ( { model | zoom = max 5000000 (model.zoom + delta) }, Cmd.none )


decodeZoomEvent : Json.Decode.Decoder Float
decodeZoomEvent =
    Json.Decode.field "deltaY" Json.Decode.float


zoomMultiplier : SpaceFocus -> Float
zoomMultiplier focus =
    case focus of
        FGalaxy ->
            -- One light year is 9460730000000000, this is about 10 light years
            94607300000000000

        FSystem _ ->
            10000000000

        FWaypoint _ ->
            1


type SpaceFocus
    = FGalaxy
    | FSystem String
    | FWaypoint String


initRegistered :
    { accessToken : String
    , agent : SpaceTrader.Agent.Agent
    }
    -> Model
    -> Model
initRegistered opt model =
    { model
        | authed =
            Registered
                { accessToken = opt.accessToken
                , agent = opt.agent
                , waypoints = Dict.empty
                , myContracts = Dict.empty
                , myShips = Dict.empty
                }
    }


modalIds : { settings : String }
modalIds =
    { settings = "settings" }


view : Model -> Browser.Document Msg
view model =
    { title = "SpaceTrader"
    , body =
        [ Ui.column
            [ Html.Attributes.class model.theme.class
            , Html.Attributes.style "height" "100vh"
            ]
            [ Ui.header.one
                [ Ui.justify.center
                , Html.Attributes.style "padding" "1rem"
                ]
                [ Html.text "SpaceTrader" ]
            , case model.authed of
                Unregistered m ->
                    viewUnregistered m

                Registered m ->
                    viewRegistered model m
            , Ui.Button.default
                [ Ui.justify.end
                , Ui.align.end
                , Html.Attributes.style "padding" "1rem"
                ]
                { label = Html.text "⚙️"
                , onClick = Just OpenSettingsClicked
                }
            , Ui.Modal.view modalIds.settings
                [-- Html.Attributes.class model.theme
                ]
                [ Ui.column
                    [ Ui.gap 1 ]
                    [ Html.label
                        []
                        [ Html.text "Theme: "
                        , Ui.Select.view
                            []
                            { options = List.NonEmpty.toList Ui.Theme.themes
                            , toString = .label
                            , value = model.theme
                            , onChange = ThemeSelected
                            }
                        ]
                    , Ui.Button.default
                        [ Ui.justify.end
                        , Ui.align.end
                        ]
                        { label = Html.text "Save Settings"
                        , onClick = Just CloseSettingsClicked
                        }
                    ]
                ]
            ]
        ]
    }


viewUnregistered : UnregisteredModel -> Html Msg
viewUnregistered model =
    Ui.row
        [ Ui.justify.center
        , Ui.gap 1
        ]
        [ Ui.Form.view
            { submitting = model.submittingRegistration
            , title = "Register"
            , model = model.registerFormModel
            , toMsg = RegistrationFormMsg
            , id = "registration-form"
            , onSubmit = RegistrationFormSubmitted
            , serverSideErrors =
                Maybe.map
                    (\registrationServerError ->
                        Dict.singleton "callsign" [ registrationServerError ]
                    )
                    model.registrationServerError
            }
            registrationForm
        , Ui.Form.view
            { submitting = model.submittingLogin
            , title = "Login"
            , model = model.loginFormModel
            , toMsg = LoginFormMsg
            , id = "login-form"
            , onSubmit = LoginFormSubmitted
            , serverSideErrors = Nothing
            }
            loginForm
        ]


type alias RegisterForm =
    { callsign : String
    , faction : SpaceTrader.Faction.Group
    }


registrationForm : Form.HtmlForm String RegisterForm input Msg
registrationForm =
    (\callsign faction ->
        { combine =
            Form.Validation.succeed RegisterForm
                |> Form.Validation.andMap
                    (Form.Validation.map
                        (\callsignValue ->
                            Form.Validation.succeed callsignValue
                                |> Form.Validation.withErrorIf (String.length callsignValue < 3) callsign "Must be at least 3 characters"
                                |> Form.Validation.withErrorIf (String.length callsignValue > 14) callsign "Must can't be more than 14 characters"
                        )
                        callsign
                        |> Form.Validation.andThen identity
                    )
                |> Form.Validation.andMap faction
        , view =
            \formState ->
                List.concat
                    [ Ui.Form.Field.text formState "Callsign" callsign
                    , Ui.Form.Field.select
                        { toString = SpaceTrader.Faction.groupToPrettyString }
                        formState
                        "Faction"
                        faction
                    , Ui.Form.Field.submit
                        { label =
                            if formState.submitting then
                                "Registering..."

                            else
                                "Register"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "callsign"
            (Form.Field.text
                |> Form.Field.required "Required"
            )
        |> Form.field "faction"
            (Form.Field.select
                (SpaceTrader.Faction.groups
                    |> List.map (\group -> ( SpaceTrader.Faction.groupToPrettyString group, group ))
                )
                identity
                |> Form.Field.required "Required"
            )


type alias LoginForm =
    { accessToken : String
    }


loginForm : Form.HtmlForm String LoginForm input Msg
loginForm =
    (\accessToken ->
        { combine =
            Form.Validation.succeed LoginForm
                |> Form.Validation.andMap accessToken
        , view =
            \formState ->
                List.concat
                    [ Ui.Form.Field.text formState "Access Token" accessToken
                    , Ui.Form.Field.submit
                        { label =
                            if formState.submitting then
                                "Logging in..."

                            else
                                "Login"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "accessToken"
            (Form.Field.text
                |> Form.Field.required "Required"
                |> Form.Field.password
            )


errorsView : Form.Context String input -> Form.Validation.Field String parsed kind -> Html msg
errorsView { submitAttempted, errors } field =
    if submitAttempted || Form.Validation.statusAtLeast Form.Validation.Blurred field then
        errors
            |> Form.errorsForField field
            |> List.map (\error -> Html.li [ Html.Attributes.style "color" "red" ] [ Html.text error ])
            |> Html.ul []

    else
        Html.ul [] []


viewField : Form.Context String input -> String -> Form.Validation.Field String parsed Form.FieldView.Input -> Html msg
viewField formState label field =
    Html.div []
        [ Html.label []
            [ Html.text (label ++ " ")
            , Form.FieldView.input [] field
            , errorsView formState field
            ]
        ]


viewRegistered : Model -> RegisteredModel -> Html Msg
viewRegistered m model =
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
                [ { label = "Callsign", value = model.agent.callsign }
                , { label = "Headquarters", value = model.agent.headquarters }
                , { label = "Credits", value = String.fromInt model.agent.credits }
                ]
            , model.myContracts
                |> Dict.values
                |> List.map (Ui.Contract.view m.timeZone m.currentTime)
                |> (::) (Html.h3 [] [ Html.text "My Contracts" ])
                |> Html.div []
            , model.myShips
                |> Dict.values
                |> List.map
                    (Ui.Ship.view
                        { onDock = ShipDockRequested
                        , onOrbit = ShipOrbitRequested
                        , onMove = ShipMoveRequested
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
                }
                { galaxyViewSize = { width = 750, height = 500 }
                , zoom = m.zoom
                , viewRotation = m.viewRotation
                , systems =
                    m.systems
                        |> cachedData
                        |> Dict.values
                }
            , case m.systems of
                Uncached ->
                    Ui.Button.default
                        []
                        { label = Html.text "Load Systems"
                        , onClick = Just SystemsLoadRequested
                        }

                Loading { current, max } ->
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
            ]
        ]
