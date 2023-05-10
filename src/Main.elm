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
import SpaceTrader.Agent
import SpaceTrader.Api
import SpaceTrader.Contract
import SpaceTrader.Faction
import SpaceTrader.Ship
import SpaceTrader.Waypoint
import Task
import Time
import Ui
import Ui.Button
import Ui.Contract
import Ui.Form
import Ui.Form.Field


main : Program Json.Encode.Value Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Model
    = Unregistered UnregisteredModel
    | Registered RegisteredModel


type alias UnregisteredModel =
    { registerFormModel : Form.Model
    , submittingRegistration : Bool
    , registrationServerError : Maybe String
    , loginFormModel : Form.Model
    , submittingLogin : Bool
    , loginServerError : Maybe String
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    }


type alias RegisteredModel =
    { accessToken : String
    , agent : SpaceTrader.Agent.Agent
    , waypoints : Dict String SpaceTrader.Waypoint.Waypoint
    , myContracts : Dict String SpaceTrader.Contract.Contract
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    }


init : Json.Encode.Value -> ( Model, Cmd Msg )
init flags =
    case Json.Decode.decodeValue decodeFlags flags of
        Err _ ->
            ( Unregistered
                { registerFormModel = Form.init
                , submittingRegistration = False
                , registrationServerError = Nothing
                , loginFormModel = Form.init
                , submittingLogin = False
                , loginServerError = Nothing
                , timeZone = Time.utc
                , currentTime = Time.millisToPosix 0
                }
            , Task.map2 CurrentTimeAndZoneReceived
                Time.now
                Time.here
                |> Task.perform identity
            )

        Ok { accessToken } ->
            ( Unregistered
                { registerFormModel = Form.init
                , submittingRegistration = False
                , registrationServerError = Nothing
                , loginFormModel = Form.init
                , submittingLogin = True
                , loginServerError = Nothing
                , timeZone = Time.utc
                , currentTime = Time.millisToPosix 0
                }
            , Cmd.batch
                [ SpaceTrader.Api.myAgent (LoginResponded accessToken)
                    { token = accessToken
                    }
                , Task.map2 CurrentTimeAndZoneReceived
                    Time.now
                    Time.here
                    |> Task.perform identity
                ]
            )


decodeFlags : Json.Decode.Decoder { accessToken : String }
decodeFlags =
    Json.Decode.map
        (\accessToken ->
            { accessToken = accessToken
            }
        )
        (Json.Decode.field "accessToken" Json.Decode.string)


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (5 * 1000 * 60) CurrentTimeReceived


port setToken : String -> Cmd msg


port clearToken : () -> Cmd msg


type Msg
    = CurrentTimeReceived Time.Posix
    | CurrentTimeAndZoneReceived Time.Posix Time.Zone
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
    | WaypointResponded String (Result Http.Error SpaceTrader.Waypoint.Waypoint)
    | MyContractsResponded (Result Http.Error (List SpaceTrader.Contract.Contract))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case model of
        Unregistered m ->
            updateUnregistered msg m

        Registered m ->
            updateRegistered msg m


updateUnregistered : Msg -> UnregisteredModel -> ( Model, Cmd Msg )
updateUnregistered msg model =
    case msg of
        CurrentTimeReceived time ->
            ( Unregistered
                { model
                    | currentTime = time
                }
            , Cmd.none
            )

        CurrentTimeAndZoneReceived time zone ->
            ( Unregistered
                { model
                    | currentTime = time
                    , timeZone = zone
                }
            , Cmd.none
            )

        -- registration
        RegistrationFormMsg msg_ ->
            let
                ( registerFormModel, formCmd ) =
                    Form.update msg_ model.registerFormModel
            in
            ( Unregistered
                { model
                    | registerFormModel = registerFormModel
                }
            , formCmd
            )

        RegistrationFormSubmitted { parsed } ->
            case parsed of
                Form.Valid registerData ->
                    ( Unregistered
                        { model
                            | submittingRegistration = True
                        }
                    , SpaceTrader.Api.register RegistrationResponded
                        registerData
                    )

                Form.Invalid _ _ ->
                    ( Unregistered model, Cmd.none )

        RegistrationResponded (Err err) ->
            -- Debug.todo (Debug.toString err)
            ( Unregistered
                { model
                    | submittingRegistration = False
                    , registrationServerError = Just (Debug.toString err)
                }
            , Cmd.none
            )

        RegistrationResponded (Ok data) ->
            ( initRegistered
                { accessToken = data.token
                , agent = data.agent
                , timeZone = model.timeZone
                , currentTime = model.currentTime
                }
            , setToken data.token
            )

        -- login
        LoginFormMsg msg_ ->
            let
                ( loginFormModel, formCmd ) =
                    Form.update msg_ model.loginFormModel
            in
            ( Unregistered
                { model
                    | loginFormModel = loginFormModel
                }
            , formCmd
            )

        LoginFormSubmitted { parsed } ->
            case parsed of
                Form.Valid loginData ->
                    ( Unregistered { model | submittingLogin = True }
                    , SpaceTrader.Api.myAgent (LoginResponded loginData.accessToken)
                        { token = loginData.accessToken
                        }
                    )

                Form.Invalid _ _ ->
                    ( Unregistered model, Cmd.none )

        LoginResponded _ (Err err) ->
            ( Unregistered
                { model
                    | submittingLogin = False
                    , loginServerError = Just (Debug.toString err)
                }
            , Cmd.none
            )

        LoginResponded accessToken (Ok agent) ->
            ( initRegistered
                { accessToken = accessToken
                , agent = agent
                , timeZone = model.timeZone
                , currentTime = model.currentTime
                }
            , Cmd.batch
                [ SpaceTrader.Api.myContracts MyContractsResponded
                    { token = accessToken }
                , setToken accessToken
                ]
            )

        _ ->
            ( Unregistered model, Cmd.none )


initRegistered :
    { accessToken : String
    , agent : SpaceTrader.Agent.Agent
    , timeZone : Time.Zone
    , currentTime : Time.Posix
    }
    -> Model
initRegistered opt =
    Registered
        { accessToken = opt.accessToken
        , agent = opt.agent
        , waypoints = Dict.empty
        , myContracts = Dict.empty
        , timeZone = opt.timeZone
        , currentTime = opt.currentTime
        }


updateRegistered : Msg -> RegisteredModel -> ( Model, Cmd Msg )
updateRegistered msg model =
    case msg of
        CurrentTimeReceived time ->
            ( Registered
                { model
                    | currentTime = time
                }
            , Cmd.none
            )

        CurrentTimeAndZoneReceived time zone ->
            ( Registered
                { model
                    | currentTime = time
                    , timeZone = zone
                }
            , Cmd.none
            )

        LogoutClicked ->
            ( Unregistered
                { registerFormModel = Form.init
                , submittingRegistration = False
                , registrationServerError = Nothing
                , loginFormModel = Form.init
                , submittingLogin = False
                , loginServerError = Nothing
                , timeZone = model.timeZone
                , currentTime = model.currentTime
                }
            , clearToken ()
            )

        WaypointResponded _ (Err err) ->
            Debug.todo (Debug.toString err)

        WaypointResponded id (Ok waypoint) ->
            ( Registered
                { model
                    | waypoints = Dict.insert id waypoint model.waypoints
                }
            , Cmd.none
            )

        MyContractsResponded (Err err) ->
            Debug.todo (Debug.toString err)

        MyContractsResponded (Ok contracts) ->
            ( Registered
                { model
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

        _ ->
            ( Registered model, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "SpaceTrader"
    , body =
        [ Ui.column
            []
            [ Ui.header.one
                [ Ui.center.justify
                , Html.Attributes.style "padding" "1rem"
                ]
                [ Html.text "SpaceTrader" ]
            , case model of
                Unregistered m ->
                    viewUnregistered m

                Registered m ->
                    viewRegistered m
            ]
        ]
    }


viewUnregistered : UnregisteredModel -> Html Msg
viewUnregistered model =
    Ui.row
        [ Ui.center.justify
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


viewRegistered : RegisteredModel -> Html Msg
viewRegistered model =
    Ui.column
        [ Html.Attributes.style "justify-self" "start"
        , Html.Attributes.style "padding" "1rem"
        ]
        [ Ui.viewLabelGroup
            (Html.div []
                [ Html.text "Agent"
                , Ui.Button.view
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
            |> List.map (Ui.Contract.view model.timeZone model.currentTime)
            |> (::) (Html.h3 [] [ Html.text "Contracts" ])
            |> Html.div []
        ]
