module SpaceTrader.Api exposing
    ( Error(..)
    , Msg(..)
    , PagedMeta
    , createSurvey
    , dockShip
    , extractShip
    , getAllSystemsInit
    , getAllSystemsUpdate
    , getShip
    , getShipCooldown
    , getSystem
    , getWaypoint
    , moveToOrbit
    , myAgent
    , myContracts
    , myShips
    , navigateShip
    , register
    , sellCargo
    , setFlightMode
    )

import Http
import Id
import Json.Decode
import Json.Encode
import Process
import SpaceTrader.Agent exposing (Agent)
import SpaceTrader.Contract exposing (Contract)
import SpaceTrader.Faction exposing (Faction)
import SpaceTrader.Id exposing (ShipId)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship exposing (Ship)
import SpaceTrader.Ship.Cargo
import SpaceTrader.Ship.Cargo.Item
import SpaceTrader.Ship.Cooldown
import SpaceTrader.Ship.Extraction
import SpaceTrader.Ship.Fuel
import SpaceTrader.Ship.Nav
import SpaceTrader.Ship.Nav.FlightMode
import SpaceTrader.Survey
import SpaceTrader.System
import SpaceTrader.Waypoint
import Task exposing (Task)



-- AUTH


register :
    { callsign : String, faction : SpaceTrader.Faction.Group }
    ->
        Task
            Error
            { agent : Agent
            , contract : Contract
            , faction : Faction
            , ship : Ship
            , token : String
            }
register opts =
    newV2
        { url = [ "register" ]
        , decoder = decodeRegister
        }
        |> withMethodPost
        |> withBody
            (Json.Encode.object
                [ ( "symbol", Json.Encode.string opts.callsign )
                , ( "faction"
                  , opts.faction
                        |> SpaceTrader.Faction.groupToString
                        |> Json.Encode.string
                  )
                ]
            )
        |> sendRequest


decodeRegister : Json.Decode.Decoder { agent : Agent, contract : Contract, faction : Faction, ship : Ship, token : String }
decodeRegister =
    Json.Decode.map5
        (\agent contract faction ship token ->
            { agent = agent
            , contract = contract
            , faction = faction
            , ship = ship
            , token = token
            }
        )
        (Json.Decode.field "agent" SpaceTrader.Agent.decode)
        (Json.Decode.field "contract" SpaceTrader.Contract.decode)
        (Json.Decode.field "faction" SpaceTrader.Faction.decode)
        (Json.Decode.field "ship" SpaceTrader.Ship.decode)
        (Json.Decode.field "token" Json.Decode.string)



-- MY THINGS


myAgent : { token : String } -> Task Error SpaceTrader.Agent.Agent
myAgent options =
    newV2
        { url = [ "my", "agent" ]
        , decoder = SpaceTrader.Agent.decode
        }
        |> withToken options.token
        |> sendRequest


myContracts : { token : String } -> Task Error (List SpaceTrader.Contract.Contract)
myContracts options =
    newV2
        { url = [ "my", "contracts" ]
        , decoder = Json.Decode.list SpaceTrader.Contract.decode
        }
        |> withToken options.token
        |> sendRequest


myShips : { token : String } -> Task Error (List SpaceTrader.Ship.Ship)
myShips options =
    newV2
        { url = [ "my", "ships" ]
        , decoder = Json.Decode.list SpaceTrader.Ship.decode
        }
        |> withToken options.token
        |> sendRequest


moveToOrbit : { token : String, shipId : ShipId } -> Task Error SpaceTrader.Ship.Nav.Nav
moveToOrbit options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "orbit" ]
        , decoder = Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode
        }
        |> withMethodPost
        |> withToken options.token
        |> sendRequest


dockShip : { token : String, shipId : ShipId } -> Task Error SpaceTrader.Ship.Nav.Nav
dockShip options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "dock" ]
        , decoder = Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode
        }
        |> withMethodPost
        |> withToken options.token
        |> sendRequest


setFlightMode : { token : String, shipId : ShipId, flightMode : SpaceTrader.Ship.Nav.FlightMode.FlightMode } -> Task Error SpaceTrader.Ship.Nav.FlightMode.FlightMode
setFlightMode options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "nav" ]
        , decoder = Json.Decode.field "flightMode" SpaceTrader.Ship.Nav.FlightMode.decode
        }
        |> withMethodPatch
        |> withToken options.token
        |> withBody
            (Json.Encode.object
                [ ( "flightMode", SpaceTrader.Ship.Nav.FlightMode.encode options.flightMode )
                ]
            )
        |> sendRequest


navigateShip :
    { token : String
    , shipId : ShipId
    , destination : SpaceTrader.Point.Waypoint.Waypoint
    }
    -> Task Error { nav : SpaceTrader.Ship.Nav.Nav, fuel : SpaceTrader.Ship.Fuel.Fuel }
navigateShip options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "navigate" ]
        , decoder =
            Json.Decode.map2
                (\nav fuel ->
                    { nav = nav
                    , fuel = fuel
                    }
                )
                (Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode)
                (Json.Decode.field "fuel" SpaceTrader.Ship.Fuel.decode)
        }
        |> withMethodPost
        |> withToken options.token
        |> withBody
            (Json.Encode.object
                [ ( "waypointSymbol", SpaceTrader.Point.Waypoint.encode options.destination )
                ]
            )
        |> sendRequest


sellCargo :
    { token : String
    , shipId : ShipId
    , item : SpaceTrader.Ship.Cargo.Item.Item
    , quantity : Int
    }
    -> Task Error { agent : SpaceTrader.Agent.Agent, cargo : SpaceTrader.Ship.Cargo.Cargo }
sellCargo options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "sell" ]
        , decoder =
            Json.Decode.map2
                (\agent cargo ->
                    { agent = agent
                    , cargo = cargo
                    }
                )
                (Json.Decode.field "agent" SpaceTrader.Agent.decode)
                (Json.Decode.field "cargo" SpaceTrader.Ship.Cargo.decode)
        }
        |> withMethodPost
        |> withToken options.token
        |> withBody
            (Json.Encode.object
                [ ( "symbol", Json.Encode.string options.item.symbol )
                , ( "units", Json.Encode.int options.quantity )
                ]
            )
        |> sendRequest


getShip :
    { token : String
    , shipId : ShipId
    }
    -> Task Error SpaceTrader.Ship.Ship
getShip options =
    Task.map2
        (\ship cooldown ->
            { ship | cooldown = cooldown }
        )
        (newV2
            { url = [ "my", "ships", Id.toString options.shipId ]
            , decoder = SpaceTrader.Ship.decode
            }
            |> withToken options.token
            |> sendRequest
        )
        (getShipCooldown { token = options.token, shipId = options.shipId })


extractShip : { token : String, shipId : ShipId } -> Task Error { extraction : SpaceTrader.Ship.Extraction.Extraction, cooldown : SpaceTrader.Ship.Cooldown.Cooldown, cargo : SpaceTrader.Ship.Cargo.Cargo }
extractShip options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "extract" ]
        , decoder =
            Json.Decode.map3
                (\extraction cooldown cargo ->
                    { extraction = extraction
                    , cooldown = cooldown
                    , cargo = cargo
                    }
                )
                (Json.Decode.field "extraction" SpaceTrader.Ship.Extraction.decode)
                (Json.Decode.field "cooldown" SpaceTrader.Ship.Cooldown.decode)
                (Json.Decode.field "cargo" SpaceTrader.Ship.Cargo.decode)
        }
        |> withMethodPost
        |> withToken options.token
        |> sendRequest


getShipCooldown : { token : String, shipId : ShipId } -> Task Error (Maybe SpaceTrader.Ship.Cooldown.Cooldown)
getShipCooldown options =
    v2_3_nobody
        { method = "GET"
        , token = options.token
        , url = [ "my", "ships", Id.toString options.shipId, "cooldown" ]
        , body = Http.emptyBody
        , decoder =
            Json.Decode.oneOf
                [ Json.Decode.maybe SpaceTrader.Ship.Cooldown.decode
                , Json.Decode.null Nothing
                ]
        , ifNoBody = Nothing
        }



-- GENERAL


getAllSystemsInit : (Result Http.Error (Msg SpaceTrader.System.System) -> msg) -> { token : String } -> Cmd msg
getAllSystemsInit toMsg options =
    getAllInit toMsg
        { token = options.token
        , url = [ "systems" ]
        , decoder = SpaceTrader.System.decode
        }


getAllSystemsUpdate :
    (Result Http.Error (Msg SpaceTrader.System.System) -> msg)
    ->
        { token : String
        , url : List String
        , page : Int
        , decoder : Json.Decode.Decoder SpaceTrader.System.System
        , data : List SpaceTrader.System.System
        , current : Int
        , max : Int
        }
    -> Cmd msg
getAllSystemsUpdate =
    getAllUpdate


getSystem : { token : String, systemId : SpaceTrader.Point.System.System } -> Task Error SpaceTrader.System.System
getSystem options =
    newV2
        { url = [ "systems", SpaceTrader.Point.System.toKey options.systemId ]
        , decoder = SpaceTrader.System.decode
        }
        |> withToken options.token
        |> sendRequest


getWaypoint : { token : String, waypointId : SpaceTrader.Point.Waypoint.Waypoint } -> Task Error SpaceTrader.Waypoint.Waypoint
getWaypoint options =
    newV2
        { url =
            [ "systems"
            , options.waypointId
                |> SpaceTrader.Point.Waypoint.toSystem
                |> SpaceTrader.Point.System.toKey
            , "waypoints"
            , SpaceTrader.Point.Waypoint.toKey options.waypointId
            ]
        , decoder = SpaceTrader.Waypoint.decode
        }
        |> withToken options.token
        |> sendRequest


createSurvey : { token : String, shipId : ShipId } -> Task Error ( List SpaceTrader.Survey.Survey, SpaceTrader.Ship.Cooldown.Cooldown )
createSurvey options =
    newV2
        { url = [ "my", "ships", Id.toString options.shipId, "survey" ]
        , decoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "surveys" (Json.Decode.list SpaceTrader.Survey.decode))
                (Json.Decode.field "cooldown" SpaceTrader.Ship.Cooldown.decode)
        }
        |> withMethodPost
        |> withToken options.token
        |> sendRequest



-- HELPERS


type Msg a
    = Complete (List a)
    | NeedsMore
        { token : String
        , url : List String
        , page : Int
        , decoder : Json.Decode.Decoder a
        , data : List a
        , current : Int
        , max : Int
        }


getAllInit :
    (Result Http.Error (Msg a) -> msg)
    ->
        { token : String
        , url : List String
        , decoder : Json.Decode.Decoder a
        }
    -> Cmd msg
getAllInit toMsg opts =
    Http.task
        { method = "GET"
        , headers = [ Http.header "Authorization" ("Bearer " ++ opts.token) ]
        , url = toUrl (baseUri :: opts.url) ++ "?limit=20&page=1"
        , body = Http.emptyBody
        , resolver =
            Http.stringResolver <|
                jsonResolver <|
                    Json.Decode.map2 Tuple.pair
                        (decodeSuccess (Json.Decode.list opts.decoder))
                        decodePagedMeta
        , timeout = Nothing
        }
        |> Task.map
            (\( list, meta ) ->
                if meta.page * meta.limit < meta.total then
                    NeedsMore
                        { token = opts.token
                        , url = opts.url
                        , decoder = opts.decoder
                        , data = list
                        , page = meta.page + 1
                        , current = List.length list
                        , max = meta.total
                        }

                else
                    Complete list
            )
        |> Task.attempt toMsg


getAllUpdate :
    (Result Http.Error (Msg a) -> msg)
    ->
        { token : String
        , url : List String
        , page : Int
        , decoder : Json.Decode.Decoder a
        , data : List a
        , current : Int
        , max : Int
        }
    -> Cmd msg
getAllUpdate toMsg opts =
    Process.sleep 600
        |> Task.andThen
            (\() ->
                Http.task
                    { method = "GET"
                    , headers = [ Http.header "Authorization" ("Bearer " ++ opts.token) ]
                    , url = toUrl (baseUri :: opts.url) ++ "?limit=20&page=" ++ String.fromInt opts.page
                    , body = Http.emptyBody
                    , resolver =
                        Http.stringResolver <|
                            jsonResolver <|
                                Json.Decode.map2 Tuple.pair
                                    (decodeSuccess (Json.Decode.list opts.decoder))
                                    decodePagedMeta
                    , timeout = Nothing
                    }
                    |> Task.map
                        (\( list, meta ) ->
                            if meta.page * meta.limit < meta.total then
                                NeedsMore
                                    { token = opts.token
                                    , url = opts.url
                                    , decoder = opts.decoder
                                    , data = list
                                    , page = meta.page + 1
                                    , current = List.length list + opts.current
                                    , max = meta.total
                                    }

                            else
                                Complete list
                        )
            )
        |> Task.attempt toMsg


type Error
    = HttpError Http.Error
    | ApiErrorDecodeError String
    | ApiError { message : String, code : Int }


jsonResolver : Json.Decode.Decoder a -> Http.Response String -> Result Http.Error a
jsonResolver decode response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.BadStatus_ metadata _ ->
            Err (Http.BadStatus metadata.statusCode)

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString decode body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Json.Decode.errorToString err))


jsonResolver2 : Json.Decode.Decoder a -> Http.Response String -> Result Error a
jsonResolver2 decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (HttpError (Http.BadUrl url))

        Http.Timeout_ ->
            Err (HttpError Http.Timeout)

        Http.NetworkError_ ->
            Err (HttpError Http.NetworkError)

        Http.BadStatus_ metadata body ->
            if metadata.statusCode >= 400 && metadata.statusCode < 500 then
                case Json.Decode.decodeString decodeApiError body of
                    Ok err ->
                        Err err

                    Err err ->
                        Err (ApiErrorDecodeError (Json.Decode.errorToString err))

            else
                Err (HttpError (Http.BadStatus metadata.statusCode))

        Http.GoodStatus_ _ body ->
            case Json.Decode.decodeString (decodeSuccess decoder) body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (HttpError (Http.BadBody (Json.Decode.errorToString err)))


decodeApiError : Json.Decode.Decoder Error
decodeApiError =
    Json.Decode.field "error" <|
        Json.Decode.map2
            (\message code ->
                ApiError
                    { message = message
                    , code = code
                    }
            )
            (Json.Decode.field "message" Json.Decode.string)
            (Json.Decode.field "code" Json.Decode.int)


type alias PagedMeta =
    { total : Int
    , page : Int
    , limit : Int
    }


decodePagedMeta : Json.Decode.Decoder PagedMeta
decodePagedMeta =
    Json.Decode.field "meta" <|
        Json.Decode.map3 PagedMeta
            (Json.Decode.field "total" Json.Decode.int)
            (Json.Decode.field "page" Json.Decode.int)
            (Json.Decode.field "limit" Json.Decode.int)


baseUri : String
baseUri =
    "https://api.spacetraders.io/v2"


toUrl : List String -> String
toUrl =
    String.join "/"


decodeSuccess : Json.Decode.Decoder a -> Json.Decode.Decoder a
decodeSuccess decoder =
    Json.Decode.field "data" decoder


v2_3_nobody :
    { method : String
    , token : String
    , body : Http.Body
    , url : List String
    , decoder : Json.Decode.Decoder a
    , ifNoBody : a
    }
    -> Task Error a
v2_3_nobody opts =
    Http.task
        { method = opts.method
        , headers = [ Http.header "Authorization" ("Bearer " ++ opts.token) ]
        , url = toUrl (baseUri :: opts.url)
        , body = opts.body
        , resolver =
            Http.stringResolver <|
                \response ->
                    case response of
                        Http.GoodStatus_ _ "" ->
                            Ok opts.ifNoBody

                        _ ->
                            jsonResolver2 opts.decoder response
        , timeout = Nothing
        }



-- NOTE: Keeping for future reference
-- v2Paged : { method : String, token : String, url : List String, body : Http.Body, expect : Http.Expect msg, page : Int } -> Cmd msg
-- v2Paged options =
--     Http.request
--         { method = options.method
--         , headers =
--             [ Http.header "Authorization" ("Bearer " ++ options.token)
--             ]
--         , url = toUrl (baseUri :: options.url) ++ "?limit=20&page=" ++ String.fromInt options.page
--         , body = options.body
--         , expect = options.expect
--         , timeout = Nothing
--         , tracker = Nothing
--         }


type Config a
    = Config
        { method : String
        , token : Maybe String
        , body : Maybe Json.Encode.Value
        , url : List String
        , decoder : Json.Decode.Decoder a
        }


newV2 : { url : List String, decoder : Json.Decode.Decoder a } -> Config a
newV2 options =
    Config
        { method = "GET"
        , token = Nothing
        , body = Nothing
        , url = options.url
        , decoder = options.decoder
        }


withMethodPost : Config a -> Config a
withMethodPost (Config config) =
    Config { config | method = "POST" }


withMethodPatch : Config a -> Config a
withMethodPatch (Config config) =
    Config { config | method = "PATCH" }


withBody : Json.Encode.Value -> Config a -> Config a
withBody body (Config config) =
    Config { config | body = Just body }


withToken : String -> Config a -> Config a
withToken token (Config config) =
    Config { config | token = Just token }


sendRequest : Config a -> Task Error a
sendRequest (Config config) =
    Http.task
        { method = config.method
        , headers =
            case config.token of
                Nothing ->
                    []

                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token) ]
        , url = toUrl (baseUri :: config.url)
        , body =
            case config.body of
                Nothing ->
                    Http.emptyBody

                Just body ->
                    Http.jsonBody body
        , resolver =
            config.decoder
                |> jsonResolver2
                |> Http.stringResolver
        , timeout = Nothing
        }
