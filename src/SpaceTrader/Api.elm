module SpaceTrader.Api exposing (Error(..), Msg(..), PagedMeta, createSurvey, dockShip, getAllSystemsInit, getAllSystemsUpdate, getSystem, getWaypoint, moveToOrbit, myAgent, myContracts, myShips, register)

import Http
import Json.Decode
import Json.Encode
import Process
import SpaceTrader.Agent exposing (Agent)
import SpaceTrader.Contract exposing (Contract)
import SpaceTrader.Faction exposing (Faction)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship exposing (Ship)
import SpaceTrader.Ship.Cooldown
import SpaceTrader.Ship.Nav
import SpaceTrader.Survey
import SpaceTrader.System
import SpaceTrader.Waypoint
import Task exposing (Task)



-- AUTH


register :
    (Result
        Http.Error
        { agent : Agent
        , contract : Contract
        , faction : Faction
        , ship : Ship
        , token : String
        }
     -> msg
    )
    -> { callsign : String, faction : SpaceTrader.Faction.Group }
    -> Cmd msg
register toMsg { callsign, faction } =
    Http.post
        { url = toUrl [ baseUri, "register" ]
        , body =
            Http.jsonBody
                (Json.Encode.object
                    [ ( "symbol", Json.Encode.string callsign )
                    , ( "faction"
                      , faction
                            |> SpaceTrader.Faction.groupToString
                            |> Json.Encode.string
                      )
                    ]
                )
        , expect = Http.expectJson toMsg (decodeSuccess decodeRegister)
        }


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


myAgent : (Result Http.Error SpaceTrader.Agent.Agent -> msg) -> { token : String } -> Cmd msg
myAgent toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "my", "agent" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess SpaceTrader.Agent.decode)
        }


myContracts : { token : String } -> Task Error (List SpaceTrader.Contract.Contract)
myContracts options =
    v2_3
        { method = "GET"
        , token = options.token
        , url = [ "my", "contracts" ]
        , body = Http.emptyBody
        , decoder = Json.Decode.list SpaceTrader.Contract.decode
        }


myShips : (Result Http.Error (List SpaceTrader.Ship.Ship) -> msg) -> { token : String } -> Cmd msg
myShips toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "my", "ships" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess (Json.Decode.list SpaceTrader.Ship.decode))
        }


moveToOrbit : { token : String, shipId : String } -> Task Error SpaceTrader.Ship.Nav.Nav
moveToOrbit options =
    v2_3
        { method = "POST"
        , token = options.token
        , url = [ "my", "ships", options.shipId, "orbit" ]
        , body = Http.emptyBody
        , decoder = Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode
        }


dockShip : { token : String, shipId : String } -> Task Error SpaceTrader.Ship.Nav.Nav
dockShip options =
    v2_3
        { method = "POST"
        , token = options.token
        , url = [ "my", "ships", options.shipId, "dock" ]
        , body = Http.emptyBody
        , decoder = Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode
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


getSystem : (Result Http.Error SpaceTrader.System.System -> msg) -> { token : String, systemId : SpaceTrader.Point.System.System } -> Cmd msg
getSystem toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "systems", SpaceTrader.Point.System.toKey options.systemId ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess SpaceTrader.System.decode)
        }


getWaypoint : { token : String, waypointId : SpaceTrader.Point.Waypoint.Waypoint } -> Task Error SpaceTrader.Waypoint.Waypoint
getWaypoint options =
    v2_3
        { method = "GET"
        , token = options.token
        , url =
            [ "systems"
            , options.waypointId
                |> SpaceTrader.Point.Waypoint.toSystem
                |> SpaceTrader.Point.System.toKey
            , "waypoints"
            , SpaceTrader.Point.Waypoint.toKey options.waypointId
            ]
        , body = Http.emptyBody
        , decoder = SpaceTrader.Waypoint.decode
        }


createSurvey :
    { token : String, shipId : String }
    -- -> (Result Error ( List SpaceTrader.Survey.Survey, SpaceTrader.Ship.Cooldown.Cooldown ) -> msg)
    -> Task Error ( List SpaceTrader.Survey.Survey, SpaceTrader.Ship.Cooldown.Cooldown )
createSurvey options =
    v2_3
        { method = "POST"
        , token = options.token
        , url = [ "my", "ships", options.shipId, "survey" ]
        , body = Http.emptyBody
        , decoder =
            Json.Decode.map2 Tuple.pair
                (Json.Decode.field "surveys" (Json.Decode.list SpaceTrader.Survey.decode))
                (Json.Decode.field "cooldown" SpaceTrader.Ship.Cooldown.decode)
        }



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


v2 : { method : String, token : String, url : List String, body : Http.Body, expect : Http.Expect msg } -> Cmd msg
v2 options =
    Http.request
        { method = options.method
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ options.token)
            ]
        , url = toUrl (baseUri :: options.url)
        , body = options.body
        , expect = options.expect
        , timeout = Nothing
        , tracker = Nothing
        }


v2_3 :
    { method : String
    , token : String
    , body : Http.Body
    , url : List String
    , decoder : Json.Decode.Decoder a
    }
    -> Task Error a
v2_3 opts =
    Http.task
        { method = opts.method
        , headers = [ Http.header "Authorization" ("Bearer " ++ opts.token) ]
        , url = toUrl (baseUri :: opts.url)
        , body = opts.body
        , resolver =
            Http.stringResolver <|
                jsonResolver2 <|
                    opts.decoder
        , timeout = Nothing
        }


v2Paged : { method : String, token : String, url : List String, body : Http.Body, expect : Http.Expect msg, page : Int } -> Cmd msg
v2Paged options =
    Http.request
        { method = options.method
        , headers =
            [ Http.header "Authorization" ("Bearer " ++ options.token)
            ]
        , url = toUrl (baseUri :: options.url) ++ "?limit=20&page=" ++ String.fromInt options.page
        , body = options.body
        , expect = options.expect
        , timeout = Nothing
        , tracker = Nothing
        }
