module SpaceTrader.Api exposing (..)

import Http
import Json.Decode
import Json.Encode
import Process
import SpaceTrader.Agent exposing (Agent)
import SpaceTrader.Contract exposing (Contract)
import SpaceTrader.Faction exposing (Faction)
import SpaceTrader.Ship exposing (Ship)
import SpaceTrader.Ship.Nav
import SpaceTrader.System
import SpaceTrader.Waypoint exposing (Waypoint)
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


myContracts : (Result Http.Error (List SpaceTrader.Contract.Contract) -> msg) -> { token : String } -> Cmd msg
myContracts toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "my", "contracts" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess (Json.Decode.list SpaceTrader.Contract.decode))
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


moveToOrbit : (Result Http.Error SpaceTrader.Ship.Nav.Nav -> msg) -> { token : String, shipId : String } -> Cmd msg
moveToOrbit toMsg options =
    v2
        { method = "POST"
        , token = options.token
        , url = [ "my", "ships", options.shipId, "orbit" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess (Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode))
        }


dockShip : (Result Http.Error SpaceTrader.Ship.Nav.Nav -> msg) -> { token : String, shipId : String } -> Cmd msg
dockShip toMsg options =
    v2
        { method = "POST"
        , token = options.token
        , url = [ "my", "ships", options.shipId, "dock" ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess (Json.Decode.field "nav" SpaceTrader.Ship.Nav.decode))
        }



-- GENERAL


listSystems : (Result Http.Error ( List SpaceTrader.System.System, PagedMeta ) -> msg) -> { token : String, page : Int } -> Cmd msg
listSystems toMsg options =
    v2Paged
        { method = "GET"
        , token = options.token
        , url = [ "systems" ]
        , body = Http.emptyBody
        , page = options.page
        , expect =
            Http.expectJson toMsg <|
                Json.Decode.map2 Tuple.pair
                    (decodeSuccess (Json.Decode.list SpaceTrader.System.decode))
                    decodePagedMeta
        }


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


getSystem : (Result Http.Error SpaceTrader.System.System -> msg) -> { token : String, systemId : String } -> Cmd msg
getSystem toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "systems", options.systemId ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess SpaceTrader.System.decode)
        }


getWaypoint : (Result Http.Error SpaceTrader.Waypoint.Waypoint -> msg) -> { token : String, systemId : String, waypointId : String } -> Cmd msg
getWaypoint toMsg options =
    v2
        { method = "GET"
        , token = options.token
        , url = [ "systems", options.systemId, "waypoints", options.waypointId ]
        , body = Http.emptyBody
        , expect = Http.expectJson toMsg (decodeSuccess SpaceTrader.Waypoint.decode)
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

        Http.GoodStatus_ metadata body ->
            case Json.Decode.decodeString decode body of
                Ok value ->
                    Ok value

                Err err ->
                    Err (Http.BadBody (Json.Decode.errorToString err))


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
