module SpaceTrader.Api exposing (..)

import Http
import Json.Decode
import Json.Encode
import SpaceTrader.Agent exposing (Agent)
import SpaceTrader.Contract exposing (Contract)
import SpaceTrader.Faction exposing (Faction)
import SpaceTrader.Ship exposing (Ship)
import SpaceTrader.Waypoint exposing (Waypoint)


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
        (Json.Decode.field "user" SpaceTrader.Agent.decode)
        (Json.Decode.field "ship" SpaceTrader.Contract.decode)
        (Json.Decode.field "ship" SpaceTrader.Faction.decode)
        (Json.Decode.field "ship" SpaceTrader.Ship.decode)
        (Json.Decode.field "token" Json.Decode.string)


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
