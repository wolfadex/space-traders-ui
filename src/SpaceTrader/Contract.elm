module SpaceTrader.Contract exposing (Contract, Type(..), decode, prettyPrintType)

import Iso8601
import Json.Decode
import SpaceTrader.Contract.Term exposing (Term)
import SpaceTrader.Faction
import Time


type alias Contract =
    { id : String
    , factionGroup : SpaceTrader.Faction.Group
    , type_ : Type
    , terms : Term
    , accepted : Bool
    , fulfilled : Bool
    , expiration : Time.Posix
    }


decode : Json.Decode.Decoder Contract
decode =
    Json.Decode.map7 Contract
        (Json.Decode.field "id" Json.Decode.string)
        (Json.Decode.field "factionSymbol" SpaceTrader.Faction.decodeGroup)
        (Json.Decode.field "type" decodeType)
        (Json.Decode.field "terms" SpaceTrader.Contract.Term.decode)
        (Json.Decode.field "accepted" Json.Decode.bool)
        (Json.Decode.field "fulfilled" Json.Decode.bool)
        (Json.Decode.field "expiration" Iso8601.decoder)


type Type
    = Porcurement
    | Transport
    | Shuttle


decodeType : Json.Decode.Decoder Type
decodeType =
    Json.Decode.string
        |> Json.Decode.andThen
            (\str ->
                case str of
                    "PROCUREMENT" ->
                        Json.Decode.succeed Porcurement

                    "TRANSPORT" ->
                        Json.Decode.succeed Transport

                    "SHUTTLE" ->
                        Json.Decode.succeed Shuttle

                    _ ->
                        Json.Decode.fail "Invalid contract type"
            )


prettyPrintType : Type -> String
prettyPrintType type_ =
    case type_ of
        Porcurement ->
            "Procurement"

        Transport ->
            "Transport"

        Shuttle ->
            "Shuttle"
