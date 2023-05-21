module SpaceTrader.Ship.Cargo exposing (Cargo, decode)

import Json.Decode
import SpaceTrader.Ship.Cargo.Item exposing (Item)


type alias Cargo =
    { capcity : Int
    , units : Int
    , inventory : List Item
    }


decode : Json.Decode.Decoder Cargo
decode =
    Json.Decode.map3 Cargo
        (Json.Decode.field "capacity" Json.Decode.int)
        (Json.Decode.field "units" Json.Decode.int)
        (Json.Decode.field "inventory" (Json.Decode.list SpaceTrader.Ship.Cargo.Item.decode))
