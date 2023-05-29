module SpaceTrader.Id exposing
    ( ContractId
    , ContractIdKey
    , ShipId
    , ShipIdKey
    )

import Id exposing (Id)


type alias ShipId =
    Id ShipIdKey


type ShipIdKey
    = ShipIdKey Never


type alias ContractId =
    Id ContractIdKey


type ContractIdKey
    = ContractIdKey Never
