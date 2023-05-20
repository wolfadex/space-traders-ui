port module Port exposing
    ( cacheSystems
    , clearToken
    , closeModal
    , openModal
    , setToken
    , storeSettings
    )

import Json.Encode


port setToken : String -> Cmd msg


clearToken : Cmd msg
clearToken =
    clearToken_ ()


port clearToken_ : () -> Cmd msg


port openModal : String -> Cmd msg


port closeModal : String -> Cmd msg


port storeSettings : Json.Encode.Value -> Cmd msg


port cacheSystems : Json.Encode.Value -> Cmd msg
