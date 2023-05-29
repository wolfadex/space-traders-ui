module Generate exposing (main)

{-|

@docs main

-}

import Elm
import Elm.Annotation
import Elm.Case
import Gen.CodeGen.Generate as Generate
import Gen.Maybe
import GenericDict


{-| Generate an example custom dictionary for `comparable`.
-}
main : Program {} () ()
main =
    [ let
        keyType : Elm.Annotation.Annotation
        keyType =
            Elm.Annotation.namedWith [] "SpaceTrader.Point.System.System" []

        decls : List Elm.Declaration
        decls =
            GenericDict.init
                { keyType = keyType
                , toComparable =
                    \e ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "SpaceTrader", "Point", "System" ]
                                , name = "toKey"
                                , annotation = Just (Elm.Annotation.function [ keyType ] Elm.Annotation.string)
                                }
                            )
                            [ e ]
                , namespace = []
                }
                |> GenericDict.withTypeName "SystemDict"
                |> GenericDict.useElmFastDict
                |> GenericDict.generateDeclarations
      in
      Elm.file [ "SpaceTrader", "Point", "SystemDict" ] decls
    , let
        keyType : Elm.Annotation.Annotation
        keyType =
            Elm.Annotation.namedWith [] "SpaceTrader.Point.Waypoint.Waypoint" []

        decls : List Elm.Declaration
        decls =
            GenericDict.init
                { keyType = keyType
                , toComparable =
                    \e ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "SpaceTrader", "Point", "Waypoint" ]
                                , name = "toKey"
                                , annotation = Just (Elm.Annotation.function [ keyType ] Elm.Annotation.string)
                                }
                            )
                            [ e ]
                , namespace = []
                }
                |> GenericDict.withTypeName "WaypointDict"
                |> GenericDict.useElmFastDict
                |> GenericDict.generateDeclarations
      in
      Elm.file [ "SpaceTrader", "Point", "WaypointDict" ] decls
    , let
        keyType : Elm.Annotation.Annotation
        keyType =
            Elm.Annotation.namedWith [ "Id" ] "Id" [ Elm.Annotation.var "a" ]

        decls : List Elm.Declaration
        decls =
            GenericDict.init
                { keyType = keyType
                , toComparable =
                    \e ->
                        Elm.apply
                            (Elm.value
                                { importFrom = [ "Id" ]
                                , name = "toString"
                                , annotation = Just (Elm.Annotation.function [ keyType ] Elm.Annotation.string)
                                }
                            )
                            [ e ]
                , namespace = []
                }
                |> GenericDict.withTypeName "IdDict"
                |> GenericDict.useElmFastDict
                |> GenericDict.generateDeclarations
      in
      Elm.file [ "Id", "Dict" ] decls
    ]
        |> Generate.run
