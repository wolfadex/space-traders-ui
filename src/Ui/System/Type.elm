module Ui.System.Type exposing (..)

import Html exposing (Html)
import SpaceTrader.System.Type exposing (Type(..))
import Ui


view : SpaceTrader.System.Type.Type -> String
view type_ =
    case type_ of
        NeutronStar ->
            "Neutron Star"

        RedStar ->
            "Red Star"

        OrangeStar ->
            "Orange Star"

        BlueStar ->
            "Blue Star"

        YoungStar ->
            "Young Star"

        WhiteDwarf ->
            "White Dwarf"

        BlackHole ->
            "Black Hole"

        Hypergiant ->
            "Hypergiant"

        Nebula ->
            "Nebula"

        Unstable ->
            "Unstable"
