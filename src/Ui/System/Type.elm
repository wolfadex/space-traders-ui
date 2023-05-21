module Ui.System.Type exposing (view)

import SpaceTrader.System.Type exposing (Type(..))


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
