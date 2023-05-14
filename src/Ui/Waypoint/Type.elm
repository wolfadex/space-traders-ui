module Ui.Waypoint.Type exposing (..)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Waypoint.Type exposing (Type(..))
import Ui


view : SpaceTrader.Waypoint.Type.Type -> String
view type_ =
    case type_ of
        Planet ->
            "Planet"

        GasGiant ->
            "Gas Giant"

        Moon ->
            "Moon"

        OrbitalStation ->
            "Orbital Station"

        JumpGate ->
            "Jump Gate"

        AsteroidField ->
            "Asteroid Field"

        Nebula ->
            "Nebula"

        DebrisField ->
            "Debris Field"

        GravityWell ->
            "Gravity Well"
