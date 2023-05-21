module Ui.Waypoint.Type exposing (view)

import SpaceTrader.Waypoint.Type exposing (Type(..))


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
