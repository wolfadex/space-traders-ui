module Ui.Ship.Nav exposing (view)

import Html exposing (Html)
import Html.Attributes
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship.Nav
import Ui
import Ui.Ship.Nav.Status


view :
    { onDock : msg
    , onOrbit : msg
    , onMove : msg
    , onSystemClicked : String -> msg
    }
    -> SpaceTrader.Ship.Nav.Nav
    -> Html msg
view opts nav =
    Html.div
        [ Ui.grid
        , Html.Attributes.style "grid-template-columns" "6rem 1fr"
        ]
        -- (Ui.viewLabeled
        --     { label = "System"
        --     , value =
        --         Ui.Button.link [ Html.Attributes.style "font-weight" "bold" ]
        --             { label = Ui.text nav.system
        --             , onClick =
        --                 nav.system
        --                     |> opts.onSystemClicked
        --                     |> Just
        --             }
        --     }
        --     ++ Ui.viewLabeled
        --         { label = "Waypoint"
        --         , value = Ui.text <| nav.waypoint
        --         }
        --     ++ [ Ui.Ship.Nav.Status.view opts
        --             nav.status
        --        ]
        -- )
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "System:" ]

        -- , Ui.Button.link [ Html.Attributes.style "width" "fit-content" ]
        --     { label = Ui.text nav.system
        --     , onClick =
        --         nav.system
        --             |> opts.onSystemClicked
        --             |> Just
        --     }
        , Html.span [] [ Ui.text (SpaceTrader.Point.System.toLabel nav.system) ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "Waypoint:" ]
        , Html.span [] [ Ui.text (SpaceTrader.Point.Waypoint.toShortLabel nav.waypoint) ]
        , Html.div [ Html.Attributes.style "grid-column" "1 / 3" ]
            [ Ui.Ship.Nav.Status.view opts
                nav.status
            ]
        ]
