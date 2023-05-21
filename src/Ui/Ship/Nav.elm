module Ui.Ship.Nav exposing (..)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import SpaceTrader.Ship.Nav
import SpaceTrader.Ship.Nav.Status
import Ui
import Ui.Button
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
        --             { label = Html.text nav.system
        --             , onClick =
        --                 nav.system
        --                     |> opts.onSystemClicked
        --                     |> Just
        --             }
        --     }
        --     ++ Ui.viewLabeled
        --         { label = "Waypoint"
        --         , value = Html.text <| nav.waypoint
        --         }
        --     ++ [ Ui.Ship.Nav.Status.view opts
        --             nav.status
        --        ]
        -- )
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "System:" ]

        -- , Ui.Button.link [ Html.Attributes.style "width" "fit-content" ]
        --     { label = Html.text nav.system
        --     , onClick =
        --         nav.system
        --             |> opts.onSystemClicked
        --             |> Just
        --     }
        , Html.span [] [ Html.text nav.system ]
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Html.text "Waypoint:" ]
        , Html.span [] [ Html.text nav.waypoint ]
        , Html.div [ Html.Attributes.style "grid-column" "1 / 3" ]
            [ Ui.Ship.Nav.Status.view opts
                nav.status
            ]
        ]
