module Ui.Ship exposing
    ( CargoSellForm
    , TransitForm
    , view
    , viewBrief
    )

import Form
import Form.Field
import Form.Validation
import Html exposing (Html)
import Html.Attributes
import Id
import Route
import SpaceTrader.Id exposing (ShipId)
import SpaceTrader.Point.System
import SpaceTrader.Point.Waypoint
import SpaceTrader.Ship
import SpaceTrader.Ship.Cargo.Item
import SpaceTrader.Ship.Nav.FlightMode
import SpaceTrader.Ship.Nav.Status
import Time
import Time.Distance
import Ui
import Ui.Button
import Ui.Form
import Ui.Form.Field
import Ui.Ship.Cargo
import Util.Time


viewBrief : SpaceTrader.Ship.Ship -> Html msg
viewBrief ship =
    Html.div
        [ Html.Attributes.style "border" "0.125rem solid"
        , Html.Attributes.style "border-radius" "0.25rem"
        , Html.Attributes.style "padding" "0.5rem"
        , Ui.grid
        , Ui.gap 0.5
        , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "ID:" ]
        , Ui.link []
            { label = Ui.text (Id.toLabel ship.id)
            , route = Route.fromShip ship.id
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "Waypoint:" ]
        , Html.span []
            [ Ui.link []
                { label =
                    ship.nav.system
                        |> SpaceTrader.Point.System.toLabel
                        |> Ui.text
                , route = Route.fromSystem ship.nav.system
                }
            , Ui.text "-"
            , Ui.link []
                { label =
                    ship.nav.waypoint
                        |> SpaceTrader.Point.Waypoint.toShortLabel
                        |> Ui.text
                , route = Route.fromWaypoint ship.nav.waypoint
                }
            ]
        , Html.span
            [ Html.Attributes.style "font-weight" "bold"
            ]
            [ Ui.text "Fuel:" ]
        , Ui.progress []
            { max = toFloat ship.fuel.capacity
            , current = toFloat ship.fuel.current
            }
        , Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text "Cargo:" ]
        , Html.span [] [ Ui.Ship.Cargo.viewBrief ship.cargo ]
        ]


view :
    { onDock : ShipId -> msg
    , onOrbit : ShipId -> msg
    , onMove : ShipId -> Ui.Form.Submission String TransitForm -> msg
    , onExtract : ShipId -> msg
    , onRefresh : ShipId -> msg
    , onFlightModeSelected : ShipId -> SpaceTrader.Ship.Nav.FlightMode.FlightMode -> msg
    , onRefreshCooldown : ShipId -> msg
    , currentTime : Time.Posix
    , transitForm : Form.Model
    , onTransitFormMsg : ShipId -> Form.Msg msg -> msg
    , transitableWaypoints : List SpaceTrader.Point.Waypoint.Waypoint
    , cargoSellForm : Form.Model
    , onCargoSellFormMsg : ShipId -> Form.Msg msg -> msg
    , onCargoSell : ShipId -> Ui.Form.Submission String CargoSellForm -> msg
    }
    -> SpaceTrader.Ship.Ship
    -> Html msg
view opts ship =
    Html.div
        [ Ui.gap 0.5
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "flex-direction" "column"
        , Html.Attributes.style "max-width" "40rem"
        ]
        [ Html.div
            [ Ui.grid
            , Ui.gap 1
            , Html.Attributes.style "grid-template-columns" "5.5rem 1fr"
            ]
            [ Ui.Button.small
                [ Html.Attributes.style "padding" "0 0.5rem"
                ]
                { label = Ui.text "Refresh"
                , onClick = Just (opts.onRefresh ship.id)
                }
            ]
        , labeled "Fuel" <|
            Ui.progress [ Html.Attributes.style "max-width" "20rem" ]
                { max = toFloat ship.fuel.capacity
                , current = toFloat ship.fuel.current
                }
        , labeled "Waypoint" <|
            Html.span []
                [ Ui.link []
                    { label =
                        ship.nav.system
                            |> SpaceTrader.Point.System.toLabel
                            |> Ui.text
                    , route = Route.fromSystem ship.nav.system
                    }
                , Ui.text "-"
                , Ui.link []
                    { label =
                        ship.nav.waypoint
                            |> SpaceTrader.Point.Waypoint.toShortLabel
                            |> Ui.text
                    , route = Route.fromWaypoint ship.nav.waypoint
                    }
                ]
        , ship.nav.status
            |> SpaceTrader.Ship.Nav.Status.prettyPrint
            |> Ui.text
            |> labeled "Status"
        , SpaceTrader.Ship.Nav.FlightMode.allModes
            |> List.map
                (\mode ->
                    { label =
                        mode
                            |> SpaceTrader.Ship.Nav.FlightMode.toLabel
                            |> Ui.text
                    , onClick = opts.onFlightModeSelected ship.id mode
                    , selected = ship.nav.flightMode == mode
                    }
                )
            |> Ui.Button.multi []
            |> labeled "Flight mode"
        , labeled "Actions" <|
            Html.div [ Ui.grid, Ui.gap 0.5, Html.Attributes.style "max-width" "20rem" ]
                (case ship.nav.status of
                    SpaceTrader.Ship.Nav.Status.InOrbit ->
                        [ Ui.Button.default []
                            { label = Ui.text "Dock"
                            , onClick = Just (opts.onDock ship.id)
                            }
                        , Ui.Button.default []
                            { label =
                                Ui.text <|
                                    case ship.cooldown of
                                        Just cooldown ->
                                            if cooldown.expiration |> Util.Time.isAfter opts.currentTime then
                                                "Cooldown complete " ++ Time.Distance.inWords cooldown.expiration opts.currentTime ++ "..."

                                            else
                                                "Extract"

                                        Nothing ->
                                            "Extract"
                            , onClick =
                                case ship.cooldown of
                                    Just cooldown ->
                                        if cooldown.expiration |> Util.Time.isAfter opts.currentTime then
                                            Nothing

                                        else
                                            Just <| opts.onExtract ship.id

                                    Nothing ->
                                        Just <| opts.onExtract ship.id
                            }
                        , Form.renderHtml
                            { submitting = False
                            , state = opts.transitForm
                            , toMsg = opts.onTransitFormMsg ship.id
                            }
                            (Form.options ("ship-transit-form-" ++ Id.toString ship.id)
                                |> Form.withOnSubmit (opts.onMove ship.id)
                            )
                            [ Ui.grid
                            , Ui.gap 1
                            ]
                            (transitForm opts.transitableWaypoints)
                        ]

                    SpaceTrader.Ship.Nav.Status.InTransit ->
                        [ Ui.text
                            ("Arriving at "
                                ++ SpaceTrader.Point.Waypoint.toShortLabel ship.nav.route.destination.symbol
                                ++ " in "
                                ++ Time.Distance.inWords ship.nav.route.arrival opts.currentTime
                            )
                        ]

                    SpaceTrader.Ship.Nav.Status.Docked ->
                        [ Ui.Button.default []
                            { label = Ui.text "Orbit"
                            , onClick = Just (opts.onOrbit ship.id)
                            }
                        ]
                )
        , labeled "Cargo" <|
            Html.div [ Ui.grid, Ui.gap 1 ]
                [ Ui.Ship.Cargo.view ship.cargo
                , Html.div
                    [ Html.Attributes.style "width" "100%"
                    , Html.Attributes.style "border-bottom" "1px solid"
                    ]
                    []
                , Form.renderHtml
                    { submitting = False
                    , state = opts.cargoSellForm
                    , toMsg = opts.onCargoSellFormMsg ship.id
                    }
                    (Form.options ("ship-cargo-sell-form-" ++ Id.toString ship.id)
                        |> Form.withOnSubmit (opts.onCargoSell ship.id)
                    )
                    [ Ui.grid
                    , Ui.gap 1
                    ]
                    (cargoSellForm ship.cargo.inventory)
                ]
        ]


labeled : String -> Html msg -> Html msg
labeled label content =
    Html.div
        [ Ui.grid
        , Ui.gap 1
        , Html.Attributes.style "grid-template-columns" "6.5rem 1fr"
        ]
        [ Html.span [ Html.Attributes.style "font-weight" "bold" ] [ Ui.text (label ++ ":") ]
        , content
        ]


type alias TransitForm =
    { destination : SpaceTrader.Point.Waypoint.Waypoint
    }


transitForm : List SpaceTrader.Point.Waypoint.Waypoint -> Form.HtmlForm String TransitForm input msg
transitForm transitableWaypoints =
    (\destination ->
        { combine =
            Form.Validation.succeed TransitForm
                |> Form.Validation.andMap destination
        , view =
            \formState ->
                List.concat
                    [ Ui.Form.Field.select { toString = SpaceTrader.Point.Waypoint.toShortLabel }
                        formState
                        "Travel to"
                        destination
                    , Ui.Form.Field.submit
                        { label =
                            if formState.submitting then
                                "In transit..."

                            else
                                "Travel"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "destination"
            (Form.Field.select
                (List.map (\waypoint -> ( SpaceTrader.Point.Waypoint.toKey waypoint, waypoint ))
                    transitableWaypoints
                )
                (\_ -> "Invalid")
                |> Form.Field.required "Select a destination"
            )


type alias CargoSellForm =
    { item : SpaceTrader.Ship.Cargo.Item.Item
    , quantity : Int
    }


cargoSellForm : List SpaceTrader.Ship.Cargo.Item.Item -> Form.HtmlForm String CargoSellForm input msg
cargoSellForm inventory =
    (\item quantity ->
        { combine =
            Form.Validation.succeed CargoSellForm
                |> Form.Validation.andMap item
                |> Form.Validation.andMap
                    (Form.Validation.map2
                        (\itm quantityValue ->
                            let
                                maxQuantity : Int
                                maxQuantity =
                                    inventory
                                        |> List.filter (\i -> i.symbol == itm.symbol)
                                        |> List.head
                                        |> Maybe.map .units
                                        |> Maybe.withDefault 0
                            in
                            Form.Validation.succeed quantityValue
                                |> Form.Validation.withErrorIf (quantityValue < 1) quantity "Must be greather than 0"
                                |> Form.Validation.withErrorIf (quantityValue > maxQuantity)
                                    quantity
                                    ("Can't sell more than " ++ String.fromInt maxQuantity ++ " " ++ itm.name)
                        )
                        item
                        quantity
                        |> Form.Validation.andThen identity
                    )
        , view =
            \formState ->
                List.concat
                    [ Ui.Form.Field.select { toString = SpaceTrader.Ship.Cargo.Item.toLabel }
                        formState
                        "Item"
                        item
                    , Ui.Form.Field.text
                        { formState = formState
                        , label = "Quantity"
                        , field = quantity
                        , hint = Nothing
                        }
                    , Ui.Form.Field.submit
                        { label =
                            if formState.submitting then
                                "Selling..."

                            else
                                "Sell"
                        , disabled = formState.submitting
                        }
                    ]
        }
    )
        |> Form.form
        |> Form.field "item"
            (Form.Field.select
                (List.map (\item -> ( item.symbol, item ))
                    inventory
                )
                (\_ -> "Invalid")
                |> Form.Field.required "Select an item to sell"
            )
        |> Form.field "quantity"
            (Form.Field.int
                { invalid =
                    \_ -> "Must be an integer greater than 0"
                }
                |> Form.Field.required "Specify a quantity to sell"
            )
