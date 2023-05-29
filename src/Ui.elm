module Ui exposing
    ( none, text, header
    , progress, dateTime
    , link, externalLink, navLink
    , grid, gap, align, justify
    , column, row
    )

{-|


## Common

@docs none, text, header


## Special

@docs progress, dateTime


## Links

@docs link, externalLink, navLink


## Layout

@docs grid, gap, align, justify


## TODO

@docs column, row

-}

import Html exposing (Html)
import Html.Attributes
import Route exposing (Route)
import Time


column : List (Html.Attribute msg) -> List (Html msg) -> Html msg
column attrs =
    Html.div
        (attrs
            ++ [ Html.Attributes.style "display" "grid"
               , Html.Attributes.style "align-items" "start"
               ]
        )


row : List (Html.Attribute msg) -> List (Html msg) -> Html msg
row attrs =
    Html.div
        (attrs
            ++ [ Html.Attributes.style "display" "grid"
               , Html.Attributes.style "grid-auto-flow" "column"
               ]
        )


grid : Html.Attribute msg
grid =
    Html.Attributes.style "display" "grid"



-- width : { fill : Html.Attribute msg }
-- width =
--     { fill = Html.Attributes.style "width" "100%"
--     }


justify : { center : Html.Attribute msg, start : Html.Attribute msg, end : Html.Attribute msg }
justify =
    { center = Html.Attributes.style "justify-self" "center"
    , start = Html.Attributes.style "justify-self" "start"
    , end = Html.Attributes.style "justify-self" "end"
    }


align : { center : Html.Attribute msg, start : Html.Attribute msg, end : Html.Attribute msg }
align =
    { center = Html.Attributes.style "align-self" "center"
    , start = Html.Attributes.style "align-self" "start"
    , end = Html.Attributes.style "align-self" "end"
    }


gap : Float -> Html.Attribute msg
gap f =
    Html.Attributes.style "gap" (String.fromFloat f ++ "rem")


dateTime : Time.Zone -> Time.Posix -> String
dateTime timeZone time =
    let
        year =
            time
                |> Time.toYear timeZone
                |> String.fromInt

        month =
            time
                |> Time.toMonth timeZone
                |> monthToString

        day =
            time
                |> Time.toDay timeZone
                |> String.fromInt

        hour =
            time
                |> Time.toHour timeZone
                |> String.fromInt

        minute =
            time
                |> Time.toMinute timeZone
                |> String.fromInt
                |> String.padLeft 2 '0'

        second =
            time
                |> Time.toSecond timeZone
                |> String.fromInt
                |> String.padLeft 2 '0'
    in
    year ++ " " ++ month ++ " " ++ day ++ " @ " ++ hour ++ ":" ++ minute ++ ":" ++ second


monthToString : Time.Month -> String
monthToString month =
    case month of
        Time.Jan ->
            "January"

        Time.Feb ->
            "February"

        Time.Mar ->
            "March"

        Time.Apr ->
            "April"

        Time.May ->
            "May"

        Time.Jun ->
            "June"

        Time.Jul ->
            "July"

        Time.Aug ->
            "August"

        Time.Sep ->
            "September"

        Time.Oct ->
            "October"

        Time.Nov ->
            "November"

        Time.Dec ->
            "December"


progress : List (Html.Attribute msg) -> { max : Float, current : Float } -> Html msg
progress attr opt =
    Html.div
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "1.5rem"
        , Html.Attributes.style "border" "0.125rem solid"
        ]
        [ Html.div
            [ Html.Attributes.style "width" (String.fromFloat (opt.current / opt.max * 100) ++ "%")
            , Html.Attributes.style "height" "100%"
            , Html.Attributes.style "background-color" "var(--blue-dark)"
            , Html.Attributes.style "transition" "width 0.5s ease-in-out"
            ]
            []
        ]


link : List (Html.Attribute msg) -> { label : Html msg, route : Route } -> Html msg
link attr opts =
    Html.a
        ([ Html.Attributes.href (Route.toUrlString opts.route)
         , Html.Attributes.style "color" "var(--blue-light)"
         ]
            ++ attr
        )
        [ opts.label ]


externalLink : List (Html.Attribute msg) -> { label : Html msg, link : String } -> Html msg
externalLink attr opts =
    Html.a
        ([ Html.Attributes.href opts.link
         , Html.Attributes.style "color" "var(--blue-light)"
         , Html.Attributes.target "_blank"
         , Html.Attributes.rel "noopener noreferrer"
         ]
            ++ attr
        )
        [ opts.label ]


{-| A specially styled link for internal links in the left nav
-}
navLink : { label : String, route : Route } -> Bool -> Html msg
navLink opts focused =
    Html.a
        [ Html.Attributes.classList
            [ ( "nav-button", True )
            , ( "nav-button-focused", focused )
            ]
        , Html.Attributes.href (Route.toUrlString opts.route)
        , grid
        ]
        [ Html.div [ Html.Attributes.class "nav-button-top", justify.end ] []
        , Html.div
            [ Html.Attributes.class "nav-button-middle"
            , grid
            ]
            [ Html.span
                [ Html.Attributes.style "font-size" "1rem" ]
                [ text opts.label ]
            ]
        , Html.div [ Html.Attributes.class "nav-button-bottom", justify.end ] []
        ]


none : Html msg
none =
    Html.text ""


{-| If you want to show nothing, use `Ui.none` instead.
-}
text : String -> Html msg
text =
    Html.text


header :
    { one : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , two : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , three : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    , four : List (Html.Attribute msg) -> List (Html msg) -> Html msg
    }
header =
    { one = \attr -> Html.h1 (Html.Attributes.style "margin" "0" :: attr)
    , two = \attr -> Html.h2 (Html.Attributes.style "margin" "0" :: attr)
    , three = \attr -> Html.h3 (Html.Attributes.style "margin" "0" :: attr)
    , four = \attr -> Html.h4 (Html.Attributes.style "margin" "0" :: attr)
    }
