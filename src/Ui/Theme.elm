module Ui.Theme exposing (..)

import Json.Decode
import Json.Encode
import List.NonEmpty


type alias Theme =
    { primary : String
    , secondary : String
    , label : String
    , class : String
    }


decode : Json.Decode.Decoder Theme
decode =
    Json.Decode.map4 Theme
        (Json.Decode.field "primary" Json.Decode.string)
        (Json.Decode.field "secondary" Json.Decode.string)
        (Json.Decode.field "label" Json.Decode.string)
        (Json.Decode.field "class" Json.Decode.string)


encode : Theme -> Json.Encode.Value
encode theme =
    Json.Encode.object
        [ ( "primary", Json.Encode.string theme.primary )
        , ( "secondary", Json.Encode.string theme.secondary )
        , ( "label", Json.Encode.string theme.label )
        , ( "class", Json.Encode.string theme.class )
        ]


themes : List.NonEmpty.NonEmpty Theme
themes =
    List.NonEmpty.init
        { primary = "teal", secondary = "#fff9e1", label = "Theme 1", class = "theme-1" }
        [ { primary = "cyan", secondary = "#9e4c34", label = "Theme 2", class = "theme-2" }
        , { primary = "#0f5166", secondary = "skyblue", label = "Theme 3", class = "theme-3" }
        , { primary = "indigo", secondary = "#c4ccbb", label = "Theme 4", class = "theme-4" }
        , { primary = "orangered", secondary = "#480e12", label = "Theme 5", class = "theme-5" }
        , { primary = "orange", secondary = "#4e1f2b", label = "Theme 6", class = "theme-6" }
        , { primary = "orange", secondary = "#1c4c60", label = "Theme 7", class = "theme-7" }
        , { primary = "teal", secondary = "#e7ffe7", label = "Theme 8", class = "theme-8" }
        , { primary = "cyan", secondary = "#615564", label = "Theme 9", class = "theme-9" }
        , { primary = "#615564", secondary = "cyan", label = "Theme 10", class = "theme-10" }
        ]



-- .theme-1 {
-- color: teal;
-- background-color: #fff9e1;
-- }
-- .theme-2 {
-- color: cyan;
-- background-color: #9e4c34;
-- }
-- .theme-3 {
-- color: #0f5166;
-- background-color: skyblue;
-- }
-- .theme-4 {
-- color: indigo;
-- background-color: #c4ccbb;
-- }
-- .theme-5 {
-- color: orangered;
-- background-color: #480e12;
-- }
-- .theme-6 {
-- color: orange;
-- background-color: #4e1f2b;
-- }
-- .theme-7 {
-- color: orange;
-- background-color: #1c4c60;
-- }
-- .theme-8 {
-- color: teal;
-- background-color: #e7ffe7;
-- }
-- .theme-9 {
-- color: cyan;
-- background-color: #615564;
-- }
-- .theme-10 {
-- color: #615564;
-- background-color: cyan;
-- }
