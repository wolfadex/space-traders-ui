module Ui.Form exposing (Submission, view)

import Dict exposing (Dict)
import Form
import Html exposing (Html)
import Html.Attributes
import Ui


view :
    { submitting : Bool
    , title : String
    , model : Form.Model
    , toMsg : Form.Msg msg -> msg
    , serverSideErrors : Maybe (Dict String (List String))
    , onSubmit : Submission String parsed -> msg
    , id : String
    }
    -> Form.HtmlForm String parsed () msg
    -> Html msg
view options config =
    Ui.column
        [ Html.Attributes.style "border" "0.125rem solid "
        , Html.Attributes.style "border-radius" "0.5rem"
        , Html.Attributes.style "max-width" "50rem"
        , Html.Attributes.style "padding" "1rem"
        , Html.Attributes.style "background-color" "var(--blue)"
        , Ui.gap 1
        ]
        [ Ui.header.three
            [ Html.Attributes.style "border-bottom" "0.125rem solid "
            ]
            [ Ui.text options.title ]
        , Form.renderHtml
            { submitting = options.submitting
            , state = options.model
            , toMsg = options.toMsg
            }
            (Form.options options.id
                |> Form.withOnSubmit options.onSubmit
                |> Form.withServerResponse
                    (Maybe.map
                        (\serverSideErrors ->
                            { persisted =
                                { fields = Nothing
                                , clientSideErrors = Nothing
                                }
                            , serverSideErrors = serverSideErrors
                            }
                        )
                        options.serverSideErrors
                    )
            )
            [ Html.Attributes.style "display" "grid"
            , Html.Attributes.style "gap" "1rem"
            ]
            config
        ]


type alias Submission err a =
    { fields : List ( String, String )
    , method : Form.Method
    , action : String
    , parsed : Form.Validated err a
    }
