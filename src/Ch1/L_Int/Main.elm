module Ch1.L_Int.Main exposing (main)


import Browser
import Html as H
import Html.Attributes as HA


main : Program () Model msg
main =
    Browser.sandbox
        { init = init
        , update = update
        , view = view
        }


-- MODEL


type alias Model =
    {}


init : Model
init =
    {}


-- UPDATE


update : msg -> Model -> Model
update =
    always identity


-- VIEW


view : Model -> H.Html msg
view _ =
    H.div []
        [ H.textarea
            [ HA.rows 20
            , HA.cols 80
            , HA.placeholder "(+ (read) (- 8))"
            ]
            []
        , H.p [] [ H.button [] [ H.text "Run" ] ]
        , H.div
            [ HA.class "console" ]
            [ viewConsoleLine "12"
            , viewConsoleLine "01234567890123456789012345678901234567890123456789012345678901234567890123456789"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            , viewConsoleLine "12"
            ]
        ]

viewConsoleLine : String -> H.Html msg
viewConsoleLine text =
    H.div [ HA.class "console__line" ] [ H.text text ]
