module Ch1.L_Int.Main exposing (main)


import Browser
import Html as H


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
    H.text "Hello, world!"
