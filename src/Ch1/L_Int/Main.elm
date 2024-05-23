module Ch1.L_Int.Main exposing (main)

import Browser
import Browser.Dom as BD
import Ch1.L_Int.CPSInterpreter as I
import Html as H
import Html.Attributes as HA
import Html.Events as HE
import Task


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }



-- MODEL


type alias Model =
    { source : String
    , lines : List Line
    , maybeReadIntState : Maybe ReadIntState
    }


type Line
    = Success String
    | Error String


type alias ReadIntState =
    { value : String
    , cont : I.Continuation
    }


init : () -> ( Model, Cmd msg )
init _ =
    ( { source = "(+ (read) (- 8))"
      , lines = []
      , maybeReadIntState = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = InputSource String
    | ClickedRun
    | InputValue String
    | SubmittedValue
    | Focused


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InputSource source ->
            ( { model | source = source }
            , Cmd.none
            )

        ClickedRun ->
            case I.run model.source of
                Ok effect ->
                    case effect of
                        I.Value n ->
                            ( { model | lines = model.lines ++ [ Success <| String.fromInt n ] }
                            , Cmd.none
                            )

                        I.ReadInt readIntCont ->
                            ( { model | maybeReadIntState = Just { value = "", cont = readIntCont } }
                            , focus readInputId
                            )

                Err (I.SyntaxError _) ->
                    ( { model | lines = model.lines ++ [ Error "Syntax Error" ] }
                    , Cmd.none
                    )

        InputValue value ->
            case model.maybeReadIntState of
                Just readIntState ->
                    let
                        newReadIntState =
                            { readIntState | value = value }
                    in
                    ( { model | maybeReadIntState = Just newReadIntState }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        SubmittedValue ->
            case model.maybeReadIntState of
                Just { value, cont } ->
                    case String.toInt value of
                        Just m ->
                            let
                                valueLine =
                                    Success <| "> " ++ String.fromInt m
                            in
                            case I.resume m cont of
                                I.Value n ->
                                    ( { model
                                      | lines = model.lines ++ [ valueLine, Success <| String.fromInt n ]
                                      , maybeReadIntState = Nothing
                                      }
                                    , Cmd.none
                                    )

                                I.ReadInt readIntCont ->
                                    ( { model
                                      | lines = model.lines ++ [ valueLine ]
                                      , maybeReadIntState = Just { value = "", cont = readIntCont }
                                      }
                                    , focus readInputId
                                    )


                        Nothing ->
                            ( model, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        Focused ->
            ( model, Cmd.none )


readInputId : String
readInputId =
    "read-input"


focus : String -> Cmd Msg
focus =
    BD.focus >> Task.attempt (always Focused)


-- VIEW


view : Model -> H.Html Msg
view { source, lines, maybeReadIntState } =
    H.div []
        [ H.textarea
            [ HA.rows 20
            , HA.cols 80
            , HA.placeholder "(+ (read) (- 8))"
            , HA.value source
            , HE.onInput InputSource
            ]
            []
        , H.p []
            [ let
                buttonAttrs =
                    if maybeReadIntState == Nothing then
                        [ HE.onClick ClickedRun ]

                    else
                        [ HA.disabled True ]
              in
              H.button buttonAttrs [ H.text "Run" ]
            ]
        , let
            outputLineViews =
                List.map viewOutputLine lines

            inputLineView =
                case maybeReadIntState of
                    Nothing ->
                        []

                    Just { value } ->
                        [ H.form
                            [ HA.class "console__line console__line--input"
                            , HE.onSubmit SubmittedValue
                            ]
                            [ H.span [] [ H.text ">" ]
                            , H.input
                                [ HA.id readInputId
                                , HA.type_ "text"
                                , HA.value value
                                , HE.onInput InputValue
                                ]
                                []
                            ]
                        ]
          in
          H.div [ HA.class "console" ] ( outputLineViews ++ inputLineView )
        ]


viewOutputLine : Line -> H.Html msg
viewOutputLine line =
    case line of
        Success text ->
            H.div [ HA.class "console__line" ] [ H.text text ]

        Error text ->
            H.div [ HA.class "console__line console__line--error" ] [ H.text text ]
