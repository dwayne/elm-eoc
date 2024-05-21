module Ch1.L_Int.Interpreter exposing
    ( Error(..)
    , Input
    , RuntimeError(..)
    , run
    )

import Ch1.L_Int.AST exposing (..)
import Ch1.L_Int.Parser as P


type alias Input =
    List Int


type Error
    = SyntaxError P.Error
    | RuntimeError RuntimeError


type RuntimeError
    = MissingInput


run : String -> Input -> Result Error Int
run source input =
    case P.parse source of
        Ok (Program expr) ->
            interpretExpr expr input
                |> Tuple.first
                |> Result.mapError RuntimeError

        Err err ->
            Err <| SyntaxError err


interpretExpr : Expr -> Input -> ( Result RuntimeError Int, Input )
interpretExpr expr input =
    case expr of
        Int n ->
            ( Ok n
            , input
            )

        Prim Read ->
            case input of
                [] ->
                    ( Err MissingInput
                    , input
                    )

                n :: restInput ->
                    ( Ok n
                    , restInput
                    )

        Prim (Negate aExpr) ->
            interpretExpr aExpr input
                |> Tuple.mapFirst (Result.map negate)

        Prim (Add aExpr bExpr) ->
            let
                ( aResult, input1 ) =
                    interpretExpr aExpr input
            in
            case aResult of
                Ok a ->
                    let
                        ( bResult, input2 ) =
                            interpretExpr bExpr input1
                    in
                    case bResult of
                        Ok b ->
                            ( Ok <| a + b
                            , input2
                            )

                        Err _ ->
                            ( bResult
                            , input2
                            )

                Err _ ->
                    ( aResult
                    , input1
                    )

        Prim (Sub aExpr bExpr) ->
            let
                ( aResult, input1 ) =
                    interpretExpr aExpr input
            in
            case aResult of
                Ok a ->
                    let
                        ( bResult, input2 ) =
                            interpretExpr bExpr input1
                    in
                    case bResult of
                        Ok b ->
                            ( Ok <| a - b
                            , input2
                            )

                        Err _ ->
                            ( bResult
                            , input2
                            )

                Err _ ->
                    ( aResult
                    , input1
                    )
