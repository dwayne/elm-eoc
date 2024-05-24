module Ch2.L_Var.Interpreter exposing
    ( Error(..)
    , Input
    , RuntimeError(..)
    , run
    , runProgram
    )

import Ch2.L_Var.AST as AST exposing (..)
import Ch2.L_Var.Parser as P


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
        Ok program ->
            runProgram program input
                |> Result.mapError RuntimeError

        Err err ->
            Err <| SyntaxError err


runProgram : AST.Program -> Input -> Result RuntimeError Int
runProgram (Program expr) =
    interpretExpr expr >> Tuple.first


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

        Var _ ->
            Debug.todo "Implement Var"

        Let _ _ _ ->
            Debug.todo "Implement Let"
