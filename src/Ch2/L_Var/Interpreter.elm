module Ch2.L_Var.Interpreter exposing
    ( Error(..)
    , Input
    , RuntimeError(..)
    , run
    , runProgram
    )

import Ch2.L_Var.AST as AST exposing (..)
import Ch2.L_Var.Env as Env
import Ch2.L_Var.Parser as P


type alias Input =
    List Int


type alias Env =
    Env.Env Id Int


type Error
    = SyntaxError P.Error
    | RuntimeError RuntimeError


type RuntimeError
    = MissingInput
    | IdentifierNotFound Id


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
    interpretExpr expr Env.empty >> Tuple.first


interpretExpr : Expr -> Env -> Input -> ( Result RuntimeError Int, Input )
interpretExpr expr env input =
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
            interpretExpr aExpr env input
                |> Tuple.mapFirst (Result.map negate)

        Prim (Add aExpr bExpr) ->
            let
                ( aResult, input1 ) =
                    interpretExpr aExpr env input
            in
            case aResult of
                Ok a ->
                    let
                        ( bResult, input2 ) =
                            interpretExpr bExpr env input1
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
                    interpretExpr aExpr env input
            in
            case aResult of
                Ok a ->
                    let
                        ( bResult, input2 ) =
                            interpretExpr bExpr env input1
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

        Var id ->
            case Env.find id env of
                Just n ->
                    ( Ok n
                    , input
                    )

                Nothing ->
                    ( Err <| IdentifierNotFound id
                    , input
                    )

        Let id e body ->
            let
                ( eResult, input1 ) =
                    interpretExpr e env input
            in
            case eResult of
                Ok v ->
                    interpretExpr body (Env.extend id v env) input1

                Err _ ->
                    ( eResult
                    , input1
                    )
