module Ch2.Passes.Uniquify exposing (uniquify)

import Ch2.L_Var.AST as AST exposing (..)
import Lib.AList as Env


uniquify : AST.Program -> AST.Program
uniquify (AST.Program expr) =
    uniquifyExpr Env.empty expr 1
        |> Tuple.first
        |> AST.Program


type alias Env =
    Env.AList Id Id


uniquifyExpr : Env -> Expr -> Int -> ( Expr, Int )
uniquifyExpr env expr tag =
    case expr of
        Int n ->
            ( expr, tag )

        Prim Read ->
            ( expr, tag )

        Prim (Negate a) ->
            let
                ( uA, tag1 ) =
                    uniquifyExpr env a tag
            in
            ( Prim (Negate uA), tag1 )

        Prim (Add a b) ->
            let
                ( uA, tag1 ) =
                    uniquifyExpr env a tag

                ( uB, tag2 ) =
                    uniquifyExpr env b tag1
            in
            ( Prim (Add uA uB), tag2 )

        Prim (Sub a b) ->
            let
                ( uA, tag1 ) =
                    uniquifyExpr env a tag

                ( uB, tag2 ) =
                    uniquifyExpr env b tag1
            in
            ( Prim (Sub uA uB), tag2 )

        Var id ->
            case Env.find id env of
                Just uniqueId ->
                    ( Var uniqueId, tag )

                Nothing ->
                    --
                    -- id is a free variable. Leave it unchanged.
                    --
                    ( expr, tag )

        Let id e body ->
            let
                ( uE, tag1 ) =
                    uniquifyExpr env e tag

                newId =
                    --
                    -- N.B. The parser doesn't allow a period
                    -- in the syntax for variable names. So, the
                    -- newId cannot clash with any existing variable
                    -- names in the AST since we're adding a period.
                    --
                    id ++ "." ++ String.fromInt tag1

                tag2 =
                    tag1 + 1

                ( uBody, tag3 ) =
                    uniquifyExpr (Env.extend id newId env) body tag2
            in
            ( Let newId uE uBody, tag3 )
