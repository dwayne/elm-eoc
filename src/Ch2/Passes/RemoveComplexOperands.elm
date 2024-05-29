module Ch2.Passes.RemoveComplexOperands exposing (rco)

import Ch2.L_Var.AST as AST exposing (..)
import Lib.AList as Env


type alias Env =
    Env.AList Id Expr


rco : AST.Program -> AST.Program
rco (Program expr) =
    rcoExpr expr 1
        |> Tuple.first
        |> Program


rcoExpr : Expr -> Int -> ( Expr, Int )
rcoExpr expr tag =
    case expr of
        Int n ->
            ( expr, tag )

        Var id ->
            ( expr, tag )

        Prim Read ->
            ( expr, tag )

        Prim (Negate aComplex) ->
            let
                ( ( aAtom, env ), tag1 ) =
                    rcoAtom aComplex tag
            in
            ( makeExpr aAtom (Prim (Negate aAtom)) env, tag1 )

        _ ->
            Debug.todo "To be implemented"


rcoAtom : Expr -> Int -> ( ( Expr, Env ), Int )
rcoAtom expr tag =
    case expr of
        Int n ->
            ( ( expr, Env.empty ), tag )

        Var id ->
            ( ( expr, Env.empty ), tag )

        Prim Read ->
            let
                tmpId =
                    "tmp." ++ String.fromInt tag
            in
            ( ( Var tmpId, Env.extend tmpId expr Env.empty ), tag + 1 )

        Prim (Negate aComplex) ->
            let
                ( ( aAtom, env ), tag1 ) =
                    rcoAtom aComplex tag

                tmpId =
                    "tmp." ++ String.fromInt tag1
            in
            ( ( Var tmpId
              , Env.extend tmpId (makeExpr aAtom (Prim (Negate aAtom)) env) env
              )
            , tag1 + 1
            )

        _ ->
            Debug.todo "To be implemented"


makeExpr : Expr -> Expr -> Env -> Expr
makeExpr atom expr env =
    case atom of
        Int n ->
            expr

        Var id ->
            case Env.find id env of
                Just value ->
                    Let id value expr

                Nothing ->
                    expr

        _ ->
            --
            -- This should NEVER happen.
            --
            expr
