module Ch1.L_Int.PartialEvaluator exposing (Error, partialEval, partialEvalProgram)

import Ch1.L_Int.AST as AST exposing (..)
import Ch1.L_Int.Parser as P


type alias Error =
    P.Error


partialEval : String -> Result Error AST.Program
partialEval =
    P.parse >> Result.map partialEvalProgram


partialEvalProgram : AST.Program -> AST.Program
partialEvalProgram (Program expr) =
    Program <| peExpr expr


peExpr : Expr -> Expr
peExpr expr =
    case expr of
        Int _ ->
            expr

        Prim Read ->
            expr

        Prim (Negate aExpr) ->
            peNegate (peExpr aExpr)

        Prim (Add aExpr bExpr) ->
            peAdd (peExpr aExpr) (peExpr bExpr)

        Prim (Sub aExpr bExpr) ->
            peSub (peExpr aExpr) (peExpr bExpr)


peNegate : Expr -> Expr
peNegate aExpr =
    case aExpr of
        Int a ->
            Int -a

        _ ->
            Prim (Negate aExpr)


peAdd : Expr -> Expr -> Expr
peAdd aExpr bExpr =
    case ( aExpr, bExpr ) of
        ( Int a, Int b ) ->
            Int <| a + b

        _ ->
            Prim (Add aExpr bExpr)


peSub : Expr -> Expr -> Expr
peSub aExpr bExpr =
    case ( aExpr, bExpr ) of
        ( Int a, Int b ) ->
            Int <| a + b

        _ ->
            Prim (Sub aExpr bExpr)
