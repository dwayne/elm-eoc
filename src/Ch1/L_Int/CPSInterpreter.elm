module Ch1.L_Int.CPSInterpreter exposing
    ( Continuation
    , Effect(..)
    , Error(..)
    , applyCont
    , run
    )

import Ch1.L_Int.AST as AST exposing (..)
import Ch1.L_Int.Parser as P


type Effect
    = Value Int
    | ReadInt Continuation


type Continuation
    = EndCont
    | NegateCont Continuation
    | Add1Cont AST.Expr Continuation
    | Add2Cont Int Continuation
    | Sub1Cont AST.Expr Continuation
    | Sub2Cont Int Continuation


type Error
    = SyntaxError P.Error


run : String -> Result Error Effect
run source =
    case P.parse source of
        Ok program ->
            Ok <| runProgram program

        Err err ->
            Err <| SyntaxError err


runProgram : AST.Program -> Effect
runProgram (Program expr) =
    interpretExpr expr EndCont


interpretExpr : Expr -> Continuation -> Effect
interpretExpr expr cont =
    case expr of
        Int n ->
            applyCont n cont

        Prim Read ->
            ReadInt cont

        Prim (Negate aExpr) ->
            interpretExpr aExpr (NegateCont cont)

        Prim (Add aExpr bExpr) ->
            interpretExpr aExpr (Add1Cont bExpr cont)

        Prim (Sub aExpr bExpr) ->
            interpretExpr aExpr (Sub1Cont bExpr cont)


applyCont : Int -> Continuation -> Effect
applyCont n cont =
    case cont of
        EndCont ->
            Value n

        NegateCont nextCont ->
            applyCont -n nextCont

        Add1Cont bExpr nextCont ->
            interpretExpr bExpr (Add2Cont n nextCont)

        Add2Cont a nextCont ->
            applyCont (a + n) nextCont

        Sub1Cont bExpr nextCont ->
            interpretExpr bExpr (Sub2Cont n nextCont)

        Sub2Cont a nextCont ->
            applyCont (a - n) nextCont
