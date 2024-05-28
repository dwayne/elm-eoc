module Test.Ch2.Passes.Uniquify exposing (suite)

import Ch2.L_Var.AST as AST exposing (..)
import Ch2.L_Var.Parser as P
import Ch2.Passes.Uniquify exposing (uniquify)
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch2.Passes.Uniquify" <|
        List.map testUniquify
            [ ( """
                (let ([x 32])
                  (+ (let ([x 10]) x) x))
                """
              , Let "x.1" (Int 32) <|
                    Prim (Add (Let "x.2" (Int 10) (Var "x.2")) (Var "x.1"))
              )
            , ( """
                (let ([x (let ([x 4])
                           (+ x 1))])
                  (+ x 2))
                """
              , Let "x.2"
                    (Let "x.1"
                        (Int 4)
                        (Prim (Add (Var "x.1") (Int 1)))
                    )
                    (Prim (Add (Var "x.2") (Int 2)))
              )

            --
            -- Exercise 2.2
            --
            , ( "x", Var "x" )
            , ( """
                (let ([a 10])
                  (- (let ([a 5]) a)))
                """
              , Let "a.1" (Int 10) <|
                    Prim (Negate (Let "a.2" (Int 5) (Var "a.2")))
              )
            , ( """
                (- (let ([x 3]) x)
                   (let ([x 2]) x))
                """
              , Prim <|
                    Sub
                        (Let "x.1" (Int 3) (Var "x.1"))
                        (Let "x.2" (Int 2) (Var "x.2"))
              )
            ]


testUniquify : ( String, AST.Expr ) -> Test
testUniquify ( source, expr ) =
    let
        expectedProgram =
            AST.Program expr
    in
    test source <|
        \_ ->
            case P.parse source |> Result.map uniquify of
                Ok actualProgram ->
                    if expectedProgram == actualProgram then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expectedProgram ++ ", actual = " ++ Debug.toString actualProgram

                Err err ->
                    Expect.fail <| Debug.toString err
