module Test.Ch2.L_Var.Parser exposing (suite)

import Ch2.L_Var.AST exposing (..)
import Ch2.L_Var.Parser as P
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch2.L_Var.Parser"
        [ parseSuite
        ]


parseSuite : Test
parseSuite =
    describe "parse" <|
        List.map testParse
            [ ( "-5", Int -5 )
            , ( "0", Int 0 )
            , ( "5", Int 5 )
            , ( "(read)", Prim Read )
            , ( " ( read ) ", Prim Read )
            , ( "(-5)", Prim (Negate (Int 5)) )
            , ( "(- 5)", Prim (Negate (Int 5)) )
            , ( "(- 8 6)", Prim (Sub (Int 8) (Int 6)) )
            , ( "(+ 15 9)", Prim (Add (Int 15) (Int 9)) )
            , ( "(+ 8 -6)", Prim (Add (Int 8) (Int -6)) )

            --
            -- From (1.1)
            --
            , ( "(+ (read) (- 8))"
              , Prim (Add (Prim Read) (Prim (Negate (Int 8))))
              )

            --
            -- Test Var and Let
            --
            , ( "x", Var "x" )
            , ( "(let ([y 5]) y)", Let "y" (Int 5) (Var "y") )
            , ( "(let ([x (+ 12 20)]) (+ 10 x))"
              , Let
                    "x"
                    (Prim (Add (Int 12) (Int 20)))
                    (Prim (Add (Int 10) (Var "x")))
              )
            , ( "(let ([x 32]) (+ (let ([x 10]) x) x))"
              , Let "x" (Int 32) <|
                    Prim <|
                        Add
                            (Let "x" (Int 10) (Var "x"))
                            (Var "x")
              )
            , ( "(let ([x (read)]) (let ([y (read)]) (+ x (- y))))"
              , Let "x" (Prim Read) <|
                    Let "y" (Prim Read) <|
                        Prim <|
                            Add (Var "x") (Prim (Negate (Var "y")))
              )
            ]


testParse : ( String, Expr ) -> Test
testParse ( input, expectedExpr ) =
    test input <|
        \_ ->
            case P.parse input of
                Ok actualProgram ->
                    let
                        expectedProgram =
                            Program expectedExpr
                    in
                    if expectedProgram == actualProgram then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expectedProgram ++ ", actual = " ++ Debug.toString actualProgram

                Err e ->
                    Expect.fail <| Debug.toString e
