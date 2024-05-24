module Test.Ch2.L_Var.Interpreter exposing (suite)

import Ch2.L_Var.Interpreter as I
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch2.L_Var.Interpreter" <|
        List.map testRun
            [ ( "(+ 10 32)", [], 42 )
            , ( "(+ 10 (- (+ 12 20)))", [], -22 )
            , ( "(+ (read) (- 8))", [ 50 ], 42 )
            , ( "(let ([y 5]) y)", [], 5 )
            , ( "(let ([x (+ 12 20)]) (+ 10 x))", [], 42 )
            , ( "(let ([x 32]) (+ (let ([x 10]) x) x))", [], 42 )
            , ( "(let ([x (read)]) (let ([y (read)]) (+ x (- y))))", [ 52, 10 ], 42 )
            ]


testRun : ( String, I.Input, Int ) -> Test
testRun ( source, input, expected ) =
    test (Debug.toString { source = source, input = input }) <|
        \_ ->
            case I.run source input of
                Ok actual ->
                    if expected == actual then
                        Expect.pass

                    else
                        Expect.fail <| "expected = " ++ Debug.toString expected ++ ", actual = " ++ Debug.toString actual

                Err e ->
                    Expect.fail <| Debug.toString e
