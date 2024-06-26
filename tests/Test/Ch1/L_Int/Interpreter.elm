module Test.Ch1.L_Int.Interpreter exposing (suite)

import Ch1.L_Int.Interpreter as I
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch1.L_Int.Interpreter" <|
        List.map testRun
            [ ( "(+ 10 32)", [], 42 )
            , ( "(+ 10 (- (+ 12 20)))", [], -22 )
            , ( "(+ (read) (- 8))", [ 50 ], 42 )
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
