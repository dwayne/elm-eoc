module Test.Ch1.L_Int.PartialEvaluator exposing (suite)

import Ch1.L_Int.AST exposing (..)
import Ch1.L_Int.Interpreter as I
import Ch1.L_Int.Parser as P
import Ch1.L_Int.PartialEvaluator as PE
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch1.L_Int.PartialEvaluator"
        [ partialEvalSuite
        , partialEvalProgramSuite
        ]


partialEvalSuite : Test
partialEvalSuite =
    describe "partialEval"
        [ let
            input =
                "(+ (read) (- (+ 5 3)))"
          in
          test input <|
            \_ ->
                PE.partialEval input
                    |> Expect.equal
                        (Ok <|
                            Program (Prim (Add (Prim Read) (Int -8)))
                        )
        ]


partialEvalProgramSuite : Test
partialEvalProgramSuite =
    describe "partialEvalProgram" <|
        List.map testPartialEvalProgram
            [ "(+ 10 (- (+ 5 3)))"
            , "(+ 1 (+ 3 1))"
            , "(- (+ 3 (- 5)))"
            ]


testPartialEvalProgram : String -> Test
testPartialEvalProgram source =
    test source <|
        \_ ->
            case P.parse source of
                Ok program ->
                    Expect.equal
                        (I.runProgram program [])
                        (I.runProgram (PE.partialEvalProgram program) [])

                Err e ->
                    Expect.fail <| Debug.toString e
