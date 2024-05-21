module Test.Ch1.L_Int.Parser exposing (suite)

import Ch1.L_Int.AST exposing (..)
import Ch1.L_Int.Parser as P
import Expect
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "Ch1.L_Int.Parser"
        [ parseSuite
        , leafSuite
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


leafSuite : Test
leafSuite =
    describe "leaf"
        [ test "(read)" <|
            \_ ->
                leaf (Prim Read)
                    |> Expect.equal True
        , test "(- 8)" <|
            \_ ->
                leaf (Prim (Negate (Int 8)))
                    |> Expect.equal False
        , test "8" <|
            \_ ->
                leaf (Int 8)
                    |> Expect.equal True
        ]


leaf : Expr -> Bool
leaf expr =
    case expr of
        Int _ ->
            True

        Prim prim ->
            case prim of
                Read ->
                    True

                Negate _ ->
                    False

                Add _ _ ->
                    False

                Sub _ _ ->
                    False
