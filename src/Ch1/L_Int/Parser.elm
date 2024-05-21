module Ch1.L_Int.Parser exposing (Error, parse)

import Ch1.L_Int.AST as AST exposing (..)
import Ch1.L_Int.Lexer as L
import Parser as P exposing ((|.), (|=), Parser)


type alias Error =
    List P.DeadEnd


parse : String -> Result Error AST.Program
parse =
    P.run program


program : Parser AST.Program
program =
    P.succeed Program
        |. L.spaces
        |= expr
        |. P.end


expr : Parser Expr
expr =
    P.oneOf
        [ intExpr
        , primExpr
        ]


intExpr : Parser Expr
intExpr =
    P.map Int L.int


primExpr : Parser Expr
primExpr =
    P.succeed Prim
        |. L.symbol "("
        |= P.oneOf
            [ P.succeed Read
                |. L.keyword "read"
            , P.succeed
                (\a maybeB ->
                    case maybeB of
                        Nothing ->
                            Negate a

                        Just b ->
                            Sub a b
                )
                |. L.symbol "-"
                |= P.lazy (\_ -> expr)
                |= optional (P.lazy (\_ -> expr))
            , P.succeed Add
                |. L.symbol "+"
                |= P.lazy (\_ -> expr)
                |= P.lazy (\_ -> expr)
            ]
        |. L.symbol ")"



-- HELPERS


optional : Parser a -> Parser (Maybe a)
optional p =
    P.oneOf
        [ P.map Just p
        , P.succeed Nothing
        ]
