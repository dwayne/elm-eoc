module Ch2.L_Var.Parser exposing (Error, parse)

import Ch2.L_Var.AST as AST exposing (..)
import Lib.Lexer as L
import Parser as P exposing ((|.), (|=), Parser)
import Set


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
        , varExpr
        , parenExpr
        ]


intExpr : Parser Expr
intExpr =
    P.map Int L.int


varExpr : Parser Expr
varExpr =
    P.map Var id


parenExpr : Parser Expr
parenExpr =
    P.succeed identity
        |. L.symbol "("
        |= P.oneOf
            [ P.succeed (Prim Read)
                |. L.keyword "read"
            , P.succeed
                (\a maybeB ->
                    case maybeB of
                        Nothing ->
                            Prim <| Negate a

                        Just b ->
                            Prim <| Sub a b
                )
                |. L.symbol "-"
                |= P.lazy (\_ -> expr)
                |= optional (P.lazy (\_ -> expr))
            , P.succeed (\a b -> Prim <| Add a b)
                |. L.symbol "+"
                |= P.lazy (\_ -> expr)
                |= P.lazy (\_ -> expr)
            , P.succeed Let
                |. L.keyword "let"
                |. L.symbol "("
                |. L.symbol "["
                |= id
                |= P.lazy (\_ -> expr)
                |. L.symbol "]"
                |. L.symbol ")"
                |= P.lazy (\_ -> expr)
            ]
        |. L.symbol ")"


id : Parser String
id =
    let
        reserved =
            Set.fromList
                [ "let"
                , "read"
                ]
    in
    L.makeIdentifier reserved



-- HELPERS


optional : Parser a -> Parser (Maybe a)
optional p =
    P.oneOf
        [ P.map Just p
        , P.succeed Nothing
        ]
