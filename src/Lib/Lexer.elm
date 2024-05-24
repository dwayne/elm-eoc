module Lib.Lexer exposing
    ( int
    , keyword
    , spaces
    , symbol
    )

import Parser as P exposing ((|.), (|=), Parser)


int : Parser Int
int =
    P.succeed
        (\isNegative n ->
            if isNegative then
                -n

            else
                n
        )
        |= chompOptional ((==) '-')
        |= digits


digits : Parser Int
digits =
    chompOneOrMore Char.isDigit
        |> P.getChompedString
        |> P.map (Maybe.withDefault 0 << String.toInt)
        |> lexeme


keyword : String -> Parser ()
keyword =
    lexeme << P.keyword


symbol : String -> Parser ()
symbol =
    lexeme << P.symbol


lexeme : Parser a -> Parser a
lexeme p =
    P.succeed identity
        |= p
        |. spaces


spaces : Parser ()
spaces =
    P.spaces



-- HELPERS


chompOptional : (Char -> Bool) -> Parser Bool
chompOptional isGood =
    P.oneOf
        [ P.chompIf isGood
            |> P.map (always True)
        , P.succeed False
        ]


chompOneOrMore : (Char -> Bool) -> Parser ()
chompOneOrMore isGood =
    P.succeed ()
        |. P.chompIf isGood
        |. P.chompWhile isGood
