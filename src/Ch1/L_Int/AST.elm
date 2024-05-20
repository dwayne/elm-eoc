module Ch1.L_Int.AST exposing
    ( Expr(..)
    , Prim(..)
    , Program(..)
    )


type Program
    = Program Expr


type Expr
    = Int Int
    | Prim Prim


type Prim
    = Read
    | Negate Expr
    | Add Expr Expr
    | Sub Expr Expr
