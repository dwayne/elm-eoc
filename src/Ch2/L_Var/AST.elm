module Ch2.L_Var.AST exposing
    ( Expr(..)
    , Id
    , Prim(..)
    , Program(..)
    )


type Program
    = Program Expr


type Expr
    = Int Int
    | Prim Prim
    | Var Id
    | Let Id Expr Expr


type Prim
    = Read
    | Negate Expr
    | Add Expr Expr
    | Sub Expr Expr


type alias Id =
    String
