module Expr where

-- Expr Sequence: Expr1; ... ; Exprn
-- [Expr]

data Expr =
    Lit Int
  | Var String
  | BinOp BinOpKind Expr Expr
  | Assign String Expr

data BinOpKind = ADD | SUB | MUL | DIV


