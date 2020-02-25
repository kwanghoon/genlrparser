module Expr where

import Type

data Expr =
    Var String
  | TypeAbs [String] Expr
  | LocAbs [String] Expr
  | Abs [(String, Location)] Expr
  | Let [Binding] Expr
  | Case Expr [Alternative]
  | App Expr Expr
  | TypeApp Expr [String]
  | LocApp Expr [String]
  | Tuple [Expr]
  | Prim PrimOp [Expr]
  | Const Lit

data Lit =
    IntLit Int
  | StrLit String
  | BoolLit Bool
  | UnitLit

data PrimOp =
    NotPrimOp
  | OrPrimOp
  | AndPrimOp
  | EqPrimOp
  | NeqPrimOp
  | LTPrimOp
  | LEPrimOp
  | GTPrimOp
  | GEPrimOp
  | AddPrimOp
  | SubPrimOp
  | MulPrimOp
  | DivPrimOp
  | MinusPrimOp

data Bidning =
  Binding String Type Expr 

data Alternative =
  Alternative String [String] Expr
