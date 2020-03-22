{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module CSExpr where

import Location
import Type
import Text.JSON.Generic

data Expr =
    ValExpr Value
  | Let [BindingDecl] Expr
  | Case Expr [Alternative]  -- including pi_i (V)
  | App Value Value
  | TypeApp Expr [Type]
  | LocApp Expr [Location]
  deriving (Show, Typeable, Data)

data Value =
    Var String
  | Constr String [Type] [Value]    -- including (V,W)
  | Closure [Value] CodeName  
  | UnitM Value
  | BindM String Expr Expr
  | Req Value Value
  | Call Value Value
  | GenApp Location Value Value
  deriving (Show, Typeable, Data)

data BindingDecl =
    Binding String Type Expr
    deriving (Show, Typeable, Data)

data Alternative =
    Alternative String [String] Expr
    deriving (Show, Typeable, Data)

data Code =
    Code [String] [String] [String] OpenCode  -- [alpha] [loc]. [x]. OpenCode
    deriving (Show, Typeable, Data)

data OpenCode =
    CodeAbs     [(String, Type)] Expr
  | CodeTypeAbs [String] Expr
  | CodeLocAbs  [String] Expr
  deriving (Show, Typeable, Data)
  

data CodeName =
    CodeName String [Type] [Location]
    deriving (Show, Typeable, Data)

