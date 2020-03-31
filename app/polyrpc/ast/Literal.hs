{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Literal where

import Prim
import Text.JSON.Generic

data Literal =
    IntLit Int
  | StrLit String
  | BoolLit Bool
  | UnitLit
-- For aeson  
--  deriving (Show, Generic)
  deriving (Show, Typeable, Data)

typeOfLiteral (IntLit _) = int_type
typeOfLiteral (StrLit _) = string_type
typeOfLiteral (BoolLit _) = bool_type
typeOfLiteral (UnitLit) = unit_type

trueLit  = "True"
falseLit = "False"
unitLit  = "()"

