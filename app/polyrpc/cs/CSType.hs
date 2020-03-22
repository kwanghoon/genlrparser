{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module CSType where

import Text.JSON.Generic

import Location


data Type =
    TypeVarType String
  | TupleType [Type]
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type
  | ConType String [Type]
  | RefType Location Type
  | CloType Type   -- Clo A
  | MonType Type   -- T A
  deriving (Show, Typeable, Data)

data CodeType =
    CodeType [String] [String] [Type] Type  -- [alpha] [loc]. [type]. Type
    deriving (Show, Typeable, Data)

