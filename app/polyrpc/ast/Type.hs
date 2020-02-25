module Type where

data Location =
    Location String

data Type =
    UnitType
  | IntType
  | BoolType
  | StringType
  | TypeVarType String
  | TupleType [Type]
  | FunType Type Locatin Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type

data TypeDecl =
  DataType String [String] [ConDecl]

data ConDecl =
  ConDecl String [Type]

