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
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type

-- data TypeDecl =
--   TypeDecl String [String] [ConDecl]

-- data ConDecl =
--   ConDecl String [Type]


