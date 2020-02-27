module Type where

data Location =
    Location String

data Type =
  --   UnitType
  -- | IntType
  -- | BoolType
  -- | StringType
    TypeVarType String
  | TupleType [Type]
  | FunType Type Location Type
  | TypeAbsType [String] Type
  | LocAbsType [String] Type
  | ConType String [Type]

equalType :: Type -> Type -> Bool

-- data TypeDecl =
--   TypeDecl String [String] [ConDecl]

-- data ConDecl =
--   ConDecl String [Type]

-- Names
isTypeName (c:s) = isUpper c
isTypeName _     = False

isTypeVarName (c:s) = isLower c
isTypeVarName _ = False

isBindingName (c:s) = isLower c
isBindingName _     = False

isConstructorName (c:s) = isUpper c
isConstructorName _     = False


-- Predefined type names
unitType   = "Unit"
intType    = "Int"
boolType   = "Bool"
stringType = "String"

data Location = Location String

-- Predefined location names
clientLoc = Location "client"
serverLoc = Location "server"


