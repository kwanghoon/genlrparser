{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Expr where

import Location
import Type
-- For aeson
-- import GHC.Generics
-- import Data.Aeson
import Text.JSON.Generic

--
data Expr =
    Var String
  | TypeAbs [String] Expr
  | LocAbs [String] Expr
  | Abs [(String, Type, Location)] Expr
  | Let [BindingDecl] Expr
  | Case Expr [Alternative]
  | App Expr Expr (Maybe Location)
  | TypeApp Expr [Type]
  | LocApp Expr [Location]
  | Tuple [Expr]
  | Prim PrimOp [Expr]
  | Lit Literal
  | Constr String [Location] [Type] [Expr]
-- For aeson  
--  deriving (Show, Generic)
  deriving (Show, Typeable, Data)

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

data PrimOp =
    NotPrimOp  --{l}. Bool -l-> Bool
  | OrPrimOp   --{l}. (Bool, Bool) -l-> Bool
  | AndPrimOp  --{l}. (Bool, Bool) -l-> Bool
  | EqPrimOp   --{l}. (Bool, Bool) -l-> Bool
  | NeqPrimOp  --{l}. (Bool, Bool) -l-> Bool
  | LtPrimOp   --{l}. (Int, Int) -l-> Bool
  | LePrimOp   --{l}. (Int, Int) -l-> Bool
  | GtPrimOp   --{l}. (Int, Int) -l-> Bool
  | GePrimOp   --{l}. (Int, Int) -l-> Bool
  | AddPrimOp  --{l}. (Int, Int) -l-> Int
  | SubPrimOp  --{l}. (Int, Int) -l-> Int
  | MulPrimOp  --{l}. (Int, Int) -l-> Int
  | DivPrimOp  --{l}. (Int, Int) -l-> Int
  | NegPrimOp  --{l}. Int -l-> Int
-- For aeson  
--  deriving (Show, Eq, Generic)
  deriving (Show, Eq, Typeable, Data)


singleTypeAbs (TypeAbs [] expr) = expr
singleTypeAbs (TypeAbs [a] expr) = TypeAbs [a] expr
singleTypeAbs (TypeAbs (a:as) expr) = TypeAbs [a] (singleTypeAbs (TypeAbs as expr))
singleTypeAbs other = other

singleLocAbs (LocAbs [] expr) = expr
singleLocAbs (LocAbs [l] expr) = LocAbs [l] expr
singleLocAbs (LocAbs (l:ls) expr) = LocAbs [l] (singleLocAbs (LocAbs ls expr))
singleLocAbs other = other

singleAbs (Abs [] expr) = expr
singleAbs (Abs [t] expr) = Abs [t] expr
singleAbs (Abs (t:ts) expr) = Abs [t] (singleAbs (Abs ts expr))
singleAbs other = other

singleTypeApp (TypeApp expr []) = expr
singleTypeApp (TypeApp expr [ty]) = TypeApp expr [ty]
singleTypeApp (TypeApp expr (ty:tys)) = singleTypeApp (TypeApp (TypeApp expr [ty]) tys)
singleTypeApp other = other

singleLocApp (LocApp expr []) = expr
singleLocApp (LocApp expr [l]) = LocApp expr [l]
singleLocApp (LocApp expr (l:ls)) = singleLocApp (LocApp (LocApp expr [l]) ls)
singleLocApp other = other


primType tyname = ConType tyname [] []

bool_type = primType boolType
int_type  = primType intType
unit_type = primType unitType
string_type = primType stringType

primOpTypes :: [(PrimOp, ([Type], Type))]
primOpTypes =
  [ (NotPrimOp, ([bool_type], bool_type))
  , (OrPrimOp,  ([bool_type, bool_type], bool_type))
  , (AndPrimOp, ([bool_type, bool_type], bool_type))
  , (EqPrimOp,  ([bool_type, bool_type], bool_type))
  , (NeqPrimOp, ([bool_type, bool_type], bool_type))
  , (LtPrimOp,  ([int_type, int_type], bool_type))
  , (LePrimOp,  ([int_type, int_type], bool_type))
  , (GtPrimOp,  ([int_type, int_type], bool_type))
  , (GePrimOp,  ([int_type, int_type], bool_type))
  , (AddPrimOp, ([int_type, int_type], int_type))
  , (SubPrimOp, ([int_type, int_type], int_type))
  , (MulPrimOp, ([int_type, int_type], int_type))
  , (DivPrimOp, ([int_type, int_type], int_type))
  , (NegPrimOp, ([int_type], int_type))
  ]

lookupPrimOpType primop =
  [ (tys,ty) | (primop1,(tys,ty)) <- primOpTypes, primop==primop1]

data BindingDecl =
    Binding String Type Expr
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)

--
-- The four forms of data type declarations supported now.
--
--  data D =                             C1 | ... | Cn
--  data D = [a1 ... ak]               . C1 | ... | Cn 
--  data D = {l1 ... li}               . C1 | ... | Cn 
--  data D = {l1 ... li} . [a1 ... ak] . C1 | ... | Cn
--
data DataTypeDecl =
    DataType String [LocationVar] [TypeVar] [TypeConDecl] -- 
    deriving (Show, Typeable, Data)

data TopLevelDecl =
    BindingTopLevel BindingDecl
  | DataTypeTopLevel DataTypeDecl
  | LibDeclTopLevel String Type 
  deriving (Show, Typeable, Data)

data TypeConDecl =
   TypeCon String [Type]
   deriving (Show, Typeable, Data)

data Alternative =
    Alternative String [String] Expr
  | TupleAlternative [String] Expr
  deriving (Show, Typeable, Data)

--
-- For aeson
-- instance ToJSON Expr where
-- instance ToJSON Literal where
-- instance ToJSON PrimOp where
-- instance ToJSON BindingDecl where
-- instance ToJSON DataTypeDecl where
-- instance ToJSON TopLevelDecl where
-- instance ToJSON TypeConDecl where
-- instance ToJSON Alternative where


--
-- For type-checker
type TypeInfo = [(String, [String])]

type ConTypeInfo = [(String, ([Type], String, [String]))]

type BindingTypeInfo = [(String, Type)]

type DataTypeInfo = [(String, ([String], [(String,[Type])]))]

data GlobalTypeInfo = GlobalTypeInfo
       { _typeInfo :: TypeInfo
       , _conTypeInfo :: ConTypeInfo
       , _dataTypeInfo :: DataTypeInfo
       , _bindingTypeInfo :: BindingTypeInfo }
       
data Env = Env
       { _locVarEnv  :: [String]
       , _typeVarEnv :: [String]
       , _varEnv     :: BindingTypeInfo }


--
data AST =
    ASTExprSeq { fromASTExprSeq :: [Expr] }
  | ASTExpr    { fromASTExpr    :: Expr   }
  | ASTIdSeq   { fromASTIdSeq   :: [String] }
  | ASTId      { fromASTId      :: String }
  | ASTTypeSeq { fromASTTypeSeq :: [Type] }
  | ASTType    { fromASTType    :: Type  }
  | ASTLocationSeq { fromASTLocationSeq :: [Location] }
  | ASTLocation    { fromASTLocation    :: Location  }
  
  | ASTBindingDeclSeq { fromASTBindingDeclSeq :: [BindingDecl] }
  | ASTBindingDecl    { fromASTBindingDecl    :: BindingDecl  }

  | ASTDataTypeDecl { fromASTDataTypeDecl :: DataTypeDecl }

  | ASTTopLevelDeclSeq { fromASTTopLevelDeclSeq :: [TopLevelDecl] }
  
  | ASTTypeConDeclSeq { fromASTTypeConDeclSeq :: [TypeConDecl] }
  | ASTTypeConDecl { fromASTTypeConDecl :: TypeConDecl }
  
  | ASTIdTypeLocSeq { fromASTIdTypeLocSeq :: [(String,Type,Location)] }
  | ASTIdTypeLoc { fromASTIdTypeLoc :: (String,Type,Location) }
  
  | ASTAlternativeSeq { fromASTAlternativeSeq :: [Alternative] }
  | ASTAlternative { fromASTAlternative :: Alternative }
  
  | ASTLit { fromASTLit :: Literal }

  | ASTTriple { fromASTTriple :: ([String], [String], [TypeConDecl]) }
  
toASTExprSeq exprs = ASTExprSeq exprs
toASTExpr expr     = ASTExpr expr
toASTIdSeq   ids   = ASTIdSeq ids
toASTId   id       = ASTId id
toASTTypeSeq types = ASTTypeSeq types
toASTType ty     = ASTType ty
toASTLocationSeq locations = ASTLocationSeq locations
toASTLocation location     = ASTLocation location

toASTBindingDeclSeq bindings = ASTBindingDeclSeq bindings
toASTBindingDecl binding     = ASTBindingDecl binding

toASTDataTypeDecl datatype     = ASTDataTypeDecl datatype

toASTTopLevelDeclSeq toplevel = ASTTopLevelDeclSeq toplevel

toASTTypeConDeclSeq typecondecls = ASTTypeConDeclSeq typecondecls
toASTTypeConDecl typecondecl     = ASTTypeConDecl typecondecl

toASTIdTypeLocSeq idtypelocs = ASTIdTypeLocSeq idtypelocs
toASTIdTypeLoc idtypeloc     = ASTIdTypeLoc idtypeloc

toASTAlternativeSeq alts = ASTAlternativeSeq alts
toASTAlternative alt     = ASTAlternative alt

toASTTriple triple = ASTTriple triple

toASTLit lit     = ASTLit lit

