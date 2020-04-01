{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Expr(Expr(..), AST(..), BindingDecl(..), DataTypeDecl(..)
  , initEnv
  , TopLevelDecl(..), TypeConDecl(..), Alternative(..)
  , TypeInfo, ConTypeInfo, BindingTypeInfo, DataTypeInfo
  , GlobalTypeInfo(..), Env(..)
  , singleTypeAbs, singleLocAbs, singleAbs
  , singleTypeApp, singleLocApp
  , toASTExprSeq, toASTExpr
  , toASTIdSeq, toASTId
  , toASTTypeSeq, toASTType
  , toASTLocationSeq, toASTLocation
  , toASTBindingDeclSeq, toASTBindingDecl
  , toASTDataTypeDecl, toASTTopLevelDeclSeq
  , toASTTypeConDeclSeq, toASTTypeConDecl
  , toASTIdTypeLocSeq, toASTIdTypeLoc
  , toASTAlternativeSeq, toASTAlternative
  , toASTTriple, toASTLit
  ) where

import Location
import Prim
import Literal
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
  | Case Expr (Maybe Type) [Alternative]
  | App Expr (Maybe Type) Expr (Maybe Location)
  | TypeApp Expr (Maybe Type) [Type]
  | LocApp Expr (Maybe Type) [Location]
  | Tuple [Expr]
  | Prim PrimOp [Expr]
  | Lit Literal
  | Constr String [Location] [Type] [Expr]
-- For aeson  
--  deriving (Show, Generic)
  deriving (Show, Typeable, Data)



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

singleTypeApp (TypeApp expr maybe []) = expr
singleTypeApp (TypeApp expr maybe [ty]) = TypeApp expr maybe [ty]
singleTypeApp (TypeApp expr maybe (ty:tys)) =
  singleTypeApp
    (TypeApp
       (TypeApp expr maybe [ty]) (skimTypeAbsType maybe) tys)
singleTypeApp other = other

skimTypeAbsType Nothing = Nothing
skimTypeAbsType (Just (TypeAbsType (tyvar:tyvars) ty)) = Just (TypeAbsType tyvars ty)
skimTypeAbsType maybe = error $ "[skimTypeAbsType]: " ++ show maybe

singleLocApp (LocApp expr maybe []) = expr
singleLocApp (LocApp expr maybe [l]) = LocApp expr maybe [l]
singleLocApp (LocApp expr maybe (l:ls)) =
  singleLocApp
     (LocApp (LocApp expr maybe [l]) (skimLocAbsType maybe) ls)
singleLocApp other = other

skimLocAbsType Nothing = Nothing
skimLocAbsType (Just (LocAbsType (locvar:locvars) ty)) = Just (LocAbsType locvars ty)
skimLocAbsType maybe = error $ "[skimLocAbsType]: " ++ show maybe

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

-- [(Name, Location Vars, Type Vars)]
type TypeInfo = [(String, [String], [String])] 

-- [(ConName, (ConArgTypes, DTName, LocationVars, TypeVars))]
type ConTypeInfo = [(String, ([Type], String, [String], [String]))] 

type BindingTypeInfo = [(String, Type)]

-- [ (DTName, LocationVars, TypeVars, [(ConName, ArgTypes)]) ]
type DataTypeInfo = [(String, ([String], [String], [(String,[Type])]))]

data GlobalTypeInfo = GlobalTypeInfo
       { _typeInfo :: TypeInfo
       , _conTypeInfo :: ConTypeInfo
       , _dataTypeInfo :: DataTypeInfo
       , _bindingTypeInfo :: BindingTypeInfo }
       
data Env = Env
       { _locVarEnv  :: [String]
       , _typeVarEnv :: [String]
       , _varEnv     :: BindingTypeInfo }

initEnv = Env { _locVarEnv=[], _typeVarEnv=[], _varEnv=[] }

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

