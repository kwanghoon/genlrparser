{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module CSExpr where

import Location
import Prim
import Literal
import CSType
import qualified Expr as SE
import Text.JSON.Generic

data Expr =
    ValExpr Value
  | Let [BindingDecl] Expr
  | Case Expr [Alternative]  -- including pi_i (V)
  | App Value Value
  | TypeApp Expr [Type]
  | LocApp Expr [Location]
  | Prim PrimOp [Value]
  deriving (Show, Typeable, Data)

data Value =
    Var String
  | Lit Literal
  | Tuple [Value]
  | Constr String [Type] [Value]
  | Closure [Value] CodeName  
  | UnitM Value
  | BindM String Expr Expr
  | Req Value Value
  | Call Value Value
  | GenApp Location Value Value
  deriving (Show, Typeable, Data)

data BindingDecl =
    Binding String Type Value   -- Not Expr but Value!
    deriving (Show, Typeable, Data)

data DataTypeDecl =
    DataType String [String] [TypeConDecl]
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)

data TopLevelDecl =
    BindingTopLevel BindingDecl
  | DataTypeTopLevel DataTypeDecl
  | LibDeclTopLevel String Type 
-- For aeson  
--  deriving (Show, Generic)
    deriving (Show, Typeable, Data)

data TypeConDecl =
   TypeCon String [Type]
-- For aeson  
--  deriving (Show, Generic)
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

--
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
data FunctionStore = FunctionStore
   { _clientstore :: [(String, (Type, Code))]
   , _serverstore :: [(String, (Type, Code))]
   , _new   :: Int
   }

addClientFun :: FunctionStore -> String -> Type -> Code -> FunctionStore
addClientFun fnstore name ty code =
   fnstore {_clientstore = (name,(ty,code)) : (_clientstore fnstore)}

addServerFun :: FunctionStore -> String -> Type -> Code -> FunctionStore
addServerFun fnstore name ty code =
   fnstore {_serverstore = (name,(ty,code)) : (_serverstore fnstore)}

newName :: FunctionStore -> (Int, FunctionStore)
newName fnstore = let n = _new fnstore in (n, fnstore{_new =n+1})

initFunctionStore = FunctionStore
   { _clientstore=[]
   , _serverstore=[]
   , _new        = 1
   }
   
