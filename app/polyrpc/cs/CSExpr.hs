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
  | Case Value Type [Alternative]  -- including pi_i (V)
  | App Value Type Value
  | TypeApp Value Type [Type]
  | LocApp Value Type [Location]
  | Prim PrimOp [Value]
  deriving (Show, Typeable, Data)

data Value =
    Var String
  | Lit Literal
  | Tuple [Value]
  | Constr String [Location] [Type] [Value]
  | Closure [Value] CodeName  
  | UnitM Value
  | BindM [BindingDecl] Expr
  | Req Value Type Value
  | Call Value Type Value
  | GenApp Location Value Type Value
  deriving (Show, Typeable, Data)

data BindingDecl =
    Binding String Type Expr
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
  | TupleAlternative [String] Expr    
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
    CodeName String [Location] [Type]
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
    deriving (Show, Typeable, Data)
       
data Env = Env
   { _locVarEnv  :: [String]
   , _typeVarEnv :: [String]
   , _varEnv     :: BindingTypeInfo }

initEnv = Env { _locVarEnv=[], _typeVarEnv=[], _varEnv=[] }

--
data FunctionStore = FunctionStore
   { _clientstore :: [(String, (CodeType, Code))]
   , _serverstore :: [(String, (CodeType, Code))]
   , _new   :: Int
   }
   deriving (Show, Typeable, Data)

addClientFun :: FunctionStore -> String -> CodeType -> Code -> FunctionStore
addClientFun fnstore name ty code =
   fnstore {_clientstore = (name,(ty,code)) : (_clientstore fnstore)}

addServerFun :: FunctionStore -> String -> CodeType -> Code -> FunctionStore
addServerFun fnstore name ty code =
   fnstore {_serverstore = (name,(ty,code)) : (_serverstore fnstore)}

addFun :: Location -> FunctionStore -> String -> CodeType -> Code -> FunctionStore
addFun loc funstore name ty code
  | isClient loc = addClientFun funstore name ty code
  | isServer loc = addServerFun funstore name ty code
  | otherwise    = addServerFun (addClientFun funstore name ty code) name ty code

newName :: FunctionStore -> (String, FunctionStore)
newName fnstore = let n = _new fnstore in ("f" ++ show n, fnstore{_new =n+1})

newVar :: FunctionStore -> (String, FunctionStore)
newVar fnstore = let n = _new fnstore in ("x" ++ show n, fnstore{_new =n+1})

newVars :: Int -> FunctionStore -> ([String], FunctionStore)
newVars 0 funStore = ([], funStore)
newVars n funStore = 
    let (x,  funStore1) = newVar funStore
        (xs, funStore2) = newVars (n-1) funStore1
    in  (x:xs, funStore2)

initFunctionStore = FunctionStore
   { _clientstore=[]
   , _serverstore=[]
   , _new        = 1
   }
   
