module TypeCheck where

import Type

typeCheck :: Monad m => [TopLevelDecl] -> m [TopLevelDecl]
typeCheck toplevelDecls = toplevelDecls

splitTopLevelDecls :: Monad m => [TopLevelDecl] -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecls toplevelDecls =
  (\p -> (concat (fst p), concat (snd p))) $ unzip $ map splitTopLevelDecl $ toplevelDecls

splitTopLevelDecl :: Monad m :: TopLevelDecl -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecl (BindingTopLevel bindingDecl)   = return ([bindingDecl], [])
splitTopLevelDecl (DataTypeTopLevel datatypeDecl) = return ([], [datatypeDecl])


-- Collect datatype decls with predefined types
type TypeInfo = [(String, [String])]

allDataTypeDecls :: Monad m => [DataTypeDecl] -> m TypeInfo
allDataTypeDecls datatypeDecls = do
  let collectedOnes = collectDataTypeDecls datatypeDecls
  return (predefinedDataTypeDecls ++ collectedOnes)

predefinedDataTypeDecls :: [(String, [String])]
predefinedDataTypeDecls = [
    (unitType,   []),
    (intType,    []),
    (boolType,   []),
    (stringType, [])
  ]

collectDataTypeDecls :: Monad m => [DataTypeDecl] -> m [(String, [String])]
collectDataTypeDecls datatypeDecls = do
  let nameTyvarsPairList = map collectDataTypeDecl datatypeDecls
  return nameTyvarsPairList

collectDataTypeDecl (DataType name tyvars typeConDecls) =
  if isTypeName name && and (map isTypeVarName tyvars) && allUnique tyvars
  then (name, tyvars)
  else error $ "[TypeCheck] collectDataTypeDecls: Invalid names: " ++ name ++ " " ++ show tyvars

allUnique [] = True
allUnique (x:xs) =
  if elem x xs then False else allUnique xs

tychkDataTypeDecl :: Monad m => DataTypeDecl -> m DataTypeDecl
tychkDataTypeDecl typeInfo (DataType name tyvars typeConDecls) = do
  elabTypeConDecls <- mapM (wfTypeConDecl typeInfo tyvars) typeConDecls
  return (DataType name tyvars elabTypeConDecls)

wfTypeConDecl typeInfo tyvars (ConDecl con tys) = do
  elabTys <- mapM (wfType typeInfo tyvars) tys
  return (ConDecl con elabTys)

!!!HERE HERE!!!
wfType typeInfo tyvars (TypeVarType x) = do
  if elem x tyvars then return TypeVarType x
  else if isConstructorName x
       then do  <- lookupConstr typeInfo 

-- Collect names and types of bindings
type BindingTypeInfo = [(String, Type)]

allBindingDecls :: Monad m => TypeInfo -> [BindingDecl] -> m BindingTypeInfo
allBindingDecls typeInfo bindingDecls = do
  mapM (allBindingDecl typeInfo) bindingDecls

allBindingDecl :: Monad m => TypeInfo -> BindingDecl -> m (String, Type)
allBindingDecl typeInfo (Binding name ty expr) =
  if freeTyvars typeInfo ty == [] then return (name, ty)
  else error $ "[TypeCheck] allBindingDecl: " ++ name ++ " has type with free variables"

-- Well-formed data type declarations

data GlobalTypeInfo = GlobalTypeInfo
       { _typeInfo :: TypeInfo, _bindingTypeInfo :: BindingTypeInfo }

!!!HERE HERE!!!
lookupConstr :: Monad m => TypeInfo -> String -> m ([[Type], Type)
lookupConstr typeInfo x = do
  let found = [ | <- typeInfo ]

--
data TypeChkEnv = TypeChkEnv
       { _varEnv :: [(String,Type)], _locVarEnv :: [String], _typeVarEnv :: [String] }

emptyTypeChkEnv = {_varEnv=[], _locVarEnv=[], _typeVarEnv=[]}

lookupVar :: Monad m => GlobalTypeInfo -> TypeChkEnv -> String -> m Type
lookupVar gti tychkEnv x = do
  let found = [ty | (y,ty) <- _varEnv tychkEnv, x==y]
  if found /= [] then return (head found)
  else do let found' = [ty | (z,ty) <- _bindingTypeInfo gti, x==z]
          if found' /= [] then return (head found')
          else error $ "lookupVar: Not found: " ++ x

--
tychk :: Monad m => TypeInfo -> [BindingDecl] -> m [BindingDecl]
tychk typeInfo bindingDecls = do
  bindingTypeInfo <- allBindingDecls bindingDecls
  let gti = {_typeInfo=typeInfo, _bindingTypeInfo=bindingTypeInfo}
  mapM (tychkBindingDecl gti) bindingDecls

tychkBindingDecl :: Monad m => GlobalTypeInfo -> BindingDecl -> m BindingDecl
tychkBindingDecl gti (Binding name ty expr) = do
  (elabExpr, tyExpr) <- tychkExpr gti emptyTypeChkEnv clientLoc expr ty
  if equalType ty tyExpr
  then return (Binding name ty, elabExpr)

tychkExpr :: Monad m =>
  GlobalTypeInfo -> TypeChkEnv -> Location -> Expr -> m (Expr, Type)
tychkExpr gti tychkEnv loc (Var x) ty
  | isBindingName x = do
      var_x_ty <- lookupVar tychkEnv x
      if equalType ty var_x_ty then return (Var x, ty)
      else error $ "tychkExpr: Var " ++ x ++ " " ++ show ty  ++ " != " show var_x_ty
  | isConstructorName x = do
      lookupConstr gti x 
      return (Constr x [], ty)
  
 