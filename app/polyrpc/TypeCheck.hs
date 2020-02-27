module TypeCheck where

import Type

typeCheck :: Monad m => [TopLevelDecl] -> m [TopLevelDecl]
typeCheck toplevelDecls = do
  (bindingDecls, datatypeDecls) <- splitTopLevelDecls toplevelDecls
  typeInfo <- allDataTypeDecls
  bindingTypeInfo <- allBindingDecls typeInfo bindingDecls
  elab_datatypeDecls <- mapM (elabDataTypeDecls typeInfo) datatypeDecls
  elab_bindingTypeInfo <- mapM (\(f,ty)->
                                  do elab_ty <- elabType typeInfo ty
                                     return (f,elab_ty)) bindingTypeInfo
  elab_conTypeinfo <- allConTypeDecls elab_datatypeDecls
  
  let gti = GlobalTypeInfo {_typeInfo=typeInfo
                           , _bindingTypeInfo=elab_bindingTypeInfo
                           , _conTypeInfo=elabl_conTypeInfo}
    
  return topleveLDecls

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
  if isTypeName name && and (map isTypeVarName tyvars) && allUnique tyvars == []
  then (name, tyvars)
  else error $ "[TypeCheck] collectDataTypeDecls: Invalid names: " ++ name ++ " " ++ show tyvars

allUnique [] = []
allUnique (x:xs) =
  if elem x xs then [x] else allUnique xs

--
type ConTypeInfo = [(String, ([Type], String, [String]))]

allConTypeDecls :: Monad m => [DataTypeDecl] -> m ConTypeInfo
allConTypeDecls datatypeDecls = do
  conTypeInfo <- concatM (mapM allConTypeDecls_ datatypeDecls)
  case allUnique [con | (con,_) <- conTypeInfo] of
    [] -> return conTypeInfo
    (con:_) -> error $ "allConTypeDecls: duplicate constructor: " ++ con

allConTypeDecls_ (DataType name tyvars typeConDecls) = do
  return [ (con, (tys, name tyvars)) | ConDecl con tys <- typeConDecls ]
  
-- Elaboration of datatype declarations
--  by elaborating Int as an identifier into ConType Int [],
--     checking duplicate type variables in each datatype declaration, and
--     checking duplicate constructor names in all datatype declarations.
elabDataTypeDecl :: Monad m => TypeInfo -> m DataTypeDecl
elabDataTypeDecl typeInfo (DataType name tyvars typeConDecls) = do
  elab_typeConDecls <- mapM (elabTypeConDecl typeInfo tyvars) typeConDecls
  return (DataType name tyvars elab_typeConDecls)

elabTypeConDecl :: Monad m => GlobalTypeInfo -> [String] -> TypeConDecl -> m TypeConDecl
elabTypeConDecl typeInfo tyvars (ConDecl con tys) = do
  elab_tys <- mapM (elabType typeInfo tyvars) tys
  return (ConDecl con elab_tys)

elabType gti tyvars (TypeVarType x) = do
  if elem x tyvars then return TypeVarType x else
  if isConstructorName x then
    do _tyvars <- lookupTypeCon (_conTypeInfo gti) x
       if _tyvars == []
       then ConType x []
       else error $ "[TypeCheck]: elabType: Invalid type constructor: " ++ x
  else
    error $ "[TypeCheck] elabType: Not found: " ++ x

elabType typeInfo tyvars (TupleType tys) = do
  elab_tys <- mapM (elabType typeInfo tyvars) tys
  return (TupleType elab_tys)

elabType typeInfo tyvars (FunType ty1 loc ty2) = do
  elab_ty1 <- elabType typeInfo tyvars ty1
  elab_ty2 <- elabType typeInfo tyvars ty2
  return (FunType elab_ty1 loc elab_ty2)

elabType typeInfo tyvars (TypeAbsType abs_tyvars ty) = do
  elab_ty <- elabType typeInfo (tyvars ++ abs_tyvars) ty
  return (TypeAbsType abs_tyvars elab_ty)

elabType typeInfo tyvars (LocAbsType abs_tyvars ty) = do
  elab_ty <- elabType typeInfo (tyvars ++ abs_tyvars) ty
  return (LocAbsType abs_tyvars elab_ty)

elabType typeInfo tyvars (ConType name tys) = do
  _tyvars <- lookupTypeCon typeInfo name
  if length _tyvars == length tys
    then do elab_tys <- mapM (elabType typeInfo tyvars) tys
            return (ConType name elab_tys)
    else error $ "[TypeCheck]: elabType: Invalud args for ConType: " ++ name

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
       { _typeInfo :: TypeInfo
       , _bindingTypeInfo :: BindingTypeInfo
       , _conTypeInfo :: ConTypeInfo }

lookupTypeCon :: Monad m => TypeInfo -> String -> m [String]
lookupTypeCon typeInfo x = do
  let found = [tyvars | (name, tyvars) <- typeInfo, x==name]
  if found /= [] 
    then return (head found)
    else error $ "lookupConstr: Not found construct : " ++ x 

lookupConstr :: Monad m => ConTypeInfo -> String -> m ([Type], String, [String])
lookupConstr conTypeInfo x = do
  let found = [z | (con, z) <- conTypeInfo, x==con]
  if fond /= []
    then return z
    else error $ "lookupConstr: Not found construct: " ++ x 

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
elaborate :: Monad m => GlobalTypeInfo -> [BindingDecl] -> m [BindingDecl]
elaborate gti bindingDecls = do
  bindingTypeInfo <- allBindingDecls bindingDecls
  let gti = {_typeInfo=typeInfo, _bindingTypeInfo=bindingTypeInfo}
  mapM (tychkBindingDecl gti) bindingDecls

elabBindingDecl :: Monad m => GlobalTypeInfo -> BindingDecl -> m BindingDecl
elabBindingDecl gti (Binding name ty expr) = do
  (elabExpr, tyExpr) <- tychkExpr gti emptyTypeChkEnv clientLoc expr ty
  if equalType ty tyExpr
  then return (Binding name ty, elabExpr)

elabExpr :: Monad m =>
  GlobalTypeInfo -> TypeChkEnv -> Location -> Expr -> m (Expr, Type)
elabExpr gti tychkEnv loc (Var x) ty
  | isBindingName x = do
      var_x_ty <- lookupVar tychkEnv x
      if equalType ty var_x_ty then return (Var x, ty)
      else error $ "tychkExpr: Var " ++ x ++ " " ++ show ty  ++ " != " show var_x_ty
  | isConstructorName x = do
      lookupConstr gti x 
      return (Constr x [], ty)
  
 
