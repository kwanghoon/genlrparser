module TypeCheck where

import Type

typeCheck :: Monad m => [TopLevelDecl] -> m ([DataTypeDecl], [BindingDecl])
typeCheck toplevelDecls = do
  (bindingDecls, datatypeDecls) <- splitTopLevelDecls toplevelDecls
  
  typeInfo <- allDataTypeDecls
  
  elab_datatypeDecls <- mapM (elabDataTypeDecls typeInfo) datatypeDecls
  
  elab_bindingTypeInfo <- elabBindingTypeInfo typeInfo bindingDecls
                          
  elab_conTypeinfo <- allConTypeDecls elab_datatypeDecls
  
  let gti = GlobalTypeInfo {_typeInfo=typeInfo
                           , _bindingTypeInfo=elab_bindingTypeInfo
                           , _conTypeInfo=elabl_conTypeInfo}
            
  partial_elab_bindingDecls <- mapM (\((f,ty),(_,_,expr) -> (f,ty,expr)))
                                (zip elab_bindingTypeInfo bindingDecls)
                               
  elab_bindingDecls <- elaborate gti elab_binding partial_elab_bindingDecls
  
  return (elab_datatypeDecls, elab_bindingDecls)

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

elabType typeInfo tyvars (TypeVarType x) = do
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

elabBindingTypeInfo typeInfo bindingDecls =
  mapM (\(Binding f ty _)-> do
           elab_ty <- elabType typeInfo [] ty
           return (f,elab_ty))

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
data Env = Env
       { _varEnv :: [(String,Type)], _locVarEnv :: [String], _typeVarEnv :: [String] }

emptyEnv = {_varEnv=[], _locVarEnv=[], _typeVarEnv=[]}

lookupVar :: Monad m => GlobalTypeInfo -> TypeChkEnv -> String -> m Type
lookupVar gti tychkEnv x = do
  let found = [ty | (y,ty) <- _varEnv tychkEnv, x==y]
  if found /= [] then return (head found)
  else do let found' = [ty | (z,ty) <- _bindingTypeInfo gti, x==z]
          if found' /= [] then return (head found')
          else error $ "lookupVar: Not found: " ++ x

--
elaborate :: Monad m => GlobalTypeInfo -> [BindingDecl] -> m [BindingDecl]
elaborate gti bindingDecls =
  mapM (elabBindingDecl gti) bindingDecls

elabBindingDecl :: Monad m => GlobalTypeInfo -> BindingDecl -> m BindingDecl
elabBindingDecl gti (Binding name ty expr) = do
  elab_expr <- tychkExpr gti emptyEnv clientLoc expr ty
  return (Binding name ty elab_expr)

elabExpr :: Monad m =>
  GlobalTypeInfo -> Env -> Location -> Expr -> Type -> m Expr
elabExpr gti env loc (Var x) ty
  | isBindingName x =        -- if it is a term variable
  case lookupEnv x env of    -- try to find it in the local var env or
    (x_ty:_) ->
      if equalType x_ty ty
      then return (Var x)
      else error $ "[TypeCheck] Incorrect type for local var " ++ x
    [] ->
      case lookupBindingTypeInfo (_bindingTypeInfo gti) x of
        (g_ty:_) ->          -- try to find it in the global bindings
          if equalType g_ty ty
          then return (Var x)
          else error $ "[TypeCheck] Incorrect type for local var " ++ x
        
  | isConstructorName x =    -- if it is a constructor
      case lookupConTypeInfo (_conTypeInfo gti) x  of
        (([], name, tyvars):_) ->
          if unifiableType (ConType name tyvars) ty
          then return (Constr x [])
          else error $ "[TypeCheck] Incorrect type for constructor " ++ x
        ((_, name, tyvars):_) ->
          error $ "[TypeCheck] Incorrect arguments for constructor " ++ x
        [] -> error $ "[TypeCheck] Not found constructor " ++ x
  
elabExpr gti env loc (TypeAbs tyvars expr) (TypeAbsType tyvars' ty)
  | tyvars == tyvars' = do
      let typeVarEnv = _typeVarEnv gti
      let typeVarEnv' = reverse tyvars ++ typeVarEnv
      elabExpr gti (env{_typeVarEnv=typeVarEnv'}) loc expr ty
  | otherwise =
      error $ "[TypeCheck] different tyvar names in TypeAbs: "
                 ++ tyvars ++ " != " += tyvars'
elabExpr gti env loc (TypeAbs tyvars expr) ty =
  error $ "[TypeCheck] Incorrect type for TypeAbs"

elabExpr gti env loc (LocAbs locvars expr) (LocAbsType locvars' ty)
  | locvars == locvars' = do
      let locVarEnv = _locVarEnv gti
      let locVarEnv' = reverse locvars ++ locVarEnv
      elabExpr gti (env{_locVarEnv=locVarEnv'}) loc expr ty
  | otherwise =
      error $ "[TypeCheck] different locvar names in LocAbs: "
                 ++ locvars ++ " != " += locvars'
elabExpr gti env loc (LocAbs locvars expr) ty =
  error $ "[TypeCheck] Incorrect type for LocAbs"

elabExpr gti env loc_0 (Abs ((var,loc):[]) expr) (FunType argty loc1 retty) =
  | loc == loc1 = do
      let varEnv = _varEnv gti
      let varEnv' = (var,argty):varEnv
      elabExpr gti (env{_varEnv=varEnv'}) loc expr retty
  | otherwise = error $ "[TypeCheck] Incorrect location for Abs " ++ var
                            ++ ": " ++ loc ++ "!=" ++ loc1

elabExpr gti env loc_0 (Abs ((var,loc):varLocList) expr) (FunType argty loc1 retty) =
  | loc == loc1 = do
      let varEnv = _varEnv gti
      let varEnv' = (var,argty):varEnv
      elabExpr gti (env{_varEnv=varEnv'}) loc (Abs varLocList expr) retty
  | otherwise = error $ "[TypeCheck] Incorrect location for Abs " ++ var
                            ++ ": " ++ loc ++ "!=" ++ loc1

elabExpr gti env loc_0 (Abs varLocList expr) ty =
  error $ "[TypeCheck] Incorrect type for Abs " ++ show varLocList

elabExpr gti env loc_0 (Let letBindingDecls expr) ty = do
  letBindingTypeInfo <- allBindingDecls 

!!!HEREHERE!!!

