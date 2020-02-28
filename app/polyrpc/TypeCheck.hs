module TypeCheck where

import Type

typeCheck :: Monad m => [TopLevelDecl] -> m ([DataTypeDecl], [BindingDecl])
typeCheck toplevelDecls = do
  -- 1. split
  (bindingDecls, userDatatypes) <- splitTopLevelDecls toplevelDecls

  let datatypeDecls = builtinDatatypes ++ userDatatypes

  -- 2. collect all types, builtin or user-defined ones
  typeInfo <- collectDataTypeDecls datatypeDecls
  
  -- 3. elaborate data types
  elab_datatypeDecls <- elabDataTypeDecls typeInfo datatypeDecls
  dataTypeInfo <- collectDataTypeInfo elab_datatypeDecls
  
  -- 4. elaborate constructor types
  conTypeinfo <- elabConTypeDecls elab_datatypeDecls
  
  -- 5. elaborate types declared in the bindings
  partial_elab_bindingDecls <- elabBindingTypes typeInfo bindingDecls
  
  bindingTypeInfo <- bindingTypes partial_elab_bindingDecls
                          
  -- 6. elaborate bindings
  let gti = GlobalTypeInfo
              { _typeInfo=typeInfo
	      , _conTypeInfo=conTypeInfo
	      , _dataTypeInfo=dataTypeInfo
              , _bindingTypeInfo=bindingTypeInfo }
            
  elab_bindingDecls <- elaborate gti partial_elab_bindingDecls

  -- 7. return elaborated data types and bindings
  return (elab_datatypeDecls, elab_bindingDecls)

----------------------------------------------------------------------------
-- 1. Split toplevel declarations into datatypes and bindings
----------------------------------------------------------------------------

splitTopLevelDecls :: Monad m =>
  [TopLevelDecl] -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecls toplevelDecls =
  (\p -> (concat (fst p), concat (snd p)))
    $ unzip $ map splitTopLevelDecl $ toplevelDecls

splitTopLevelDecl :: Monad m =>
  TopLevelDecl -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecl (BindingTopLevel bindingDecl)   = return ([bindingDecl], [])
splitTopLevelDecl (DataTypeTopLevel datatypeDecl) = return ([], [datatypeDecl])


----------------------------------------------------------------------------
-- 2. Collect bultin types and user-defined datatyps
----------------------------------------------------------------------------

type TypeInfo = [(String, [String])]

lookupTypeCon :: Monad m => TypeInfo -> String -> m [String]
lookupTypeCon typeInfo x = do
  let found = [tyvars | (name, tyvars) <- typeInfo, x==name]
  if found /= [] 
    then return (head found)
    else error $ "lookupConstr: Not found construct : " ++ x 

builtinDatatypes :: [DataTypeDecl]
builtinDatatypes = [
    (DataType unitType   [] []), -- data Unit
    (DataType intType    [] []), -- data Int
    (DataType boolType   []      -- data Bool = { True | False }
      [ TypeCon trueLit  []
      , TypeCon falseLit [] ]), 
    (DataType stringType [] [])  -- data String
  ]
  

collectDataTypeDecls :: Monad m => [DataTypeDecl] -> m TypeInfo
collectDataTypeDecls datatypeDecls = do
  let nameTyvarsPairList = map collectDataTypeDecl datatypeDecls
  return nameTyvarsPairList

collectDataTypeDecl (DataType name tyvars typeConDecls) =
  if isTypeName name && and (map isTypeVarName tyvars) && allUnique tyvars == []
  then (name, tyvars)
  else error $ "[TypeCheck] collectDataTypeDecls: Invalid names: "
                 ++ name ++ " " ++ show tyvars

----------------------------------------------------------------------------
-- 3. Elaboration of datatype declarations
--  by elaborating Int as an identifier into ConType Int [],
--     checking duplicate type variables in each datatype declaration, and
--     checking duplicate constructor names in all datatype declarations.
----------------------------------------------------------------------------

elabDataTypeDecls :: Monad m => TypeInfo -> m [DataTypeDecl]
elabDataTypeDecls typeInfo datatypeDecls =
  mapM (elabDataTypeDecl typeInfo) datatypeDecls

elabDataTypeDecl :: Monad m => TypeInfo -> m DataTypeDecl
elabDataTypeDecl typeInfo (DataType name tyvars typeConDecls) = do
  elab_typeConDecls <- mapM (elabTypeConDecl typeInfo tyvars) typeConDecls
  return (DataType name tyvars elab_typeConDecls)

elabTypeConDecl :: Monad m =>
  GlobalTypeInfo -> [String] -> TypeConDecl -> m TypeConDecl
elabTypeConDecl typeInfo tyvars (ConDecl con tys) = do
  elab_tys <- mapM (elabType typeInfo tyvars) tys
  return (ConDecl con elab_tys)

----------------------------------------------------------------------------
-- 4. Elaboration of constructor types
----------------------------------------------------------------------------

type ConTypeInfo = [(String, ([Type], String, [String]))]

lookupConstr :: GlobalTypeInfo -> String -> [([Type], String, [String])]
lookupConstr gti x = [z | (con, z) <- _conTypeInfo gti, x==con]

elabConTypeDecls :: Monad m => [DataTypeDecl] -> m ConTypeInfo
elabConTypeDecls elab_datatypeDecls = do
  conTypeInfo <- concatM (mapM elabConTypeDecl elab_datatypeDecls)
  case allUnique [con | (con,_) <- conTypeInfo] of
    [] -> return conTypeInfo
    (con:_) -> error $ "allConTypeDecls: duplicate constructor: " ++ con

elabConTypeDecl (DataType name tyvars typeConDecls) = do
  return [ (con, (tys, name tyvars)) | ConDecl con tys <- typeConDecls ]

----------------------------------------------------------------------------
-- 5. Elaboration of types declared in bindings
----------------------------------------------------------------------------

type BindingTypeInfo = [(String, Type)]

elabBindingTypes :: Monad m => TypeInfo -> [BindingDecl] -> m [BindingDecl]
elabBindingTypes typeInfo bindingDecls =
  mapM (\(Binding f ty expr)-> do
           elab_ty <- elabType typeInfo [] ty
           return (f,elab_ty,expr))

bindingTypes :: Monad m => [BindingDecl] -> m [(String,Type)]
bindigTypes partial_elab_bindingDecls =
  mapM (\(f,ty,_) -> (f,ty)) partial_elab_bindingDecls

----------------------------------------------------------------------------
-- 6. Elaboration of bindings
----------------------------------------------------------------------------

data GlobalTypeInfo = GlobalTypeInfo
       { _typeInfo :: TypeInfo
       , _conTypeInfo :: ConTypeInfo
       , _dataTypeInfo :: DataTypeInfo
       , _bindingTypeInfo :: BindingTypeInfo }

elaborate :: Monad m => GlobalTypeInfo -> [BindingDecl] -> m [BindingDecl]
elaborate gti bindingDecls =
  mapM (elabBindingDecl gti) bindingDecls

elabBindingDecl :: Monad m => GlobalTypeInfo -> BindingDecl -> m BindingDecl
elabBindingDecl gti (Binding name ty expr) = do
  let env = emptyEnv{_varEnv=_bindingTypeInfo gti}
  (elab_expr,elab_ty) <- elabExpr gti env clientLoc expr
  if equalType elab_ty ty
  then return (Binding name ty elab_expr)
  else error $ "[TypeCheck] elabBindingDecl: Incorrect types: " ++ name

----------------------------------------------------------------------------
-- [Common] Elaboration of types
----------------------------------------------------------------------------
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

----------------------------------------------------------------------------
-- [Common] Elaboration of expressions
----------------------------------------------------------------------------

data Env = Env
       { _locVarEnv  :: [String]
       , _typeVarEnv :: [String]
       , _varEnv     :: BindingTypeInfo }

emptyEnv = {_varEnv=[], _locVarEnv=[], _typeVarEnv=[]}

lookupVar :: Env -> String -> Type
lookupVar env x = [ty | (y,ty) <- _varEnv env, x==y]

lookupLocVar :: Env -> String -> Bool
lookupLocVar env x = elem x (_locVarEnv env

lookupTypeVar :: Env -> String -> Bool
lookupTypeVar env x = elem x (_typeVarEnv env)

--
type DataTypeInfo = [(String, (String, [TypeConDecl]))]

lookupDataTypeName gti x = [info | (y,info) <- _dataTypeInfo gti, x==y]

collectDataTypeInfo datatypeDecls = do
  mapM get datatypeDecls
  where get (DataType name tyvars tycondecls) = (name, (tyvar,tycondecls))

--
elabExpr :: Monad m =>
  GlobalTypeInfo -> Env -> Location -> Expr -> m (Expr, Type)
elabExpr gti env loc (Var x)
  | isBindingName x =        -- if it is a term variable
  case lookupVar env x of    -- try to find it in the local var env or
    (x_ty:_) -> return (Var x, x_ty)
    [] -> error $ "[TypeCheck] Not found constructor " ++ x
        
  | isConstructorName x =    -- if it is a constructor
      case lookupConstr gti x  of
        (([], tyname, []):_) -> return (Constr x [], ConType tyname [])
	
        (([], tyname, tyvars):_) ->
          return (TypeAbs tyvars (Const x [])
	         , TypeAbsType tyvars (ConType tyname tyvars))
		 
        ((tys, tyname, []):_) ->
	  let vars = ["arg"++show i | i<-[1..]]
	  let locs = loc : locs
	  let varTypeLocList = zip3 vars tys locs
	  let finaltype = foldr (\((ty,loc) ty0)-> FunType ty loc ty0)
	                    (ConType tyname []) (zip tys locs)
          return (Abs varTypeLocList (Const x vars), finaltype)
	  
        ((tys, tyname, tyvars):_) ->
	  let vars = ["arg"++show i | i<-[1..]]
	  let locs = loc : locs
	  let varTypeLocList = zip3 vars tys locs
	  let finaltype = foldr (\((ty,loc) ty0)-> FunType ty loc ty0)
	                    (ConType tyname tyvars) (zip tys locs)
          return (Abs varTypeLocList (Const x vars)
	         , TypeAbsType tyvars finaltype)
	
        [] -> error $ "[TypeCheck] elabExpr: Not found constructor " ++ x
  
elabExpr gti env loc (TypeAbs tyvars expr) = do
  let typeVarEnv = _typeVarEnv gti
  let typeVarEnv' = reverse tyvars ++ typeVarEnv
  (elab_expr, elab_ty) <- elabExpr gti (env{_typeVarEnv=typeVarEnv'}) loc expr
  return (TypeAbs tyvars elab_expr, TypeAbsType tyvars elab_ty)

elabExpr gti env loc (LocAbs locvars expr) = do
  let locVarEnv = _locVarEnv gti
  let locVarEnv' = reverse locvars ++ locVarEnv
  (elab_expr, elab_ty) <- elabExpr gti (env{_locVarEnv=locVarEnv'}) loc expr
  return (LocAbs locvars elab_expr, LocAbsType locvars elab_ty)

elabExpr gti env loc_0 (Abs [(var,argty,loc)] expr)  = do
  let varEnv = _varEnv env
  let varEnv' = (var,argty):varEnv
  (elab_expr, ret_ty) <- elabExpr gti (env{_varEnv=varEnv'}) loc expr
  return (Abs [(var,argty,loc)] elab_expr, FunType argty loc ret_ty)

elabExpr gti env loc_0 (Abs ((var,argty,loc):varTypeLocList) expr)  = do
  let varEnv = _varEnv env
  let varEnv' = (var,argty):varEnv
  (elab_expr, ret_ty) <-
    elabExpr gti (env{_varEnv=varEnv'}) loc (Abs varLocTypeList expr)
  return (Abs [(var,argty,loc)] elab_expr, FunType argty loc ret_ty)

elabExpr gti env loc_0 (Abs [] expr)  =
  error $ "[TypeCheck] elabExpr: empty argument Abs"

elabExpr gti env loc (Let letBindingDecls expr) = do
  let typeInfo = _typeInfo gti
  partial_elab_letBindingDecls <- elabBindingTypes typeInfo letBindingDecls
  letBindingTypeInfo <- bindingTypes partial_elab_letBindingDecls
 
  let letBindingTypeInfo = letBindingTypeInfo ++ _bidningTypeInfo gti
  let gti1 = gti {_bindingTypeInfo=letBindingTypeInfo}
  elab_letBindingDecls <- elaborate gti1 partial_elab_letBindingDecls

  let varEnv = letBindingTypeInfo ++ _varEnv gti
  (elab_expr, elab_ty) <- elabExpr gti (env {_varEnv=varEnv}) expr
  return (Let elab_letBindingDecls elab_expr, elab_ty)

elabExpr gti env loc (Case expr []) =
  error $ "[TypeCheck] empty alternatives"

elabExpr gti env loc (Case expr alts) = do
  (elab_caseexpr, casety) <- elabExpr gti env loc expr
  case casety of
    ConType tyconName tys ->
      case lookupDataTypeName tyconName of
        ((tyvars, tycondecls):_) -> do
          (elab_alts, altty) <- elabAlts gti evn loc tys tyvars tycondecls alts
          return (Case elab_expr elab_alts, altty)
        [] -> error $ "[TypeCheck] elabExpr: invalid constructor type: " ++ tyconName
    _ -> error $ "[TypeCheck] elabExpr: case expr not constructor type"

elabExpr gti env loc (App left_expr right_expr _) = do
  (elab_left_expr, left_ty) <- elabExpr gti env loc left_expr
  (elab_right_expr, right_ty) <- elabExpr gti env loc right_expr
  case elab_left_ty of
    FunType argty loc0 retty ->
      if equalType argty right_ty
      then return (App elab_left_expr elab_right_expr (Just loc0), retty)
      else error $ "[TypeCheck] elabExpr: not equal arg type in app: "
    _ -> error $ "[TypeCheck] elabExpr: not function type in app: "

elabExpr gti env loc (TypeApp expr tys) = do
  (elab_expr, elab_ty) = elabExpr gti env loc expr expr
  case elab_ty of
    TypeAbsType tyvars ty0 ->
      if length tyvars == length tys
      then return (TypeApp elab_expr tys, doSubst (zip tyvars tys) ty0)
      else error $ "[TypeCheck] elabExpr: not equal length of arg types in type app: "
    _ -> error $ "[TypeCheck] elabExpr: not type-abstraction type in type app: "

elabExpr gti env loc (LocApp expr locs) = do
  (elab_expr, elab_ty) = elabExpr gti env loc expr expr
  case elab_ty of
    LocAbsType locvars ty0 ->
      if length locvars == length tys
      then return (TypeApp elab_expr tys, doSubstLoc (zip locvars locs) ty0)
      else error $ "[TypeCheck] elabExpr: not equal length of arg locations in location app: "
    _ -> error $ "[TypeCheck] elabExpr: not location-abstraction type in type app: "

elabExpr gti env loc (Tuple exprs) = do
  elabExprTyList <- mapM (elabExpr gti env loc) exprs
  let (elab_exprs, tys) = unzip elabExprTyList
  return (Tuple elab_exprs, TupleType tys)

elabExpr gti env loc (Prim op exprs) = do
  elabExprTyList <- mapM (elabExpr gti env loc) exprs
  let (elab_exprs, tys) = unzip elabExprTyList
  case lookupPrimOpType of
    ((argtys, retty):_) -> do
      if length tys==length argtys && and (map equalType (zip argtys tys))
      then return (Prim op elab_exprs, retty)
      else error $ "[TypeCheck] elabExpr: incorrect arg types in Prim op: "
    [] -> error $ "[TypeCheck] elabExpr: type not found type in Prim op: "

elabExpr git env loc (Lit literal) = return (Lit literal, typeOfLiteral literal)

elabExpr git env loc (Const conname exprs) = do
  elabExprTyList <- mapM (elabExpr git env loc) exprs
  let (elab_exprs, tys) = unzip elabExprTyList
  case lookupConstr gti conname of
    ((argtys,tyname,tyvars):_) ->
      case unifyTypes argtys tys of
        Just subst -> return (Const conname elab_exprs, doSubst subst (ConType tyname tyvars))
        Nothing -> error $ "[TypeCheck] elabExpr: constructor arg types incorrect: " ++ conname
            
    [] -> error $ "[TypeCheck] elabExpr: constructor not found: " ++ conname

--
elabAlts gti env loc tys tyvars tycondecls [alt] = do
  let subst = zip tyvars tys
  (elab_alt, elab_ty) <- elabAlt gti env loc subst tycondecls alt
  return ([elab_alt], elab_ty)
  
elabAlts gti env loc tys tyvars tycondecls (alt@(Alternatvie con args _):alts) = do
  let subst = zip tyvars tys
  (elab_alt, elab_ty1)  <- elabAlt gti env loc subst tycondecls alt
  (elab_alts, elab_ty2) <- elabAlts gti env loc tys tycondecls alts
  if equalType elab_ty1 elab_ty2
  then return (elab_alt:elab_alts, elab_ty1)
  else error $ "[TypeCheck] elabAlts: not equal alt type: " ++ con ++ show args

lookupCon tycondecls con =
  [tys | TypeCon conname tys <- tycondecls, con==conname]

elabAlt gti env loc subst tycondecls (Alternative con args expr) = do
  case lookupCon tycondecls con of
    (tys:_) -> 
      if length tys==length args
      then do let tys' = map (doSubst subst) tys
              let varEnv = _varEnv env
              let varEnv' = zip args tys' ++ varEnv
              (elab_expr, elab_ty) <- elabExpr gti (env {_varEnv=varEnv'}) loc expr
	      return (Alternative con args elab_expr, elab_ty)
      else error $ "[TypeCheck] elabAlt: invalid arg length: " ++ con ++ show args
      
    [] -> error $ "[TypeCheck] elabAlt: constructor not found"



----------------------------------------------------------------------------
-- Common Utils
----------------------------------------------------------------------------
allUnique [] = []
allUnique (x:xs) =
  if elem x xs then [x] else allUnique xs

