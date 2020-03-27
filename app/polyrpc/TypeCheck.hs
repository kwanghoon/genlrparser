module TypeCheck where

import Location
import Type
import Expr
import BasicLib

typeCheck :: Monad m => [TopLevelDecl] -> m [TopLevelDecl]
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
  conTypeInfo <- elabConTypeDecls elab_datatypeDecls
  
  -- 5. elaborate types declared in the bindings
  partial_elab_bindingDecls <- elabBindingTypes typeInfo bindingDecls
  
  bindingTypeInfo <- bindingTypes partial_elab_bindingDecls
                          
  -- 6. elaborate bindings
  let gti = GlobalTypeInfo
              { _typeInfo=typeInfo
              , _conTypeInfo=conTypeInfo
              , _dataTypeInfo=dataTypeInfo
              , _bindingTypeInfo=basicLib ++ bindingTypeInfo }
            
  elab_bindingDecls <- elaborate gti partial_elab_bindingDecls

  -- 7. return elaborated data types and bindings
  let elab_toplevels = [ LibDeclTopLevel x ty | (x,ty) <- basicLib]
                       ++ [ DataTypeTopLevel dt | dt <- elab_datatypeDecls]
                       ++ [ BindingTopLevel bd | bd <- elab_bindingDecls]
  
  return elab_toplevels

----------------------------------------------------------------------------
-- 1. Split toplevel declarations into datatypes and bindings
----------------------------------------------------------------------------

splitTopLevelDecls :: Monad m =>
  [TopLevelDecl] -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecls toplevelDecls = do
  bindingsDatatypeList <- mapM splitTopLevelDecl toplevelDecls
  let (bindings,datatypes) = unzip bindingsDatatypeList
  return (concat bindings, concat datatypes)

splitTopLevelDecl :: Monad m =>
  TopLevelDecl -> m ([BindingDecl], [DataTypeDecl])
splitTopLevelDecl (BindingTopLevel bindingDecl)   = return ([bindingDecl], [])
splitTopLevelDecl (DataTypeTopLevel datatypeDecl) = return ([], [datatypeDecl])


----------------------------------------------------------------------------
-- 2. Collect bultin types and user-defined datatyps
----------------------------------------------------------------------------

-- type TypeInfo = [(String, [String])]

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

elabDataTypeDecls :: Monad m => TypeInfo -> [DataTypeDecl] -> m [DataTypeDecl]
elabDataTypeDecls typeInfo datatypeDecls =
  mapM (elabDataTypeDecl typeInfo) datatypeDecls

elabDataTypeDecl :: Monad m => TypeInfo -> DataTypeDecl -> m DataTypeDecl
elabDataTypeDecl typeInfo (DataType name tyvars typeConDecls) = do
  elab_typeConDecls <- mapM (elabTypeConDecl typeInfo tyvars) typeConDecls
  return (DataType name tyvars elab_typeConDecls)

elabTypeConDecl :: Monad m => TypeInfo -> [String] -> TypeConDecl -> m TypeConDecl
elabTypeConDecl typeInfo tyvars (TypeCon con tys) = do
  elab_tys <- mapM (elabType typeInfo tyvars []) tys
  return (TypeCon con elab_tys)

----------------------------------------------------------------------------
-- 4. Elaboration of constructor types
----------------------------------------------------------------------------

-- type ConTypeInfo = [(String, ([Type], String, [String]))]

lookupConstr :: GlobalTypeInfo -> String -> [([Type], String, [String])]
lookupConstr gti x = [z | (con, z) <- _conTypeInfo gti, x==con]

elabConTypeDecls :: Monad m => [DataTypeDecl] -> m ConTypeInfo
elabConTypeDecls elab_datatypeDecls = do
  conTypeInfoList <- mapM elabConTypeDecl elab_datatypeDecls
  let conTypeInfo = concat conTypeInfoList
  case allUnique [con | (con,_) <- conTypeInfo] of
    [] -> return conTypeInfo
    (con:_) -> error $ "allConTypeDecls: duplicate constructor: " ++ con

elabConTypeDecl :: Monad m => DataTypeDecl -> m ConTypeInfo
elabConTypeDecl (DataType name tyvars typeConDecls) = do
  return [ (con, (tys, name, tyvars)) | TypeCon con tys <- typeConDecls ]

----------------------------------------------------------------------------
-- 5. Elaboration of types declared in bindings
----------------------------------------------------------------------------

-- type BindingTypeInfo = [(String, Type)]

elabBindingTypes :: Monad m => TypeInfo -> [BindingDecl] -> m [BindingDecl]
elabBindingTypes typeInfo bindingDecls =
  mapM (\(Binding f ty expr)-> do
           elab_ty <- elabType typeInfo [] [] ty
           return (Binding f elab_ty expr)) bindingDecls

bindingTypes :: Monad m => [BindingDecl] -> m [(String,Type)]
bindingTypes partial_elab_bindingDecls =
  mapM (\(Binding f ty _) -> return (f,ty)) partial_elab_bindingDecls

----------------------------------------------------------------------------
-- 6. Elaboration of bindings
----------------------------------------------------------------------------

-- data GlobalTypeInfo = GlobalTypeInfo
--        { _typeInfo :: TypeInfo
--        , _conTypeInfo :: ConTypeInfo
--        , _dataTypeInfo :: DataTypeInfo
--        , _bindingTypeInfo :: BindingTypeInfo }

elaborate :: Monad m => GlobalTypeInfo -> [BindingDecl] -> m [BindingDecl]
elaborate gti bindingDecls =
  mapM (elabBindingDecl gti) bindingDecls

elabBindingDecl :: Monad m => GlobalTypeInfo -> BindingDecl -> m BindingDecl
elabBindingDecl gti (Binding name ty expr) = do
  let env = emptyEnv{_varEnv=_bindingTypeInfo gti}
  (elab_expr,elab_ty) <- elabExpr gti env clientLoc expr
  if equalType elab_ty ty
  then return (Binding name ty elab_expr)
  else error $ "[TypeCheck] elabBindingDecl: Incorrect types: " ++ name ++ "\n" ++ show elab_ty ++ "\n" ++ show ty

----------------------------------------------------------------------------
-- [Common] Elaboration of types
----------------------------------------------------------------------------
elabType :: Monad m => TypeInfo -> [String] -> [String] -> Type -> m Type
elabType typeInfo tyvars locvars (TypeVarType x) = do
  if elem x tyvars then return (TypeVarType x)
  else if isConstructorName x then
          do _tyvars <- lookupTypeCon typeInfo x
             if _tyvars == []
             then return (ConType x [])
             else error $ "[TypeCheck]: elabType: Invalid type constructor: " ++ x
       else
          error $ "[TypeCheck] elabType: Not found: " ++ x ++ " in " ++ show tyvars

elabType typeInfo tyvars locvars (TupleType tys) = do
  elab_tys <- mapM (elabType typeInfo tyvars locvars) tys
  return (TupleType elab_tys)

elabType typeInfo tyvars locvars (FunType ty1 (Location loc) ty2) = do
  elab_ty1 <- elabType typeInfo tyvars locvars ty1
  elab_ty2 <- elabType typeInfo tyvars locvars ty2
  let loc0 = if loc `elem` locvars
             then LocVar loc else Location loc
  return (FunType elab_ty1 loc0 elab_ty2)
  
elabType typeInfo tyvars locvars (FunType ty1 (LocVar _) ty2) =
  error $ "[TypeCheck] elabType: FunType: LocVar"

elabType typeInfo tyvars locvars (TypeAbsType abs_tyvars ty) = do
  elab_ty <- elabType typeInfo (tyvars ++ abs_tyvars) locvars ty
  return (TypeAbsType abs_tyvars elab_ty)

elabType typeInfo tyvars locvars (LocAbsType abs_locvars ty) = do
  elab_ty <- elabType typeInfo tyvars (locvars ++ abs_locvars) ty
  return (LocAbsType abs_locvars elab_ty)

elabType typeInfo tyvars locvars (ConType name tys) = do
  _tyvars <- lookupTypeCon typeInfo name
  if length _tyvars == length tys
    then do elab_tys <- mapM (elabType typeInfo tyvars locvars) tys
            return (ConType name elab_tys)
    else error $ "[TypeCheck]: elabType: Invalud args for ConType: " ++ name

elabType typeInfo tyvars locvars (RefType (Location loc) ty) = do
  elab_ty <- elabType typeInfo tyvars locvars ty
  let loc0 = if loc `elem` locvars then LocVar loc else Location loc
  return (RefType loc0 elab_ty)


elabLocation :: Monad m => [String] -> Location -> m Location
elabLocation locvars (Location loc)
  | loc `elem` locvars = return (LocVar loc)
  | otherwise = return (Location loc)
elabLocation locvars (LocVar x)
  | x `elem` locvars = return (LocVar x)
  | otherwise = error $ "[TypeCheck] elabLocation: Not found LocVar " ++ x

----------------------------------------------------------------------------
-- [Common] Elaboration of expressions
----------------------------------------------------------------------------

-- data Env = Env
--        { _locVarEnv  :: [String]
--        , _typeVarEnv :: [String]
--        , _varEnv     :: BindingTypeInfo }

emptyEnv = Env {_varEnv=[], _locVarEnv=[], _typeVarEnv=[]}

lookupVar :: Env -> String -> [Type]
lookupVar env x = [ty | (y,ty) <- _varEnv env, x==y]

lookupLocVar :: Env -> String -> Bool
lookupLocVar env x = elem x (_locVarEnv env)

lookupTypeVar :: Env -> String -> Bool
lookupTypeVar env x = elem x (_typeVarEnv env)

--
-- type DataTypeInfo = [(String, ([String], [(String,[Type])]))]

lookupDataTypeName gti x = [info | (y,info) <- _dataTypeInfo gti, x==y]

collectDataTypeInfo :: Monad m => [DataTypeDecl] -> m DataTypeInfo
collectDataTypeInfo datatypeDecls = do
  mapM get datatypeDecls
  where get (DataType name tyvars tycondecls) =
          return (name, (tyvars,map f tycondecls))
        f (TypeCon s tys) = (s,tys)

--
elabExpr :: Monad m =>
  GlobalTypeInfo -> Env -> Location -> Expr -> m (Expr, Type)
elabExpr gti env loc (Var x)
  | isConstructorName x =    -- if it is a constructor
      case lookupConstr gti x  of
        (([], tyname, []):_) -> return (Constr x [] [], ConType tyname [])
        
        (([], tyname, tyvars):_) ->
          return (singleTypeAbs (TypeAbs tyvars (Constr x (map TypeVarType tyvars) []))
                 , singleTypeAbsType (TypeAbsType tyvars (ConType tyname (map TypeVarType tyvars))))
                 
        ((tys, tyname, []):_) -> do
          let vars = take (length tys) ["arg"++show i | i<-[1..]]
          let locs = loc : locs
          let varTypeLocList = zip3 vars tys locs
          let finaltype = foldr (\(ty,loc) ty0 -> FunType ty loc ty0)
                            (ConType tyname []) (zip tys locs)
          return (singleAbs (Abs varTypeLocList (Constr x [] (map Var vars))), finaltype)
          
        ((tys, tyname, tyvars):_) -> do
          let vars = take (length tys) ["arg"++show i | i<-[1..]]
          let locs = loc : locs
          let varTypeLocList = zip3 vars tys locs
          let finaltype = foldr (\(ty,loc) ty0 -> FunType ty loc ty0)
                            (ConType tyname (map TypeVarType tyvars)) (zip tys locs)
          return (singleTypeAbs (TypeAbs tyvars
                  (singleAbs (Abs varTypeLocList
                   (Constr x (map TypeVarType tyvars) (map Var vars)))))
                 , singleTypeAbsType (TypeAbsType tyvars finaltype))
        
        [] -> error $ "[TypeCheck] elabExpr: Not found constructor " ++ x
  
  | otherwise =    --  isBindingName x =        -- if it is a term variable
  case lookupVar env x of    -- try to find it in the local var env or
    (x_ty:_) -> return (Var x, x_ty)
    [] -> error $ "[TypeCheck] Not found constructor " ++ x
        
elabExpr gti env loc (TypeAbs tyvars expr) = do
  let typeVarEnv = _typeVarEnv env
  let typeVarEnv' = reverse tyvars ++ typeVarEnv
  (elab_expr, elab_ty) <- elabExpr gti (env{_typeVarEnv=typeVarEnv'}) loc expr
  return (singleTypeAbs (TypeAbs tyvars elab_expr), singleTypeAbsType (TypeAbsType tyvars elab_ty))

elabExpr gti env loc (LocAbs locvars expr) = do
  let locVarEnv = _locVarEnv env
  let locVarEnv' = reverse locvars ++ locVarEnv
  (elab_expr, elab_ty) <- elabExpr gti (env{_locVarEnv=locVarEnv'}) loc expr
  return (singleLocAbs (LocAbs locvars elab_expr), singleLocAbsType (LocAbsType locvars elab_ty))

elabExpr gti env loc_0 (Abs [(var,argty,loc)] expr)  = do
  elab_argty <- elabType (_typeInfo gti) (_typeVarEnv env) (_locVarEnv env) argty
  elab_loc <- elabLocation (_locVarEnv env) loc
  let varEnv = _varEnv env
  let varEnv' = (var,elab_argty):varEnv
  (elab_expr, ret_ty) <- elabExpr gti (env{_varEnv=varEnv'}) elab_loc expr
  return (Abs [(var,elab_argty,elab_loc)] elab_expr, FunType elab_argty elab_loc ret_ty)  

elabExpr gti env loc_0 (Abs ((var,argty,loc):varTypeLocList) expr)  = do
  elab_argty <- elabType (_typeInfo gti) (_typeVarEnv env) (_locVarEnv env) argty
  elab_loc <- elabLocation (_locVarEnv env) loc
  let varEnv = _varEnv env
  let varEnv' = (var,elab_argty):varEnv
  (elab_expr, ret_ty) <-
    elabExpr gti (env{_varEnv=varEnv'}) elab_loc (singleAbs (Abs varTypeLocList expr))
  return (Abs [(var,elab_argty,elab_loc)] elab_expr, FunType elab_argty elab_loc ret_ty)

elabExpr gti env loc_0 (Abs [] expr)  =
  error $ "[TypeCheck] elabExpr: empty argument Abs"

elabExpr gti env loc (Let letBindingDecls expr) = do
  let typeInfo = _typeInfo gti
  partial_elab_letBindingDecls <- elabBindingTypes typeInfo letBindingDecls
  letBindingTypeInfo <- bindingTypes partial_elab_letBindingDecls
 
  let letBindingTypeInfo' = letBindingTypeInfo ++ _bindingTypeInfo gti
  let gti1 = gti {_bindingTypeInfo=letBindingTypeInfo'}
  elab_letBindingDecls <- elaborate gti1 partial_elab_letBindingDecls

  let varEnv = letBindingTypeInfo ++ _varEnv env
  (elab_expr, elab_ty) <- elabExpr gti (env {_varEnv=varEnv}) loc expr
  return (Let elab_letBindingDecls elab_expr, elab_ty)

elabExpr gti env loc (Case expr []) =
  error $ "[TypeCheck] empty alternatives"

elabExpr gti env loc (Case expr alts) = do
  (elab_caseexpr, casety) <- elabExpr gti env loc expr
  case casety of
    ConType tyconName tys ->
      case lookupDataTypeName gti tyconName of
        ((tyvars, tycondecls):_) -> do
          (elab_alts, altty) <- elabAlts gti env loc tys tyvars tycondecls alts
          return (Case elab_caseexpr elab_alts, altty)
        [] -> error $ "[TypeCheck] elabExpr: invalid constructor type: " ++ tyconName

    TupleType tys -> do
      (elab_alts, altty) <- elabAlts gti env loc tys [] [] alts
      return (Case elab_caseexpr elab_alts, altty)
    
    _ -> error $ "[TypeCheck] elabExpr: case expr not constructor type"

elabExpr gti env loc (App left_expr right_expr l) = do
  (elab_left_expr, left_ty) <- elabExpr gti env loc left_expr
  (elab_right_expr, right_ty) <- elabExpr gti env loc right_expr
  case left_ty of
    FunType argty loc0 retty ->
      if equalType argty right_ty
      then return (App elab_left_expr elab_right_expr (Just loc0), retty)
      else error $ "[TypeCheck] elabExpr: not equal arg type in app:\n" ++ show (App left_expr right_expr l) ++ "\n" ++ show argty ++ "\n" ++ show right_ty
    _ -> error $ "[TypeCheck] elabExpr: not function type in app:\n" ++ show (App left_expr right_expr l) ++ "\n" ++ show left_ty

elabExpr gti env loc (TypeApp expr tys) = do
  elab_tys <- mapM (elabType (_typeInfo gti) (_typeVarEnv env) (_locVarEnv env)) tys
  (elab_expr, elab_ty) <- elabExpr gti env loc expr
  case elab_ty of
    TypeAbsType tyvars ty0 ->
      if length tyvars == length elab_tys
      then return (singleTypeApp (TypeApp elab_expr elab_tys), doSubst (zip tyvars elab_tys) ty0)
      else error $ "[TypeCheck] elabExpr: not equal length of arg types in type app: "
    _ -> error $ "[TypeCheck] elabExpr: not type-abstraction type in type app: "

elabExpr gti env loc (LocApp expr locs) = 
  let f (Location loc0) = if loc0 `elem` (_locVarEnv env) then LocVar loc0 else Location loc0
      f (LocVar x)      = error $ "[TypeCheck] elabExpr: LocApp: LocVar: " ++ x
  in do
  let locs0 = map f locs
  (elab_expr, elab_ty) <- elabExpr gti env loc expr
  case elab_ty of
    LocAbsType locvars ty0 ->
      if length locvars == length locs
      then return (singleLocApp (LocApp elab_expr locs0), doSubstLoc (zip locvars locs0) ty0)
      else error $ "[TypeCheck] elabExpr: not equal length of arg locations in location app: " ++ show locvars ++ " " ++ show locs
    _ -> error $ "[TypeCheck] elabExpr: not location-abstraction type in type app: "

elabExpr gti env loc (Tuple exprs) = do
  elabExprTyList <- mapM (elabExpr gti env loc) exprs
  let (elab_exprs, tys) = unzip elabExprTyList
  return (Tuple elab_exprs, TupleType tys)

elabExpr gti env loc (Prim op exprs) = do
  elabExprTyList <- mapM (elabExpr gti env loc) exprs
  let (elab_exprs, tys) = unzip elabExprTyList
  case lookupPrimOpType op of
    ((argtys, retty):_) -> do
      if length tys==length argtys && and (map (uncurry equalType) (zip argtys tys))
      then return (Prim op elab_exprs, retty)
      else error $ "[TypeCheck] elabExpr: incorrect arg types in Prim op: "
    [] -> error $ "[TypeCheck] elabExpr: type not found type in Prim op: "

elabExpr gti env loc (Lit literal) = return (Lit literal, typeOfLiteral literal)

elabExpr gti env loc (Constr conname contys exprs) = do
  elab_contys <- mapM (elabType (_typeInfo gti) (_typeVarEnv env) (_locVarEnv env)) contys
  elabExprTyList <- mapM (elabExpr gti env loc) exprs
  let (elab_exprs, elab_tys) = unzip elabExprTyList
  case lookupConstr gti conname of
    ((argtys,tyname,tyvars):_) ->
      case unifyTypes argtys elab_tys of
        Just subst ->
          return (Constr conname elab_contys elab_exprs -- BUG??
                 , doSubst subst (ConType tyname (map TypeVarType tyvars)))
        Nothing -> error $ "[TypeCheck] elabExpr: constructor arg types incorrect: " ++ conname
            
    [] -> error $ "[TypeCheck] elabExpr: constructor not found: " ++ conname

-- elabExpr gti env loc expr = error $ "[TypeCheck] elabExpr: " ++ show expr

--
elabAlts gti env loc tys tyvars tycondecls [alt] = do
  let subst = zip tyvars tys
  (elab_alt, elab_ty) <- elabAlt gti env loc subst tycondecls tys alt
  return ([elab_alt], elab_ty)
  
elabAlts gti env loc tys tyvars tycondecls (alt:alts) = do
  let subst = zip tyvars tys
  (elab_alt, elab_ty1)  <- elabAlt gti env loc subst tycondecls tys alt
  (elab_alts, elab_ty2) <- elabAlts gti env loc tys tyvars tycondecls alts
  if equalType elab_ty1 elab_ty2
  then return (elab_alt:elab_alts, elab_ty1)
  else error $ "[TypeCheck] elabAlts: not equal alt type: " ++
                             (case alt of {
                               Alternative con args _ -> con ++ show args;
                               TupleAlternative args _ -> show args })

lookupCon tycondecls con =
  [tys | (conname, tys) <- tycondecls, con==conname]

elabAlt gti env loc subst tycondecls externTys (Alternative con args expr) = do
-- externTys only for TupleAlternative
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

elabAlt gti env loc subst tycondecls externTys (TupleAlternative args expr) = do
-- subst==[], tycondecls==[]
  let varEnv  = _varEnv env
  let varEnv' = zip args externTys ++ varEnv
  (elab_expr, elab_ty) <- elabExpr gti (env {_varEnv=varEnv'}) loc expr
  return (TupleAlternative args elab_expr, elab_ty)


----------------------------------------------------------------------------
-- Common Utils
----------------------------------------------------------------------------
allUnique [] = []
allUnique (x:xs) =
  if elem x xs then [x] else allUnique xs
