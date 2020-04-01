module Compile where

import Location

import qualified Type as ST
import qualified Expr as SE
import Literal

import qualified CSType as TT
import qualified CSExpr as TE

import Control.Monad

compile :: Monad m => SE.GlobalTypeInfo -> [SE.TopLevelDecl] ->
                      m (TE.GlobalTypeInfo, [TE.TopLevelDecl], TE.FunctionStore)
compile s_gti s_topleveldecls = do
  t_gti <- compileGTI s_gti
  (funStore, t_topleveldecls) <- compTopLevels t_gti TE.initFunctionStore s_topleveldecls
  return (t_gti, t_topleveldecls, funStore)

-----

--------------
-- Compile GTI
--------------
compileGTI :: Monad m => SE.GlobalTypeInfo -> m TE.GlobalTypeInfo
compileGTI (SE.GlobalTypeInfo
    { SE._typeInfo        = typeInfo,
      SE._conTypeInfo     = conTypeInfo,
      SE._dataTypeInfo    = dataTypeInfo,
      SE._bindingTypeInfo = bindingTypeInfo }) = do
  target_typeInfo <- compTypeInfo typeInfo
  target_conTypeInfo <- compConTypeInfo conTypeInfo
  target_dataTypeInfo <- compDataTypeInfo dataTypeInfo
  target_bindingTypeInfo <- compBindingTypeInfo bindingTypeInfo
  return (TE.GlobalTypeInfo
    { TE._typeInfo        = target_typeInfo,
      TE._conTypeInfo     = target_conTypeInfo,
      TE._dataTypeInfo    = target_dataTypeInfo,
      TE._bindingTypeInfo = target_bindingTypeInfo })

compTypeInfo :: Monad m => SE.TypeInfo -> m TE.TypeInfo
compTypeInfo typeInfo = return typeInfo

compConTypeInfo :: Monad m => SE.ConTypeInfo -> m TE.ConTypeInfo
compConTypeInfo conTypeInfo = mapM compConTypeInfo' conTypeInfo
  where
    compConTypeInfo' (cname, (argtys, dtname, locvars, tyvars)) = do
      target_argtys <- mapM compValType argtys
      return (cname, (target_argtys, dtname, locvars, tyvars))
      
compDataTypeInfo :: Monad m => SE.DataTypeInfo -> m TE.DataTypeInfo
compDataTypeInfo dataTypeInfo = mapM compDataTypeInfo' dataTypeInfo

compDataTypeInfo' (dtname, (locvars, tyvars, cnameArgtysList)) = do
  target_cnameArgtysList <- 
     mapM (\ (cname,argtys)-> do target_argtys <- mapM compValType argtys
                                 return (cname,target_argtys)) cnameArgtysList
  return (dtname, (locvars, tyvars, target_cnameArgtysList))

compBindingTypeInfo :: Monad m => SE.BindingTypeInfo -> m TE.BindingTypeInfo
compBindingTypeInfo bindingTypeInfo = mapM compBindingTypeInfo' bindingTypeInfo
  where
    compBindingTypeInfo' (x,ty) = do
      target_ty <- compValType ty
      return (x,target_ty)

----------------------
-- Compile value types
----------------------
compValType :: Monad m => ST.Type -> m TT.Type
compValType (ST.TypeVarType s) = return (TT.TypeVarType s)

compValType (ST.TupleType tys) = do
  t_tys <- mapM compValType tys
  return (TT.TupleType t_tys)
  
compValType (ST.FunType ty1 loc ty2) = do
  t_ty1 <- compValType ty1
  t_ty2 <- compType ty2
  return (TT.CloType (TT.FunType t_ty1 loc t_ty2))

compValType (ST.TypeAbsType alphas ty) = do
  t_ty <- compType ty
  return (TT.CloType (TT.TypeAbsType alphas t_ty))

compValType (ST.LocAbsType ls ty) = do
  t_ty <- compType ty
  return (TT.CloType (TT.LocAbsType ls t_ty))

compValType (ST.ConType s locs tys) = do
  t_tys <- mapM compValType tys
  return (TT.ConType s locs t_tys)

----------------------------
-- Compile computation types
----------------------------
compType :: Monad m => ST.Type -> m TT.Type
compType ty = do
  t_ty <- compValType ty
  return (TT.MonType t_ty)

--------------------
-- Compile toplevels
--------------------

compTopLevels :: Monad m => TE.GlobalTypeInfo -> TE.FunctionStore ->
                            [SE.TopLevelDecl] -> m (TE.FunctionStore, [TE.TopLevelDecl])
compTopLevels t_gti funStore [] = return (funStore, [])
compTopLevels t_gti funStore (toplevel:toplevels) = do
  (funStore1,t_toplevel) <- compTopLevel t_gti funStore toplevel
  (funStore2,t_toplevels) <- compTopLevels t_gti funStore1 toplevels
  return (funStore2, t_toplevel++t_toplevels)


compTopLevel :: Monad m => TE.GlobalTypeInfo -> TE.FunctionStore ->
                           SE.TopLevelDecl -> m (TE.FunctionStore, [TE.TopLevelDecl])
compTopLevel t_gti funStore (SE.LibDeclTopLevel x ty) = return (funStore, [])

compTopLevel t_gti funStore (SE.DataTypeTopLevel
               (SE.DataType dtname locvars tyvars tycondecls)) = return (funStore, [])

compTopLevel t_gti funStore (SE.BindingTopLevel bindingDecl) = do
  (funStore1, target_bindingDecl) <- compBindingDecl t_gti SE.initEnv clientLoc funStore bindingDecl
  return (funStore1, [TE.BindingTopLevel target_bindingDecl])

-------------------------------
-- Compile binding declarations
-------------------------------
--
-- Note: InterTE.Binding x ty expr as do x:ty <- expr
--
compBindingDecl :: Monad m =>
  TE.GlobalTypeInfo -> SE.Env -> Location ->
  TE.FunctionStore -> SE.BindingDecl -> m (TE.FunctionStore, TE.BindingDecl)
compBindingDecl t_gti env loc funStore (SE.Binding x ty expr) = do
  target_ty <- compValType ty
  (funStore1, target_val) <- compExpr t_gti env loc ty funStore expr 
  return (funStore1, TE.Binding x target_ty target_val)

-- partialCompBindingDecl :: Monad m => SE.BindingDecl -> m (String, TT.Type)
-- partialCompBindingDecl (SE.Binding x s_ty _) = do
--   target_ty <- compValType s_ty
--   return (x, target_ty)

-- compExpr
compExpr :: Monad m =>
  TE.GlobalTypeInfo -> SE.Env -> Location -> ST.Type ->
  TE.FunctionStore -> SE.Expr -> m (TE.FunctionStore, TE.Value)
compExpr t_gti env loc s_ty funStore (SE.Var x) = do
  (funStore1, target_var_x) <- compVal t_gti env loc s_ty funStore (SE.Var x)
  return (funStore1, TE.UnitM target_var_x)

compExpr t_gti env loc s_ty funStore (SE.TypeAbs tyvars expr) = do
  (funStore1, target_tyabs) <- compExpr t_gti env loc s_ty funStore (SE.TypeAbs tyvars expr)
  return (funStore1, TE.UnitM target_tyabs)

compExpr t_gti env loc s_ty funStore (SE.LocAbs tyvars expr) = do
  (funStore1, target_locabs) <- compExpr t_gti env loc s_ty funStore (SE.LocAbs tyvars expr)
  return (funStore1, TE.UnitM target_locabs)
  
compExpr t_gti env loc s_ty funStore (SE.Lit lit) = do
  (funStore1, target_lit) <- compVal t_gti env loc s_ty funStore (SE.Lit lit)
  return (funStore1, TE.UnitM target_lit)


-------------------------------------------
-- compVal: compilation of value expression
-------------------------------------------
compVal :: Monad m =>
  TE.GlobalTypeInfo -> SE.Env -> Location -> ST.Type ->
  TE.FunctionStore -> SE.Expr -> m (TE.FunctionStore, TE.Value)

-- Var
compVal t_gti env loc s_ty funStore (SE.Var x) = return (funStore, TE.Var x)

-- TypeAbs
compVal t_gti env loc (ST.TypeAbsType tyvars0 s_ty) funStore (SE.TypeAbs tyvars1 expr) = do
  -- Assume tyvars0 == tyvars1
  t_ty <- compValType s_ty
  let target_ty = TT.TypeAbsType tyvars0 t_ty
  let env1 = env {SE._typeVarEnv = tyvars1++SE._typeVarEnv env}
  (funStore1, target_expr) <- compExpr t_gti env1 loc s_ty funStore expr
  let opencode = TE.CodeTypeAbs tyvars1 target_expr

  mkClosure env loc funStore1 target_ty opencode

compVal t_gti env loc s_ty funStore (SE.TypeAbs tyvars1 expr) =
  error $ "[compVal] Not type-abstraction type: " ++ show s_ty

-- LocAbs
compVal t_gti env loc (ST.LocAbsType locvars0 s_ty) funStore (SE.LocAbs locvars1 expr) = do
  -- Assume tyvars0 == tyvars1
  t_ty <- compValType s_ty
  let target_ty = TT.LocAbsType locvars0 t_ty
  let env1 = env {SE._locVarEnv = locvars1++SE._locVarEnv env}
  (funStore1, target_expr) <- compExpr t_gti env1 loc s_ty funStore expr
  let opencode = TE.CodeLocAbs locvars1 target_expr

  mkClosure env loc funStore1 target_ty opencode

compVal t_gti env loc s_ty funStore (SE.LocAbs locvars1 expr) = do
  error $ "[compVal] Not location-abstraction type: " ++ show s_ty

-- Abs
compVal t_gti env loc (ST.FunType s_argty s_loc s_resty) funStore (SE.Abs xtylocs expr) = do
  -- Assume tyvars0 == tyvars1
  t_argty <- compValType s_argty
  t_resty <- compType s_resty
  let target_ty = TT.FunType t_argty s_loc t_resty
  let s_xtys = [(x,ty) | (x,ty,_) <- xtylocs]
  t_xtys <- mapM (\(x,ty) -> do { t_ty <- compValType ty; return (x,t_ty) }) s_xtys
  let env1 = env {SE._varEnv = (s_xtys ++ SE._varEnv env)}
  (funStore1, target_expr) <- compExpr t_gti env1 s_loc s_resty funStore expr
  let opencode = TE.CodeAbs t_xtys target_expr

  mkClosure env loc funStore1 target_ty opencode

compVal t_gti env loc s_ty funStore (SE.Abs xtylocs expr) =
  error $ "[compVal] Not abstraction type: " ++ show s_ty

-- Let
compVal t_gti env loc s_ty funStore (SE.Let bindingDecls expr) = do
  let bindingTypeInfo = [(x,ty) | SE.Binding x ty expr <- bindingDecls]
  let bindingTypeInfo1 = (bindingTypeInfo ++ SE._varEnv env)
  let env1 = env { SE._varEnv=bindingTypeInfo1 }
  (funStore2, t_bindingDecls) <-
    foldM (\(funStore0, bindingDecls0) -> \bindingDecl0 -> do
              (funStore1,bindingDecl1)
                 <- compBindingDecl t_gti env1 loc funStore0 bindingDecl0
              return (funStore1, bindingDecl1:bindingDecls0))
          (funStore, [])
          bindingDecls
  (funStore3, t_expr) <- compExpr t_gti env loc s_ty funStore2 expr
  return (funStore3, TE.BindM t_bindingDecls t_expr)

--Case
-- compVal t_gti env loc s_ty funStore (SE.Case expr alternatives) = do
--   (funStore1, target_expr) <- compExpr t_gti env loc s

-- Lit
compVal t_gti env loc s_ty funStore (SE.Lit lit) = return (funStore, TE.Lit lit)

--
-- Utility shared by compVal(SE.TypeAbs), compVal(SE.LocAbs), compVal(SE.Abs)
--
mkClosure env loc funStore target_ty opencode = do
  let (fname,funStore1) = TE.newName funStore
  let locvars = SE._locVarEnv env
  let tyvars  = SE._typeVarEnv env
  let (freevars, freetys) = unzip $ SE._varEnv env 
  let target_freevars = map TE.Var freevars
  
  target_freetys <- mapM compValType freetys
  let codename = TE.CodeName fname (map Location locvars) (map TT.TypeVarType tyvars)
  let codety = TT.CodeType locvars tyvars target_freetys target_ty
  let code = TE.Code locvars tyvars freevars opencode

  let funStore2 = TE.addFun loc funStore1 fname codety code
  return (funStore2, TE.Closure target_freevars codename)

