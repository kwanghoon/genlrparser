module Compile where

import Location

import qualified Type as ST
import qualified Expr as SE
import Literal

import qualified CSType as TT
import qualified CSExpr as TE

compile :: Monad m => SE.GlobalTypeInfo -> [SE.TopLevelDecl] -> m (TE.GlobalTypeInfo, [TE.TopLevelDecl])
compile s_gti s_topleveldecls = do
  t_gti <- compileGTI s_gti
  t_topleveldecls' <- mapM (compTopLevel t_gti) s_topleveldecls
  let t_topleveldecls = concat t_topleveldecls'
  return (t_gti, t_topleveldecls)

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
compTopLevel :: Monad m => TE.GlobalTypeInfo -> SE.TopLevelDecl -> m [TE.TopLevelDecl]
compTopLevel gti (SE.LibDeclTopLevel x ty) = return []

compTopLevel gti (SE.DataTypeTopLevel
               (SE.DataType dtname locvars tyvars tycondecls)) = return []

compTopLevel gti (SE.BindingTopLevel bindingDecl) = do
  target_bindingDecl <- compBindingDecl gti bindingDecl
  return [TE.BindingTopLevel target_bindingDecl]

-------------------------------
-- Compile binding declarations
-------------------------------
--
-- Note: InterTE.Binding x ty expr as do x:ty <- expr
--
compBindingDecl :: Monad m => TE.GlobalTypeInfo -> SE.BindingDecl -> m TE.BindingDecl
compBindingDecl gti (SE.Binding x ty expr) = do
  target_ty <- compValType ty
  target_val <- compExpr gti TE.initEnv expr
  return (TE.Binding x target_ty target_val)

-- compExpr
compExpr :: Monad m => TE.GlobalTypeInfo -> TE.Env -> SE.Expr -> m TE.Value
compExpr gti env (SE.Var x) = do
  target_var_x <- compVal gti env (SE.Var x)
  return (TE.UnitM target_var_x)

compExpr gti env (SE.TypeAbs tyvars expr) = error "compExpr: Not implemented"


-- compVal
compVal :: Monad m => TE.GlobalTypeInfo -> TE.Env -> SE.Expr -> m TE.Value
compVal gti env (SE.Var x) = return (TE.Var x)

compVal gti evn (SE.Lit lit) = return (TE.Lit lit)