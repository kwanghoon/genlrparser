module Compile where

import Location

import qualified Type as ST
import qualified Expr as SE
import Literal
import Prim

import qualified CSType as TT
import qualified CSExpr as TE

import Control.Monad

compile :: Monad m =>
  SE.GlobalTypeInfo -> [SE.TopLevelDecl] ->
  m (TE.GlobalTypeInfo, [TE.TopLevelDecl],TE.FunctionStore, TE.Expr)
compile s_gti s_topleveldecls = do
  t_gti <- compileGTI s_gti
  (funStore, t_libs, t_bindingDecls) <- compTopLevels s_gti TE.initFunctionStore s_topleveldecls
  let main = TE.ValExpr (TE.UnitM (TE.Var SE.mainName))
  return (t_gti, t_libs, funStore, TE.ValExpr $ TE.BindM t_bindingDecls main)


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

compTopLevels :: Monad m =>
  SE.GlobalTypeInfo -> TE.FunctionStore ->
  [SE.TopLevelDecl] -> m (TE.FunctionStore, [TE.TopLevelDecl], [TE.BindingDecl])
compTopLevels s_gti funStore [] = return (funStore, [], [])
compTopLevels s_gti funStore (toplevel:toplevels) = do
  (funStore1, t_toplevel, bindingDecls1) <- compTopLevel s_gti funStore toplevel
  (funStore2, t_toplevels, bindingDecls2) <- compTopLevels s_gti funStore1 toplevels
  return (funStore2, t_toplevel++t_toplevels, bindingDecls1++bindingDecls2)

compTopLevel :: Monad m =>
  SE.GlobalTypeInfo -> TE.FunctionStore ->
  SE.TopLevelDecl -> m (TE.FunctionStore, [TE.TopLevelDecl], [TE.BindingDecl])
  
compTopLevel s_gti funStore (SE.LibDeclTopLevel x ty) = do
  target_ty <- compValType ty
  return (funStore, [TE.LibDeclTopLevel x target_ty], [])

compTopLevel s_gti funStore (SE.DataTypeTopLevel
               (SE.DataType dtname locvars tyvars tycondecls)) = return (funStore, [], [])

compTopLevel s_gti funStore (SE.BindingTopLevel bindingDecl) = do
  (funStore1, t_bindingDecl) <- compBindingDecl s_gti SE.initEnv clientLoc funStore bindingDecl
  return ( funStore1, [], [t_bindingDecl] )

-------------------------------
-- Compile binding declarations
-------------------------------
--
-- Note: InterTE.Binding x ty expr as do x:ty <- expr
--
compBindingDecl :: Monad m =>
  SE.GlobalTypeInfo -> SE.Env -> Location ->
  TE.FunctionStore -> SE.BindingDecl -> m (TE.FunctionStore, TE.BindingDecl)
  
compBindingDecl s_gti env loc funStore (SE.Binding x ty expr) = do
  target_ty <- compValType ty
  (funStore1, target_expr) <- compExpr s_gti env loc ty funStore expr 
  return (funStore1, TE.Binding x target_ty target_expr)

-- compExpr
compExpr :: Monad m =>
  SE.GlobalTypeInfo -> SE.Env -> Location -> ST.Type ->
  TE.FunctionStore -> SE.Expr -> m (TE.FunctionStore, TE.Expr)   -- Ending with 'ValExpr Expr'??
  
compExpr s_gti env loc s_ty funStore (SE.Var x) = 
  return (funStore, TE.ValExpr $ TE.UnitM (TE.Var x))

compExpr s_gti env loc (ST.TypeAbsType tyvars0 s_ty) funStore (SE.TypeAbs tyvars1 expr) = do
  -- Assume tyvars0 == tyvars1
  t_ty <- compValType s_ty
  let target_ty = TT.TypeAbsType tyvars0 t_ty
  let env1 = env {SE._typeVarEnv = tyvars1++SE._typeVarEnv env}
  (funStore1, target_expr) <- compExpr s_gti env1 loc s_ty funStore expr
  let opencode = TE.CodeTypeAbs tyvars1 target_expr

  (funStore2, closure) <- mkClosure env loc funStore1 target_ty opencode
  return (funStore2, TE.ValExpr $ TE.UnitM closure)

compExpr s_gti env loc s_ty funStore (SE.TypeAbs tyvars expr) = do
  error $ "[compVal] Not type-abstraction type: " ++ show s_ty


compExpr s_gti env loc (ST.LocAbsType locvars0 s_ty) funStore (SE.LocAbs locvars1 expr) = do
  -- Assume tyvars0 == tyvars1
  t_ty <- compValType s_ty
  let target_ty = TT.LocAbsType locvars0 t_ty
  let env1 = env {SE._locVarEnv = locvars1++SE._locVarEnv env}
  (funStore1, target_expr) <- compExpr s_gti env1 loc s_ty funStore expr
  let opencode = TE.CodeLocAbs locvars1 target_expr

  (funStore2, closure) <- mkClosure env loc funStore1 target_ty opencode
  return (funStore1, TE.ValExpr $ TE.UnitM closure)

compExpr s_gti env loc s_ty funStore (SE.LocAbs locvars1 expr) = do
  error $ "[compExpr] Not location-abstraction type: " ++ show s_ty


compExpr s_gti env loc (ST.FunType s_argty s_loc s_resty) funStore (SE.Abs xtylocs expr) = do
  -- Assume tyvars0 == tyvars1
  t_argty <- compValType s_argty
  t_resty <- compType s_resty
  let target_ty = TT.FunType t_argty s_loc t_resty
  let s_xtys = [(x,ty) | (x,ty,_) <- xtylocs]
  t_xtys <- mapM (\(x,ty) -> do { t_ty <- compValType ty; return (x,t_ty) }) s_xtys
  let env1 = env {SE._varEnv = (s_xtys ++ SE._varEnv env)}
  (funStore1, target_expr) <- compExpr s_gti env1 s_loc s_resty funStore expr
  let opencode = TE.CodeAbs t_xtys target_expr

  (funStore2, closure) <- mkClosure env s_loc funStore1 target_ty opencode
  return (funStore2, TE.ValExpr $ TE.UnitM closure)
  
compExpr s_gti env loc s_ty funStore (SE.Abs xtylocs expr) = do
  error $ "[compExpr] Not abstraction type: " ++ show s_ty ++ ", " ++ show (SE.Abs xtylocs expr)


compExpr s_gti env loc (ST.TupleType tys) funStore (SE.Tuple exprs) = do
  let (xs, funStore1) = TE.newVars (length exprs) funStore
  (funStore2, h) <-
     foldM (\ (funStore0, f) -> \ (x, s_ty, expr) -> do
       (funStore1, target_expr) <- compExpr s_gti env loc s_ty funStore0 expr
       t_ty <- compValType s_ty
       let g = TE.BindM [TE.Binding x t_ty target_expr] . TE.ValExpr . f
       return (funStore1, g)) (funStore1, \x->x) (zip3 xs tys exprs)
  return (funStore2, TE.ValExpr $ h (TE.UnitM (TE.Tuple (map TE.Var xs))))


compExpr s_gti env loc s_ty funStore (SE.Tuple exprs) = do
  error $ "[compExpr]: Not tuple type: " ++ show s_ty

compExpr s_gti env loc s_ty funStore (SE.Lit lit) = 
  return (funStore, TE.ValExpr $ TE.UnitM (TE.Lit lit))

compExpr s_gti env loc s_ty funStore (SE.Constr cname locs argtys exprs tys) = do
  let (xs, funStore1) = TE.newVars (length exprs) funStore
  t_argtys <- mapM compValType argtys
  (funStore2, h) <-
     foldM (\ (funStore0, f) -> \ (x, s_ty, expr) -> do
       (funStore1, target_expr) <- compExpr s_gti env loc s_ty funStore0 expr
       t_ty <- compValType s_ty
       let g = TE.BindM [TE.Binding x t_ty target_expr] . TE.ValExpr . f
       return (funStore1, g)) (funStore1, \x->x) (zip3 xs tys exprs)
  return (funStore2, TE.ValExpr $ h $ TE.UnitM $ TE.Constr cname locs t_argtys (map TE.Var xs))

compExpr s_gti env loc s_ty funStore (SE.Let bindingDecls expr) = do
  let bindingTypeInfo = [(x,ty) | SE.Binding x ty expr <- bindingDecls]
  let bindingTypeInfo1 = (bindingTypeInfo ++ SE._varEnv env)
  let env1 = env { SE._varEnv=bindingTypeInfo1 }
  (funStore2, t_bindingDecls) <-
    foldM (\(funStore0, bindingDecls0) -> \bindingDecl0 -> do
              (funStore1,bindingDecl1)
                 <- compBindingDecl s_gti env1 loc funStore0 bindingDecl0
              return (funStore1, bindingDecl1:bindingDecls0))
          (funStore, [])
          bindingDecls
  (funStore3, t_expr) <- compExpr s_gti env loc s_ty funStore2 expr
  return (funStore3, TE.ValExpr $ TE.BindM t_bindingDecls t_expr)
   
compExpr s_gti env loc s_ty funStore (SE.Case expr (Just case_ty) alts) = do
  let (x, funStore0) = TE.newVar funStore
  target_case_ty <- compValType case_ty
  (funStore1, target_expr) <- compExpr s_gti env loc case_ty funStore0 expr
  case case_ty of
    ST.ConType tyconName locs tys ->
      case SE.lookupDataTypeName s_gti tyconName of
        ((locvars, tyvars, tycondecls):_) -> do
           (funStore2, target_alts) <-
              compAlts s_gti env loc locs locvars tys tyvars tycondecls s_ty funStore1 alts
           return (funStore2, TE.ValExpr $
                                TE.BindM [ TE.Binding x target_case_ty target_expr ]
                                 (TE.Case (TE.Var x) target_case_ty target_alts))
        [] -> error $ "[compExpr] invalid constructor type: " ++ tyconName
 
    ST.TupleType tys -> do
      (funStore3, target_alts) <- compAlts s_gti env loc [] [] tys [] [] s_ty funStore1 alts
      return (funStore3, TE.ValExpr $
                           TE.BindM [ TE.Binding x target_case_ty target_expr ]
                             (TE.Case (TE.Var x) target_case_ty target_alts))

compExpr s_gti env loc s_ty funStore (SE.Case expr maybe alternatives) = do
  error $ "[compExpr] No case expression type: " ++ show (SE.Case expr maybe alternatives)

compExpr s_gti env loc s_ty funStore (SE.App left (Just (ST.FunType argty locfun resty)) right maybeLoc) = do
   let ([f,x], funStore1) = TE.newVars 2 funStore
   (funStore2, target_left) <- compExpr s_gti env loc (ST.FunType argty locfun resty) funStore1 left
   (funStore3, target_right) <- compExpr s_gti env loc argty funStore2 right
   target_funty <- compValType (ST.FunType argty locfun resty)
   target_argty <- compValType argty
   let app = if loc==locfun then
                TE.App (TE.Var f) target_funty (TE.Var x)
             else if loc==clientLoc && locfun==serverLoc then
                TE.ValExpr $ TE.Req (TE.Var f) target_funty (TE.Var x)
             else if loc==serverLoc && locfun==clientLoc then
                TE.ValExpr $ TE.Call (TE.Var f) target_funty (TE.Var x)
             else
                TE.ValExpr $ TE.GenApp locfun (TE.Var f) target_funty (TE.Var x)
   return (funStore3,
           TE.ValExpr $ TE.BindM [TE.Binding f target_funty target_left]
                          (TE.ValExpr
                            (TE.BindM [TE.Binding x target_argty target_right]
                             app)))

compExpr s_gti env loc s_ty funStore (SE.App left Nothing right maybeLoc) = do
   error $ "[compExpr] App"
   

compExpr s_gti env loc s_ty funStore (SE.TypeApp expr (Just left_s_ty) tys) = do
   let (f, funStore1) = TE.newVar funStore
   (funStore2, target_expr) <- compExpr s_gti env loc left_s_ty funStore1 expr
   target_left_s_ty <- compValType left_s_ty
   target_tys <- mapM compValType tys
   return (funStore2,
           TE.ValExpr $ TE.BindM [TE.Binding f target_left_s_ty target_expr]
                         (TE.TypeApp (TE.Var f) target_left_s_ty target_tys))

compExpr s_gti env loc s_ty funStore (SE.TypeApp expr Nothing tys) =
   error $ "[compExpr] TypeApp"

compExpr s_gti env loc s_ty funStore (SE.LocApp expr (Just left_s_ty) locs) = do
   let (f, funStore1) = TE.newVar funStore
   (funStore2, target_expr) <- compExpr s_gti env loc left_s_ty funStore1 expr
   target_left_s_ty <- compValType left_s_ty
   return (funStore2,
           TE.ValExpr $ TE.BindM [TE.Binding f target_left_s_ty target_expr]
                         (TE.LocApp (TE.Var f) target_left_s_ty locs))

compExpr s_gti env loc s_ty funStore (SE.LocApp expr Nothing locs) =
   error $ "[compExpr] LocApp"

compExpr s_gti env loc s_ty funStore (SE.Prim primop exprs) = do
  let (y, funStore0) = TE.newVar funStore
  let (xs, funStore1) = TE.newVars (length exprs) funStore0
  case SE.lookupPrimOpType primop of
    ((argtys, retty):_) -> do
      (funStore2, h) <-
        foldM (\ (funStore0, f) -> \ (x, s_ty, expr) -> do
          (funStore1, target_expr) <- compExpr s_gti env loc s_ty funStore0 expr
          t_ty <- compValType s_ty
          let g = TE.ValExpr . TE.BindM [TE.Binding x t_ty target_expr] . f
          return (funStore1, g)) (funStore1, \x->x) (zip3 xs argtys exprs)
      target_retty <- compValType retty
      return (funStore2,
               h (TE.Let [TE.Binding y target_retty (TE.Prim primop (map TE.Var xs))]
                         (TE.ValExpr (TE.UnitM (TE.Var y)))))
      
    [] -> error $ "[compExpr] Not found Prim " ++ show primop


-----------
-- compAlts
-----------
compAlts s_gti env loc locs locvars tys tyvars tycondecls s_ty funStore [alt] = do
  let substLoc = zip locvars locs
  let substTy = zip tyvars tys
  (funStore1, target_alt) <- compAlt s_gti env loc substLoc substTy tycondecls [] s_ty funStore alt
  return (funStore1, [target_alt])

compAlts s_gti env loc locs locvars tys tyvars tycondecls s_ty funStore (alt:alts) = do
  let substLoc = zip locvars locs
  let substTy = zip tyvars tys
  (funStore1, target_alt)  <- compAlt  s_gti env loc substLoc substTy tycondecls [] s_ty funStore  alt
  (funStore2, target_alts) <- compAlts s_gti env loc locs locvars tys tyvars tycondecls s_ty funStore1 alts
  return (funStore2, target_alt:target_alts)
  

compAlt s_gti env loc substLoc substTy tycondecls externTys s_ty funStore (SE.Alternative con args expr) = do
-- externTys only for TupleAlternative
  case SE.lookupCon tycondecls con of
    (tys:_) ->
      if length tys==length args
      then do let tys' = map (ST.doSubst substTy) (map (ST.doSubstLoc substLoc) tys)
              let varEnv = SE._varEnv env
              let varEnv' = zip args tys' ++ varEnv
              let env1 = env {SE._varEnv=varEnv'}
              (funStore1, target_expr) <- compExpr s_gti env1 loc s_ty funStore expr
              return (funStore1, TE.Alternative con args target_expr)
      else error $ "[compAlt]: invalid arg length: " ++ con ++ show args

compAlt s_gti env loc substLoc substTy tycondecls externTys s_ty funStore (SE.TupleAlternative args expr) = do 
-- substTy==[], tycondecls==[]
  let varEnv  = SE._varEnv env
  let varEnv' = zip args externTys ++ varEnv
  let env1 = env {SE._varEnv=varEnv'}
  (funStore1, target_expr) <- compExpr s_gti env loc s_ty funStore expr
  return (funStore1, TE.TupleAlternative args target_expr)

--
-- Utility shared by compExpr(SE.TypeAbs), compExpr(SE.LocAbs), compExpr(SE.Abs)
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
  return (funStore2, TE.Closure target_freevars target_freetys codename)

