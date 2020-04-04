module Verify where

import Location
import Prim
import Literal
import qualified Expr as SE
import CSType
import CSExpr


---------------------
-- Verify CS programs
---------------------

type LibDecl = TopLevelDecl

verify :: Monad m => GlobalTypeInfo -> [LibDecl] -> FunctionStore -> Expr -> m ()
verify gti libs funStore mainexpr =
  case mainExprType gti of
    [] -> error $ "[verify] no main binding"
    (mainty:_) -> do
      verifyFunStore gti funStore
      let clientFunStore = _clientstore funStore
      verifyExpr (gti,clientFunStore) clientLoc initEnv mainty mainexpr


mainExprType gti =
  [ty | (x,ty) <- _bindingTypeInfo gti, x==SE.mainName]

-------------------------
-- Verify function stores
-------------------------

type GlobalInfo = (GlobalTypeInfo, GlobalCodeInfo)

type GlobalCodeInfo = [(String, (CodeType, Code))]

verifyFunStore :: Monad m => GlobalTypeInfo -> FunctionStore -> m()
  
verifyFunStore gti funStore = do
  let clientFunStore = _clientstore funStore
  let serverFunStore = _serverstore funStore
  verifyFunStoreAt gti clientLoc clientFunStore
  verifyFunStoreAt gti serverLoc serverFunStore

verifyFunStoreAt :: Monad m => GlobalTypeInfo -> Location -> GlobalCodeInfo -> m()
  
verifyFunStoreAt gti loc gci = 
  mapM_ (\(f, (codety, code)) -> verifyCode (gti,gci) loc codety code) gci


---------------
-- Verify codes
---------------

verifyCode gtigci loc (CodeType _freeLocVars _freeTyVars freeVarTys ty)
                      (Code freeLocVars freeTyVars freeVars openCode) = do
  
  assert (_freeLocVars == freeLocVars)  --  (1) _freeLocVars==freeLocVars
    ("[verifyCode] Not equal free loc vars: "
                   ++ show _freeLocVars ++ " != " ++ show freeLocVars)
  
  assert ( _freeTyVars == freeTyVars)  --  (2) _freeTyVars==freeTyVars
    ("[verifyCode] Not equal free ty vars: "
                   ++ show _freeTyVars ++ " != " ++ show freeTyVars)
  
  assert (length freeVars == length freeVarTys)  -- (3) length freeVars==length freeVarTys
    ("[verifyCode] Not equal free variables and types: "
                   ++ show freeVars ++ " !: " ++ show freeVarTys)

  --  (4) All loc vars occurring in freeVarTys must be in freeLocVars
  --  (5) All ty vars occurring in freeVarTys must be in freeTyVars
  
  let env = Env { _locVarEnv=freeLocVars
                , _typeVarEnv=freeTyVars
                , _varEnv=zip freeVars freeVarTys}

   -- TODO: free locvars, free tyvars, free vars are closed.

  verifyOpenCode gtigci loc env ty openCode

--------------------
-- Verify open codes
--------------------

verifyOpenCode gtigci loc env (FunType argty locfun resty) (CodeAbs ((x,ty):xTys) expr) = do
  assert (null xTys)  --  (1) xTys == []
    ("[verifyOpenCode] CodeAbs has more than two args? " ++ show xTys)
  
  assert (equalType argty ty)  --   (2) argty == ty
    ("[verifyOpenCode] not equal types: " ++ show argty ++ " != " ++ show ty)

  let env1 = env {_varEnv = (x,ty) : _varEnv env}
  
  verifyExpr gtigci locfun env1 resty expr

verifyOpenCode gtigci loc env (TypeAbsType (tyvar1:tyvars1) ty) (CodeTypeAbs (tyvar2:tyvars2)  expr) = do
  --   (1) tyvar1 == tyvar2
  let _ty = if tyvar1 == tyvar2 then ty
            else doSubst [(tyvar1, TypeVarType tyvar2)] ty

  assert (tyvars1 == [])  --   (2) tyvars1 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two ty args? " ++ show tyvars1)
  assert (tyvars2 == [])  --   (3) tyvars2 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two ty args? " ++ show tyvars2)
  
  let env1 = env {_typeVarEnv = tyvar2 : _typeVarEnv env}

  verifyExpr gtigci loc env1 _ty expr


verifyOpenCode gtigci loc env (LocAbsType (locvar1:locvars1) ty) (CodeLocAbs (locvar2:locvars2) expr) = do
  --   (1) locvar1 == locvar2
  let _ty = if locvar1 == locvar2 then ty
            else doSubstLoc [(locvar1, LocVar locvar2)] ty

  assert (locvars1 == [])  --   (2) locvars1 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two loc args? " ++ show locvars1)
  assert (locvars2 == [] ) --   (3) locvars2 == []
    ("[verifyOpenCode] CodeTypeAbs has more than two loc args? " ++ show locvars2)

  let env1 = env {_locVarEnv = locvar2 : _locVarEnv env}

  verifyExpr gtigci loc env1 _ty expr

verifyOpenCode gtigci loc env ty openCode =
  error $ "[verifyOpenCode] Not well-typed: " ++ show ty ++ "," ++ show openCode


--------------------
-- Verify code names
--------------------

verifyCodeName :: Monad m => GlobalInfo -> Type -> [Type] -> CodeName -> m ()

verifyCodeName (gti, gci) someAbsTy freeVarTys (CodeName f locs tys) =
  case [(codeType, code) | (g, (codeType, code)) <- gci, f==g] of
    [] -> error $ "[verifyCodeName] Code not found: " ++ f
    ((CodeType locvars0 tyvars0 freeVarTys0 ty, Code locvars1 tyvars1 freeVarTys1 _):_) -> do

      assert (locvars0 == locvars1)  --   (1) locvars0 == locvars1
        ("[verifyCodeName] No equal loc var names: "
           ++ show locvars0 ++ " != " ++ show locvars1)
      
      assert (tyvars0 == tyvars1)  --   (2) tyvars0 == tyvars1
        ("[verifyCodeName] No equal type var names: "
                       ++ show tyvars0 ++ " != " ++ show tyvars1)

      let _freeVarTys1 = map TypeVarType freeVarTys1
      
      assert (and $ map (uncurry equalType) (zip freeVarTys0 _freeVarTys1))  --  (3) freeVarTys0 == freeVarTys1
        ("[verifyCodeName] Not equal free var types: "
                       ++ show freeVarTys0 ++ " != " ++ show freeVarTys1)

      --  freeVarTys0 {locs/locvars0} [tys/tyvars0] == freeVarTys
      --  ty {locs/locvars0} [tys/tyvars0] == someAbsTy

      let substTy  = zip tyvars0 tys
      let substLoc = zip locvars0 locs
      
      let substed_freeVarTys0 = map (doSubstLoc substLoc . doSubst substTy) freeVarTys0
      let substed_ty = (doSubstLoc substLoc . doSubst substTy) ty

      let equal (ty1, ty2) =
            assert (equalType ty1 ty2)
              ("[verifyCodeName] Not equal type: " ++ show ty1 ++ " != " ++ show ty2)

      mapM_ equal $ zip (substed_ty : substed_freeVarTys0) (someAbsTy : freeVarTys)


---------------------
-- Verify expressions
---------------------

verifyExpr :: Monad m => GlobalInfo -> Location -> Env -> Type -> Expr -> m ()

verifyExpr gtigci loc env ty (ValExpr v) = verifyValue gtigci loc env ty v

verifyExpr gtigci loc env ty (Let bindingDecls expr) = do
  let (xtys, exprs) =  unzip [((x,ty), expr) | Binding x ty expr <- bindingDecls]
  let (xs, tys) = unzip xtys
  let env1 = env {_varEnv = xtys ++ _varEnv env}
  mapM_ (\ (vty, expr) -> verifyExpr gtigci loc env1 vty expr) $ zip tys exprs
  verifyExpr gtigci loc env1 ty expr

verifyExpr gtigci loc env ty (App left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc funloc)  --   (1) loc == funloc
    ("[verifyExpr] Not equal locations: " ++ show loc ++ " != " ++ show funloc)
  assert (equalType ty resty)  --   (2) ty == resty
    ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyExpr gtigci loc env ty (TypeApp left (CloType (TypeAbsType tyvars bodyty)) tys) = do
  assert (length tyvars == length tys)  --   (1) length tyvars == length tys
    ("[verifyExpr] Not equal arities: " ++ show tyvars ++ " != " ++ show tys)

  verifyValue gtigci loc env (CloType (TypeAbsType tyvars bodyty)) left
  let subst = zip tyvars tys
  let substed_bodyty = doSubst subst bodyty
  
  assert (equalType substed_bodyty ty)
    ("[verifyExpr] Not equal type: " ++ show substed_bodyty ++ " != " ++ show ty)

verifyExpr gtigci loc env ty (LocApp left (CloType (LocAbsType locvars bodyty)) locs) = do
  assert (length locvars == length locs)  --   (1) length locvars == length locs
    ("[verifyExpr] Not equal arities: " ++ show locvars ++ " != " ++ show locs)
  
  verifyValue gtigci loc env (CloType (LocAbsType locvars bodyty)) left
  let substLoc = zip locvars locs
  let substed_bodyty = doSubstLoc substLoc bodyty
  
  assert (equalType substed_bodyty ty)
    ("[verifyExpr] Not equal type: " ++ show substed_bodyty ++ " != " ++ show ty)

verifyExpr gtigci loc env ty (Prim prim vs) = do
  case lookupPrimOpType prim of
    [] -> error $ "[verifyExpr] Not found prim: " ++ show prim
    ((argtys,resty):_) -> do
       mapM_ (\ (argty, v) -> verifyValue gtigci loc env argty v) (zip argtys vs)
       assert (equalType ty resty)  --   (1) ty == resty
         ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)

verifyExpr gtigci loc env ty expr = 
  error $ "[verifyExpr]: not well-typed: " ++ show expr ++ " : " ++ show ty


----------------
-- Verify values
----------------

verifyValue :: Monad m => GlobalInfo -> Location -> Env -> Type -> Value -> m ()

verifyValue gtigci loc env ty (Var x) = do
  case [ty | (y,ty) <- _varEnv env, x==y] of
    []    -> error $ "[verifyExpr] Variable not found: " ++ x
    (_:_) -> return ()

verifyValue gtigci loc env ty (Lit lit) = return ()

verifyValue gtigci loc env (TupleType tys) (Tuple vs) =
  mapM_ ( \ (ty,v) -> verifyValue gtigci loc env ty v ) (zip tys vs)

verifyValue gtigci loc env (CloType ty) (Closure vs tys codeName) = do
  let env0 = env {_varEnv = [] }
  mapM_ ( \ (ty,v) -> verifyValue gtigci loc env0 ty v) (zip tys vs)
  verifyCodeName gtigci ty tys codeName

verifyValue gtigci loc env (MonType ty) (UnitM v) = verifyValue gtigci loc env ty v

verifyValue gtigci loc env (MonType ty) (BindM bindingDecls expr) = do
  let (xtys, exprs) =  unzip [((x,ty), expr) | Binding x ty expr <- bindingDecls]
  let (xs, tys) = unzip xtys
  let env1 = env {_varEnv = xtys ++ _varEnv env}
  let monadic_tys = map MonType tys
  mapM_ (\ (mty, expr) -> verifyExpr gtigci loc env1 mty expr) $ zip monadic_tys exprs
  verifyExpr gtigci loc env1 (MonType ty) expr
  
verifyValue gtigci loc env ty (Req left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc clientLoc)  --   (1) loc == client
    ("[verifyValue] Not client location: " ++ show loc)
  assert (equalLoc funloc serverLoc)  --   (2) funloc == server
    ("[verifyValue] Not server location: " ++ show funloc)
  assert (equalType ty resty)  --   (3) ty == resty
    ("[verifyExpr] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty (Call left (CloType (FunType argty funloc resty)) right) = do
  assert (equalLoc loc serverLoc)  --   (1) loc == server
    ("[verifyValue] Not server location: " ++ show loc)
  assert (equalLoc funloc clientLoc)  --   (2) funloc == client
    ("[verifyValue] Not client location: " ++ show funloc)
  assert (equalType ty resty)  --   (3) ty == resty
    ("[verifyValue] Not equal types: " ++ show ty ++ " != " ++ show resty)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty (GenApp funloc0 left (CloType (FunType argty funloc resty)) right) = do
  assert (equalType ty resty)  --   (1) ty == resty
    ("[verifyValue] Not equal types: " ++ show ty ++ " != " ++ show resty)
  assert (equalLoc funloc0 funloc)  --   (2) funloc0 == funloc
    ("[verifyValue] Not equal locations: " ++ show funloc0 ++ " != " ++ show funloc)
  
  verifyValue gtigci loc env (CloType (FunType argty funloc resty)) left
  verifyValue gtigci loc env argty right

verifyValue gtigci loc env ty value =
  error $ "[verifyValue]: not well-typed: " ++ show value ++ " : " ++ show ty

---
assert cond msg = if cond then return () else error msg
