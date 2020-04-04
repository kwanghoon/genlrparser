module Verify where

import Location
import CSType
import CSExpr


---------------------
-- Verify CS programs
---------------------

type LibDecl = TopLevelDecl

verify :: Monad m => GlobalTypeInfo -> [LibDecl] -> FunctionStore -> Expr -> m ()
verify gti libs funStore mainexpr = do
  return ()

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
  
  -- Assertion
  --  (1) _freeLocVars==freeLocVars
  --  (2) _freeTyVars==freeTyVars
  --  (3) length freeVars==freeVarTys
  --  (4) All loc vars occurring in freeVarTys must be in freeLocVars
  --  (5) All ty vars occurring in freeVarTys must be in freeTyVars
  
  let env = Env { _locVarEnv=freeLocVars
                , _typeVarEnv=freeTyVars
                , _varEnv=zip freeVars freeVarTys}

  verifyOpenCode gtigci loc env ty openCode

verifyOpenCode gtigci loc env (FunType argty locfun resty) (CodeAbs ((x,ty):xTys) expr) = do
  -- Assertion
  --   (1) xTys == []
  --   (2) argty == ty

  let env1 = env {_varEnv = (x,ty) : _varEnv env}
  
  verifyExpr gtigci locfun env1 resty expr

verifyOpenCode gtigci loc env (TypeAbsType (tyvar1:tyvars1) ty) (CodeTypeAbs (tyvar2:tyvars2)  expr) = do
  -- Assertion
  --   (1) tyvar1 == tyvar2
  --   (2) tyvars1 == []
  --   (3) tyvars2 == []

  let env1 = env {_typeVarEnv = tyvar2 : _typeVarEnv env}

  verifyExpr gtigci loc env1 ty expr

verifyOpenCode gtigci loc env (LocAbsType (locvar1:locvars1) ty) (CodeLocAbs (locvar2:locvars2) expr) = do
  -- Assertion
  --   (1) locvar1 == locvar2
  --   (2) locvars1 == []
  --   (3) locvars2 == []

  let env1 = env {_locVarEnv = locvar2 : _locVarEnv env}

  verifyExpr gtigci loc env1 ty expr

verifyOpenCode gtigci loc env ty openCode =
  error $ "[verifyOpenCode] Not well-typed: " ++ show ty ++ "," ++ show openCode


---------------------
-- Verify expressions
---------------------

verifyExpr :: Monad m =>
  GlobalInfo -> Location -> Env -> Type -> Expr -> m ()

verifyExpr gtigci loc env ty expr = return ()

