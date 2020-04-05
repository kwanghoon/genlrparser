{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Execute where

import Location
import CSType
import CSExpr hiding (Env(..))

--import Text.JSON.Generic

-- type Memory = [(Int,Value)]


-- Configuration

type EvalContext = Expr -> Expr
type EvalContextValue = Value -> Expr

type Stack = [EvalContext]

data Config =
    ClientConfig Expr Stack Stack   -- <M;Delta_c | Delta_s
  | ServerConfig Stack Expr Stack   -- <Delta_c | M;Delta_s>
  | Stop Value
--  deriving (Show, Typeable, Data)  


--
execute :: Monad m => GlobalTypeInfo -> FunctionStore -> Expr -> m ()
execute gti funStore mainExpr = return ()

--
initConfig main_expr = ClientConfig main_expr [] []

-- TODO: 
doSubstExpr subst expr = expr

doSubstLocExpr substLoc expr = expr

doSubstTyExpr substTy expr = expr

calc op vs = head vs

--
applyEvCxt [] expr = expr
applyEvCxt (evcxt:evcxts) expr = applyEvCxt evcxts (evcxt expr)

toFun [] = \x->x
toFun (evcxt:evcxts) = toFun evcxts . evcxt

-----------------------------------------------------------
-- < EvCtx[ Value]; Client stack | Server stack> ==> Config
-----------------------------------------------------------

clientExpr :: Monad m => FunctionStore -> [EvalContext] -> Expr -> Stack -> Stack -> m Config

clientExpr fun_store evctx (ValExpr v) client_stack server_stack =
  clientValue fun_store evctx v client_stack server_stack

-- (E-Let)
clientExpr fun_store evctx (Let [Binding x ty b@(ValExpr v)] expr) client_stack server_stack = do
  let subst = [(x,ValExpr v)]
  return $ ClientConfig (applyEvCxt evctx (doSubstExpr subst expr)) client_stack server_stack

-- (let x = Elet[] in M)
clientExpr fun_store evctx (Let [Binding x ty b@(_)] expr) client_stack server_stack = do
  clientExpr fun_store ((\bexpr->Let [Binding x ty bexpr] expr):evctx) b client_stack server_stack

-- (E-Proj-i) or (E-Tuple)
clientExpr fun_store evctx (Case (Tuple vs) casety [TupleAlternative xs expr]) client_stack server_stack = do
  let subst = zip xs vs
  return $ ClientConfig (applyEvCxt evctx (doSubstExpr subst expr)) client_stack server_stack

-- (E-Proj-i) or (E-Data constructor)
clientExpr fun_store evctx (Case (Constr cname locs tys vs argtys) casety alts) client_stack server_stack = do
  case [(dname,xs,expr) | Alternative dname xs expr <- alts, cname==dname] of
    ((_,xs,expr):_) -> do
      let subst = zip xs vs
      return $ ClientConfig (applyEvCxt evctx (doSubstExpr subst expr)) client_stack server_stack
      
    [] -> error $ "[clientExpr] Case alternative not found: " ++ cname

-- (E-App)
clientExpr fun_store evctx (App (Closure vs vstys codename) funty arg) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname,(codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeAbs [(x,_)] expr)):_) -> do
      let subst    = zip fvvars vs ++ [(x,arg)]
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig (applyEvCxt evctx substed_expr) client_stack server_stack
    
    [] -> error $ "[clientExpr] Client abs code not found: " ++ fname

-- (E-TApp)
clientExpr fun_store evctx (TypeApp (Closure vs vstys codename) funty [argty]) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeTypeAbs [a] expr)):_) -> do
      let subst    = zip fvvars vs 
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys ++ [(a,argty)]
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig (applyEvCxt evctx substed_expr) client_stack server_stack
      
    [] -> error $ "[clientExpr] Client tyabs code not found: " ++ fname

-- (E-LApp)
clientExpr fun_store evctx (LocApp (Closure vs vstys codename) funty [argloc]) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeLocAbs [l] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = zip locvars locs ++ [(l,argloc)]
      let substTy  = zip tyvars tys
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig (applyEvCxt evctx substed_expr) client_stack server_stack

    [] -> error $ "[clientExpr] Client locabs code not found: " ++ fname

clientExpr fun_store evctx (Prim primop vs) client_stack server_stack = do
  let v = calc primop vs
  return $ ClientConfig (applyEvCxt evctx (ValExpr v)) client_stack server_stack
  

--
clientValue :: Monad m => FunctionStore -> [EvalContext] -> Value -> Stack -> Stack -> m Config

-- (E-Unit-C)
clientValue fun_store [] (UnitM v) client_stack (top_evctx:server_stack) =
  return $ ServerConfig client_stack (top_evctx (ValExpr (UnitM v))) server_stack

-- (E-Req)
clientValue fun_store evctx (Req f funty arg) client_stack server_stack = do
  let client_stack1 = if null evctx then client_stack else (toFun evctx):client_stack
  return $ ServerConfig client_stack1 (App f funty arg) server_stack

-- (E-Gen-C-C) and (E-Gen-C-S)
clientValue fun_store evctx (GenApp loc f funty arg) client_stack server_stack = do
  if loc==clientLoc then
    return $ ClientConfig (applyEvCxt evctx (App f funty arg)) client_stack server_stack
  else if loc==serverLoc then
    return $ ClientConfig (applyEvCxt evctx (ValExpr (Req f funty arg))) client_stack server_stack
  else
    error $ "[clientValue] GenApp: Unexpected location : " ++ show loc

-- (E-Do)
clientValue fun_store evctx (BindM [Binding x ty b@(ValExpr (UnitM v))] expr) client_stack server_stack = do
  let subst = [(x,ValExpr v)]
  return $ ClientConfig (applyEvCxt evctx (doSubstExpr subst expr)) client_stack server_stack

-- ( do x<-E[] in M )
clientValue fun_store evctx (BindM [Binding x ty b@(_)] expr) client_stack server_stack = do
  clientExpr fun_store ((\bexpr->ValExpr (BindM [Binding x ty bexpr] expr)):evctx) b client_stack server_stack

  

------------------------------------------------------------
-- < Client stack | EvCtx[ Value ]; Server stack> ==> Config
------------------------------------------------------------

serverExpr :: Monad m => FunctionStore -> Stack -> [EvalContext] -> Expr -> Stack -> m Config

serverExpr fun_store client_stack evctx (ValExpr v) server_stack =
  serverValue fun_store client_stack evctx v server_stack

-- (E-Let)
serverExpr fun_store client_stack evctx (Let [Binding x ty b@(ValExpr v)] expr) server_stack = do
  let subst = [(x,ValExpr v)]
  return $ ServerConfig client_stack (applyEvCxt evctx (doSubstExpr subst expr)) server_stack

-- (let x = Elet[] in M)
serverExpr fun_store client_stack evctx (Let [Binding x ty b@(_)] expr) server_stack = do
  serverExpr fun_store client_stack ((\bexpr->Let [Binding x ty bexpr] expr):evctx) b server_stack

-- (E-Proj-i) or (E-Tuple)
serverExpr fun_store client_stack evctx (Case (Tuple vs) casety [TupleAlternative xs expr]) server_stack = do
  let subst = zip xs vs
  return $ ServerConfig client_stack (applyEvCxt evctx (doSubstExpr subst expr)) server_stack

-- (E-Proj-i) or (E-Data constructor)
serverExpr fun_store client_stack evctx (Case (Constr cname locs tys vs argtys) casety alts) server_stack = do
  case [(dname,xs,expr) | Alternative dname xs expr <- alts, cname==dname] of
    ((_,xs,expr):_) -> do
      let subst = zip xs vs
      return $ ServerConfig client_stack (applyEvCxt evctx (doSubstExpr subst expr)) server_stack
      
    [] -> error $ "[serverExpr] Case alternative not found: " ++ cname

-- (E-App)
serverExpr fun_store client_stack evctx (App (Closure vs vstys codename) funty arg) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname,(codetyps,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeAbs [(x,_)] expr)):_) -> do
      let subst    = zip fvvars vs ++ [(x,arg)]
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack (applyEvCxt evctx substed_expr) server_stack

    [] -> error $ "[serverExpr] Client abs code not found: " ++ fname

-- (E-TApp)
serverExpr fun_store client_stack evctx (TypeApp (Closure vs vstys codename) funty [argty]) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeTypeAbs [a] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys  ++ [(a,argty)]
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack (applyEvCxt evctx substed_expr) server_stack

    [] -> error $ "[clientExpr] Client tyabs code not found: " ++ fname
      
-- (E-LApp)
serverExpr fun_store client_stack evctx (LocApp (Closure vs vstys codename) funty [argloc]) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeLocAbs [l] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = zip locvars locs ++ [(l,argloc)]
      let substTy  = zip tyvars tys
      let substed_expr = doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack (applyEvCxt evctx substed_expr) server_stack

    [] -> error $ "[serverExpr] Client locabs code not found: " ++ fname

serverExpr fun_store client_stack evctx (Prim primop vs) server_stack = do
  let v = calc primop vs
  return $ ServerConfig client_stack (applyEvCxt evctx (ValExpr v)) server_stack
      

--
serverValue :: Monad m => FunctionStore -> Stack -> [EvalContext] -> Value -> Stack -> m Config

-- (E-Unit-S-E)
serverValue fun_store [] [] (UnitM v) [] =
  return $ ClientConfig (ValExpr (UnitM v)) [] []

-- (E-Unit-S)
serverValue fun_store (top_evctx:client_stack) [] (UnitM v) server_stack =
  return $ ClientConfig (top_evctx (ValExpr (UnitM v))) client_stack server_stack

-- (E-Call)
serverValue fun_store client_stack evctx (Call f funty arg) server_stack = do
  let server_stack1 = if null evctx then server_stack else (toFun evctx):server_stack
  return $ ClientConfig (App f funty arg) client_stack server_stack1

-- (E-Gen-C-C) and (E-Gen-S-C)
serverValue fun_store client_stack evctx (GenApp loc f funty arg) server_stack = do
  if loc==serverLoc then
    return $ ServerConfig client_stack (applyEvCxt evctx (App f funty arg)) server_stack
  else if loc==clientLoc then
    return $ ServerConfig client_stack (applyEvCxt evctx (ValExpr (Call f funty arg))) server_stack
  else
    error $ "[serverValue] GenApp: Unexpected location : " ++ show loc

-- (E-Do)
serverValue fun_store client_stack evctx (BindM [Binding x ty b@(ValExpr (UnitM v))] expr) server_stack = do
  let subst = [(x,ValExpr v)]
  return $ ServerConfig client_stack (applyEvCxt evctx (doSubstExpr subst expr)) server_stack

-- ( do x<-E[] in M ) : b is one of BindM, Call, and GenApp.
serverValue fun_store client_stack evctx (BindM [Binding x ty b@(_)] expr) server_stack = do
  serverExpr fun_store client_stack ((\bexpr->ValExpr (BindM [Binding x ty bexpr] expr)):evctx) b server_stack

