{-# LANGUAGE DeriveDataTypeable, DeriveGeneric #-}

module Execute where

import Location
import Prim
import Literal
import CSType
import CSExpr hiding (Env(..))

--import Text.JSON.Generic

-- type Memory = [(Int,Value)]


-- Configuration

type EvalContext = Expr -> Expr

type Stack = [EvalContext]

data Config =
    ClientConfig [EvalContext] Expr Stack Stack   -- <M;Delta_c | Delta_s
  | ServerConfig Stack [EvalContext] Expr Stack   -- <Delta_c | M;Delta_s>
--  deriving (Show, Typeable, Data)  


--
execute :: GlobalTypeInfo -> FunctionStore -> Expr -> IO Value
execute gti funStore mainExpr = do
  v <- run funStore (initConfig mainExpr)
  return v

--
run :: FunctionStore -> Config -> IO Value

run funStore (ClientConfig [] (ValExpr (UnitM v)) [] []) = do
  putStrLn $ "[DONE]: [Client] " ++ show (ValExpr (UnitM v)) ++ "\n"
  return v

run funStore (ClientConfig evctx expr client_stack server_stack) = do
  putStrLn $ "[STEP] [Client] " ++ show expr ++ "\n"
  putStrLn $ "       EvCtx    " ++ showEvCxt evctx ++ "\n"
  putStrLn $ "       c stk    " ++ showStack client_stack ++ "\n"
  putStrLn $ "       s stk    " ++ showStack server_stack ++ "\n"
  config <- clientExpr funStore [] (applyEvCxt evctx expr) client_stack server_stack
  run funStore config

run funStore (ServerConfig client_stack evctx expr server_stack) = do
  putStrLn $ "[STEP] [Server] " ++ show expr ++ "\n"
  putStrLn $ "       EvCtx    " ++ showEvCxt evctx ++ "\n"
  putStrLn $ "       c stk    " ++ showStack client_stack ++ "\n"
  putStrLn $ "       s stk    " ++ showStack server_stack ++ "\n"
  config <- serverExpr funStore client_stack [] (applyEvCxt evctx expr) server_stack
  run funStore config

--
initConfig main_expr = ClientConfig [] main_expr [] []

--
applyEvCxt [] expr = expr
applyEvCxt (evcxt:evcxts) expr = applyEvCxt evcxts (evcxt expr)

toFun [] = \x->x
toFun (evcxt:evcxts) = toFun evcxts . evcxt

showEvCxt cxt = show $ applyEvCxt cxt (ValExpr (Var "HOLE"))

showStack stk = show $ map showEvCxt [[cxt] | cxt <- stk]

-----------------------------------------------------------
-- < EvCtx[ Value]; Client stack | Server stack> ==> Config
-----------------------------------------------------------

clientExpr :: Monad m => FunctionStore -> [EvalContext] -> Expr -> Stack -> Stack -> m Config

clientExpr fun_store evctx (ValExpr v) client_stack server_stack =
  clientValue fun_store evctx v client_stack server_stack

-- (E-Let)
clientExpr fun_store evctx (Let [Binding x ty b@(ValExpr v)] expr) client_stack server_stack = do
  let subst = [(x,v)]
  return $ ClientConfig evctx (doSubstExpr subst expr) client_stack server_stack

-- (let x = Elet[] in M)
clientExpr fun_store evctx (Let [Binding x ty b@(_)] expr) client_stack server_stack = do
  clientExpr fun_store ((\bexpr->Let [Binding x ty bexpr] expr):evctx) b client_stack server_stack

-- (E-Proj-i) or (E-Tuple)
clientExpr fun_store evctx (Case (Tuple vs) casety [TupleAlternative xs expr]) client_stack server_stack = do
  let subst = zip xs vs
  return $ ClientConfig evctx (doSubstExpr subst expr) client_stack server_stack

-- (E-Proj-i) or (E-Data constructor) or (E-if)
clientExpr fun_store evctx (Case (Constr cname locs tys vs argtys) casety alts) client_stack server_stack = do
  case [(dname,xs,expr) | Alternative dname xs expr <- alts, cname==dname] of
    ((_,xs,expr):_) -> do
      let subst = zip xs vs
      return $ ClientConfig evctx (doSubstExpr subst expr) client_stack server_stack
      
    [] -> error $ "[clientExpr] Case alternative not found: " ++ cname

-- (E-Proj-i) or (E-Data constructor) or (E-if)
clientExpr fun_store evctx (Case (Lit (BoolLit b)) casety alts) client_stack server_stack = do
  let [Alternative b1 _ expr1,Alternative b2 _ expr2] = alts
  let text_b = show b
  if text_b==b1 then return $ ClientConfig evctx expr1 client_stack server_stack
  else if text_b==b2 then return $ ClientConfig evctx expr2 client_stack server_stack
  else error $ "[cilentExpr] Case unexpected: " ++ show b ++ "? " ++ b1 ++ " " ++ b2

-- (E-App)
clientExpr fun_store evctx (App clo@(Closure vs vstys codename recf) funty arg) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname,(codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeAbs [(x,_)] expr)):_) -> do
      let subst    = [(x,arg)] ++ zip fvvars vs 
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig evctx substed_expr client_stack server_stack
    
    [] -> error $ "[clientExpr] Client abs code not found: " ++ fname

-- (E-TApp)
clientExpr fun_store evctx (TypeApp clo@(Closure vs vstys codename recf) funty [argty]) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeTypeAbs [a] expr)):_) -> do
      let subst    = zip fvvars vs 
      let substLoc = zip locvars locs
      let substTy  = [(a,argty)] ++ zip tyvars tys 
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig evctx substed_expr client_stack server_stack
      
    [] -> error $ "[clientExpr] Client tyabs code not found: " ++ fname

-- (E-LApp)
clientExpr fun_store evctx (LocApp clo@(Closure vs vstys codename recf) funty [argloc]) client_stack server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_clientstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeLocAbs [l] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = [(l,argloc)] ++ zip locvars locs 
      let substTy  = zip tyvars tys
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ClientConfig evctx substed_expr client_stack server_stack

    [] -> error $ "[clientExpr] Client locabs code not found: " ++ fname

clientExpr fun_store evctx (Prim primop vs) client_stack server_stack = do
  let v = calc primop vs
  return $ ClientConfig evctx (ValExpr v) client_stack server_stack

clientExpr fun_store evctx expr client_stack server_stack = 
  error $ "[clientExpr] Unexpected: " ++ show expr ++ "\n" ++ show (applyEvCxt evctx expr) ++ "\n"
  
--
clientValue :: Monad m => FunctionStore -> [EvalContext] -> Value -> Stack -> Stack -> m Config

-- (E-Unit-C)
clientValue fun_store [] (UnitM v) client_stack (top_evctx:server_stack) =
  return $ ServerConfig client_stack [] (top_evctx (ValExpr (UnitM v))) server_stack

-- (E-Req)
clientValue fun_store evctx (Req f funty arg) client_stack server_stack = do
  let client_stack1 = if null evctx then client_stack else (toFun evctx):client_stack
  return $ ServerConfig client_stack1 [] (App f funty arg) server_stack

-- (E-Gen-C-C) and (E-Gen-C-S)
clientValue fun_store evctx (GenApp loc f funty arg) client_stack server_stack = do
  if loc==clientLoc then
    return $ ClientConfig evctx (App f funty arg) client_stack server_stack
  else if loc==serverLoc then
    return $ ClientConfig evctx (ValExpr (Req f funty arg)) client_stack server_stack
  else
    error $ "[clientValue] GenApp: Unexpected location : " ++ show loc

-- (E-Do)
clientValue fun_store evctx (BindM [Binding x ty b@(ValExpr (UnitM v))] expr) client_stack server_stack = do
  let subst = [(x,v)]
  return $ ClientConfig evctx (doSubstExpr subst expr) client_stack server_stack

-- ( do x<-E[] in M )
clientValue fun_store evctx (BindM [Binding x ty b@(_)] expr) client_stack server_stack = do
  clientExpr fun_store ((\bexpr->ValExpr (BindM [Binding x ty bexpr] expr)):evctx) b client_stack server_stack

clientValue fun_store evctx v client_stack server_stack =
  error $ "[clientValue] Unexpected: " ++ show v ++ "\n" ++ show (applyEvCxt evctx (ValExpr v)) ++ "\n" 
  

------------------------------------------------------------
-- < Client stack | EvCtx[ Value ]; Server stack> ==> Config
------------------------------------------------------------

serverExpr :: Monad m => FunctionStore -> Stack -> [EvalContext] -> Expr -> Stack -> m Config

serverExpr fun_store client_stack evctx (ValExpr v) server_stack =
  serverValue fun_store client_stack evctx v server_stack

-- (E-Let)
serverExpr fun_store client_stack evctx (Let [Binding x ty b@(ValExpr v)] expr) server_stack = do
  let subst = [(x,v)]
  return $ ServerConfig client_stack evctx (doSubstExpr subst expr) server_stack

-- (let x = Elet[] in M)
serverExpr fun_store client_stack evctx (Let [Binding x ty b@(_)] expr) server_stack = do
  serverExpr fun_store client_stack ((\bexpr->Let [Binding x ty bexpr] expr):evctx) b server_stack

-- (E-Proj-i) or (E-Tuple) or (E-if)
serverExpr fun_store client_stack evctx (Case (Tuple vs) casety [TupleAlternative xs expr]) server_stack = do
  let subst = zip xs vs
  return $ ServerConfig client_stack evctx (doSubstExpr subst expr) server_stack

-- (E-Proj-i) or (E-Data constructor) or (E-if)
serverExpr fun_store client_stack evctx (Case (Constr cname locs tys vs argtys) casety alts) server_stack = do
  case [(dname,xs,expr) | Alternative dname xs expr <- alts, cname==dname] of
    ((_,xs,expr):_) -> do
      let subst = zip xs vs
      return $ ServerConfig client_stack evctx (doSubstExpr subst expr) server_stack
      
    [] -> error $ "[serverExpr] Case alternative not found: " ++ cname

serverExpr fun_store evctx client_stack (Case (Lit (BoolLit b)) casety alts) server_stack = do
  let [Alternative b1 _ expr1,Alternative b2 _ expr2] = alts
  let text_b = show b
  if text_b==b1 then return $ ServerConfig client_stack evctx expr1 server_stack
  else if text_b==b2 then return $ ServerConfig client_stack evctx expr2 server_stack
  else error $ "[cilentExpr] Case unexpected: " ++ show b ++ "? " ++ b1 ++ " " ++ b2

-- (E-App)
serverExpr fun_store client_stack evctx (App clo@(Closure vs vstys codename recf) funty arg) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname,(codetyps,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeAbs [(x,_)] expr)):_) -> do
      let subst    = [(x,arg)] ++ zip fvvars vs
      let substLoc = zip locvars locs
      let substTy  = zip tyvars tys
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack evctx substed_expr server_stack

    [] -> error $ "[serverExpr] Server abs code not found: " ++ fname

-- (E-TApp)
serverExpr fun_store client_stack evctx (TypeApp clo@(Closure vs vstys codename recf) funty [argty]) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeTypeAbs [a] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = zip locvars locs
      let substTy  = [(a,argty)] ++ zip tyvars tys
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack evctx substed_expr server_stack

    [] -> error $ "[serverExpr] Server tyabs code not found: " ++ fname ++ "\n"
                      ++ ", " ++ show [gname | (gname,_)<-_serverstore fun_store] ++ "\n"
                      ++ ", " ++ show [gname | (gname,_)<-_clientstore fun_store] ++ "\n"
      
-- (E-LApp)
serverExpr fun_store client_stack evctx (LocApp clo@(Closure vs vstys codename recf) funty [argloc]) server_stack = do
  let CodeName fname locs tys = codename
  case [code | (gname, (codetype,code))<-_serverstore fun_store, fname==gname] of
    ((Code locvars tyvars fvvars (CodeLocAbs [l] expr)):_) -> do
      let subst    = zip fvvars vs
      let substLoc = [(l,argloc)] ++ zip locvars locs
      let substTy  = zip tyvars tys
      let substed_expr = doRec clo recf $ doSubstExpr subst (doSubstTyExpr substTy (doSubstLocExpr substLoc expr))
      return $ ServerConfig client_stack evctx substed_expr server_stack

    [] -> error $ "[serverExpr] Server locabs code not found: " ++ fname

serverExpr fun_store client_stack evctx (Prim primop vs) server_stack = do
  let v = calc primop vs
  return $ ServerConfig client_stack evctx (ValExpr v) server_stack
      

--
serverValue :: Monad m => FunctionStore -> Stack -> [EvalContext] -> Value -> Stack -> m Config

-- (E-Unit-S-E)
serverValue fun_store [] [] (UnitM v) [] =
  return $ ClientConfig [] (ValExpr (UnitM v)) [] []

-- (E-Unit-S)
serverValue fun_store (top_evctx:client_stack) [] (UnitM v) server_stack =
  return $ ClientConfig [] (top_evctx (ValExpr (UnitM v))) client_stack server_stack

-- (E-Call)
serverValue fun_store client_stack evctx (Call f funty arg) server_stack = do
  let server_stack1 = if null evctx then server_stack else (toFun evctx):server_stack
  return $ ClientConfig [] (App f funty arg) client_stack server_stack1

-- (E-Gen-C-C) and (E-Gen-S-C)
serverValue fun_store client_stack evctx (GenApp loc f funty arg) server_stack = do
  if loc==serverLoc then
    return $ ServerConfig client_stack evctx (App f funty arg) server_stack
  else if loc==clientLoc then
    return $ ServerConfig client_stack evctx (ValExpr (Call f funty arg)) server_stack
  else
    error $ "[serverValue] GenApp: Unexpected location : " ++ show loc

-- (E-Do)
serverValue fun_store client_stack evctx (BindM [Binding x ty b@(ValExpr (UnitM v))] expr) server_stack = do
  let subst = [(x,v)]
  return $ ServerConfig client_stack evctx (doSubstExpr subst expr) server_stack

-- ( do x<-E[] in M ) : b is one of BindM, Call, and GenApp.
serverValue fun_store client_stack evctx (BindM [Binding x ty b@(_)] expr) server_stack = do
  serverExpr fun_store client_stack ((\bexpr->ValExpr (BindM [Binding x ty bexpr] expr)):evctx) b server_stack

serverValue fun_store client_stack evctx v server_stack = do
  error $ "[serverValue]: Unexpected: " ++ show v ++ "\n"
                 ++ show [f | (f,_)<-_clientstore fun_store] ++ "\n"
                 ++ show [f | (f,_)<-_serverstore fun_store] ++ "\n"

-----------------------
-- Primitive operations
-----------------------

calc MkRecOp [Closure vs tys codename [], Lit (StrLit f)] = Closure vs tys codename [f]

calc primop vs = Lit $ calc' primop (map (\(Lit lit)-> lit) vs)


calc' :: PrimOp -> [Literal] -> Literal

calc' NotPrimOp [BoolLit b] = BoolLit (not b)

calc' OrPrimOp [BoolLit x, BoolLit y] = BoolLit (x || y)

calc' AndPrimOp [BoolLit x, BoolLit y] = BoolLit (x && y)

calc' EqPrimOp [IntLit x, IntLit y] = BoolLit (x==y)

calc' NeqPrimOp [IntLit x, IntLit y] = BoolLit (x/=y)

calc' LtPrimOp [IntLit x, IntLit y] = BoolLit (x<y)

calc' LePrimOp [IntLit x, IntLit y] = BoolLit (x<=y)

calc' GtPrimOp [IntLit x, IntLit y] = BoolLit (x>y)

calc' GePrimOp [IntLit x, IntLit y] = BoolLit (x>=y)

calc' AddPrimOp [IntLit x, IntLit y] = IntLit (x+y)

calc' SubPrimOp [IntLit x, IntLit y] = IntLit (x-y)

calc' MulPrimOp [IntLit x, IntLit y] = IntLit (x*y)

calc' DivPrimOp [IntLit x, IntLit y] = IntLit (x `div` y)

calc' NegPrimOp [IntLit x] = IntLit (-x)

calc' operator operands =
  error $ "[PrimOp] Unexpected: " ++ show operator ++ " " ++ show operands


--
doRec clo [] expr = expr
doRec (Closure vs tys codename recf) [f] expr = doSubstExpr [(f, Closure vs tys codename [f])] expr
doRec clo recf expr = error $ "[doRec] Unexpected" ++ show clo ++ ", " ++ show recf ++ ", " ++ show expr


----------------
-- Substitutions
----------------

--
elim x subst = [(y,e) | (y,e)<-subst, y/=x]

elims xs subst = foldl (\subst0 x0 -> elim x0 subst0) subst xs


--
doSubstExpr :: [(String,Value)] -> Expr -> Expr

doSubstExpr subst (ValExpr v) = ValExpr (doSubstValue subst v)

doSubstExpr subst (Let bindingDecls expr) =
  let bindingDecls1 =
       map (\(Binding x ty expr) ->
              Binding x ty (doSubstExpr (elim x subst) expr)) bindingDecls
      
      elimed_subst = elims (map (\(Binding x _ _) -> x) bindingDecls) subst

      expr1 = doSubstExpr elimed_subst expr
  in Let bindingDecls1 expr1

doSubstExpr subst (Case v casety [TupleAlternative xs expr]) =
  let subst1 = elims xs subst
  in  Case (doSubstValue subst v) casety
        [TupleAlternative xs (doSubstExpr subst1 expr)]

doSubstExpr subst (Case v casety alts) =
  Case (doSubstValue subst v) casety
     (map (\(Alternative cname xs expr) ->
            let subst1 = elims xs subst
            in  Alternative cname xs (doSubstExpr subst1 expr)) alts)

doSubstExpr subst (App v funty arg) =
  App (doSubstValue subst v) funty (doSubstValue subst arg)

doSubstExpr subst (TypeApp v funty tyargs) =
  TypeApp (doSubstValue subst v) funty tyargs

doSubstExpr subst (LocApp v funty locargs) =
  LocApp (doSubstValue subst v) funty locargs

doSubstExpr subst (Prim op vs) = Prim op (map (doSubstValue subst) vs)



--
doSubstValue :: [(String,Value)] -> Value -> Value

doSubstValue subst (Var x) =
  case [v | (y,v) <- subst, x==y] of
    (v:_) -> v
    []    -> (Var x)

doSubstValue subst (Lit lit) = (Lit lit)

doSubstValue subst (Tuple vs) = Tuple (map (doSubstValue subst) vs)

doSubstValue subst (Constr cname locs tys vs argtys) =
  Constr cname locs tys (map (doSubstValue subst) vs) argtys

doSubstValue subst (Closure vs fvtys (CodeName fname locs tys) recf) =
  Closure (map (doSubstValue subst) vs) fvtys (CodeName fname locs tys) recf

doSubstValue subst (UnitM v) = UnitM (doSubstValue subst v)

doSubstValue subst (BindM bindingDecls expr) =
  let bindingDecls1 =
         (map (\(Binding x ty bexpr) ->
                let subst1 = elim x subst
                in  Binding x ty (doSubstExpr subst1 bexpr))) bindingDecls

      elimed_subst = elims (map (\(Binding x _ _) -> x) bindingDecls) subst
      
      expr1 = doSubstExpr elimed_subst expr
  in  BindM bindingDecls1 expr1

doSubstValue subst (Req f funty arg) =
  Req (doSubstValue subst f) funty (doSubstValue subst arg)

doSubstValue subst (Call f funty arg) =
  Call (doSubstValue subst f) funty (doSubstValue subst arg)

doSubstValue subst (GenApp loc f funty arg) =
  GenApp loc (doSubstValue subst f) funty (doSubstValue subst arg)

--doSubstValue subst v = error $ "[doSubstValue] Unexpected: " ++ show v


--
doSubstLocExpr :: [(String,Location)] -> Expr -> Expr

doSubstLocExpr substLoc (ValExpr v) = ValExpr (doSubstLocValue substLoc v)

doSubstLocExpr substLoc (Let bindingDecls expr) =
  let bindingDecls1 =
       map (\(Binding x ty bexpr) ->
              Binding x
               (doSubstLoc substLoc ty)
                 (doSubstLocExpr substLoc bexpr)) bindingDecls

  in  Let bindingDecls1 (doSubstLocExpr substLoc expr)

doSubstLocExpr substLoc (Case v casety [TupleAlternative xs expr]) =
  Case (doSubstLocValue substLoc v) (doSubstLoc substLoc casety)
    [TupleAlternative xs (doSubstLocExpr substLoc expr)]

doSubstLocExpr substLoc (Case v casety alts) =
  Case (doSubstLocValue substLoc v) (doSubstLoc substLoc casety)
    (map (\(Alternative cname xs expr) ->
            Alternative cname xs (doSubstLocExpr substLoc expr)) alts)

doSubstLocExpr substLoc (App v funty arg) =
  App (doSubstLocValue substLoc v)
        (doSubstLoc substLoc funty)
          (doSubstLocValue substLoc arg)

doSubstLocExpr substLoc (TypeApp v funty tyargs) =
  TypeApp (doSubstLocValue substLoc v)
        (doSubstLoc substLoc funty)
          (map (doSubstLoc substLoc) tyargs)

doSubstLocExpr substLoc (LocApp v funty locargs) =
  LocApp (doSubstLocValue substLoc v)
        (doSubstLoc substLoc funty)
          (map (doSubstLocOverLocs substLoc) locargs)

doSubstLocExpr substLoc (Prim op vs) =
  Prim op (map (doSubstLocValue substLoc) vs)


--
doSubstLocValue :: [(String,Location)] -> Value -> Value

doSubstLocValue substLoc (Var x) = Var x

doSubstLocValue substLoc (Lit lit) = Lit lit

doSubstLocValue substLoc (Tuple vs) = Tuple (map (doSubstLocValue substLoc) vs)

doSubstLocValue substLoc (Constr cname locs tys vs argtys) =
  Constr cname
        (map (doSubstLocOverLocs substLoc) locs)
          (map (doSubstLoc substLoc) tys)
            (map (doSubstLocValue substLoc) vs)
              (map (doSubstLoc substLoc) argtys)

doSubstLocValue substLoc (Closure vs fvtys (CodeName f locs tys) recf) =
  Closure (map (doSubstLocValue substLoc) vs)
    (map (doSubstLoc substLoc) fvtys )
    (CodeName f (map (doSubstLocOverLocs substLoc) locs) (map (doSubstLoc substLoc) tys))
    recf

doSubstLocValue substLoc (UnitM v) = UnitM (doSubstLocValue substLoc v)

doSubstLocValue substLoc (BindM bindingDecls expr) =
  let bindingDecls1 =
         (map (\(Binding x ty bexpr) ->
            Binding x
              (doSubstLoc substLoc ty)
                 (doSubstLocExpr substLoc bexpr))) bindingDecls
  in  BindM bindingDecls1 (doSubstLocExpr substLoc expr)

doSubstLocValue substLoc (Req f funty arg) =
  Req (doSubstLocValue substLoc f)
        (doSubstLoc substLoc funty)
          (doSubstLocValue substLoc arg)

doSubstLocValue substLoc (Call f funty arg) =
  Call (doSubstLocValue substLoc f)
         (doSubstLoc substLoc funty)
           (doSubstLocValue substLoc arg)

doSubstLocValue substLoc (GenApp loc f funty arg) =
  GenApp (doSubstLocOverLocs substLoc loc)
           (doSubstLocValue substLoc f)
             (doSubstLoc substLoc funty)
             (doSubstLocValue substLoc arg)


--
doSubstTyExpr :: [(String,Type)] -> Expr -> Expr

doSubstTyExpr substTy (ValExpr v) = ValExpr (doSubstTyValue substTy v)

doSubstTyExpr substTy (Let bindingDecls expr) =
  let bindingDecls1 =
        map (\(Binding x ty expr) ->
               Binding x (doSubst substTy ty) (doSubstTyExpr substTy expr)) bindingDecls

  in  Let bindingDecls1 (doSubstTyExpr substTy expr)

doSubstTyExpr substTy (Case v casety [TupleAlternative xs expr]) =
  Case (doSubstTyValue substTy v) (doSubst substTy casety)
    [TupleAlternative xs (doSubstTyExpr substTy expr)]

doSubstTyExpr substTy (Case v casety alts) =
  Case (doSubstTyValue substTy v) (doSubst substTy casety)
    (map (\ (Alternative cname xs expr) ->
            Alternative cname xs (doSubstTyExpr substTy expr)) alts)

doSubstTyExpr substTy (App v funty arg) =
  App (doSubstTyValue substTy v) (doSubst substTy funty) (doSubstTyValue substTy arg)

doSubstTyExpr substTy (TypeApp v funty tyargs) =
  TypeApp (doSubstTyValue substTy v) (doSubst substTy funty) (map (doSubst substTy) tyargs)

doSubstTyExpr substTy (LocApp v funty locargs) =
  LocApp (doSubstTyValue substTy v) (doSubst substTy funty) locargs

doSubstTyExpr substTy (Prim op vs) =
  Prim op (map (doSubstTyValue substTy) vs)
  
--
doSubstTyValue :: [(String,Type)] -> Value -> Value


doSubstTyValue substTy (Var x) = (Var x)

doSubstTyValue substTy (Lit lit) = Lit lit

doSubstTyValue substTy (Tuple vs) = Tuple (map (doSubstTyValue substTy) vs)

doSubstTyValue substTy (Constr cname locs tys vs argtys) =
  Constr cname locs
     (map (doSubst substTy) tys)
       (map (doSubstTyValue substTy) vs)
         (map (doSubst substTy) argtys)

doSubstTyValue substTy (UnitM v) = UnitM (doSubstTyValue substTy v)

doSubstTyValue substTy (Closure vs fvtys (CodeName fname locs tys) recf) =
  Closure (map (doSubstTyValue substTy) vs)
          (map (doSubst substTy) fvtys)
          (CodeName fname locs (map (doSubst substTy) tys))
          recf

doSubstTyValue substTy (BindM bindingDecls expr) =
  let bindingDecls1 =
        map (\ (Binding x ty bexpr) ->
               Binding x (doSubst substTy ty) (doSubstTyExpr substTy bexpr)) bindingDecls
  in  BindM bindingDecls1 (doSubstTyExpr substTy expr)


doSubstTyValue substTy (Req f funty arg) =
  Req (doSubstTyValue substTy f) (doSubst substTy funty) (doSubstTyValue substTy arg)

doSubstTyValue substTy (Call f funty arg) =
  Call (doSubstTyValue substTy f) (doSubst substTy funty) (doSubstTyValue substTy arg)

doSubstTyValue substTy (GenApp loc f funty arg) =
  GenApp loc (doSubstTyValue substTy f) (doSubst substTy funty) (doSubstTyValue substTy arg)
