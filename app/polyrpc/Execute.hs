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

data Config =
    ClientConfig Expr [EvalContext] [EvalContext]   -- <M;Delta_c | Delta_s
  | ServerConfig [EvalContext] Expr [EvalContext]   -- <Delta_c | M;Delta_s>
--  deriving (Show, Typeable, Data)  


--
execute :: Monad m => GlobalTypeInfo -> FunctionStore -> Expr -> m ()
execute gti funStore mainExpr = return ()


--
initConfig main_expr = ClientConfig main_expr [] []

--
{-
run :: Monad m => Config -> m Config

run (ClientConfig (ValExpr val) client_stack server_stack) = return ()

clientValue (BindM [Binding x ty b@(ValExpr (BindM _ _))] expr) client_stack server_stack = do
  (value1, client_stack1, server_stack1) <- clientValue b client_stack server_stack
  return $ ClientConfig (BindM [Binding x ty (ValExpr value1) expr] expr) client_stack1  server_stack1

clientValue (BindM [Binding x ty b@(ValExpr (UnitM v))] expr) client_stack server_stack = do
  let subst = [(x,v)]
  return $ ClientConfig (doSubstExpr subst expr) client_stack server_stack

clientValue (BindM [Binding x ty b@(ValExpr (Req f fty arg))] expr) client_stack server_stack = do
  let evalCtx = \ret -> BindM [Binding x ty (ValExpr ret)] expr
  return $ ServerConfig (evalCtx:client_stack) (App f fty arg) server_stack

clientValue (BindM [Binding x ty b@(ValExpr (GenApp loc f fty arg))] expr) client_stack server_stack = do
  if loc==clientLoc
  then return $ ClientConfig (BindM [Binding x ty (App f fty arg)] expr) client_stack server_stack
  else return $ ClientConfig (BindM [Binding x ty (ValExpr (Req f fty arg))] expr) client_stack server_stack

clientValue (BindM [Binding x ty b@(ValExpr v)] expr) client_stack server_stack =
  error $ "[clientValue] BindM: Unexpected bound expression: " ++ show (ValExpr v)
  

clientValue (BindM [Binding x ty (Let [Binding x ty0 b@(Let _ _)] expr0)] expr) client_stack server_stack = do
  (value1, client_stack1, server_stack1) <- clientValue b client_stack server_stack
  return (BindM [Binding x ty (Let [Binding x ty0 (ValExpr value1)] expr0)] expr, client_stack1, server_stack1)

clientValue (BindM [Binding x ty b@(Case _ _ _)] expr) client_stack server_stack = return ()

clientValue (BindM [Binding x ty b@(App _ _ _)] expr) client_stack server_stack = return ()

clientValue (BindM [Binding x ty b@(TypeApp _ _ _)] expr) client_stack server_stack = return ()

clientValue (BindM [Binding x ty b@(LocApp _ _ _)] expr) client_stack server_stack = return ()

clientValue (BindM [Binding x ty b@(Prim _ _)] expr) client_stack server_stack = return ()


clientValue evalCtx (Req f fty arg) client_stack server_stack = do
  return $ ServerConfig (evalCtx:client_stack) (App f fty arg) server_stack
-}