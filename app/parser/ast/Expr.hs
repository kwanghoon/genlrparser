module Expr where

data AST =
    ASTSeq  { fromAstSeq  :: [Expr] } -- Expr Sequence: Expr1; ... ; Exprn
  | ASTExpr { fromAstExpr :: Expr   }

toAstSeq :: [Expr] -> AST
toAstSeq exprs = ASTSeq exprs

toAstExpr :: Expr -> AST
toAstExpr expr = ASTExpr expr

data Expr =
    Lit { fromLit :: Int }
  | Var { fromVar :: String }
  | BinOp { kindFromBinOp :: BinOpKind,
            leftOpFromBinOp :: Expr,
            rightOpFromBinOp :: Expr }
  | Assign { lhsFromAssign :: String,
             rhsFromAssign :: Expr  }

data BinOpKind = ADD | SUB | MUL | DIV


