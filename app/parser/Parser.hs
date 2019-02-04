module Parser where

import CommonParserUtil
import Expr

data AST = ASTExpr Expr | ASTExprSeq [Expr]

parserSpec :: ParserSpec
parserSpec = ParserSpec
  {
    startSymbol = "SeqExpr'",
    
    parserSpecList =
    [
      ("SeqExpr' -> SeqExpr", \rhs -> get rhs 1),
      
      ("SeqExpr -> SeqExpr ; AssignExpr", \rhs -> get rhs 1 ++ [get rhs 3]),
      
      ("SeqExpr -> AssignExpr", \rhs -> [get rhs 1]),
      
      ("AssignExpr -> identifier = AssignExpr",
        \rhs -> Assign (getText rhs 1) (get rhs 3)),
      
      ("AssignExpr -> AdditiveExpr", \rhs -> get rhs 1),

      ("AdditiveExpr -> AdditiveExpr + MultiplicativeExpr",
        \rhs -> BinOp Add (get rhs 1) (get rhs 3)),

      ("AdditiveExpr -> AdditiveExpr - MultiplicativeExpr",
        \rhs -> BinOp Sub (get rhs 1) (get rhs 3)),

      ("AssignExpr -> MultiplicativeExpr", \rhs -> get rhs 1),

      ("MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr",
        \rhs -> BinOp Mul (get rhs 1) (get rhs 3)),

      ("MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr",
        \rhs -> BinOp Div (get rhs 1) (get rhs 3)),

      ("MultiplicativeExpr -> PrimaryExpr", \rhs -> get rhs 1),
      
      ("PrimaryExpr -> identifier", \rhs -> Var (getText rhs 1)),

      ("PrimaryExpr -> integer_number", \rhs -> Lit (read (getText rhs 1)),

      ("PrimaryExpr -> ( AssignExpr )", \rhs -> get rhs 2)
    ],
    
    baseDir = "./",
    actionTblFile = "actiontable.txt",
    gotoTblFile = "gototable.txt",
    grammarFile = "grammar.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "genlrparser-exe"
  }


