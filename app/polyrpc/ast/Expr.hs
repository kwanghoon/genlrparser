module Expr where

import Type

--
data AST =
    ASTExprSeq { fromASTExprSeq :: [Expr] }
  | ASTExpr    { fromASTExpr    :: Expr   }
  | ASTIdSeq   { fromASTIdSeq   :: [String] }
  | ASTId      { fromASTId      :: String }
  | ASTTypeSeq { fromASTTypeSeq :: [Type] }
  | ASTType    { fromASTType    :: Type  }
  | ASTLocationSeq { fromASTLocationSeq :: [Location] }
  | ASTLocation    { fromASTLocation    :: Location  }
  | ASTTopLevel { fromASTTopLevel :: [TopLevel] }
  | ASTBindingSeq { fromASTBindingSeq :: [Binding] }
  | ASTBinding    { fromASTBinding    :: Binding  }
  | ASTBindingSeq { fromASTBindingSeq :: [Binding] }
  | ASTBinding    { fromASTBinding    :: Binding  }
  | ASTTypeConDeclSeq { fromASTTypeConDeclSeq :: [TypeConDecl] }
  | ASTTypeConDecl { fromASTTypeConDecl :: TypeConDecl }
  | ASTDataType { fromASTDataType :: DataType }
  | ASTIdLocSeq { fromASTIdLocSeq :: [(String,Location)] }
  | ASTIdLoc { fromASTIdLoc :: (String,Location) }
  | ASTAlternativeSeq { fromASTAlternativeSeq :: [Alternative] }
  | ASTAlternative { fromASTAlternative :: Alternative }
  | ASTLit { fromASTLit :: Lit }

toASTExprSeq exprs = ASTExprSeq exprs
toASTExpr expr     = ASTExpr expr
toASTIdSeq   ids   = ASTIdSeq ids
toASTId   id       = ASTId id
toASTTypeSeq types = ASTTypeSeq types
toASTType type     = ASTType type
toASTLocationSeq locations = ASTLocationSeq locations
toASTLocation location     = ASTLocation location
toASTTopLevel toplevel = ASTTopLevel toplevel
toASTBindingSeq bindings = ASTBindingSeq bindings
toASTBinding binding     = ASTBinding binding
toASTTypeConDeclSeq typecondecls = ASTTypeConDeclSeq typecondecls
toASTTypeConDecl typecondecl     = ASTTypeConDecl typecondecl
toASTDataType datatype     = ASTDataType datatype
toASTIdLocSeq idlocs = ASTIdLocSeq idlocs
toASTIdLoc idloc     = ASTIdLoc idloc
toASTAlternativeSeq alts = ASTAlternativeSeq alts
toASTIdLoc alt     = ASTAlternative alt
toASTLit lit     = ASTLit lit

--
data Expr =
    Var String
  | TypeAbs [String] Expr
  | LocAbs [String] Expr
  | Abs [(String, Location)] Expr
  | Let [Binding] Expr
  | Case Expr [Alternative]
  | App Expr Expr
  | TypeApp Expr [String]
  | LocApp Expr [String]
  | Tuple [Expr]
  | Prim PrimOp [Expr]
  | Const Lit

data Lit =
    IntLit Int
  | StrLit String
  | BoolLit Bool
  | UnitLit

trueLit  = "True"
falseLit = "False"
unitLit  = "()"

data PrimOp =
    NotPrimOp
  | OrPrimOp
  | AndPrimOp
  | EqPrimOp
  | NeqPrimOp
  | LtPrimOp
  | LePrimOp
  | GtPrimOp
  | GePrimOp
  | AddPrimOp
  | SubPrimOp
  | MulPrimOp
  | DivPrimOp
  | NegPrimOp

data Binding =
    Binding String Type Expr

data DataType
    DataType String [String] [TypeConDecl]

data TopLevelDecl =
    BindingDecl Binding
  | DataTypeDecl DataType

data TypeConDecl =
   TypeConDecl String [Type]

data Alternative =
  Alternative String [String] Expr
