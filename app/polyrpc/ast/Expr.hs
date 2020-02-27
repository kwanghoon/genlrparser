module Expr where

import Type

--
data Expr =
    Var String
  | TypeAbs [String] Expr
  | LocAbs [String] Expr
  | Abs [(String, Location)] Expr
  | Let [BindingDecl] Expr
  | Case Expr [Alternative]
  | App Expr Expr
  | TypeApp Expr [Type]
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

data BindingDecl =
    Binding String Type Expr

data DataTypeDecl =
    DataType String [String] [TypeConDecl]

data TopLevelDecl =
    BindingTopLevel BindingDecl
  | DataTypeTopLevel DataTypeDecl

data TypeConDecl =
   TypeCon String [Type]

data Alternative =
  Alternative String [String] Expr


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
  
  | ASTBindingDeclSeq { fromASTBindingDeclSeq :: [BindingDecl] }
  | ASTBindingDecl    { fromASTBindingDecl    :: BindingDecl  }

  | ASTDataTypeDecl { fromASTDataTypeDecl :: DataTypeDecl }

  | ASTTopLevelDeclSeq { fromASTTopLevelDeclSeq :: [TopLevelDecl] }
  
  | ASTTypeConDeclSeq { fromASTTypeConDeclSeq :: [TypeConDecl] }
  | ASTTypeConDecl { fromASTTypeConDecl :: TypeConDecl }
  
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
toASTType ty     = ASTType ty
toASTLocationSeq locations = ASTLocationSeq locations
toASTLocation location     = ASTLocation location

toASTBindingDeclSeq bindings = ASTBindingDeclSeq bindings
toASTBindingDecl binding     = ASTBindingDecl binding

toASTDataTypeDecl datatype     = ASTDataTypeDecl datatype

toASTTopLevelDeclSeq toplevel = ASTTopLevelDeclSeq toplevel

toASTTypeConDeclSeq typecondecls = ASTTypeConDeclSeq typecondecls
toASTTypeConDecl typecondecl     = ASTTypeConDecl typecondecl

toASTIdLocSeq idlocs = ASTIdLocSeq idlocs
toASTIdLoc idloc     = ASTIdLoc idloc

toASTAlternativeSeq alts = ASTAlternativeSeq alts
toASTAlternative alt     = ASTAlternative alt

toASTLit lit     = ASTLit lit

