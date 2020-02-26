module Parser where

import CommonParserUtil
import Token
import Expr


parserSpec :: ParserSpec Token AST
parserSpec = ParserSpec
  {
    startSymbol = "TopLevel'",
    
    parserSpecList =
    [
      ("TopLevel' -> TopLevel", \rhs -> get rhs 1),

      ("Identifiers -> identifier", \rhs -> toASTIdSeq [get rhs 1] ),

      ("Identifiers -> identifier Identifiers",
        \rhs -> toASTIdSeq (get rhs 1 : fromASTIdSeq (get rhs 2)) ),

      ("OptIdentifiers -> ", \rhs -> toASTIdSeq [] ),

      ("OptIdentifiers -> Identifiers", \rhs -> get rhs 1 ),

      ("Type -> PolyAbsType", \rhs -> get rhs 1 ),

      ("Type -> PolyAbsType LocFun Type",
        \rhs -> toASTType (FunType
	     		  (fromASTType (get rhs 1))
			  (fromASTLocation (get rhs 2))
			  (fromASTType (get rhs 3)) ),

      ("PolyAbsType -> { Identifiers } . PolyAbsType",
        \rhs -> ToASTType (LocAbs
	                  (fromASTIdSeq (get rhs 2))
			  (fromASTExpr (get rhs 5)) ),

      ("PolyAbsType -> [ Identifiers ] . PolyAbsType",
        \rhs -> ToASTType (TypeAbs
	                  (fromASTIdSeq (get rhs 2))
			  (fromASTExpr (get rhs 5)) ),

      ("PolyAbsType -> PrimaryType", \rhs -> get rhs 1),

      ("PrimaryType -> Unit", \rhs -> toASTType UnitType),

      ("PrimaryType -> Int", \rhs -> toASTType IntType),

      ("PrimaryType -> Bool", \rhs -> toASTType BooleanType),

      ("PrimaryType -> String", \rhs -> toASTType StringType),

      ("PrimaryType -> identifer", \rhs -> toASTType (TypeVarType (fromASTId (get rhs 1))) ),

      ("PrimaryType -> TupleType", \rhs -> get rhs 1 ),

      ("PrimaryType -> ( Type )", \rhs -> get rhs 2 ),

      ("TupleType -> ( Type , TypeSeq )",
        \rhs -> ToASTType (TupleType $
	    (fromASTType (get rhs 2)) : (fromASTTypeSeq (get rhs 4))) ),

      ("TypeSeq -> Type", \rhs -> toASTTypeSeq [get rhs 1] ),

      ("TypeSeq -> Type , TypeSeq",
        \rhs -> toASTTypeSeq $ fromASTType (get rhs 1) : (fromASTTypeSeq (get rhs 3)) ),

      ("Types -> Type", \rhs -> toASTTypeSeq [fromASTType (get rhs 1)] ),

      ("Types -> Type Types",
        \rhs -> toASTTypeSeq $ fromASTType (get rhs 1) : fromASTType (get rhs 2) ),

      ("OptTypes -> ", \rhs -> toASTTypeSeq [] ),

      ("OptTypes -> Types", \rhs -> get rhs 1 ),

      ("TopLevel -> Binding",
        \rhs -> toASTTopLevel [BindingDecl (fromASTBinding (get rhs 1 ))] ),

      ("TopLevel -> Binding ; TopLevel",
        \rhs -> toASTTopLevel
            $ BindingDecl (fromASTBinding (get rhs 1)) : (fromASTTopLevel (get rhs 3)) ),

      ("TopLevel -> DataTypeDecl",
        \rhs -> toASTTopLevel (DataTypeDecl (fromASTDataType (get rhs 1))) )

      ("TopLevel -> DataTypeDecl ; TopLevel",
        \rhs -> toASTTopLevel
	    $ DataTypeDecl (fromASTDataType (get rhs 1)) : (fromASTTopLevel (get rhs 3)) ),

      ("DataTypeDecl -> data Identifiers = { TypeConDecls }",
        \rhs ->
	    let ids = fromASTIdSeq (get rhs 2)
	    in  toASTDataType (DataType (head ids) (tail ids))
	         (fromASTTypeConDeclSeq (get rhs 5)) ),

      ("TypeConDecl -> identifier OptTypes",
        \rhs -> toASTTypeConDecl (TypeConDecl (fromASTId (get rhs 1))
	         (fromASTTypeSeq (get rhs 2))) ),

      ("TypeConDecls -> TypeConDecl",
        \rhs -> toASTTypeConDeclSeq [ fromASTTypeConDecl (ge rhs 1) ] ),

      ("TypeConDecls -> TypeConDecl | TypeConDecls",
        \rhs -> toASTTypeConDeclSeq $
	          fromASTTypeConDecl (ge rhs 1) : fromASTTypeConDeclSeq (get rhs 3) ),

      ("Binding -> identifier : Type = LExpr",
        \rhs -> toASTBinding (
	          Binding (fromASTId (get rhs 1))
		          (fromASTType (get rhs 3)) (fromASTExpr (get rhs 5))) ),

      ("Bindings -> Binding",
        \rhs -> toASTBindingSeq [ fromBinding (get rhs 1) ] ),

      ("Bindings -> Binding ; Bindings",
        \rhs -> toASTBindingSeq $ fromBinding (get rhs 1) : fromBindingSeq (get rhs 3) ),

      ("LExpr -> { Identifiers } . LExpr",
        \rhs -> toAstExp (LocAbs (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 5))) ),

      ("LExpr -> [ Identifiers ] . LExpr",
        \rhs -> toAstExp (TypeAbs (fromASTIdSeq (get rhs 2)) (fromASTExpr (get rhs 5))) ),

      ("LExpr -> \ IdLocSeq . LExpr",
        \rhs -> toAstExp (Abs (fromASTIdLocSeq (get rhs 2)) (fromASTExpr (get rhs 4))) ),

      ("LExpr -> let { Bindings } LExpr end",
        \rhs -> toAstExp (Let (fromASTBindingSeq (get rhs 3)) (fromASTExpr (get rhs 5))) ),

      ("LExpr -> if Expr then LExpr else LExpr",
        \rhs -> toAstExp (Case (fromASTExpr (get rhs 2))
	          [ Alternative trueLit  [] (fromASTExpr (get rhs 4))
		  , Alternative falseLit [] (fromASTExpr (get rhs 6)) ]) ),

      ("LExpr -> case Expr { Alternatives }",
        \rhs -> toAstExp (Case (fromASTExpr (get rhs 2)) (fromASTAlternativeSeq (get rhs 4))) ),

      ("LExpr -> Expr", \rhs -> get rhs 1 ),

      ("IdLocSeq -> IdLoc", \rhs -> toASTIdLocSeq [fromASTId (get rhs 1)] ),

      ("IdLocSeq -> IdLoc IdLocSeq",
        \rhs -> toASTIdLocSeq $ fromASTIdLoc (get rhs 1) : fromASTIdLocSeq (get rhs 2) ),

      ("IdLoc -> identifier @ Location",
        \rhs -> toASTIdLoc (fromASTId (get rhs 1), fromASTLocation (get rhs 3)) ),


      ("Location -> identifier", \rhs -> toASTLocation (Location fromASTId (get rhs 1)) ),

      ("Alternatives -> Alternative", \rhs -> toASTAlternativeSeq [fromASTAlternative (get rhs 1)] ),

      ("Alternatives -> Alternative ; Alternatives",
        \rhs -> toASTAlternativeSeq $ fromASTAlternative (get rhs 1) : fromASTAlternativeSeq (get rhs 3) ),

      ("Expr -> Expr Term",
        \rhs -> toASTExpr (App (fromASTExpr (get rhs 1)) (fromASTExpr (get rhs 2))) ),

      ("Expr -> Expr [ Identifiers ]",
        \rhs -> toASTExpr (TypeApp (fromASTExpr (get rhs 1)) (fromASTIdSeq (get rhs 3))) ),

      ("Expr -> Expr { Identifiers }",
        \rhs -> toASTExpr (LocApp (fromASTExpr (get rhs 1)) (fromASTIdSeq (get rhs 3))) ),

      ("Expr -> Tuple", \rhs -> get rhs 1 ),

      ("Expr -> ConditionalExpr", \rhs -> get rhs 1 ),

      ("Tuple -> ( LExpr , LExprSeq )",
        \rhs -> toASTExpr (Tuple $ fromASTExpr (get rhs 2) : fromASTExprSeq (get rhs 4)) ),

      ("LExprSeq -> LExpr", \rhs -> get rhs 1 ),

      ("LExprSeq -> LExpr , LExprSeq",
        \rhs -> toASTExprSeq ( fromASTExpr (get rhs 1) : fromASTExprSeq (get rhs 3)) ),

      ("ConditionalExpr -> LogicNot", \rhs -> get rhs 1 ),

      ("LogicNot -> ! LogicNot", \rhs -> toASTExpr (Prim NotPrimOp [fromASTExpr (get rhs 2)]) ),

      ("LogicNot -> LogicNot", \rhs -> get rhs 1 ),

      ("LogicOr -> LogicOr or LogicAnd",
        \rhs -> toASTExpr (Prim OrPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("LogicOr -> LogicAnd", \rhs -> get rhs 1),

      ("LogicAnd -> LogicAnd and CompEqNeq",
        \rhs -> toASTExpr (Prim AndPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("LogicAnd -> CompEqNeq", \rhs -> get rhs 1),

      ("CompEqNeq -> CompEqNeq == Comp",
        \rhs -> toASTExpr (Prim EqPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("CompEqNeq -> CompEqNeq != Comp",
        \rhs -> toASTExpr (Prim NeqPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("CompEqNeq -> Comp", \rhs -> get rhs 1 ),

      ("Comp -> Comp < ArithAddSub",
        \rhs -> toASTExpr (Prim LtPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp <= ArithAddSub",
        \rhs -> toASTExpr (Prim LePrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp > ArithAddSub",
        \rhs -> toASTExpr (Prim GtPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> Comp >= ArithAddSub",
        \rhs -> toASTExpr (Prim GePrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("Comp -> ArithAddSub", \rhs -> get rhs 1 ),

      ("ArithAddSub -> ArithAddSub + ArithMulDiv",
        \rhs -> toASTExpr (Prim AddPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithAddSub -> ArithAddSub - ArithMulDiv",
        \rhs -> toASTExpr (Prim SubPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithAddSub -> ArithMulDiv", \rhs -> get rhs 1 ),

      ("ArithMulDiv -> ArithMulDiv * ArithUnary",
        \rhs -> toASTExpr (Prim MulPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithMulDiv -> ArithMulDiv / ArithUnary",
        \rhs -> toASTExpr (Prim DivPrimOp [fromASTExpr (get rhs 1), fromASTExpr (get rhs 3)]) ),

      ("ArithMulDiv -> ArithUnary", \rhs -> get rhs 1 ),

      ("ArithUnary -> - Term", \rhs -> toASTExpr (Prim NegPrimOp [fromASTExpr (get rhs 2)]) ),

      ("ArithUnary -> Term", \rhs -> get rhs 1 ),

      ("Term -> identifier", \rhs -> toASTExpr (Var (getText rhs 1)) ),

      ("Term -> integer", \rhs -> toASTExpr (Const (IntLit (read (getText rhs 1)))) ),

      ("Term -> string", \rhs -> toASTExpr (Const (StrLit (getText rhs 1))) ),
      
      ("Term -> boolean", \rhs -> toASTExpr (Const (BoolLit (read (getText rhs 1)))) ),

      ("Term -> ( )", \rhs -> toASTExpr (Const UnitLit) ),

      ("Term -> ( LExpr )", \rhs -> get rhs 2 )
    ],
    
    baseDir = "./",
    actionTblFile = "action_table.txt",  
    gotoTblFile = "goto_table.txt",
    grammarFile = "prod_rules.txt",
    parserSpecFile = "mygrammar.grm",
    genparserexe = "genlrparser-exe"
  }
