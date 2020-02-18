## An LALR(1) Parser Builder

Copyright (C) 2013- Kwanghoon Choi


### (1) A Parser Table Generator

- LR(1)/ LALR(1) parser tables
- Grammar files written in Haskell datatype CFG

How to run an automation generator (genlrparser-exe):
~~~~
  $ stack build
  
  $ cat grm/example.grm
  CFG "S'" [
    ProductionRule "S'" [Nonterminal "S"],
    ProductionRule "S" [Nonterminal "C", Nonterminal "C"],
    ProductionRule "C" [Terminal "c", Nonterminal "C"],
    ProductionRule "C" [Terminal "d"]
  ]
  
  $ stack exec genlrparser-exe grm/example.grm
  7 states
  I0:
  [S' -> .S, $]
  [S -> .C C, $]
  [C -> .c C, c]
  [C -> .c C, d]
  [C -> .d, c]
  [C -> .d, d]

  I1:
  [S' -> S., $]

  I2:
  [S -> C.C, $]
  [C -> .c C, $]
  [C -> .d, $]

  I3:
  [C -> c.C, $]
  [C -> c.C, c]
  [C -> c.C, d]
  [C -> .c C, $]
  [C -> .d, $]
  [C -> .c C, c]
  [C -> .d, c]
  [C -> .c C, d]
  [C -> .d, d]

  I4:
  [C -> d., $]
  [C -> d., c]
  [C -> d., d]

  I5:
  [S -> C C., $]

  I6:
  [C -> c C., $]
  [C -> c C., c]
  [C -> c C., d]


  0: S' -> S             <--- prod_rules.txt
  1: S -> C C
  2: C -> c C
  3: C -> d
  
  0	c	Shift 3  <--- action_table.txt
  0	d	Shift 4
  1	$	Accept
  2	c	Shift 3
  2	d	Shift 4
  3	c	Shift 3
  3	d	Shift 4
  4	$	Reduce 3
  4	c	Reduce 3
  4	d	Reduce 3
  5	$	Reduce 1
  6	$	Reduce 2
  6	c	Reduce 2
  6	d	Reduce 2

  0	S	1         <--- goto_table.txt
  0	C	2
  2	C	5
  3	C	6

  $ stack exec genlrparser grm/example.grm -output prod_rules.txt action_table.txt goto_table.txt

  ... to write the automaton printed above in the three text files ...
~~~~


### (2) A Parser Builder in Haskell
- Not a parser generator
- A library-based parser builder
- Write your LALR(1) parser in Haskell, not in Happy (+ Alex) nor in any other parser language

How to write and run a parser :

~~~~
  $ stack build

  $ ls app/parser/*.hs
  app/parser/Lexer.hs  app/parser/Main.hs  app/parser/Parser.hs  app/parser/Token.hs

  $ cat app/parser/Lexer.hs
  module Lexer(lexerSpec) where

  import Prelude hiding (EQ)
  import CommonParserUtil
  import Token

  mkFn :: Token -> (String -> Maybe Token)
  mkFn tok = \text -> Just tok

  skip :: String -> Maybe Token
  skip = \text -> Nothing

  lexerSpec :: LexerSpec Token
  lexerSpec = LexerSpec
    {
      endOfToken    = END_OF_TOKEN,
      lexerSpecList = 
        [ ("[ \t\n]", skip),
          ("[0-9]+" , mkFn INTEGER_NUMBER),
          ("\\("    , mkFn OPEN_PAREN),
          ("\\)"    , mkFn CLOSE_PAREN),
          ("\\+"    , mkFn ADD),
          ("\\-"    , mkFn SUB),
          ("\\*"    , mkFn MUL),
          ("\\/"    , mkFn DIV),
          ("\\="    , mkFn EQ),
          ("\\;"    , mkFn SEMICOLON),
          ("[a-zA-Z][a-zA-Z0-9]*"    , mkFn IDENTIFIER)
        ]
    } 


  $ cat app/parser/Parser.hs

  module Parser where

  import CommonParserUtil
  import Token
  import Expr


  parserSpec :: ParserSpec Token AST
  parserSpec = ParserSpec
    {
      startSymbol = "SeqExpr'",
    
      parserSpecList =
      [
        ("SeqExpr' -> SeqExpr", \rhs -> get rhs 1),
      
        ("SeqExpr -> SeqExpr ; AssignExpr",
          \rhs -> toAstSeq (
            fromAstSeq (get rhs 1) ++ [fromAstExpr (get rhs 3)]) ),
      
        ("SeqExpr -> AssignExpr", \rhs -> toAstSeq [fromAstExpr (get rhs 1)]),
      
        ("AssignExpr -> identifier = AssignExpr",
          \rhs -> toAstExpr (Assign (getText rhs 1) (fromAstExpr (get rhs 3))) ),
      
        ("AssignExpr -> AdditiveExpr", \rhs -> get rhs 1),

        ("AdditiveExpr -> AdditiveExpr + MultiplicativeExpr",
          \rhs -> toAstExpr (
            BinOp Expr.ADD (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

        ("AdditiveExpr -> AdditiveExpr - MultiplicativeExpr",
          \rhs -> toAstExpr (
            BinOp Expr.SUB (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

        ("AdditiveExpr -> MultiplicativeExpr", \rhs -> get rhs 1),

        ("MultiplicativeExpr -> MultiplicativeExpr * PrimaryExpr",
          \rhs -> toAstExpr (
            BinOp Expr.MUL (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

        ("MultiplicativeExpr -> MultiplicativeExpr / PrimaryExpr",
          \rhs -> toAstExpr (
            BinOp Expr.DIV (fromAstExpr (get rhs 1)) (fromAstExpr (get rhs 3))) ),

        ("MultiplicativeExpr -> PrimaryExpr", \rhs -> get rhs 1),
      
        ("PrimaryExpr -> identifier", \rhs -> toAstExpr (Var (getText rhs 1)) ),

        ("PrimaryExpr -> integer_number",
          \rhs -> toAstExpr (Lit (read (getText rhs 1))) ),

        ("PrimaryExpr -> ( AssignExpr )", \rhs -> get rhs 2)
      ],
    
      baseDir = "./",
      actionTblFile = "action_table.txt",  
      gotoTblFile = "goto_table.txt",
      grammarFile = "prod_rules.txt",
      parserSpecFile = "mygrammar.grm",
      genparserexe = "genlrparser-exe"
    }

  $ cat app/parser/example/oneline.arith
  1 + 2 - 3 * 4 / 5
  
  $ cat app/parser/example/multiline.arith
  x = 123;
  x = x + 1;
  y = x; 
  y = y - 1 * 2 / 3;
  z = y = x

  $ stack exec parser-exe
  Enter your file: app/parser/example/oneline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  ((1 + 2) - ((3 * 4) / 5))
  
  $ stack exec parser-exe
  Enter your file: app/parser/example/multiline.arith
  Lexing...
  Parsing...
  done.
  Pretty Printing...
  (x = 123); (x = (x + 1)); (y = x); (y = (y - ((1 * 2) / 3))); (z = (y = x))
~~~~


### (3) Documents

- [Parser generators sharing LR automaton generators and accepting general-purpose programming language-based specifications, J. of KIISE, 47(1), January 2020](http://swlab.jnu.ac.kr/paper/kiise202001.pdf) Written in Korean.

- This LALR(1) parser automation generator library is used by [Java parser](https://github.com/kwanghoon/swlab_parser_builder), [C++ parser](https://github.com/tlsdorye/swlab-parser-lib), and [Python parser](https://github.com/limjintack/swlab_parser_python).

- Parser in action
  * <img src="https://github.com/kwanghoon/genlrparser/blob/master/doc/parserinaction.png"/>

- Parser tool architecture
  * <img src="https://github.com/kwanghoon/genlrparser/blob/master/doc/parsertoolarchitecture.png"/>

- [A topdown approach to writing a compiler](https://github.com/kwanghoon/swlab_parser_builder/blob/master/doc/tutorial_swlab_parser_builder.txt) Written in Korean.
