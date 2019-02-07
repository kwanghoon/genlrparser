module CommonParserUtil where

import Terminal
import TokenInterface

import Text.Regex.TDFA
import System.Exit
import System.Process

import SaveProdRules
import AutomatonType
import LoadAutomaton

-- Lexer Specification
type RegExpStr    = String
type LexFun token = String -> Maybe token 

type LexerSpecList token  = [(RegExpStr, LexFun token)]
data LexerSpec token =
  LexerSpec { endOfToken    :: token,
              lexerSpecList :: LexerSpecList token
            }

-- Parser Specification
type ProdRuleStr = String
type ParseFun token ast = Stack token ast -> ast

type ParserSpecList token ast = [(ProdRuleStr, ParseFun token ast)]
data ParserSpec token ast =
  ParserSpec { startSymbol    :: String,
               parserSpecList :: ParserSpecList token ast,
               baseDir        :: String,   -- ex) ./
               actionTblFile  :: String,   -- ex) actiontable.txt
               gotoTblFile    :: String,   -- ex) gototable.txt
               grammarFile    :: String,   -- ex) grammar.txt
               parserSpecFile :: String,   -- ex) mygrammar.grm
               genparserexe   :: String    -- ex) genlrparse-exe
             }

-- Specification
data Spec token ast =
  Spec (LexerSpec token) (ParserSpec token ast)

--------------------------------------------------------------------------------  
-- The lexing machine
--------------------------------------------------------------------------------  
type Line = Int
type Column = Int

lexing :: TokenInterface token =>
          LexerSpec token -> String -> IO [Terminal token]
lexing lexerspec text = lexing_ lexerspec 1 1 text

lexing_ :: TokenInterface token =>
           LexerSpec token -> Line -> Column -> String -> IO [Terminal token]
lexing_ lexerspec line col [] = do
  let eot = endOfToken lexerspec 
  return [Terminal (fromToken eot) line col eot]
   
lexing_ lexerspec line col text = do
  (matchedText, theRestText, maybeTok) <-
    matchLexSpec line col (lexerSpecList lexerspec) text
  let (line_, col_) = moveLineCol line col matchedText
  terminalList <- lexing_ lexerspec line_ col_ theRestText
  case maybeTok of
    Nothing  -> return terminalList
    Just tok -> do
      let terminal = Terminal matchedText line col tok
      return (terminal:terminalList)

matchLexSpec :: TokenInterface token =>
                Line -> Column -> LexerSpecList token -> String
             -> IO (String, String, Maybe token)
matchLexSpec line col [] text = do
  putStr $ "No matching lexer spec at "
  putStr $ "Line " ++ show line
  putStr $ "Column " ++ show col
  putStr $ " : "
  putStr $ take 10 text
  exitWith (ExitFailure (-1))

matchLexSpec line col ((aSpec,tokenBuilder):lexerspec) text = do
  let (pre, matched, post) = text =~ aSpec :: (String,String,String)
  case pre of
    "" -> return (matched, post, tokenBuilder matched)
    _  -> matchLexSpec line col lexerspec text


moveLineCol :: Line -> Column -> String -> (Line, Column)
moveLineCol line col ""          = (line, col)
moveLineCol line col ('\n':text) = moveLineCol (line+1) 1 text
moveLineCol line col (ch:text)   = moveLineCol line (col+1) text
  
--------------------------------------------------------------------------------  
-- The parsing machine
--------------------------------------------------------------------------------

-- Stack
data StkElem token ast =
    StkState Int
  | StkTerminal (Terminal token)
  | StkNonterminal ast 

type Stack token ast = [StkElem token ast]

get :: Stack token ast -> Int -> ast
get stack i =
  case stack !! (i-1) of
    StkNonterminal ast -> ast
    _ -> error $ "get: out of bound: " ++ show i

getText :: Stack token ast -> Int -> String
getText stack i = 
  case stack !! (i-1) of
    StkTerminal (Terminal text _ _ _) -> text
    _ -> error $ "getText: out of bound: " ++ show i

-- Automaton

--
parsing :: TokenInterface token =>
           ParserSpec token ast -> [Terminal token] -> IO ()
parsing parserSpec terminalList = do
  -- 1. Save the production rules in the parser spec (Parser.hs).
  saveProdRules specFileName sSym pSpecList

  -- 2. Run the following command to generate prod_rules/action_table/goto_table files.
  --     stack exec -- genlrparser-exe mygrammar.grm -output prod_rules.txt action_table.txt goto_table.txt
  exitCode <- rawSystem "stack"
    [ "exec", "--",
      "genlrparser-exe", specFileName, "-output",
      grammarFileName, actionTblFileName, gotoTblFileName
    ]
  case exitCode of
    ExitFailure code -> exitWith exitCode
    ExitSuccess -> putStrLn "action table/goto table/grammar files are successfully generated..."

  -- 3. Load automaton files (prod_rules/action_table/goto_table.txt)
  --
  (actionTbl, gotoTbl, prodRules) <- loadAutomaton grammarFileName actionTblFileName gotoTblFileName
  
  putStrLn "done."
  where
    specFileName      = parserSpecFile parserSpec
    grammarFileName   = grammarFile    parserSpec
    actionTblFileName = actionTblFile  parserSpec
    gotoTblFileName   = gotoTblFile    parserSpec
    
    sSym      = startSymbol parserSpec
    pSpecList = map fst (parserSpecList parserSpec)

