module CommonParserUtil where

import Terminal
import TokenInterface

import Text.Regex.TDFA
import System.Exit
import System.Process
import Control.Monad

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

--
parsing :: TokenInterface token =>
           ParserSpec token ast -> [Terminal token] -> IO ast
parsing parserSpec terminalList = do
  -- 1. Save the production rules in the parser spec (Parser.hs).
  writtenBool <- saveProdRules specFileName sSym pSpecList

  -- 2. If the grammar file is written,
  --    run the following command to generate prod_rules/action_table/goto_table files.
  --     stack exec -- genlrparser-exe mygrammar.grm -output prod_rules.txt action_table.txt goto_table.txt
  when writtenBool generateAutomaton

  -- 3. Load automaton files (prod_rules/action_table/goto_table.txt)
  (actionTbl, gotoTbl, prodRules) <-
    loadAutomaton grammarFileName actionTblFileName gotoTblFileName

  -- 4. Run the automaton
  ast <- runAutomaton actionTbl gotoTbl prodRules pFunList terminalList
  
  -- putStrLn "done." -- It was for the interafce with Java-version RPC calculus interpreter.
  
  return ast

  where
    specFileName      = parserSpecFile parserSpec
    grammarFileName   = grammarFile    parserSpec
    actionTblFileName = actionTblFile  parserSpec
    gotoTblFileName   = gotoTblFile    parserSpec
    
    sSym      = startSymbol parserSpec
    pSpecList = map fst (parserSpecList parserSpec)
    pFunList  = map snd (parserSpecList parserSpec)

    generateAutomaton = do
      exitCode <- rawSystem "stack"
                  [ "exec", "--",
                    "genlrparser-exe", specFileName, "-output",
                    grammarFileName, actionTblFileName, gotoTblFileName
                  ]
      case exitCode of
        ExitFailure code -> exitWith exitCode
        ExitSuccess -> putStrLn ("Successfully generated: " ++
                                 actionTblFileName ++ ", "  ++
                                 gotoTblFileName ++ ", " ++
                                 grammarFileName);

-- Stack

data StkElem token ast =
    StkState Int
  | StkTerminal (Terminal token)
  | StkNonterminal ast String -- String for printing Nonterminal instead of ast

type Stack token ast = [StkElem token ast]

emptyStack = []

get :: Stack token ast -> Int -> ast
get stack i =
  case stack !! (i-1) of
    StkNonterminal ast _ -> ast
    _ -> error $ "get: out of bound: " ++ show i

getText :: Stack token ast -> Int -> String
getText stack i = 
  case stack !! (i-1) of
    StkTerminal (Terminal text _ _ _) -> text
    _ -> error $ "getText: out of bound: " ++ show i

push :: a -> [a] -> [a]
push elem stack = elem:stack

pop :: [a] -> (a, [a])
pop (elem:stack) = (elem, stack)
pop []           = error "Attempt to pop from the empty stack"

prStack :: TokenInterface token => Stack token ast -> String
prStack [] = "end"
prStack (StkState i : stack) = "S" ++ show i ++ " : " ++ prStack stack
prStack (StkTerminal (Terminal text _ _ token) : stack) =
  fromToken token ++ "(" ++ text ++ ")" ++ " : " ++ prStack stack
prStack (StkNonterminal ast str : stack) = str ++ " : " ++ prStack stack

-- Utility for Automation
currentState :: Stack token ast -> Int
currentState (StkState i : stack) = i
currentState _                    = error "No state found in the stack top"

tokenTextFromTerminal :: TokenInterface token => Terminal token -> String
tokenTextFromTerminal (Terminal _ _ _ token) = fromToken token

lookupActionTable :: TokenInterface token => ActionTable -> Int -> (Terminal token) -> Maybe Action
lookupActionTable actionTbl state terminal =
  lookupTable actionTbl (state,tokenTextFromTerminal terminal)
     ("Not found in the action table: " ++ terminalToString terminal) 

lookupGotoTable :: GotoTable -> Int -> String -> Maybe Int
lookupGotoTable gotoTbl state nonterminalStr =
  lookupTable gotoTbl (state,nonterminalStr)
     ("Not found in the goto table: ")

lookupTable :: (Eq a, Show a) => [(a,b)] -> a -> String -> Maybe b
lookupTable tbl key msg =   
  case [ val | (key', val) <- tbl, key==key' ] of
    [] -> Nothing -- error $ msg ++ " : " ++ show key
    (h:_) -> Just h


-- Note: take 1th, 3rd, 5th, ... of 2*len elements from stack and reverse it!
-- example) revTakeRhs 2 [a1,a2,a3,a4,a5,a6,...]
--          = [a4, a2]
revTakeRhs :: Int -> [a] -> [a]
revTakeRhs 0 stack = []
revTakeRhs n (_:nt:stack) = revTakeRhs (n-1) stack ++ [nt]

-- Automaton

runAutomaton :: TokenInterface token =>
  {- static part -}
  ActionTable -> GotoTable -> ProdRules -> [ParseFun token ast] -> 
  {- dynamic part -}
  [Terminal token] ->
  {- AST -}
  IO ast
runAutomaton actionTbl gotoTbl prodRules pFunList terminalList = do
  let initStack = push (StkState 0) emptyStack
  run terminalList initStack
  
  where
    {- run :: TokenInterface token => [Terminal token] -> Stack token ast -> IO ast -}
    run terminalList stack = do
      let state = currentState stack
      let terminal = head terminalList
      let text  = tokenTextFromTerminal terminal
      let action =
           case lookupActionTable actionTbl state terminal of
             Just action -> action
             Nothing -> error $ ("Not found in the action table: " ++ terminalToString terminal) ++ " : " ++ show (state, tokenTextFromTerminal terminal) ++ "\n" ++ prStack stack ++ "\n"
      
      debug ("\nState " ++ show state)
      debug ("Token " ++ text)
      debug ("Stack " ++ prStack stack)
      
      case action of
        Accept -> do
          debug "Accept"
          
          case stack !! 1 of
            StkNonterminal ast _ -> return ast
            _ -> fail "Not Stknontermianl on Accept"
        
        Shift toState -> do
          debug ("Shift " ++ show toState)
          
          let stack1 = push (StkTerminal (head terminalList)) stack
          let stack2 = push (StkState toState) stack1
          run (tail terminalList) stack2
          
        Reduce n -> do
          debug ("Reduce " ++ show n)
          
          let prodrule   = prodRules !! n
          
          debug ("\t" ++ show prodrule)
          
          let builderFun = pFunList  !! n
          let lhs        = fst prodrule
          let rhsLength  = length (snd prodrule)
          let rhsAst = revTakeRhs rhsLength stack
          let ast = builderFun rhsAst
          let stack1 = drop (rhsLength*2) stack
          let topState = currentState stack1
          let toState =
               case lookupGotoTable gotoTbl topState lhs of
                 Just state -> state
                 Nothing -> error $ ("Not found in the goto table: ") ++ " : " ++ show (topState,lhs) ++ "\n" ++ prStack stack ++ "\n"
  
          let stack2 = push (StkNonterminal ast lhs) stack1
          let stack3 = push (StkState toState) stack2
          run terminalList stack3

flag = False

debug :: String -> IO ()
debug msg = if flag then putStrLn msg else return ()
