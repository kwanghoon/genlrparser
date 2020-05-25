module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import EmacsServer
import System.IO

import Data.Typeable
import Control.Exception

main :: IO ()
main = do
  emacsServer computeCand
  
  -- text <- readline "Enter text to parse: "
  -- doProcess text

-- Computing candidates for syntax completion

computeCand :: String -> Int -> IO [String]
computeCand str cursorPos = ((do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  return ["successfully parsed"])
  `catch` \e -> case e :: LexError of _ -> return ["lex error"])
  `catch` \e -> case e :: ParseError Token AST of
                  NotFoundAction _ state stk actTbl gotoTbl prodRules pFunList -> do
                    candidates <- compCandidates [] state actTbl gotoTbl prodRules pFunList stk -- return ["candidates"]
                    putStrLn (show candidates)
                    return (map show candidates)
                  NotFoundGoto state _ stk actTbl gotoTbl prodRules pFunList -> do
                    candidates <- compCandidates [] state actTbl gotoTbl prodRules pFunList stk
                    putStrLn (show candidates)
                    return (map show candidates)


-- The normal parser
doProcess text = do
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec text
  mapM_ (putStrLn . terminalToString) terminalList
  putStrLn "Parsing..."
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Pretty Printing..."
  putStrLn (show exprSeqAst)
  
  
readline msg = do
  putStr msg
  hFlush stdout
  readline'

readline' = do
  ch <- getChar
  if ch == '\n' then
    return ""
  else
    do line <- readline'
       return (ch:line)


