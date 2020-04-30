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

--
computeCand :: String -> Int -> IO [String]
computeCand str cursorPos = (do
  terminalList <- lexing lexerSpec str 
  ast <- parsing parserSpec terminalList 
  return ["successfully parsed"])
  `catch` \e -> case e :: LexError of _ -> return ["lex error"]
  `catch` \e -> case e :: ParseError Token AST of _ -> return ["candidates"]


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


