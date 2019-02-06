module Main where

-- import Lib
-- import GenLRParserTable
import CommonParserUtil

import Lexer
import Terminal
import Parser

main :: IO ()
main = do
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec "123 + x123 \n * z"
  mapM_ putStrLn $ map terminalToString terminalList
  putStrLn "Parsing..."
  parsing parserSpec terminalList
  
