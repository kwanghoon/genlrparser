{-# LANGUAGE DeriveGeneric #-}

module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import Type
import Expr
import TypeCheck
-- import Compile

import Text.JSON.Generic
import Text.JSON.Pretty
import Text.PrettyPrint
-- For aeson
--import qualified Data.ByteString.Lazy.Char8 as B
--import Data.Aeson.Encode.Pretty
import Data.Maybe
import System.IO 
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  cmd  <- getCmd args
  let bool  = _flag_print_json cmd
  let files = _files cmd
  let build bool file jsonfile toplevels =
        if bool == False
        then return ()
        else writeFile jsonfile $ render $ pp_value $ toJSON (toplevels :: [TopLevelDecl])
  mapM_ (uncurry doProcess)
    [((build bool file jsonfile), file) | file <- files, let jsonfile = file++".json"]

doProcess cont line = do
  text <- readFile line 
  putStrLn "Lexing..."
  terminalList <- lexing lexerSpec text
  mapM_ (putStrLn) (map terminalToString terminalList)
  putStrLn "Parsing..."
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Dumping..."
  putStrLn $ show $ fromASTTopLevelDeclSeq exprSeqAst
  let toplevelDecls = fromASTTopLevelDeclSeq exprSeqAst
  putStrLn "Type checking..."
  elab_toplevelDecls <- typeCheck toplevelDecls
  putStrLn "Dumping..."
  putStrLn $ show $ elab_toplevelDecls
  -- putStrLn "Compiling..."
  -- cs_toplevelDecls <- compile elab_toplevelDecls
  putStrLn "Success..."
  cont elab_toplevelDecls
  
  
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

--
data Cmd = Cmd { _flag_print_json :: Bool, _files :: [String] }

getCmd :: Monad m => [String] -> m Cmd
getCmd args = collect (Cmd {_flag_print_json=False, _files=[]}) args 

collect :: Monad m => Cmd -> [String] -> m Cmd
collect cmd [] = return cmd
collect cmd ("--output-json":args) = do
  let new_cmd = cmd { _flag_print_json = True }
  collect new_cmd args
collect cmd (arg:args) = do
  let old_files = _files cmd 
  let new_cmd = cmd { _files = old_files ++ [arg] }
  collect new_cmd args

  

