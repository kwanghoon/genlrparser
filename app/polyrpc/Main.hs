{-# LANGUAGE DeriveGeneric #-}

module Main where

import CommonParserUtil

import Token
import Lexer
import Terminal
import Parser
import Type
import Expr
import qualified CSType as TT
import qualified CSExpr as TE
import TypeCheck
import Compile
import Verify
import Execute

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
  
  let files = _files cmd
  
  mapM_ (doProcess cmd) files -- [ ((build cmd file), file) | file <- files ]

doProcess cmd file = do
  putStrLn $ "[Reading] " ++ file
  text <- readFile file

  putStrLn "[Lexing]"
  terminalList <- lexing lexerSpec text
  mapM_ (putStrLn) (map terminalToString terminalList)

  putStrLn "[Parsing]"
  exprSeqAst <- parsing parserSpec terminalList
  putStrLn "Dumping..."
  putStrLn $ show $ fromASTTopLevelDeclSeq exprSeqAst
  let toplevelDecls = fromASTTopLevelDeclSeq exprSeqAst

  putStrLn "[Type checking]"
  (gti, elab_toplevelDecls) <- typeCheck toplevelDecls
  putStrLn "Dumping..."
  putStrLn $ show $ elab_toplevelDecls

  print_rpc cmd file elab_toplevelDecls

  putStrLn "[Compiling]"
  (t_gti, funStore, t_expr) <- compile gti elab_toplevelDecls
  putStrLn "Dumping...\nGlobal type information:\n"
  putStrLn $ (show t_gti ++ "\n\nFunction stores:")
  putStrLn $ (show funStore ++ "\n\nMain expression:")
  putStrLn $ (show t_expr ++ "\n")

  print_cs cmd file funStore t_expr

  putStrLn "[Verifying generated codes]"
  verify t_gti funStore t_expr
  putStrLn "[Well-typed]"

  putStrLn "[Executing codes]"
  v <- execute t_gti funStore t_expr
  putStrLn $ "[Result]\n" ++ show v

  putStrLn "[Success]"

--
print_rpc cmd file elab_toplevelDecls = do
  let jsonfile = prefixOf file ++ ".json"
  if _flag_print_rpc_json cmd
  then do putStrLn $ "Writing to " ++ jsonfile
          writeFile jsonfile $ render
             $ pp_value $ toJSON (elab_toplevelDecls :: [TopLevelDecl])
  else return ()

print_cs cmd file funStore t_expr = do
  let jsonfile = prefixOf file ++ "_cs.json"
  if _flag_print_cs_json cmd
  then do putStrLn $ "Writing to " ++ jsonfile
          writeFile jsonfile $ render
             $ pp_value $ toJSON (funStore :: TE.FunctionStore, t_expr :: TE.Expr)
  else return ()

prefixOf str = reverse (removeDot (dropWhile (/='.') (reverse str)))
  where removeDot []     = []
        removeDot (x:xs) = xs  -- x must be '.'

--
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
data Cmd = Cmd { _flag_print_rpc_json :: Bool
               , _flag_print_cs_json :: Bool
               , _files :: [String] }

initCmd =
  Cmd { _flag_print_rpc_json = False
      , _flag_print_cs_json  = False
      , _files = []
      }

getCmd :: Monad m => [String] -> m Cmd
getCmd args = collect initCmd args 

collect :: Monad m => Cmd -> [String] -> m Cmd
collect cmd [] = return cmd
collect cmd ("--output-json":args) = do
  let new_cmd = cmd { _flag_print_rpc_json = True }
  collect new_cmd args
collect cmd ("--output-rpc-json":args) = do  
  let new_cmd = cmd { _flag_print_rpc_json = True }
  collect new_cmd args
collect cmd ("--output-cs-json":args) = do  
  let new_cmd = cmd { _flag_print_cs_json = True }
  collect new_cmd args
collect cmd (arg:args) = do
  let old_files = _files cmd 
  let new_cmd = cmd { _files = old_files ++ [arg] }
  collect new_cmd args

  

