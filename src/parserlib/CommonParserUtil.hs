module CommonParserUtil where

import Terminal
import TokenInterface

import Text.Regex.TDFA
import System.Exit

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
type ParseFun ast = () -> ast

type ParserSpecList ast = [(ProdRuleStr, ParseFun ast)]
data ParserSpec ast = ParserSpec (ParserSpecList ast)

-- Specification
data Spec token ast =
  Spec (LexerSpec token) (ParserSpec ast)

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
parsing x = x

