module ReadGrammar where

import CFG

import Data.List(intersperse)
import System.IO
import System.Environment (getArgs)

readGrammar :: Monad m => [String] -> m [ProductionRule]
readGrammar lines = do
  lhsRhssPairList <- rep NoState lines
  let nonterminals = map fst lhsRhssPairList
  return $ concat (map (convert nonterminals) lhsRhssPairList)

-- Checking
convert :: [String] -> (String, [[String]]) -> [ProductionRule]
convert nonterminals (lhs, rhss) =
  map (\rhs -> ProductionRule lhs
               (map (\s -> if s `elem` nonterminals
                           then Nonterminal s
                           else Terminal s) rhs)) rhss

-- Parsing
data State =
    NoState
  | Lhs String
  | Rule String [[String]]
  deriving Eq

-- Note
--  * take the first word. After that, it may be regarded as a comment.
begin :: Monad m => State -> [Char] -> m State
begin NoState  []       = return NoState
begin NoState  (';':cs) = return NoState
begin NoState cs        = return (Lhs (takeWord cs))
begin (Lhs lhs) (' ':' ':'=':[])
  = return (Rule lhs [[]])
begin (Lhs lhs) (' ':' ':'=':' ':cs)
  = return (Rule lhs [words cs])
begin (Rule lhs rhs) []
  = return NoState
begin (Rule lhs rhs) (' ':' ':'|':' ':cs)
  = return (Rule lhs (rhs ++ [words cs]))
begin (Lhs lhs) cs = error $ "begin: Lhs " ++ lhs ++ " with " ++ cs
begin (Rule lhs rhs) cs
  | words cs == [] = return NoState
  | otherwise      = error $ "begin: Rule " ++ lhs ++ " with " ++ cs

takeWord :: String -> String
takeWord []        = []
takeWord (' ':cs)  = []
takeWord ('\t':cs) = []
takeWord (c:cs)    = c : takeWord cs

rep :: Monad m => State -> [String] -> m [(String, [[String]])]
rep NoState             [] = return []
rep (Lhs lhs)           [] = error "rep: Can't end with Lhs"
rep (Rule lhs rhss) [] = return [(lhs, rhss)]
rep state (s:ss) = do
  state1 <- begin state s
  lhsRhsPairList <- rep state1 ss
  case (state, state1) of
    (NoState, NoState) -> return lhsRhsPairList
    (NoState, Lhs lhs) -> return lhsRhsPairList
    (NoState, Rule lhs rhss) -> error "rep: Nostate can't change to Rule lhs rhss."
    (Lhs lhs, NoState) -> error $ "rep: Lhs " ++ lhs ++ " can't change to Nostate."
    (Lhs lhs, Lhs lhs') -> error "rep: Lhs lhs can't change to itself."
    (Lhs lhs, Rule _ _) -> return lhsRhsPairList
    (Rule lhs rhss, NoState) -> return ((lhs,rhss):lhsRhsPairList)
    (Rule _ _, Lhs _) -> error "rep: Rule lhs rhss can't change to Lhs lhs."
    (Rule _ _, Rule lhs rhss) -> return lhsRhsPairList

----
-- For testing with grm/polyrpc.lgrm
-- 

test fun = do
  args <- getArgs
  repTest fun args

repTest fun [] = return ()
repTest fun (arg:args) = do
  text <- readFile arg
  fun text
  repTest fun args

parsing text = do
  lhsRhssPairList <- rep NoState (lines text)
  mapM_ (\(lhs,rhss) -> prLhsRhss lhs rhss) lhsRhssPairList

prLhsRhss :: String -> [[String]] -> IO ()
prLhsRhss lhs rhss = do
  putStrLn lhs
  mapM_ (\rhs ->
         do { putStr "\t"
            ; mapM_ (\s -> do {putStr s; putStr " "}) rhs
            ; putStrLn ""} )  rhss

conversion text = do
  prodrules <- readGrammar (lines text)
  putStrLn "["
  putStrLn $ concat (intersperse ",\n" (map prodRuleToStr prodrules))  -- May replace prodRuleToStr with show
  putStrLn "]"
    