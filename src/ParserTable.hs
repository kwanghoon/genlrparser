module ParserTable where

import CFG

-- LR(1) item
data Item = Item ProductionRule Int [ExtendedSymbol] {- except Epsilon -}
            deriving Eq
                     
type Items  = [Item]
type Itemss = [Items]

instance Show Item where
  showsPrec p (Item (ProductionRule x syms) j [esym])
    = (++) "[" 
      . (++) x
      . (++) " -> "
      . show_ys (take j syms)
      . (++) "." 
      . show_ys (drop j syms)
      . (++) ", "
      . (++) (show esym)
      . (++) "]"
      
prItem :: Items -> IO ()
prItem xs = do prItem' xs
               putStrLn ""
  where
    prItem' []     = return ()
    prItem' (x:xs) = do putStrLn (show x)
                        prItem' xs
    
  
prItems :: Itemss -> IO ()
prItems xs = prItems' 0 xs

prItems' n []       = return ()
prItems' n (is:iss) =
  do putStrLn ("I" ++ show n ++ ":")
     prItem is
     prItems' (n+1) iss

-- LR(1) Table             
data Action = Shift Int | Reduce Int | Accept | Reject
            deriving (Show, Eq)
                     
type ActionTable = [(Int, ExtendedSymbol, Action)] -- state, terminal, action
type GotoTable   = [(Int, Symbol, Int)]    -- state, nonterminal, state

lookupTable :: (Eq a, Eq b) => a -> b -> [(a,b,c)] -> Maybe c
lookupTable i x [] 
  = Nothing 
lookupTable i x ((j,y,a):tbl)
  = if i == j && x == y then Just a 
    else lookupTable i x tbl
    
prActTbl [] = return ()
prActTbl ((i,x,a):actTbl) = 
  do putStrLn (show i ++ "\t" ++ show x ++ "\t" ++ show a)
     prActTbl actTbl
     
prGtTbl [] = return ()     
prGtTbl ((i,x,j):gtTbl) =
  do putStrLn (show i ++ "\t" ++ show x ++ "\t" ++ show j)
     prActTbl gtTbl


-- LALR(1) Table
data LALRAction = LALRShift [Int] | LALRReduce Int | LALRAccept | LALRReject
            deriving (Show, Eq)
                     
type LALRActionTable = [([Int], ExtendedSymbol, LALRAction)]
type LALRGotoTable   = [([Int], Symbol, [Int])]