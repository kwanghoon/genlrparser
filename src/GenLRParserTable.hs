--------------------------------------------------------------------------------
-- An LR Parser Table Generator
-- 
-- Copyright(c) 2013 Kwanghoon Choi. All rights reserved.
--
-- Usage:
--  $ ghci GenLRParserTable
--  *GenLRParserTable> prParseTable (calcLR1ParseTable g1)
--  *GenLRParserTable> prLALRParseTable (calcLALRParseTable g1)
--------------------------------------------------------------------------------

module GenLRParserTable where

import Data.List
import Data.Maybe
import System.Environment (getArgs)

import CFG
import ParserTable

_main = do
  args <- getArgs
  mapM f args
  where
    f file = do
      grammar <- readFile file
      prLALRParseTable (calcLALRParseTable (read grammar))
    
--
indexPrule :: AUGCFG -> ProductionRule -> Int
indexPrule augCfg prule = indexPrule' prules prule 0
  where
    CFG _ prules = augCfg
  
indexPrule' []     prule n = error ("indexPrule: not found " ++ show prule)
indexPrule' (r:rs) prule n = 
  if r == prule then n else indexPrule' rs prule (n+1)
                            
prPrules ps = prPrules' ps 0

prPrules' [] n = return ()
prPrules' (prule:prules) n = 
  do putStrLn (show n ++ ": " ++ show prule)
     prPrules' prules (n+1)
      
--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------
symbols :: CFG -> [Symbol]
symbols (CFG start prules) 
  = [Nonterminal x | Nonterminal x <- syms] ++
    [Terminal x    | Terminal x    <- syms]
  where
    f (ProductionRule x syms) = Nonterminal x:syms
    syms = nub (Nonterminal start : concat (map f prules))

--
first :: [(Symbol, [ExtendedSymbol])] -> Symbol -> [ExtendedSymbol]
first tbl x = fromJust (lookup x tbl)

first_ :: [(Symbol, [ExtendedSymbol])] -> [Symbol] -> [ExtendedSymbol]
first_ tbl []     = []
first_ tbl (z:zs) = let zRng = first tbl z in
  if elem Epsilon zRng 
  then union ((\\) zRng [Epsilon]) (first_ tbl zs)
  else zRng
                                                            
extFirst :: [(Symbol, [ExtendedSymbol])] -> ExtendedSymbol -> [ExtendedSymbol]
extFirst tbl (Symbol x)    = first tbl x
extFirst tbl (EndOfSymbol) = [EndOfSymbol]
extFirst tbl (Epsilon)     = error "extFirst_ : Epsilon"

extFirst_ :: [(Symbol, [ExtendedSymbol])] -> [ExtendedSymbol] -> [ExtendedSymbol]
extFirst_ tbl []     = []
extFirst_ tbl (z:zs) = let zRng = extFirst tbl z in
  if elem Epsilon zRng 
  then union ((\\) zRng [Epsilon]) (extFirst_ tbl zs)
  else zRng
  
--
calcFirst :: CFG -> [(Symbol, [ExtendedSymbol])]
calcFirst cfg = calcFirst' cfg (initFirst cfg) (symbols cfg)
    
initFirst cfg =
  let syms         = symbols cfg
      CFG _ prules = cfg
  in [(Terminal x, [Symbol (Terminal x)]) 
     | Terminal x <- syms]
     ++    
     [(Nonterminal x, [Epsilon | ProductionRule y [] <- prules, x == y])
     | Nonterminal x <- syms]

calcFirst' cfg currTbl syms =
  let (isChanged, nextFst) = calcFirst'' cfg currTbl syms in
  if isChanged then calcFirst' cfg nextFst syms else currTbl
                                                 

calcFirst'' cfg tbl [] 
  = (False, [])
calcFirst'' cfg tbl (Terminal x:therest)
  = calcFirst''' cfg tbl (False, (Terminal x, first tbl (Terminal x))) therest
calcFirst'' cfg tbl (Nonterminal x:therest) 
  = calcFirst''' cfg tbl (ischanged, (Nonterminal x, rng)) therest
    where
      CFG start prules = cfg
      
      addendum   = f [zs | ProductionRule y zs <- prules, x == y]
      currRng    = first tbl (Nonterminal x)
      ischanged  = (\\) addendum currRng /= []
      rng        = union addendum currRng
      
      f []       = []
      f (zs:zss) = union (first_ tbl zs) (f zss)
                   
calcFirst''' cfg tbl (bool1, oneupdated) therest =
  let (bool2, therestupdated) = calcFirst'' cfg tbl therest in
  (bool1 || bool2, oneupdated:therestupdated)


--
follow :: [(Symbol, [ExtendedSymbol])] -> Symbol -> [ExtendedSymbol]
follow tbl x = case lookup x tbl of
  Nothing -> error (show x ++ " : " ++ show tbl)
  Just z  -> z

--
calcFollow :: CFG -> [(Symbol, [ExtendedSymbol])]
calcFollow cfg = calcFollow' (calcFirst cfg) prules (initFollow cfg) 
  where CFG _ prules = cfg

initFollow cfg = 
  let CFG start prules = cfg
  in  [(Nonterminal x, [EndOfSymbol | x == start])
      | Nonterminal x <- symbols cfg]
      
calcFollow' fstTbl prules currTbl = 
  let (isChanged, nextFlw) = calcFollow'' fstTbl currTbl prules False in
  if isChanged then calcFollow' fstTbl prules nextFlw else currTbl
                                                      
calcFollow'' fstTbl flwTbl []                            b = (b, flwTbl)
calcFollow'' fstTbl flwTbl (ProductionRule y zs:therest) b =
  calcFollow'' fstTbl tbl' therest b'
  where
    (b',tbl') = f zs flwTbl b
    
    _y             = Nonterminal y
    
    f []                 tbl b = (b, tbl)
    f [Terminal z]       tbl b = (b, tbl)
    f [Nonterminal z]    tbl b =
      let flwZ = follow flwTbl (Nonterminal z)
          zRng = union flwZ (follow flwTbl _y)
          isChanged = (\\) zRng flwZ /= []
      in  (isChanged, upd (Nonterminal z) zRng tbl)
    f (Terminal z:zs)    tbl b = f zs tbl b
    f (Nonterminal z:zs) tbl b =
      let fstZS = first_ fstTbl zs
          flwZ  = follow flwTbl (Nonterminal z)
          zRng  = union (follow flwTbl (Nonterminal z))
                    (union ((\\) fstZS [Epsilon])
                      (if elem Epsilon fstZS 
                       then follow flwTbl _y
                       else []))
          isChanged = (\\) zRng flwZ /= []
      in  f zs (upd (Nonterminal z) zRng tbl) isChanged
    
    upd z zRng tbl = [if z == x then (x, zRng) else (x,xRng) | (x,xRng) <- tbl]
    
--     
closure :: AUGCFG -> Items -> Items
closure augCfg items = 
  if isChanged 
  then closure augCfg itemsUpdated  -- loop over items
  else items
  where
    CFG s prules = augCfg
    (isChanged, itemsUpdated) 
      = closure' (calcFirst augCfg) prules items items False
                       
                  
closure' fstTbl prules cls [] b = (b, cls)
closure' fstTbl prules cls (Item (ProductionRule x alphaBbeta) d lookahead:items) b = 
  if _Bbeta /= []
  then f cls b prules
  else closure' fstTbl prules cls items b
  where
    _Bbeta = drop d alphaBbeta
    _B     = head _Bbeta
    beta   = tail _Bbeta
    
    -- loop over production rules
    f cls b [] = closure' fstTbl prules cls items b
    f cls b (r@(ProductionRule y gamma):rs) = 
      if _B == Nonterminal y
      then (if lookahead == [] 
            then flrzero cls b r rs -- closure for LR(0)
            else g cls b r rs (extFirst_ fstTbl (map Symbol beta ++ lookahead))) -- closure for LR(1)
      else f cls b rs

    flrzero cls b r rs = 
      let item = Item r 0 []
      in  if elem item cls then f cls b rs 
          else f (cls ++ [item]) True rs

    -- loop over terminal symbols
    g cls b r rs [] = f cls b rs
    g cls b r rs (Symbol (Terminal t) : fstSyms) =
      let item = Item r 0 [Symbol (Terminal t)]
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Symbol (Nonterminal t) : fstSyms) = g cls b r rs fstSyms
    g cls b r rs (EndOfSymbol : fstSyms) = 
      let item = Item r 0 [EndOfSymbol]
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Epsilon : fstSyms) = error "closure: Epsilon"
    
--    
calcLR0Items :: AUGCFG -> Itemss
calcLR0Items augCfg = calcItems' augCfg syms iss0
  where 
    CFG _S prules = augCfg
    i0   = Item (head prules) 0 []  -- The 1st rule : S' -> S.
    is0  = closure augCfg [i0]
    iss0 = [ is0 ]

    syms = (\\) (symbols augCfg) [Nonterminal _S]
    -- syms = [ sym | sym <- symbols augCfg, sym /= Nonterminal _S]

calcLR1Items :: AUGCFG -> Itemss
calcLR1Items augCfg = calcItems' augCfg syms iss0
  where 
    CFG _S prules = augCfg
    i0   = Item (head prules) 0 [EndOfSymbol]  -- The 1st rule : S' -> S.
    is0  = closure augCfg [i0]
    iss0 = [ is0 ]

    syms = (\\) (symbols augCfg) [Nonterminal _S]
    -- syms = [ sym | sym <- symbols augCfg, sym /= Nonterminal _S]
  
calcItems' augCfg syms currIss  =
  if isUpdated
  then calcItems' augCfg syms nextIss
  else currIss
  where
    (isUpdated, nextIss) = f currIss False currIss
    
    -- loop over sets of items
    f []       b currIss = (b, currIss)
    f (is:iss) b currIss = g is iss b currIss syms
    
    -- loop over symbols
    g is iss b currIss []     = f iss b currIss
    g is iss b currIss (x:xs) = 
      let is' = goto augCfg is x
      in  if is' == [] || elemItems is' currIss 
          then g is iss b currIss xs 
          else g is iss True (currIss ++ [is']) xs

elemItems :: Items -> Itemss -> Bool       
elemItems is0 []       = False
elemItems is0 (is:iss) = eqItems is0 is || elemItems is0 iss
                         
eqItems :: Items -> Items -> Bool                         
eqItems is1 is2 = (\\) is1 is2 == [] && (\\) is2 is1 == []

indexItem :: Itemss -> Items -> Int
indexItem items item = indexItem' items item 0

indexItem' (item1:items) item2 n
  = if eqItems item1 item2 then n else indexItem' items item2 (n+1)
indexItem' [] item n = error ("indexItem: not found " ++ show item)

goto :: AUGCFG -> Items -> Symbol -> Items
goto augCfg items x = closure augCfg itemsOverX
  where
    itemsOverX = [ Item (ProductionRule z alphaXbeta) (j+1) y
                 | Item (ProductionRule z alphaXbeta) j     y <- items
                 , let _Xbeta = drop j alphaXbeta
                 , _Xbeta /= []
                 , x == head _Xbeta ]
                 


--------------------------------------------------------------------------------
-- Canonical LR Parser
--------------------------------------------------------------------------------
calcLR0ParseTable :: AUGCFG -> (Itemss, ProductionRules, ActionTable, GotoTable)
calcLR0ParseTable augCfg = ([], [], [], [])

calcLR1ParseTable :: AUGCFG -> (Itemss, ProductionRules, ActionTable, GotoTable)
calcLR1ParseTable augCfg = (items, prules, actionTable, gotoTable)
  where
    CFG _S' prules = augCfg
    items = calcLR1Items augCfg
    syms  = (\\) (symbols augCfg) [Nonterminal _S']
    
    terminalSyms    = [Terminal x    | Terminal x    <- syms]
    nonterminalSyms = [Nonterminal x | Nonterminal x <- syms]
    
    f :: [(ActionTable,GotoTable)] -> (ActionTable, GotoTable)
    f l = case unzip l of (fst,snd) -> (g [] (concat fst), h [] (concat snd))
                          
    g actTbl [] = actTbl
    g actTbl ((i,x,a):triples) = 
      let bs = [a' == a | (i',x',a') <- actTbl, i' == i && x' == x ] in
      if length bs == 0
      then g (actTbl ++ [(i,x,a)]) triples
      else if and bs 
           then g actTbl triples 
           else error ("Conflict: " 
                       ++ show (i,x,a) 
                       ++ " " 
                       ++ show actTbl)
                
    h :: GotoTable -> GotoTable -> GotoTable
    h gtTbl [] = gtTbl
    h gtTbl ((i,x,j):triples) =
      let bs = [j' == j | (i',x',j') <- gtTbl, i' == i && x' == x ] in
      if length bs == 0
      then h (gtTbl ++ [(i,x,j)]) triples
      else if and bs
           then h gtTbl triples
           else error ("Conflict: "
                       ++ show (i,x,j)
                       ++ " "
                       ++ show gtTbl)
    
    (actionTable, gotoTable) = f
      [ if ys' == []
        then if y == _S' 
             then ([(from, a, Accept)   ], []) 
             else ([(from, a, Reduce ri)], [])
        else if isTerminal h 
             then ([(from, Symbol h, Shift to) ], [])
             else ([]                    , [(from, h, to)])
      | item1 <- items
      , Item (ProductionRule y ys) j [a] <- item1
      , let from = indexItem items item1
      , let ri   = indexPrule augCfg (ProductionRule y ys)
      , let ys' = drop j ys
      , let h = head ys'
      , let to = indexItem items (goto augCfg item1 h)
      ]
      
prParseTable (items, prules, actTbl, gtTbl) =
  do putStrLn (show (length items) ++ " states")
     prItems items
     putStrLn ""
     prPrules prules
     putStrLn ""
     prActTbl actTbl
     putStrLn ""
     prGtTbl gtTbl
     
prLALRParseTable (items, prules, iss, lalrActTbl, lalrGtTbl) =
  do putStrLn (show (length items) ++ " states")
     prItems items
     putStrLn ""
     prPrules prules
     putStrLn ""
     putStrLn (show (length iss) ++ " states")
     prStates iss
     putStrLn ""
     prActTbl lalrActTbl
     putStrLn ""
     prGtTbl lalrGtTbl
     
prStates [] = return ()     
prStates (is:iss) =
  do putStrLn (show is)
     prStates iss
     
--------------------------------------------------------------------------------
-- LALR Parser 
--------------------------------------------------------------------------------

calcLALRParseTable :: AUGCFG -> 
                      (Itemss, ProductionRules, [[Int]], LALRActionTable
                      , LALRGotoTable)
calcLALRParseTable augCfg = (itemss, prules, iss, lalrActTbl, lalrGtTbl)
  where
    (itemss, prules, actTbl, gtTbl) = calcLR1ParseTable augCfg
    itemss' = nubBy eqCore itemss
    iss     = [ [i | (i, items) <- zip [0..] itemss, eqCore items items']
              | items' <- itemss'] 
              
    lalrActTbl = [ (is, x, lalrAct)
                 | is <- iss
                 , let syms = nub [ y | i <- is, (j, y, a) <- actTbl, i == j ]
                 , x <- syms
                 , let lalrAct = actionCheck $
                         nub [ toLalrAction iss a
                             | i <- is
                             , let r = lookupTable i x actTbl
                             , isJust r
                             , let Just a = r ]  ]

    lalrGtTbl  = [ (is, x, js) 
                 | is <- iss
                 , let syms = nub [ y | i <- is, (j, y, k) <- gtTbl, i == j]
                 , x <- syms
                 , let js = stateCheck $ 
                         nub [ toIs iss j'
                             | i <- is
                             , (i', x', j') <- gtTbl
                             , i==i' && x==x' ]  ]
    
eqCore :: Items -> Items -> Bool    
eqCore items1 items2 = subsetCore items1 items2 && subsetCore items2 items1

subsetCore []             items2 = True
subsetCore (item1:items1) items2 = elemCore item1 items2 && subsetCore items1 items2
  
elemCore (Item prule1 i1 a) [] = False
elemCore (Item prule1 i1 a) (Item prule2 i2 _:items) = 
  if prule1 == prule2 && i1 == i2 
  then True else elemCore (Item prule1 i1 a) items
    
toLalrAction :: [[Int]] -> Action -> LALRAction
toLalrAction iss (Shift i)  = LALRShift (toIs iss i)
toLalrAction iss (Reduce i) = LALRReduce i
toLalrAction iss (Accept)   = LALRAccept
toLalrAction iss (Reject)   = LALRReject

toIs []       i = error ("toIs: not found" ++ show i)
toIs (is:iss) i = if elem i is then is else toIs iss i

actionCheck :: [LALRAction] -> LALRAction
actionCheck [a] = a
actionCheck as  = error ("LALR Action Conflict: " ++ show as)

stateCheck :: [[Int]] -> [Int]
stateCheck [is] = is
stateCheck iss  = error ("LALR State Conflict: " ++ show iss)
