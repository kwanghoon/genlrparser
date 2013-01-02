--------------------------------------------------------------------------------
-- An LR Parser Table Generator
-- 
-- Copyright(c) 2013 Kwanghoon Choi. All rights reserved.
--
-- Usage:
--  $ ghci GenLRParserTable
--  *GenLRParserTable> prParseTable (calcParseTable g1)
--  *GenLRParserTable> prLALRParseTable (calcLALRParseTable g1)
--------------------------------------------------------------------------------

module GenLRParserTable where

import Data.List
import Data.Maybe

--------------------------------------------------------------------------------
-- Context Free Grammar
--------------------------------------------------------------------------------
data Symbol = Nonterminal String | Terminal String 
    deriving Eq
             
instance Show Symbol where
  showsPrec p (Nonterminal x) = (++) x
  showsPrec p (Terminal x)    = (++) x
  
isTerminal (Terminal x) = True  
isTerminal _            = False
  
data ExtendedSymbol = Symbol Symbol | Epsilon | EndOfSymbol
    deriving Eq
             
instance Show ExtendedSymbol where
  showsPrec p (Symbol sym)    = (++) (show sym)
  showsPrec p (Epsilon)       = (++) "epsilon"
  showsPrec p (EndOfSymbol)   = (++) "$"
  
isExtendedTerminal (Symbol (Terminal x)) = True  
isExtendedTerminal (EndOfSymbol)         = True  
isExtendedTerminal _                     = False

isExtendedNonterminal (Symbol (Nonterminal x)) = True  
isExtendedNonterminal _                        = False

data ProductionRule = ProductionRule String [Symbol]
         deriving Eq
                  
instance Show ProductionRule where
  showsPrec p (ProductionRule x ys) = (++) x . (++) " -> " . show_ys ys
  
type ProductionRules = [ProductionRule]  
  
show_ys []     = (++) ""  
show_ys [y] = (++) (show y) 
show_ys (y:ys) = (++) (show y) . (++) " " . show_ys ys

data CFG = CFG String [ProductionRule]
         deriving Show

type AUGCFG = CFG

data Item = Item ProductionRule Int ExtendedSymbol {- except Epsilon -}
            deriving Eq
                     
type Items  = [Item]
type Itemss = [Items]

instance Show Item where
  showsPrec p (Item (ProductionRule x syms) j esym)
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
closure' fstTbl prules cls (Item (ProductionRule x alphaBbeta) d a:items) b = 
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
      then g cls b r rs (extFirst_ fstTbl (map Symbol beta ++ [a]))
      else f cls b rs

    -- loop over terminal symbols
    g cls b r rs [] = f cls b rs
    g cls b r rs (Symbol (Terminal t) : fstSyms) =
      let item = Item r 0 (Symbol (Terminal t))
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Symbol (Nonterminal t) : fstSyms) = g cls b r rs fstSyms
    g cls b r rs (EndOfSymbol : fstSyms) = 
      let item = Item r 0 EndOfSymbol
      in  if elem item cls 
          then g cls b r rs fstSyms 
          else g (cls++[item]) True r rs fstSyms
    g cls b r rs (Epsilon : fstSyms) = error "closure: Epsilon"
    
--    
calcItems :: AUGCFG -> Itemss
calcItems augCfg = calcItems' augCfg syms iss0
  where 
    CFG _S prules = augCfg
    i0   = Item (head prules) 0 EndOfSymbol  -- The 1st rule : S' -> S.
    is0  = closure augCfg [i0]
    iss0 = [ is0 ]

    syms = (\\) (symbols augCfg) [Nonterminal _S]
  
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
                 
--                 
data Action = Shift Int | Reduce Int | Accept | Reject
            deriving (Show, Eq)
                     
type ActionTable = [(Int, ExtendedSymbol, Action)] -- state, terminal, action
type GotoTable   = [(Int, Symbol, Int)]    -- state, nonterminal, state

lookupActionTable i x [] 
  = Nothing 
lookupActionTable i x ((j,y,a):actTbl)
  = if i == j && x == y then Just a 
    else lookupActionTable i x actTbl
    

prActTbl [] = return ()
prActTbl ((i,x,a):actTbl) = 
  do putStrLn (show i ++ "\t" ++ show x ++ "\t" ++ show a)
     prActTbl actTbl
     
prGtTbl [] = return ()     
prGtTbl ((i,x,j):gtTbl) =
  do putStrLn (show i ++ "\t" ++ show x ++ "\t" ++ show j)
     prActTbl gtTbl

--------------------------------------------------------------------------------
-- Canonical LR Parser
--------------------------------------------------------------------------------
calcParseTable :: AUGCFG -> (Itemss, ProductionRules, ActionTable, GotoTable)
calcParseTable augCfg = (items, prules, actionTable, gotoTable)
  where
    CFG _S' prules = augCfg
    items = calcItems augCfg
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
      , Item (ProductionRule y ys) j a <- item1
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
data LALRAction = LALRShift [Int] | LALRReduce Int | LALRAccept | LALRReject
            deriving (Show, Eq)
                     
type LALRActionTable = [([Int], ExtendedSymbol, LALRAction)]
type LALRGotoTable   = [([Int], Symbol, [Int])]

calcLALRParseTable :: AUGCFG -> 
                      (Itemss, ProductionRules, [[Int]], LALRActionTable
                      , LALRGotoTable)
calcLALRParseTable augCfg = (itemss, prules, iss, lalrActTbl, lalrGtTbl)
  where
    (itemss, prules, actTbl, gtTbl) = calcParseTable augCfg
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
                             , let r = lookupActionTable i x actTbl
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

--------------------------------------------------------------------------------
-- Utility
--------------------------------------------------------------------------------

-- C enum type declaration for states
cgStates iss = cgEnum "STATE" (cgStates' iss)
     
cgStates' [] = return ()  
cgStates' [is] = 
  do putStr "\t"
     cgState is

cgStates' [is1,is2] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStrLn ""

cgStates' [is1,is2,is3] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStrLn ""

cgStates' [is1,is2,is3,is4] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStrLn ""
     
cgStates' [is1,is2,is3,is4,is5] = 
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStr ", "
     cgState is5
     putStrLn ""
     
cgStates' (is1:is2:is3:is4:is5:iss) =
  do putStr "\t"
     cgState is1
     putStr ", "
     cgState is2
     putStr ", "
     cgState is3
     putStr ", "
     cgState is4
     putStr ", "
     cgState is5
     putStrLn ","
     cgStates' iss
     
cgState is = putStr (cgToState is) 
     
cgToState is = "S" ++ cgToState' is

cgToState' []     = ""
cgToState' [i]    = show i
cgToState' (i:is) = show i ++  "_" ++ cgToState' is

-- C enum type declaration for nonterminals

cgNonterminals augCfg = 
  cgEnum "Nonterminal" (cgNonterminals' nonterminals)
  where
    CFG s prules = augCfg
    nonterminals = nub $ [s] ++ [x | ProductionRule x _ <- prules]
    
cgNonterminals' []     = return ()    
cgNonterminals' [x]    = 
  do putStr "\t"
     putStr $ cgToCName $ x
     putStrLn ""
cgNonterminals' [x1,x2]    = 
  do putStr "\t"
     putStr $ cgToCName $ x1
     putStr ", "
     putStr $ cgToCName $ x2
     putStrLn ""
cgNonterminals' (x1:x2:xs) = 
  do putStr "\t"
     putStr $ cgToCName $ x1
     putStr ", "
     putStr $ cgToCName $ x2
     putStr ", "
     putStrLn ""
     cgNonterminals' xs
     
cgToCName x = "NONTERMINAL_" ++ cgToCName' x

cgToCName' []     = []      -- CAUTION: Don't use S' with S_ for nonterminals.
cgToCName' (c:cs) = 
  (if c == '\'' then '_' else c) : cgToCName' cs 

cgEnum name action =
  do putStrLn ("enum " ++ name ++ " {")
     action
     putStrLn "};"

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g1 from Example 4.33 in the Dragon book (2nd Ed.)
--------------------------------------------------------------------------------
g1 = CFG "E'" [p0,p1,p2,p3,p4,p5,p6]

-- E' -> E
p0 = ProductionRule "E'" [Nonterminal "E"]

-- E -> E + T
p1 = ProductionRule "E" [Nonterminal "E", Terminal "+", Nonterminal "T"] 

-- E -> T
p2 = ProductionRule "E" [Nonterminal "T"]

-- T -> T * F
p3 = ProductionRule "T" [Nonterminal "T", Terminal "*", Nonterminal "F"]

-- T -> F
p4 = ProductionRule "T" [Nonterminal "F"]

-- F -> ( E )
p5 = ProductionRule "F" [Terminal "(", Nonterminal "E", Terminal ")"]

-- F -> id
p6 = ProductionRule "F" [Terminal "id"]

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g2 from Example 4.2 in the Dragon book (2nd Ed.)
--------------------------------------------------------------------------------
g2 = CFG "S'" [q1,q2,q3,q4]

q1 = ProductionRule "S'" [Nonterminal "S"]
q2 = ProductionRule "S" [Nonterminal "C", Nonterminal "C"]
q3 = ProductionRule "C" [Terminal "c", Nonterminal "C"]
q4 = ProductionRule "C" [Terminal "d"]

--------------------------------------------------------------------------------
-- [Sample CFG Grammar] : g3 from the LF calculus
--------------------------------------------------------------------------------
g3 = CFG "S'" [lfp0,lfp1,lfp2,lfp5,lfp6,lfp7,lfp8,lfp9,lfp10,lfp11
              ,lfp12,lfp13,lfp14,lfp15,lfp16,lfp17,lfp18,lfp19,lfp20,lfp21
              ,lfp22,lfp23,lfp24,lfp25,lfp26,lfp27,lfp28,lfp29,lfp30,lfp31]

lfp0 = ProductionRule "S'" [Nonterminal "Program"]
lfp1 = ProductionRule "Program" [Nonterminal "Decl"]

lfp2 = ProductionRule "Decl" [Nonterminal "TypeDeclaration", 
                              Nonterminal "TermDeclaration", 
                              Nonterminal "DefDeclaration"]

lfp5 = ProductionRule "TypeDeclaration" 
       [Terminal "atType", Nonterminal "TyDecls"]
lfp6 = ProductionRule "TermDeclaration"
       [Terminal "atTerm", Nonterminal "TmDecls"]
lfp7 = ProductionRule "DefDeclaration" []
lfp8 = ProductionRule "DefDeclaration"
       [Terminal "atDef", Nonterminal "DefDecls"]
       
lfp9 = ProductionRule "TyDecls"
       [Terminal "var", Terminal ":", Nonterminal "K", Terminal "." ]
lfp10 = ProductionRule "TyDecls"
       [Terminal "var", Terminal ":", Nonterminal "K", Terminal "."
       , Nonterminal "TyDecls" ]
       
lfp11 = ProductionRule "TmDecls"
       [Terminal "var", Terminal ":", Nonterminal "A", Terminal "." ]
lfp12 = ProductionRule "TmDecls"
       [Terminal "var", Terminal ":", Nonterminal "A", Terminal "."
       , Nonterminal "TmDecls" ]
       
lfp13 = ProductionRule "DefDecls"
       [Terminal "var", Terminal "=", Nonterminal "M", Terminal "." ]
lfp14 = ProductionRule "DefDecls"
       [Terminal "var", Terminal "=", Nonterminal "M", Terminal "."
       , Nonterminal "DefDecls" ]
       
lfp15 = ProductionRule "K" [Terminal "Type"]
lfp16 = ProductionRule "K" [Terminal "Pi", Terminal "var", Terminal ":"
                           , Nonterminal "A", Terminal ".", Nonterminal "K"]
lfp17 = ProductionRule "K" [Terminal "(", Nonterminal "K", Terminal ")"]        
lfp18 = ProductionRule "K" [Nonterminal "A1", Terminal "arrow", Nonterminal "K"]

lfp19 = ProductionRule "A" [Terminal "Pi", Terminal "var", Terminal ":"
                           , Nonterminal "A", Terminal ".", Nonterminal "A"]
lfp20 = ProductionRule "A" [Nonterminal "A1"]        
lfp21 = ProductionRule "A" [Nonterminal "A1", Terminal "arrow", Nonterminal "A"]

lfp22 = ProductionRule "A1" [Terminal "var"]
lfp23 = ProductionRule "A1" [Terminal "(", Nonterminal "A", Terminal ")"]
lfp24 = ProductionRule "A1" [Nonterminal "A1", Terminal "var"]
lfp25 = ProductionRule "A1" [Nonterminal "A1", Terminal "(", Nonterminal "M"
                            , Terminal ")"]
        
lfp26 = ProductionRule "M" [Terminal "Lam", Terminal "var", Terminal ":", 
                            Nonterminal "A", Terminal ".", Nonterminal "M"]
lfp27 = ProductionRule "M" [Nonterminal "M1"]

lfp28 = ProductionRule "M1" [Terminal "var"]
lfp29 = ProductionRule "M1" [Terminal "(", Nonterminal "M", Terminal ")"]
lfp30 = ProductionRule "M1" [Nonterminal "M1", Terminal "var"]
lfp31 = ProductionRule "M1" [Nonterminal "M1", Terminal "(", Nonterminal "M",
                             Terminal ")"]
