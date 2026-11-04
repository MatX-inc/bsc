-- | Building and printing Lean text: names, doc comments, wrapping, the line
-- writer with its cut points, and balanced tables (@.leaf@/@.node@ trees).
module Lean.Print
    ( leanKeywords, leanKeywordSet, commentSafe, doc, chunksOf, wrapJoin, wrap
    , W, WS(..), runW, w, cut, Split(..)
    , tab, tabOr, tabRange, stab, stabKeyed, taball, stabproof, allBThm, tabpair, tabproof, tabproofI
    , allL, tup, tupcong, andc, andget, andterm, listCong, nat, fmt, splitN, splitW
    ) where

import Control.Monad.State.Strict (State, gets, modify', runState)
import Data.List (intercalate)
import qualified Data.Map.Strict as M (Map, empty, findWithDefault, insert)
import qualified Data.Set as S (Set, fromList)
import qualified Data.Sequence as Sq (Seq, empty, fromList, index, length, (|>))

-- | The words Lean 4 (v4.35) does not accept as identifiers, and a few more
-- it does accept but that read badly as names.
leanKeywords :: [String]
leanKeywords = words
    "abbrev at attribute axiom break builtin_initialize by calc catch class \
    \coinductive coinductive_fixpoint continue decreasing_by declare_syntax_cat def \
    \deriving do elab elab_rules else end example export extends false finally for \
    \forall from fun have hiding if import in include inductive inductive_fixpoint \
    \infix infixl infixr initialize instance let local macro macro_rules match meta \
    \mod mut mutual namespace noncomputable nomatch nofun nonrec notation omit opaque \
    \open partial partial_fixpoint postfix prefix private protected public \
    \recommended_spelling renaming return run_cmd run_elab run_meta scoped section \
    \seal set_option show sorry structure suffices syntax termination_by then theorem \
    \this true try unif_hint unless universe unsafe unseal using variable where with \
    \Type Prop Sort"

leanKeywordSet :: S.Set String
leanKeywordSet = S.fromList leanKeywords

-- | Text that cannot open or close a Lean block comment.
commentSafe :: String -> String
commentSafe ('/' : '-' : rest) = '/' : ' ' : commentSafe ('-' : rest)
commentSafe ('-' : '/' : rest) = '-' : ' ' : commentSafe ('/' : rest)
commentSafe (c : rest) = c : commentSafe rest
commentSafe [] = []

-- | A Lean doc comment, wrapped.
doc :: String -> [String]
doc s0 = case wrap 92 (commentSafe s0) of
    [l]      -> ["/-- " ++ l ++ " -/"]
    (l : ls) -> ("/-- " ++ l) : map ("    " ++) (init ls) ++ ["    " ++ last ls ++ " -/"]
    []       -> []

chunksOf :: Int -> [a] -> [[a]]
chunksOf n xs = case splitAt n xs of
    (a, []) -> [ a | not (null a) ]
    (a, b)  -> a : chunksOf n b

-- | Items joined by a separator, as many to a line as fit in a width.
wrapJoin :: Int -> String -> [String] -> [String]
wrapJoin w sep = go
  where go [] = []
        go (x : xs) = let (line, rest) = fill (length x) [x] xs in line : go rest
        -- the line so far: its length, and its pieces, last first
        fill n parts (y : ys) | n' <= w = fill n' (y : sep : parts) ys
          where n' = n + length sep + length y
        fill _ parts ys = (concat (reverse parts), ys)

wrap :: Int -> String -> [String]
wrap w = wrapJoin w " " . words

-- ---------------------------------------------------------------- tables

-- | How a table definition is cut (split_struct): whole, a chunk of its own, or a node.
data Split = Whole Int Int | Chunk Int Int | Node Int Split Split

data WS = WS { wsL :: !(Sq.Seq String), wsSplits :: !(M.Map String Split), wsCuts :: [Int] }

type W = State WS

runW :: W a -> (a, WS)
runW m = runState m (WS Sq.empty M.empty [])

w :: String -> W ()
w s = modify' (\st -> st { wsL = wsL st Sq.|> s })

-- | A top-level position where the file may be cut into parts.
cut :: W ()
cut = modify' (\st -> st { wsCuts = wsCuts st ++ [Sq.length (wsL st)] })

-- | Python's %-formatting with %s and %d both given as strings ('%%' is '%').
fmt :: String -> [String] -> String
fmt ('%' : '%' : r) as = '%' : fmt r as
fmt ('%' : c : r) (a : as) | c `elem` "sd" = a ++ fmt r as
fmt ('%' : c : _) [] | c `elem` "sd" = error "schedcheck: fmt: too few arguments"
fmt (c : r) as = c : fmt r as
fmt [] [] = []
fmt [] _ = error "schedcheck: fmt: too many arguments"

nat :: Int -> String
nat = show

splitN, splitW :: Int
splitN = 128      -- entries per table definition
splitW = 20000    -- source characters per table definition

tab :: [String] -> String
tab items = tabRange items 0 (length items)

-- | A balanced table of the items, or the one-leaf table of @dflt@ when there are none.
tabOr :: String -> [String] -> String
tabOr dflt [] = "(.leaf (" ++ dflt ++ "))"
tabOr _ items = tab items

tabRange :: [String] -> Int -> Int -> String
tabRange items = go
  where
    v = Sq.fromList items
    go lo hi | hi - lo == 1 = "(.leaf (" ++ Sq.index v lo ++ "))"
             | otherwise = let mid = (lo + hi) `div` 2
                           in "(.node " ++ show mid ++ " " ++ go lo mid ++ "\n  " ++ go mid hi ++ ")"

splitStruct :: [String] -> Int -> Split
splitStruct items wd = go 0 (length items) True
  where
    pre = Sq.fromList (scanl (+) 0 (map length items))
    p i = Sq.index pre i
    go lo hi top
        | (p hi - p lo <= wd && hi - lo <= splitN) || hi - lo == 1 = if top then Whole lo hi else Chunk lo hi
        | otherwise = let mid = (lo + hi) `div` 2 in Node mid (go lo mid False) (go mid hi False)

setSplit :: String -> Split -> W ()
setSplit n s = modify' (\st -> st { wsSplits = M.insert n s (wsSplits st) })

getSplit :: String -> W Split
getSplit n = gets (\st -> M.findWithDefault (error ("schedcheck: no table " ++ n)) n (wsSplits st))

-- | `def name : ty` as tab(items), its subtrees of at most splitW characters their own definitions.
stab :: String -> String -> [String] -> W ()
stab name ty items = do
    let st = splitStruct items splitW
    setSplit name st
    let render x = case x of
            Whole lo hi -> return (tabRange items lo hi)
            Chunk lo hi -> do
                let nm = name ++ "_" ++ show lo ++ "_" ++ show hi
                w ("def " ++ nm ++ " : " ++ ty ++ " :=\n  " ++ tabRange items lo hi)
                return nm
            Node mid a b -> do
                ra <- render a
                rb <- render b
                return ("(.node " ++ show mid ++ " " ++ ra ++ "\n  " ++ rb ++ ")")
    top <- render st
    w ("def " ++ name ++ " : " ++ ty ++ " :=\n  " ++ top)

-- | A search tree over ascending keys (each node's key: its right half's first entry).
stabKeyed :: String -> String -> [Int] -> [String] -> W ()
stabKeyed name ty keys items = do
    let st = splitStruct items splitW
        kv = Sq.fromList keys
        iv = Sq.fromList items
        tr lo hi | hi - lo == 1 = "(.leaf (" ++ Sq.index iv lo ++ "))"
                 | otherwise = let mid = (lo + hi) `div` 2
                               in "(.node " ++ show (Sq.index kv mid) ++ " " ++ tr lo mid ++ "\n  " ++ tr mid hi ++ ")"
        render x = case x of
            Whole lo hi -> return (tr lo hi)
            Chunk lo hi -> do
                let nm = name ++ "_" ++ show lo ++ "_" ++ show hi
                w ("def " ++ nm ++ " : " ++ ty ++ " :=\n  " ++ tr lo hi)
                return nm
            Node mid a b -> do
                ra <- render a
                rb <- render b
                return ("(.node " ++ show (Sq.index kv mid) ++ " " ++ ra ++ "\n  " ++ rb ++ ")")
    setSplit name st
    top <- render st
    w ("def " ++ name ++ " : " ++ ty ++ " :=\n  " ++ top)

slice :: [a] -> Int -> Int -> [a]
slice xs lo hi = take (hi - lo) (drop lo xs)

-- | A proof of Tab.All Q name: pairs shaped as the table, one leaf proof per entry.
taball :: String -> [String] -> W String
taball name leaves = do
    st <- getSplit name
    let render x = case x of
            Node _ a b -> "⟨" ++ render a ++ ",\n  " ++ render b ++ "⟩"
            Whole lo hi -> tabpair (slice leaves lo hi)
            Chunk lo hi -> tabpair (slice leaves lo hi)
    return (render st)

stabproof :: String -> String -> String -> String -> (String -> Int -> Int -> String) -> [String] -> Bool -> W String
stabproof name thm params args stmt leaves one = do
    st <- getSplit name
    let f = if one then tabproof else tabpair
        render x = case x of
            Whole lo hi -> return (f (slice leaves lo hi))
            Chunk lo hi -> do
                let nm = thm ++ "_" ++ show lo ++ "_" ++ show hi
                w ("theorem " ++ nm ++ " " ++ params ++ " : " ++ stmt (name ++ "_" ++ show lo ++ "_" ++ show hi) lo hi
                   ++ " :=\n  " ++ f (slice leaves lo hi))
                return (nm ++ " " ++ args)
            Node _ a b -> do
                ra <- render a
                rb <- render b
                return ("⟨" ++ ra ++ ",\n  " ++ rb ++ "⟩")
    render st

-- | theorem thm : Tab.allB f name 0 n = true, one kernel decision per subtree definition.
allBThm :: String -> String -> String -> String -> W ()
allBThm thm f name n = do
    st <- getSplit name
    let render x = case x of
            Chunk lo hi -> do
                let nm = thm ++ "_" ++ show lo ++ "_" ++ show hi
                w ("theorem " ++ nm ++ " : Tab.allB (" ++ f ++ ") " ++ name ++ "_" ++ show lo ++ "_" ++ show hi ++ " "
                   ++ show lo ++ " " ++ show hi ++ " = true := by decide +kernel")
                return nm
            Node _ a b -> do
                ra <- render a
                rb <- render b
                return ("(Tab.allB_node " ++ ra ++ " " ++ rb ++ ")")
            Whole _ _ -> error "schedcheck: allB_thm on a whole table"
    case st of
        Whole _ _ -> w ("theorem " ++ thm ++ " : Tab.allB (" ++ f ++ ") " ++ name ++ " 0 " ++ n ++ " = true := by decide +kernel")
        _ -> do
            r <- render st
            w ("theorem " ++ thm ++ " : Tab.allB (" ++ f ++ ") " ++ name ++ " 0 " ++ n ++ " = true :=\n  " ++ r)

tabpair :: [String] -> String
tabpair = tabpairI "  "

tabpairI :: String -> [String] -> String
tabpairI ind items = go 0 (length items)
  where
    v = Sq.fromList items
    go lo hi | hi - lo == 1 = Sq.index v lo
             | otherwise = let mid = (lo + hi) `div` 2 in "⟨" ++ go lo mid ++ ",\n" ++ ind ++ go mid hi ++ "⟩"

tabproof :: [String] -> String
tabproof = tabproofI "  "

tabproofI :: String -> [String] -> String
tabproofI _ [] = error "schedcheck: a balanced proof of no entries"
tabproofI ind items = go 0 (length items)
  where
    v = Sq.fromList items
    go lo hi | hi - lo == 1 = "Tab.one (" ++ Sq.index v lo ++ ")"
             | otherwise = let mid = (lo + hi) `div` 2 in "⟨" ++ go lo mid ++ ",\n" ++ ind ++ go mid hi ++ "⟩"

allL :: [String] -> String
allL [] = "trivial"
allL items = "⟨" ++ intercalate ", " (items ++ ["trivial"]) ++ "⟩"

tup :: [String] -> String
tup [] = "ops.z"
tup [x] = x
tup (x : xs) = "ops.tup (" ++ x ++ ") (" ++ tup xs ++ ")"

tupcong :: [String] -> String
tupcong [] = "rfl"
tupcong [c] = c
tupcong (c : cs) = "congr2 ops.tup (" ++ c ++ ") (" ++ tupcong cs ++ ")"

-- | Congruence for a right-nested && over equalities.
andc :: [String] -> String
andc [c] = c
andc (c : cs) = "congr2 and (" ++ c ++ ") (" ++ andc cs ++ ")"
andc [] = error "schedcheck: andc []"

-- | From h : (b0 && (b1 && ...)) = true with n conjuncts, the i-th.
andget :: String -> Int -> Int -> String
andget h n i = let t = iterate (\x -> "((Bool.and_eq_true _ _).mp " ++ x ++ ").2") h !! i
               in if i < n - 1 then "((Bool.and_eq_true _ _).mp " ++ t ++ ").1" else t

andterm :: [String] -> String
andterm [t] = t
andterm (t : ts) = "(" ++ t ++ ") && (" ++ andterm ts ++ ")"
andterm [] = error "schedcheck: andterm []"

listCong :: [String] -> String
listCong [] = "rfl"
listCong (t : ts) = "congr2 List.cons (" ++ t ++ ") (" ++ listCong ts ++ ")"
