-- | bsc's disjointness solver (Yices or STP, by the flags) as a SAT oracle for
-- Boolean skeletons: AND gates over free one-bit atoms, every name fresh.
module Lean.Solver
    ( Solver, Verdict(..), newSolver, solveCircuit, solveCNF, solverQueries, readDimacs
    , Circuit, circuitN, newCircuit, cCone, cGate, cNew, cAtom, cAnd, cOr, cIte, solveC
    ) where

import ASyntax (aFalse, aTBool, aTrue, defaultAId, ADef(ADef), AExpr(ASDef, ASPort, APrim))
import ASyntaxUtil (aNot)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef, writeIORef)
import DisjointTest (DisjointTestState, initDisjointTestState, addADefToDisjointTestState, checkDisjointExpr)
import Error (ErrorHandle)
import Flags (Flags)
import Id (mk_homeless_id)
import Prim (PrimOp(..))
import qualified Data.IntMap.Strict as IM (IntMap, empty, lookup, member, insert)
import qualified Data.IntSet as IS (member, IntSet, empty, fromList, insert, toList, union)
import qualified Data.Map.Strict as M (Map, empty, lookup, insert)
import qualified Data.Set as S (fromList, toAscList)

-- | UNSAT is the only verdict a caller may rely on; Unknown never counts as UNSAT.
data Verdict = Unsat | Sat | Unknown
    deriving (Eq, Show)

data Solver = Solver
    { svState   :: IORef DisjointTestState
    , svDefined :: IORef IS.IntSet      -- gates of the current encoder already given to bsc
    , svQueries :: IORef Int
    , svPrefix  :: IORef String         -- the encoder whose gates 'svDefined' holds
    , svFresh   :: IORef Int            -- for fresh encoder prefixes
    }

newSolver :: ErrorHandle -> Flags -> IO Solver
newSolver errh flags = do
    st <- initDisjointTestState "schedlean" errh flags [] [] []
    Solver <$> newIORef st <*> newIORef IS.empty <*> newIORef 0 <*> newIORef "" <*> newIORef 0

solverQueries :: Solver -> IO Int
solverQueries = readIORef . svQueries

-- | Are @roots@ and @clauses@ satisfiable (variable 1 true, @gates@ ANDs, the rest free)?  @prefix@,
-- fresh per encoder, names its variables (bsc memoises proofs by expression).
solveCircuit :: Solver -> String -> IM.IntMap [Int] -> [[Int]] -> [Int] -> IO Verdict
solveCircuit sv prefix gates clauses roots = do
    cur <- readIORef (svPrefix sv)
    if cur == prefix then return () else writeIORef (svDefined sv) IS.empty >> writeIORef (svPrefix sv) prefix
    defined <- readIORef (svDefined sv)
    let need = reachable (concat clauses ++ roots)
        -- one definition per gate not yet defined, operands first
        new = [ ADef (gid v) aTBool (andL xs) [] | v <- need, Just xs <- [IM.lookup v gates]
                                                           , not (IS.member v defined) ]
    nq <- readIORef (svQueries sv)
    -- the query is one definition too, so that bsc's proof memo compares a name, not the formula
    let qid = mk_homeless_id (prefix ++ "query" ++ show nq)
        q = ADef qid aTBool (andE (andL roots : [ orL c | c <- clauses ])) []
    st0 <- readIORef (svState sv)
    st1 <- addADefToDisjointTestState st0 (new ++ [q])
    (r, st2) <- checkDisjointExpr st1 (ASDef aTBool qid) aTrue
    writeIORef (svState sv) st2
    modifyIORef' (svDefined sv) (IS.union (IS.fromList [ v | v <- need, IM.member v gates ]))
    modifyIORef' (svQueries sv) (+ 1)
    return $ case r of
        Just True -> Unsat
        Just False -> Sat
        Nothing -> Unknown
  where
    gid v = mk_homeless_id (prefix ++ "g" ++ show v)
    var v | v == 1 = aTrue
          | IM.member v gates = ASDef aTBool (gid v)
          | otherwise = ASPort aTBool (mk_homeless_id (prefix ++ "a" ++ show v))
    litE l = if l > 0 then var l else aNot (var (negate l))
    -- aAnds and aOrs without their quadratic nub (the operands are literals, made distinct here)
    andL ls = andE (map litE (IS.toList (IS.fromList ls)))
    orL ls = case map litE (IS.toList (IS.fromList ls)) of
        [] -> aFalse
        [e] -> e
        es -> APrim defaultAId aTBool PrimBOr es
    andE es = case es of
        [] -> aTrue
        [e] -> e
        _ -> APrim defaultAId aTBool PrimBAnd es
    -- the gates the query reaches, each after its operands (an explicit stack, no deep recursion)
    reachable ls = go [ (abs l, False) | l <- ls ] IS.empty []
      where go [] _ acc = reverse acc
            go ((v, True) : st) seen acc = go st seen (v : acc)
            go ((v, False) : st) seen acc
                | IS.member v seen = go st seen acc
                | otherwise = case IM.lookup v gates of
                    Nothing -> go st (IS.insert v seen) acc
                    Just xs -> go ([ (abs x, False) | x <- xs ] ++ (v, True) : st) (IS.insert v seen) acc

-- | A plain CNF (no gates), for checking the backend against cadical's verdicts.
solveCNF :: Solver -> String -> [[Int]] -> IO Verdict
solveCNF sv prefix cls = do
    writeIORef (svDefined sv) IS.empty
    writeIORef (svPrefix sv) prefix
    solveCircuit sv prefix IM.empty cls []

-- | The clauses of a DIMACS file.
readDimacs :: String -> [[Int]]
readDimacs txt = clauses (concatMap words [ l | l <- lines txt, not (null l), head l `notElem` "pc" ])
  where clauses ws = case break (== "0") ws of
            ([], []) -> []
            (c, []) -> [map read c]
            (c, _ : rest) -> map read c : clauses rest

-- ---------------------------------------------------------------- circuits

-- | A Boolean circuit being built: variable 1 is true, each gate the AND of its
-- literals, each atom (keyed by @k@) a free variable.
data Circuit k = Circuit { circuitN :: !Int, ciGates :: !(IM.IntMap [Int]), ciAtoms :: !(M.Map k Int)
                         , ciPrefix :: String, ciAnds :: !(M.Map [Int] Int) }

-- | A fresh circuit, its variables named apart from every other's.
newCircuit :: Solver -> IO (Circuit k)
newCircuit sv = do
    k <- readIORef (svFresh sv)
    writeIORef (svFresh sv) (k + 1)
    return (Circuit 1 IM.empty M.empty ("q" ++ show k ++ "_") M.empty)

cNew :: Circuit k -> (Int, Circuit k)
cNew c = let v = circuitN c + 1 in (v, c { circuitN = v })

cAtom :: Ord k => k -> Circuit k -> (Int, Circuit k)
cAtom key c = case M.lookup key (ciAtoms c) of
    Just v -> (v, c)
    Nothing -> let (v, c') = cNew c in (v, c' { ciAtoms = M.insert key v (ciAtoms c') })

-- | satc's AND: true literals dropped, false absorbing, one literal itself.
cAnd :: [Int] -> Circuit k -> (Int, Circuit k)
cAnd xs0 c
    | any (== (-1)) xs = (-1, c)
    | null xs = (1, c)
    | [x] <- xs = (x, c)
    | small, Just v <- M.lookup key (ciAnds c) = (v, c)
    | otherwise = let (v, c') = cNew c
                  in (v, c' { ciGates = IM.insert v xs (ciGates c'), ciAnds = if small then M.insert key v (ciAnds c') else ciAnds c' })
  where xs = filter (/= 1) xs0
        -- one gate per set of operands (a circuit shared by several queries)
        key = S.toAscList (S.fromList xs)
        small = length (take 9 xs) <= 8

cOr :: [Int] -> Circuit k -> (Int, Circuit k)
cOr xs c = let (v, c') = cAnd (map negate xs) c in (negate v, c')

cIte :: Int -> Int -> Int -> Circuit k -> (Int, Circuit k)
cIte i a b c0 = let (x, c1) = cAnd [i, a] c0
                    (y, c2) = cAnd [negate i, b] c1
                in cOr [x, y] c2

-- | A gate's literals (Nothing for an atom or the constant).
cGate :: Circuit k -> Int -> Maybe [Int]
cGate c v = IM.lookup (abs v) (ciGates c)

-- | The variables the literals reach through gates.
cCone :: Circuit k -> [Int] -> IS.IntSet
cCone c ls = go (map abs ls) IS.empty
  where go [] seen = seen
        go (v : st) seen
            | IS.member v seen = go st seen
            | otherwise = go (maybe [] (map abs) (IM.lookup v (ciGates c)) ++ st) (IS.insert v seen)

-- | Is the conjunction of @roots@ and @clauses@ satisfiable in the circuit?
solveC :: Solver -> Circuit k -> [[Int]] -> [Int] -> IO Verdict
solveC sv c = solveCircuit sv (ciPrefix c) (ciGates c)
