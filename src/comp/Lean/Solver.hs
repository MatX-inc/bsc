-- | bsc's disjointness solver (Yices or STP, by the flags) as a SAT oracle for
-- Boolean skeletons: AND gates over free one-bit atoms, every name fresh.
module Lean.Solver
    ( Solver, Verdict(..), newSolver, solveCircuit, solveCNF, solverQueries, readDimacs
    ) where

import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS

import ASyntax
import ASyntaxUtil(aAnds, aOrs, aNot)
import DisjointTest(DisjointTestState, initDisjointTestState, addADefToDisjointTestState, checkDisjointExpr)
import Error(ErrorHandle)
import Flags(Flags)
import Id(mk_homeless_id)

-- | UNSAT is the only verdict a caller may rely on; Unknown never counts as UNSAT.
data Verdict = Unsat | Sat | Unknown
    deriving (Eq, Show)

data Solver = Solver
    { svState   :: IORef DisjointTestState
    , svDefined :: IORef IS.IntSet      -- gates of the current encoder already given to bsc
    , svQueries :: IORef Int
    }

newSolver :: ErrorHandle -> Flags -> IO Solver
newSolver errh flags = do
    st <- initDisjointTestState "schedlean" errh flags [] [] []
    Solver <$> newIORef st <*> newIORef IS.empty <*> newIORef 0

solverQueries :: Solver -> IO Int
solverQueries = readIORef . svQueries

-- | Is the conjunction of @roots@ and @clauses@ satisfiable, where variable 1
-- is true, each variable in @gates@ is the AND of its literals, and the rest
-- are free?  @prefix@ names this encoder's variables; it must be fresh for
-- each encoder (bsc memoises proofs by expression).
solveCircuit :: Solver -> String -> IM.IntMap [Int] -> [[Int]] -> [Int] -> IO Verdict
solveCircuit sv prefix gates clauses roots = do
    defined <- readIORef (svDefined sv)
    let need = reachable (concat clauses ++ roots)
        -- one definition per gate not yet defined, operands first
        new = [ ADef (gid v) aTBool (aAnds (map litE xs)) [] | v <- need, Just xs <- [IM.lookup v gates]
                                                           , not (IS.member v defined) ]
    st0 <- readIORef (svState sv)
    st1 <- if null new then return st0 else addADefToDisjointTestState st0 new
    let q = aAnds (map litE roots ++ [ aOrs (map litE c) | c <- clauses ])
    (r, st2) <- checkDisjointExpr st1 q aTrue
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
    solveCircuit sv prefix IM.empty cls []

-- | The clauses of a DIMACS file.
readDimacs :: String -> [[Int]]
readDimacs txt = clauses (concatMap words [ l | l <- lines txt, not (null l), head l `notElem` "pc" ])
  where clauses ws = case break (== "0") ws of
            ([], []) -> []
            (c, []) -> [map read c]
            (c, _ : rest) -> map read c : clauses rest
