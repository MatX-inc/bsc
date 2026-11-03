{-# LANGUAGE LambdaCase, MagicHash #-}
-- | The flat (whole-module) Lean emitter, kept behind --flat for comparison.
module Lean.Sched.Flat where

import Control.Exception(evaluate)
import Control.Monad(foldM, forM, forM_, when)
import Control.Monad.State.Strict(State, evalState, gets, modify', runState)
import qualified Data.ByteString as BS
import Data.Char(isAsciiLower, isAsciiUpper, isDigit, toUpper)
import Data.Containers.ListUtils(nubOrd)
import Data.IORef(IORef, newIORef, readIORef, writeIORef)
import qualified Data.Foldable as F
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import GHC.Exts(isTrue#, reallyUnsafePtrEquality#)
import Data.List(intercalate, isPrefixOf, mapAccumL, partition, sortOn, tails, transpose)
import qualified Data.Map.Lazy as ML
import qualified Data.Map.Strict as M
import Data.Maybe(catMaybes, fromMaybe, isJust, isNothing, listToMaybe, mapMaybe)
import qualified Data.Sequence as Sq
import qualified Data.Set as S
import System.Console.GetOpt(ArgDescr(..), ArgOrder(..), OptDescr(..), getOpt, usageInfo)
import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)
import System.FilePath(takeFileName)
import System.IO.Unsafe(unsafePerformIO)
import System.IO(IOMode(..), hPutStr, hPutStrLn, hSetEncoding, stderr,
                 stdout, utf8, withFile)

import ABin(ABin(..), ABinModInfo(..))
import ASchedule(AScheduleInfo(..), MethodId(..), UniqueUse(..))
import AScheduleInfo(SchedNode(..), erdbToList)
import AState(aState)
import AVerilogUtil(vNameToTask)
import ASyntax
import ASyntaxUtil(aType, aAnd, aOr, aOrs, aSubst, argInputPorts)
import AUses(Rule(..), buildUseMaps, extractCondition, rumToMethodUseMap,
             ruleName, rulePred, ruleAncestor)
import BackendNamingConventions(isRWire, isRWire0, isBypassWire)
import DisjointTest(initDisjointTestState, checkDisjointExprWithCtx)
import Error(ErrorHandle, initErrorHandle)
import Flags(Flags, unSpecTo, useDPI)
import GenABin(readABinFile)
import Id(getIdString, getIdBaseString, mkIdCanFire, mkIdWillFire, mkRdyId,
          isRdyId, getIdPosition, setIdBaseString, mk_homeless_id)
import IntegerUtil(aaaa)
import IntLit(IntLit(..))
import PPrint(ppString)
import Pragma(SchedulePragma(..))
import Prim(PrimOp(..))
import RSchedule(ratToNestedLists)
import SchedInfo(SchedInfo(..), MethodConflictInfo(..))
import VModInfo(vName, vFields, vSched, vPath, VFieldInfo(..), VPathInfo(..),
                getVNameString, mkNamedOutputs)

import Lean.Print
import Lean.BV
import Lean.Sched.Types
import Lean.Sched.Conflicts
import Lean.Sched.Order
import Lean.Sched.Extract
import Lean.Sched.OutOfOrder
import Lean.Sched.BscFacts

-- | A symbolic value: a Lean expression over the cycle's free variables.
--
-- A compound term carries an 'Info' about itself, made in constant time from
-- its parts', so that terms that differ almost always differ at once and
-- smart constructors that compare their arguments stay cheap.  Equality,
-- order and 'Show' are those of the plain tree, with the operands of @&&@
-- and @||@ as lists (the instances below), which the 'Info' only makes
-- quicker: two compound terms are equal when their ids are.
data Term
    = TIn String                  -- ^ @i.x@: a free variable of the cycle
    | TView String                -- ^ @v.x@: a value observed within the cycle
    | TRef String Bool            -- ^ shared definition @d i@ (or @d i v@)
    | THw String                  -- ^ hardware definition @Hw.d i@
    | TAtHw String                -- ^ shared definition at the hardware view
    | TLit Bool
    | TConst Sort Integer         -- ^ a datapath constant, by value
    | TStr String                 -- ^ a string constant
    | TSt String                  -- ^ @s.x@: a field of the state before a step
    | TDef [String]               -- ^ @d i@: a version of the sequential execution (by qualified name)
    | TAt String [String]         -- ^ shared definition at a step's view @V@: @d i (V i)@
    | TUnset Sort                 -- ^ the code of a token no call has set (@0#w@)
    | TRd !Int Sort               -- ^ in a step's effect, the k-th field it reads
    | TProj [String] String       -- ^ a field of a named view: @(V i).f@
    | TFld !Int Sort              -- ^ field k of a state by field numbers: @(s k).toX@
    | TVF String [Term] String    -- ^ a field of a view function at some calls: @(i.vf a b).f@
    | TAtVF String String [Term]  -- ^ a shared definition at that view: @d i (i.vf a b)@
    | TNot {-# UNPACK #-} !Info Term
    | TAnd {-# UNPACK #-} !Info TSet
    | TOr {-# UNPACK #-} !Info TSet
    | TEq {-# UNPACK #-} !Info Term Term
    | TCond {-# UNPACK #-} !Info Term Term Term   -- ^ @bif c then a else b@
    | TAdd {-# UNPACK #-} !Info Term Term         -- ^ @a + b@ of task-call positions ('SPos')

-- | A hash of a term (equal terms have equal hashes), whether it is
-- 'dynamic', and its id ('intern'), computed when first needed.
data Info = Info !Int !Bool Int

-- | A compound term as 'intern' knows it: its constructor and its operands,
-- an atom as itself and a compound term by its id.
data NodeKey = NodeKey !Int [NodeArg]
    deriving (Eq, Ord)

data NodeArg = NAtom Term | NNode !Int
    deriving (Eq, Ord)

-- | The id of a compound term, one per distinct term (hash-consing): equal
-- terms, made apart, get the same id, so that comparing them is a single
-- step however large they are.  The ids are numbered in the order they are
-- first asked for, which depends on evaluation order; only their equality is
-- ever used, and that does not.  (The operands' ids are computed before the
-- table is read, as their own calls update it.)
intern :: NodeKey -> Int
intern k@(NodeKey _ args) = unsafePerformIO $ do
    _ <- evaluate (F.foldl' (flip seq) () args)
    (next, table) <- readIORef internTable
    case M.lookup k table of
        Just i  -> return i
        Nothing -> do
            writeIORef internTable (next + 1, M.insert k next table)
            return next
{-# NOINLINE intern #-}

internTable :: IORef (Int, M.Map NodeKey Int)
internTable = unsafePerformIO (newIORef (0, M.empty))
{-# NOINLINE internTable #-}

-- | The id of a compound term ('intern').
termId :: Term -> Int
termId t = case t of
    TNot (Info _ _ i) _      -> i
    TAnd (Info _ _ i) _      -> i
    TOr (Info _ _ i) _       -> i
    TEq (Info _ _ i) _ _     -> i
    TCond (Info _ _ i) _ _ _ -> i
    TAdd (Info _ _ i) _ _    -> i
    _ -> error "schedlean: termId of an atom"

nodeArg :: Term -> NodeArg
nodeArg t | atomic t  = NAtom t
          | otherwise = NNode (termId t)

-- | The operands of @&&@ or @||@: distinct, in order, none of them a
-- literal or (by the smart constructors) of the same operator.  An operand
-- can be added at either end, and one already there moved to the front, in
-- logarithmic time: each has a position (any integer, in order) and is
-- indexed by its hash.  The hash of the whole is the sum of its operands',
-- which only ever join or move.
data TSet = TSet
    { tsOps   :: !(M.Map Int Term)       -- ^ by position
    , tsIndex :: !(M.Map HKey Int)       -- ^ each operand's position
    , tsHash  :: !Int
    , tsDyn   :: !Bool                   -- ^ is any operand 'dynamic'
    }

-- | A term ordered by its hash first: for a set of terms, whose order does
-- not matter, compared quickly.  (An atom's hash is not stored, and would
-- cost as much to compute as comparing the atom itself, so atoms, which come
-- first, are compared as they are.)
newtype HKey = HKey Term

instance Eq HKey where
    HKey a == HKey b = a == b

instance Ord HKey where
    compare (HKey a) (HKey b) = case (atomic a, atomic b) of
        (True, True)   -> compare a b
        (True, False)  -> LT
        (False, True)  -> GT
        (False, False) -> compare (termHash a) (termHash b) <> compare a b

tsList :: TSet -> [Term]
tsList = M.elems . tsOps

tsEmpty :: TSet
tsEmpty = TSet M.empty M.empty 0 False

-- | Add an operand at the end, unless it is there already.
tsSnoc :: TSet -> Term -> TSet
tsSnoc ts x
    | M.member (HKey x) (tsIndex ts) = ts
    | otherwise = tsAt (maybe 0 ((+ 1) . fst) (M.lookupMax (tsOps ts))) x ts

-- | Put an operand at the front, moving it there if it is there already.
tsCons :: Term -> TSet -> TSet
tsCons x ts = case (M.lookup (HKey x) (tsIndex ts), M.lookupMin (tsOps ts)) of
    (Just k, Just (k0, _))
        | k == k0   -> ts
        | otherwise -> ts { tsOps = M.insert (k0 - 1) x (M.delete k (tsOps ts))
                          , tsIndex = M.insert (HKey x) (k0 - 1) (tsIndex ts) }
    (_, lo) -> tsAt (maybe 0 (subtract 1 . fst) lo) x ts

tsAt :: Int -> Term -> TSet -> TSet
tsAt k x ts = TSet { tsOps = M.insert k x (tsOps ts), tsIndex = M.insert (HKey x) k (tsIndex ts)
                   , tsHash = tsHash ts + termHash x, tsDyn = tsDyn ts || dynamic x }

-- | The hash of a term: an atom's from its contents, a compound term's
-- from its 'Info'.
termHash :: Term -> Int
termHash t = case t of
    TIn x       -> str 1 x
    TView x     -> str 2 x
    TRef x dyn  -> str (if dyn then 3 else 4) x
    THw x       -> str 5 x
    TAtHw x     -> str 6 x
    TLit b      -> if b then 7 else 8
    TConst _ v  -> mix 9 (fromInteger (v `mod` 1000000007))
    TStr x      -> str 10 x
    TSt x       -> str 11 x
    TDef xs     -> F.foldl' str 17 xs
    TAt x v     -> F.foldl' str (str 18 x) v
    TUnset s    -> str 19 (show s)
    TRd k s     -> mix (str 20 (show s)) k
    TProj v f   -> F.foldl' str (str 21 f) v
    TFld k s    -> mix (str 22 (show s)) k
    TVF v as f  -> F.foldl' (\h a -> mix h (termHash a)) (str (str 23 v) f) as
    TAtVF x v as -> F.foldl' (\h a -> mix h (termHash a)) (str (str 24 x) v) as
    TNot (Info h _ _) _      -> h
    TAnd (Info h _ _) _      -> h
    TOr (Info h _ _) _       -> h
    TEq (Info h _ _) _ _     -> h
    TCond (Info h _ _) _ _ _ -> h
    TAdd (Info h _ _) _ _    -> h
  where str = F.foldl' (\h c -> mix h (fromEnum c))

mix :: Int -> Int -> Int
mix h x = h * 1000003 + x + 7

-- the compound constructors, with their 'Info'
mkNot :: Term -> Term
mkNot a = TNot (Info (mix 12 (termHash a)) (dynamic a) (intern (NodeKey 0 [nodeArg a]))) a

mkAnd, mkOr :: TSet -> Term
mkAnd ts = TAnd (Info (mix 13 (tsHash ts)) (tsDyn ts) (intern (NodeKey 1 (map nodeArg (tsList ts))))) ts
mkOr ts = TOr (Info (mix 14 (tsHash ts)) (tsDyn ts) (intern (NodeKey 2 (map nodeArg (tsList ts))))) ts

mkEq :: Term -> Term -> Term
mkEq a b = TEq (Info (mix (mix 15 (termHash a)) (termHash b)) (dynamic a || dynamic b)
                     (intern (NodeKey 3 [nodeArg a, nodeArg b]))) a b

mkCond :: Term -> Term -> Term -> Term
mkCond c a b = TCond (Info (mix (mix (mix 16 (termHash c)) (termHash a)) (termHash b))
                           (dynamic c || dynamic a || dynamic b)
                           (intern (NodeKey 4 [nodeArg c, nodeArg a, nodeArg b]))) c a b

mkAdd :: Term -> Term -> Term
mkAdd a b = TAdd (Info (mix (mix 25 (termHash a)) (termHash b)) (dynamic a || dynamic b)
                       (intern (NodeKey 5 [nodeArg a, nodeArg b]))) a b

instance Eq Term where
    a == b
      -- (one term compared with itself, as a shared subterm often is, is
      -- known equal without a walk; a False here proves nothing)
      | isTrue# (reallyUnsafePtrEquality# a b) = True
      | otherwise = case (a, b) of
            (TIn x, TIn y)               -> x == y
            (TView x, TView y)           -> x == y
            (TRef x d1, TRef y d2)       -> x == y && d1 == d2
            (THw x, THw y)               -> x == y
            (TAtHw x, TAtHw y)           -> x == y
            (TLit x, TLit y)             -> x == y
            (TConst s x, TConst s' y)    -> s == s' && x == y
            (TStr x, TStr y)             -> x == y
            (TSt x, TSt y)               -> x == y
            (TDef x, TDef y)             -> x == y
            (TAt x v, TAt y w)           -> x == y && v == w
            (TUnset s, TUnset s')        -> s == s'
            (TRd k s, TRd k' s')         -> k == k' && s == s'
            (TProj v f, TProj w g)       -> f == g && v == w
            (TFld k s, TFld k' s')       -> k == k' && s == s'
            (TVF v as f, TVF w bs g)     -> f == g && v == w && as == bs
            (TAtVF x v as, TAtVF y w bs) -> x == y && v == w && as == bs
            (TNot i _, TNot j _)         -> sameNode i j
            (TAnd i _, TAnd j _)         -> sameNode i j
            (TOr i _, TOr j _)           -> sameNode i j
            (TEq i _ _, TEq j _ _)       -> sameNode i j
            (TCond i _ _ _, TCond j _ _ _) -> sameNode i j
            (TAdd i _ _, TAdd j _ _)     -> sameNode i j
            _                            -> False

-- | Are two compound terms equal?  (Different hashes say no at once.)
sameNode :: Info -> Info -> Bool
sameNode (Info h _ i) (Info h' _ i') = h == h' && i == i'

-- | The order of the plain tree: by constructor, then by the fields in turn.
instance Ord Term where
    compare a b
      | isTrue# (reallyUnsafePtrEquality# a b) = EQ   -- (as for '==')
      | otherwise = case (a, b) of
            (TIn x, TIn y)               -> compare x y
            (TView x, TView y)           -> compare x y
            (TRef x d1, TRef y d2)       -> compare x y <> compare d1 d2
            (THw x, THw y)               -> compare x y
            (TAtHw x, TAtHw y)           -> compare x y
            (TLit x, TLit y)             -> compare x y
            (TConst s x, TConst s' y)    -> compare s s' <> compare x y
            (TStr x, TStr y)             -> compare x y
            (TSt x, TSt y)               -> compare x y
            (TDef x, TDef y)             -> compare x y
            (TAt x v, TAt y w)           -> compare x y <> compare v w
            (TUnset s, TUnset s')        -> compare s s'
            (TRd k s, TRd k' s')         -> compare k k' <> compare s s'
            (TProj v f, TProj w g)       -> compare v w <> compare f g
            (TFld k s, TFld k' s')       -> compare k k' <> compare s s'
            (TVF v as f, TVF w bs g)     -> compare v w <> compare f g <> compare as bs
            (TAtVF x v as, TAtVF y w bs) -> compare x y <> compare v w <> compare as bs
            -- (equal compound terms are known equal by 'sameNode')
            (TNot i x, TNot j y)         -> unlessSame i j (compare x y)
            (TAnd i x, TAnd j y)         -> unlessSame i j (compare (tsList x) (tsList y))
            (TOr i x, TOr j y)           -> unlessSame i j (compare (tsList x) (tsList y))
            (TEq i x1 x2, TEq j y1 y2)   -> unlessSame i j (compare x1 y1 <> compare x2 y2)
            (TCond i x1 x2 x3, TCond j y1 y2 y3) ->
                unlessSame i j (compare x1 y1 <> compare x2 y2 <> compare x3 y3)
            (TAdd i x1 x2, TAdd j y1 y2) -> unlessSame i j (compare x1 y1 <> compare x2 y2)
            _                            -> compare (tag a) (tag b)
      where
        unlessSame i j o = if sameNode i j then EQ else o
        tag :: Term -> Int
        tag t = case t of
            TIn _ -> 0; TView _ -> 1; TRef _ _ -> 2; THw _ -> 3; TAtHw _ -> 4; TLit _ -> 5
            TConst _ _ -> 6; TStr _ -> 7; TSt _ -> 8; TNot _ _ -> 9; TAnd _ _ -> 10; TOr _ _ -> 11
            TEq _ _ _ -> 12; TCond _ _ _ _ -> 13; TDef _ -> 14; TAt _ _ -> 15; TUnset _ -> 16
            TRd _ _ -> 17; TProj _ _ -> 18; TFld _ _ -> 19; TVF _ _ _ -> 20; TAtVF _ _ _ -> 21
            TAdd _ _ _ -> 22

-- | As the plain tree would be shown.
instance Show Term where
    showsPrec d t = showParen (d > 10) $ case t of
        TIn x          -> showString "TIn " . showsPrec 11 x
        TView x        -> showString "TView " . showsPrec 11 x
        TRef x dyn     -> showString "TRef " . showsPrec 11 x . showChar ' ' . showsPrec 11 dyn
        THw x          -> showString "THw " . showsPrec 11 x
        TAtHw x        -> showString "TAtHw " . showsPrec 11 x
        TLit b         -> showString "TLit " . showsPrec 11 b
        TConst s v     -> showString "TConst " . showsPrec 11 s . showChar ' ' . showsPrec 11 v
        TStr x         -> showString "TStr " . showsPrec 11 x
        TSt x          -> showString "TSt " . showsPrec 11 x
        TDef x         -> showString "TDef " . showsPrec 11 x
        TAt x v        -> showString "TAt " . showsPrec 11 x . showChar ' ' . showsPrec 11 v
        TUnset s       -> showString "TUnset " . showsPrec 11 s
        TRd k s        -> showString "TRd " . showsPrec 11 k . showChar ' ' . showsPrec 11 s
        TProj v f      -> showString "TProj " . showsPrec 11 v . showChar ' ' . showsPrec 11 f
        TFld k s       -> showString "TFld " . showsPrec 11 k . showChar ' ' . showsPrec 11 s
        TVF v as f     -> showString "TVF " . showsPrec 11 v . showChar ' ' . showsPrec 11 as
                          . showChar ' ' . showsPrec 11 f
        TAtVF x v as   -> showString "TAtVF " . showsPrec 11 x . showChar ' ' . showsPrec 11 v
                          . showChar ' ' . showsPrec 11 as
        TNot _ x       -> showString "TNot " . showsPrec 11 x
        TAnd _ ts      -> showString "TAnd " . showsPrec 11 (tsList ts)
        TOr _ ts       -> showString "TOr " . showsPrec 11 (tsList ts)
        TEq _ x y      -> showString "TEq " . showsPrec 11 x . showChar ' ' . showsPrec 11 y
        TCond _ x y z  -> showString "TCond " . showsPrec 11 x . showChar ' ' . showsPrec 11 y
                          . showChar ' ' . showsPrec 11 z
        TAdd _ x y     -> showString "TAdd " . showsPrec 11 x . showChar ' ' . showsPrec 11 y

tNot :: Term -> Term
tNot (TLit b)   = TLit (not b)
tNot (TNot _ t) = t
tNot t          = mkNot t

-- | @&&@ of terms: the operands of those that are @&&@ themselves and the
-- others, without @true@ and repeats (the first of each kept), and @false@
-- if any is.  The operands of the largest @&&@ among them are kept as they
-- are; the others are added before or after them one by one, so that a
-- chain of @&&@ built up a step at a time costs a logarithm a step.
tAnd :: [Term] -> Term
tAnd = junction (\t -> case t of { TAnd _ ts -> Just ts; _ -> Nothing }) True mkAnd

tOr :: [Term] -> Term
tOr = junction (\t -> case t of { TOr _ ts -> Just ts; _ -> Nothing }) False mkOr

-- | 'tAnd' (unit @true@) and 'tOr' (unit @false@).
junction :: (Term -> Maybe TSet) -> Bool -> (TSet -> Term) -> [Term] -> Term
junction ops unit build ts
    | TLit (not unit) `elem` ts = TLit (not unit)
    | otherwise = case tsList result of
        []  -> TLit unit
        [x] -> x
        _   -> build result
  where
    pieces = [ maybe (Left t) Right (ops t) | t <- ts, t /= TLit unit ]
    sizes = [ (M.size (tsOps s), k) | (k, Right s) <- zip [0 :: Int ..] pieces ]
    result = case sizes of
        [] -> F.foldl' snocPiece tsEmpty pieces
        _  -> let k = snd (maximumOn fst sizes)
                  (before, rest) = splitAt k pieces
              in case rest of
                  Right base : after -> foldr consPiece (F.foldl' snocPiece base after) before
                  _ -> F.foldl' snocPiece tsEmpty pieces
    snocPiece acc p = either (tsSnoc acc) (F.foldl' tsSnoc acc . tsList) p
    consPiece p acc = either (`tsCons` acc) (\s -> foldr tsCons acc (tsList s)) p
    -- the first of the largest
    maximumOn f = foldr1 (\x y -> if f x >= f y then x else y)

tEq :: Term -> Term -> Term
tEq a b | a == b = TLit True
tEq (TLit x) (TLit y) = TLit (x == y)
tEq (TConst s x) (TConst s' y) | s == s' = TLit (x == y)
tEq (TStr x) (TStr y) = TLit (x == y)
tEq (TLit True) b = b
tEq a (TLit True) = a
tEq (TLit False) b = tNot b
tEq a (TLit False) = tNot a
tEq a b = mkEq a b

tCond :: Term -> Term -> Term -> Term
tCond (TLit True) a _ = a
tCond (TLit False) _ b = b
tCond c a b
    | a == b = a
    | a == TLit True && b == TLit False = c
    | a == TLit False && b == TLit True = tNot c
    | otherwise = mkCond c a b

-- | @a + b@ of task-call positions, a zero dropped.
tAdd :: Term -> Term -> Term
tAdd a (TConst SPos 0) = a
tAdd (TConst SPos 0) b = b
tAdd a b = mkAdd a b

-- | One more position when a call is made: @bif c then 1 else 0@.
tInc :: Term -> Term
tInc c = tCond c (TConst SPos 1) (TConst SPos 0)

atomic :: Term -> Bool
atomic t = case t of
    TIn _ -> True; TView _ -> True; TRef _ _ -> True; THw _ -> True; TAtHw _ -> True
    TLit _ -> True; TConst _ _ -> True; TStr _ -> True; TSt _ -> True; TDef _ -> True
    TAt _ _ -> True; TUnset _ -> True; TRd _ _ -> True; TProj _ _ -> True; TFld _ _ -> True
    TVF _ _ _ -> True; TAtVF _ _ _ -> True; _ -> False

-- | Does a term depend on the observing step (mention its 'View')?
dynamic :: Term -> Bool
dynamic t = case t of
    TView _     -> True
    TRef _ dyn  -> dyn
    TNot (Info _ dyn _) _      -> dyn
    TAnd (Info _ dyn _) _      -> dyn
    TOr (Info _ dyn _) _       -> dyn
    TEq (Info _ dyn _) _ _     -> dyn
    TCond (Info _ dyn _) _ _ _ -> dyn
    TAdd (Info _ dyn _) _ _    -> dyn
    _           -> False

-- | Every subterm of a term, the term first (each distinct compound subterm
-- once; an atom as often as it is met).
universeT :: Term -> [Term]
universeT t0 = go [t0] IS.empty
  where go [] _ = []
        go (t : ts) seen
            | atomic t = t : go ts seen
            | IS.member (termId t) seen = go ts seen
            | otherwise = t : go (subTerms t ++ ts) (IS.insert (termId t) seen)

subTerms :: Term -> [Term]
subTerms t = case t of
    TNot _ a -> [a]; TAnd _ ts -> tsList ts; TOr _ ts -> tsList ts; TEq _ a b -> [a, b]
    TCond _ a b c -> [a, b, c]
    TAdd _ a b -> [a, b]
    _ -> []

-- | Rewrite a term: @f@ replaces a subterm, or leaves it to be rebuilt from
-- its rewritten parts by the smart constructors.  A subterm nothing in
-- which is replaced is kept as it is rather than rebuilt: every compound
-- term is the smart constructors' work, which rebuilding from the same
-- parts would only repeat (comparing the parts again).  Each distinct
-- compound subterm is rewritten once, however often it occurs ('termId').
mapTerm :: (Term -> Maybe Term) -> Term -> Term
mapTerm f t = fromMaybe t (evalState (changed t) M.empty)
  where
    -- the rewritten term, or Nothing if it is the same
    changed x
        | atomic x  = return (f x)
        | otherwise = do
            known <- gets (M.lookup (termId x))
            case known of
                Just r  -> return r
                Nothing -> do
                    r <- rewrite x
                    modify' (M.insert (termId x) r)
                    return r
    rewrite x = case f x of
        Just y  -> return (Just y)
        Nothing -> case x of
            TNot _ a      -> fmap tNot <$> changed a
            TAnd _ ts     -> fmap tAnd <$> parts (tsList ts)
            TOr _ ts      -> fmap tOr <$> parts (tsList ts)
            TEq _ a b     -> do
                ma <- changed a
                mb <- changed b
                return $ case (ma, mb) of
                    (Nothing, Nothing) -> Nothing
                    _ -> Just (tEq (fromMaybe a ma) (fromMaybe b mb))
            TCond _ a b c -> do
                ma <- changed a
                mb <- changed b
                mc <- changed c
                return $ case (ma, mb, mc) of
                    (Nothing, Nothing, Nothing) -> Nothing
                    _ -> Just (tCond (fromMaybe a ma) (fromMaybe b mb) (fromMaybe c mc))
            TAdd _ a b    -> do
                ma <- changed a
                mb <- changed b
                return $ case (ma, mb) of
                    (Nothing, Nothing) -> Nothing
                    _ -> Just (tAdd (fromMaybe a ma) (fromMaybe b mb))
            _             -> return Nothing
    parts xs = do
        ms <- mapM changed xs
        return (if all isNothing ms then Nothing else Just (zipWith fromMaybe xs ms))

-- | Where a free variable comes from; the 'Inputs' structure is grouped so.
data Group = GInput | GStart | GWire | GDatapath | GTask | GUnspec | GStep String String
    deriving (Eq, Ord, Show)

data Var = Var { vaName :: String, vaSort :: Sort, vaGroup :: Group, vaNote :: String }

-- | A value that changes within a cycle, as one observer sees it.
data Field = Field
    { fdName :: String
    , fdSort :: Sort
    , fdHw   :: Term               -- ^ what the hardware sees
    , fdKind :: FieldKind
    , fdNote :: String
    }

data FieldKind
    = FRead Port [Term]            -- ^ a child output; the call's arguments
    | FApp String String [Term] [Term]
      -- ^ an application from a definition: op, its arguments here, in the hardware
    | FAV Port                     -- ^ an action-value result

-- (the terms by hash first: the keys are only looked up)
data FieldKey = KRead (String, String, Maybe Integer) Int [HKey]
              | KApp String Sort [HKey]
              | KAV (String, String, Maybe Integer) Int
    deriving (Eq, Ord)

-- | A Lean definition.
data LDef = LDef
    { ldName :: String, ldSort :: Sort, ldBody :: Term, ldDyn :: Bool }

-- | @premises ⇒ lhs = rhs@, a congruence (functional consistency) fact.
data Cong = Cong { cgName :: String, cgPremises :: [Premise], cgLhs :: Term, cgRhs :: Term
                 , cgNote :: String }

data Premise = PEq Term Term        -- ^ a sequential value equals a hardware value
             | PPort Port [Term]
               -- ^ a port's fields (its enable, then its arguments), as some
               -- point of the sequential execution has them, agree with the hardware
             | PTrue Term
               -- ^ a condition holds: the reading step does not fire after a
               -- use its read may not follow

data Side = Seq | Hw deriving (Eq, Ord, Show)

data Acc = Acc
    { acDesign   :: Design
    , acVars     :: M.Map String Var
    , acVarOrder :: [String]                 -- ^ newest first
    , acAppVars  :: M.Map (String, Sort, [HKey]) String   -- ^ (only looked up)
    , acAppInfo  :: M.Map String (Side, String, Sort, [Term])  -- ^ each, as first made
    , acFields   :: M.Map String Field
    , acFieldOrd :: [String]                 -- ^ newest first
    , acFieldKey :: M.Map FieldKey String
    , acConsts   :: M.Map Sort (S.Set Integer)
    , acStrings  :: S.Set String
    , acSeqMemo  :: M.Map String Term
    , acHwMemo   :: M.Map String Term
    , acSeqDefs  :: [LDef]                   -- ^ newest first
    , acHwDefs   :: [LDef]
    , acHwApps   :: M.Map String (String, [Term], Term)
    , acSeqApps  :: M.Map String (String, [Term], Term)
    , acStatic   :: [Cong]
    , acMuxes    :: [(String, [Term])]
    , acCtx      :: Maybe String
    , acFresh    :: Int
    , acUnspec   :: M.Map (Side, String) Int    -- ^ unspecified values met, per definition
    , acNextSuffix :: M.Map (Bool, String) Int
      -- ^ for 'freshName': below this suffix, every @base#k@ is taken
    , acReach    :: M.Map String Term
      -- ^ the conditions of 'reachPass' made so far, by name ('shareReach')
    , acWarnings :: [String]
    }

type Gen = State Acc

warn :: String -> Gen ()
warn w = modify' (\a -> a { acWarnings = w : acWarnings a })

fresh :: Gen Int
fresh = do
    n <- gets acFresh
    modify' (\a -> a { acFresh = n + 1 })
    return n

-- | A free variable of the cycle (created on first use).
inputVar :: Group -> String -> Sort -> String -> Gen Term
inputVar g name s note = do
    vs <- gets acVars
    case M.lookup name vs of
        Just _  -> return ()
        Nothing -> modify' (\a -> a { acVars = M.insert name (Var name s g note) vs
                                    , acVarOrder = name : acVarOrder a })
    return (TIn name)

-- | A name not yet used for a variable (an 'Inputs' field), or for a 'View'
-- field: the two live in different structures, so each is checked alone.
--
-- The search for a free @base#k@ starts where the last one for the base
-- stopped: names are never removed, and every caller takes the name it is
-- given, so the suffixes below it are all still taken.
freshName :: Bool -> String -> Gen String
freshName forField base = do
    taken <- if forField then (\fs n -> M.member n fs) <$> gets acFields
                         else (\vs n -> M.member n vs) <$> gets acVars
    let pick k = if taken (named k) then pick (k + 1) else k
        named k = base ++ "#" ++ show k
    if not (taken base) then return base else do
        k0 <- gets (M.findWithDefault 1 (forField, base) . acNextSuffix)
        let k = pick k0
        modify' (\a -> a { acNextSuffix = M.insert (forField, base) (k + 1) (acNextSuffix a) })
        return (named k)

constant :: AType -> Integer -> Gen Term
constant t v = case sortOf t of
    SBool -> return (TLit (v /= 0))
    SUnit -> return (TLit True)
    s     -> do modify' (\a -> a { acConsts = M.insertWith S.union s (S.singleton v) (acConsts a) })
                return (TConst s v)

stringConst :: String -> Gen Term
stringConst s = do
    modify' (\a -> a { acStrings = S.insert s (acStrings a) })
    return (TStr s)

-- | The value bsc's Verilog back end gives an unspecified value
-- (@-unspecified-to@); with @X@ or @Z@, any value: a fresh variable.
unspecified :: Side -> AType -> Gen Term
unspecified side t = do
    mode <- gets (dUnspec . acDesign)
    let n = case sortOf t of { SBits w -> w; SBool -> 1; _ -> 0 }
    case mode of
        "0" -> constant t 0
        "1" -> constant t (2 ^ n - 1)
        "A" -> constant t (aaaa n)
        -- X or Z: any value, but the same one in both semantics, so the
        -- k-th unspecified value of a definition is one variable
        _   -> do ctx <- gets acCtx
                  name <- case ctx of
                      Just c -> do
                          k <- gets (M.findWithDefault 0 (side, c) . acUnspec)
                          modify' (\a -> a { acUnspec = M.insert (side, c) (k + 1) (acUnspec a) })
                          return ("unspecified@" ++ c ++ "#" ++ show k)
                      Nothing -> (\k -> "unspecified#" ++ show k) <$> fresh
                  inputVar GUnspec name (sortOf t) "an unspecified value"

-- | Evaluate an expression in one of the two semantics.
eval :: Side -> AExpr -> Gen Term
eval side e = case e of
    ASInt _ t (IntLit { ilValue = v }) -> constant t v
    ASStr _ _ s       -> stringConst s
    ASAny _ (Just v)  -> eval side v
    ASAny t Nothing   -> unspecified side t
    ASDef t i         -> ident side t (getIdString i)
    ASPort t i        -> ident side t (getIdString i)
    ASParam t i       -> inputVar GInput (getIdString i) (sortOf t) "module parameter"
    APrim _ t op args -> prim side t op args
    AMethCall t o m args | side == Seq -> childRead t o m args 0 e
    ATupleSel t inner@(AMethCall _ o m args) k
        | side == Seq -> childRead t o m args (fromInteger k - 1) inner
    AMethValue t o m | side == Seq -> avResult t o m 0
    ATupleSel t (AMethValue _ o m) k | side == Seq -> avResult t o m (fromInteger k - 1)
    ATupleSel t inner k -> tupleSelect side t inner k
    AFunCall t _ f _ args -> apply side ("function " ++ f) t args
    ANoInlineFunCall t _ (ANoInlineFun f ns _ _) args ->
        apply side (unwords ("noinline" : f : map show ns)) t args
    ATaskValue t _ f _ cookie -> do
        ctx <- gets acCtx
        appTerm Seq ("task " ++ f) (sortOf t) [TConst SUnit cookie]
            ("result of " ++ f ++ maybe "" (\c -> ", as " ++ c) ctx) GTask
    _ -> do warn ("cannot model the expression " ++ ppString e)
            k <- fresh
            inputVar GUnspec ("unmodelled#" ++ show k) (sortOf (aType e)) (take 60 (ppString e))

-- | A name in an expression: a module input, a definition, or (in the
-- hardware) a child's output wire.  In the sequential semantics a WILL_FIRE
-- signal is a scheduling decision, so it is read at the hardware's view.
ident :: Side -> AType -> String -> Gen Term
ident side t name = do
    d <- gets acDesign
    case () of
      _ | M.member name (dInputs d) -> inputVar GInput name (sortOf t) "module input"
        | side == Seq, "WILL_FIRE_" `isPrefixOf` name, M.member name (dSeqDefs d) ->
            atHardware <$> defTerm Seq name
        | side == Seq, M.member name (dSeqDefs d) -> defTerm Seq name
        | side == Hw,  M.member name (dHwDefs d)  -> defTerm Hw name
        | side == Hw,  Just (p, k) <- M.lookup name (dStateOuts d) -> childWire p k
        | otherwise -> do warn ("unknown signal " ++ name)
                          inputVar GUnspec name (sortOf t) "unknown signal"

-- | A shared definition read at the hardware's view rather than the step's.
atHardware :: Term -> Term
atHardware (TRef x True) = TAtHw x
atHardware t = t

-- | A child's output wire, in the hardware.
childWire :: Port -> Int -> Gen Term
childWire p k = do
    d <- gets acDesign
    let (sig0, t) = ptResults p !! k
        static = case lookupMeth d (ptInst p) (ptMeth p) of
            Just mt -> null (mtDeps mt) && null (ptArgs p) && ptKind p == ValueMethod
            Nothing -> False
        -- methods reading the same output port are one variable
        sig = if static then M.findWithDefault sig0 sig0 (chAlias (childOf d p)) else sig0
    inputVar (if static then GStart else GWire) sig (sortOf t) (portLabel p)

-- | A read of a child's value method, in the sequential semantics.
--
-- A method without arguments that sees no same-cycle calls returns the same
-- value whoever reads it, so it is the hardware's wire itself.  Otherwise
-- the value is an application of the method to (the state of the calls it
-- sees, its arguments), and a congruence ties it to the hardware's output
-- for the same copy.  A copy some step reads where the annotations rule
-- the read out ('dHavocPorts') is a value of the reader's view in every
-- case: there it is arbitrary unless its congruence says otherwise.
childRead :: AType -> AId -> AId -> [AExpr] -> Int -> AExpr -> Gen Term
childRead t o m args k use = do
    d <- gets acDesign
    let inst = getIdString o
        meth = getIdBaseString m
    case lookupMeth d inst meth >>= \mt -> fmap ((,) mt) (readPortFor d mt inst use) of
        Nothing -> do
            warn ("read of an unknown or unused method " ++ inst ++ "." ++ meth)
            inputVar GUnspec (inst ++ "." ++ meth) (sortOf t) "unknown method"
        Just (_, port) | null (drop k (ptResults port)) || length args' /= length (ptArgs port) ->
            unmatched port
        Just (mt, port) -> do
            hwOut <- childWire port k
            argTs <- mapM (eval Seq) args'
            let sig = fst (ptResults port !! k)
                -- a read some step makes where the annotations rule it out
                -- is a value of the reading step's own
                havoc = S.member port (dHavocPorts d)
            if null argTs && null (mtDeps mt) && not havoc
                then return hwOut
                else if null (mtDeps mt) && not (any dynamic argTs) && not havoc
                then do
                    -- context-free, but the copy's argument is a hardware mux
                    hwArgs <- mapM (\(s, ty) -> ident Hw ty s) (ptArgs port)
                    v <- appTerm Seq ("read " ++ portLabel port ++ "#" ++ show k) (sortOf t) argTs
                             (portLabel port ++ "(...)") GDatapath
                    when (v /= hwOut) $
                        addStatic Cong { cgName = "read_" ++ termName v
                                       , cgPremises = zipWith PEq argTs hwArgs
                                       , cgLhs = v, cgRhs = hwOut
                                       , cgNote = portLabel port ++ " returns the same value for the same arguments" }
                    return v
                else field (KRead (portKey port) k (map HKey argTs)) (sortOf t) hwOut (FRead port argTs)
                           (if null argTs then sig else "") (portLabel port)
  where
    args' = concatMap argInputPorts args
    -- a read that does not line up with the copy's ports: say so, and leave
    -- its value unconstrained rather than relate it to the wrong wires
    unmatched port = do
        warn ("a read of " ++ portLabel port ++ " does not match its ports; its value is unconstrained")
        n <- fresh
        inputVar GUnspec ("unmodelled#" ++ show n) (sortOf t) (portLabel port)

-- | The result of an action-value method called by the step.
avResult :: AType -> AId -> AId -> Int -> Gen Term
avResult t o m k = do
    d <- gets acDesign
    let inst = getIdString o
        meth = getIdBaseString m
    case lookupMeth d inst meth of
        Just mt | (port : _) <- mtCopies mt, not (null (drop k (ptResults port))) -> do
            hwOut <- childWire port k
            if null (ptArgs port) && null (mtDeps mt)
                then return hwOut
                else field (KAV (portKey port) k) (sortOf t) hwOut (FAV port)
                           (fst (ptResults port !! k)) (portLabel port)
        _ -> do warn ("result of an unknown action-value method " ++ inst ++ "." ++ meth)
                inputVar GUnspec (inst ++ "." ++ meth) (sortOf t) "unknown method"

-- | A value observed within the cycle: a field of the 'View'.
field :: FieldKey -> Sort -> Term -> FieldKind -> String -> String -> Gen Term
field key s hw kind preferred note = do
    keys <- gets acFieldKey
    case M.lookup key keys of
        Just n  -> return (TView n)
        Nothing -> do
            ctx <- gets acCtx
            n <- freshName True (if null preferred then fromMaybe "observed" ctx else preferred)
            modify' (\a -> a { acFields = M.insert n (Field n s hw kind note) (acFields a)
                             , acFieldOrd = n : acFieldOrd a
                             , acFieldKey = M.insert key n keys })
            return (TView n)

termName :: Term -> String
termName (TIn n) = n
termName t = show t

addStatic :: Cong -> Gen ()
addStatic c = modify' (\a -> a { acStatic = c : acStatic a })

-- | An application of an uninterpreted function.
appTerm :: Side -> String -> Sort -> [Term] -> String -> Group -> Gen Term
appTerm side op s args note grp
    | side == Seq && any dynamic args = do
        ctx <- gets acCtx
        field (KApp op s (map HKey args)) s (TLit False) (FApp (fromMaybe "" ctx) op args []) "" note
    | otherwise = do
        apps <- gets acAppVars
        case M.lookup (op, s, map HKey args) apps of
            Just n  -> return (TIn n)
            Nothing -> do
                ctx <- gets acCtx
                n <- freshName False (fromMaybe "t" ctx)
                modify' (\a -> a { acAppVars = M.insert (op, s, map HKey args) n apps
                                 , acAppInfo = M.insert n (side, op, s, args) (acAppInfo a) })
                inputVar grp n s note

-- | An uninterpreted application of an operator to sub-expressions.
apply :: Side -> String -> AType -> [AExpr] -> Gen Term
apply side op t args = do
    xs <- mapM (eval side) args
    appTerm side op (sortOf t) xs op GDatapath

-- | Is this primitive application datapath (abstracted) rather than control?
datapathPrim :: AType -> PrimOp -> [AExpr] -> Bool
datapathPrim t op args =
    not (op `elem` [PrimBAnd, PrimBOr, PrimBNot, PrimIf, PrimMux, PrimPriMux, PrimCase,
                    PrimArrayDynSelect, PrimEQ, PrimEQ3, PrimBuildArray])
    && not (allBool && op `elem` boolOps)
  where allBool = sortOf t == SBool && not (null args) && all ((== SBool) . sortOf . aType) args
        boolOps = [PrimAnd, PrimOr, PrimXor, PrimInv, PrimULT, PrimULE, PrimSLT, PrimSLE,
                   PrimAdd, PrimSub, PrimMul, PrimNeg, PrimSignExt]

primName :: PrimOp -> String
primName = drop 4 . show

prim :: Side -> AType -> PrimOp -> [AExpr] -> Gen Term
prim side t op args
    | datapathPrim t op args = apply side (primName op) t args
    | otherwise = case (op, args) of
        (PrimBAnd, _)       -> tAnd <$> mapM ev args
        (PrimBOr, _)        -> tOr <$> mapM ev args
        (PrimBNot, [a])     -> tNot <$> ev a
        (PrimIf, [c, a, b]) -> tCond <$> ev c <*> ev a <*> ev b
        (PrimMux, _)        -> mux True
        (PrimPriMux, _)     -> mux False
        (PrimCase, idx : dflt : arms) -> do
            i <- ev idx
            dv <- ev dflt
            alts <- forM (pairUp arms) $ \(l, v) -> (,) <$> ev l <*> ev v
            return (foldr (\(l, v) acc -> tCond (tEq i l) v acc) dv alts)
        (PrimArrayDynSelect, [arr, idx]) -> do
            i <- ev idx
            es <- arrayElems arr
            let w = case sortOf (aType idx) of { SBool -> 1; SBits b -> b; _ -> 0 }
                len = length es
                n = min len (if w >= 20 then len else 2 ^ w)
            ks <- mapM (\k -> constant (aType idx) (toInteger k)) [0 .. n - 1]
            vs <- mapM ev (take n es)
            dflt <- if n == len && len == 2 ^ w && not (null vs)
                        then return (last vs) else unspecified side t
            return (foldr (\(kk, v) acc -> tCond (tEq i kk) v acc) dflt (zip ks vs))
        (PrimEQ, [a, b])    -> tEq <$> ev a <*> ev b
        (PrimEQ3, [a, b])   -> tEq <$> ev a <*> ev b
        _ -> do
            xs <- mapM ev args
            case (op, xs) of
                (PrimAnd, _)      -> return (tAnd xs)
                (PrimOr, _)       -> return (tOr xs)
                (PrimXor, [a, b]) -> return (tNot (tEq a b))
                (PrimInv, [a])    -> return (tNot a)
                (PrimULT, [a, b]) -> return (tAnd [tNot a, b])
                (PrimULE, [a, b]) -> return (tOr [tNot a, b])
                (PrimSLT, [a, b]) -> return (tAnd [a, tNot b])
                (PrimSLE, [a, b]) -> return (tOr [a, tNot b])
                (PrimAdd, [a, b]) -> return (tNot (tEq a b))
                (PrimSub, [a, b]) -> return (tNot (tEq a b))
                (PrimMul, [a, b]) -> return (tAnd [a, b])
                (PrimNeg, [a])    -> return a
                (PrimSignExt, [a]) -> return a
                _ -> apply side (primName op) t args
  where
    ev = eval side
    -- Mux / PriMux: (selector, value) pairs, first true selector wins; the
    -- last pair aState adds is the default arm.  A parallel mux is correct
    -- only if its selectors are one-hot, which the theorem also proves.
    mux parallel = do
        let ps = pairUp args
            (arms, dflt) = case reverse ps of
                ((APrim _ _ PrimBNot [_], v) : rest) -> (reverse rest, Just v)
                _                                    -> (ps, Nothing)
        sels <- mapM (ev . fst) arms
        vals <- mapM (ev . snd) arms
        dv <- maybe (unspecified side t) ev dflt
        when (parallel && side == Hw && length sels > 1) $ do
            ctx <- gets acCtx
            modify' (\a -> a { acMuxes = (fromMaybe "mux" ctx, sels) : acMuxes a })
        return (foldr (\(s, v) acc -> tCond s v acc) dv (zip sels vals))
    arrayElems arr = case arr of
        APrim _ _ PrimBuildArray es -> return es
        ASDef _ i -> do
            d <- gets acDesign
            let defs = if side == Seq then dSeqDefs d else dHwDefs d
            maybe (return []) (arrayElems . adef_expr) (M.lookup (getIdString i) defs)
        _ -> return []

pairUp :: [a] -> [(a, a)]
pairUp (a : b : rest) = (a, b) : pairUp rest
pairUp _ = []

-- | Select a component of a tuple (SplitPorts values).
tupleSelect :: Side -> AType -> AExpr -> Integer -> Gen Term
tupleSelect side t inner k = case inner of
    ATuple _ es | k >= 1, (x : _) <- drop (fromInteger k - 1) es -> eval side x
    ASDef _ i -> do
        d <- gets acDesign
        let defs = if side == Seq then dSeqDefs d else dHwDefs d
        case M.lookup (getIdString i) defs of
            Just def -> tupleSelect side t (adef_expr def) k
            Nothing  -> unselectable
    -- a component of an uninterpreted function's result is an uninterpreted
    -- function of the same arguments, one per component
    _ | Just (op, args) <- appOf inner -> apply side (op ++ " component " ++ show k) t args
    _ -> unselectable
  where
    unselectable = do
        warn ("cannot select component " ++ show k ++ " of " ++ ppString inner)
        n <- fresh
        inputVar GUnspec ("unmodelled#" ++ show n) (sortOf t) (ppString inner)

-- | The value of a definition: inlined if it is a mere name or constant,
-- otherwise a reference to a Lean definition.  CAN_FIRE, WILL_FIRE and RDY
-- signals always get a definition, so the proof reads like the hardware.
defTerm :: Side -> String -> Gen Term
defTerm side name = do
    memo <- gets (if side == Seq then acSeqMemo else acHwMemo)
    case M.lookup name memo of
        Just t  -> return t
        Nothing -> do
            d <- gets acDesign
            let def = (if side == Seq then dSeqDefs d else dHwDefs d) M.! name
                s   = sortOf (adef_type def)
                e   = adef_expr def
            outer <- gets acCtx
            modify' (\a -> a { acCtx = Just name })
            body <- case appOf e of
                Just (op, args) -> do
                    xs <- mapM (eval side) args
                    r <- appTerm side op s xs op GDatapath
                    recordApp side name op xs r
                    return r
                Nothing -> eval side e
            modify' (\a -> a { acCtx = outer })
            t <- if atomic body && not (keep name)
                     then return body
                     else do
                         let ld = LDef name s body (dynamic body)
                         modify' (\a -> if side == Seq then a { acSeqDefs = ld : acSeqDefs a }
                                                       else a { acHwDefs = ld : acHwDefs a })
                         return (if side == Seq then TRef name (dynamic body) else THw name)
            modify' (\a -> if side == Seq then a { acSeqMemo = M.insert name t (acSeqMemo a) }
                                          else a { acHwMemo = M.insert name t (acHwMemo a) })
            return t
  where
    keep n = any (`isPrefixOf` n) ["CAN_FIRE_", "WILL_FIRE_", "RDY_"]

recordApp :: Side -> String -> String -> [Term] -> Term -> Gen ()
recordApp side name op xs r = modify' $ \a ->
    if side == Hw then a { acHwApps = M.insert name (op, xs, r) (acHwApps a) }
                  else a { acSeqApps = M.insert name (op, xs, r) (acSeqApps a) }

-- | An application left uninterpreted: its operator and arguments.
appOf :: AExpr -> Maybe (String, [AExpr])
appOf e = case e of
    APrim _ t op args | datapathPrim t op args -> Just (primName op, args)
    AFunCall _ _ f _ args -> Just ("function " ++ f, args)
    ANoInlineFunCall _ _ (ANoInlineFun f ns _ _) args ->
        Just (unwords ("noinline" : f : map show ns), args)
    _ -> Nothing

-- | Give each application a step observes its hardware counterpart, oldest
-- first, so the observed values among its arguments already have theirs.
-- The counterpart is the hardware's definition of the same name when that
-- applies the same operator; otherwise the operator applied to the
-- arguments' hardware values (an application nested in a definition, or
-- in no definition).  Either way the step's value equals it when the
-- arguments agree, which the observed value's congruence states ('buildSeq').
pairApplications :: Gen ()
pairApplications = do
    names <- gets (reverse . acFieldOrd)
    forM_ names $ \n -> do
        f <- gets ((M.! n) . acFields)
        case fdKind f of
            FApp def op xs _ -> do
                byName <- gets (M.lookup def . acHwApps)
                (ys, hv) <- case byName of
                    Just (op', ys, hv) | op' == op && length ys == length xs -> return (ys, hv)
                    _ -> do
                        fs <- gets acFields
                        let ys = map (atHwView fs) xs
                        outer <- gets acCtx
                        modify' (\a -> a { acCtx = Just n })
                        hv <- appTerm Hw op (fdSort f) ys (fdNote f) GDatapath
                        modify' (\a -> a { acCtx = outer })
                        return (ys, hv)
                modify' (\a -> a { acFields = M.insert n f { fdHw = hv, fdKind = FApp def op xs ys }
                                                       (acFields a) })
            _ -> return ()
  where
    atHwView fs = mapTerm $ \case
        TView g     -> fdHw <$> M.lookup g fs
        TRef x True -> Just (TAtHw x)
        _           -> Nothing

-- ----------------------------------------------------------------------------
-- ** When a read reaches a value
-- ----------------------------------------------------------------------------
--
-- What a system task prints is observable, so a task may not print a value
-- read where the annotations rule the read out; and a value method read
-- after a read of another that its annotations forbid sees an arbitrary
-- value only when the earlier read mattered.  Both ask when the value of a
-- read reaches a value the step produces: here, as a condition of the
-- sequential semantics.
--
-- A read reaches a value along the selections between them, as 'eval'
-- makes them: the arm of an if, a case, a mux or an array selection that
-- the read is in counts only when its condition selects it (for the arms of
-- a priority mux or a case, a condition implied by its being selected; for
-- an array, its index being the element's).  Through any other operator,
-- and into a selection's conditions, the read reaches the result whenever
-- it reaches the operand.  The condition is the OR, over the paths from the
-- value down to the read, of the AND of the selections on each path.
--
-- That holds of several reads at once: if none of a set of reads reaches
-- an expression, the expression has the same value whatever all of them
-- return.  (By induction on the expression: none reaches a selection's
-- condition, which is then the same, and picks the same arm, which none
-- reaches.)  So a condition over several reads may be read off each read's.
-- The conditions are exact about which arm is selected, not about
-- operators that ignore an operand (@x && false@ still counts @x@).
--
-- The paths go through definitions, which many expressions share.  Worked
-- out per definition and per read, the conditions of a chain of n
-- selections over n reads would take n^2/2 definitions; so they are worked
-- out from the top instead.  For a step, each definition on the way gets
-- the condition under which its value reaches what the step produces, and
-- a read's condition is the OR, over the definitions that read it, of
-- theirs AND'ed with how the read reaches them.  A definition that only
-- one other refers to, and no step directly, gets to the step only through
-- that one, so its condition is that one's AND'ed with how it reaches that
-- one.  Along a chain of such definitions, how each reaches the top of the
-- chain (its /head/: a definition several refer to, or a step refers to
-- directly) is a definition of its own, @<def>\@up@, the same for every
-- step; so are the conditions under which the reads of the chain, and the
-- heads it refers to, reach the head (@<head>\@reaches\@<copy>@,
-- @<head>\@reaches\@<head>@: a copy is named @inst.meth@, a definition has
-- no dot).  Only a head's condition is the step's own:
-- @<head>\@prints\@<step>@ for what the step's system tasks print,
-- @<head>\@does\@<step>@ for all the step does.  So each definition is
-- walked once, and a step adds a condition for each head on its way to the
-- reads it asks about: on a chain, one.
--
-- The selections inside a definition are evaluated under a context of
-- their own (@<def>\@reaches@): an unspecified value in one is a variable
-- of its own, which only adds a free variable to the theorem, rather than
-- taking the number of one of the definition's.

-- | What an expression's value comes from, for 'reachPass': a definition it
-- refers to, or a copy it reads.
data RChild = RDef String | RRead Port
    deriving (Eq, Ord)

-- | The copies of interest (those of every step's question), and the
-- copies each definition reads ('exprReads', its table shared).
data ReachCtx = ReachCtx { rcTargets :: S.Set Port, rcDefReads :: String -> S.Set Port }

-- | The reads of copies of @targets@ an expression makes, and the
-- definitions it refers to that read one, each with the conditions under
-- which it reaches the expression's value (each AND'ed with @c0@), one per
-- occurrence.  It stops at a definition, and walks into the expression
-- otherwise, following the selections as above.
reachEdges :: ReachCtx -> S.Set Port -> Term -> AExpr -> Gen (M.Map RChild [Term])
reachEdges rc targets c0 e0 = go c0 e0 M.empty
  where
    add k c = M.insertWith (++) k [c]
    goAll c xs acc = foldM (\a x -> go c x a) acc xs
    go c e acc
        | c == TLit False = return acc
        | otherwise = case e of
            AMethCall _ o m args -> readOf c o m e args acc
            ATupleSel _ inner@(AMethCall _ o m args) _ -> readOf c o m inner args acc
            ATupleSel _ (ATuple _ es) k | k >= 1, (x : _) <- drop (fromInteger k - 1) es -> go c x acc
            ATupleSel _ inner _ -> go c inner acc
            ATuple _ xs -> goAll c xs acc
            ASAny _ (Just v) -> go c v acc
            ASDef _ i
                -- a WILL_FIRE signal is read at the hardware's view, not by the step
                | "WILL_FIRE_" `isPrefixOf` n -> return acc
                | S.disjoint (rcDefReads rc n) targets -> return acc
                | otherwise -> return (add (RDef n) c acc)
              where n = getIdString i
            APrim _ t op args | not (datapathPrim t op args) -> prim c op args acc
            APrim _ _ _ args -> goAll c args acc
            AFunCall { ae_args = xs } -> goAll c xs acc
            ANoInlineFunCall { ae_args = xs } -> goAll c xs acc
            _ -> return acc
    readOf c o m use args acc = do
        d <- gets acDesign
        let inst = getIdString o
            port = lookupMeth d inst (getIdBaseString m) >>= \mt -> readPortFor d mt inst use
            acc' = case port of
                Just p | S.member p targets -> add (RRead p) c acc
                _ -> acc
        goAll c args acc'
    prim c op args acc = case (op, args) of
        (PrimIf, [cd, a, b]) -> do
            acc1 <- go c cd acc
            ct <- eval Seq cd
            acc2 <- go (tAnd [c, ct]) a acc1
            go (tAnd [c, tNot ct]) b acc2
        (PrimMux, _) -> mux
        (PrimPriMux, _) -> mux
        (PrimCase, idx : dflt : arms) -> do
            acc1 <- go c idx acc
            it <- eval Seq idx
            -- (each arm when the index is its label: the first such is
            -- selected, so this is implied by its being selected)
            (acc2, labels) <- foldM (\(a, ls) (l, v) -> do
                                         a1 <- go c l a
                                         lt <- eval Seq l
                                         a2 <- go (tAnd [c, tEq it lt]) v a1
                                         return (a2, tEq it lt : ls))
                                    (acc1, []) (pairUp arms)
            go (tAnd (c : map tNot (reverse labels))) dflt acc2
        (PrimArrayDynSelect, [arr, idx]) -> do
            d <- gets acDesign
            let es = arrayElemsSeq d arr
                w = case sortOf (aType idx) of { SBool -> 1; SBits b -> b; _ -> 0 }
                len = length es
                -- (as 'eval': the elements an index of its width can select)
                n = min len (if w >= 20 then len else 2 ^ w)
            acc1 <- go c idx acc
            if null es
                -- an array 'eval' does not see into: every element counts
                then go c arr acc1
                else do
                    it <- eval Seq idx
                    ks <- mapM (\k -> constant (aType idx) (toInteger k)) [0 .. n - 1]
                    -- Element k counts when the index is k.  When the elements
                    -- cover every index, 'eval' also takes the last for an index
                    -- that is none of them, which only a token can be: a real
                    -- index is one of the constants, all of them terms of the
                    -- file, so the theorem about tokens covers every real one.
                    foldM (\a (kk, x) -> go (tAnd [c, tEq it kk]) x a) acc1 (zip ks (take n es))
        _ -> goAll c args acc
      where
        -- (selector, value) pairs, the first true selector winning, and the
        -- default arm 'eval' recognises last
        mux = do
            let ps = pairUp args
                (arms, dflt) = case reverse ps of
                    ((APrim _ _ PrimBNot [_], v) : rest) -> (reverse rest, Just v)
                    _                                    -> (ps, Nothing)
            acc1 <- goAll c (map fst arms) acc
            sels <- mapM (eval Seq . fst) arms
            acc2 <- foldM (\a (s, v) -> go (tAnd [c, s]) v a) acc1 (zip sels (map snd arms))
            maybe (return acc2) (\v -> go (tAnd (c : map tNot sels)) v acc2) dflt

-- | The OR of the conditions under which something reaches a value, but
-- for one that is another AND'ed with more (@x || c && x@ is @x@): a read
-- that reaches a selection's condition and one of its arms reaches it
-- whenever it reaches the condition.
orAll :: [Term] -> Term
orAll ts = tOr [ t | t <- ts, not (absorbed t) ]
  where
    present = S.fromList (map HKey ts)
    -- (an operand of a conjunction looked up in the others, or the others
    -- in its operands, whichever are fewer)
    absorbed t = case t of
        TAnd _ ops
            | S.size present <= M.size (tsOps ops) -> any (\(HKey x) -> M.member (HKey x) (tsIndex ops)) (S.toList present)
            | otherwise -> any (\o -> S.member (HKey o) present) (tsList ops)
        _ -> False

-- | The definitions on the way from the steps' values to the copies of
-- interest, each with what it refers to ('reachEdges', the conditions
-- relative to its own value), and how they hang together.
data RGraph = RGraph
    { rgEdges  :: M.Map String (M.Map RChild Term)
    , rgParent :: M.Map String String
      -- ^ a definition one other refers to and no step does directly: that one
    , rgOrder  :: M.Map String Int
      -- ^ each definition's place: after every definition that refers to it
    , rgRegion :: ML.Map String (M.Map Port [(String, Term, Bool)], M.Map String [(String, Term, Bool)])
      -- ^ of each head, the reads of its chain and the heads its chain
      -- refers to, each with the definition of the chain that does, how,
      -- and whether that definition reaches the head unconditionally
    }

-- | The definitions on the way from the given ones (those the steps refer
-- to) to the copies of interest.
reachGraph :: ReachCtx -> [String] -> Gen RGraph
reachGraph rc roots = do
    edges <- explore roots M.empty
    let rootSet = S.fromList roots
        parents = M.fromListWith S.union [ (c, S.singleton n) | (n, es) <- M.toList edges, RDef c <- M.keys es ]
        parentOf = M.fromList [ (c, S.findMin ps) | (c, ps) <- M.toList parents
                                                  , S.size ps == 1, S.notMember c rootSet ]
        kids n = [ c | RDef c <- maybe [] M.keys (M.lookup n edges) ]
        -- (depth first: each definition once it has been put after all it
        -- refers to, so the list has every definition before them)
        visit (seen, out) n
            | S.member n seen = (seen, out)
            | otherwise = let (seen', out') = F.foldl' visit (S.insert n seen, out) (kids n)
                          in (seen', n : out')
        order = M.fromList (zip (snd (F.foldl' visit (S.empty, []) roots)) [0 :: Int ..])
        -- a head's chain: through the definitions only one of the chain
        -- refers to, each of which therefore refers to the chain once
        region h = walk True (M.empty, M.empty) h
        walk open acc n = F.foldl' (step open n) acc (M.toList (M.findWithDefault M.empty n edges))
        step open n (occ, hs) (ch, l) = case ch of
            RRead p -> (M.insertWith (++) p [(n, l, open)] occ, hs)
            RDef c | M.member c parentOf -> walk (open && l == TLit True) (occ, hs) c
                   | otherwise -> (occ, M.insertWith (++) c [(n, l, open)] hs)
    return RGraph { rgEdges = edges, rgParent = parentOf, rgOrder = order
                  , rgRegion = ML.fromList [ (h, region h) | h <- M.keys edges, M.notMember h parentOf ] }
  where
    explore [] acc = return acc
    explore (n : ns) acc
        | M.member n acc = explore ns acc
        | otherwise = do
            d <- gets acDesign
            es <- case M.lookup n (dSeqDefs d) of
                Just df -> do
                    outer <- gets acCtx
                    modify' (\a -> a { acCtx = Just (n ++ "@reaches") })
                    m <- reachEdges rc (rcTargets rc) (TLit True) (adef_expr df)
                    modify' (\a -> a { acCtx = outer })
                    return (M.map orAll m)
                Nothing -> return M.empty
            explore ([ c | RDef c <- M.keys es ] ++ ns) (M.insert n es acc)

-- | A condition of 'reachPass' as a definition of its own, made once by its
-- name (an atom is itself).
shareReach :: String -> Gen Term -> Gen Term
shareReach name mk = do
    memo <- gets acReach
    case M.lookup name memo of
        Just r -> return r
        Nothing -> do
            t <- mk
            r <- if atomic t then return t else do
                     modify' (\a -> a { acSeqDefs = LDef name SBool t (dynamic t) : acSeqDefs a })
                     return (TRef name (dynamic t))
            modify' (\a -> a { acReach = M.insert name r (acReach a) })
            return r

-- | When a definition's value reaches its chain's head's (@<def>\@up@;
-- @true@ for the head).
upRef :: RGraph -> String -> Gen Term
upRef g m = case M.lookup m (rgParent g) of
    Nothing -> return (TLit True)
    Just p -> shareReach (m ++ "@up") $ do
        up <- upRef g p
        return (tAnd [up, M.findWithDefault (TLit False) (RDef m) (M.findWithDefault M.empty p (rgEdges g))])

-- | When a read of a copy (@Left@), or a head's value (@Right@), reaches a
-- head's value through its chain (@<head>\@reaches\@<copy or head>@).
chainRef :: RGraph -> String -> Either Port String -> Gen Term
chainRef g h x = shareReach (h ++ "@reaches@" ++ either portLabel id x) $ do
    let (occ, hs) = ML.findWithDefault (M.empty, M.empty) h (rgRegion g)
        uses = either (\p -> M.findWithDefault [] p occ) (\k -> M.findWithDefault [] k hs) x
    -- (a use that reaches the head unconditionally is all of it: no
    -- definitions are made for the others, which it absorbs)
    if any (\(_, l, open) -> open && l == TLit True) uses then return (TLit True)
        else orAll <$> mapM (\(n, l, _) -> (\up -> tAnd [up, l]) <$> upRef g n) uses

-- | For one step, from what its values refer to (each with the condition
-- under which it counts, 'reachEdges'): the condition under which the read
-- of each copy of @targets@ reaches one of them.  The heads are taken in
-- the graph's order, so each once every head that refers to it is done.
-- A head's condition is a definition, @<head>\@<label>@, when more than
-- one term uses it; one term only, it is written there, so that a run of
-- heads each of which leads to one other is still written once.
reachPass :: ReachCtx -> RGraph -> String -> S.Set Port -> M.Map RChild [Term] -> Gen (M.Map Port Term)
reachPass rc g label targets top = do
    found <- heads (S.fromList (map keyOf (M.keys start))) start []
    return (M.map orAll (M.fromListWith (++) (direct ++ found)))
  where
    relevant n = not (S.disjoint (rcDefReads rc n) targets)
    direct = [ (p, ts) | (RRead p, ts) <- M.toList top, S.member p targets ]
    start = M.fromList [ (n, ts) | (RDef n, ts) <- M.toList top, relevant n ]
    keyOf n = (M.findWithDefault (-1) n (rgOrder g), n)
    -- the copies of interest a head's chain reads, by the fewer to walk
    readsOf occ
        | S.size targets < M.size occ = [ p | p <- S.toList targets, M.member p occ ]
        | otherwise = [ p | p <- M.keys occ, S.member p targets ]
    heads queue incoming found = case S.minView queue of
        Nothing -> return found
        Just ((_, h), queue') -> do
            let c = orAll (M.findWithDefault [] h incoming)
                (occ, hs) = ML.findWithDefault (M.empty, M.empty) h (rgRegion g)
                ps = readsOf occ
                ks = [ k | k <- M.keys hs, relevant k ]
            if c == TLit False then heads queue' incoming found else do
                -- (a definition only when more than one term uses it)
                here <- if length ps + length ks <= 1 then return c
                        else shareReach (h ++ "@" ++ label) (return c)
                rs <- forM ps $ \p -> (\t -> (p, [tAnd [here, t]])) <$> chainRef g h (Left p)
                kcs <- forM ks $ \k -> (\t -> (k, tAnd [here, t])) <$> chainRef g h (Right k)
                heads (F.foldl' (\q (k, _) -> S.insert (keyOf k) q) queue' kcs)
                      (F.foldl' (\m (k, t) -> M.insertWith (++) k [t] m) incoming kcs)
                      (rs ++ found)

-- | The elements of an array, through the sequential definitions, as
-- 'eval' finds them (none when it cannot).
arrayElemsSeq :: Design -> AExpr -> [AExpr]
arrayElemsSeq d arr = case arr of
    APrim _ _ PrimBuildArray es -> es
    ASDef _ i -> maybe [] (arrayElemsSeq d . adef_expr) (M.lookup (getIdString i) (dSeqDefs d))
    _ -> []

-- ----------------------------------------------------------------------------
-- ** The proof object
-- ----------------------------------------------------------------------------

-- | A step, evaluated.
data StepT = StepT
    { sxStep    :: Step
    , sxFire    :: Term                      -- ^ hardware WILL_FIRE
    , sxGuard   :: Maybe Term                -- ^ the guard, as the step sees it
    , sxCalls   :: [(Port, Term, [Term])]
    , sxOutputs :: [(Output, Term)]
    , sxTasks   :: [(Term, Site)]
      -- ^ its system tasks' sites, each with when its value reaches what the
      -- task prints or whether it prints ('reachPass')
    , sxFlags   :: [(Port, Term)]
      -- ^ the copies in 'dReadFlags' it reads, each with when the value read
      -- reaches what the step does: then a later read may not follow it
    , sxTaskCalls :: [(Int, Term, [Term])]
      -- ^ its system-task calls in program order: index, condition, arguments
    , sxFields  :: [String]                  -- ^ fields of the step's view
    , sxFieldSet :: S.Set String             -- ^ the same, as a set
    , sxIllegal :: [(String, Term)]          -- ^ why the step could be illegal
    }

-- | Everything the Lean file states and proves.
data Proof = Proof
    { pfSteps     :: [StepT]
    , pfVars      :: [Var]                   -- ^ in creation order
    , pfFields    :: [Field]
    , pfSeqDefs   :: [LDef]                  -- ^ dependency order
    , pfHwDefs    :: [LDef]
    , pfPorts     :: [Port]                  -- ^ child action ports in use
    , pfFlagPorts :: [Port]                  -- ^ value ports with a read flag ('dReadFlags')
    , pfHwPorts   :: M.Map String Term       -- ^ hardware value of each port signal
    , pfOutputs   :: [(String, Sort, Term, [Mask])]
    , pfEnv       :: [(String, String, Term)]  -- ^ name, comment, statement
    , pfPrims     :: [(String, String, Term)]
    , pfOracles   :: [(String, String, Term)]
    , pfUser      :: [(String, String, Term)]
    , pfCongs     :: [Cong]                  -- ^ the context-free congruences, then the observers'
    , pfStateFields :: [(String, Sort, String)]  -- ^ the State's fields, with comments
    , pfSeq       :: SeqOut                  -- ^ the sequential execution, field by field
    , pfMuxes     :: [(String, [Term])]
    , pfConsts    :: M.Map Sort [Integer]
    , pfStrings   :: [String]
    , pfWidths    :: M.Map Sort Integer
    , pfCounts    :: M.Map Sort Int
    , pfTaskPorts :: [TaskPort]              -- ^ the system-task calls' fields of the State
    , pfOrderFacts :: [(String, String, String, [(String, String)], [(String, String)])]
      -- ^ an exported @a SB b@ (or @SBR@); the pairs of their steps that do
      -- not commute, each of which must run in that order; and those that do
    , pfWarnings  :: [String]
    , pfErrors    :: [String]
      -- ^ why the file would not prove what it says: then it is not written
    }

-- | A system-task call's fields of the State: its position among the
-- cycle's calls made, from 1 (0 when it is not made), and its arguments.
data TaskPort = TaskPort
    { tpPos  :: String
    , tpArgs :: [(String, Sort)]
    , tpNote :: String               -- ^ the call, for comments
    }

-- | The State field counting the system-task calls made so far.
tasksField :: String
tasksField = "$tasks"

-- | A task's arguments as the theorem compares them: a zero-bit one has no value.
taskArgs :: [AExpr] -> [AExpr]
taskArgs = filter ((/= SUnit) . sortOf . aType)

-- | Run the analysis.
analyse :: Design -> [DisjointFact] -> Proof
analyse d facts = finish (runState build acc0)
  where
    acc0 = Acc { acDesign = d, acVars = M.empty, acVarOrder = [], acAppVars = M.empty, acAppInfo = M.empty
               , acFields = M.empty, acFieldOrd = [], acFieldKey = M.empty
               , acConsts = M.empty, acStrings = S.empty
               , acSeqMemo = M.empty, acHwMemo = M.empty, acSeqDefs = [], acHwDefs = []
               , acHwApps = M.empty, acSeqApps = M.empty, acStatic = [], acMuxes = []
               , acCtx = Nothing, acFresh = 1, acUnspec = M.empty, acNextSuffix = M.empty
               , acReach = M.empty, acWarnings = [] }
    ports = [ p | c <- map (dChildren d M.!) (dChildOrder d), mt <- methodsOf c
                , mtKind mt /= ValueMethod, p <- mtCopies mt ]
    methodsOf c = map (chMethods c M.!) (chOrder c)
    valueCopies = [ p | c <- M.elems (dChildren d), mt <- M.elems (chMethods c)
                      , mtKind mt == ValueMethod, p <- mtCopies mt, not (null (ptArgs p)) ]
    build = do
        -- module inputs first, in port order
        forM_ (dInputOrder d) $ \n -> inputVar GInput n (sortOf (dInputs d M.! n)) "module input"
        -- the hardware: every child input, every module output
        -- (and the argument wires of value methods, which congruences mention)
        hwPorts <- fmap M.fromList $ forM [ (s, t) | p <- ports ++ valueCopies
                                                   , (s, t) <- maybe [] (\e -> [(e, aTBool)]) (ptEnable p) ++ ptArgs p ] $
            \(s, t) -> (,) s <$> hwSignal s t
        hwOuts <- forM (dOutputs d) $ \(o, t) -> (,) o <$> hwSignal o t
        -- the sequential semantics, step by step
        evaluated <- forM (dSteps d) $ \st -> do
            fire0 <- ident Seq aTBool (stFire st)
            fire <- case (stKind st, stMethod st) of
                -- a value method's result is used only when it is ready (an
                -- urgent rule can block it through RDY alone), and never in a
                -- cycle in which the parent enables a method it exports C with
                (ValueMethodStep, Just m) -> do
                    -- the ready signal as the hardware has it (like WILL_FIRE),
                    -- when it is a port: the conditions are exactly those of the
                    -- result's mask, since always_ready leaves the parent to use
                    -- the result whatever the internal RDY says
                    rdy <- case (M.lookup m (dReady d), M.lookup m (dReadyDef d)) of
                        (Just _, Just r) | M.member r (dSeqDefs d) -> atHardware <$> defTerm Seq r
                        _ -> return (TLit True)
                    let partners = [ TIn en | o <- M.findWithDefault [] m (dExportedCBy d)
                                           , Just en <- [M.lookup o (dEnables d)] ]
                    return (tAnd (fire0 : rdy : map tNot partners))
                _ -> return fire0
            g    <- traverse (eval Seq) (stGuard st)
            cls  <- forM (stCalls st) $ \c ->
                        (,,) (clPort c) <$> eval Seq (clCond c) <*> mapM (eval Seq) (clArgs c)
            outs <- forM (stOutputs st) $ \o -> (,) o <$> eval Seq (ouSeq o)
            tcs  <- forM (stTaskCalls st) $ \tc ->
                        (,,) (tcTag tc) <$> eval Seq (tcCond tc) <*> mapM (eval Seq) (taskArgs (tcArgs tc))
            -- what the step's values refer to, for 'reachPass': for the copies
            -- its system tasks read where the annotations rule them out, its
            -- tasks; for the copies with a read flag it reads, all it does:
            -- its guard, its calls, its outputs and its tasks.  An argument
            -- of a call or a task counts only when the call is made or the
            -- task's condition holds.
            let taskPorts = S.fromList [ siPort s | (_, _, ss) <- stTaskSites st, s <- ss ]
                flagged = S.fromList [ p | p <- stReadSites st ++ [ p | (_, _, ps) <- stTaskReads st, p <- ps ]
                                         , S.member p readFlagSet ]
                edges ts = reachEdges reachCtx ts
                -- (the task's condition evaluated only when an argument
                -- reads something of interest)
                taskEdges ts (c, args) = do
                    rc <- edges ts (TLit True) c
                    ras <- M.unionsWith (++) <$> mapM (edges ts (TLit True)) args
                    if M.null ras then return rc else do
                        ct <- eval Seq c
                        return (M.unionWith (++) rc (M.map (map (\t -> tAnd [ct, t])) ras))
            taskTop <- if S.null taskPorts then return M.empty else
                M.unionsWith (++) <$> mapM (taskEdges taskPorts) [ (c, args) | (c, args, ss) <- stTaskSites st, not (null ss) ]
            flagTop <- if S.null flagged then return M.empty else do
                rg <- maybe (return M.empty) (edges flagged (TLit True)) (stGuard st)
                rcs <- forM (zip (stCalls st) cls) $ \(c, (_, ct, _)) -> do
                           r <- edges flagged (TLit True) (clCond c)
                           M.unionsWith (++) . (r :) <$> mapM (edges flagged ct) (clArgs c)
                ros <- mapM (edges flagged (TLit True) . ouSeq) (stOutputs st)
                rts <- mapM (taskEdges flagged) [ (c, args) | (c, args, ps) <- stTaskReads st, any (`S.member` flagged) ps ]
                return (M.unionsWith (++) (rg : rcs ++ ros ++ rts))
            return ( StepT { sxStep = st, sxFire = fire, sxGuard = g, sxCalls = cls
                           , sxOutputs = outs, sxTasks = [], sxFlags = [], sxTaskCalls = tcs
                           , sxFields = [], sxFieldSet = S.empty, sxIllegal = [] }
                   , (taskPorts, taskTop, flagged, flagTop) )
        -- when each read of interest reaches what the step asks about, from
        -- the definitions on the way for all the steps at once
        graph <- reachGraph reachCtx (S.toList (S.fromList [ n | (_, (_, tt, _, ft)) <- evaluated
                                                               , RDef n <- M.keys tt ++ M.keys ft ]))
        steps <- forM (zip [0 :: Int ..] evaluated) $ \(k, (sx, (taskPorts, taskTop, flagged, flagTop))) -> do
            let st = sxStep sx
                label = if M.findWithDefault 0 (stName st) stepNameCount > (1 :: Int)
                            then stName st ++ "#" ++ show k else stName st
            taskReach <- if M.null taskTop then return M.empty
                         else reachPass reachCtx graph ("prints@" ++ label) taskPorts taskTop
            flagReach <- if M.null flagTop then return M.empty
                         else reachPass reachCtx graph ("does@" ++ label) flagged flagTop
            -- (a copy's sites are the same for every task of the step that
            -- reads it, so one condition, over all the step's tasks, serves
            -- each site once)
            let sites = distinctSites [ s | (_, _, ss) <- stTaskSites st, s <- ss ]
            return sx { sxTasks = [ (t, s) | s <- sites, Just t <- [M.lookup (siPort s) taskReach], t /= TLit False ]
                      , sxFlags = [ (p, t) | p <- S.toList flagged, Just t <- [M.lookup p flagReach], t /= TLit False ] }
        -- assumptions about the environment, from the exported schedule
        env <- fmap concat $ sequence
            [ forM [ (en, rdy) | (m, en) <- M.toList (dEnables d), Just rdy <- [M.lookup m (dReady d)] ] $
                  \(en, rdy) -> do
                      r <- hwSignal rdy aTBool
                      return ("ready_" ++ en, en ++ " only when " ++ rdy, tOr [tNot (TIn en), r])
            , return [ ("conflict_" ++ a ++ "_" ++ b, a ++ " C " ++ b ++ ": never enabled together"
                       , tNot (tAnd [ea, eb]))
                     | (a, b) <- nubOrd [ (min x y, max x y) | (x, y) <- dExportedC d ]
                     , Just ea <- [enableOf a], Just eb <- [enableOf b] ] ]
        -- what bsc's wire primitives do within the cycle
        prims <- fmap concat $ forM (map (dChildren d M.!) (dChildOrder d)) $ \c ->
            forM (chWires c) $ \(o, inp) -> do
                let t = maybe aTBool (\(p, k) -> snd (ptResults p !! k)) (M.lookup o (dStateOuts d))
                ov <- hwSignal o t
                iv <- hwSignal inp t
                return ( "wire_" ++ o
                       , chInst c ++ " (" ++ chModule c ++ "): " ++ o ++ " is " ++ inp ++ " within the cycle"
                       , tEq ov iv )
        -- disjointness: recorded by bsc, and re-derived by bsc's solver
        recorded <- fmap catMaybes $ forM (dDisjoint d) $ \(a, b) -> do
            ca <- canFire a
            cb <- canFire b
            return $ case (ca, cb) of
                (Just x, Just y) -> Just ("disjoint_" ++ a ++ "_" ++ b
                                         , "bsc proved the guards of " ++ a ++ " and " ++ b ++ " disjoint"
                                         , tNot (tAnd [x, y]))
                _ -> Nothing
        -- (a chain's members once, the longest chain of each name; a chained
        -- fact is on the OR of the first k, `Hw.<chain>.upto_k`)
        let chainsUsed = M.map fst (M.fromListWith (\new old -> if snd new > snd old then new else old)
                                                   [ (cnName c, (c, n)) | f <- facts, Just (c, n) <- [dfChain f] ])
            chainNo = M.fromList (zip (M.keys chainsUsed) [1 :: Int ..])
            chainKey c k = "uses_" ++ show (chainNo M.! cnName c) ++ "\1upto_" ++ show k
        chainTerms <- forM (M.elems chainsUsed) $ \c -> (,) (cnName c) <$> mapM (evalAtHw . snd) (cnMembers c)
        let chainMap = M.fromList chainTerms
            chainLDefs = concat [ [ LDef (chainKey c k) SBool (tOr [upto c (k - 1), m]) False
                                  | (k, m) <- zip [2 :: Int ..] (drop 1 ms) ]
                                | c <- M.elems chainsUsed, let ms = chainMap M.! cnName c ]
            upto c k = if k == 1 then head' (chainMap M.! cnName c) else THw (chainKey c k)
            head' xs = case xs of { (x : _) -> x; [] -> TLit False }
        derived <- forM (zip [1 :: Int ..] facts) $ \(k, f) -> do
            c1 <- case dfChain f of
                Just (c, n) -> return (upto c n)
                Nothing     -> evalAtHw (dfCond1 f)
            c2 <- evalAtHw (dfCond2 f)
            return ("solver_" ++ show k, "bsc's solver: " ++ dfStep1 f ++ " and " ++ dfStep2 f
                                         ++ " never use " ++ dfWhy f
                   , tNot (tAnd [c1, c2]))
        -- bsc's run-time checks, as bsc makes them: a mutually_exclusive
        -- assertion is checked by a rule of its own (__me_check_N) that the
        -- rules never *fire* together, which is weaker than their guards
        -- never holding together
        user <- fmap concat $ forM (dSteps d) $ \st ->
            forM (zip [1 :: Int ..] (stChecks st)) $ \(k, c) -> do
                fire <- hwSignal (stFire st) aTBool
                ct <- evalAtHw c
                let meCheck = "__me_check" `isPrefixOf` stName st
                return ( if meCheck then "mutually_exclusive_" ++ drop (length "__me_check_") (stName st)
                                         ++ (if k > 1 then "_" ++ show k else "")
                                    else "check_" ++ stName st ++ "_" ++ show k
                       , if meCheck then "user assertion (mutually_exclusive), as bsc checks it: "
                                         ++ "the rules never fire in the same cycle"
                                    else "bsc's run-time check in " ++ stName st ++ " never fires"
                       , tNot (tAnd [fire, ct]) )
        -- the hardware's system-task calls, in its order, and how many of
        -- the calls before each it makes ('Hw.«$tasks@k»')
        hwTasks <- forM (dHwTasks d) $ \h -> (,,) h <$> eval Hw (htCond h) <*> mapM (eval Hw) (taskArgs (htArgs h))
        -- (a call made is at the count after it)
        let posStep (k, pos, acc) (_, en, _) = do
                let next = tAdd pos (tInc en)
                    name = tasksField ++ "@" ++ show (k + 1)
                next' <- if atomic next then return next else do
                    modify' (\a -> a { acHwDefs = LDef name SPos next False : acHwDefs a })
                    return (THw name)
                return (k + 1, next', tCond en next' (TConst SPos 0) : acc)
        (_, hwCount, hwPosRev) <- foldM posStep (0 :: Int, TConst SPos 0, []) hwTasks
        let hwTaskTerms = zipWith (\(h, en, as) pos -> (h, en, as, pos)) hwTasks (reverse hwPosRev)
        -- every observed value now exists; give the applications theirs
        pairApplications
        return (hwPorts, hwOuts, steps, env, prims, recorded ++ derived, user, chainLDefs, (hwTaskTerms, hwCount))
    -- a method's enable: its input, or True for an action method with no
    -- enable port (always_enabled: the parent enables it in every cycle)
    enableOf m = case M.lookup m (dEnables d) of
        Just en -> Just (TIn en)
        Nothing | S.member m actionStepMethods -> Just (TLit True)
        _ -> Nothing
    actionStepMethods = S.fromList [ m | st <- dSteps d, stKind st /= ValueMethodStep, Just m <- [stMethod st] ]
    canFire r = case M.lookup r (dStepMap d) of
        Just st | M.member (stCanFire st) (dHwDefs d) -> Just <$> hwSignal (stCanFire st) aTBool
        _ -> return Nothing
    hwSignal s t = ident Hw t s
    -- A condition on what steps observe, stated of the hardware: it is
    -- printed in the hardware's context, where an observed value is its
    -- hardware counterpart ('ppT'), known only once 'pairApplications' ran.
    evalAtHw = eval Seq

    finish ((hwPorts0, hwOuts, steps0, env, prims, oracles, user, chainLDefs, (hwTaskTerms, hwCount)), acc) =
        let fields   = reverse [ acFields acc M.! n | n <- acFieldOrd acc ]
            fieldMap = M.fromList [ (fdName f, f) | f <- fields ]
            seqDefs  = reverse (acSeqDefs acc)
            hwDefs0  = reverse (acHwDefs acc)
            seqMap   = M.fromList [ (ldName l, l) | l <- seqDefs ]
            -- a hardware definition equal to the shared one at the hardware view
            -- is not repeated: the hardware uses the shared one
            shared   = F.foldl' decide S.empty hwDefs0
            decide sh h = case M.lookup (ldName h) seqMap of
                Just s | ldSort s == ldSort h
                       , rewrite sh (atHw sh (ldBody s)) == rewrite sh (ldBody h) -> S.insert (ldName h) sh
                _ -> sh
            atHw sh = mapTerm $ \case
                TView f -> fdHw <$> M.lookup f fieldMap
                TRef x dyn | x `S.member` sh -> Just (if dyn then TAtHw x else TRef x False)
                           | otherwise -> Just (TIn ("seq " ++ x))
                _ -> Nothing
            rewrite sh = mapTerm $ \case
                THw x | x `S.member` sh -> Just (sharedRef x)
                _ -> Nothing
            sharedRef x = case M.lookup x seqMap of
                Just s | ldDyn s -> TAtHw x
                _ -> TRef x False
            rw = rewrite shared
            hwDefs = [ h { ldBody = rw (ldBody h) } | h <- hwDefs0, not (ldName h `S.member` shared) ]
            -- the fields each step observes, closed under the arguments of
            -- the applications they are
            defFields = ML.fromList [ (ldName l, termFields (ldBody l)) | l <- seqDefs ]  -- knot-tied
            -- (added to a set as they are met, rather than a set made of each subterm)
            termFields t = fieldsInto t S.empty
            fieldsInto t acc = case t of
                _ | not (dynamic t) -> acc   -- (no view in it)
                TView f -> S.insert f acc
                TRef x True -> S.union (ML.findWithDefault S.empty x defFields) acc
                _ -> foldr fieldsInto acc (subTerms t)
            -- the fields each field's arguments mention, computed once
            fieldDeps = ML.map (\f -> case fdKind f of
                                          FRead _ xs -> S.unions (map termFields xs)
                                          FApp _ _ xs _ -> S.unions (map termFields xs)
                                          FAV _ -> S.empty) fieldMap
            -- the least superset closed under them, by a worklist
            closeFields fs0 = go fs0 (S.toList fs0)
              where go seen [] = seen
                    go seen (n : todo) =
                        let new = [ m | m <- S.toList (ML.findWithDefault S.empty n fieldDeps)
                                      , not (S.member m seen) ]
                        in go (foldr S.insert seen new) (new ++ todo)
            -- each sequential call's fields, then those of each hardware call
            -- matching none (or with other argument sorts); the hardware's values
            seqCalls = [ (stName (sxStep sx), tc) | sx <- steps0, tc <- stTaskCalls (sxStep sx) ]
            seqSorts = IM.fromList [ (tcTag tc, map (sortOf . aType) (taskArgs (tcArgs tc))) | (_, tc) <- seqCalls ]
            hwSorts h = map (sortOf . aType) (taskArgs (htArgs h))
            (_, hwAssigned) = mapAccumL assign (dTaskTags d) hwTaskTerms
            assign next (h, en, as, pos) = case htTag h of
                Just k | IM.lookup k seqSorts == Just (hwSorts h) -> (next, (k, False, h, en, as, pos))
                _ -> (next + 1, (next, True, h, en, as, pos))
            taskPort k srts note = TaskPort { tpPos = taskPosField k
                                            , tpArgs = [ (taskArgField k j, srt) | (j, srt) <- zip [1 :: Int ..] srts ]
                                            , tpNote = note }
            taskPorts = [ taskPort (tcTag tc) (seqSorts IM.! tcTag tc)
                                   (tcName tc ++ " in " ++ r ++ tcPos tc ++ (if tcCheck tc then ", a run-time check" else ""))
                        | (r, tc) <- seqCalls ]
                        ++ [ taskPort k (hwSorts h) ("the hardware's " ++ htName h ++ " [" ++ htId h ++ "], which no rule makes")
                           | (k, True, h, _, _, _) <- hwAssigned ]
            portByTag = IM.fromList (zip (map (\(_, tc) -> tcTag tc) seqCalls ++ [ k | (k, True, _, _, _, _) <- hwAssigned ])
                                         taskPorts)
            hwTaskVals = concat [ (tpPos tp, pos) : zip (map fst (tpArgs tp)) as
                                | (k, _, _, _, as, pos) <- hwAssigned, let tp = portByTag IM.! k ]
                         ++ [ (tasksField, hwCount) | not (null taskPorts) ]
            hwPorts = M.union hwPorts0 (M.fromList hwTaskVals)
            usedPorts = allUsedPorts steps0
            -- the action copies of each instance, numbered, and the ones
            -- each called copy may not follow, in that order
            candMap = ML.fromList
                [ (i, useIndex snd (zip [0 :: Int ..]
                                        [ p | mt <- M.elems (chMethods c), mtKind mt /= ValueMethod, p <- mtCopies mt ]))
                | (i, c) <- M.toList (dChildren d) ]
            earlierOf p2 = map snd (mergeOn fst (conflicting d NoPrecede p2 (candMap ML.! ptInst p2)))
            earlierMap = ML.fromList [ (p2, earlierOf p2) | p2 <- usedPorts ]
            earlier p2 = fromMaybe (earlierOf p2) (ML.lookup p2 earlierMap)
            steps = map (stepLegality d earlier . withFields) steps0
            withFields sx =
                let ts = sxFire sx : maybe [] pure (sxGuard sx)
                         ++ concat [ c : as | (_, c, as) <- sxCalls sx ] ++ map snd (sxOutputs sx)
                         ++ map fst (sxTasks sx) ++ map snd (sxFlags sx)
                         ++ concat [ c : as | (_, c, as) <- sxTaskCalls sx ]
                    fs = closeFields (S.unions (map termFields ts))
                in sx { sxFire = rw (sxFire sx), sxFields = S.toList fs, sxFieldSet = fs }
            -- the State's fields: each child's ports, the outputs, the
            -- execution's own record and the read flags
            portsByInst = M.fromListWith (++) [ (ptInst p, [p]) | p <- reverse ports ]
            childFields c =
                concat [ [ (en, SBool, portLabel p ++ " called") | Just en <- [ptEnable p] ]
                         ++ [ (a, sortOf t, "  argument " ++ show k) | (k, (a, t)) <- zip [1 :: Int ..] (ptArgs p) ]
                       | p <- M.findWithDefault [] (chInst c) portsByInst ]
            stateFields =
                concat [ childFields (dChildren d M.! c) | c <- dChildOrder d ]
                ++ [ (o, sortOf t, "output " ++ o) | (o, t) <- dOutputs d ]
                ++ [ ("illegal_access", SBool, "a submodule saw an order its annotations forbid")
                   , ("guard_violated", SBool, "a rule ran with a false guard") ]
                ++ [ (stateField p, SBool, portLabel p ++ " read where its value mattered") | p <- dReadFlags d ]
                ++ [ (tasksField, SPos, "calls made so far") | not (null taskPorts) ]
                ++ concat [ (tpPos tp, SPos, tpNote tp)
                            : [ (a, srt, "  argument " ++ show j) | (j, (a, srt)) <- zip [1 :: Int ..] (tpArgs tp) ]
                          | tp <- taskPorts ]
            seqOut = buildSeq (withOrders d) d steps fields fieldMap (M.map rw hwPorts) rw stateFields
            -- The conditions of 'reachPass' the execution refers to.  One a
            -- step's run simplified away (its read is out of order only after
            -- a use no earlier step makes in the schedule's order) is left out.
            reachNames = S.fromList [ n | TRef n _ <- M.elems (acReach acc) ]
            reachRefs t = [ n | x <- universeT t, n <- refName x, S.member n reachNames ]
            refName x = case x of
                TRef n _ -> [n]; TAt n _ -> [n]; TAtHw n -> [n]; TAtVF n _ _ -> [n]; _ -> []
            seqRoots = [ sdTerm (sdBody x) | x <- soDefs seqOut ]
                       ++ concat [ map snd (sbW sb ++ sbO sb) ++ sbAfter sb | sb <- soSteps seqOut ]
                       ++ concat [ cgLhs c : cgRhs c : [ t | PTrue t <- cgPremises c ] | c <- soCongs seqOut ]
            liveReach = reachClose S.empty (concatMap reachRefs seqRoots)
            reachClose seen ns = case ns of
                [] -> seen
                (n : rest) | S.member n seen -> reachClose seen rest
                           | otherwise -> reachClose (S.insert n seen)
                                                     (maybe [] (reachRefs . ldBody) (M.lookup n seqMap) ++ rest)
            congs = reverse (acStatic acc) ++ appCongs ++ twinCongs
            -- a definition both semantics compute as an application, but of
            -- arguments written differently: equal arguments, equal results
            appCongs = [ Cong { cgName = "app_" ++ n
                              , cgPremises = [ PEq x y | (x, y) <- zip xs ys, x /= y ]
                              , cgLhs = sv, cgRhs = hv
                              , cgNote = n ++ " is the same function of the same arguments" }
                       | (n, (op, xs, sv@(TIn _))) <- M.toList (acSeqApps acc)
                       , Just (op', ys, hv) <- [M.lookup n (acHwApps acc)]
                       , op == op', length xs == length ys, sv /= hv ]
            -- two applications (by either semantics, or one made to stand for a
            -- step's observed value) of the same arguments, up
            -- to whose copy of a definition they name: equal arguments, equal
            -- results (the hardware's argument is often its own definition of
            -- the same name, or the sequential one is outside any definition)
            -- (an argument may itself be the variable for one side's
            -- application of a definition, which a congruence by name ties to
            -- the other side's: it too stands for the definition)
            twinKey = mapTerm $ \case
                TRef x _ -> Just (TIn ("def " ++ x))
                THw x    -> Just (TIn ("def " ++ x))
                TAtHw x  -> Just (TIn ("def " ++ x))
                TIn v | Just x <- M.lookup v appDef -> Just (TIn ("def " ++ x))
                      | Just w <- M.lookup v readWire -> Just (TIn ("wire " ++ w))
                      | S.member v wires -> Just (TIn ("wire " ++ v))
                _        -> Nothing
            appDef = M.fromList [ (v, x) | (x, (_, _, TIn v)) <- M.toList (acSeqApps acc) ++ M.toList (acHwApps acc) ]
            -- a step's read of a context-free value method (a variable that a
            -- read_ congruence ties to the copy's output wire) stands for that
            -- wire, as the hardware reads it
            readWire = M.fromList [ (v, w) | c <- acStatic acc, "read_" `isPrefixOf` cgName c
                                           , TIn v <- [cgLhs c], TIn w <- [cgRhs c] ]
            wires = S.fromList (M.elems readWire)
            twins = M.elems (M.fromListWith (++) [ ((op, srt, map twinKey xs), [(n, side, xs)])
                                                 | (n, (side, op, srt, xs)) <- M.toList (acAppInfo acc) ])
            byName = S.fromList [ (cgLhs c, cgRhs c) | c <- appCongs ]
            twinCongs = [ Cong { cgName = "app_" ++ n1 ++ "_" ++ n2
                               , cgPremises = [ PEq x y | (x, y) <- zip xs ys, x /= y ]
                               , cgLhs = TIn n1, cgRhs = TIn n2
                               , cgNote = n1 ++ " and " ++ n2 ++ " apply one operator to the same arguments" }
                        | grp <- twins, (n1, _, xs) <- grp, (n2, _, ys) <- grp, n1 < n2
                        , not (S.member (TIn n1, TIn n2) byName), not (S.member (TIn n2, TIn n1) byName) ]
            vars0 = reverse [ acVars acc M.! n | n <- acVarOrder acc ] ++ soVars seqOut
            counts = M.fromListWith (+) ([ (vaSort v, 1) | v <- vars0, isToken (vaSort v) ]
                                         ++ [ (s, S.size cs) | (s, cs) <- M.toList (acConsts acc) ]
                                         ++ [ (SString, S.size (acStrings acc)) | not (S.null (acStrings acc)) ])
            widths = M.map (\n -> max 1 (ceilLog2 (toInteger n + 1))) counts
                     `M.union` M.fromList [ (SPos, max 1 (ceilLog2 (toInteger (length taskPorts) + 1))) | not (null taskPorts) ]
            muxes = [ (n, map rw ss) | (n, ss) <- reverse (acMuxes acc) ]
            (envFacts, envChains) = disjointChains "conflicts" [ (n, c, rw t) | (n, c, t) <- env ]
            (userTerms, tailDefs) = orTails [ rw t | (_, _, t) <- user ]
            (oracleFacts, oracleChains) = disjointChains "disjoint" [ (n, c, rw t) | (n, c, t) <- oracles ]
        in Proof
            { pfSteps = steps
            , pfVars = vars0
            , pfFields = fields
            , pfSeqDefs = [ l | l <- seqDefs, S.notMember (ldName l) reachNames || S.member (ldName l) liveReach ]
            , pfHwDefs = hwDefs ++ oneHotDefs muxes ++ envChains ++ oracleChains ++ [ l { ldBody = rw (ldBody l) } | l <- chainLDefs ]
                         ++ tailDefs
            , pfPorts = ports
            , pfFlagPorts = dReadFlags d
            , pfHwPorts = M.map rw hwPorts
            , pfOutputs = [ (o, sortOf t, rw v, M.findWithDefault [] o masksMap)
                          | ((o, t), (_, v)) <- zip (dOutputs d) hwOuts ]
            , pfEnv = envFacts
            , pfPrims = [ (n, c, rw t) | (n, c, t) <- prims ]
            , pfOracles = oracleFacts
            , pfUser = zipWith (\(n, c, _) t -> (n, c, t)) user userTerms
            , pfCongs = [ c { cgRhs = rw (cgRhs c), cgPremises = map (rwPremise rw) (cgPremises c) } | c <- congs ]
                        ++ soCongs seqOut
            , pfStateFields = stateFields
            , pfSeq = seqOut
            , pfMuxes = muxes
            , pfConsts = M.map S.toAscList (acConsts acc)
            , pfStrings = S.toList (acStrings acc)
            , pfWidths = widths
            , pfCounts = counts
            , pfTaskPorts = taskPorts
            , pfOrderFacts = [ (a, r, b, pairs, skipped) | (a, r, b) <- dExportedSB d
                                 , let all' = [ (x, y) | x <- methodSteps d a, y <- methodSteps d b ]
                                 , let (pairs, skipped) = partition (\(x, y) -> not (commute d instUses x y)) all'
                                 , not (null all') ]
            , pfWarnings = reverse (acWarnings acc)
              -- a site left out because a step fires only when its CAN_FIRE
              -- holds relies on the disjointness fact's CAN_FIRE being the
              -- one the step's firing implies: the same Lean definition
            , pfErrors = [ "the hardware's " ++ n ++ " is not the sequential one, which reads left out of "
                           ++ "the arbitrary ones rely on (an internal error)"
                         | n <- dRelied d, not (S.member n shared) ]
            }
    -- each step's uses, by instance, for 'commute' (the first step of a name)
    instUses n = ML.lookup n instUsesMap
    instUsesMap = ML.map stepInstUses (dStepMap d)
    -- the masks of each output, over the steps in order
    masksMap = M.map (concat . reverse)
                     (M.fromListWith (++) [ (ouName out, [ouMask out]) | st <- dSteps d, out <- stOutputs st ])
    allUsedPorts sxs = S.toList (S.fromList [ p | s <- sxs, (p, _, _) <- sxCalls s ])
    readFlagSet = S.fromList (dReadFlags d)
    -- the copies whose reads 'reachPass' follows: those a system task reads
    -- where the annotations rule them out, and those with a read flag
    reachCtx = ReachCtx { rcTargets = S.fromList ([ siPort s | st <- dSteps d, (_, _, ss) <- stTaskSites st, s <- ss ]
                                                  ++ dReadFlags d)
                        , rcDefReads = snd (readTables d) }
    -- steps of the same name, which a step's own conditions tell apart
    stepNameCount = M.fromListWith (+) [ (stName st, 1) | st <- dSteps d ]
    -- (the first of each, by the copy read and the use)
    distinctSites ss = reverse (snd (F.foldl' (\(seen, acc) s -> let k = (siPort s, siUse s, siWithin s)
                                                                in if S.member k seen then (seen, acc)
                                                                   else (S.insert k seen, s : acc))
                                              (S.empty, []) ss))

-- | Are orders other than the schedule's checked?
withOrders :: Design -> Bool
withOrders d = not (null (drop 1 (dOrders d)))

rwPremise :: (Term -> Term) -> Premise -> Premise
rwPremise f (PEq a b) = PEq (f a) (f b)
rwPremise f (PTrue t) = PTrue (f t)
rwPremise _ p = p

isToken :: Sort -> Bool
isToken s = case s of { SBits _ -> True; SString -> True; _ -> False }

ceilLog2 :: Integer -> Integer
ceilLog2 n = until (\k -> 2 ^ k >= n) (+ 1) 0

-- | The legality bookkeeping of one step: which earlier calls would make
-- each of its calls illegal, which pairs of its own calls may not share a
-- rule, and which uses would make a system task's read illegal.  The calls'
-- conditions are exact.  Its other reads are not checked: where the
-- annotations rule one out, it sees an arbitrary value instead ('Site').
-- What a system task prints is observable, and must be what the hardware
-- prints, so a task's reads are not made arbitrary: a read the annotations
-- rule out is illegal when its value reaches the task's condition, or one
-- of its arguments while the condition holds ('reachPass').
--
-- @earlier p2@ gives the action copies of p2's instance that p2 may not
-- follow, in the order of the child's methods.
stepLegality :: Design -> (Port -> [Port]) -> StepT -> StepT
stepLegality d earlier sx = sx { sxIllegal = cross ++ within ++ tasks }
  where
    uses = [ (p, c) | (p, c, _) <- sxCalls sx ]
    cross = [ (portLabel p2 ++ " after " ++ portLabel p1, tAnd [c2, usedRef p1])
            | (p2, c2) <- uses, p1 <- earlier p2 ]
    -- (each use is paired with the later uses it conflicts with, in use
    -- order: going backwards, each use meets an index of the later ones)
    within = fst (foldr meet ([], emptyUseIndex) (zip [0 :: Int ..] uses))
    meet (j, (p1, c1)) (facts, later) =
        ( [ (portLabel p1 ++ " and " ++ portLabel p2 ++ " in one rule", tAnd [c1, c2])
          | (_, (p2, c2)) <- mergeOn fst (conflicting d NoShare p1 later) ]
          ++ facts
        , useIndexCons (fst . snd) (j, (p1, c1)) later )
    usedRef p = TSt (stateField p)
    callCond = M.fromListWith (\new old -> tOr [old, new]) [ (p, c) | (p, c, _) <- sxCalls sx ]
    tasks = [ if siWithin s
                then ("a system task can print " ++ portLabel (siPort s) ++ " read with a call of "
                      ++ portLabel (siUse s) ++ " in one rule", tAnd [tc, M.findWithDefault (TLit False) (siUse s) callCond])
                else ("a system task can print " ++ portLabel (siPort s) ++ " read after " ++ portLabel (siUse s)
                     , tAnd [tc, usedRef (siUse s)])
            | (tc, s) <- sxTasks sx ]

-- | The State field that records whether a port has been used this cycle.
stateField :: Port -> String
stateField p = case ptEnable p of
    Just en -> en
    Nothing -> portLabel p

-- | The selector chains of 'Hw.oneHot': for each parallel mux, whether some
-- selector up to the k-th is set (from the second on; the first is its own
-- selector), and the one-hot condition over all of them.  A selector set
-- once an earlier one is set is exactly a pair of selectors set together,
-- so this says what the pairwise condition says, in a size linear in the
-- selectors.
oneHotDefs :: [(String, [Term])] -> [LDef]
oneHotDefs [] = []
oneHotDefs muxes = chains ++ [LDef oneHotKey SBool body False]
  where
    counts = M.fromListWith (+) [ (n, 1 :: Int) | (n, _) <- muxes ]
    named = reverse (snd (F.foldl' nameOne (M.empty :: M.Map String Int, []) muxes))
    nameOne (seen, acc) (n, ss) =
        let k = M.findWithDefault 0 n seen + 1
            nm = "mux_" ++ n ++ (if counts M.! n > 1 then "#" ++ show k else "")
        in (M.insert n k seen, (nm, ss) : acc)
    seenName nm k = nm ++ "\1seen_" ++ show k
    seenT nm ss k = case ss of
        (s1 : _) | k == 1 -> s1
        _ -> THw (seenName nm k)
    chains = [ LDef (seenName nm k) SBool (tOr [seenT nm ss (k - 1), s]) False
             | (nm, ss) <- named, (k, s) <- zip [2 :: Int ..] (drop 1 (take (length ss - 1) ss)) ]
    body = tAnd [ tNot (tAnd [seenT nm ss (k - 1), s]) | (nm, ss) <- named, (k, s) <- zip [2 :: Int ..] (drop 1 ss) ]

-- | Facts that two conditions never hold together, as fewer facts: where
-- a set of conditions is pairwise disjoint by the facts (a clique, found
-- greedily), each member is stated disjoint from the OR of those before it
-- (a chain of definitions, @Hw.<tag>_n.upto_k@).  The chain's facts say
-- exactly what the pairs' facts say, and grow with the members rather than
-- with the pairs.  Other facts, and the pairs no chain of four or more
-- covers (a shorter chain would not be smaller), are kept as they are.
disjointChains :: String -> [(String, String, Term)] -> ([(String, String, Term)], [LDef])
disjointChains tag facts = (others ++ kept ++ concat chainFacts, concat chainDefs)
  where
    pairOf t = case t of
        TNot _ (TAnd _ ts) | [a, b] <- tsList ts -> Just (a, b)
        _ -> Nothing
    parsed = [ (f, pairOf t) | f@(_, _, t) <- facts ]
    others = [ f | (f, Nothing) <- parsed ]
    pairs = [ (f, a, b) | (f, Just (a, b)) <- parsed ]
    -- the conditions, numbered in the order met, and who is disjoint from whom
    vertexOf = F.foldl' (\m t -> if M.member t m then m else M.insert t (M.size m) m) M.empty
                        (concat [ [a, b] | (_, a, b) <- pairs ])
    vertexTerm = IM.fromList [ (k, t) | (t, k) <- M.toList vertexOf ]
    edges = [ (f, vertexOf M.! a, vertexOf M.! b) | (f, a, b) <- pairs ]
    adj = IM.fromListWith IS.union (concat [ [(u, IS.singleton v), (v, IS.singleton u)] | (_, u, v) <- edges ])
    neighbours u = IM.findWithDefault IS.empty u adj
    edgeKey u v = (min u v, max u v)
    -- each condition in no clique yet starts one, grown by the lowest-numbered
    -- condition (in none) disjoint from all its members; a condition joins
    -- one clique at most, so each is grown from, or intersected with, once
    cliques = reverse (fst (F.foldl' grow ([], IS.empty) (IM.keys vertexTerm)))
    grow (cs, used) u
        | IS.member u used = (cs, used)
        | otherwise =
            let extend members cand = case IS.minView cand of
                    Nothing -> members
                    Just (w, rest) -> extend (w : members) (IS.intersection rest (neighbours w))
                c = IS.toAscList (IS.fromList (extend [u] (IS.difference (neighbours u) used)))
            in if length c < 4 then (cs, used) else (c : cs, IS.union used (IS.fromList c))
    coveredAll = S.fromList [ edgeKey x y | c <- cliques, (k, x) <- zip [1 :: Int ..] c, y <- drop k c ]
    kept = [ f | (f, u, v) <- edges, not (S.member (edgeKey u v) coveredAll) ]
    (chainFacts, chainDefs) = unzip (zipWith chain [1 :: Int ..] cliques)
    chain n c =
        let base = tag ++ "_" ++ show n
            name k = base ++ "\1upto_" ++ show k
            members = IM.fromList (zip [1 ..] c)
            member k = vertexTerm IM.! (members IM.! k)
            upto k = if k == 1 then member 1 else THw (name k)
            m = length c
        in ( [ ( base ++ "_" ++ show k
               , "group " ++ show n ++ " (" ++ show m ++ " conditions, every two of them disjoint): member "
                 ++ show k ++ " is disjoint from each before it"
               , tNot (tAnd [upto (k - 1), member k]) )
             | k <- [2 .. m] ]
           , [ LDef (name k) SBool (tOr [upto (k - 1), member k]) False | k <- [2 .. m - 1] ] )

-- | Long disjunctions whose tails recur, written with a definition for each
-- tail: bsc checks a @mutually_exclusive@ group by testing each rule against
-- the OR of the later ones, which written out grows with the square of the
-- group.  Each disjunction of three or more disjuncts becomes a definition,
-- its first disjunct OR'ed with the definition of the rest (the last two
-- disjuncts OR'ed), each tail defined once however many disjunctions end
-- with it: @Hw.ors.<k>@.  (A tail is known by its first disjunct and the
-- tail after it, so finding it takes one lookup.)
orTails :: [Term] -> ([Term], [LDef])
orTails ts0 = (ts', reverse (otDefs st))
  where
    (ts', st) = runState (mapM go ts0) (OrTails IM.empty M.empty [])
    go t | atomic t = return t
         | otherwise = do
             known <- gets (IM.lookup (termId t) . otMemo)
             case known of
                 Just r  -> return r
                 Nothing -> do
                     r <- case t of
                         TOr _ s | (_ : _ : _ : _) <- tsList s -> mapM go (tsList s) >>= tailOf
                         TOr _ s       -> tOr <$> mapM go (tsList s)
                         TAnd _ s      -> tAnd <$> mapM go (tsList s)
                         TNot _ a      -> tNot <$> go a
                         TEq _ a b     -> tEq <$> go a <*> go b
                         TCond _ c a b -> tCond <$> go c <*> go a <*> go b
                         TAdd _ a b    -> tAdd <$> go a <*> go b
                         _             -> return t
                     modify' (\o -> o { otMemo = IM.insert (termId t) r (otMemo o) })
                     return r
    -- the definition of a tail of two or more disjuncts, from the last
    tailOf ops = case reverse ops of
        (z : y : rest) -> do
            base <- named (y, Left z) (tOr [y, z])
            F.foldlM (\acc x -> named (x, Right acc) (tOr [x, acc])) base rest
        _ -> return (tOr ops)
    named k body = do
        table <- gets otTable
        case M.lookup k table of
            Just n  -> return n
            Nothing -> do
                let n = THw ("ors\1" ++ show (M.size table + 1))
                modify' (\o -> o { otTable = M.insert k n table
                                 , otDefs = LDef ("ors\1" ++ show (M.size table + 1)) SBool body False : otDefs o })
                return n

data OrTails = OrTails { otMemo :: IM.IntMap Term, otTable :: M.Map (Term, Either Term Term) Term, otDefs :: [LDef] }

-- | The name of 'Hw.oneHot' among the hardware's definitions (no name bsc
-- gives has the marker).
oneHotKey :: String
oneHotKey = "\1oneHot"

-- | A hardware definition's Lean name: @Hw.x@, or for a one-hot chain
-- @Hw.mux_x.seen_k@.
hwName :: String -> String
hwName x = case break (== '\1') x of
    ("", _ : rest) -> "Hw." ++ rest
    (a, _ : rest)  -> "Hw." ++ q a ++ "." ++ q rest
    _              -> "Hw." ++ q x

-- ----------------------------------------------------------------------------
-- ** The sequential execution, field by field
-- ----------------------------------------------------------------------------
--
-- The one-rule-at-a-time execution in the schedule's order is stated as
-- versions of the State's fields (static single assignment): for each step
-- and each field it writes, one definition, of the versions of the fields it
-- reads that the last earlier step left (or State.init's).  Nothing is
-- written for a field a step leaves alone, so the whole execution is as
-- large as what the steps do, and the theorem's final state is a State of
-- the last versions.
--
-- Two kinds of field are updated differently.  A /flag/ (illegal_access,
-- guard_violated, an enable, a read flag) is set once any step sets it: its
-- new value is the old one OR'ed with the step's contribution.  Any other
-- field (an argument, an output) keeps the value the last step that wrote it
-- gave it.
--
-- What a step observes of the values that change within the cycle (its
-- view) is, when only the schedule's order is checked, one variable per
-- value, which a congruence ties to the hardware's value when the step sees
-- the calls the hardware sees.  When other orders are checked too
-- ('dOrders'), a step run in another order may see other calls; so its view
-- is a function of the calls it sees (@view\@<step>@, a free variable of the
-- cycle), applied in the schedule's order to the calls it sees there, with
-- the same congruences.  Each step is then also stated as an effect on the
-- whole State ('StepB'), which section 5 composes in any order, and whose
-- value at the schedule's state is, definitionally, the versions above.

-- | What a step does to one field of the State.
data Effect
    = ESticky [(Term, String)]
      -- ^ a flag: what sets it, each with an explanation (the fields it
      -- reads appear as 'TSt')
    | EArms [(Term, Term)]
      -- ^ the calls that write it, in order, each a condition and a value: a
      -- later call wins, and with none the field keeps its value
    | EUncond Term                   -- ^ a module output, which the step sets
    | ETasks [(Term, Int, [(Int, Term, Term)])]
      -- ^ (of the count of calls made) each system-task call: when it is made,
      -- its position's field, and its arguments' fields, values and unset values

-- | The fields of the k-th system-task call: its position, from 1 (0 when
-- it is not made), and its arguments ('TaskPort').
taskPosField :: Int -> String
taskPosField k = "$task" ++ show k

taskArgField :: Int -> Int -> String
taskArgField k j = "$task" ++ show k ++ "_" ++ show j

-- | A step's system-task calls as ordinary effects, counts written out; a
-- call's own fields are set (unset when it is not made), never read.
expandTasks :: (Int, Effect) -> [(Int, Effect)]
expandTasks (g, e) = case e of
    ETasks calls ->
        let step cnt (en, posF, args) =
                let next = tAdd cnt (tInc en)
                in ( next
                   , (posF, EUncond (tCond en next (TConst SPos 0)))
                     : [ (a, EUncond (tCond en x z)) | (a, x, z) <- args ] )
            (final, effs) = mapAccumL step (TSt tasksField) calls
        in concat effs ++ [(g, EUncond final)]
    _ -> [(g, e)]

-- | Which calls of a child an observed value sees: those before the step,
-- also the step's own, or every call of the cycle.
data PKind = PBefore | PAfter | PCycle deriving Eq

-- | An observed value of a step, and its congruence.
data ObsSpec = ObsSpec
    { osField :: Field
    , osPrem  :: [(PKind, Port, [Int])]
      -- ^ the ports whose calls it must see as the hardware does (with the
      -- ids of each port's fields: its enable, then its arguments)
    , osEqs   :: [(Term, Term)]   -- ^ its arguments, as the step and the hardware have them
    , osNoCall :: [Either Int Term]
      -- ^ for a read the annotations rule out ('Site'), the uses that make
      -- it arbitrary when the step fires: the field (by id) recording a use
      -- by an earlier step, or the step's own condition for a call of its own
    , osHw    :: Term
    , osNote  :: String }

-- | A step, as the field-by-field execution needs it.
data StepS = StepS
    { ssName    :: String
    , ssDescr   :: String
    , ssFire    :: Term                        -- ^ its WILL_FIRE, or the definition of it
    , ssFireDef :: Maybe Term                  -- ^ its WILL_FIRE, when that needs a definition
    , ssEffects :: [(Int, Effect)]             -- ^ by field, in order
    , ssObs     :: [ObsSpec]                   -- ^ its view's values, arguments first
    , ssFields  :: S.Set String }              -- ^ the same, by name

-- | A step as an effect on the whole State, for the other orders: the fields
-- it reads, the fields it sets to a value computed from them, and the flags
-- it sets when a condition computed from them holds, by the State's field
-- numbers.  The values and conditions mention the k-th field read as 'TRd'
-- k, and the step's view (its view function applied to some of the fields
-- read) as 'TView' and dynamic 'TRef'.
data StepB = StepB
    { sbName  :: String
    , sbDescr :: String
    , sbR     :: [Int]
    , sbW     :: [(Int, Term)]
    , sbO     :: [(Int, Term)]
    , sbView  :: Maybe (String, [(Int, Bool)])
      -- ^ its view function, and the reads (indices into 'sbR') it is applied
      -- to, each AND'ed with the step's WILL_FIRE when marked ('sbFire')
    , sbFire  :: Term
    , sbAfter :: [Term]
      -- ^ the versions of the fields of 'sbW', then of 'sbO', once it ran
    }

-- | The sequential execution as the Lean file states it.
data SeqOut = SeqOut
    { soDefs   :: [SDef]                 -- ^ in the order printed
    , soViews  :: [ViewT]
    , soVars   :: [Var]                  -- ^ the observed values' variables
    , soCongs  :: [Cong]                 -- ^ and their congruences
    , soFinals :: M.Map String Term      -- ^ each field at the end of the cycle
    , soSteps  :: [StepB]                -- ^ each step's effect (with other orders only)
    , soGroups :: [(String, String, [SDef], Maybe ViewT, [SDef])]
      -- ^ each step: its name, description, WILL_FIRE definition (which its
      -- view may be applied to), view and other definitions, as printed
    }

-- | A definition of the execution: a field once a step ran, or the step's
-- WILL_FIRE.
data SDef = SDef { sdName :: [String], sdSort :: Sort, sdBody :: SBody }

-- | A flag's previous value OR'ed with what sets it (each with a comment);
-- the previous value OR'ed with one condition (in parentheses: the effect of
-- 'StepB' computes it so); or a term.
data SBody = BOr Term [(Term, String)] | BOrC Term Term | BT Term

sdTerm :: SBody -> Term
sdTerm b = case b of
    BOr p ds -> tOr (p : map fst ds)
    BOrC p c -> tOr [p, c]
    BT t     -> t

-- | A step's view: its Lean name, its step, and either the variable of each
-- observed value or its view function and the versions it is applied to.
data ViewT = ViewT { vwName :: [String], vwStep :: String, vwVars :: [(String, String)]
                   , vwFun :: Maybe (String, [Term]) }

-- | The name of a step's WILL_FIRE definition, kept apart from the fields'.
fireNameFor :: S.Set String -> String
fireNameFor taken = until (`S.notMember` taken) (++ "'") "fire"

-- | @a && b@, as written: not simplified, so that it is the same term as the
-- one a definition that computes it unfolds to.
rawAnd :: Term -> Term -> Term
rawAnd a b = mkAnd (tsAt 1 b (tsAt 0 a tsEmpty))

-- | Replace atoms of a term without simplifying anything: the result has the
-- term's shape, so it is the term with the atoms replaced also as Lean sees
-- it, up to unfolding definitions.  (The smart constructors could simplify a
-- term once an atom became a literal, which Lean would not do by unfolding.)
-- Each distinct compound subterm is rebuilt once.
substRaw :: (Term -> Maybe Term) -> Term -> Term
substRaw f t0 = evalState (go t0) IM.empty
  where
    go t | atomic t  = return (fromMaybe t (f t))
         | otherwise = do
             known <- gets (IM.lookup (termId t))
             case known of
                 Just r  -> return r
                 Nothing -> do
                     r <- case t of
                         TNot _ a      -> mkNot <$> go a
                         TAnd _ ts     -> mkAnd <$> set ts
                         TOr _ ts      -> mkOr <$> set ts
                         TEq _ a b     -> mkEq <$> go a <*> go b
                         TCond _ c a b -> mkCond <$> go c <*> go a <*> go b
                         TAdd _ a b    -> mkAdd <$> go a <*> go b
                         _             -> return t
                     modify' (IM.insert (termId t) r)
                     return r
    -- (each operand at its own position: none is dropped as a repeat)
    set ts = F.foldl' (\acc (k, x) -> tsAt k x acc) tsEmpty <$> mapM (\(k, x) -> (,) k <$> go x) (M.toList (tsOps ts))

-- | 'substRaw' that keeps each subterm nothing in which is replaced as it
-- is, so that a term with nothing to replace costs one walk.
substKeep :: (Term -> Maybe Term) -> Term -> Term
substKeep f t0 = fromMaybe t0 (evalState (go t0) IM.empty)
  where
    -- the new term, or Nothing if it is the same
    go t | atomic t  = return (f t)
         | otherwise = do
             known <- gets (IM.lookup (termId t))
             case known of
                 Just r  -> return r
                 Nothing -> do
                     r <- case t of
                         TNot _ a      -> fmap mkNot <$> go a
                         TAnd _ ts     -> fmap mkAnd <$> set ts
                         TOr _ ts      -> fmap mkOr <$> set ts
                         TEq _ a b     -> do
                             ma <- go a
                             mb <- go b
                             return (if isNothing ma && isNothing mb then Nothing
                                     else Just (mkEq (fromMaybe a ma) (fromMaybe b mb)))
                         TCond _ c a b -> do
                             mc <- go c
                             ma <- go a
                             mb <- go b
                             return (if isNothing mc && isNothing ma && isNothing mb then Nothing
                                     else Just (mkCond (fromMaybe c mc) (fromMaybe a ma) (fromMaybe b mb)))
                         TAdd _ a b    -> do
                             ma <- go a
                             mb <- go b
                             return (if isNothing ma && isNothing mb then Nothing
                                     else Just (mkAdd (fromMaybe a ma) (fromMaybe b mb)))
                         _             -> return Nothing
                     modify' (IM.insert (termId t) r)
                     return r
    set ts = do
        let kxs = M.toList (tsOps ts)
        ms <- mapM (go . snd) kxs
        return (if all isNothing ms then Nothing
                else Just (F.foldl' (\acc ((k, x), m) -> tsAt k (fromMaybe x m) acc) tsEmpty (zip kxs ms)))

-- | @a || b || ...@ as written, nothing simplified ('rawAnd').
rawOr :: [Term] -> Term
rawOr ts = case ts of
    []  -> TLit False
    [x] -> x
    _   -> mkOr (F.foldl' (\acc (k, x) -> tsAt k x acc) tsEmpty (zip [0 :: Int ..] ts))

-- | The body of a definition of the execution as Lean reads it.
rawSBody :: SBody -> Term
rawSBody b = case b of
    BOr p ds -> rawOr ([ p | p /= TLit False ] ++ map fst ds)
    BOrC p c -> rawOr [p, c]
    BT t     -> t

-- | A definition the file prints: a shared one (section 2), the hardware's
-- (section 4) or one of the execution's (section 3).
data DKey = DShared String | DHw String | DSeq [String]
    deriving (Eq, Ord)

-- | Build the field-by-field execution of the schedule's order, and with
-- other orders, each step's effect.
buildSeq :: Bool -> Design -> [StepT] -> [Field] -> M.Map String Field -> M.Map String Term
         -> (Term -> Term) -> [(String, Sort, String)] -> SeqOut
buildSeq withOrders d steps fields fieldMap hwPorts rw stateFields =
    SeqOut { soDefs = concatMap roDefs outs, soViews = mapMaybe roView outs, soVars = concatMap roVars outs
           , soCongs = concatMap congsOf outs, soFinals = finals, soSteps = mapMaybe roB outs
           , soGroups = [ (ssName st, ssDescr st, fds, roView o, rest)
                        | o <- outs, let st = roStep o
                        , let (fds, rest) = span (\x -> sdName x == ["Seq", ssName st, fireName]) (roDefs o) ] }
  where
    fieldIds = M.fromList (zip [ f | (f, _, _) <- stateFields ] [0 :: Int ..])
    fieldInfo = IM.fromList [ (k, (f, s)) | ((f, s, _), k) <- zip stateFields [0 ..] ]
    fieldName g = fst (fieldInfo IM.! g)
    fieldSort g = snd (fieldInfo IM.! g)
    fireName = fireNameFor (S.fromList [ f | (f, _, _) <- stateFields ])
    fieldIdx = M.fromList (zip (map fdName fields) [0 :: Int ..])
    zeroOf g = case fieldSort g of { SPos -> TConst SPos 0; s | isToken s -> TUnset s; _ -> TLit False }
    -- the schedule's run: each step with the versions before and after it
    (outs, finalVers) = runAll (IM.fromList [ (g, zeroOf g) | g <- IM.keys fieldInfo ])
                               (S.fromList [ ["Seq", stName (sxStep sx), fireName] | sx <- steps ])
                               (map mkStep steps)
    finals = M.fromList [ (fieldName g, t) | (g, t) <- IM.toList finalVers ]
    runAll cur _ [] = ([], cur)
    runAll cur names (st : rest) =
        let (o, cur', names') = runStep cur names st
            (os, fin) = runAll cur' names' rest
        in (o : os, fin)

    -- the steps, as 'StepS'
    mkStep sx =
        let st = sxStep sx
            name = stName st
            fire = sxFire sx
            (fireT, fireDef) = if atomic fire then (fire, Nothing) else (TDef ["Seq", name, fireName], Just fire)
            fa c = tAnd [fireT, c]
            fid x = M.lookup x fieldIds
            outs' = [ (ouName o, EUncond t) | (o, t) <- sxOutputs sx ]
            guardE = [ ("guard_violated", ESticky [(fa (tNot g), "")]) | Just g <- [sxGuard sx], g /= TLit True ]
            illE = case sxIllegal sx of
                [] -> []
                rs -> let grouped = M.toList (M.fromListWith (++) [ (t, [why]) | (why, t) <- reverse rs ])
                      in [ ("illegal_access", ESticky [ (fa t, intercalate "; " ws) | (t, ws) <- grouped ]) ]
            -- the value copies whose reads a later read may not follow: a
            -- step that reads one sets its flag when it fires and the value
            -- read reaches what it does
            flagE = [ (stateField p, ESticky [(fa c, "")]) | (p, c) <- sxFlags sx ]
            callsOf = M.fromListWith (++) [ (p, [(c, as)]) | (p, c, as) <- reverse (sxCalls sx) ]
            taskE = [ ( tasksField
                      , ETasks [ ( fa c, fidOf (taskPosField k)
                                 , [ (g, x, zeroOf g) | (j, x) <- zip [1 :: Int ..] as, let g = fidOf (taskArgField k j) ] )
                               | (k, c, as) <- sxTaskCalls sx ] )
                    | not (null (sxTaskCalls sx)) ]
            fidOf x = fromMaybe (error ("schedlean: no field " ++ x)) (fid x)
            callE = concat
                [ [ (en, ESticky [(fa (tOr (map fst calls)), "")]) | Just en <- [ptEnable p] ]
                  -- (each argument's values over the calls: every call has one per port)
                  ++ [ (a, EArms (zip (map (fa . fst) calls) col))
                     | ((a, _), col) <- zip (ptArgs p) (transpose (map snd calls)) ]
                | p <- nubOrd [ p | (p, _, _) <- sxCalls sx ], let calls = callsOf M.! p ]
            -- (a field written twice keeps the last)
            effs0 = [ (g, e) | (f, e) <- outs' ++ guardE ++ illE ++ flagE ++ callE ++ taskE, Just g <- [fid f] ]
            lastOf = IM.fromList [ (g, k) | (k, (g, _)) <- zip [0 :: Int ..] effs0 ]
            effs = [ (g, e) | (k, (g, e)) <- zip [0 ..] effs0, lastOf IM.! g == k ]
            -- the observed values, arguments first
            flds = sortOn (\f -> M.findWithDefault 0 f fieldIdx) (sxFields sx)
            sitesOf = M.fromListWith (++) [ (siPort s, [s]) | s <- reverse (stSites st) ]
            callConds = M.fromListWith (\new old -> tOr [old, new]) [ (c, k) | (c, k, _) <- sxCalls sx ]
            specs = [ obsSpec sx sitesOf callConds f | fn <- flds, Just f <- [M.lookup fn fieldMap] ]
        in StepS { ssName = name, ssDescr = stDescr st, ssFire = fireT, ssFireDef = fireDef, ssEffects = effs
                 , ssObs = specs, ssFields = sxFieldSet sx }

    -- an observed value's congruence, given the step's sites by the copy
    -- read and its calls' conditions by copy
    obsSpec sx sitesOf callConds f =
        let name = stName (sxStep sx)
            hwArg (s, _) = M.findWithDefault (TIn s) s hwPorts
            callArgs = M.fromListWith (++) [ (c, [as]) | (c, _, as) <- reverse (sxCalls sx) ]
            portFields c = mapMaybe (`M.lookup` fieldIds) (maybe [] pure (ptEnable c) ++ map fst (ptArgs c))
            -- which calls of a dependency a value sees: those before the step
            -- (a SBR m, or a call it may not follow, which makes it see an
            -- arbitrary value), also the step's own (a SB m), or -- when only a
            -- combinational path relates them and they are CF, so order
            -- cannot matter -- every call of the cycle
            deps p = case lookupMeth d (ptInst p) (ptMeth p) of
                Just mt -> [ (kind, c, portFields c)
                           | let m = ptMeth p, let conf = chConf (childOf d p)
                           , a <- mtDeps mt, Just am <- [lookupMeth d (ptInst p) a], c <- mtCopies am
                           , let kind | S.member (a, m) (cfSB conf) = PAfter
                                      | S.member (a, m) (cfSBR conf) || not (mayFollow conf m a) = PBefore
                                      | otherwise = PCycle ]
                Nothing -> []
            (prem, eqs) = case fdKind f of
                FRead p xs -> (deps p, zip xs (map hwArg (ptArgs p)))
                FApp _ _ xs ys -> ([], [ (x, y) | (x, y) <- zip xs ys, x /= y ])
                FAV p -> (deps p, [ (x, hwArg a) | as <- M.findWithDefault [] p callArgs, (x, a) <- zip as (ptArgs p) ])
            -- the reads here the annotations rule out, and the uses that make
            -- each arbitrary: an earlier step's, recorded in a field of the
            -- State (a call's enable, a read's flag), or the step's own call
            sites = case fdKind f of
                FRead p _ -> M.findWithDefault [] p sitesOf
                _ -> []
            noCall s
                | siWithin s = Right (rw (M.findWithDefault (TLit False) (siUse s) callConds))
                | otherwise = maybe (Right (TLit True)) Left (M.lookup (stateField (siUse s)) fieldIds)
            whenItems p args =
                [ "it sees the same calls of " ++ intercalate ", " (map ((ptInst p ++ ".") ++) deps')
                | let deps' = maybe [] mtDeps (lookupMeth d (ptInst p) (ptMeth p)), not (null deps') ]
                ++ [ "its arguments agree" | args ]
            whenText ws = if null ws then "" else " when " ++ intercalate " and " ws
            siteItem s
                | siWithin s = "calling " ++ portLabel (siUse s) ++ " itself"
                | ptKind (siUse s) == ValueMethod = "after a read of " ++ portLabel (siUse s)
                | otherwise = "after a call of " ++ portLabel (siUse s)
            unlessText = case sites of
                [] -> ""
                ss -> ", unless it fires " ++ intercalate " or " (map siteItem ss)
                      ++ ", which its annotations forbid: then what it sees is arbitrary"
            note = case fdKind f of
                FRead p xs -> name ++ " sees " ++ portLabel p ++ " as the hardware does"
                              ++ whenText (whenItems p (not (null xs))) ++ unlessText
                FAV p      -> name ++ " gets the hardware's result from " ++ portLabel p
                              ++ whenText (whenItems p True)
                FApp {}    -> fdNote f ++ ": equal arguments give equal results"
        in ObsSpec { osField = f, osPrem = prem, osEqs = [ (rw x, rw y) | (x, y) <- eqs ]
                   , osNoCall = map noCall sites, osHw = rw (fdHw f), osNote = note }

    -- One step of the schedule's run, from the versions before it: its
    -- definitions, its view, and the versions after it.
    runStep cur names st =
        let name = ssName st
            viewName = ["View", "at_" ++ name]
            -- With only the schedule's order checked, a value the step sees
            -- that nothing relates to the hardware's but its being the same
            -- value (no calls it sees, the hardware's arguments, no use that
            -- would make it arbitrary) is the hardware's: it gets no variable
            -- and the step's view keeps View.hw's.  The values are decided in
            -- order, an application after its arguments.
            (asHw, specs) = if withOrders then (M.empty, ssObs st) else triage
            triage = let (h, ks) = F.foldl' decide (M.empty, []) (ssObs st) in (h, reverse ks)
            decide (h, ks) s
                | null (osPrem s) && all (noCallVoid h) (osNoCall s) && all (\(x, y) -> clWith h x == y) (osEqs s)
                    = (M.insert (fdName (osField s)) (osHw s) h, ks)
                | otherwise = (h, s : ks)
            noCallVoid h nc = case nc of
                Left g  -> cur IM.! g == TLit False
                Right c -> clWith h c == TLit False
            clWith h = mapTerm $ \t -> case t of
                TView f | Just v <- M.lookup f h -> Just v
                        | otherwise -> Just (TIn (f ++ "@" ++ name))
                TRef x True -> Just (TAt x viewName)
                _ -> Nothing
            -- an observed value at the schedule's view (it is one of the
            -- step's: its terms mention no other)
            obsTerm g | not (S.member g (ssFields st)) =
                          error ("schedlean: " ++ name ++ " observes " ++ g ++ " outside its view")
                      | Just v <- M.lookup g asHw = v
                      | withOrders = TProj viewName g
                      | otherwise  = TIn (g ++ "@" ++ name)
            -- a shared definition at the step's view, which is the hardware's
            -- when every value it sees is
            atView x = if null specs && not withOrders then TAtHw x else TAt x viewName
            -- the fields of the ports its view sees, before the step (its own
            -- calls are a function of these and the view; a whole cycle's
            -- calls are the same in every order that ends in the same state),
            -- and the fields recording uses that make a read arbitrary, each
            -- of those AND'ed with the step's WILL_FIRE: a step that does not
            -- fire sees what the hardware does in every order, which only its
            -- ready signal shows
            viewArgs = S.toAscList (S.fromList ([ (g, False) | s <- specs, (kind, _, gs) <- osPrem s, kind /= PCycle, g <- gs ]
                                                ++ [ (g, True) | s <- specs, Left g <- osNoCall s ]))
            viewPorts = map fst viewArgs
            fireT = ssFire st
            -- (unsimplified, as the step's effect has it: `Run.link` is by definition)
            viewArg t gated = if gated then rawAnd fireT t else t
            vfun = "view@" ++ name
            view | null specs = Nothing
                 | withOrders = Just (ViewT viewName name [] (Just (vfun, [ viewArg (cur IM.! g) gated
                                                                         | (g, gated) <- viewArgs ])))
                 | otherwise  = Just (ViewT viewName name [ (n, n ++ "@" ++ name) | n <- map (fdName . osField) specs ] Nothing)
            vars | null specs = []
                 | withOrders = [ Var vfun (SView (map fieldSort viewPorts)) (GStep name "")
                                      ("what " ++ name ++ " observes, by the calls it sees") ]
                 | otherwise  = [ Var (fdName f ++ "@" ++ name) (fdSort f) (GStep name "") (fdNote f)
                                | f <- map osField specs ]
            fireD = [ SDef ["Seq", name, fireName] SBool (BT t) | Just t <- [ssFireDef st] ]
            -- a fresh name for a definition of the step
            defName ns g = let full = ["Seq", name, fieldName g]
                               alt k = ["Seq", name, fieldName g ++ "#" ++ show k]
                           in if S.notMember full ns then full
                              else alt (until (\k -> S.notMember (alt k) ns) (+ 1) (2 :: Int))
            define (ds, c, ns) g body =
                let nm = defName ns g
                in (SDef nm (fieldSort g) body : ds, IM.insert g (TDef nm) c, S.insert nm ns)
            keep (ds, c, ns) g t = (ds, IM.insert g t c, ns)
            -- each field's new version: the step's terms read the versions
            -- before it ('cur')
            (fdsR, cur', names', stepB) =
                if withOrders then effectsB else
                let cl = mapTerm $ \t -> case t of
                        TView f -> Just (obsTerm f)
                        TRef x True -> Just (atView x)
                        TSt x | Just g <- M.lookup x fieldIds -> Just (cur IM.! g)
                        _ -> Nothing
                    one acc (g, e) =
                        let prev = cur IM.! g
                        in case e of
                            EArms arms ->
                                let body = foldr (\(c, x) r -> tCond (cl c) (cl x) r) prev (reverse arms)
                                in if body == prev then acc else define acc g (BT body)
                            EUncond x -> define acc g (BT (cl x))
                            -- a version of the count after each call made,
                            -- which is the call's position
                            ETasks calls ->
                                let call (a0, cnt) (en, posF, args) =
                                        let en' = cl en
                                            a1@(_, c1, _) = define a0 g (BT (tAdd cnt (tInc en')))
                                            cnt' = c1 IM.! g
                                            a2 = F.foldl' (\a (af, x, z) -> define a af (BT (tCond en' (cl x) z))) a1 args
                                            a3 = define a2 posF (BT (tCond en' cnt' (TConst SPos 0)))
                                        in if en' == TLit False then (a0, cnt) else (a3, cnt')
                                in fst (F.foldl' call (acc, prev) calls)
                            ESticky xs ->
                                case [ (t', why) | (t, why) <- xs, let t' = cl t, t' /= TLit False ] of
                                    [] -> acc
                                    ds -> define acc g (BOr prev ds)
                    (r, c, ns) = F.foldl' one ([], cur, names) (ssEffects st)
                in (r, c, ns, Nothing)
            -- with other orders: the step's effect, and the versions it gives
            effectsB =
                let effs = concatMap expandTasks (ssEffects st)
                    stName' g = TSt (fieldName g)
                    ws0 = [ (g, u) | (g, e) <- effs
                                   , u <- case e of
                                       EArms arms -> [foldr (\(c, x) r -> tCond c x r) (stName' g) (reverse arms)]
                                       EUncond x  -> [x]
                                       ESticky _  -> []
                                       ETasks _   -> []
                                   , u /= stName' g ]
                    os0 = [ (g, c) | (g, ESticky xs) <- effs, let c = tOr (map fst xs), c /= TLit False ]
                    reads = S.toAscList (S.fromList (concatMap (stFields . snd) ws0 ++ concatMap (stFields . snd) os0
                                                     ++ viewPorts))
                    rIdx = M.fromList (zip reads [0 :: Int ..])
                    readArr = IM.fromList (zip [0 ..] reads)
                    toRd = mapTerm $ \t -> case t of
                        TSt x | Just g <- M.lookup x fieldIds -> Just (TRd (rIdx M.! g) (fieldSort g))
                        _ -> Nothing
                    ws = [ (g, toRd u) | (g, u) <- ws0 ]
                    os = [ (g, toRd c) | (g, c) <- os0 ]
                    -- the same at the schedule's state, without simplifying
                    sub = substRaw $ \t -> case t of
                        TRd k _ -> Just (cur IM.! (readArr IM.! k))
                        TView f -> Just (obsTerm f)
                        TRef x True -> Just (TAt x viewName)
                        _ -> Nothing
                    oneW acc (g, u) = let body = sub u
                                      in if body == cur IM.! g then keep acc g body else define acc g (BT body)
                    -- ('false || c' is 'c' to Lean)
                    oneO acc (g, c) = let prev = cur IM.! g
                                      in define acc g (if prev == TLit False then BT (sub c) else BOrC prev (sub c))
                    acc1 = F.foldl' oneW ([], cur, names) ws
                    (r, c', ns) = F.foldl' oneO acc1 os
                    sb = StepB { sbName = name, sbDescr = ssDescr st, sbR = reads, sbW = ws, sbO = os
                               , sbView = if null specs then Nothing
                                          else Just (vfun, [ (rIdx M.! g, gated) | (g, gated) <- viewArgs ])
                               , sbFire = fireT
                               , sbAfter = [ c' IM.! g | (g, _) <- ws ++ os ] }
                in (r, c', ns, Just sb)
        in ( StepOut { roStep = st, roSpecs = specs, roDefs = fireD ++ reverse fdsR, roView = view, roVars = vars
                     , roB = stepB, roBefore = cur, roAfter = cur', roObs = obsTerm, roViewName = viewName }
           , cur', names' )
    -- the fields a term reads (as 'TSt')
    stFields t = case t of
        TSt x | Just g <- M.lookup x fieldIds -> [g]
        _ -> concatMap stFields (subTerms t)

    -- the congruences of a step's observed values, at the versions they see
    -- (a read the annotations rule out, only when the step does not fire
    -- after a use that makes it arbitrary: whatever a step that does not
    -- fire sees, only its ready signal shows, as the hardware has it)
    congsOf o =
        [ Cong { cgName = fdName f ++ "_at_" ++ ssName st
               , cgPremises = [ PTrue t | nc <- osNoCall spec, let t = noCall nc, t /= TLit True ]
                              ++ [ PPort c [ vers kind g | g <- gs ] | (kind, c, gs) <- osPrem spec ]
                              ++ [ PEq (cl x) y | (x, y) <- osEqs spec ]
               , cgLhs = roObs o (fdName f), cgRhs = osHw spec, cgNote = osNote spec }
        | let st = roStep o
        , spec <- roSpecs o, let f = osField spec ]
      where
        noCall nc = tOr [ tNot (ssFire (roStep o))
                        , tNot (case nc of { Left g -> roBefore o IM.! g; Right c -> cl c }) ]
        vers kind g = case kind of
            PBefore -> roBefore o IM.! g
            PAfter  -> roAfter o IM.! g
            PCycle  -> finalVers IM.! g
        cl = mapTerm $ \t -> case t of
            TView g -> Just (roObs o g)
            TRef x True -> Just (TAt x (roViewName o))
            _ -> Nothing

-- | One step of the schedule's run, as 'buildSeq' makes it.
data StepOut = StepOut
    { roStep    :: StepS
    , roSpecs   :: [ObsSpec]             -- ^ the values its view has of its own
    , roDefs    :: [SDef]
    , roView    :: Maybe ViewT
    , roVars    :: [Var]
    , roB    :: Maybe StepB
    , roBefore   :: IM.IntMap Term       -- ^ each field's version before the step
    , roAfter    :: IM.IntMap Term       -- ^ and after it
    , roObs  :: String -> Term       -- ^ an observed value at the step's view
    , roViewName :: [String] }

-- | The Lean namespace for a module: its name, capitalised.
leanNamespace :: Design -> String
leanNamespace d = case dModule d of
    (c : cs) -> toUpper c : cs
    []       -> "Module"

-- | Names Lean generates in a structure's namespace, and the ones this file
-- adds there ('State.init', 'View.hw', 'Hw.oneHot').
structureNames :: [String]
structureNames = words
    "mk rec recOn casesOn noConfusion noConfusionType ctorIdx ctorElim ctorElimType \
    \below brecOn binductionOn ibelow inj injEq sizeOf_spec ext ext_iff \
    \init hw oneHot"

structureNameSet :: S.Set String
structureNameSet = S.fromList structureNames

-- | Names of definitions in the root namespace must not be captured by the
-- local variables the generated code uses.
rootName :: String -> String
rootName n | n `elem` ["i", "s", "h", "v", "fire"] = q (n ++ "'")
           | otherwise = q n

-- | The constructor and the accessor of 'Run.Val' for values of a sort.
valCon, valGet :: Sort -> String
valCon s = "v" ++ sortName s
valGet s = "to" ++ sortName s

-- | How the printer renders a 'TView': inside a definition with a view
-- parameter @v@, as the hardware sees it, or at a view given as text.
data VMode = VParam | VHw | VSub String

-- | A view, for the equations the proof supplies: the hardware's, or one
-- step's (by its Lean name).
data ViewKey = VKHw | VKAt [String]
            | VKFun String [Term]          -- ^ a view function at some calls
    deriving (Eq, Ord)

-- | An application the theorem reaches, whose equation the proof supplies.
data Inst = IDef String (Maybe ViewKey)   -- ^ a shared definition (at a view, if it has one)
          | IHw String                    -- ^ a hardware definition
          | IProj ViewKey String          -- ^ a field of a view
          | IVer [String]                 -- ^ a definition of the sequential execution
    deriving (Eq, Ord)

-- | How a term being searched for applications prints a 'TView'.
data RMode = RInline        -- ^ as the hardware's value, written out
           | RProj ViewKey  -- ^ as a field of a view (in a definition at that view)
    deriving (Eq, Ord)

data PCtx = PCtx
    { pcView   :: VMode
    , pcFields :: M.Map String Field
    , pcConst  :: M.Map (Sort, Integer) String
    , pcStr    :: M.Map String String
    , pcWidth  :: Sort -> Integer
    , pcShadow :: S.Set String
      -- ^ names of definitions that a definition of the sequential
      -- execution's namespace could hide, so written with the module's
    , pcNs     :: String
    , pcPath   :: Char -> String -> String
      -- ^ a field's path in @Inputs@ (@'i'@), @View@ (@'v'@) or @State@
      -- (@'s'@): @part<k>.x@ in a structure split into parts
    }

-- | A qualified Lean name, each component quoted as needed.
qual :: [String] -> String
qual = intercalate "." . map q

-- | A term on one line.  Precedence: 0 top, 2 operand of @||@, 3 of @&&@,
-- 4 of @==@ or @!@, 5 argument of an application.
ppT :: PCtx -> Int -> Term -> String
ppT c p t = ppS c p t ""

-- | 'ppT' as a difference list, so that nesting does not copy the text.
ppS :: PCtx -> Int -> Term -> ShowS
ppS c p t = case t of
    TIn x      -> str "i." . str (pcPath c 'i' x)
    TSt x      -> str "s." . str (pcPath c 's' x)
    TView f    -> case pcView c of
        VParam -> str "v." . str (vpath f)
        VHw    -> maybe (str "v." . str (vpath f)) (ppS c p . fdHw) (M.lookup f (pcFields c))
        VSub vt -> str vt . str "." . str (vpath f)
    TRef x dyn -> app (rootRef x : "i" : [ viewArg | dyn ])
    THw x      -> app [hwName x, "i"]
    TAtHw x    -> app [rootRef x, "i", "(View.hw i)"]
    TAt x v    -> app [rootRef x, "i", "(" ++ qual v ++ " i)"]
    TDef xs    -> app [qual xs, "i"]
    TUnset s   -> str ("0#" ++ show (pcWidth c s))
    TRd k s    -> str ("(Run.rd rs " ++ show k ++ ")." ++ valGet s)
    TProj v f  -> str ("(" ++ qual v ++ " i)." ++ vpath f)
    TFld k s   -> str ("(s " ++ show k ++ ")." ++ valGet s)
    TVF v as f -> str "(" . viewFun v as . str (")." ++ vpath f)
    TAtVF x v as -> parenS (p >= 5) (str (rootRef x) . str " i (" . viewFun v as . str ")")
    TLit b     -> str (if b then "true" else "false")
    TConst SPos v -> str (show v ++ "#" ++ show (pcWidth c SPos))
    TConst s v -> str (M.findWithDefault (show v) (s, v) (pcConst c))
    TStr s     -> str (M.findWithDefault (show s) s (pcStr c))
    TNot _ a   -> parenS (p >= 4) (str "!" . ppS c 4 a)
    TAnd _ ts  -> parenS (p >= 4) (joinS " && " (operands isAnd 3 (tsList ts)))
    TOr _ ts   -> parenS (p >= 3) (joinS " || " (operands isOr 2 (tsList ts)))
    TEq _ a b  -> parenS (p >= 4) (ppS c 4 a . str " == " . ppS c 4 b)
    TCond _ x a b -> parenS (p >= 2) (str "bif " . ppS c 0 x . str " then " . ppS c 0 a
                                    . str " else " . ppS c 0 b)
    TAdd _ a b -> parenS (p >= 5) (ppS c 4 a . str " + " . ppS c 5 b)
  where
    str = showString
    app ws = parenS (p >= 5) (str (unwords ws))
    vpath = pcPath c 'v'
    viewArg = case pcView c of
        VParam -> "v"
        VHw    -> "(View.hw i)"
        VSub vt -> vt
    rootRef x | S.member x (pcShadow c) = q (pcNs c) ++ "." ++ rootName x
              | otherwise = rootName x
    viewFun v as = str ("i." ++ pcPath c 'i' v) . foldr (\a acc -> str " " . ppS c 5 a . acc) id as
    -- (&& and || associate to the left; an operand after the first that is
    -- of the same operator, which only a term built without the smart
    -- constructors has, keeps its parentheses)
    operands same lvl xs = [ if k > (0 :: Int) && same x then parenS True (ppS c 0 x) else ppS c lvl x
                           | (k, x) <- zip [0 ..] xs ]
    isAnd x = case x of { TAnd _ _ -> True; _ -> False }
    isOr x = case x of { TOr _ _ -> True; _ -> False }
    joinS sep fs = case fs of
        []         -> id
        (f : rest) -> f . foldr (\g acc -> showString sep . g . acc) id rest
    parenS b f = if b then showChar '(' . f . showChar ')' else f

-- | A term within a width, over as many lines as it needs.
layout :: PCtx -> Int -> Int -> Term -> [String]
layout c w p t
    | null (drop w flat) = [flat]   -- (forcing at most w + 1 characters of it)
    | needsParens = parenLines (layout c (w - 2) 0 t)
    | otherwise = case t of
        TAnd _ ts -> joinOp "&&" (operandLines isAnd 3 (spine isAnd (tsList ts)))
        TOr _ ts  -> joinOp "||" (operandLines isOr 2 (spine isOr (tsList ts)))
        TCond {} -> condChain t
        _ -> [flat]
  where
    flat = ppT c p t
    -- (a first operand of the same operator, as a definition written in
    -- makes, is the left of one chain, laid out as one list)
    spine same xs = case xs of
        (h : rest) -> go h rest
        []         -> []
      where go x acc | same x, (h : r) <- opsOf x = go h (r ++ acc)
                     | otherwise = x : acc
    opsOf x = case x of { TAnd _ ts -> tsList ts; TOr _ ts -> tsList ts; _ -> [] }
    -- (as in 'ppS': a later operand of the same operator keeps its parentheses)
    operandLines same lvl xs = [ if k > (0 :: Int) && same x then parenLines (layout c (w - 5) 0 x)
                                 else layout c (w - 3) lvl x
                               | (k, x) <- zip [0 ..] xs ]
    isAnd x = case x of { TAnd _ _ -> True; _ -> False }
    isOr x = case x of { TOr _ _ -> True; _ -> False }
    needsParens = case t of
        TCond {} -> p >= 2; TOr {} -> p >= 3; TAnd {} -> p >= 4; TEq {} -> p >= 4; TNot {} -> p >= 4
        TAdd {} -> p >= 5
        _ -> False
    -- a priority mux reads best as one arm per line
    condChain x = go True x
      where go first (TCond _ s a b) =
                let lead = (if first then "" else "else ") ++ "bif " ++ ppT c 0 s ++ " then "
                in case layout c (w - length lead) 0 a of
                       [l] -> (lead ++ l) : go False b
                       ls  -> (lead ++ "(") : map ("    " ++) ls ++ ["  )"] ++ go False b
            go _ e = case layout c (w - 5) 0 e of
                         (l : ls) -> ("else " ++ l) : map ("     " ++) ls
                         []       -> []

parenLines :: [String] -> [String]
parenLines ls = case ls of
    []       -> ["()"]
    [l]      -> ["(" ++ l ++ ")"]
    (l : rs) -> ("(" ++ l) : map (" " ++) (init rs) ++ [" " ++ last rs ++ ")"]

joinOp :: String -> [[String]] -> [String]
joinOp _ [] = []
joinOp op (first : rest) = first ++ concatMap (lead (op ++ " ")) rest
  where lead pre (l : ls) = (pre ++ l) : map (replicate (length pre) ' ' ++) ls
        lead _ []         = []

-- | @header body@, on one line when it fits, else the body indented below.
defLines :: String -> [String] -> [String]
defLines header [b] | length header + length b < 100 = [header ++ " " ++ b]
defLines header bs = header : map ("  " ++) bs

-- | The most fields a Lean structure may have.
maxFields :: Int
maxFields = 255

-- | Part of a section: a small definition, which may share a @mutual@
-- block; comment lines, which may sit in one; or anything else.
data Chunk = CDef [String] | CNote [String] | COther [String]

-- | A section's chunks, small definitions in @mutual@ blocks of up to 100
-- (fewer commands); one over 20,000 characters stays a command of its own.
bundle :: [Chunk] -> [String]
bundle = go [] (0 :: Int)
  where
    -- the open block, last chunk first, and how many definitions it has
    go run n cs = case cs of
        [] -> close run n []
        (CDef ls : rest)
            | sum (map length ls) > 20000 -> close run n (ls ++ go [] 0 rest)
            | n + 1 == 100 -> close (ls : run) (n + 1) (go [] 0 rest)
            | otherwise -> go (ls : run) (n + 1) rest
        (CNote ls : rest)
            | n == 0 -> ls ++ go [] 0 rest
            | otherwise -> go (ls : run) n rest
        (COther ls : rest) -> close run n (ls ++ go [] 0 rest)
    close run n after
        | n >= 2 = case concat (reverse run) of
                       ("" : ls) -> "" : "mutual" : ls ++ "end" : after
                       ls        -> "mutual" : ls ++ "end" : after
        | otherwise = concat (reverse run) ++ after

-- | A banner comment for one of the five sections.
banner :: String -> [String]
banner title =
    [ "", "-- " ++ replicate 76 '=', "-- " ++ title, "-- " ++ replicate 76 '=', "" ]

-- | A line of code with an optional trailing comment.
type CLine = (String, String)

renderCLines :: [CLine] -> [String]
renderCLines ls = map render ls
  where -- comments line up, but not past column 64: a longer line's comment
        -- follows it directly
        width = maximum (0 : [ length code | (code, cmt) <- ls, not (null cmt), length code <= 64 ])
        render (code, cmt) | null cmt  = code
                           | otherwise = code ++ replicate (width - length code) ' ' ++ "  -- "
                                         ++ filter (/= '\n') cmt

-- | Print the whole Lean file.
emitLean :: Design -> Proof -> String
emitLean d pf = unlines $ concat
    [ fileHeader, ["import Std.Tactic.BVDecide", ""], overview, leanOptions
    , ["", "namespace " ++ q ns]
    , banner "1. Submodule & Method State Structure", section1
    , banner "2. Combinational Definitions & Symbolic Datapath Tokens", section2
    , banner "3. Pre-Scheduled Sequential Semantics (One-Rule-At-A-Time)", section3
    , banner "4. Post-Scheduled Synchronous Hardware Semantics (CAN_FIRE / WILL_FIRE / Muxes)", section4
    , banner "5. Disjointness Oracles & Main Schedule Soundness Theorem", section5
    , ["", "end " ++ q ns, "", "#print axioms " ++ q ns ++ ".schedule_sound"] ]
  where
    ns       = leanNamespace d
    steps    = pfSteps pf
    names    = map (stName . sxStep) steps
    fieldMap = M.fromList [ (fdName f, f) | f <- pfFields pf ]
    hasView  = not (null (pfFields pf))
    sq       = pfSeq pf
    ctx m    = PCtx m fieldMap constNames strNames width S.empty ns fieldPath
    -- a field's path in a structure split into parts (no accessor names it)
    fieldPath kind x = case kind of
        'i' | splitInputs, Just k <- M.lookup x inputPartOf -> q (partField k) ++ "." ++ q x
        'v' -> viewPath x
        's' -> sp x
        _   -> q x
    inputPartOf = M.fromList [ (vaName v, k) | (k, vs) <- inputParts, v <- vs ]
    inPath = fieldPath 'i'
    -- (the names a definition in a namespace of section 3 could see first)
    stateNames = S.fromList [ n | (n, _, _) <- stateFieldList ]
    shadow   = S.union stateNames (S.fromList ["any", "val", "stepSequential", fireNameFor stateNames])
    hwC      = ctx VHw
    -- (in a definition of the `Hw` namespace, where `Hw.x` would hide a
    -- shared definition `x` the hardware does not share, being one a step's
    -- view changes)
    hwDefC   = hwC { pcShadow = S.fromList (map ldName (pfHwDefs pf)) }
    -- (in section 3, whose definitions' namespaces hold the fields' names)
    seqC     = hwC { pcShadow = shadow }
    width s  = M.findWithDefault 1 s (pfWidths pf)
    constNames = M.fromList [ ((s, v), q (constLabel s v)) | (s, vs) <- M.toList (pfConsts pf), v <- vs ]
    constLabel s v = case s of
        SBits n -> show n ++ "'d" ++ show v
        _       -> show v
    strNames = M.fromList [ (str, q (filter (/= '»') (show str))) | str <- pfStrings pf ]
    zero s   = if isToken s || s == SPos then "0#" ++ show (width s) else "false"
    boolStmt t = ppT hwC 4 t ++ " = true"

    -- A definition used once (a field of a step record counted as often as
    -- `Agree` reads it) is written in; one facts or views name is kept.
    inlined k = M.lookup k useCounts == Just (1 :: Int)
    useCounts = M.fromListWith (\a b -> min 2 (a + b)) $
        [ (k, if ok then 1 else 2) | (vp, t) <- bodySites, (k, ok) <- refOccs vp t [] ]
        ++ [ (k, if ok then max 1 m else 2) | (m, t) <- fieldSites, (k, ok) <- refOccs False t [] ]
        ++ [ (k, 2) | t <- fixedSites, k <- namesIn t ]
    bodySites = [ (True, ldBody l) | l <- pfSeqDefs pf ]
                ++ [ (False, ldBody l) | l <- pfHwDefs pf ]
                ++ [ (False, t) | x <- soDefs sq, t <- sbodyParts (sdBody x) ]
    sbodyParts b = case b of { BOr p ds -> p : map fst ds; BOrC p c -> [p, c]; BT t -> [t] }
    fieldSites = [ (M.findWithDefault 0 ('s', n) agreeUses, t) | (n, _, _) <- stateFieldList, Just t <- [M.lookup n (soFinals sq)] ]
                 ++ [ (M.findWithDefault 0 ('h', n) agreeUses, t) | not modeB, (n, _, _) <- stateFieldList, Just t <- [hwTerm n] ]
    -- (with other orders, `stepScheduled` keeps the hardware's definitions,
    -- whose muxes mutate.py's order mutant reorders)
    fixedSites = [ t | modeB, (n, _, _) <- stateFieldList, Just t <- [hwTerm n] ]
                 ++ [ t | (_, _, t) <- pfEnv pf ++ pfPrims pf ++ pfOracles pf ++ pfUser pf ]
                 ++ map snd (concatMap congTerms (pfCongs pf))
                 ++ map fdHw (pfFields pf)
                 ++ [ a | v <- soViews sq, Just (_, as) <- [vwFun v], a <- as ]
                 ++ concat [ sbFire sb : map snd (sbW sb ++ sbO sb) ++ sbAfter sb | modeB, sb <- sbs ]
    -- the definition an atom names, and whether a use there may be written in
    -- (not at a view other than the one the definition has)
    refKey vparam t = case t of
        TRef x dyn  -> Just (DShared x, vparam || not dyn)
        TAtHw x     -> Just (DShared x, False)
        TAt x _     -> Just (DShared x, False)
        TAtVF x _ _ -> Just (DShared x, False)
        THw x       -> Just (DHw x, x /= oneHotKey)
        TDef ns     -> Just (DSeq ns, True)
        _           -> Nothing
    -- the definitions a term names, as often as it is printed
    refOccs vp t rest = case t of
        TVF _ as _   -> foldr (refOccs vp) rest as
        TAtVF _ _ as -> keyed (foldr (refOccs vp) rest as)
        _ | atomic t -> keyed rest
          | otherwise -> foldr (refOccs vp) rest (subTerms t)
      where keyed r = maybe r (: r) (refKey vp t)
    -- the definitions a term names (each distinct subterm looked at once)
    namesIn t0 = go [t0] IS.empty
      where go [] _ = []
            go (t : ts) seen = case t of
                TVF _ as _   -> go (as ++ ts) seen
                TAtVF _ _ as -> keyed t (go (as ++ ts) seen)
                _ | atomic t -> keyed t (go ts seen)
                  | IS.member (termId t) seen -> go ts seen
                  | otherwise -> go (subTerms t ++ ts) (IS.insert (termId t) seen)
            keyed t r = maybe r ((: r) . fst) (refKey False t)
    -- a term with the definitions used once written in
    expand = substKeep (\a -> case refKey True a of
                                  Just (k, _) | inlined k -> ML.lookup k expBodies
                                  _ -> Nothing)
    -- each definition's body as Lean reads it, those used once written in
    expBodies = ML.fromList ([ (DShared (ldName l), expand (ldBody l)) | l <- pfSeqDefs pf ]
                             ++ [ (DHw (ldName l), expand (ldBody l)) | l <- pfHwDefs pf ]
                             ++ [ (DSeq (sdName x), expand (rawSBody (sdBody x))) | x <- soDefs sq ])
    sharedBody l = ML.findWithDefault (ldBody l) (DShared (ldName l)) expBodies
    hwBody l = ML.findWithDefault (ldBody l) (DHw (ldName l)) expBodies
    expSBody b = case b of
        BOr p ds -> BOr (expand p) [ (expand t, why) | (t, why) <- ds ]
        BOrC p c -> BOrC (expand p) (expand c)
        BT t     -> BT (expand t)

    fileHeader =
        [ "/-"
        , "  " ++ dModule d ++ ": the schedule bsc chose is sound"
        , ""
        , "  Generated by schedlean --flat from " ++ commentSafe (takeFileName (dSource d))
          ++ " (package " ++ commentSafe (dPackage d) ++ "),"
        , "  written by " ++ commentSafe (dVersion d) ++ "."
        , "  Check it with `lean " ++ ns ++ ".lean`: it needs only the Lean 4 core."
        , "-/" ]

    overview =
        ( [ "/-!", "# Schedule soundness for `" ++ dModule d ++ "`", ""
          , "Each clock cycle, bsc's hardware for `" ++ dModule d ++ "` must do what running the rules and"
          , "methods that fire, one at a time, in the logical execution order"
          , "" ]
          ++ map ("    " ++) (wrap 76 (intercalate ", " names))
          ++ [ ""
             , "would do.  The theorem `schedule_sound` (section 5) proves it: in every cycle,"
             , ""
             , "* every rule that fires has a true guard where it runs,"
             , "* no submodule has its action methods called in an order its scheduling"
             , "  annotations forbid" ++ (if tasks > 0 then "," else ", and") ]
          ++ [ l | tasks > 0
                 , l <- [ "* no system task prints, or decides whether to print on, a value read where"
                        , "  those annotations forbid the read: such a read is illegal when its value can"
                        , "  reach the task's condition, or one of its arguments while the condition"
                        , "  holds, following the selections (if, case, mux, array index) on the way, and" ] ]
          ++ [ "* the hardware makes the same method calls on every submodule as that"
             , "  sequential execution, with the same arguments whenever a call happens, and"
             , "  produces the same module outputs" ++ (if tasks > 0 then ", and" else ".") ]
          ++ [ l | tasks > 0
                 , l <- [ "* it makes the same system-task calls, in the same order (the order of its"
                        , "  always block for tasks), with the same arguments." ] ]
          ++ (if S.null (dHavocPorts d) then []
              else [ ""
                   , "A value method read where its submodule's annotations forbid it, after a call"
                   , "in the cycle that it may not follow, sees an arbitrary value in the sequential"
                   , "execution: a free variable of the cycle, which a congruence ties to the"
                   , "hardware's value only when the step does not fire or no such call came first."
                   , "So the agreement holds whatever such a read returns." ])
          ++ (if not modeB then []
              else [ ""
                   , "A parent may call two methods the module exports as conflict-free in either order."
                   , "The theorem `exported_CF` (section 5) proves the same of each order `CF.orders` lists:"
                   , "for each such pair, the schedule's order with the steps of the method that comes"
                   , "first in it, and those that must follow them, moved after the other's.  It states"
                   , "each step's effect on the whole state (section 3), and shows that the order's run"
                   , "is the schedule's, as each step moved commutes with the steps it moves past." ])
          ++ [ ""
             , "**The model.**  Submodules are black boxes known only by their scheduling"
             , "annotations (section 1).  Datapath values are symbolic tokens and datapath"
             , "operators uninterpreted functions; only the control logic is concrete"
             , "(section 2).  Section 3 is the one-rule-at-a-time execution; section 4 is"
             , "the hardware, transcribed from bsc's own back end (`aState`)."
             , "" ]
          ++ wrap 80 assumed
          ++ [ "The proof is by `bv_decide`, whose SAT certificate Lean checks with compiled"
             , "code: the axioms printed at the end are Lean's standard three plus that check." ]
          ++ (if tasks > 0
                then [ "", "**System tasks.** " ++ show tasks ++ " system-task call(s) (`$display` and the like, bsc's"
                     , "run-time checks among them).  Each is compared with the hardware's call: its position"
                     , "among the calls made (`TaskPos`, 0 if not made) and, when made, its arguments.  The"
                     , "hardware's order is that of the module's always block for tasks, as bsc's Verilog"
                     , "back end prints it; a value-returning task's result is a free variable both share."
                     , "By the third point above, what a task prints, and whether it prints, depends only"
                     , "on reads the annotations allow where it runs.  This is a property of this module's"
                     , "Verilog: calls of different modules in one cycle come in an order Verilog leaves open." ]
                else [])
          ++ [ "-/" ] )
      where tasks = length (pfTaskPorts pf)
            -- the hypotheses this file has, in the order the theorem takes them
            assumed = case [ text | (has, text) <- assumptions, has ] of
                [] -> "**What is assumed:** nothing; `schedule_sound` has no hypotheses."
                ts -> "**What is assumed** (the hypotheses of `schedule_sound`): " ++ listText ts ++ "."
            assumptions =
                [ (not (null (pfEnv pf)), "that the parent obeys the module's exported schedule (`Environment`)")
                , (not (null (pfPrims pf)), "what bsc's wire primitives do, from their Verilog (`Primitives`)")
                , (not (null (pfOracles pf)), "facts bsc's disjointness solver proved (`Oracles`)")
                , (not (null (pfUser pf)), "the user's `mutually_exclusive`/`conflict_free` assertions and \
                                           \bsc's other run-time checks (`Assertions`)")
                , (not (null (pfCongs pf)), "functional consistency of the uninterpreted functions \
                                         \(Ackermann's congruences, `Congruence`)") ]
            listText ts = case reverse ts of
                [t]          -> t
                (t : before) -> intercalate "; " (reverse before) ++ "; and " ++ t
                []           -> ""

    leanOptions =
        [ "", "set_option maxRecDepth 4000000"
        , "set_option maxHeartbeats 0"
        , "set_option linter.unusedVariables false"
        , "set_option autoImplicit false"
        , "set_option genInjectivity false" ]

    -- 1. ---------------------------------------------------------------------
    section1 =
        [ "/-! The token sorts come first, since `State` uses them.  Datapath values are"
        , "never concrete: a value wider than one bit is a token, a name for a value"
        , "rather than the value.  Distinct terms get distinct codes, and code 0 is kept"
        , "for an argument no call has set (`State.init`).  A sort with n terms gets"
        , "enough bits for n + 1 codes, so by the small-model property of equality logic"
        , "the theorem holds for the real values too. -/", "" ]
        ++ concat [ doc ("`" ++ sortLabel s ++ "` values: " ++ show (M.findWithDefault 0 s (pfCounts pf))
                         ++ (if M.findWithDefault 0 s (pfCounts pf) == 1 then " term" else " terms")
                         ++ " in the proof and the unset code, so " ++ show w ++ "-bit tokens.")
                    ++ ["abbrev " ++ sortName s ++ " := BitVec " ++ show w, ""]
                  | (s, w) <- M.toList (pfWidths pf), s /= SPos ]
        ++ concat [ doc ("Positions in the cycle's system-task calls, from 1 (0 if not made): "
                         ++ show (length (pfTaskPorts pf)) ++ " calls in all, so " ++ show w
                         ++ "-bit numbers.  Unlike a token, a position is a number.")
                    ++ ["abbrev TaskPos := BitVec " ++ show w, ""]
                  | Just w <- [M.lookup SPos (pfWidths pf)] ]
        ++ doc ("What one cycle does, as seen by the submodules: for each copy of each action \
                \method in use, whether it is called and with what arguments; the module's \
                \outputs; " ++ (if null (pfTaskPorts pf) then "and whether the execution broke a rule." else
                "whether the execution broke a rule; and for each system-task call, its position \
                \among the calls made, from 1 (0 if it is not made), and its arguments."))
        ++ (if not splitState
              then ["structure State where"] ++ map ("  " ++) (renderCLines stateFields)
              else concat [ ["structure State." ++ partType k ++ " where"]
                            ++ map ("  " ++) (renderCLines [ (q n ++ " : " ++ sortName srt, cmt) | (n, srt, cmt) <- fs ])
                            ++ [""]
                          | (k, fs) <- stateParts ]
                   ++ ["structure State where"]
                   ++ [ "  " ++ q (statePartField k) ++ " : State." ++ partType k | (k, _) <- stateParts ])
        ++ ["", "/-- The start of a cycle: nothing called yet. -/"
           , "noncomputable abbrev State.init : State :="]
        ++ stateLiteral (const zero) []
    sortLabel s = case s of { SBits n -> "Bit#(" ++ show n ++ ")"; SString -> "String"; _ -> show s }
    -- a State with more fields than a structure may have is split like Inputs
    splitState = length stateFieldList > maxFields
    stateParts = zip [1 :: Int ..] (chunksOf maxFields stateFieldList)
    nStateParts = length stateParts
    statePartOf = M.fromList [ (n, k) | (k, fs) <- stateParts, (n, _, _) <- fs ]
    statePrefix = freePrefix "part" [ n | (n, _, _) <- stateFieldList ]
    statePartField k = statePrefix ++ show k
    -- the field as the left of an update: its path when State is split
    sp n | splitState, Just k <- M.lookup n statePartOf = q (statePartField k) ++ "." ++ q n
         | otherwise = q n
    -- a State literal, each field's value given by its name and sort, with
    -- an optional comment per field
    stateLiteral val cmts0
        | not splitState =
            renderCLines [ (code, cmt n)
                         | ((code, _), (n, _, _)) <- zip (literal [ (n, val n srt) | (n, srt, _) <- stateFieldList ])
                                                         stateFieldList ]
        | otherwise =
            concat [ [ (if k == 1 then "  { " else "    ") ++ q (statePartField k) ++ " :=" ]
                     ++ map ("    " ++) (renderCLines [ (code, cmt n)
                                                     | ((code, _), (n, _, _)) <- zip (literal [ (n, val n srt) | (n, srt, _) <- fs ]) fs ])
                     ++ [ if k == nStateParts then "  }" else "    ," ]
                   | (k, fs) <- stateParts ]
      where -- (the first comment of a field, as 'lookup' finds it)
            cmts = M.fromListWith (\_ old -> old) cmts0
            cmt n = M.findWithDefault "" n cmts
    stateFieldList = pfStateFields pf
    -- each child's State fields, its ports in order
    portsByInst = M.fromListWith (++) [ (ptInst p, [p]) | p <- reverse (pfPorts pf) ]
    childFs = M.fromList [ (chInst c, childFields c) | c <- M.elems (dChildren d) ]
    childFields c =
        concat [ [ (en, SBool, portLabel p ++ " called") | Just en <- [ptEnable p] ]
                 ++ [ (a, sortOf t, "  argument " ++ show k) | (k, (a, t)) <- zip [1 :: Int ..] (ptArgs p) ]
               | p <- M.findWithDefault [] (chInst c) portsByInst ]
    stateFields = concat
        [ ( [ (l, "") | not (null fs), l <- childHeader c ] ++ [ (q n ++ " : " ++ sortName s, cmt) | (n, s, cmt) <- fs ] )
        | c <- map (dChildren d M.!) (dChildOrder d), let fs = childFs M.! chInst c ]
        ++ [ ("-- module outputs", "") | not (null (pfOutputs pf)) ]
        ++ [ (q o ++ " : " ++ sortName s, "") | (o, s, _, _) <- pfOutputs pf ]
        ++ [ ("-- the sequential execution's own record", "")
           , ("illegal_access : Bool", "a submodule saw an order its annotations forbid")
           , ("guard_violated : Bool", "a rule ran with a false guard") ]
        ++ [ ("-- reads so far that mattered: a later read of a value method that may not follow one sees an arbitrary value", "")
           | not (null flagFields) ]
        ++ [ (q n ++ " : Bool", cmt) | (n, cmt) <- flagFields ]
        ++ [ ("-- the system-task calls, in the schedule's order (then the hardware's own), and the count so far", "")
           | not (null (pfTaskPorts pf)) ]
        ++ [ (q tasksField ++ " : TaskPos", "calls made so far") | not (null (pfTaskPorts pf)) ]
        ++ concat [ (q (tpPos tp) ++ " : TaskPos", tpNote tp)
                    : [ (q a ++ " : " ++ sortName srt, "  argument " ++ show j) | (j, (a, srt)) <- zip [1 :: Int ..] (tpArgs tp) ]
                  | tp <- pfTaskPorts pf ]
    flagFields = [ (stateField p, portLabel p ++ " read where its value mattered") | p <- pfFlagPorts pf ]
    -- a child's header: its module and annotations (wrapped), and whether bsc
    -- made it (the conflict_free checks' wires have names starting with _)
    childHeader c =
        [ "-- " ++ chInst c ++ " : " ++ chModule c
          ++ (if take 1 (chInst c) == "_" then "  (inserted by bsc for a conflict_free check)" else "") ]
        ++ map ("--   " ++) (wrap 76 (unwords (words (chSched c))))
    literal kvs = case kvs of
        [] -> [("  {}", "")]
        _  -> let len = length kvs
              in [ ((if k == 1 then "  { " else "    ") ++ q n ++ " := " ++ v
                    ++ (if k == len then " }" else ","), "")
                 | (k, (n, v)) <- zip [1 :: Int ..] kvs ]

    -- 2. ---------------------------------------------------------------------
    section2 = bundle $
        [ CDef (doc ("Constant " ++ constLabel s v ++ " (token code " ++ show k ++ ").")
                ++ ["noncomputable abbrev " ++ q (constLabel s v) ++ " : " ++ sortName s ++ " := "
                    ++ show k ++ "#" ++ show (width s)])
        | (s, vs) <- M.toList (pfConsts pf), (k, v) <- zip [1 :: Integer ..] vs ]
        ++ [ CDef (doc ("String constant " ++ show str ++ ".")
                   ++ ["noncomputable abbrev " ++ strNames M.! str ++ " : TokStr := "
                       ++ show k ++ "#" ++ show (width SString)])
           | (k, str) <- zip [1 :: Integer ..] (pfStrings pf) ]
        ++ [ COther ([ "" | not (M.null (pfConsts pf) && null (pfStrings pf)) ]
                     ++ (if hasView then viewStruct else [])
                     ++ doc "The free variables of one cycle.  The theorem holds for every value of each."
                     ++ inputsDecl
                     ++ (if hasView then viewHw else [])) ]
        ++ [ CDef (sharedDef l) | l <- pfSeqDefs pf, not (inlined (DShared (ldName l))) ]
    groups = nubOrd (map vaGroup (pfVars pf))
    groupTitle g = case g of
        GInput    -> "module inputs"
        GStart    -> "submodule outputs at the start of the cycle"
        GWire     -> "submodule outputs that see same-cycle calls, as the hardware's wires"
        GDatapath -> "results of uninterpreted datapath operators (Ackermann variables)"
        GTask     -> "results of system tasks"
        GUnspec   -> "values bsc leaves unspecified"
        GStep r o -> "what " ++ r ++ " observes of values that change within the cycle, or that it reads"
                     ++ " where the annotations forbid" ++ o
    -- the variables by group (by rank, then first appearance), in order
    -- within each ('sortOn' is stable)
    groupIndex = M.fromList (zip groups [0 :: Int ..])
    inputVars = map snd (sortOn fst [ ((groupRank (vaGroup v), groupIndex M.! vaGroup v), v) | v <- pfVars pf ])
    fieldLines vs =
        let byGroup = M.fromListWith (++) [ (vaGroup v, [v]) | v <- reverse vs ]
        in concat
            [ ("-- " ++ groupTitle g, "") : [ (q (vaName v) ++ " : " ++ sortName (vaSort v), note v)
                                            | v <- M.findWithDefault [] g byGroup ]
            | g <- nubOrd (map vaGroup vs) ]
    -- More variables than a structure may have go in parts, each variable
    -- written by its path (`i.part<k>.x`).
    splitInputs = length inputVars > maxFields
    inputParts = zip [1 :: Int ..] (chunksOf maxFields inputVars)
    partPrefix = freePrefix "part" (map vaName inputVars)
    partField k = partPrefix ++ show k
    partType k = "Part" ++ show k
    inputsDecl
        | not splitInputs = ["structure Inputs where"] ++ map ("  " ++) (renderCLines (fieldLines inputVars))
        | otherwise =
            concat [ ["structure Inputs." ++ partType k ++ " where"] ++ map ("  " ++) (renderCLines (fieldLines vs)) ++ [""]
                   | (k, vs) <- inputParts ]
            ++ ["structure Inputs where"]
            ++ [ "  " ++ q (partField k) ++ " : Inputs." ++ partType k | (k, _) <- inputParts ]
    groupRank g = case g of
        GInput -> 0 :: Int; GStart -> 1; GWire -> 2; GDatapath -> 3; GTask -> 4; GUnspec -> 5; GStep _ _ -> 6
    note v = let n = vaNote v in if n == vaName v then "" else n
    viewStruct =
        doc ("Values that can change within a cycle -- outputs of submodules that see \
                     \same-cycle calls or that a step reads where the annotations forbid, and what \
                     \is computed from them -- as one observer sees them.")
        ++ (if not splitView
              then ["structure View where"] ++ map ("  " ++) (renderCLines (viewLines (pfFields pf)))
              else concat [ ["structure View." ++ partType k ++ " where"]
                            ++ map ("  " ++) (renderCLines (viewLines fs)) ++ [""]
                          | (k, fs) <- viewParts ]
                   ++ ["structure View where"]
                   ++ [ "  " ++ q (viewPartField k) ++ " : View." ++ partType k | (k, _) <- viewParts ])
        ++ [""]
    viewHw =
        ["", "/-- The values the hardware sees. -/"
           , "noncomputable def View.hw (i : Inputs) : View :="]
        ++ (if not splitView
              then renderCLines (literal [ (fdName f, ppT hwC 0 (fdHw f)) | f <- pfFields pf ])
              else concat [ [ (if k == 1 then "  { " else "    ") ++ q (viewPartField k) ++ " :=" ]
                            ++ map ("    " ++) (map fst (literal [ (fdName f, ppT hwC 0 (fdHw f)) | f <- fs ]))
                            ++ [ if k == nViewParts then "  }" else "    ," ]
                          | (k, fs) <- viewParts ])
    viewLines fs = [ (q (fdName f) ++ " : " ++ sortName (fdSort f), fdNote f) | f <- fs ]
    -- a View with more fields than a structure may have is split like Inputs
    splitView = length (pfFields pf) > maxFields
    viewParts = zip [1 :: Int ..] (chunksOf maxFields (pfFields pf))
    nViewParts = length viewParts
    viewPartOf = M.fromList [ (fdName f, k) | (k, fs) <- viewParts, f <- fs ]
    viewPrefix = freePrefix "part" (map fdName (pfFields pf))
    viewPartField k = viewPrefix ++ show k
    viewPath f | splitView, Just k <- M.lookup f viewPartOf = q (viewPartField k) ++ "." ++ q f
               | otherwise = q f
    sharedDef l =
        [""]
        ++ defLines ("noncomputable def " ++ rootName (ldName l) ++ " (i : Inputs)"
                     ++ (if ldDyn l then " (v : View)" else "") ++ " : " ++ sortName (ldSort l) ++ " :=")
                    (layout (ctx VParam) 92 0 (sharedBody l))

    -- 3. ---------------------------------------------------------------------
    section3 =
        [ "/-! The one-rule-at-a-time execution, field by field.  Each step runs only if the"
        , "hardware fires it (`WILL_FIRE`).  It reads the submodules as they stand *before* it"
        , "-- the start of the cycle, or for an output that sees same-cycle calls, the step's"
        , "own view -- checks its guard and the legality of each action-method call against"
        , "what earlier steps did, and records its calls.  A value it reads where the"
        , "submodule's annotations forbid it (after a call it may not follow) is arbitrary:"
        , "its view has a value of its own there, which a congruence (section 5) ties to the"
        , "hardware's only when the step does not fire or no such call came first." ]
        ++ (if not (any (not . null . sxTasks) steps) then [] else
           [ "A system task's reads are not arbitrary: one the annotations forbid is illegal when"
           , "its value can reach what the task prints, or whether it prints, following the"
           , "selections on the way." ])
        ++ (if not (any (\sx -> not (null (sxTasks sx)) || not (null (sxFlags sx))) steps) then [] else
           [ "When a read reaches a value is worked out from the top.  A definition several others"
           , "refer to, or a step itself, heads a chain of definitions only one other refers to:"
           , "`<def>@up` says when one of the chain reaches its head, `<head>@reaches@<copy>` when a"
           , "read in the chain does, and `<head>@reaches@<def>` when the head of another chain it"
           , "refers to does, the same for every step.  `<head>@prints@<step>` says when a head's"
           , "value reaches what the step's tasks print or whether they print, and"
           , "`<head>@does@<step>` when it reaches anything the step does." ])
        ++
        [ ""
        , "For each field a step writes, `Seq.<step>.<field>` is the field once the step has"
        , "run, in terms of each field as the last earlier step left it (or as `State.init` has"
        , "it); a field the step leaves alone has no definition.  A later call of a method"
        , "replaces an earlier one's arguments; a flag (`illegal_access`, `guard_violated`, a"
        , "call's enable" ++ (if null flagFields then "" else ", a read") ++ ") is set once any step sets it." ]
        ++ [ l | not (null (pfTaskPorts pf))
               , l <- [ "A system-task call made records its arguments and its position, the count of calls"
                      , "made up to it (`«$tasks»`, of which a step has a version after each call it makes);"
                      , "one not made, position 0." ] ]
        ++ [ "Here as elsewhere, a value used in one place only is written there, not defined. -/" ]
        ++ bundle (concatMap seqGroup (soGroups sq))
        ++ [ "", "/-- One cycle, one rule at a time: each field as the last step that wrote it left it. -/"
           , "noncomputable def stepSequential (i : Inputs) : State :=" ]
        ++ stateLiteral (\n s -> maybe (zero s) (longValue . expand) (M.lookup n (soFinals sq))) []
        ++ (if modeB then runSection3 else [])
    -- (a long value, as definitions written in make, over several lines)
    longValue t = intercalate ("\n" ++ replicate 10 ' ') (layout seqC 80 0 t)
    -- a step's definitions, after a comment naming it (those written where
    -- they are used left out)
    seqGroup (_, descr, fds, view, ds)
        | isNothing view && null defs = []
        | otherwise = CNote ["", "-- " ++ filter (/= '\n') descr]
                      : map (CDef . printDef) (filter keepDef fds) ++ [ CDef (viewDef vw) | Just vw <- [view] ]
                      ++ map (CDef . printDef) (filter keepDef ds)
      where defs = filter keepDef (fds ++ ds)
    keepDef x = not (inlined (DSeq (sdName x)))
    printDef x = defLines ("noncomputable def " ++ qual (sdName x) ++ " (i : Inputs) : " ++ sortName (sdSort x) ++ " :=")
                          (bodyLines (expSBody (sdBody x)))
    -- a flag's definition reads as the value before OR'ed with what sets it,
    -- one reason to a line
    bodyLines b = case b of
        BT t -> layout seqC 92 0 t
        BOr p ds ->
            let items = [ (ppT seqC 2 p, "") | p /= TLit False ] ++ [ (ppT seqC 3 t, why) | (t, why) <- ds ]
                flat = intercalate " || " (map fst items)
            in if null items then ["false"]
               else if all (null . snd) items && length flat <= 92 then [flat]
               else renderCLines [ ((if k == 0 then "" else "|| ") ++ code, why)
                                 | (k, (code, why)) <- zip [0 :: Int ..] items ]
        -- (the condition in parentheses: `Run` ORs it in as one value)
        BOrC p c ->
            let pl = ppT seqC 2 p
            in case layout seqC 86 0 c of
                   [l] | length pl + length l < 86 -> [pl ++ " || (" ++ l ++ ")"]
                   ls -> (pl ++ " ||") : map ("  " ++) (parenLines ls)
    viewDef vw = case vwFun vw of
        Nothing ->
            let nFlds = length (vwVars vw)
            in [""] ++ doc ("What " ++ vwStep vw ++ " sees of the values that change within the cycle"
                            ++ (if S.null (dHavocPorts d) then "" else " or that it may read out of order") ++ ".")
               ++ [ "noncomputable def " ++ qual (vwName vw) ++ " (i : Inputs) : View :="
               , "  { View.hw i with" ]
               ++ [ "    " ++ viewPath f ++ " := i." ++ inPath v ++ (if j == nFlds then " }" else ",")
                  | (j, (f, v)) <- zip [1 :: Int ..] (vwVars vw) ]
        Just (vf, args) ->
            [""] ++ doc ("What " ++ vwStep vw ++ " sees of the values that change within the cycle"
                         ++ (if S.null (dHavocPorts d) then "" else " or that it may read out of order")
                         ++ ": its view as a function of the calls it sees, at the calls it sees in this order.")
            ++ defLines ("noncomputable def " ++ qual (vwName vw) ++ " (i : Inputs) : View :=")
                        (wrapJoin 88 " " (("i." ++ inPath vf) : map (ppT seqC 5) args))
    -- Other orders ------------------------------------------------------------
    --
    -- With exported conflict-free pairs ('withOrders'), each step is also an
    -- effect on the whole State ('StepB'), composed by a few generic Lean
    -- definitions ('runGeneric', 'runLemmas').  Every order of 'dOrders' is
    -- the schedule's with some steps of a range moved after the others
    -- ('orderBlock'); it is the schedule's own run when each moved step
    -- commutes with each later step of the range that stays.  A step that
    -- reads and flags nothing and sets only fields no other step touches
    -- commutes with every step ('Run.free'); two steps whose fields do not
    -- overlap but for flags both set commute by a generic theorem and a
    -- decided check ('Run.indep'); other pairs by facts 'bv_decide' proves
    -- ('Run.comms').  The schedule's own run is the field-by-field
    -- execution above, step by step and by definition ('Run.link').
    modeB    = withOrders d
    sbs      = soSteps sq
    nSteps   = length sbs
    sbArr    = IM.fromList (zip [0 :: Int ..] sbs)
    posOfStep = M.fromList (zip (map sbName sbs) [0 :: Int ..])
    fieldNum = M.fromList (zip [ n | (n, _, _) <- stateFieldList ] [0 :: Int ..])
    fieldOf  = IM.fromList (zip [0 :: Int ..] stateFieldList)
    fieldSortN k = let (_, s, _) = fieldOf IM.! k in s
    fieldNameN k = let (n, _, _) = fieldOf IM.! k in n
    nFields  = IM.size fieldOf
    -- the sorts of the fields, one constructor of `Run.Val` each
    valSorts = M.elems (M.fromList [ (sortName s, s) | (_, s, _) <- stateFieldList ] `M.union` M.singleton "Bool" SBool)
    fpOfPos p = let sb = sbArr IM.! p in (sbR sb, map fst (sbW sb), map fst (sbO sb))
    touches p = let (r, w, o) = fpOfPos p in r ++ w ++ o
    -- the orders, each with its block and its methods (indices into 'cfMethods')
    blocks = [ (o, orderBlockOf o) | o <- drop 1 (dOrders d) ]
    orderBlockOf o = fromMaybe (error ("schedlean: internal error: order " ++ orName o
                                       ++ " is not the schedule's with a block moved"))
                               (orderBlock posOfStep o)
    cfMethods = nubOrd (concat [ [snd (orMoved o), fst (orMoved o)] | (o, _) <- blocks ])
    cfMethodIdx = M.fromList (zip cfMethods [0 :: Int ..])
    cfMethodArr = IM.fromList (zip [0 :: Int ..] cfMethods)
    -- each step's owner: the one step that touches a field, if only one does
    owners = IM.fromListWith (++) [ (f, [p]) | p <- [0 .. nSteps - 1], f <- nubOrd (touches p) ]
    ownerOf f = case IM.lookup f owners of { Just [p] -> Just p; _ -> Nothing }
    isFree p = let (r, w, o) = fpOfPos p in null r && null o && all (\f -> ownerOf f == Just p) w
    -- 'Run.indep' as Lean computes it
    indep (xr, xw, xo) (yr, yw, yo) =
        let ys = IS.fromList; yR = ys yr; yW = ys yw; yO = ys yo; xR = ys xr; xW = ys xw; xO = ys xo
        in all (\w -> not (IS.member w yR || IS.member w yW || IS.member w yO)) xw
           && all (\w -> not (IS.member w xR || IS.member w xW || IS.member w xO)) yw
           && all (\o -> not (IS.member o yR)) xo && all (\o -> not (IS.member o xR)) yo
    -- for each moved step, the later steps it must commute with: of the
    -- range, up to its end, those that stay
    rangeEnds = IM.fromListWith max [ (last ms, t) | (_, (_, t, ms)) <- blocks, not (null ms) ]
    extraNeeds = IM.fromListWith IS.union
        [ (x, IS.fromList [ y | y <- [x + 1 .. t - 1], not (IS.member y mset) ])
        | (_, (_, t, ms)) <- blocks, let mset = IS.fromList ms, x <- init' ms ]
    init' xs = if null xs then [] else init xs
    needsOf x = IS.union (maybe IS.empty (\t -> IS.fromAscList [x + 1 .. t - 1]) (IM.lookup x rangeEnds))
                         (IM.findWithDefault IS.empty x extraNeeds)
    movedSteps = IS.toAscList (IS.union (IM.keysSet rangeEnds) (IM.keysSet extraNeeds))
    -- the intervals of each moved step that is not free, each a maximal run
    -- of consecutive steps it must commute with, split into the pieces where
    -- the check decides it and the steps that need a proof
    intervals = [ (x, run) | x <- movedSteps, not (isFree x), run <- runsOf (IS.toAscList (needsOf x)) ]
    runsOf ys = case ys of
        [] -> []
        (y : rest) -> let (r, rest') = spanRun y rest in r : runsOf rest'
    spanRun y rest = case rest of
        (z : more) | z == y + 1 -> let (r, rest') = spanRun z more in (y : r, rest')
        _ -> ([y], rest)
    commutesFree x y = indep (fpOfPos y) (fpOfPos x)
    semPairs = [ (x, y) | (x, run) <- intervals, y <- run, not (commutesFree x y) ]
    hasSem = not (null semPairs)

    -- Section 3: the definitions -----------------------------------------------
    runSection3 =
        [ "", "/-! ### Other orders", ""
        , "A parent may call two methods the module exports as conflict-free in either order, so"
        , "the orders that run either one first must be right too.  For them each step is stated"
        , "once more, as an effect on the whole State: `Run.fp.<step>` lists the fields it reads,"
        , "the fields it sets to the values `Run.U.<step>` computes from those, and the flags it"
        , "sets when the conditions `Run.C.<step>` computes from those hold, each by its number in"
        , "`State` (from 0).  `Run.seg` runs some steps of the schedule's order, and"
        , "`CF.Order.run` an order of `CF.orders`: the schedule's, but for some steps of a range"
        , "moved after the others.  At the schedule's states, each effect is, by definition, the"
        , "step's definitions above (`Run.link`, section 5). -/", "" ]
        ++ doc "A field's value, of one of the sorts of `State`'s fields."
        ++ ["inductive Run.Val where"]
        ++ [ "  | " ++ valCon s ++ " (x : " ++ sortName s ++ ")" | s <- valSorts ]
        ++ [""]
        ++ [ "def Run.Val." ++ valGet s ++ " : Run.Val → " ++ sortName s ++ " | ." ++ valCon s ++ " x => x"
             ++ (if length valSorts > 1 then " | _ => " ++ zero s else "")
           | s <- valSorts ]
        ++ map snd (filter fst runGeneric3)
        ++ ["", "/-- The start of a cycle, by field number. -/"
           , "noncomputable def Run.init (k : Nat) : Run.Val :="]
        ++ map ("  " ++) (natTree "k" 0 nFields (\k -> "." ++ valCon (fieldSortN k) ++ " " ++ zero (fieldSortN k))
                                  ".vBool false")
        ++ concatMap stepEffDefs sbs
        ++ ["", "/-- The steps of the schedule's order, by position (none after the last). -/"
           , "def Run.n : Nat := " ++ show nSteps
           , "def Run.fpOf (k : Nat) : List Nat × List Nat × List Nat :="]
        ++ map ("  " ++) (natTree "k" 0 nSteps (\k -> qual ["Run", "fp", sbName (sbArr IM.! k)]) "([], [], [])")
        ++ ["noncomputable def Run.uOf (i : Inputs) (k : Nat) : List Run.Val → List Run.Val :="]
        ++ map ("  " ++) (natTree "k" 0 nSteps (\k -> qual ["Run", "U", sbName (sbArr IM.! k)] ++ " i") "fun _ => []")
        ++ ["noncomputable def Run.cOf (i : Inputs) (k : Nat) : List Run.Val → List Bool :="]
        ++ map ("  " ++) (natTree "k" 0 nSteps (\k -> qual ["Run", "C", sbName (sbArr IM.! k)] ++ " i") "fun _ => []")
        ++ runSteps3
        ++ ["", "/-- A state by field numbers as a `State`. -/"
           , "noncomputable def Run.toState (s : Run.St) : State :="]
        ++ stateLiteral (\n srt -> "(s " ++ show (fieldNum M.! n) ++ ")." ++ valGet srt) []
        ++ ordersDefs3

    -- the generic part of section 3 (each line with whether to print it)
    runGeneric3 = [ (True, l) | l <-
        [ ""
        , "/-- A `State` as a function of its fields' numbers. -/"
        , "abbrev Run.St := Nat → Run.Val"
        , ""
        , "/-- The `k`-th field a step reads. -/"
        , "def Run.rd (rs : List Run.Val) (k : Nat) : Run.Val := rs.getD k (.vBool false)"
        , ""
        , "/-- What a step does: the fields it reads (`R`), the fields it sets (`W`) to the values"
        , "    `U` computes from them, and the flags (`O`) it sets when the conditions `C` computes"
        , "    from them hold. -/"
        , "structure Run.Eff where"
        , "  R : List Nat"
        , "  W : List Nat"
        , "  O : List Nat"
        , "  U : List Run.Val → List Run.Val"
        , "  C : List Run.Val → List Bool"
        , ""
        , "/-- Field `k` once a step ran that read `rs`, given its value `old` before. -/"
        , "def Run.Eff.val (e : Run.Eff) (rs : List Run.Val) (old : Run.Val) (k : Nat) : Run.Val :="
        , "  let v1 := match (e.W.zip (e.U rs)).lookup k with"
        , "    | some v => v"
        , "    | none => old"
        , "  match (e.O.zip (e.C rs)).lookup k with"
        , "    | some c => .vBool (v1.toBool || c)"
        , "    | none => v1"
        , ""
        , "/-- A step run on a state. -/"
        , "def Run.Eff.apply (e : Run.Eff) (s : Run.St) : Run.St := fun k => e.val (e.R.map s) (s k) k" ] ]

    -- a step's effect: its footprint and what it computes
    stepEffDefs sb =
        let nm = sbName sb
            names ks = if null ks then "nothing" else unwords [ fieldNameN k | k <- ks ]
            readArr = IM.fromList (zip [0 ..] (sbR sb))
            letV = case sbView sb of
                Nothing -> []
                Just (vf, idxs) -> [ unwords (("let v := i." ++ inPath vf)
                                              : [ ppT effC 5 (if gated then rawAnd (sbFire sb) rd else rd)
                                                | (j, gated) <- idxs, let rd = TRd j (fieldSortN (readArr IM.! j)) ]) ]
            items ls = case ls of
                [] -> ["[]"]
                _  -> concat [ case l of
                                   (x : xs) -> ((if k == 0 then "[" else ", ") ++ x) : map ("  " ++) xs
                                   [] -> []
                             | (k, l) <- zip [0 :: Int ..] ls ] ++ ["]"]
            uItems = [ case layout effC 84 5 u of
                           [l] -> ["." ++ valCon (fieldSortN g) ++ " " ++ l]
                           ls  -> ("." ++ valCon (fieldSortN g)) : map ("  " ++) ls
                     | (g, u) <- sbW sb ]
            cItems = [ layout effC 86 0 c | (_, c) <- sbO sb ]
            joinItems ls = case items ls of
                xs | length (concat xs) + length xs < 80 -> [concat xs]
                   | otherwise -> xs
        in ["", "/-- " ++ commentSafe (sbDescr sb) ++ ": reads " ++ names (sbR sb) ++ "; sets "
                ++ names (map fst (sbW sb)) ++ "; flags " ++ names (map fst (sbO sb)) ++ ". -/"]
           ++ ["def " ++ qual ["Run", "fp", nm] ++ " : List Nat × List Nat × List Nat := ("
               ++ numList (sbR sb) ++ ", " ++ numList (map fst (sbW sb)) ++ ", " ++ numList (map fst (sbO sb)) ++ ")"]
           ++ defLines ("noncomputable def " ++ qual ["Run", "U", nm] ++ " (i : Inputs) (rs : List Run.Val) : List Run.Val :=")
                       (if null (sbW sb) then ["[]"] else letV ++ joinItems uItems)
           ++ defLines ("noncomputable def " ++ qual ["Run", "C", nm] ++ " (i : Inputs) (rs : List Run.Val) : List Bool :=")
                       (if null (sbO sb) then ["[]"] else letV ++ joinItems cItems)
    numList ks = "[" ++ intercalate ", " (map show ks) ++ "]"
    -- (in the effects' namespaces, a shared definition named like one of
    -- theirs is written with the module's name)
    effC = (ctx VParam) { pcShadow = S.fromList (runNames ++ map sbName sbs) }
    runNames = words "Val St rd Eff init n fp U C fpOf uOf cOf eff step seg run toState ss ssOf \
                     \indep Comm CommIv Upto All Moves covered touches free owner upd link final comms"

    runSteps3 =
        [ "", "/-- The step at position `k` of the schedule's order. -/"
        , "noncomputable def Run.eff (i : Inputs) (k : Nat) : Run.Eff :="
        , "  { R := (Run.fpOf k).1, W := (Run.fpOf k).2.1, O := (Run.fpOf k).2.2, U := Run.uOf i k, C := Run.cOf i k }"
        , ""
        , "noncomputable def Run.step (i : Inputs) (k : Nat) (s : Run.St) : Run.St := (Run.eff i k).apply s"
        , ""
        , "/-- Run the `k` steps of the schedule's order from position `a`. -/"
        , "noncomputable def Run.seg (i : Inputs) : Nat → Nat → Run.St → Run.St"
        , "  | _, 0, s => s"
        , "  | a, k + 1, s => Run.seg i (a + 1) k (Run.step i a s)"
        , ""
        , "/-- Run the steps at the positions listed, in that order. -/"
        , "noncomputable def Run.run (i : Inputs) : List Nat → Run.St → Run.St"
        , "  | [], s => s"
        , "  | k :: l, s => Run.run i l (Run.step i k s)" ]

    ordersDefs3 =
        [ "", "/-- An order other than the schedule's: the schedule's, except that of the steps at"
        , "    positions `f` to `to - 1`, those `M` lists run after the others.  The exported"
        , "    method `moved` (`CF.methods`) runs after `other` in it. -/"
        , "structure CF.Order where"
        , "  moved : Nat"
        , "  other : Nat"
        , "  f : Nat"
        , "  to : Nat"
        , "  M : List Nat"
        , "  deriving DecidableEq"
        , ""
        , "/-- One cycle, one rule at a time, in an order. -/"
        , "noncomputable def CF.Order.run (i : Inputs) (o : CF.Order) : Run.St :="
        , "  Run.seg i o.to (Run.n - o.to)"
        , "    (Run.run i ((List.range' o.f (o.to - o.f)).filter (fun y => !o.M.contains y)"
        , "                ++ (List.range' o.f (o.to - o.f)).filter (fun y => o.M.contains y))"
        , "      (Run.seg i 0 o.f Run.init))"
        , ""
        , "/-- The methods of the pairs the module exports as conflict-free, and the positions of"
        , "    their steps (`CF.stepsOf`). -/"
        , "def CF.methods : List String :=" ]
        ++ map ("  " ++) (bracketItems (map show cfMethods))
        ++ [ "", "/-- The positions of each method's steps (in the order of `CF.methods`). -/"
           , "def CF.stepsOf (m : Nat) : List Nat :=" ]
        ++ map ("  " ++) (natTree "m" 0 (length cfMethods) (\k -> numList (methodPositions (cfMethodArr IM.! k))) "[]")
        ++ unpackDefs
        ++ [ "", "/-- For each pair the module exports as conflict-free, the order that runs one of its"
           , "    methods first (the one whose steps come later in the schedule's order)"
             ++ (if length blocks > 64 then ", packed in numbers (`CF.ordersOf`). -/" else ". -/") ]
        ++ ordersList
        ++ [ "", "/-- An order runs all the steps of `other` before any of `moved`'s. -/"
           , "def CF.Order.wf (o : CF.Order) : Bool :="
           , "  o.M.all (fun p => o.f ≤ p && p < o.to)"
           , "  && (CF.stepsOf o.other).all (fun q => q < o.to && !o.M.contains q)"
           , "  && (CF.stepsOf o.moved).all (fun p => o.M.contains p || o.to ≤ p)" ]
    methodPositions m = mapMaybe (`M.lookup` posOfStep) (methodSteps d m)
    -- A long list of orders is packed into numbers, which Lean elaborates at
    -- once (a list literal costs it time for each element): each order is
    -- the base-2^16 digits moved, other, f, to, the length of M, then M,
    -- lowest first, each plus one, so that a number ends where its digits
    -- run out.
    ordersList
        | length blocks <= 64 =
            "def CF.orders : List CF.Order :=" : map ("  " ++) (bracketItems [ orderLit o b | (o, b) <- blocks ])
        | otherwise =
            concat [ ["def " ++ chunkName k ++ " : List CF.Order :="
                     , "  CF.ordersOf " ++ show (length ch) ++ " (CF.digits " ++ show (orderDigits ch)
                       ++ " " ++ show (packOrders ch) ++ ")"]
                   | (k, ch) <- zip [0 :: Int ..] orderChunks ]
            ++ [ "def CF.orders : List CF.Order :=" ]
            ++ map ("  " ++) (wrapJoin 96 " " (words (appendTree chunkNames)))
    -- (each chunk is checked apart: one check of a long list recurses as deep)
    orderChunks = if length blocks <= 64 then [blocks] else chunksOf 64 blocks
    chunkName k = "CF.orders" ++ show k
    chunkNames = [ chunkName k | (k, _) <- zip [0 :: Int ..] orderChunks ]
    -- the chunks' concatenation as a balanced tree, and the proof that every
    -- order of it passes its check by the same tree
    halves xs = splitAt (length xs `div` 2) xs
    appendTree xs = case xs of
        [x] -> x
        _   -> let (a, b) = halves xs in "(" ++ appendTree a ++ " ++ " ++ appendTree b ++ ")"
    checksTree xs = case xs of
        [x] -> x ++ ".checks"
        _   -> let (a, b) = halves xs in "(List.forall_mem_append.mpr ⟨" ++ checksTree a ++ ", " ++ checksTree b ++ "⟩)"
    packOrders ch = F.foldr (\dg acc -> toInteger dg + 1 + 65536 * acc) (0 :: Integer) (concatMap orderDigitList ch)
    orderDigits ch = sum (map (length . orderDigitList) ch)
    orderDigitList (o, (f, t, ms)) = [ cfMethodIdx M.! snd (orMoved o), cfMethodIdx M.! fst (orMoved o), f, t, length ms ] ++ ms
    unpackDefs
        | length blocks <= 64 = []
        | otherwise =
            [ "", "/-- The base-2^16 digits of a number, lowest first, each less one (at most `k` of them). -/"
            , "def CF.digits : Nat → Nat → List Nat"
            , "  | 0, _ => []"
            , "  | k + 1, x => if x = 0 then [] else (x % 65536 - 1) :: CF.digits k (x / 65536)"
            , ""
            , "/-- The orders a list of digits lists (at most `k` of them): for each, `moved`, `other`,"
            , "    `f`, `to`, the length of `M`, then `M`. -/"
            , "def CF.ordersOf : Nat → List Nat → List CF.Order"
            , "  | k + 1, moved :: other :: f :: to :: len :: rest =>"
            , "    ⟨moved, other, f, to, rest.take len⟩ :: CF.ordersOf k (rest.drop len)"
            , "  | _, _ => []" ]
    orderLit o (f, t, ms) = "⟨" ++ show (cfMethodIdx M.! snd (orMoved o)) ++ ", " ++ show (cfMethodIdx M.! fst (orMoved o))
                            ++ ", " ++ show f ++ ", " ++ show t ++ ", " ++ numList ms ++ "⟩"
    -- items of a list literal, as many to a line as fit
    bracketItems xs = case wrapJoin 90 ", " xs of
        []       -> ["[]"]
        [l]      -> ["[" ++ l ++ "]"]
        (l : ls) -> let ws = ("[" ++ l) : map (" " ++) ls
                    in map (++ ",") (init ws) ++ [last ws ++ "]"]

    -- a binary tree of `if`s over a variable's value from lo to hi - 1
    -- (a subtree whose leaves are all the same is that leaf)
    natTree v lo hi leaf dflt = go lo hi True
      where
        leaves = IM.fromList [ (k, leaf k) | k <- [lo .. hi - 1] ]
        same a b = all (== leaves IM.! a) [ leaves IM.! k | k <- [a + 1 .. b - 1] ]
        go a b top
            | top = ("if " ++ v ++ " < " ++ show hi ++ " then") : map ("  " ++) (go a b False) ++ ["else " ++ dflt]
            | b - a <= 1 || same a b = [leaves IM.! a]
            | otherwise = let m = (a + b) `div` 2
                          in ["if " ++ v ++ " < " ++ show m ++ " then"] ++ map ("  " ++) (go a m False)
                             ++ ["else"] ++ map ("  " ++) (go m b False)

    -- Section 5: the lemmas and the theorem -----------------------------------
    runSection5 =
        [ "", "/-! ### Other orders", ""
        , "Every order of `CF.orders` agrees with the hardware too: its run is the schedule's"
        , "(`CF.Order.run_eq`) because each step it moves commutes with each later step of its"
        , "range that stays, and the schedule's run is the field-by-field execution of section 3"
        , "(`Run.link`, `Run.final`).  First, the generic lemmas. -/" ]
        ++ runLemmas
        ++ ssDefs
        ++ ssLemmas
        ++ semComms
        ++ ownerDefs
        ++ ivDefs
        ++ linkThm
        ++ finalThm
        ++ checksThm
        ++ exportedThm

    -- the schedule's state before each position, by the versions of section 3
    ssDefs =
        [ "", "/-! The schedule's state before each position, by the definitions of section 3: each"
        , "step sets the fields it writes to their versions once it ran. -/" ]
        ++ concat [ defLines ("noncomputable def " ++ qual ["Run", "ss", sbName sb] ++ " (i : Inputs) : List Run.Val :=")
                             (bracketItems [ "." ++ valCon (fieldSortN g) ++ " " ++ ppT seqC 5 t
                                           | (g, t) <- zip (map fst (sbW sb) ++ map fst (sbO sb)) (sbAfter sb) ])
                  | sb <- sbs ]
        ++ ["noncomputable def Run.ssOf (i : Inputs) (k : Nat) : List Run.Val :="]
        ++ map ("  " ++) (natTree "k" 0 nSteps (\k -> qual ["Run", "ss", sbName (sbArr IM.! k)] ++ " i") "[]")
        ++ [ "noncomputable def Run.ss (i : Inputs) : Nat → Run.St"
           , "  | 0 => Run.init"
           , "  | k + 1 => Run.upd (Run.ss i k) (((Run.fpOf k).2.1 ++ (Run.fpOf k).2.2).zip (Run.ssOf i k))" ]

    linkThm =
        [ "", "/-- The schedule's run is the field-by-field execution of section 3, step by step. -/"
        , "theorem Run.link (i : Inputs) : Run.seg i 0 Run.n Run.init = Run.ss i Run.n :="
        , "  Run.seg_ss i Run.n" ]
        ++ map ("    " ++) (anonLines (
               [ "Run.Eff.apply_upd (Run.eff i " ++ show k ++ ") (Run.ss i " ++ show k ++ ") ("
                 ++ qual ["Run", "ss", sbName sb] ++ " i) rfl " ++ anon (replicate (length (sbW sb) + length (sbO sb)) "rfl" ++ ["trivial"])
               | (k, sb) <- zip [0 :: Int ..] sbs ] ++ ["trivial"]))
    -- (a proof of `Run.All P []`, which is `True`, is `trivial`)
    anon xs = if xs == ["trivial"] then "trivial" else "⟨" ++ intercalate ", " xs ++ "⟩"
    anonLines xs | xs == ["trivial"] = ["trivial"]
    anonLines xs = case wrapJoin 90 ", " xs of
        []       -> ["⟨⟩"]
        [l]      -> ["⟨" ++ l ++ "⟩"]
        (l : ls) -> let ws = ("⟨" ++ l) : map (" " ++) ls
                    in map (++ ",") (init ws) ++ [last ws ++ "⟩"]

    -- each field at the end: the version its last writer left (or the start's)
    lastWriter = IM.fromList [ (g, p) | (p, sb) <- zip [0 :: Int ..] sbs, g <- map fst (sbW sb) ++ map fst (sbO sb) ]
    finalThm =
        [ "", "/-- Each field's last writer, plus one (0 if none writes it). -/"
        , "def Run.lastw (j : Nat) : Nat :=" ]
        ++ map ("  " ++) (natTree "j" 0 nFields (\k -> show (maybe 0 (+ 1) (IM.lookup k lastWriter))) "0")
        ++ [ "", "/-- At the end of the cycle the schedule's state is `stepSequential`: no step after the"
           , "    last that writes a field writes it again. -/"
           , "theorem Run.final (i : Inputs) : Run.toState (Run.ss i Run.n) = stepSequential i := by"
           , "  have : Run.ss i Run.n = fun j => Run.ss i (Run.lastw j) j :="
           , "    funext (Run.ss_last i Run.lastw"
           , "      (fun j => by by_cases hj : j < " ++ show nFields
           , "                   · exact (by decide +kernel : ∀ j < " ++ show nFields ++ ", Run.lastw j ≤ Run.n) j hj"
           , "                   · simp [Run.lastw, hj])"
           , "      (fun m => by by_cases hm : m < Run.n"
           , "                   · exact (by decide +kernel : ∀ m < Run.n, ((Run.fpOf m).2.1 ++ (Run.fpOf m).2.2).all"
           , "                       (fun j => m + 1 ≤ Run.lastw j) = true) m hm"
           , "                   · simp only [Run.n] at hm; simp [Run.fpOf, hm]))"
           , "  rw [this]; rfl" ]

    ownerDefs =
        [ "", "/-- For each field, the one step that reads, sets or flags it, if only one does. -/"
        , "def Run.owner (k : Nat) : Option Nat :=" ]
        ++ map ("  " ++) (natTree "k" 0 nFields (\f -> maybe "none" (\p -> "some " ++ show p) (ownerOf f)) "none")
        ++ [ "", "theorem Run.owners_ok : ∀ k, (Run.touches k).all (fun f => Run.owner f == none || Run.owner f == some k) = true := by"
           , "  intro k"
           , "  by_cases hk : k < Run.n"
           , "  · exact (by decide +kernel : ∀ k < Run.n, (Run.touches k).all"
           , "      (fun f => Run.owner f == none || Run.owner f == some k) = true) k hk"
           , "  · simp only [Run.n] at hk"
           , "    simp [Run.touches, Run.fpOf, hk]" ]

    ivDefs =
        [ "", "/-- For each step an order moves (other than a free one), the ranges of later steps it"
        , "    commutes with. -/"
        , "def CF.iv : List (Nat × Nat × Nat) :=" ]
        ++ map ("  " ++) (bracketItems [ "(" ++ show x ++ ", " ++ show y0 ++ ", " ++ show (last run + 1) ++ ")"
                                       | (x, run@(y0 : _)) <- intervals ])
        ++ [ "", "theorem CF.iv_ok (i : Inputs)" ++ hArg ++ " : Run.All (Run.CommIv i) CF.iv :=" ]
        ++ map ("  " ++) (anonLines (map ivProof intervals ++ ["trivial"]))
        ++ [ "", "/-- The same, by step. -/"
           , "def CF.ivOf (x : Nat) : List (Nat × Nat) :=" ]
        ++ map ("  " ++) (natTree "x" 0 nSteps (\x -> "[" ++ intercalate ", " [ "(" ++ show y0 ++ ", " ++ show (last run + 1) ++ ")"
                                                                        | (x', run@(y0 : _)) <- intervals, x' == x ] ++ "]")
                                  "[]")
        ++ [ "", "theorem CF.ivOf_ok (i : Inputs)" ++ hArg ++ " : ∀ x, Run.All (fun t => Run.CommIv i (x, t.1, t.2)) (CF.ivOf x) :="
           , "  Run.ivOf_sound i CF.iv (CF.iv_ok i" ++ hUse ++ ") CF.ivOf (by decide +kernel)"
           , "    (fun x hx => by simp only [Run.n] at hx ⊢; simp [CF.ivOf, hx])" ]
    -- an interval's proof: its pieces, joined
    ivProof (x, run) =
        let pieces = pieceRuns run
            pf (a, b, False) = "Run.commIv_of i " ++ show x ++ " " ++ show a ++ " " ++ show b ++ " (by decide +kernel)"
            pf (a, _, True)  = "Run.commIv_one i " ++ show x ++ " " ++ show a ++ " (" ++ commName x a ++ " i" ++ hUse ++ ")"
            go acc lo [] = (acc, lo)
            go acc lo (p@(_, b, _) : ps) = go ("(Run.commIv_join i " ++ show x ++ " " ++ show lo ++ " " ++ show (pStart p)
                                               ++ " " ++ show b ++ " " ++ acc ++ " (" ++ pf p ++ "))") lo ps
            pStart (a, _, _) = a
        in case pieces of
               (p0@(a0, _, _) : ps) -> fst (go ("(" ++ pf p0 ++ ")") a0 ps)
               [] -> "trivial"
      where
        -- maximal runs the check decides, and single steps that need a proof
        pieceRuns ys = case ys of
            [] -> []
            (y : rest)
                | commutesFree x y -> let (r, rest') = span (commutesFree x) rest
                                      in (y, y + 1 + length r, False) : pieceRuns rest'
                | otherwise -> (y, y + 1, True) : pieceRuns rest
    commName x y = qual ["Comm", sbName (sbArr IM.! x), sbName (sbArr IM.! y)]

    checksThm =
        [ "", "/-- The steps that are free (`Run.free`), as a table. -/"
        , "def Run.isFree (x : Nat) : Bool :=" ]
        ++ map ("  " ++) (natTree "x" 0 nSteps (\x -> if isFree x then "true" else "false") "false")
        ++ [ "", "theorem Run.isFree_ok (x : Nat) (h : Run.isFree x = true) : Run.free Run.owner x = true := by"
           , "  by_cases hx : x < Run.n"
           , "  · exact (by decide +kernel : ∀ x < Run.n, Run.isFree x = true → Run.free Run.owner x = true) x hx h"
           , "  · simp only [Run.n] at hx; simp [Run.isFree, hx] at h"
           , "", "/-- An order is well formed, runs all of one method's steps before the other's, and"
           , "    moves only steps that commute with the later steps of its range that stay. -/"
           , "def CF.Order.check (o : CF.Order) : Bool :="
           , "  decide (o.f ≤ o.to) && decide (o.to ≤ Run.n) && o.ok Run.isFree CF.ivOf && o.wf" ]
        ++ (if length orderChunks == 1
              then [ "", "theorem CF.checks : ∀ o ∈ CF.orders, o.check = true := by decide +kernel" ]
              else concat [ [ "theorem " ++ chunkName k ++ ".checks : ∀ o ∈ " ++ chunkName k ++ ", o.check = true := by decide +kernel" ]
                          | (k, _) <- zip [0 :: Int ..] orderChunks ]
                   ++ [ "", "theorem CF.checks : ∀ o ∈ CF.orders, o.check = true :=" ]
                   ++ map ("  " ++) (wrapJoin 96 " " (words (checksTree chunkNames))))

    exportedThm =
        [ "", "/-- **The orders a parent may choose.**  For each pair of methods the module exports as"
        , "    conflict-free, the order that runs either one first agrees with the hardware too"
          ++ (if null (pfTaskPorts pf) then ". -/" else ",")
        ] ++ [ "    its system-task calls included, each at its position in that order. -/" | not (null (pfTaskPorts pf)) ] ++
        [ "theorem exported_CF (i : Inputs)" ++ hArg ++ " :"
        , "    ∀ o ∈ CF.orders, Agree i (Run.toState (o.run i)) (stepScheduled i) = true := by"
        , "  intro o ho"
        , "  have hc := CF.checks o ho"
        , "  simp only [CF.Order.check, Bool.and_eq_true, decide_eq_true_eq] at hc"
        , "  obtain ⟨⟨⟨h1, h2⟩, h3⟩, -⟩ := hc"
        , "  rw [CF.Order.run_eq i _ (fun x y hx hxy => Run.free_comm i Run.owner Run.owners_ok x y (Run.isFree_ok x hx) hxy)"
        , "    CF.ivOf (CF.ivOf_ok i" ++ hUse ++ ") o h1 h2 h3,"
        , "    Run.link i, Run.final i]"
        , "  exact " ++ (if null (pfMuxes pf) then "schedule_sound i" ++ hUse else "(schedule_sound i" ++ hUse ++ ").1") ]
        ++ (if length blocks > 32 then
              [ "", "/-! `exported_<a>_CF_<b>`, the order of `CF.orders` that runs `<a>` and `<b>` either way,"
              , "is `exported_CF i h o (by decide)` for its entry `o`. -/" ]
            else concat
              [ [ "", "/-- The module exports `" ++ a ++ " CF " ++ b ++ "`: a parent may call `" ++ x
                  ++ "` before `" ++ y ++ "`. -/"
                , "theorem " ++ q ("exported_" ++ a ++ "_CF_" ++ b) ++ " (i : Inputs)" ++ hArg ++ " :"
                , "    Agree i (Run.toState (CF.Order.run i " ++ orderLit o blk ++ ")) (stepScheduled i) = true :="
                , "  exported_CF i" ++ hUse ++ " _ (by decide)" ]
              | (o, blk) <- blocks, let (a, b) = orPair o, let (x, y) = orMoved o ])

    -- the semantic commutations: for each pair, each field either step
    -- writes, as either order leaves it, proved equal by `bv_decide`
    semFacts = [ ((x, y), [ (k, xy k, yx k) | k <- nubOrd (writes y ++ writes x) ])
               | (x, y) <- semPairs
               , let xy = after (sbArr IM.! y) (after (sbArr IM.! x) base)
                     yx = after (sbArr IM.! x) (after (sbArr IM.! y) base) ]
    writes p = let sb = sbArr IM.! p in map fst (sbW sb) ++ map fst (sbO sb)
    base k = TFld k (fieldSortN k)
    -- a field once a step ran on a state (each field's value as a term), as
    -- 'Run.Eff.val' computes it, without simplifying.  Given the step and the
    -- state, it indexes the step's reads and writes once for all the fields.
    after sb st = \k ->
        let v1 = maybe (st k) sub (IM.lookup k ws)
        in maybe v1 (\c -> mkOr (tsAt 1 (sub c) (tsAt 0 v1 tsEmpty))) (IM.lookup k os)
      where
        reads = IM.fromList (zip [0 ..] (map st (sbR sb)))
        ws = IM.fromList (sbW sb)
        os = IM.fromList (sbO sb)
        view = fmap (\(vf, idxs) -> (vf, [ if gated then rawAnd (sbFire sb) (reads IM.! j) else reads IM.! j
                                         | (j, gated) <- idxs ])) (sbView sb)
        sub = substRaw $ \t -> case t of
            TRd j _ -> Just (reads IM.! j)
            TView f | Just (vf, as) <- view -> Just (TVF vf as f)
            TRef x True | Just (vf, as) <- view -> Just (TAtVF x vf as)
            _ -> Nothing
    -- The facts left to prove (those whose two sides differ).  Each pair's
    -- are a definition, a balanced tree of conjunctions, and the pairs' are
    -- gathered by a balanced tree of definitions, so that a pair reaches its
    -- facts through logarithmically many unfoldings of small definitions.
    hardFacts = [ (pr, [ f | f@(_, l, r) <- fs, l /= r ]) | (pr, fs) <- semFacts ]
    provedPairs = [ (pr, fs) | (pr, fs) <- hardFacts, not (null fs) ]
    nProved = length provedPairs
    provedIdx = M.fromList (zip (map fst provedPairs) [0 :: Int ..])
    provedArr = IM.fromList (zip [0 ..] (map fst provedPairs))
    semName (x, y) = "Run.sem_" ++ show x ++ "_" ++ show y
    -- the definition gathering the pairs [a, b) of 'provedPairs'
    semNode a b
        | b - a == 1 = semName (provedArr IM.! a)
        | otherwise = "Run.sems_" ++ show a ++ "_" ++ show b
    -- the tree's inner definitions, each after its two halves
    semNodes a b
        | b - a <= 1 = []
        | otherwise = let h = a + (b - a) `div` 2 in semNodes a h ++ semNodes h b ++ [(a, h, b)]
    semDefs =
        concat [ [ "", "/-- Steps " ++ show x ++ " and " ++ show y ++ " leave each field both write as either order"
                     ++ " would. -/"
                 , "def " ++ semName pr ++ " (i : Inputs) (s : Run.St) : Prop :=" ]
                 ++ map ("  " ++) (wrapJoin 96 " " (words (balanced [ eqStmt l r | (_, l, r) <- fs ])))
               | (pr@(x, y), fs) <- provedPairs ]
        ++ (if nProved < 2 then [] else
              [ "", "/-! The pairs' facts together, as a balanced tree. -/", "" ]
              ++ [ "def " ++ semNode a b ++ " (i : Inputs) (s : Run.St) : Prop := "
                   ++ semNode a h ++ " i s ∧ " ++ semNode h b ++ " i s"
                 | (a, h, b) <- semNodes 0 nProved ])
    semNames = [ semName pr | (pr, _) <- provedPairs ] ++ [ semNode a b | (a, _, b) <- semNodes 0 nProved ]
    eqStmt l r = ppT hwC 4 l ++ " = " ++ ppT hwC 4 r
    balanced xs = case xs of
        []  -> "True"
        [x] -> x
        _   -> let (a', b') = splitAt (length xs `div` 2) xs in "(" ++ balanced a' ++ ") ∧ (" ++ balanced b' ++ ")"
    pathOf :: Int -> Int -> [Int]
    pathOf j m = if m <= 1 then [] else let h = m `div` 2 in if j < h then 1 : pathOf j h else 2 : pathOf (j - h) (m - h)
    projs = concatMap (\b' -> "." ++ show b')
    semComms
        | not hasSem = []
        | otherwise =
            (if null provedPairs then [] else
                semDefs
                ++ [ "", "/-- Steps that write the same fields commute too: each field ends the same in either"
                   , "    order. -/"
                   , "theorem Run.comms (i : Inputs)" ++ hArg ++ " (s : Run.St) : " ++ semNode 0 nProved ++ " i s := by" ]
                ++ commsProof)
            ++ concat
               [ [ "", "theorem " ++ commName x y ++ " (i : Inputs)" ++ hArg ++ " : Run.Comm i " ++ show x ++ " " ++ show y ++ " :="
                 , "  Run.comm_at i " ++ show x ++ " " ++ show y ++ " " ++ numList [ k | (k, _, _) <- fs ] ++ " (by decide +kernel)" ]
                 ++ (if M.null hardPos
                       then [ "    (fun s => " ++ anon (map (const "rfl") fs ++ ["trivial"]) ++ ")" ]
                       else [ "    (fun s => by"
                            , "      have c := (Run.comms i" ++ hUse ++ " s)" ++ projs (pathOf (provedIdx M.! (x, y)) nProved)
                              ++ "; unfold " ++ semName (x, y) ++ " at c"
                            , "      exact " ++ anon (map (fieldPf hardPos) fs ++ ["trivial"]) ++ ")" ])
               | ((x, y), fs) <- semFacts
               , let hardPos = M.fromList (zip [ k | (k, l, r) <- fs, l /= r ] [0 :: Int ..]) ]
      where
        -- a field both orders leave alike, or its fact, by its path in the pair's tree
        fieldPf hardPos (k, l, r)
            | l == r = "rfl"
            | otherwise = "congrArg Run.Val." ++ valCon (fieldSortN k) ++ " c" ++ projs (pathOf (hardPos M.! k) (M.size hardPos))
    -- the proof of 'Run.comms': the hypotheses and the equations they and the
    -- facts reach, then `bv_decide`
    commsProof =
        (if null hyps then [] else
            [ "  obtain ⟨" ++ intercalate ", " [ var | (_, var, _, _) <- hyps ] ++ "⟩ := h" ]
            ++ concat [ bracket ", " ("  obtain ⟨", "⟩ := " ++ var) (map q [ n | (n, _, _) <- fs ])
                      | (_, var, _, fs) <- hyps ])
        ++ haveBlocks (map eqHave commInsts)
        ++ (if null viewCongs then [] else
              "  -- a view function gives equal values at arguments equal in value" : map ("  " ++) viewCongs)
        ++ bracket ", " ("  dsimp only [", "]") semNames
        ++ [ "  " ++ bvDecide ]
    (commInsts, _, _) = closure S.empty S.empty (hypRoots ++ [ (RInline, t) | (_, fs) <- provedPairs, (_, l, r) <- fs, t <- [l, r] ])
    -- Functional consistency of the view functions, proved: `bv_decide`
    -- takes each application of one as a value of its own, and in the two
    -- orders of a pair a step's view is applied to the versions of the calls
    -- it sees as each order leaves them, written differently but often equal
    -- (always, when the step does not fire, for the uses that make its reads
    -- arbitrary).  For each two applications of a view function in one
    -- pair's facts, and each field both are read at (in the facts, or in the
    -- definitions whose equations the proof has at them): equal arguments,
    -- equal fields.
    viewCongs =
        [ "have : " ++ concatMap (\(a, b) -> ppT hwC 4 a ++ " = " ++ ppT hwC 4 b ++ " → ") diffs
          ++ "(i." ++ inPath v ++ concatMap ((' ' :) . ppT hwC 5) as1 ++ ")." ++ viewPath g ++ " = (i." ++ inPath v
          ++ concatMap ((' ' :) . ppT hwC 5) as2 ++ ")." ++ viewPath g ++ " :="
          ++ (if null diffs then " rfl" else "\n    fun " ++ unwords hs ++ " => congrArg (fun w : View => w." ++ viewPath g ++ ") ("
                                            ++ F.foldl' (\acc p -> "congr (" ++ acc ++ ") " ++ p) ("congrArg i." ++ inPath v ++ " " ++ head' prfs) (drop 1 prfs)
                                            ++ ")")
        | ((v, as1), (_, as2)) <- S.toList appPairs
        , let apps = M.findWithDefault M.empty v vfApps
              gs1 = M.findWithDefault S.empty as1 apps
              gs2 = M.findWithDefault S.empty as2 apps
        , length as1 == length as2
        , let pairs = zip as1 as2
              diffs = [ (a, b) | (a, b) <- pairs, a /= b ]
              hs = [ "h" ++ show j | (j, _) <- zip [1 :: Int ..] diffs ]
              -- (each position's proof: its premise's, numbered as the
              -- premises are, or rfl where the arguments are the same)
              prfs = [ if a == b then "rfl" else "h" ++ show j
                     | ((a, b), j) <- zip pairs (scanl (\n (a', b') -> if a' /= b' then n + 1 else n) (1 :: Int) pairs) ]
        , g <- S.toList (S.intersection gs1 gs2) ]
      where head' xs = case xs of { (x : _) -> x; [] -> "rfl" }
    -- the applications of each view function the proof of `Run.comms` meets,
    -- each with the fields read at it: in the facts, and in the bodies of the
    -- definitions whose equations it has at such an application
    vfApps = M.fromListWith (M.unionWith S.union)
        ([ (v, M.singleton as (S.singleton g)) | (_, fs) <- provedPairs, (_, l, r) <- fs, (v, as, g) <- vfAtoms l ++ vfAtoms r ]
         ++ [ (v, M.singleton as (S.fromList [ g | TView g <- universeT b ]))
            | IDef n (Just (VKFun v as)) <- commInsts, Just b <- [ML.lookup (DShared n) expBodies] ])
    vfAtoms t = [ (v, as, g) | TVF v as g <- universeT t ]
    -- the pairs of applications of one view function in one pair's facts
    appPairs = S.fromList
        [ (a1, a2)
        | (_, fs) <- provedPairs
        , let apps = S.toList (S.fromList [ a | (_, l, r) <- fs, t <- universeT l ++ universeT r
                                              , a <- case t of { TVF v as _ -> [(v, as)]; TAtVF _ v as -> [(v, as)]; _ -> [] } ])
        , (a1, k) <- zip apps [1 :: Int ..], a2 <- drop k apps, fst a1 == fst a2 ]
    runLemmas =
        [ "theorem Run.lookup_zip_none {α : Type} (l : List Nat) (vs : List α) (k : Nat) (h : l.contains k = false) :"
        , "    (l.zip vs).lookup k = none := by"
        , "  induction l generalizing vs with"
        , "  | nil => rfl"
        , "  | cons a l ih =>"
        , "    cases vs with"
        , "    | nil => rfl"
        , "    | cons v vs =>"
        , "      simp only [List.contains_cons, Bool.or_eq_false_iff] at h"
        , "      simp only [List.zip_cons_cons, List.lookup_cons]"
        , "      rw [h.1]; exact ih vs h.2"
        , ""
        , "theorem Run.lookup_zip_mem {α : Type} (l : List Nat) (vs : List α) (k : Nat) (v : α)"
        , "    (h : (l.zip vs).lookup k = some v) : l.contains k = true := by"
        , "  cases hc : l.contains k"
        , "  · rw [Run.lookup_zip_none l vs k hc] at h; cases h"
        , "  · rfl"
        , ""
        , "theorem Run.Eff.apply_other (e : Run.Eff) (s : Run.St) (k : Nat) (hw : e.W.contains k = false)"
        , "    (ho : e.O.contains k = false) : e.apply s k = s k := by"
        , "  simp only [Run.Eff.apply, Run.Eff.val, Run.lookup_zip_none _ _ k hw, Run.lookup_zip_none _ _ k ho]"
        , ""
        , "theorem Run.not_contains_of {l l' : List Nat} {r : Nat} (hr : r ∈ l)"
        , "    (h : ∀ w ∈ l', l.contains w = false) : l'.contains r = false := by"
        , "  cases e : l'.contains r"
        , "  · rfl"
        , "  · have := h r (List.contains_iff_mem.mp e)"
        , "    rw [List.contains_iff_mem.mpr hr] at this; cases this"
        , ""
        , "/-- Neither step reads or sets a field the other sets or flags, but both may flag one. -/"
        , "def Run.indep (x y : List Nat × List Nat × List Nat) : Bool :="
        , "  x.2.1.all (fun w => !y.1.contains w && !y.2.1.contains w && !y.2.2.contains w) &&"
        , "  y.2.1.all (fun w => !x.1.contains w && !x.2.1.contains w && !x.2.2.contains w) &&"
        , "  x.2.2.all (fun o => !y.1.contains o) && y.2.2.all (fun o => !x.1.contains o)"
        , ""
        , "/-- Independent steps commute: flags only accumulate. -/"
        , "theorem Run.Eff.comm (x y : Run.Eff) (h : Run.indep (x.R, x.W, x.O) (y.R, y.W, y.O) = true) (s : Run.St) :"
        , "    x.apply (y.apply s) = y.apply (x.apply s) := by"
        , "  simp only [Run.indep, Bool.and_eq_true, List.all_eq_true, Bool.not_eq_true'] at h"
        , "  obtain ⟨⟨⟨hxw, hyw⟩, hxo⟩, hyo⟩ := h"
        , "  have rx : x.R.map (y.apply s) = x.R.map s := List.map_congr_left (fun r hr =>"
        , "    Run.Eff.apply_other y s r (Run.not_contains_of hr (fun w hw => (hyw w hw).1.1))"
        , "      (Run.not_contains_of hr (fun w hw => hyo w hw)))"
        , "  have ry : y.R.map (x.apply s) = y.R.map s := List.map_congr_left (fun r hr =>"
        , "    Run.Eff.apply_other x s r (Run.not_contains_of hr (fun w hw => (hxw w hw).1.1))"
        , "      (Run.not_contains_of hr (fun w hw => hxo w hw)))"
        , "  funext k"
        , "  show x.val (x.R.map (y.apply s)) (y.apply s k) k = y.val (y.R.map (x.apply s)) (x.apply s k) k"
        , "  rw [rx, ry]"
        , "  show x.val (x.R.map s) (y.val (y.R.map s) (s k) k) k = y.val (y.R.map s) (x.val (x.R.map s) (s k) k) k"
        , "  simp only [Run.Eff.val]"
        , "  cases hLx : (x.W.zip (x.U (x.R.map s))).lookup k with"
        , "  | some v =>"
        , "    have e := hxw k (List.contains_iff_mem.mp (Run.lookup_zip_mem _ _ _ _ hLx))"
        , "    simp only [Run.lookup_zip_none _ _ k e.1.2, Run.lookup_zip_none _ _ k e.2]"
        , "  | none =>"
        , "    cases hLy : (y.W.zip (y.U (y.R.map s))).lookup k with"
        , "    | some w =>"
        , "      have e := hyw k (List.contains_iff_mem.mp (Run.lookup_zip_mem _ _ _ _ hLy))"
        , "      simp only [Run.lookup_zip_none _ _ k e.2]"
        , "    | none =>"
        , "      cases hOx : (x.O.zip (x.C (x.R.map s))).lookup k <;>"
        , "      cases hOy : (y.O.zip (y.C (y.R.map s))).lookup k <;>"
        , "      simp only [Run.Val.toBool, Bool.or_right_comm]"
        , ""
        , "/-- Two steps that agree on every field either sets or flags commute. -/"
        , "theorem Run.Eff.comm_on (x y : Run.Eff) (s : Run.St) (D : List Nat)"
        , "    (hD : (x.W ++ x.O ++ y.W ++ y.O).all (fun k => D.contains k) = true)"
        , "    (h : ∀ k ∈ D, x.apply (y.apply s) k = y.apply (x.apply s) k) :"
        , "    x.apply (y.apply s) = y.apply (x.apply s) := by"
        , "  funext k"
        , "  cases hk : D.contains k"
        , "  · have hD' := List.all_eq_true.mp hD"
        , "    have off : ∀ l : List Nat, (∀ w ∈ l, D.contains w = true) → l.contains k = false := fun l hl => by"
        , "      cases e : l.contains k"
        , "      · rfl"
        , "      · have := hl k (List.contains_iff_mem.mp e); rw [hk] at this; cases this"
        , "    have hxw := off x.W (fun w hw => hD' w (by simp [hw]))"
        , "    have hxo := off x.O (fun w hw => hD' w (by simp [hw]))"
        , "    have hyw := off y.W (fun w hw => hD' w (by simp [hw]))"
        , "    have hyo := off y.O (fun w hw => hD' w (by simp [hw]))"
        , "    rw [Run.Eff.apply_other x _ k hxw hxo, Run.Eff.apply_other y _ k hyw hyo,"
        , "      Run.Eff.apply_other y _ k hyw hyo, Run.Eff.apply_other x _ k hxw hxo]"
        , "  · exact h k (List.contains_iff_mem.mp hk)"
        , ""
        , "/-- `P` holds at `a`, `a + 1`, ..., `a + k - 1`. -/"
        , "def Run.Upto (P : Nat → Prop) : Nat → Nat → Prop"
        , "  | _, 0 => True"
        , "  | a, k + 1 => P a ∧ Run.Upto P (a + 1) k"
        , ""
        , "theorem Run.upto_mem (P : Nat → Prop) (a k : Nat) (h : Run.Upto P a k) :"
        , "    ∀ y, a ≤ y → y < a + k → P y := by"
        , "  induction k generalizing a with"
        , "  | zero => intro y h1 h2; omega"
        , "  | succ k ih =>"
        , "    intro y h1 h2"
        , "    obtain ⟨hp, hr⟩ := h"
        , "    by_cases e : y = a"
        , "    · exact e ▸ hp"
        , "    · exact ih (a + 1) hr y (by omega) (by omega)"
        , ""
        , "/-- `P` holds of every member of a list. -/"
        , "def Run.All {α : Type} (P : α → Prop) : List α → Prop"
        , "  | [] => True"
        , "  | a :: l => P a ∧ Run.All P l"
        , ""
        , "theorem Run.all_mem {α : Type} (P : α → Prop) (l : List α) (h : Run.All P l) : ∀ a ∈ l, P a := by"
        , "  induction l with"
        , "  | nil => intro a ha; cases ha"
        , "  | cons b l ih =>"
        , "    intro a ha"
        , "    rcases List.mem_cons.mp ha with rfl | ha"
        , "    · exact h.1"
        , "    · exact ih h.2 a ha"
        , ""
        , "/-- Steps `x` and `y` of the schedule's order commute. -/"
        , "def Run.Comm (i : Inputs) (x y : Nat) : Prop :="
        , "  ∀ s, Run.step i y (Run.step i x s) = Run.step i x (Run.step i y s)"
        , ""
        , "theorem Run.comm_of_indep (i : Inputs) (x y : Nat) (h : Run.indep (Run.fpOf y) (Run.fpOf x) = true) :"
        , "    Run.Comm i x y := fun s => Run.Eff.comm _ _ h s"
        , ""
        , "/-- Steps commute when each field either sets or flags ends the same in both orders. -/"
        , "theorem Run.comm_at (i : Inputs) (x y : Nat) (D : List Nat)"
        , "    (hD : ((Run.fpOf y).2.1 ++ (Run.fpOf y).2.2 ++ (Run.fpOf x).2.1 ++ (Run.fpOf x).2.2).all"
        , "      (fun k => D.contains k) = true)"
        , "    (h : ∀ s, Run.All (fun k => Run.step i y (Run.step i x s) k = Run.step i x (Run.step i y s) k) D) :"
        , "    Run.Comm i x y :="
        , "  fun s => Run.Eff.comm_on _ _ s D hD (Run.all_mem _ _ (h s))"
        , ""
        , "/-- Step `t.1` commutes with every step from position `t.2.1` to `t.2.2 - 1`. -/"
        , "def Run.CommIv (i : Inputs) (t : Nat × Nat × Nat) : Prop :="
        , "  ∀ y, t.2.1 ≤ y → y < t.2.2 → Run.Comm i t.1 y"
        , ""
        , "theorem Run.commIv_of (i : Inputs) (x a b : Nat)"
        , "    (h : (List.range' a (b - a)).all (fun y => Run.indep (Run.fpOf y) (Run.fpOf x)) = true) :"
        , "    Run.CommIv i (x, a, b) := by"
        , "  intro y h1 h2"
        , "  exact Run.comm_of_indep i x y (List.all_eq_true.mp h y (List.mem_range'_1.mpr ⟨h1, by simp at h2 ⊢; omega⟩))"
        , ""
        , "theorem Run.commIv_one (i : Inputs) (x y : Nat) (h : Run.Comm i x y) : Run.CommIv i (x, y, y + 1) := by"
        , "  intro z h1 h2"
        , "  have : z = y := by simp at h1 h2; omega"
        , "  exact this ▸ h"
        , ""
        , "theorem Run.commIv_join (i : Inputs) (x a b c : Nat) (h1 : Run.CommIv i (x, a, b)) (h2 : Run.CommIv i (x, b, c)) :"
        , "    Run.CommIv i (x, a, c) := by"
        , "  intro y ha hc"
        , "  by_cases hb : y < b"
        , "  · exact h1 y ha hb"
        , "  · exact h2 y (by simp at hb ⊢; omega) hc"
        , ""
        , "theorem Run.run_append (i : Inputs) (l₁ l₂ : List Nat) (s : Run.St) :"
        , "    Run.run i (l₁ ++ l₂) s = Run.run i l₂ (Run.run i l₁ s) := by"
        , "  induction l₁ generalizing s with"
        , "  | nil => rfl"
        , "  | cons k l ih => exact ih _"
        , ""
        , "theorem Run.seg_run (i : Inputs) (a k : Nat) (s : Run.St) :"
        , "    Run.seg i a k s = Run.run i (List.range' a k) s := by"
        , "  induction k generalizing a s with"
        , "  | zero => rfl"
        , "  | succ k ih => rw [List.range'_succ]; exact ih (a + 1) _"
        , ""
        , "theorem Run.seg_add (i : Inputs) (a k l : Nat) (s : Run.St) :"
        , "    Run.seg i a (k + l) s = Run.seg i (a + k) l (Run.seg i a k s) := by"
        , "  induction k generalizing a s with"
        , "  | zero => rw [Nat.zero_add]; rfl"
        , "  | succ k ih =>"
        , "    rw [Nat.succ_add]"
        , "    show Run.seg i (a + 1) (k + l) (Run.step i a s) = Run.seg i (a + (k + 1)) l (Run.seg i (a + 1) k (Run.step i a s))"
        , "    rw [ih, Nat.add_assoc, Nat.add_comm 1 k]"
        , ""
        , "theorem Run.move (i : Inputs) (x : Nat) (l : List Nat) (s : Run.St)"
        , "    (h : ∀ y ∈ l, Run.Comm i x y) : Run.run i (x :: l) s = Run.run i (l ++ [x]) s := by"
        , "  induction l generalizing s with"
        , "  | nil => rfl"
        , "  | cons y l ih =>"
        , "    show Run.run i l (Run.step i y (Run.step i x s)) = Run.run i (l ++ [x]) (Run.step i y s)"
        , "    rw [h y (List.mem_cons_self ..) s]"
        , "    exact ih _ (fun z hz => h z (List.mem_cons_of_mem _ hz))"
        , ""
        , "/-- In a list of positions, each one `p` selects commutes with each later one it does not. -/"
        , "def Run.Moves (i : Inputs) (p : Nat → Bool) : List Nat → Prop"
        , "  | [] => True"
        , "  | x :: l => (p x = true → ∀ y ∈ l, p y = false → Run.Comm i x y) ∧ Run.Moves i p l"
        , ""
        , "theorem Run.blocks (i : Inputs) (p : Nat → Bool) (w : List Nat) (s : Run.St) (h : Run.Moves i p w) :"
        , "    Run.run i w s = Run.run i (w.filter (fun y => !p y) ++ w.filter p) s := by"
        , "  induction w generalizing s with"
        , "  | nil => rfl"
        , "  | cons x l ih =>"
        , "    obtain ⟨hx, hl⟩ := h"
        , "    cases hp : p x"
        , "    · rw [List.filter_cons_of_pos (by simp [hp]), List.filter_cons_of_neg (by simp [hp])]"
        , "      exact ih _ hl"
        , "    · rw [List.filter_cons_of_neg (by simp [hp]), List.filter_cons_of_pos hp]"
        , "      show Run.run i l (Run.step i x s) = _"
        , "      rw [ih _ hl]"
        , "      show Run.run i (x :: (List.filter (fun y => !p y) l ++ List.filter p l)) s = _"
        , "      rw [← List.cons_append, Run.run_append, Run.move i x _ s, ← Run.run_append, List.append_assoc]"
        , "      · rfl"
        , "      · intro y hy"
        , "        have hy' := List.mem_filter.mp hy"
        , "        exact hx hp y hy'.1 (by simpa using hy'.2)"
        , ""
        , "theorem Run.moves_range (i : Inputs) (p : Nat → Bool) (f k : Nat)"
        , "    (h : ∀ x y, f ≤ x → x < y → y < f + k → p x = true → p y = false → Run.Comm i x y) :"
        , "    Run.Moves i p (List.range' f k) := by"
        , "  induction k generalizing f with"
        , "  | zero => trivial"
        , "  | succ k ih =>"
        , "    rw [List.range'_succ]"
        , "    refine ⟨fun hp y hy hq => h f y (Nat.le_refl _) ?_ ?_ hp hq, ih (f + 1) ?_⟩"
        , "    · have := List.mem_range'_1.mp hy; omega"
        , "    · have := List.mem_range'_1.mp hy; omega"
        , "    · intro x y h1 h2 h3 hp hq; exact h x y (by omega) h2 (by omega) hp hq"
        , ""
        , "/-- Some interval of step `x` covers the positions from `a` to `b - 1`. -/"
        , "def Run.covered (ivOf : Nat → List (Nat × Nat)) (x a b : Nat) : Bool :="
        , "  (ivOf x).any (fun t => t.1 ≤ a && b ≤ t.2)"
        , ""
        , "theorem Run.all_of_mem {α : Type} (P : α → Prop) (l : List α) (h : ∀ a ∈ l, P a) : Run.All P l := by"
        , "  induction l with"
        , "  | nil => trivial"
        , "  | cons b l ih => exact ⟨h b (List.mem_cons_self ..), ih (fun a ha => h a (List.mem_cons_of_mem _ ha))⟩"
        , ""
        , "/-- The intervals by step are some of a list of proved intervals. -/"
        , "theorem Run.ivOf_sound (i : Inputs) (iv : List (Nat × Nat × Nat)) (hiv : Run.All (Run.CommIv i) iv)"
        , "    (ivOf : Nat → List (Nat × Nat)) (hsub : ∀ x < Run.n, (ivOf x).all (fun t => iv.contains (x, t.1, t.2)) = true)"
        , "    (hnil : ∀ x, ¬ x < Run.n → ivOf x = []) :"
        , "    ∀ x, Run.All (fun t => Run.CommIv i (x, t.1, t.2)) (ivOf x) := by"
        , "  intro x"
        , "  by_cases hx : x < Run.n"
        , "  · exact Run.all_of_mem _ _ (fun t ht =>"
        , "      Run.all_mem _ iv hiv (x, t.1, t.2) (List.contains_iff_mem.mp (List.all_eq_true.mp (hsub x hx) t ht)))"
        , "  · rw [hnil x hx]; trivial"
        , ""
        , "theorem Run.covered_comm (i : Inputs) (ivOf : Nat → List (Nat × Nat))"
        , "    (hiv : ∀ x, Run.All (fun t => Run.CommIv i (x, t.1, t.2)) (ivOf x))"
        , "    (x a b : Nat) (hc : Run.covered ivOf x a b = true) : ∀ y, a ≤ y → y < b → Run.Comm i x y := by"
        , "  intro y h1 h2"
        , "  obtain ⟨t, ht, e⟩ := List.any_eq_true.mp hc"
        , "  simp only [Bool.and_eq_true, decide_eq_true_eq] at e"
        , "  exact Run.all_mem _ _ (hiv x) t ht y (by simp; omega) (by simp; omega)"
        , ""
        , "/-- Each step `o` moves commutes with each later step of the order's range"
        , "    that stays: a free step does (it commutes with every other), and so does"
        , "    one an interval covers, from it to the range's end, or at that step. -/"
        , "def CF.Order.ok (free : Nat → Bool) (ivOf : Nat → List (Nat × Nat)) (o : CF.Order) : Bool :="
        , "  o.M.all (fun x => free x || Run.covered ivOf x (x + 1) o.to ||"
        , "    (List.range' (x + 1) (o.to - (x + 1))).all (fun y => o.M.contains y || Run.covered ivOf x y (y + 1)))"
        , ""
        , "theorem CF.Order.moves (i : Inputs) (free : Nat → Bool) (hfree : ∀ x y, free x = true → x ≠ y → Run.Comm i x y)"
        , "    (ivOf : Nat → List (Nat × Nat)) (hiv : ∀ x, Run.All (fun t => Run.CommIv i (x, t.1, t.2)) (ivOf x))"
        , "    (o : CF.Order) (hok : o.ok free ivOf = true) :"
        , "    Run.Moves i (fun y => o.M.contains y) (List.range' o.f (o.to - o.f)) := by"
        , "  apply Run.moves_range"
        , "  intro x y h1 h2 h3 hp hq"
        , "  have hx : x ∈ o.M := List.contains_iff_mem.mp hp"
        , "  have := List.all_eq_true.mp hok x hx"
        , "  rcases Bool.or_eq_true_iff.mp this with hc | hc"
        , "  · rcases Bool.or_eq_true_iff.mp hc with hc | hc"
        , "    · exact hfree x y hc (by omega)"
        , "    · exact Run.covered_comm i ivOf hiv x (x + 1) o.to hc y h2 (by omega)"
        , "  · have hy := List.all_eq_true.mp hc y (List.mem_range'_1.mpr ⟨by omega, by omega⟩)"
        , "    rcases Bool.or_eq_true_iff.mp hy with hm | hm"
        , "    · rw [hq] at hm; cases hm"
        , "    · exact Run.covered_comm i ivOf hiv x y (y + 1) hm y (Nat.le_refl _) (Nat.lt_succ_self _)"
        , ""
        , "theorem CF.Order.run_eq (i : Inputs) (free : Nat → Bool) (hfree : ∀ x y, free x = true → x ≠ y → Run.Comm i x y)"
        , "    (ivOf : Nat → List (Nat × Nat)) (hiv : ∀ x, Run.All (fun t => Run.CommIv i (x, t.1, t.2)) (ivOf x))"
        , "    (o : CF.Order) (h1 : o.f ≤ o.to) (h2 : o.to ≤ Run.n) (hok : o.ok free ivOf = true) :"
        , "    o.run i = Run.seg i 0 Run.n Run.init := by"
        , "  have e : Run.n = o.f + (o.to - o.f) + (Run.n - o.to) := by omega"
        , "  have e2 : o.f + (o.to - o.f) = o.to := by omega"
        , "  unfold CF.Order.run"
        , "  rw [← Run.blocks i _ _ _ (o.moves i free hfree ivOf hiv hok), ← Run.seg_run]"
        , "  conv => rhs; rw [e, Run.seg_add, Run.seg_add, Nat.zero_add, Nat.zero_add, e2]"
        , ""
        , "/-- A State, by field numbers, with the fields `l` lists set. -/"
        , "def Run.upd (s : Run.St) (l : List (Nat × Run.Val)) : Run.St := fun k =>"
        , "  match l.lookup k with"
        , "  | some v => v"
        , "  | none => s k"
        , ""
        , "theorem Run.lookup_zip_some {α : Type} (l : List Nat) (vs : List α) (k : Nat) (v : α)"
        , "    (h : (l.zip vs).lookup k = some v) : (k, v) ∈ l.zip vs := by"
        , "  induction l generalizing vs with"
        , "  | nil => cases h"
        , "  | cons a l ih =>"
        , "    cases vs with"
        , "    | nil => cases h"
        , "    | cons w vs =>"
        , "      simp only [List.zip_cons_cons, List.lookup_cons] at h ⊢"
        , "      cases e : k == a"
        , "      · rw [e] at h; exact List.mem_cons_of_mem _ (ih vs h)"
        , "      · rw [e] at h; cases h; rw [beq_iff_eq.mp e]; exact List.mem_cons_self .."
        , ""
        , "theorem Run.lookup_zip_ne_none {α : Type} (l : List Nat) (vs : List α) (k : Nat)"
        , "    (hlen : vs.length = l.length) (hk : l.contains k = true) : (l.zip vs).lookup k ≠ none := by"
        , "  induction l generalizing vs with"
        , "  | nil => cases hk"
        , "  | cons a l ih =>"
        , "    cases vs with"
        , "    | nil => cases hlen"
        , "    | cons w vs =>"
        , "      simp only [List.zip_cons_cons, List.lookup_cons]"
        , "      cases e : k == a"
        , "      · simp only [List.contains_cons, e, Bool.false_or] at hk"
        , "        exact ih vs (by simpa using hlen) hk"
        , "      · simp"
        , ""
        , "theorem Run.Eff.apply_upd (e : Run.Eff) (s : Run.St) (vs : List Run.Val)"
        , "    (hlen : vs.length = (e.W ++ e.O).length) (h : Run.All (fun p => e.apply s p.1 = p.2) ((e.W ++ e.O).zip vs)) :"
        , "    e.apply s = Run.upd s ((e.W ++ e.O).zip vs) := by"
        , "  funext k"
        , "  unfold Run.upd"
        , "  cases hl : ((e.W ++ e.O).zip vs).lookup k with"
        , "  | some v => exact Run.all_mem _ _ h (k, v) (Run.lookup_zip_some _ _ k v hl)"
        , "  | none =>"
        , "    have hk : (e.W ++ e.O).contains k = false := by"
        , "      cases c : (e.W ++ e.O).contains k"
        , "      · rfl"
        , "      · exact absurd hl (Run.lookup_zip_ne_none _ _ k hlen c)"
        , "    simp only [List.contains_append, Bool.or_eq_false_iff] at hk"
        , "    exact Run.Eff.apply_other e s k hk.1 hk.2"
        , ""
        , "theorem Run.upd_other (s : Run.St) (ks : List Nat) (vs : List Run.Val) (j : Nat) (h : ks.contains j = false) :"
        , "    Run.upd s (ks.zip vs) j = s j := by"
        , "  simp only [Run.upd, Run.lookup_zip_none ks vs j h]"
        , ""
        , "/-- The fields step `k` reads, sets or flags. -/"
        , "def Run.touches (k : Nat) : List Nat := (Run.fpOf k).1 ++ (Run.fpOf k).2.1 ++ (Run.fpOf k).2.2"
        , ""
        , "/-- A step that reads nothing, flags nothing and sets only fields no other step"
        , "    touches: it commutes with every other step. -/"
        , "def Run.free (owner : Nat → Option Nat) (x : Nat) : Bool :="
        , "  (Run.fpOf x).1.isEmpty && (Run.fpOf x).2.2.isEmpty && (Run.fpOf x).2.1.all (fun f => owner f == some x)"
        , ""
        , "theorem Run.free_comm (i : Inputs) (owner : Nat → Option Nat)"
        , "    (hown : ∀ k, (Run.touches k).all (fun f => owner f == none || owner f == some k) = true)"
        , "    (x y : Nat) (hx : Run.free owner x = true) (hxy : x ≠ y) : Run.Comm i x y := by"
        , "  intro s"
        , "  apply Run.Eff.comm"
        , "  show Run.indep (Run.fpOf y) (Run.fpOf x) = true"
        , "  simp only [Run.free, Bool.and_eq_true, List.isEmpty_iff] at hx"
        , "  obtain ⟨⟨hr, ho⟩, hw⟩ := hx"
        , "  -- a field x sets is one no other step touches"
        , "  have own : ∀ f, f ∈ (Run.fpOf x).2.1 → f ∈ Run.touches y → False := by"
        , "    intro f hf hfy"
        , "    have h1 := List.all_eq_true.mp hw f hf"
        , "    have h2 := List.all_eq_true.mp (hown y) f hfy"
        , "    rw [beq_iff_eq.mp h1] at h2"
        , "    simp at h2"
        , "    exact hxy h2"
        , "  have offx : ∀ w, w ∈ Run.touches y → (Run.fpOf x).2.1.contains w = false := fun w hw' => by"
        , "    cases c : (Run.fpOf x).2.1.contains w"
        , "    · rfl"
        , "    · exact (own w (List.contains_iff_mem.mp c) hw').elim"
        , "  have offy : ∀ w, w ∈ (Run.fpOf x).2.1 → ∀ l : List Nat, (∀ v ∈ l, v ∈ Run.touches y) → l.contains w = false :="
        , "    fun w hw' l hl => by"
        , "      cases c : l.contains w"
        , "      · rfl"
        , "      · exact (own w hw' (hl w (List.contains_iff_mem.mp c))).elim"
        , "  unfold Run.indep"
        , "  simp only [Bool.and_eq_true, List.all_eq_true]"
        , "  refine ⟨⟨⟨fun w hwy => ?_, fun w hwx => ?_⟩, fun o ho' => ?_⟩, fun o ho' => ?_⟩"
        , "  · simp only [hr, ho, List.contains_nil, Bool.not_false, offx w (by simp [Run.touches, hwy])]"
        , "    exact ⟨⟨trivial, trivial⟩, trivial⟩"
        , "  · simp only [Bool.not_eq_true']"
        , "    exact ⟨⟨offy w hwx _ (fun v hv => by simp [Run.touches, hv]), offy w hwx _ (fun v hv => by simp [Run.touches, hv])⟩,"
        , "      offy w hwx _ (fun v hv => by simp [Run.touches, hv])⟩"
        , "  · simp [hr]"
        , "  · rw [ho] at ho'; cases ho'"
        ]
    ssLemmas =
        [ "theorem Run.seg_ss (i : Inputs) (n : Nat) (h : Run.Upto (fun k => Run.step i k (Run.ss i k) = Run.ss i (k + 1)) 0 n) :"
        , "    Run.seg i 0 n Run.init = Run.ss i n := by"
        , "  have : ∀ a k, a + k = n → Run.Upto (fun k => Run.step i k (Run.ss i k) = Run.ss i (k + 1)) a k →"
        , "      Run.seg i a k (Run.ss i a) = Run.ss i n := by"
        , "    intro a k"
        , "    induction k generalizing a with"
        , "    | zero => intro e _; simp at e; subst e; rfl"
        , "    | succ k ih =>"
        , "      intro e hk"
        , "      show Run.seg i (a + 1) k (Run.step i a (Run.ss i a)) = _"
        , "      rw [hk.1]; exact ih (a + 1) (by omega) hk.2"
        , "  exact this 0 n (Nat.zero_add n) h"
        , ""
        , "theorem Run.ss_frame (i : Inputs) (j a k : Nat)"
        , "    (h : (List.range' a k).all (fun m => !((Run.fpOf m).2.1 ++ (Run.fpOf m).2.2).contains j) = true) :"
        , "    Run.ss i (a + k) j = Run.ss i a j := by"
        , "  induction k with"
        , "  | zero => rfl"
        , "  | succ k ih =>"
        , "    rw [List.range'_concat, List.all_append, Bool.and_eq_true] at h"
        , "    show Run.upd (Run.ss i (a + k)) _ j = _"
        , "    rw [Run.upd_other _ _ _ j (by simpa using h.2)]"
        , "    exact ih h.1"
        , ""
        , "/-- At the end of the cycle each field is as its last writer left it: `lastw j` is one"
        , "    more than the position of the last step that sets or flags `j` (0 if none does). -/"
        , "theorem Run.ss_last (i : Inputs) (lastw : Nat → Nat) (hle : ∀ j, lastw j ≤ Run.n)"
        , "    (hw : ∀ m, ((Run.fpOf m).2.1 ++ (Run.fpOf m).2.2).all (fun j => m + 1 ≤ lastw j) = true) (j : Nat) :"
        , "    Run.ss i Run.n j = Run.ss i (lastw j) j := by"
        , "  have h := Run.ss_frame i j (lastw j) (Run.n - lastw j) (by"
        , "    apply List.all_eq_true.mpr"
        , "    intro m hm"
        , "    have hm' := (List.mem_range'_1.mp hm).1"
        , "    cases c : ((Run.fpOf m).2.1 ++ (Run.fpOf m).2.2).contains j"
        , "    · rfl"
        , "    · have := List.all_eq_true.mp (hw m) j (List.contains_iff_mem.mp c)"
        , "      simp at this; omega)"
        , "  rwa [Nat.add_sub_cancel' (hle j)] at h"
        ]
    -- 4. ---------------------------------------------------------------------
    section4 =
        [ "/-! The hardware, transcribed from the `ASPackage` bsc's back end builds:"
        , "every submodule input (enables and argument muxes) and every module output,"
        , "as functions of the cycle's inputs.  Definitions the hardware shares with the"
        , "rules, CAN_FIRE and WILL_FIRE among them, are in section 2; a signal that"
        , "needs no definition of its own, or is used only there, is written into"
        , "`stepScheduled` directly."
        ++ (if null (pfMuxes pf) then " -/" else "") ]
        ++ (if null (pfMuxes pf) then []
            else [ "A mux is read as a priority mux; for a parallel one (`PrimMux`) the theorem"
                 , "also proves its selectors one-hot (`Hw.oneHot`), so the reading is exact. -/" ])
        ++ bundle ([ CDef (hwDef l) | l <- pfHwDefs pf, '\1' `notElem` ldName l, not (isTaskPos (ldName l)), keepHw l ]
                   ++ (case [ l | l <- pfHwDefs pf, isTaskPos (ldName l), keepHw l ] of
                         [] -> []
                         ls -> CNote [ "", "-- The hardware's system-task calls, in the order of its always block: `Hw.«$tasks@k»`"
                                     , "-- is how many of the first k it makes, the k-th's position if it makes it." ]
                               : map (CDef . hwDef) ls))
        ++ [""] ++ doc ("The cycle as the hardware runs it" ++ (if null (pfTaskPorts pf) then "." else
                ".  Each call of its always block fills the fields of the sequential call it is \
                \(of the same rule, task and source position): its position, when it is made the \
                \count of the block's calls made up to it, and its arguments."))
        ++ [ "noncomputable def stepScheduled (i : Inputs) : State :="]
        ++ stateLiteral hwValue ([ (n, "the sequential run's bookkeeping, not compared") | (n, _) <- flagFields ]
                                 ++ [ (tasksField, "the count of calls made, not compared (the positions say it)")
                                    | not (null (pfTaskPorts pf)) ])
        ++ bundle oneHotDef
    keepHw l = not (inlined (DHw (ldName l)))
    isTaskPos nm = (tasksField ++ "@") `isPrefixOf` nm
    isMux nm = nm == oneHotKey || "mux_" `isPrefixOf` nm
    isTail nm = "ors\1" `isPrefixOf` nm
    hwBoolDef l = CDef (defLines ("noncomputable def " ++ hwName (ldName l) ++ " (i : Inputs) : Bool :=")
                                 (layout hwDefC 92 0 (hwBody l)))
    disjointDefs = case [ l | l <- pfHwDefs pf, '\1' `elem` ldName l, not (isMux (ldName l)), not (isTail (ldName l))
                            , keepHw l ] of
        [] -> []
        ls -> COther [ "", "/-! Conditions that facts of section 5 say are pairwise disjoint, by groups:"
                     , "`Hw.<group>.upto_k` holds when one of the group's first k conditions does. -/" ]
              : map hwBoolDef ls
    tailDefsText = case [ l | l <- pfHwDefs pf, isTail (ldName l), keepHw l ] of
        [] -> []
        ls -> COther [ "", "/-! The tails the long disjunctions of section 5's assertions share: `Hw.ors.<k>` is a"
                     , "disjunct OR'ed with the tail after it. -/" ]
              : map hwBoolDef ls
    hwDef l =
        [""]
        ++ defLines ("noncomputable def " ++ hwName (ldName l) ++ " (i : Inputs) : " ++ sortName (ldSort l) ++ " :=")
                    (layout hwDefC 92 0 (hwBody l))
    hwValue n s = maybe (zero s) (ppT hwC 0 . expand) (hwTerm n)
    hwTerm n = case M.lookup n (pfHwPorts pf) of
        Just t  -> Just t
        Nothing -> M.lookup n outputMap
    -- each output's hardware value (the first, as a search would find it)
    outputMap = M.fromListWith (\_ old -> old) [ (o, t) | (o, _, t, _) <- pfOutputs pf ]
    oneHotDef = disjointDefs ++ tailDefsText ++ case [ l | l <- pfHwDefs pf, '\1' `elem` ldName l, isMux (ldName l), keepHw l ] of
        [] -> []
        ls -> let (chains, final) = partition ((/= oneHotKey) . ldName) ls
              in (if null chains then []
                  else [ COther [ "", "/-! For each parallel mux, `Hw.mux_<m>.seen_k`: whether some selector up to the"
                                , "k-th is set (the first is the first selector itself). -/" ] ])
                 ++ map hwBoolDef chains
                 ++ [ COther ([""] ++ doc "Every parallel mux in the hardware has at most one selector set: none is \
                                          \set once an earlier one is."
                              ++ defLines "noncomputable def Hw.oneHot (i : Inputs) : Bool :=" (layout hwDefC 92 0 (hwBody l)))
                    | l <- final ]

    -- 5. ---------------------------------------------------------------------
    section5 =
        bundle ([ CDef (agreePort k) | k <- agreeShapes ] ++ [ CDef (agreeTask k) | k <- taskShapes ])
        ++ [""]
        ++ (if null (pfTaskPorts pf)
              then [ "/-- The sequential execution was legal and agrees with the hardware on every"
                   , "    submodule call and every module output (each when it is meaningful). -/" ]
              else doc "The sequential execution was legal and agrees with the hardware on every \
                       \submodule call, every module output (each when it is meaningful) and every \
                       \system-task call: its position among the calls made (0: not made) and, when \
                       \made, its arguments, so that both make the same calls in the same order.")
        ++ [ "noncomputable def Agree (i : Inputs) (s h : State) : Bool :="]
        ++ map ("  " ++) (joinOp "&&" (map pure agreeTerms))
        ++ concatMap hypStruct hyps
        ++ hypsDecl
        ++ theorem
        ++ (if modeB then runSection5 else [])
        ++ unrealizedNote
        ++ orderTheorem
    unrealizedNote = case dUnrealized d of
        [] -> []
        ps -> ["", "/-! **Not checked.**  The module also exports "
                   ++ intercalate ", " [ "`" ++ a ++ " CF " ++ b ++ "`" | (a, b) <- ps ] ++ ", but no"
              , "order of its rules runs the second method of such a pair before the first without"
              , "breaking another of bsc's scheduling constraints, so those orders are not checked here. -/" ]
    -- a port's fields: its enable, then its arguments
    portFields p = [ (en, SBool) | Just en <- [ptEnable p] ] ++ [ (a, sortOf t) | (a, t) <- ptArgs p ]
    hwPortTerms p = [ M.findWithDefault (TIn n) n (pfHwPorts pf) | (n, _) <- portFields p ]
    -- One abbreviation per shape of port (an enable or not, the arguments'
    -- sorts), in the order the ports first have each.
    portShape p = (isJust (ptEnable p), [ sortOf t | (_, t) <- ptArgs p ])
    shapeName (en, srts) = "Agree." ++ q ((if en then "call" else "args") ++ concatMap (\x -> "_" ++ sortName x) srts)
    agreeShapes = reverse (snd (F.foldl' (\(seen, acc) k -> if S.member k seen then (seen, acc)
                                                            else (S.insert k seen, k : acc))
                                         (S.empty, []) (map portShape (pfPorts pf))))
    -- a system-task call: its position (0 when not made), then its arguments
    taskFields tp = (tpPos tp, SPos) : tpArgs tp
    taskShape tp = map snd (tpArgs tp)
    taskShapes = reverse (snd (F.foldl' (\(seen, acc) k -> if S.member k seen then (seen, acc)
                                                           else (S.insert k seen, k : acc))
                                        (S.empty, []) (map taskShape (pfTaskPorts pf))))
    taskShapeName srts = "Agree." ++ q ("task" ++ concatMap (\x -> "_" ++ sortName x) srts)
    agreeTask srts =
        let params = ("p", SPos) : [ ("a" ++ show j, srt) | (j, srt) <- zip [1 :: Int ..] srts ]
            binders = unwords [ "(" ++ v ++ " : " ++ sortName srt ++ ")" | (v, srt) <- params ++ [ ("h_" ++ v, srt) | (v, srt) <- params ] ]
            argEqs = [ "a" ++ show j ++ " == h_a" ++ show j | j <- [1 .. length srts] ]
            argText = case argEqs of
                [a] -> a
                _   -> "(" ++ intercalate " && " argEqs ++ ")"
            -- (the sequential position read once, so that its definition is written in)
            body = "p == h_p" ++ (if null argEqs then "" else " && (h_p == 0 || " ++ argText ++ ")")
        in ["", "/-- Both make a system-task call at the same position (0: neither makes it)"
                ++ (if null srts then ":" else ",")
           , "    " ++ (if null srts then "" else "with the same " ++ show (length srts) ++ " argument(s): ")
                ++ "the sequential execution's fields first,"
           , "    then the hardware's. -/"]
           ++ defLines ("noncomputable abbrev " ++ taskShapeName srts ++ " " ++ binders ++ " : Bool :=") [body]
    agreePort k@(en, srts) =
        let params = [ ("en", SBool) | en ]
                     ++ [ ("a" ++ show j, srt) | (j, srt) <- zip [1 :: Int ..] srts ]
            binders = unwords [ "(" ++ v ++ " : " ++ sortName srt ++ ")" | (v, srt) <- params ++ [ ("h_" ++ v, srt) | (v, srt) <- params ] ]
            argEqs = [ "a" ++ show j ++ " == h_a" ++ show j | j <- [1 .. length srts] ]
            argText = case argEqs of
                []  -> "true"
                [a] -> a
                _   -> "(" ++ intercalate " && " argEqs ++ ")"
            body = if en then "en == h_en" ++ (if null argEqs then "" else " && (!en || " ++ argText ++ ")")
                   else argText
        in ["", "/-- Both drive a port " ++ (if en then "with an enable" else "without one")
                ++ (if null srts then "" else " and " ++ show (length srts) ++ " argument(s)")
                ++ " the same way: the sequential"
           , "    execution's fields first, then the hardware's. -/"]
           ++ defLines ("noncomputable abbrev " ++ shapeName k ++ " " ++ binders ++ " : Bool :=") [body]
    agreeTerms =
        ["!s." ++ sp "illegal_access", "!s." ++ sp "guard_violated"]
        ++ [ unwords (shapeName (portShape p) : [ "s." ++ sp n | (n, _) <- portFields p ]
                      ++ [ "h." ++ sp n | (n, _) <- portFields p ])
           | p <- pfPorts pf ]
        ++ [ case masks of
               [] -> "s." ++ sp o ++ " == h." ++ sp o
               _  -> "(" ++ intercalate " || " (map maskNeg masks) ++ " || s." ++ sp o ++ " == h." ++ sp o ++ ")"
           | (o, _, _, masks) <- pfOutputs pf ]
        ++ [ unwords (taskShapeName (taskShape tp) : [ "s." ++ sp n | (n, _) <- taskFields tp ]
                      ++ [ "h." ++ sp n | (n, _) <- taskFields tp ])
           | tp <- pfTaskPorts pf ]
    -- how often `Agree`, its abbreviations unfolded, reads each field of the
    -- sequential state ('s') and of the hardware's ('h')
    agreeUses = M.fromListWith (+) $
        [ (('s', "illegal_access"), 1 :: Int), (('s', "guard_violated"), 1) ]
        ++ concat [ [ (('s', en), if null (ptArgs p) then 1 else 2), (('h', en), 1) ] | p <- pfPorts pf, Just en <- [ptEnable p] ]
        ++ [ ((side, a), 1) | p <- pfPorts pf, (a, _) <- ptArgs p, side <- "sh" ]
        ++ concat [ (('s', o), 1) : (('h', o), 1) : [ (('h', r), 1) | WhenOutput r <- masks ]
                  | (o, _, _, masks) <- pfOutputs pf ]
        ++ concat [ (('s', tpPos tp), 1) : (('h', tpPos tp), if null (tpArgs tp) then 1 else 2)
                    : [ ((side, a), 1) | (a, _) <- tpArgs tp, side <- "sh" ]
                  | tp <- pfTaskPorts pf ]
    maskNeg m = case m of
        WhenOutput r   -> "!h." ++ sp r
        WhenInput en   -> "!i." ++ inPath en
        WhenNotInput en -> "i." ++ inPath en
    hypStruct (nm, _, what, hs) =
        [""] ++ doc what ++ ["structure " ++ nm ++ " (i : Inputs) : Prop where"]
        ++ concat [ map ("  " ++) (doc cmt) ++ ["  " ++ q n ++ " :"] ++ ["    " ++ stmt]
                  | (n, cmt, stmt) <- hs ]
    -- Lean allows a structure at most 'maxFields' fields: more facts go to
    -- several structures, each a field of 'Hyps'
    chunkHyps nm var what facts = case chunksOf maxFields facts of
        []   -> []
        [fs] -> [(nm, var, what, fs)]
        fss  -> [ (nm ++ show k, var ++ show k
                  , what ++ "  (Part " ++ show k ++ " of " ++ show (length fss) ++ ".)", fs)
                | (k, fs) <- zip [1 :: Int ..] fss ]
    -- Every theorem takes the hypotheses as one structure, which the two
    -- proofs by bv_decide take apart one level: bv_decide takes apart each
    -- of the structures of facts itself.
    hypsDecl
        | null hyps = []
        | otherwise = ["", "/-- Everything the theorems assume, as one hypothesis. -/"
                      , "structure Hyps (i : Inputs) : Prop where"]
                      ++ [ "  " ++ var ++ " : " ++ nm ++ " i" | (nm, var, _, _) <- hyps ]
    hArg = if null hyps then "" else " (h : Hyps i)"
    hUse = if null hyps then "" else " h"
    bvDecide = "bv_decide (config := { maxSteps := 1000000000, timeout := 100000 }) types ["
               ++ intercalate ", " (["Inputs"] ++ map (("Inputs." ++) . snd) inputParts' ++ ["State"]
                                    ++ [ "State." ++ partType k | splitState, (k, _) <- stateParts ]) ++ "]"
    inputParts' = [ (k, partType k) | (k, _) <- inputParts, splitInputs ]
    -- The proof takes the hypotheses apart (bv_decide uses the local facts,
    -- not those inside a structure), and supplies the equation of every
    -- application the hypotheses reach: the definitions are opaque to
    -- bv_decide, which treats each application of one as a value of its
    -- own.  With just those it checks the hypotheses are consistent, then
    -- supplies the equations of every other application the conclusions
    -- reach, unfolds the agreement and the states it compares in the goal
    -- (each a State literal of definitions), and decides.  Each equation is
    -- printed once, in this one proof.
    proofLines goalNames =
        (if null hyps then [] else
            [ "  -- the hypotheses, each fact under its own name" ]
            ++ bracket ", " ("  obtain ⟨", "⟩ := h") [ var | (_, var, _, _) <- hyps ]
            ++ concat [ bracket ", " ("  obtain ⟨", "⟩ := " ++ var) (map q [ n | (n, _, _) <- fs ])
                      | (_, var, _, fs) <- hyps ])
        ++ [ "  -- the equation of every definition the hypotheses reach" | not (null hypInsts) ]
        ++ haveBlocks (map eqHave hypInsts)
        ++ notVacuous
        ++ [ "  -- the equation of every other definition the conclusions reach" | not (null goalInsts) ]
        ++ haveBlocks (map eqHave goalInsts)
        ++ bracket ", " ("  dsimp only [", "]") goalNames
        ++ [ "  " ++ bvDecide ]
    -- items after an opening, as many to a line as fit, then a closing
    bracket sep (open, close) xs = case wrapJoin 88 sep xs of
        []       -> [open ++ close]
        (l : ls) -> let ws = (open ++ l) : map ("    " ++) ls
                        brk = reverse (dropWhile (== ' ') (reverse sep))   -- the separator at a line's end
                    in map (++ brk) (init ws) ++ [last ws ++ close]
    goalBase = [ "stepSequential", "stepScheduled", "Agree" ] ++ map shapeName agreeShapes ++ map taskShapeName taskShapes
    -- An equation by `rfl`, the body written out (at the view): unlike
    -- `eq_1`, it makes Lean build no lemma, whose memory grows superlinearly.
    eqHave x = case x of
        IDef n Nothing  -> rflHave proofC (TRef n False) (DShared n)
        IDef n (Just k) -> rflHave (proofC { pcView = VSub (viewText k) }) (viewInst n k) (DShared n)
        IHw n           -> rflHave proofC (THw n) (DHw n)
        IVer ns         -> rflHave proofC (TDef ns) (DSeq ns)
        IProj k f       -> [ "have : " ++ viewText k ++ "." ++ viewPath f ++ " = " ++ projValue k f ++ " := rfl" ]
    viewInst n k = case k of
        VKHw       -> TAtHw n
        VKAt v     -> TAt n v
        VKFun v as -> TAtVF n v as
    rflHave c lhs key =
        let l = "have : " ++ ppT proofC 4 lhs ++ " ="
            body = ML.findWithDefault (TLit False) key expBodies
        in case layout c 86 4 body of
               [b] | length l + length b < 92 -> [l ++ " " ++ b ++ " := rfl"]
               bs -> l : map ("    " ++) (init bs) ++ ["    " ++ last bs ++ " := rfl"]
    -- (in the proof, a fact or a hypothesis of that name would hide a definition)
    proofC = hwC { pcShadow = S.fromList ("this" : [ var | (_, var, _, _) <- hyps ]
                                          ++ [ n | (_, _, _, fs) <- hyps, (n, _, _) <- fs ]) }
    -- The equations in parenthesized blocks of 64 (fewer, larger steps cost
    -- Lean less memory), as many to a line as fit.
    haveBlocks hs = concatMap block (chunksOf 64 hs)
      where
        block items =
            let n = length items
                sep = zipWith (\k ls -> if k == n then ls else onLast (++ ";") ls) [1 :: Int ..] items
            in case pack sep of
                   []       -> []
                   (l : ls) -> onLast (++ ")") (("  (" ++ l) : map ("   " ++) ls)
        pack items = case items of
            [] -> []
            ([l] : rest) -> let (line, rest') = fill l rest in line : pack rest'
            (ls : rest) -> ls ++ pack rest
        fill cur items = case items of
            ([l] : rest) | length cur + 1 + length l <= 93 -> fill (cur ++ " " ++ l) rest
            _ -> (cur, items)
        onLast f ls = case reverse ls of
            (z : zs) -> reverse (f z : zs)
            []       -> []
    viewText k = case k of
        VKHw    -> "(View.hw i)"
        VKAt v  -> "(" ++ qual v ++ " i)"
        VKFun v as -> "(i." ++ inPath v ++ concatMap (\a -> " " ++ ppT hwC 5 a) as ++ ")"
    projValue k f = case k of
        VKAt v | Just var <- M.lookup v viewVars >>= M.lookup f -> "i." ++ inPath var
        _ -> maybe (zero SBool) (ppT hwC 4 . fdHw) (M.lookup f fieldMap)
    viewVars = M.fromList [ (vwName v, M.fromList (vwVars v)) | v <- soViews sq ]
    -- the applications a term printed in a mode makes directly (in front
    -- of others, so that nesting does not copy them)
    direct m t = directTo m t []
    directTo m t rest = case t of
        TView f -> case m of
            RInline -> ML.findWithDefault [] f inlineDirect ++ rest
            RProj k | viewAtom k -> rest
                    | otherwise  -> IProj k f : rest
        TRef n True  -> IDef n (Just (case m of { RInline -> VKHw; RProj k -> k })) : rest
        TRef n False -> IDef n Nothing : rest
        TAtHw n      -> IDef n (Just VKHw) : rest
        TAt n v      -> IDef n (Just (VKAt v)) : rest
        THw n        -> IHw n : rest
        TDef ns      -> IVer ns : rest
        TAtVF n v as -> IDef n (Just (VKFun v as)) : rest
        _            -> foldr (directTo m) rest (subTerms t)
    -- (each field's, made once)
    inlineDirect = ML.map (direct RInline . fdHw) fieldMap
    -- what an application's equation mentions in turn
    -- (a body with the definitions used once written in, as its equation has it)
    instBody x = case x of
        IDef n mk -> [ (maybe RInline RProj mk, b) | Just b <- [ML.lookup (DShared n) expBodies] ]
        IHw n     -> [ (RInline, b) | Just b <- [ML.lookup (DHw n) expBodies] ]
        IProj (VKAt v) f | Just vars <- M.lookup v viewVars, M.member f vars -> []
        IProj _ f -> [ (RInline, fdHw fd) | Just fd <- [M.lookup f fieldMap] ]
        IVer ns   -> [ (RInline, b) | Just b <- [ML.lookup (DSeq ns) expBodies] ]
    hypRoots = [ (RInline, t) | (_, _, t) <- pfEnv pf ++ pfPrims pf ++ pfOracles pf ++ pfUser pf ]
               ++ concatMap congTerms (pfCongs pf)
    congTerms c = (RInline, cgLhs c) : (RInline, cgRhs c)
                  : concat [ case p of
                                 PEq x y -> [(RInline, x), (RInline, y)]
                                 PPort port ts -> map ((,) RInline) (ts ++ hwPortTerms port)
                                 PTrue t -> [(RInline, t)]
                           | p <- cgPremises c ]
    goalRoots = [ (RInline, expand t) | t <- M.elems (soFinals sq) ]
                ++ [ (RInline, expand t) | t <- M.elems (pfHwPorts pf) ]
                ++ [ (RInline, expand t) | (_, _, t, _) <- pfOutputs pf ]
                ++ [ (RInline, THw oneHotKey) | not (null (pfMuxes pf)) ]
    (hypInsts, seenH, fullH) = closure S.empty S.empty hypRoots
    (goalInsts, _, _) = closure seenH fullH goalRoots
    -- Each instance once, in the order met.  A shared definition's equation
    -- at one more view adds only that view's instances of what in it
    -- depends on the view: the rest, the same at every view, its first
    -- equation (at whichever view) added with all that it mentions.
    closure seen0 full0 roots = go seen0 full0 [] [ x | (m, t) <- roots, x <- direct m t ]
      where
        go seen full acc [] = (reverse acc, seen, full)
        go seen full acc (x : xs)
            | x `S.member` seen = go seen full acc xs
            | otherwise = case x of
                IDef n (Just k@(VKAt _)) | S.member n full ->
                    go (S.insert x seen) full (x : acc) (atInst k (ML.findWithDefault [] n defAtoms) ++ xs)
                IDef n (Just (VKAt _)) -> go (S.insert x seen) (S.insert n full) (x : acc) (mentions x ++ xs)
                _ -> go (S.insert x seen) full (x : acc) (mentions x ++ xs)
        mentions x = [ y | (m, t) <- instBody x, y <- direct m t ]
    -- the view-dependent atoms of a term, as 'directTo' in a mode RProj
    -- meets them (a term without any is not 'dynamic'), the first of each
    viewAtoms t rest = case t of
        TView f       -> Left f : rest
        TRef n True   -> Right n : rest
        _ | not (dynamic t) -> rest
        _             -> foldr viewAtoms rest (subTerms t)
    defAtoms = ML.fromList [ (n, nubOrd (viewAtoms b [])) | (DShared n, b) <- ML.toList expBodies ]
    atInst k = concatMap (either (\f -> [ IProj k f | not (viewAtom k) ]) (\n -> [IDef n (Just k)]))
    -- (with other orders, a step's view is a function's value: each field of
    -- it is an atom, with no equation)
    viewAtom k = case k of { VKHw -> False; VKAt _ -> modeB; VKFun _ _ -> True }
    congStmt c =
        let prem p = case p of
                PEq x y -> ppT hwC 4 x ++ " = " ++ ppT hwC 4 y
                PPort port ts -> unwords (shapeName (portShape port) : map (ppT hwC 5) (ts ++ hwPortTerms port))
                                 ++ " = true"
                PTrue t -> boolStmt t
        in concatMap (\p -> prem p ++ " → ") (cgPremises c) ++ ppT hwC 4 (cgLhs c) ++ " = " ++ ppT hwC 4 (cgRhs c)
    hyps = concat
        [ chunkHyps "Environment" "env" "What the module's exported schedule asks of its parent."
              [ (n, c, boolStmt t) | (n, c, t) <- pfEnv pf ]
        , chunkHyps "Primitives" "prim" "What bsc's wire primitives do: their Verilog drives each output \
                                        \from an input within the cycle."
              [ (n, c, boolStmt t) | (n, c, t) <- pfPrims pf ]
        , chunkHyps "Oracles" "bsc" "Facts bsc's disjointness solver proved: recorded by the scheduler, \
                                    \or re-derived by asking the same solver about the same expressions."
              [ (n, c, boolStmt t) | (n, c, t) <- pfOracles pf ]
        , chunkHyps "Assertions" "user" "The user's scheduling assertions and bsc's other run-time checks; bsc reports \
                                       \at simulation time if one fails."
              [ (n, c, boolStmt t) | (n, c, t) <- pfUser pf ]
        , chunkHyps "Congruence" "ack" ("Functional consistency: a submodule output, or a datapath operator, \
                                       \gives equal results for equal inputs.  Each fact relates what one step \
                                       \sees to what the hardware sees (Ackermann's reduction)."
                                       ++ (if S.null (dHavocPorts d) then ""
                                           else "  A read a submodule's annotations forbid where a step makes it \
                                                \is tied to the hardware's value only when the step does not fire \
                                                \or no use it may not follow came first; otherwise it is arbitrary."))
              [ (cgName c, cgNote c, congStmt c) | c <- pfCongs pf ] ]
    conclusion = "Agree i (stepSequential i) (stepScheduled i) = true"
                 ++ (if null (pfMuxes pf) then "" else " ∧ Hw.oneHot i = true")
    scheduleDoc =
        ["", "/-- **Schedule soundness for `" ++ dModule d ++ "`.**  In every cycle in which the"
        , "    environment obeys the exported schedule, the hardware does what executing"
        , "    the fired rules one at a time, in the logical execution order, does"
          ++ (if null (pfMuxes pf) then (if null (pfTaskPorts pf) then ". -/" else ".") else ",") ]
        ++ [ "    and every parallel mux in the hardware has one-hot selectors" ++ (if null (pfTaskPorts pf) then ". -/" else ".")
           | not (null (pfMuxes pf)) ]
        ++ (if null (pfTaskPorts pf) then [] else
           [ "    That includes its system-task calls, made in the order of its always block for"
           , "    tasks: a property of this module's Verilog, since Verilog leaves open the order of"
           , "    different modules' calls in one cycle (Bluesim, which follows the whole design's"
           , "    rule order, may print those in another). -/" ])
    theorem =
        scheduleDoc
        ++ [ "theorem schedule_sound (i : Inputs)" ++ hArg, "    : " ++ conclusion ++ " := by" ]
        ++ proofLines goalBase
    -- Hypotheses that contradict each other would make the theorem vacuous.
    -- Only assertions can (a run-time check that fails in every cycle):
    -- the other facts hold of the hardware itself.  The check needs only the
    -- hypotheses and their equations, so it comes in the proof before the
    -- conclusions' equations.
    notVacuous
        | null hyps = []
        | otherwise =
            [ "  -- **Not vacuous.**  `bv_decide` fails to derive `False` from the hypotheses.  Were"
            , "  -- they contradictory, it would succeed, this check would fail, and so would the proof." ]
            ++ [ "  -- (Only the assertions can contradict the rest: a run-time check that fails in every"
               | not (null (pfUser pf)) ]
            ++ [ "  -- cycle, say.)" | not (null (pfUser pf)) ]
            ++ [ "  fail_if_success", "    exfalso", "    " ++ bvDecide ]
    namePos = M.fromListWith (\_ old -> old) (zip names [0 :: Int ..])
    orderTheorem = case [ f | f@(_, _, _, pairs, _) <- pfOrderFacts pf, not (null pairs) ] of
        [] -> []
        fs -> ["", "/-- The logical execution order. -/"
              , "def executionOrder : List String :=" ]
              ++ map ("  " ++) (bracketItems (map show names))
              ++ [ "", "/-- The pairs of steps below, each step with its position in `executionOrder`. -/"
                 , "def executionPairs : List (String × Nat × String × Nat) :=" ]
              ++ map ("  " ++) (bracketItems [ "(" ++ show x ++ ", " ++ show (namePos M.! x) ++ ", " ++ show y ++ ", "
                                               ++ show (namePos M.! y) ++ ")"
                                             | (_, _, _, pairs, _) <- fs, (x, y) <- pairs ])
              ++ [ "" ]
              ++ doc ("The module exports " ++ intercalate "; " [ a ++ " " ++ r ++ " " ++ b | (a, r, b, _, _) <- pfOrderFacts pf ]
                      ++ ": a parent may run the first of each pair before the second.  The module's own"
                      ++ " execution order agrees: of the two methods' rules, each pair whose uses of a"
                      ++ " submodule are not all conflict-free comes in that order"
                      ++ (case concat [ sk | (_, _, _, _, sk) <- pfOrderFacts pf ] of
                            [] -> "."
                            sk -> "; the pairs that commute (" ++ intercalate ", " [ x ++ "/" ++ y | (x, y) <- sk ]
                                  ++ ") may come in either order and are not checked."))
              ++ [ "theorem exported_order_agrees :"
                 , "    ∀ p ∈ executionPairs, executionOrder[p.2.1]? = some p.1 ∧"
                 , "      executionOrder[p.2.2.2]? = some p.2.2.1 ∧ p.2.1 < p.2.2.2 := by decide +kernel" ]

-- | A Lean identifier for a bsc name, quoted with «» when necessary.  A name
-- that Lean or this file also defines in a structure's namespace
-- (@State.mk@, @State.init@, @View.hw@, @Inputs.Part1@, ...) gets a prime,
-- so that a field or definition called @init@ cannot clash with it.
q :: String -> String
q s0 | plain s && s `S.notMember` leanKeywordSet = s
     | otherwise = "«" ++ filter (/= '»') s ++ "»"
  where s = if s0 `S.member` structureNameSet || isPart s0 then s0 ++ "'" else s0
        isPart n = "Part" `isPrefixOf` n && length n > 4 && all isDigit (drop 4 n)
        plain (c : cs) = (isAsciiLower c || isAsciiUpper c) && all ok cs
        plain []       = False
        ok x = isAsciiLower x || isAsciiUpper x || isDigit x || x == '_'
