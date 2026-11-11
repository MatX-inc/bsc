{-# LANGUAGE MagicHash, UnboxedTuples #-}
-- | The typechecker's package-wide dictionary pool (see TCMisc.sat): the
-- forcing-free keys it is indexed by, its entries, and the work counters.
module DictPool(
    PK(..), KeyTable, emptyKeyTable, keyTableVersion, typeKey, isEvaluated,
    GKeys(..), gkValid,
    PoolKey, Slot(..), ExprT(..), Tmpl(..), Recipe(..), Entry(..), ReplayEv(..),
    GoalSum, Rope(..), ropeMapM_, Summary(..), emptySummary, catSummary,
    KeyState(..), DictPool(..), initDictPool,
    Cnt(..), cntName, allCnts,
    dictPoolOff, traceDictPool, traceDictPoolEntries, dictPoolVerify,
    noRecByBase
    ) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import System.IO.Unsafe(unsafeDupablePerformIO)
import GHC.IO(IO(..))
import GHC.Exts(anyToAddr#, addr2Int#, int2Addr#, andI#, notI#,
                indexAddrOffAddr#, indexWord32OffAddr#, word32ToWord#, word2Int#,
                isTrue#, (/=#), Int(..))
import GHC.Exts.Heap(ClosureType(..))

import Id
import CType
import Pred(Class)
import CSyntax(CExpr)
import SolvedBinds(SolvedBind)
import FStringCompat(fstringId)
import IOUtil(progArgs)

-- Flags (hidden).
dictPoolOff, traceDictPool, traceDictPoolEntries, dictPoolVerify :: Bool
-- (also off under the solver traces, which print the solves a hit skips)
dictPoolOff = any (`elem` progArgs)
                  ["-no-tc-dict-pool", "-trace-type", "-trace-tc-satisfy",
                   "-trace-tc-reducepred", "-trace-tcvar"]
traceDictPool = "-trace-tc-dict-pool" `elem` progArgs
-- each record and each hit
traceDictPoolEntries = "-trace-tc-dict-pool-entries" `elem` progArgs
-- diagnostic: where a hit would be taken, solve instead and count a
-- solve that draws another number of names than the record says
dictPoolVerify = "-tc-dict-pool-verify" `elem` progArgs

-- ==============================
-- Values inspected without forcing
--
-- Only the keys below ask this, and only to decline: a type with a part
-- not evaluated yet has no key, so the predicate is solved as without
-- the pool, which gives the same result as a hit.

-- | Whether the value is a constructor already (possibly behind
-- indirections), not a thunk.  An evaluatedness check only: it forces
-- nothing, keeps nothing and allocates nothing.  A tagged pointer is a
-- constructor's, and an untagged one's closure type is read from its
-- info table (laid out next to the code, without profiling), with no
-- allocation, so no collection, between taking the address and the reads.
{-# NOINLINE isEvaluated #-}
isEvaluated :: b -> Bool
isEvaluated x =
    unsafeDupablePerformIO (IO (\ s -> case anyToAddr# x s of
      (# s', a #) -> (# s', conAt (8 :: Int) a #)))
  where conAt 0 _ = False
        conAt n a
          | isTrue# (andI# (addr2Int# a) 7# /=# 0#) = True
          | otherwise =
              let p = int2Addr# (andI# (addr2Int# a) (notI# 7#))
                  info = indexAddrOffAddr# p 0#
                  ty = toEnum (I# (word2Int# (word32ToWord# (indexWord32OffAddr# info (-2#)))))
              in  if isConType ty then True
                  else if isIndType ty then conAt (n - 1) (indexAddrOffAddr# p 1#)
                  else False

isConType, isIndType :: ClosureType -> Bool
isConType t = t `elem` [CONSTR, CONSTR_1_0, CONSTR_0_1, CONSTR_2_0,
                        CONSTR_1_1, CONSTR_0_2, CONSTR_NOCAF]
isIndType t = t `elem` [IND, IND_STATIC, BLACKHOLE]

-- ==============================
-- Keys
--
-- A ground normal type's key is its canonical word (CType, "Canonical
-- identity").  #339 refuses a word to a TyStr leaf, because computing it
-- could force a lazy mkFString and intern the string early; a TyStr whose
-- string is evaluated already has its intern id, so here such a leaf gets
-- a key from that id, and an application with a refused child a key from
-- its children's keys (pool-local numbers, all negative, never confused
-- with a word).  Equal keys imply Eq-equal types.  Nothing is forced: a
-- part that is not evaluated yet, an unqualified tycon or a non-normal
-- node (synonym, type function) makes the type unkeyable (PUnknown), so
-- the caller treats it as a miss without comparing anything.

data PK = PKey !Int        -- ^ ground, normal, keyed
        | PVar [Int]       -- ^ evaluated, normal apart from type
                           --   variables (their numbers)
        | PUneval          -- ^ a part is not evaluated yet
        | PNotNormal       -- ^ not normal (synonym, type function,
                           --   unqualified tycon, TGen), or built
                           --   before the canonical-word gate opened
        deriving (Eq, Show)

data KeyTable = KeyTable {
        ktAp :: !(M.Map (Int, Int) Int),
        ktStr :: !(IM.IntMap Int),
        ktNext :: !Int
    }

emptyKeyTable :: KeyTable
emptyKeyTable = KeyTable M.empty IM.empty (-2)

-- | The key of a type, whether it needed a string key, and the table.
-- With "alloc" a new string or composite gets a key; without, the key
-- of one the table lacks is "absent", which matches no entry.
typeKey :: Bool -> KeyTable -> Type -> (PK, Bool, KeyTable)
typeKey alloc kt0 t0 = go kt0 t0
  where
    go kt t | isEvaluated t = node kt t
            | otherwise = (PUneval, False, kt)
    node kt t@(TCon c)
      | w >= 0 = (if normalWord w then PKey w else PNotNormal, False, kt)
      | not (isEvaluated c) = (PUneval, False, kt)
      | otherwise = case c of
          TyStr s _ | isEvaluated s -> strKey kt (fstringId s)
                    | otherwise -> (PUneval, False, kt)
          TyCon i _ _ | not (isEvaluated i) -> (PUneval, False, kt)
                      | isUnqualId i -> (PNotNormal, False, kt)
          _ -> let w' = typeCanonWord t
               in  (if normalWord w' then PKey w' else PNotNormal, False, kt)
      where w = typeCanonWordNode t
    node kt t@(TAp f a)
      | w >= 0 = (if normalWord w then PKey w else PNotNormal, False, kt)
      | otherwise =
          -- (the second child's triple is taken apart by a case where it
          -- is read: as a lazy pattern, GHC allocates it and a selector)
          let (kf, sf, kt1) = go kt f
          in  case kf of
                PUneval -> (PUneval, False, case go kt1 a of (_, _, kt2) -> kt2)
                _ -> case go kt1 a of
                  (ka, sa, kt2) -> case (kf, ka) of
                    (_, PUneval) -> (PUneval, False, kt2)
                    (PNotNormal, _) -> (PNotNormal, False, kt2)
                    (_, PNotNormal) -> (PNotNormal, False, kt2)
                    (PKey x, PKey y)
                      -- a node without a word whose children both have one
                      -- was built while the canonical-word gate was shut,
                      -- and an equal node built since has a word, not this key
                      | x >= 0 && y >= 0 -> (PNotNormal, False, kt2)
                      | otherwise -> apKey kt2 (sf || sa) x y
                    (PVar xs, PVar ys) -> (PVar (xs ++ ys), False, kt2)
                    (PVar xs, _) -> (PVar xs, False, kt2)
                    (_, PVar ys) -> (PVar ys, False, kt2)
      where w = typeCanonWordNode t
    node kt (TVar v)
      | isEvaluated v, TyVar _ n _ <- v, isEvaluated n = (PVar [n], False, kt)
      | otherwise = (PUneval, False, kt)
    node kt _ = (PNotNormal, False, kt)

    normalWord w = w >= 0 && odd w

    strKey kt sid = case IM.lookup sid (ktStr kt) of
      Just k -> (PKey k, True, kt)
      Nothing
        | alloc -> let k = ktNext kt
                   in  (PKey k, True, kt { ktStr = IM.insert sid k (ktStr kt), ktNext = k - 1 })
        | otherwise -> (PKey absent, True, kt)
    apKey kt s x y
      | x == absent || y == absent = (PKey absent, s, kt)
      | otherwise = case M.lookup (x, y) (ktAp kt) of
          Just k -> (PKey k, s, kt)
          Nothing
            | alloc -> let k = ktNext kt
                       in  (PKey k, s, kt { ktAp = M.insert (x, y) k (ktAp kt), ktNext = k - 1 })
            | otherwise -> (PKey absent, s, kt)

-- | A key that no table entry has (lookup mode, composite not present).
absent :: Int
absent = minBound

-- | The table's version: it only grows, and each addition moves ktNext.
keyTableVersion :: KeyTable -> Int
keyTableVersion = ktNext

-- | The keys of a context predicate's types as the hit guard last
-- computed them (TCMisc.goalsClear), with the version of the table they
-- were looked up in; GKNone if not computed yet.
data GKeys = GKNone | GKeys !Int [PK]

-- | Whether keys looked up in table version v are those a lookup in
-- version ver gives.  A key of a type with a part not evaluated can
-- change as the type is evaluated, and an absent one as the table grows;
-- any other key of a type is final.
gkValid :: Int -> Int -> [PK] -> Bool
gkValid ver v = all ok
  where ok PUneval = False
        ok (PKey k) | k == absent = v == ver
        ok _ = True

-- ==============================
-- Entries

-- | Class name, the incoherence flag, last-resort flag, fundep row, and
-- per position: Left the key of a key position or ground output, or
-- Right j for an output whose variable's number is the j-th smallest of
-- the outputs' distinct variables' (so the key holds their sharing and
-- the order of their numbers, which joins and unification go by).
type PoolKey = (Id, Bool, Bool, Int, [Either Int Int])

-- | A dictionary name in a recorded solve: the root predicate's, or the
-- one numbered c0 + k (c0 the counter at the start of the solve).
data Slot = SRoot | SDict !Int
    deriving (Eq, Show)

data ExprT = ESlot Slot | EConst CExpr | EApp ExprT [ExprT]

-- | A recorded solve's bindings, in the merge tree its SolvedBinds was
-- built with (TCMisc.rebuild makes the same tree): a binding (name,
-- closed type, evidence, class), a merge (new <++ old), or a nested
-- pooled solve whose root is the given slot and whose slots are offset.
data Tmpl = TEmpty | TLeaf Slot Type ExprT (Maybe Class) | TMerge Tmpl Tmpl
          | TSub Slot !Int Tmpl

-- | The merge tree of the SolvedBinds a solve builds, kept beside it (it
-- is read without forcing the (<++) guards, which force the names), with
-- the dictionary names its bindings bind and use given by the numbers
-- they were drawn with (TIMonad.newDictN), so that a record tells them
-- apart without comparing, or forcing, a name.
data Recipe
    = RcEmpty
    | RcInst [Int] SolvedBind  -- ^ the goal's own instance binding, whose
                               --   arguments are the dictionaries drawn
                               --   with these numbers (TCMisc.byInst)
    | RcJoin !Int !Int SolvedBind -- ^ a join binding one goal's dictionary
                               --   (the first number) to another's
    | RcMerge Recipe Recipe    -- ^ new <++ old
    | RcGoal !Int Recipe       -- ^ the solve of a nested goal, whose
                               --   dictionary has this number (-1 unknown)
    | RcPool !Int Tmpl         -- ^ the goal's pooled solve (a record or a
                               --   hit): its counter base and template
    | RcOpaque                 -- ^ anything else

-- | Effects of a solve that a hit makes again, in order.  RJoin is a
-- join's residual filter (TCMisc.joinCtxsN), whose Id comparisons force,
-- so intern, the names of the dictionaries it is given the numbers of:
-- the joined pair's, then the residual goals' in order, as drawn in the
-- record (Entry.eBase).
data ReplayEv = RATF Id [Type] Type | RPkg Id | RJoin !Int !Int [Int]

-- | Per goal class and fundep row: the keys of the undetermined
-- positions of the solve's nested goals where all of them are ground,
-- and whether some goal had an unknown key there (TCMisc.goalsClear).
type GoalSum = M.Map Id (IM.IntMap (S.Set [Int], Bool))

-- | A sequence built by concatenation in O(1).  Spine-strict, so a
-- nested record's rope is evaluated once, when it is built, and shared
-- by the records around it without being walked again.
data Rope a = RNil | RLeaf !a | RCat !(Rope a) !(Rope a)

ropeMapM_ :: Monad m => (a -> m ()) -> Rope a -> m ()
ropeMapM_ _ RNil = return ()
ropeMapM_ f (RLeaf x) = f x
ropeMapM_ f (RCat x y) = ropeMapM_ f x >> ropeMapM_ f y

-- | What a nested solve (a closed recording window, or a pool hit) leaves
-- to the recording of the solve around it.
data Summary = Summary {
        smEvents :: !(Rope ReplayEv), -- ^ ATF, package and join records, in order
        smGoals :: GoalSum,        -- ^ nested goals
        smTaintPerm :: !Bool,      -- ^ tainted by the solve itself (a newVar
                                   --   draw, a non-ground ATF result, ...),
                                   --   not by a given or the stack
        smTaint :: !Bool,          -- ^ met a goal from a given or the stack,
                                   --   drew a newVar, or recorded a
                                   --   non-ground ATF result
        smJoined :: !Bool          -- ^ the events hold a join record
    }

emptySummary :: Summary
emptySummary = Summary RNil M.empty False False False

-- | A window's summary with a nested one's added.
catSummary :: Summary -> Summary -> Summary
catSummary (Summary e1 g1 p1 t1 j1) (Summary e2 g2 p2 t2 j2) =
    Summary (RCat e1 e2) (unionGoals g1 g2) (p1 || p2) (t1 || t2) (j1 || j2)
  where unionGoals = M.unionWith (IM.unionWith (\ (a, x) (b, y) -> (S.union a b, x || y)))

data Entry = Entry {
        eId :: !Int,            -- ^ (for -trace-tc-dict-pool-entries)
        eBase :: !Int,          -- ^ the counter at the start of the solve
        eDraws :: !Int,         -- ^ counter draws of the solve
        eTmpl :: Tmpl,          -- ^ the solve's bindings
        eOuts :: [(Int, Type)], -- ^ output position -> its closed type,
                                --   for the outputs that are variables
        eEvents :: !(Rope ReplayEv), -- ^ nested ATF, package and join records
        eJoined :: !Bool,       -- ^ the events hold a join record
        eRootPkg :: Maybe Id,   -- ^ the root instance's package
        eGoals :: GoalSum,      -- ^ nested goals (TCMisc.goalsClear)
        eMaxSat :: !Int,        -- ^ sat-stack growth above the root
        eMaxSuper :: !Int,      -- ^ super-stack growth above the root
        eRefused :: !Int        -- ^ lookups the hit guard refused
                                --   (TCMisc.goalsClear)
    }

-- | What the pool knows of a key.
data KeyState = KNoRec !Int Cnt       -- ^ its solve could not be recorded:
                                      --   the count of tries for a transient
                                      --   reason, and the last reason
              | KEntry Entry

data DictPool = DictPool {
        dpKeys :: !(M.Map PoolKey KeyState),
        dpKeyTab :: !KeyTable,
        dpNext :: !Int          -- ^ the next entry's eId
    }

initDictPool :: DictPool
initDictPool = DictPool M.empty emptyKeyTable 0

-- ==============================
-- Counters (-trace-tc-dict-pool)

data Cnt = CReducePred | CSat | CLookup | CHitWord | CHitStr | CHitVarOut
         | CNumericSkip | CSkipNotNormal | CSkipUnevaluated | CSkipNoRow
         | CSkipPartialOut | CSkipOther
         | CMissNoEntry | CFallbackDepth | CFallbackContext
         | CFallbackUnify | CRecOk | CRecNotFull | CRecRecursive | CRecIncoherent
         | CRecNonGround | CRecOpen | CRecMonadAV | CRecShape | CHitBinds | CHitDraws
         | CRecError | CRecShapePool | CRecShapeRoot | CRecUnbound
         | CVerifyOk | CVerifyDraws
         | CSkipNoRec | CNoRecAdd
         | CNoRecVar | CNoRecTaint | CNoRecNested
         | CSkipRefused | CRecJoinOut | CHitJoins | CRecOutDvs | CFallbackFresh
    deriving (Eq, Ord, Show, Enum, Bounded)

-- Counter keys at noRecByBase + fromEnum c count the lookups skipped for a
-- key that reason c made unrecordable (-trace-tc-dict-pool).
noRecByBase :: Int
noRecByBase = 1000

allCnts :: [Cnt]
allCnts = [minBound .. maxBound]

cntName :: Cnt -> String
cntName c = case c of
    CReducePred -> "reducePred"; CSat -> "sat"; CLookup -> "lookup"
    CHitWord -> "hit_word"; CHitStr -> "hit_strword"; CHitVarOut -> "hit_varout"
    CNumericSkip -> "skip_numeric"; CSkipNotNormal -> "skip_notnormal"
    CSkipUnevaluated -> "skip_unevaluated"; CSkipNoRow -> "skip_noground_row"
    CSkipPartialOut -> "skip_partial_output"; CSkipOther -> "skip_other"
    CMissNoEntry -> "miss_noentry"
    CFallbackDepth -> "fallback_depth"; CFallbackContext -> "fallback_context"
    CFallbackUnify -> "fallback_unify"; CRecOk -> "rec_ok"
    CRecNotFull -> "rec_notfull"; CRecRecursive -> "rec_recursive"
    CRecIncoherent -> "rec_incoherent"; CRecNonGround -> "rec_nonground"
    CRecOpen -> "rec_open"; CRecMonadAV -> "rec_monadav"
    CRecShape -> "rec_shape"; CHitBinds -> "hit_binds"
    CHitDraws -> "hit_draws"; CRecError -> "rec_error"
    CRecShapePool -> "rec_shape_pool"; CRecShapeRoot -> "rec_shape_root"
    CRecUnbound -> "rec_unbound"
    CVerifyOk -> "verify_ok"; CVerifyDraws -> "verify_draws_differ"
    CSkipNoRec -> "skip_norec"; CNoRecAdd -> "norec_added"
    CNoRecVar -> "norec_vardraw"; CNoRecTaint -> "norec_taint"
    CNoRecNested -> "norec_nested_taint"
    CSkipRefused -> "skip_refused"
    CRecJoinOut -> "rec_join_outside"; CHitJoins -> "hit_joins"
    CRecOutDvs -> "rec_output_in_dvs"; CFallbackFresh -> "fallback_fresh"
