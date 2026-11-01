{-# LANGUAGE CPP #-}
{-# LANGUAGE GeneralizedNewtypeDeriving, PatternGuards, DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}
-- | The CType package defines the concrete representation of types and kinds.
module CType(
  -- * Types
  Type(TVar, TCon, TAp, TGen, TDefMonad),
  CType, TyVar(..), TyCon(..), TISort(..), StructSubType(..),

  -- ** Canonical identity
  typeCanonWord, typeCanonWordNode,
  isGroundType, isNormalType, isGroundNode, isNormalNode,
  enableCanonIds, disableCanonIds, CTypeStat(..), ctypeStat, bumpCTypeStat, cTypeStats,
  checkLetrecBinders,
  checkEqByWord, checkCanonIds, eqStructural,

  -- ** Examining Types
  getTyVarId, getTypeKind,
  isTNum, getTNum,
  isTStr, getTStr,
  isTVar, isTCon, isIfc, isInterface, isDictType, isDictFun, isUpdateable,
  leftCon, leftTyCon, allTyCons, allTConNames, tyConArgs,
  splitTAp, normTAp,
  isATFAp,
  isTypeBit, isTypeString,
  isTypePrimAction, isTypeAction,
  isTypeActionValue, isTypeActionValue_,
  isTypePolyBit, bitWidth,
  isTypeUnit,
  noTyVarNo, isGeneratedTVar,
  getArrows,  getRes,
  seqCType,
  -- if the above are not sufficient, use these:
  isTypeTConNoArgs, isTypeTConArgs,
  getActionValueArg,
  isTConArrow, isTConPair,

  -- ** Constructing Types
  noType, tVar, tVarKind, cTVar, cTVarKind, cTVarNum, cTCon, cTNum, cTStr,
  cTApplys, setTypePosition,

  -- * Kinds
  Kind(..), PartialKind(..),


  -- ** Examining Kinds
  isKVar, getArgKinds, getResKind,

  -- ** Constructing Kinds
  baseKVar, mkKFun,

  -- * Type Classes
  CTypeclass(..),

  -- ** Examining Type Classes
  typeclassId,

  -- * Type class constraints
  CQType(..), CPred(..),
  getCQArrows,

        ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import Data.Char(isDigit, chr)
import Data.List(union, isPrefixOf, partition)
import Data.Maybe
import qualified Data.Generics as Generic
import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.IORef(IORef, newIORef, readIORef, writeIORef, modifyIORef',
                  atomicModifyIORef')
import System.IO.Unsafe(unsafePerformIO, unsafeDupablePerformIO)
import System.IO(hPutStrLn, stderr)
import Control.Monad(when, unless)

import Eval
import PPrint
import Position
import Id
import IdPrint
import PreIds(idArrow, idPrimPair, idPrimUnit, idBit, idString,
              idPrimAction, idAction, idActionValue_, idActionValue,
              idTNumToStr, idId)
import Util(itos)
import ErrorUtil
import Pragma(IfcPragma)
import TypeOps
import PVPrint(PVPrint(..))
import FStringCompat
import SpeedyString(internCount, internedSince)
import IOUtil(progArgs)

-- Data structures

-- | Representation of types
--
-- TCon_ and TAp_ carry a slot holding the node's canonical word (see
-- "Canonical identity" below): -1 for none, else a word identifying the
-- node's structure under Eq.  The real constructors are private to this
-- module; TCon and TAp are bidirectional pattern synonyms, so every other
-- module matches and builds the public constructor names, and the
-- synonyms' builders compute the slot.
data Type = TVar TyVar         -- ^ type variable
          | TCon_ {-# UNPACK #-} !Int TyCon     -- ^ type constructor
          | TAp_ {-# UNPACK #-} !Int Type Type  -- ^ type-level application
          | TGen Position Int  -- ^ quantified type variable used in type schemes
          | TDefMonad Position -- ^ not used after CVParserImperative
    deriving (Generic.Data, Generic.Typeable)

pattern TCon :: TyCon -> Type
pattern TCon c <- TCon_ _ c
  where TCon c = mkTCon c

pattern TAp :: Type -> Type -> Type
pattern TAp f a <- TAp_ _ f a
  where TAp f a = mkTAp f a

{-# COMPLETE TVar, TCon, TAp, TGen, TDefMonad #-}

-- Prints the public constructor names, as `deriving Show` over the five
-- public constructors would, with the slot omitted (internalError texts
-- and debug dumps show TCon and TAp).
instance Show Type where
    showsPrec d (TVar v) = showParen (d > 10) $
        showString "TVar " . showsPrec 11 v
    showsPrec d (TCon c) = showParen (d > 10) $
        showString "TCon " . showsPrec 11 c
    showsPrec d (TAp f a) = showParen (d > 10) $
        showString "TAp " . showsPrec 11 f . showString " " . showsPrec 11 a
    showsPrec d (TGen p i) = showParen (d > 10) $
        showString "TGen " . showsPrec 11 p . showString " " . showsPrec 11 i
    showsPrec d (TDefMonad p) = showParen (d > 10) $
        showString "TDefMonad " . showsPrec 11 p

-- | Representation of a type variable
data TyVar = TyVar { tv_name :: Id    -- ^ name of the type variable
                   , tv_num  :: Int   -- ^ number for a generated type variable
                   , tv_kind :: Kind  -- ^ kind of the type variable
                   }
    deriving (Show, Generic.Data, Generic.Typeable)


-- | Representation of a type constructor
data TyCon = -- | A constructor for a type of value kind
             TyCon { tcon_name :: Id           -- ^ name of the type constructor
                   , tcon_kind :: (Maybe Kind) -- ^ kind of the type constructor
                   , tcon_sort :: TISort       -- ^ purpose of the type constructor
                   }
             -- | A constructor for a type of numeric kind
           | TyNum { tynum_value :: Integer  -- ^ type-level numeric value
                   , tynum_pos   :: Position -- ^ position of introduction
                   }
             -- | A constructor for a type of string kind
           | TyStr { tystr_value :: FString  -- ^ type-level string value
                   , tystr_pos   :: Position -- ^ position of introduction
                   }
    deriving (Show, Generic.Data, Generic.Typeable)

data TISort
        = -- type synonym
          TItype Integer Type
        | TIdata { tidata_cons :: [Id]
                 , tidata_enum :: Bool
                 }
        | TIstruct StructSubType [Id]
          -- primitive abstract type
          -- e.g. Integer, Bit, Module, etc.
        | TIabstract
          -- Associated type function: resolved by looking up the corresponding
          -- typeclass instance.  Stores the class that defines this type function,
          -- and the indices of the ATF params and the target param within the class's
          -- parameter list.
        | TIatf { atf_class_id   :: Id     -- the class this type function belongs to
                , atf_param_idxs :: [Int]  -- index of each ATF param in the class param list
                , atf_target_idx :: Int    -- index of the result param in the class param list
                }
        deriving (Eq, Ord, Show, Generic.Data, Generic.Typeable)


data StructSubType
        = SStruct
        | SClass
        | SDataCon { sdatacon_id :: Id
                   , sdatacon_named_fields :: Bool
                   }
        | SInterface [IfcPragma]
        | SPolyWrap { spolywrap_id :: Id         -- ^ name of the type with the wrapped field
                    , spolywrap_ctor :: Maybe Id -- ^ name of the data constructor
                    , spolywrap_field :: Id      -- ^ name of the wrapped field
                    }
        deriving (Eq, Ord, Show, Generic.Data, Generic.Typeable)

type CType = Type

-- | Representation of kinds
data Kind = KStar           -- ^ kind of a simple value type
          | KNum            -- ^ kind of a simple numeric type
          | KStr            -- ^ kind of a simple string type
          | Kfun Kind Kind  -- ^ kind of type constructors (type-level function)
          | KVar Int        -- ^ generated kind variable (used only during kind inference)
    deriving (Eq, Ord, Show, Generic.Data, Generic.Typeable)

-- Used for providing partial Kind information
data PartialKind
        = PKNoInfo -- this is what makes it partial
        | PKStar
        | PKNum
        | PKStr
        | PKfun PartialKind PartialKind
        deriving (Eq, Ord, Show)

-- | A named typeclass
newtype CTypeclass = CTypeclass Id
    deriving (Eq, Ord, Show, PPrint, HasPosition, NFData)

-- | Representation of the provisos and other class constraints
data CPred = CPred { cpred_tc   :: CTypeclass  -- ^ constraint class, e.g., "Eq"
                   , cpred_args :: [CType]     -- ^ argument types
                   }
        deriving (Eq, Ord, Show)

-- Eq instances

-- | used to do the sorting of instances
-- so that overlapping matches go to the most specific
-- TAp first because it brings forward instances with larger structure
-- see the Has_tpl_n instances in the Prelude
cmp :: Type -> Type -> Ordering
-- Equal canonical words imply equality under the structural clauses below
-- (see "Canonical identity": equal words => equal key => Eq-equal leaf
-- or Eq-equal children), so EQ may be answered from the slots.  LT and GT
-- never are: every Map and Set order over types stays the structural one.
cmp x y | i >= 0, i == typeCanonWordNode y = eqByWord x y
  where i = typeCanonWordNode x
cmp (TAp f1 a1) (TAp f2 a2) = compare (f1, a1) (f2, a2)
cmp (TAp _  _)  _           = LT
cmp (TCon c1) (TCon c2) = compare c1 c2
cmp (TCon _)  (TAp _ _) = GT
cmp (TCon _)  _         = LT
cmp (TVar _) (TCon _)   = GT
cmp (TVar _) (TAp _ _)  = GT
cmp (TVar v1) (TVar v2) = compare v1 v2
cmp (TVar _)  _         = LT
cmp (TGen _ i1) (TGen _ i2) = compare i1 i2
cmp (TGen _ _) (TDefMonad _) = LT
cmp (TGen _ _) _        = GT
cmp (TDefMonad _) (TDefMonad _) = EQ
cmp (TDefMonad _) _  = GT

-- Two different normal words answer NE at once (see "Canonical
-- identity": on normal trees the word is injective on the structure
-- cmp compares), skipping the structural descent; every other pair is
-- decided by cmp.  Only (==) takes this shortcut: compare never answers
-- LT or GT from words, so every Map and Set order over types stays the
-- structural one.  x is forced before y, as cmp forces them, and a
-- normal tree is already in weak head normal form at every node.
instance Eq Type where
    x == y | eqWordOn, normalWord i, normalWord j, i /= j = neByWord x y
           | otherwise = cmp x y == EQ
      where i = typeCanonWordNode x
            j = typeCanonWordNode y

instance Eq TyVar where
    TyVar i n _ == TyVar i' n' _  =  (n, i) == (n', i')

-- TyCon comparison and the one-tycon-per-qualified-name invariant
--
-- BSC's front end enforces one type constructor per qualified name, so a
-- qualified Id determines its (kind, sort) payload.  The invariant holds
-- ONLY for qualified names: unqualified TyCons exist during scope
-- resolution, where the same base name may denote different constructors.
-- The tconcheck build step (src/comp/tconcheck.hs) verifies the compiler's
-- handwritten copies of Prelude tycon payloads against the compiled
-- Prelude, keeping the invariant checkable.
--
-- Eq is therefore scoped by qualification:
--  * both ids qualified: the invariant's regime.  Comparison is by Id;
--    the payloads are determined by the name (checker-verified), so a
--    payload comparison would be a dead tail.
--  * either id unqualified: resolution-era fuzz, where the invariant is
--    undefined; the kind comparison is kept as a hedge.
--
-- Two pre-existing quirks, both deliberate and unchanged:
--  * Eq is non-transitive: qualEq compares by base name alone when either
--    side is unqualified, so P.T == T and T == Q.T but P.T /= Q.T.
--  * Eq and Ord disagree: Ord compares the full qualifier on both sides
--    (a lawful total order), while Eq admits the qualEq fuzz.  Containers
--    key on Ord.
instance Eq TyCon where
    TyCon i k _ == TyCon i' k' _  =  qualEq i i' && (bothQualified i i' || k == k')
    TyNum i _   == TyNum i' _     =  i == i'
    TyStr s _   == TyStr s' _     =  s == s'
    _           == _              =  False

-- both ids carry a package qualifier (see the invariant note above)
bothQualified :: Id -> Id -> Bool
bothQualified i i' = not (isUnqualId i) && not (isUnqualId i')

-- Ord instances

instance Ord Type where
    compare x y = cmp x y

instance Ord TyVar where
    TyVar i n _ <= TyVar i' n' _  =  (n, i) <= (n', i')
    TyVar i n _ <  TyVar i' n' _  =  (n, i) <  (n', i')
    TyVar i n _ >= TyVar i' n' _  =  (n, i) >= (n', i')
    TyVar i n _ >  TyVar i' n' _  =  (n, i) >  (n', i')
    TyVar i n _ `compare` TyVar i' n' _  =  (n, i) `compare` (n', i')

instance Ord TyCon where
    -- lexicographic on (base, qual); within a (base, qual) bucket either
    -- both ids are qualified -- unique by the invariant documented at Eq,
    -- so EQ without consulting the kinds -- or both are unqualified, where
    -- the kind comparison is kept, as in Eq.  This remains a lawful total
    -- order.
    TyCon i k _ `compare` TyCon i' k' _   =
        case (getIdBase i, getIdQual i) `compare` (getIdBase i', getIdQual i') of
          EQ -> if bothQualified i i' then EQ else compare k k'
          o  -> o
    TyCon _ _ _ `compare` TyNum _  _      =  LT
    TyCon _ _ _ `compare` TyStr _  _      =  LT
    TyNum _ _   `compare` TyCon _  _  _   =  GT
    TyNum i _   `compare` TyNum i' _      =  i `compare` i'
    TyNum _ _   `compare` TyStr _  _      =  LT
    TyStr _ _   `compare` TyCon _  _  _   =  GT
    TyStr _ _   `compare` TyNum _  _      =  GT
    TyStr s _   `compare` TyStr s' _      =  s `compare` s'

-- ==============================
-- Canonical identity
--
-- A TCon_ or TAp_ node carries a slot holding -1 (no word) or a word
--
--     w = 2*n + normalBit
--
-- where n is an arrival-order counter from a process-global table and
-- normalBit is 1 iff the node is normal (below).  The slot is metadata
-- only.  It is never serialized (BinData writes and reads the fields and
-- rebuilds through the smart constructors, which recompute it), never
-- printed, never ordered on, and never used to replace a node: a smart
-- constructor builds a NEW node holding the caller's children and the
-- caller's TyCon untouched, so positions, kinds, sort payloads and Id
-- properties stay exactly where the caller put them.  The word is a pure
-- function of the node's structure under the structural Eq Type, so its
-- only uses are an equality shortcut (equal words => Eq-equal) and the
-- two flags:
--
--   ground: w >= 0.  No TVar, TGen or TDefMonad anywhere inside.
--   normal: ground and odd w.  Additionally no type synonym (TItype) or
--           associated type function (TIatf) head, no primitive type
--           function (TAdd, ..., TStrCat, TNumToStr) and no Id__ anywhere
--           inside, so expandSyn, normTAp and the typechecker's type
--           function reduction are the identity on the type.
--
-- Keys.  A TAp is keyed by its children's words; a TyCon by (qualifier,
-- base, normal bit); a TyNum by its value (Eq ignores the position).
-- The kind and the sort payload are not in a TyCon's key because Eq
-- TyCon ignores both for qualified names.  The normal bit IS in the key
-- because the same qualified name arrives with different sort tags --
-- cTCon builds TIabstract placeholders for names the symbol table knows
-- as synonyms -- and the shortcuts must see the sort tags of the tree
-- they are given, as the walks they replace do (expandSyn expands a
-- TItype head and leaves a placeholder alone).  Refused (slot -1): an
-- unqualified TyCon (the one-tycon-per-qualified-name invariant behind
-- Eq TyCon does not cover it), a TyStr leaf (its FString may be an
-- unforced mkFString thunk -- cTCon, Pred.apTFun, normTAp -- and forcing
-- it would intern a string otherwise interned later), and a TAp with a
-- refused child.  Equal words therefore imply Eq-equal trees.  The
-- converse fails only across a name's normal and non-normal arrivals,
-- so NE is answered from the words only when both are normal (Eq Type):
-- a normal leaf's key is the (qualifier, base) that Ord TyCon compares
-- for two qualified names, or the TyNum value, and a normal TAp's key
-- is its normal children's words, so by induction two normal trees that
-- cmp finds EQ have the same key at every level and the same word,
-- and different normal words mean cmp is not EQ.
--
-- Forcing.  Computing a word forces both children to WHNF and, for a
-- TyCon leaf, the TyCon, its Id (two strict FString fields), the sort
-- constructor and the isUnqualId, isPrimTFunName and idId tests; nothing
-- else (not the kind, not the sort payload, not positions or props).
-- FString equality and ordering are by intern id, so forcing an Id whose
-- strings have never been interned would intern them earlier than the
-- structural paths do, which can permute the iteration order of any
-- Id-keyed Map.  The gate keeps words off while a package's parser
-- thunks, GenWrap, Deriving and symbol-table builds run: compilePackage
-- (app/bsc.hs) shuts it on entry and opens it just before DFctxreduce,
-- for each package it compiles (bsc -u compiles several in one process).
-- From then on every TyCon Id reachable from a type is a symbol-table
-- key, a Prelude PreId or a .bo-decoded Id whose strings the eager
-- string-table read already interned, so no type constructor's name is
-- interned by a word computation.  Forcing a child thunk can still run
-- typechecker code earlier than the thunk's consumer would (a
-- substitution's TyVar comparisons, say), and that code interns two
-- kinds of generated name: a fresh _tctyvarN, since Eq TyVar forces the
-- Ids of two variables with equal numbers, and the _tctempN that a
-- newTVar's lazy position retains (TIMonad: pos = getPosition x with x
-- the value binder; id_pos is strict, so forcing the TyVar's Id forces
-- the binder's Id and interns its name).  The witness that no output
-- can have moved with them has three parts, under -trace-ctype-stats:
--
--   intern_other = 0: every string interned while a word was computed
--     is exactly _tctyvarN or _tctempN (withInternDelta prints anything
--     else).  This covers every user name, every TyStr literal, every
--     PreString and the _tcdictN dictionary binders.
--   letrec_offender = 0: no binder of a letrec group with more than
--     one binder has a name interned while a word was computed
--     (checkLetrecBinders, from IConv.iConvLet).  Two passes between
--     TCheck and IExpand sort the binders of such a group with ties
--     broken on Ord Id.  Simplify.selectSimple runs SCC.scc and
--     SCC.tsort over a Cletrec's definitions, but only under
--     -simplify-csyntax, which is off by default, and the order it
--     leaves cannot survive anyway: SCC.ntsort sorts its input before
--     building the queue it pops from.  So the emitted order is
--     decided by iConvLet's SCC.tsort alone, which breaks in-degree
--     ties on Ord Id: a binder interned early could reorder the
--     emitted definitions, and IExpand's heap order and the generated
--     names with it; a _tctempN in a one-binder group cannot.
--   .bo identity with a -no-ctype-ids compile (canonids.exp): a
--     _tctyvarN Id never decides an order in this process -- Ord TyVar
--     compares the number first (user type variables share noTyVarNo
--     and are compared by Id, but their names are interned at parse
--     time, before the gate), and the Id leaves the TyVar only into
--     IConv's ITVar, whose one order-sensitive container is the
--     package's ATF cache, serialized into the .bo in key order.
--
-- intern_delta and intern_tctemp count the admitted _tctyvarN and
-- _tctempN strings, and letrec_group the groups checked, for
-- information.  The names do reach one output: the ECO capture's
-- .intern snapshot serializes the whole string table, so an earlier or
-- added name shifts the ids of the strings interned after it.  That is
-- harmless: replay restores the table only to decode the .ba and never
-- runs the typechecker, and a shift that keeps every other pair's
-- relative order changes no Ord Id answer, so the capture's .ba is
-- decoded-identical and its .eco-ast.bin, .eco-graph and .output-0.v
-- byte-identical to a compile without words.
--
-- Nodes built while the gate is shut carry -1: final for an interior
-- node (the typechecker rebuilds every spine it touches), but a leaf's
-- word is recomputed on demand by typeCanonWord, because the Type.hs
-- CAFs (tArrow, tBit, ...) are first forced before the gate and would
-- otherwise refuse every arrow type.
--
-- Table.  Ids, Integers and FStrings only, never a Type, so no type graph
-- is retained.  Never reset: an entry is a fact about a name or a pair
-- of words and cannot go stale.  Same access discipline as
-- IType.internTable (readIORef fast path, atomicModifyIORef' re-check on
-- a miss).  Other executables (bluetcl, dumpbo, tconcheck) never open
-- the gate, so every type they build carries -1 and no word is ever
-- consulted in them.

data CKey = CKAp {-# UNPACK #-} !Int {-# UNPACK #-} !Int   -- child words
          | CKCon !FString !FString !Bool                  -- qualifier, base, normal
          | CKNum !Integer
          deriving (Eq, Ord)

data CanonTable = CanonTable !(M.Map CKey Int) {-# UNPACK #-} !Int

{-# NOINLINE canonTable #-}
canonTable :: IORef CanonTable
canonTable = unsafePerformIO $ newIORef (CanonTable M.empty 0)

-- Shut while a package's front end runs (compilePackage shuts it on
-- entry) and open from enableCanonIds, just before DFctxreduce, to the
-- end of that package; see Forcing above.
{-# NOINLINE canonGate #-}
canonGate :: IORef Bool
canonGate = unsafePerformIO $ newIORef False

-- Open the gate (compilePackage, immediately before DFctxreduce).  The
-- hidden flag -no-ctype-ids keeps it shut, for A/B measurement and as a
-- fallback to the structural paths.
enableCanonIds :: IO ()
enableCanonIds =
    unless ("-no-ctype-ids" `elem` progArgs) $ writeIORef canonGate True

-- Shut the gate (compilePackage, on entry).  The table is kept: a word
-- is a fact about a name or a pair of words, so a node that got one
-- under an earlier opening keeps it, and nodes built while the gate is
-- shut carry -1.
disableCanonIds :: IO ()
disableCanonIds = writeIORef canonGate False

normalWord :: Int -> Bool
normalWord w = w >= 0 && odd w

-- Find or allocate the word of a key; the normal bit is part of every
-- key, so the word's parity is a function of the key.
lookupOrInsert :: CKey -> Bool -> IO Int
lookupOrInsert key normal = do
    CanonTable m0 _ <- readIORef canonTable
    case M.lookup key m0 of
      Just w  -> bumpCTypeStat TableHit >> return w
      Nothing -> do
        (w, new) <- atomicModifyIORef' canonTable alloc
        bumpCTypeStat (if new then TableNew else TableHit)
        return w
  where
    alloc st@(CanonTable m n) =
        case M.lookup key m of
          Just w  -> (st, (w, False))
          Nothing -> let w = 2 * n + (if normal then 1 else 0)
                     in  (CanonTable (M.insert key w m) (n + 1), (w, True))

-- The key and normal bit of a leaf, or the reason it is refused.
data LeafKey = LeafKey !CKey !Bool | LeafUnqual | LeafTyStr

leafKey :: TyCon -> LeafKey
leafKey (TyCon i _ sort)
    | isUnqualId i = LeafUnqual
    | otherwise = LeafKey (CKCon (getIdQual i) (getIdBase i) normal) normal
  where normal = case sort of
                   TItype _ _ -> False
                   TIatf {} -> False
                   _ -> not (isPrimTFunName i) && i /= idId
leafKey (TyNum n _) = LeafKey (CKNum n) True
leafKey (TyStr _ _) = LeafTyStr

-- The word of a leaf while the gate is open.
leafWord :: TyCon -> IO Int
leafWord c = case leafKey c of
    LeafKey key normal -> lookupOrInsert key normal
    LeafUnqual -> bumpCTypeStat RefuseUnqual >> return (-1)
    LeafTyStr -> bumpCTypeStat RefuseTyStr >> return (-1)

-- The word of a child about to be applied: its slot, or for a leaf built
-- before the gate the recomputed word.  Forces the child to WHNF.
childWord :: Type -> IO Int
childWord (TAp_ w _ _) = return w
childWord (TCon_ w c) | w >= 0 = return w
                      | otherwise = bumpCTypeStat LeafRecompute >> leafWord c
childWord _ = return (-1)

-- Under -trace-ctype-stats, every string interned while an action ran
-- is recorded in wordInterned and, the first time, counted by its
-- spelling: a typechecker type variable (exactly _tctyvarN, TIMonad's
-- enumId "tctyvar") in intern_delta, a typechecker value binder
-- (exactly _tctempN, enumId "tctemp") in intern_tctemp, anything else
-- in intern_other, printed as it is found (see Forcing above).  A
-- nested construction (childWord forcing a TAp thunk runs mkTAp inside
-- the outer action) has already recorded and counted its own strings.
withInternDelta :: IO a -> IO a
withInternDelta act
    | ctypeStatsOn = do
        n0 <- internCount
        r <- act
        n1 <- internCount
        when (n1 > n0) $ do
            seen <- readIORef wordInterned
            ss <- internedSince n0
            let new = [ (fs, s) | s <- ss, let fs = mkFString s,
                                  not (S.member fs seen) ]
                (tyvars, rest) = partition (isTcName "tctyvar" . snd) new
                (temps, others) = partition (isTcName "tctemp" . snd) rest
            writeIORef wordInterned (S.union seen (S.fromList (map fst new)))
            addCTypeStat InternDelta (length tyvars)
            addCTypeStat InternTcTemp (length temps)
            addCTypeStat InternOther (length others)
            mapM_ (\(_, s) -> hPutStrLn stderr ("ctype-stats intern_other " ++ show s))
                  others
        return r
    | otherwise = act

-- The strings interned while a word was computed, under
-- -trace-ctype-stats only (empty otherwise); read by checkLetrecBinders.
{-# NOINLINE wordInterned #-}
wordInterned :: IORef (S.Set FString)
wordInterned = unsafePerformIO $ newIORef S.empty

-- The spelling of a typechecker-generated name: "_" ++ str followed by
-- digits, and nothing else (a user's _tctyvarFoo counts as an offender).
isTcName :: String -> String -> Bool
isTcName str s = pre `isPrefixOf` s && not (null ds) && all isDigit ds
  where pre = '_' : str
        ds = drop (length pre) s

-- IConv.iConvLet passes the binders of every letrec group it has
-- ordered with SCC.tsort, and the group's conversion as the second
-- argument, which is returned.  Under -trace-ctype-stats, when the
-- group has more than one binder (tsort's in-degree tie-break on Ord Id
-- can then decide the emitted order), each binder whose base string was
-- interned while a word was computed is printed and counted in
-- letrec_offender, which must read 0, and the group in letrec_group
-- (see Forcing above).  Without the flag the binders are not forced.
{-# NOINLINE checkLetrecBinders #-}
checkLetrecBinders :: [Id] -> a -> a
checkLetrecBinders is x
    | ctypeStatsOn, multi = unsafePerformIO $ do
        bumpCTypeStat LetrecGroup
        seen <- readIORef wordInterned
        let offenders = [ i | i <- is, S.member (getIdBase i) seen ]
        addCTypeStat LetrecOffender (length offenders)
        mapM_ (\i -> hPutStrLn stderr ("ctype-stats letrec_offender "
                                       ++ show (getIdBaseString i) ++ " at "
                                       ++ prPosition (getIdPosition i)))
              offenders
        return x
    | otherwise = x
  where multi = case is of
                  (_ : _ : _) -> True
                  _ -> False

-- The smart constructors behind the TCon and TAp pattern synonyms.  They
-- run under unsafeDupablePerformIO: a duplicated evaluation would find
-- the same key and receive the same word through lookupOrInsert's
-- re-check, so the noDuplicate barrier is not needed on this hot path.
-- NB: inside this module every construction of a TCon or TAp goes
-- through here (the synonyms' builders are these functions).
{-# NOINLINE mkTCon #-}
mkTCon :: TyCon -> Type
mkTCon c = unsafeDupablePerformIO $ do
    on <- readIORef canonGate
    bumpCTypeStat TConBuilt
    if on
      then do w <- withInternDelta (leafWord c)
              return (TCon_ w c)
      else do bumpCTypeStat PreGateBuilt
              return (TCon_ (-1) c)

{-# NOINLINE mkTAp #-}
mkTAp :: Type -> Type -> Type
mkTAp f a = unsafeDupablePerformIO $ do
    on <- readIORef canonGate
    bumpCTypeStat TApBuilt
    if on
      then withInternDelta $ do
        wf <- childWord f
        wa <- childWord a
        if wf < 0 || wa < 0
          then do bumpCTypeStat RefuseChild
                  return (TAp_ (-1) f a)
          else do w <- lookupOrInsert (CKAp wf wa) (odd wf && odd wa)
                  return (TAp_ w f a)
      else do bumpCTypeStat PreGateBuilt
              return (TAp_ (-1) f a)

-- The canonical word of a type, -1 if none.  A leaf built before the gate
-- has its word recomputed from its key (while the gate is open); that is
-- a table lookup, so the comparison instances and the per-site shortcuts
-- read typeCanonWordNode instead.
typeCanonWord :: Type -> Int
typeCanonWord (TAp_ w _ _) = w
typeCanonWord (TCon_ w c) | w >= 0 = w
                        | otherwise = leafRecompute c
typeCanonWord _ = -1

{-# NOINLINE leafRecompute #-}
leafRecompute :: TyCon -> Int
leafRecompute c = unsafeDupablePerformIO $ do
    on <- readIORef canonGate
    if on then withInternDelta (bumpCTypeStat LeafRecompute >> leafWord c)
          else return (-1)

-- The slot as stored: O(1), no table access.  A leaf built before the
-- gate answers -1 here although typeCanonWord would recompute its word;
-- that only declines a shortcut (a -1 never asserts anything), and
-- only at a leaf: mkTAp reads its children through childWord, which
-- recomputes a pre-gate leaf, so an interior node's slot is final and
-- already holds everything a recompute could learn.
typeCanonWordNode :: Type -> Int
typeCanonWordNode (TAp_ w _ _) = w
typeCanonWordNode (TCon_ w _) = w
typeCanonWordNode _ = -1

-- The ground and normal flags (see above), recomputing a pre-gate leaf.
isGroundType, isNormalType :: Type -> Bool
isGroundType t = typeCanonWord t >= 0
isNormalType t = normalWord (typeCanonWord t)

-- The same flags read from the slot only (see typeCanonWordNode).
isGroundNode, isNormalNode :: Type -> Bool
isGroundNode t = typeCanonWordNode t >= 0
isNormalNode t = normalWord (typeCanonWordNode t)

-- Equality decided structurally, with no word shortcut
-- at any level: the oracle for -check-ctype-ids.
eqStructural :: Type -> Type -> Bool
eqStructural (TAp f1 a1) (TAp f2 a2) = eqStructural f1 f2 && eqStructural a1 a2
eqStructural (TCon c1) (TCon c2) = compare c1 c2 == EQ
eqStructural (TVar v1) (TVar v2) = compare v1 v2 == EQ
eqStructural (TGen _ i1) (TGen _ i2) = i1 == i2
eqStructural (TDefMonad _) (TDefMonad _) = True
eqStructural _ _ = False

-- An answer taken from two equal words.  Under -check-ctype-ids the two
-- types are also compared structurally, and a disagreement is an
-- internal error naming the site.
checkEqByWord :: String -> Type -> Type -> a -> a
checkEqByWord site x y r
    | checkCanonIds && not (eqStructural x y) =
        internalError (site ++ ": equal canonical words for unequal types: "
                       ++ show (x, y))
    | otherwise = r

-- The EQ answer of cmp's word shortcut.
eqByWord :: Type -> Type -> Ordering
eqByWord x y = checkEqByWord "CType.cmp" x y (ctypeStat CmpEqByWord x `seq` EQ)

-- The NE answer of (==)'s word shortcut, from two different normal words.
-- Under -check-ctype-ids the two types are also compared structurally,
-- and equality is an internal error.
neByWord :: Type -> Type -> Bool
neByWord x y
    | checkCanonIds && eqStructural x y =
        internalError ("CType.==: different normal canonical words "
                       ++ "for equal types: " ++ show (x, y))
    | otherwise = ctypeStat CmpNeByWord x `seq` False

-- The hidden flag -hack-no-eq-word (registered with the other
-- progArgs-queried flags in FlagsDecode.traceflags, so the command-line
-- parser accepts it; BSC_OPTIONS reaches progArgs too) keeps (==) on
-- cmp, so no NE is answered from the words.  Absent the flag the
-- shortcut is on (the default).
{-# NOINLINE eqWordOn #-}
eqWordOn :: Bool
eqWordOn = not ("-hack-no-eq-word" `elem` progArgs)

-- Counters, kept and printed (by bsc at exit) only under
-- -trace-ctype-stats; every bump is a no-op otherwise.  WriteBinChecked
-- and WriteBinGeneric belong to the .bo/.ba writer (BinData.indexBytes):
-- the indices it checked against the generic encoder under
-- -check-writebin, and the ones it encoded generically under
-- -hack-no-writebin.  The last two belong to the rnf memo of interned
-- ITypes (IType.hs, NFData IType): the first walks of interned nodes,
-- and the visits answered from a node's bit.
data CTypeStat
    = TApBuilt | TConBuilt | PreGateBuilt | TableHit | TableNew
    | RefuseChild | RefuseUnqual | RefuseTyStr | LeafRecompute
    | CmpEqByWord | CmpNeByWord | InternDelta | InternTcTemp | InternOther
    | LetrecGroup | LetrecOffender
    | SeqCTypeShortcut | ApSubShortcut | TvShortcut | ExpandSynShortcut
    | InstShortcut | MguShortcut | MatchShortcut | GetFreeTShortcut
    | ChkTApShortcut | IConvMemoHit | IConvMemoMiss
    | CSubstShortcut | CSubstNodeShortcut
    | WriteBinChecked | WriteBinGeneric
    | RnfGroundWalk | RnfGroundSkip
    deriving (Eq, Ord, Enum, Bounded)

statName :: CTypeStat -> String
statName TApBuilt = "tap_built"
statName TConBuilt = "tcon_built"
statName PreGateBuilt = "pre_gate_built"
statName TableHit = "table_hit"
statName TableNew = "table_new"
statName RefuseChild = "refuse_child"
statName RefuseUnqual = "refuse_unqual"
statName RefuseTyStr = "refuse_tystr"
statName LeafRecompute = "leaf_recompute"
statName CmpEqByWord = "cmp_eq_by_word"
statName CmpNeByWord = "cmp_ne_by_word"
statName InternDelta = "intern_delta"
statName InternTcTemp = "intern_tctemp"
statName InternOther = "intern_other"
statName LetrecGroup = "letrec_group"
statName LetrecOffender = "letrec_offender"
statName SeqCTypeShortcut = "seq_ctype_shortcut"
statName ApSubShortcut = "apsub_shortcut"
statName TvShortcut = "tv_shortcut"
statName ExpandSynShortcut = "expandsyn_shortcut"
statName InstShortcut = "inst_shortcut"
statName MguShortcut = "mgu_shortcut"
statName MatchShortcut = "match_shortcut"
statName GetFreeTShortcut = "getfreet_shortcut"
statName ChkTApShortcut = "chktap_shortcut"
statName IConvMemoHit = "iconv_memo_hit"
statName IConvMemoMiss = "iconv_memo_miss"
statName CSubstShortcut = "csubst_shortcut"
statName CSubstNodeShortcut = "csubst_node_shortcut"
statName WriteBinChecked = "writebin_checked"
statName WriteBinGeneric = "writebin_generic"
statName RnfGroundWalk = "rnf_ground_walk"
statName RnfGroundSkip = "rnf_ground_skip"

{-# NOINLINE ctypeStatsOn #-}
ctypeStatsOn :: Bool
ctypeStatsOn = "-trace-ctype-stats" `elem` progArgs

-- -check-ctype-ids: every answer taken from two equal words -- cmp's EQ
-- and Unify's mgu and match shortcuts -- is also decided structurally
-- (checkEqByWord), as is every NE that (==) answers from two different
-- normal words (neByWord), and every IConv memo hit is compared with a
-- fresh conversion of the type; a disagreement is an internal error.  The
-- one-sided shortcuts (apSub, tv, expandSyn, inst, getFreeT, chkTAp,
-- TCMisc's expTFun, normT, expandSynN and the badCon scan) return their
-- input on the ground or normal flag and are not checked here: they
-- rest on the flags' definition, and -no-ctype-ids turns them all off.
-- CSubst's shortcut has its own oracle, -check-csubst-shortcut.
{-# NOINLINE checkCanonIds #-}
checkCanonIds :: Bool
checkCanonIds = "-check-ctype-ids" `elem` progArgs

{-# NOINLINE ctypeStatTable #-}
ctypeStatTable :: IORef (IM.IntMap Int)
ctypeStatTable = unsafePerformIO $ newIORef IM.empty

addCTypeStat :: CTypeStat -> Int -> IO ()
addCTypeStat s n = when ctypeStatsOn $
    modifyIORef' ctypeStatTable (IM.insertWith (+) (fromEnum s) n)

bumpCTypeStat :: CTypeStat -> IO ()
bumpCTypeStat s = addCTypeStat s 1

-- Count an event on a pure path and return the second argument, which
-- callers pass as a value that varies per call so the bump cannot be
-- floated out as a constant.
{-# NOINLINE ctypeStat #-}
ctypeStat :: CTypeStat -> a -> a
ctypeStat s x | ctypeStatsOn = unsafePerformIO (bumpCTypeStat s >> return x)
              | otherwise = x

-- Every counter, in a fixed order, zeros included.
cTypeStats :: IO [(String, Int)]
cTypeStats = do
    m <- readIORef ctypeStatTable
    return [ (statName s, IM.findWithDefault 0 (fromEnum s) m)
           | s <- [minBound .. maxBound] ]





instance PPrint Type where
    pPrint d p (TCon (TyCon unit _ _)) | unit == idPrimUnit = text "()"
    pPrint d p (TCon c) = pPrint d 0 c
    pPrint d p (TVar i) = pPrint d 0 i
    pPrint d p (TAp (TAp (TCon pair) a) b) | isTConPair pair =
        pparen (p >= 0) (sep [pPrint d 0 a <> text ",", pPrint d (-1) b])
    pPrint d p (TAp (TAp (TCon arr) a) r) | isTConArrow arr =
        pparen (p > 8) (sep [pPrint d 9 a <+> text "->", pPrint d 8 r])
    pPrint d p (TAp e e') = pparen (p>9) $
        sep [pPrint d 9 e, pPrint d 10 e']
    pPrint d p (TDefMonad _) = text ("TDefMonad")
    pPrint d p (TGen _ n) = pparen True (text "TGen" <+> pPrint d p n)

instance NFData Type where
    rnf (TVar v) = rnf v
    rnf (TCon c) = rnf c
    rnf (TAp t1 t2) = rnf2 t1 t2
    rnf (TGen p i) = rnf2 p i
    rnf (TDefMonad _) = ()

instance HasPosition Type where
    getPosition (TVar var) = getPosition var
    getPosition (TCon con) = getPosition con
    getPosition (TAp f a) = getPosition f `bestPosition` getPosition a
    getPosition (TGen pos _) = pos
    getPosition (TDefMonad pos) = pos

instance NFData TyVar where
    rnf (TyVar i n k) = rnf3 i n k

instance HasPosition TyVar where
    getPosition (TyVar name _ _) = getPosition name

getTyVarId :: TyVar -> Id
getTyVarId = tv_name

instance PPrint TyVar where
    pPrint d _ (TyVar i _ _) = ppVarId d i

instance PPrint TyCon where
    pPrint d _ (TyCon i _ _) = ppConId d i
    pPrint d _ (TyNum i _) = text (itos i)
    pPrint d _ (TyStr s _) = text (show s)

instance NFData TyCon where
    rnf (TyCon i k s) = rnf3 i k s
    rnf (TyNum i p) = rnf2 i p
    rnf (TyStr s p) = rnf2 s p

instance HasPosition TyCon where
    getPosition (TyCon name k _) = getPosition name
    getPosition (TyNum _ pos) = pos
    getPosition (TyStr _ pos) = pos

instance HasPosition CQType where
    -- prefer t to ps, since that is a better position for BSV
    getPosition (CQType ps t) = getPosition t `bestPosition` getPosition ps

instance HasPosition CPred where
    getPosition (CPred c ts) = getPosition (c, ts)

data CQType = CQType [CPred] CType
    deriving (Eq, Ord, Show)

instance NFData CQType where
    rnf (CQType ps t) = rnf2 ps t


{-
-- should typeclass ids be equal if they are qualEq?
instance Eq CTypeclass where
  (==) (CTypeclass i) (Ctypeclass i') = qualEq i i'

instance Ord CTypeclass where
  compare (CTypeclass i) (CTypeclass i') | qualEq i i' = EQ
                                         | otherwise  = compare i i'
-}

-- This function is dangerous, it allows a CTypeclass to be "coerced" in type to a
-- bare Id, which in turn might be interpreted as something else.
typeclassId :: CTypeclass -> Id
typeclassId (CTypeclass i) = i

instance PVPrint CTypeclass where
   pvPrint d p (CTypeclass i) = pvPrint d p i

instance NFData CPred where
    rnf (CPred c ts) = rnf2 c ts

instance PPrint CQType where
    pPrint d p (CQType [] ct) = pPrint d p ct
    pPrint d p (CQType preds ct) = sep [text "(" <> sepList (map (pPrint d 0) preds) (text ",") <> text ")" <+> text "=>", pPrint d 0 ct]

instance PPrint CPred where
    pPrint d p (CPred (CTypeclass c) []) = ppConId d c
    pPrint d p (CPred (CTypeclass c) ts) = ppConId d c <+> sep (map (pPrint d (maxPrec+1)) ts)

noTyVarNo :: Int
noTyVarNo = -1

tVarKind :: Id -> Kind -> TyVar
tVarKind i k = TyVar i noTyVarNo k

tVar :: Id -> TyVar
-- XXX KVar (-42) below is a hack so that undefined is not visible
-- XXX when deriving Show
tVar i = tVarKind i (KVar (-42))

cTVar :: Id -> CType
cTVar i = TVar (tVar i)

cTVarKind :: Id -> Kind -> CType
cTVarKind name kind = TVar (tVarKind name kind)

cTVarNum :: Id -> CType
cTVarNum name = cTVarKind name KNum

cTCon :: Id -> CType
cTCon i | all isDigit s = cTNum (read s) (getIdPosition i)
        | head s == '"' = cTStr (mkFString $ read s) (getIdPosition i)
  where s = getIdString i
cTCon i = TCon (TyCon i Nothing TIabstract)

cTNum :: Integer -> Position -> CType
cTNum n pos = TCon (TyNum n pos)

isTNum :: CType -> Bool
isTNum (TCon (TyNum _ _)) = True
isTNum _ = False

getTNum :: CType -> Integer
getTNum (TCon (TyNum n _)) = n
getTNum t = internalError $ "getTNum: not a type-level integer -- " ++ (show t)

cTStr :: FString -> Position -> CType
cTStr s pos = TCon (TyStr s pos)

isTStr :: CType -> Bool
isTStr (TCon (TyStr _ _)) = True
isTStr _ = False

getTStr :: CType -> FString
getTStr (TCon (TyStr s _)) = s
getTStr t = internalError $ "getTNum: not a type-level string -- " ++ (show t)

isTVar :: Type -> Bool
isTVar (TVar _) = True
isTVar _ = False

isTCon :: Type -> Bool
isTCon (TCon _) = True
isTCon _ = False

isGeneratedTVar :: TyVar -> Bool
isGeneratedTVar (TyVar _ n _) = n /= noTyVarNo

isIfc :: StructSubType -> Bool
isIfc SInterface {} = True
--isIfc SStruct = True -- XXX why??
isIfc _ = False

isInterface :: CType -> Bool
isInterface t | Just (TyCon _ _ (TIstruct s _)) <- leftTyCon t = isIfc s
isInterface _ = False

isDictType :: CType -> Bool
isDictType t | Just (TyCon _ _ (TIstruct SClass _)) <- leftTyCon t = True
isDictType _ = False

isDictFun :: CType -> Bool
isDictFun t = all isDictType (res:args)
  where (args, res) = getArrows t

isUpdateable :: StructSubType -> Bool
isUpdateable SStruct = True
isUpdateable SInterface {} = True
isUpdateable _ = False

noType :: Type
noType = TGen noPosition (-1)

getCQArrows :: CQType -> ([CQType], CQType)
getCQArrows (CQType preds ctype) =
    let (args, result) = getArrows ctype
    in  (map (CQType preds) args, CQType preds result)

getArrows :: Type -> ([Type], Type)
getArrows t = getArrowsAccum [] t
    where getArrowsAccum ts (TAp (TAp (TCon arr) a) r) | isTConArrow arr
                              = getArrowsAccum (a:ts) r
          getArrowsAccum ts r = (reverse ts, r)

getRes :: Type -> Type
getRes t = snd (getArrows t)

isTConArrow :: TyCon -> Bool
isTConArrow (TyCon i _ _) =  i == idArrow noPosition
isTConArrow t = internalError("isTConArrow: not TCon " ++ show t)

isTConPair :: TyCon -> Bool
isTConPair (TyCon i _ _) =  i == idPrimPair
isTConPair t = internalError("isTConPair: not TCon " ++ show t)

-- is a type a specific TCon with no arguments
isTypeTConNoArgs :: Id -> Type -> Bool
isTypeTConNoArgs cid (TCon (TyCon i _ _)) | (i == cid) = True
isTypeTConNoArgs _ _ = False

-- is a type a specific TCon with arguments
isTypeTConArgs :: Id -> Type -> Bool
isTypeTConArgs cid (TAp (TCon (TyCon i _ _)) _) | (i == cid) = True
isTypeTConArgs _ _ = False

isTypeBit, isTypeString, isTypePrimAction, isTypeAction :: Type -> Bool
isTypeBit          = isTypeTConArgs   idBit
isTypeString       = isTypeTConNoArgs idString
isTypePrimAction   = isTypeTConNoArgs idPrimAction
isTypeAction       = isTypeTConNoArgs idAction

isTypeActionValue, isTypeActionValue_, isTypeUnit :: Type -> Bool
isTypeActionValue  = isTypeTConArgs   idActionValue
isTypeActionValue_ = isTypeTConArgs   idActionValue_
isTypeUnit         = isTypeTConNoArgs  idPrimUnit

getActionValueArg :: Type -> Type
getActionValueArg (TAp (TCon (TyCon i _ _)) arg) | (i == idActionValue) = arg
getActionValueArg t = internalError ("getActionValueArg: " ++ ppReadable t)

-- These are used during foreign function processing to determine if arguments
-- and return values are polymorphic or of a known size.
isTypePolyBit :: Type -> Bool
isTypePolyBit (TAp (TCon (TyCon i _ _)) (TAp (TCon (TyCon i' _ _)) arg))
  | (i == idActionValue) || (i == idActionValue_), (i' == idBit) = isTVar arg
isTypePolyBit (TAp (TCon (TyCon i _ _)) arg)
  | (i == idBit) || (i == idActionValue) || (i == idActionValue_) = isTVar arg
isTypePolyBit _ = False

-- Note that this is only used for foreign functions, so it does not currently handle tuples of Bits
bitWidth :: Type -> Integer
bitWidth (TAp (TCon (TyCon i _ _)) (TAp (TCon (TyCon i' _ _)) arg))
  | ((i == idActionValue) || (i == idActionValue_)) &&
    (i' == idBit) &&
    (isTNum arg) = getTNum arg
bitWidth (TAp (TCon (TyCon i _ _)) arg)
  | (i == idBit) && (isTNum arg) = getTNum arg
bitWidth t =
  internalError $ "bitWidth: not a Bit type of known width -- " ++ (show t)


cTApplys :: CType -> [CType] -> CType
cTApplys t ts = foldl TAp t ts

leftCon :: CType -> Maybe Id
leftCon (TAp f _) = leftCon f
leftCon (TCon (TyCon i _ _)) = Just i
leftCon _ = Nothing

leftTyCon :: CType -> Maybe TyCon
leftTyCon (TAp f _) = leftTyCon f
leftTyCon (TCon tc) = Just tc
leftTyCon _ = Nothing

-- Accumulates on the way down, so building the argument list costs one cons
-- per spine level rather than an append.
tyConArgs :: CType -> [CType]
tyConArgs t0 = go t0 []
  where go (TAp f a) as = go f (a:as)
        go (TCon _) as = as
        go t _ = internalError("tyConArgs: " ++ show t)

allTyCons :: CType -> [TyCon]
allTyCons (TCon c) = [c]
allTyCons (TAp f a) = allTyCons f `union` allTyCons a
allTyCons _ = []

getTConName :: TyCon -> Maybe Id
getTConName (TyCon i _ _) = Just i
getTConName (TyNum {}) = Nothing
getTConName (TyStr {}) = Nothing

allTConNames :: CType -> [Id]
allTConNames = mapMaybe getTConName . allTyCons

-- Accumulates the arguments of any head on the way down, so a split costs one
-- cons per spine level rather than an append.
splitTAp :: CType -> (CType, [CType])
splitTAp t0 = go t0 []
  where go (TAp f a) as = go f (a:as)
        go t as = (t, as)

-- Copied from the IType reduction rules (now IType.mkITAp)
normTAp :: Type -> Type -> Type
normTAp (TAp (TCon (TyCon op _ _)) (TCon (TyNum x xpos))) (TCon (TyNum y ypos))
        | isJust (res) = cTNum (fromJust res) (getPosition op)
  where res = opNumT op [x, y]

normTAp (TCon (TyCon op _ _)) (TCon (TyNum x xpos))
        | isJust (res) = cTNum (fromJust res) (getPosition op)
  where res = opNumT op [x]

normTAp (TAp (TCon (TyCon op _ _)) (TCon (TyStr x xpos))) (TCon (TyStr y ypos))
        | isJust (res) = cTStr (fromJust res) (getPosition op)
  where res = opStrT op [x, y]

normTAp (TCon (TyCon op _ _)) (TCon (TyNum x xpos))
        | op == idTNumToStr = cTStr (mkNumFString x) (getPosition op)

normTAp f a = TAp f a

isATFAp :: Type -> Bool
isATFAp t0 =
    let (f, as) = splitTAp t0
    in case f of
        TCon (TyCon _ _ (TIatf { atf_param_idxs = pIdxs })) -> length as == length pIdxs
        _ -> False

getTypeKind :: Type -> Maybe Kind
getTypeKind (TVar (TyVar _ _ k))  = Just k
getTypeKind (TCon (TyCon _ mk _)) = mk
getTypeKind (TCon (TyNum _ _)) = Just KNum
getTypeKind (TCon (TyStr _ _)) = Just KStr
getTypeKind (TAp t1 t2) = case (getTypeKind t1) of
                            Just (Kfun k1 k2) -> Just k2
                            _ -> Nothing -- don't know or isn't Kfun
getTypeKind _ = Nothing

----

-- KIMisc.newKVar starts at this number
baseKVar :: Int
baseKVar = 1000

isKVar :: Kind -> Bool
isKVar (KVar _) = True
isKVar _ = False

-- Display the kind variable with letters
showKVar :: Int -> String
showKVar v =
    let
        makeDigit x = chr (x + 97)  -- 97 = ASCII a

        showDigits :: Int -> String
        showDigits x | (x < 26) = [makeDigit x]
        showDigits x = (showDigits (x `div` 26)) ++ [makeDigit (x `mod` 26)]
    in
        if (v < baseKVar)
        then (itos v)
        else (showDigits (v - baseKVar))

-- this differs from the version in KIMisc because it does not include the kind of the result
getArgKinds :: Kind -> [Kind]
getArgKinds (Kfun a b) = a : getArgKinds b
getArgKinds _ = []

getResKind :: Kind -> Kind
getResKind (Kfun a b) = getResKind b
getResKind k = k

mkKFun :: [Kind] -> Kind -> Kind
mkKFun []     k = k
mkKFun (a:as) k = Kfun a (mkKFun as k)

instance PPrint Kind where
    pPrint _ _ KStar = text "*"
    pPrint _ _ KNum = text "#"
    pPrint _ _ KStr = text "$"
    pPrint d p (Kfun l r) = pparen (p>9) $ pPrint d 10 l <+> text "->" <+> pPrint d 9 r
    pPrint _ _ (KVar i) = text (showKVar i)

instance NFData Kind where
    rnf KStar = ()
    rnf KNum = ()
    rnf KStr = ()
    rnf (Kfun k1 k2) = rnf2 k1 k2
    rnf (KVar n) = rnf n

----

instance PPrint PartialKind where
    pPrint _ _ PKNoInfo = text "?"
    pPrint _ _ PKStar = text "*"
    pPrint _ _ PKNum = text "#"
    pPrint _ _ PKStr = text "$"
    pPrint d p (PKfun l r) =
        pparen (p>9) $ pPrint d 10 l <+> text "->" <+> pPrint d 9 r

instance NFData PartialKind where
    rnf PKNoInfo = ()
    rnf PKStar = ()
    rnf PKNum = ()
    rnf PKStr = ()
    rnf (PKfun k1 k2) = rnf2 k1 k2

----

instance PPrint TISort where
    pPrint d p (TItype n t) = pparen (p>0) $ text "TItype" <+> pPrint d 0 n <+> pPrint d 1 t
    pPrint d p (TIdata is enum) = pparen (p>0) $ text (if enum then "TIdata (enum)" else "TIdata") <+> pPrint d 1 is
    pPrint d p (TIstruct ss is) = pparen (p>0) $ text "TIstruct" <+> pPrint d 1 ss <+> pPrint d 1 is
    pPrint d p (TIabstract) = text "TIabstract"
    pPrint d p (TIatf { atf_param_idxs = pIdxs, atf_class_id = cls }) =
        pparen (p>0) $ text "TIatf" <+> pPrint d 0 (length pIdxs) <+> pPrint d 0 cls

instance NFData TISort where
    rnf (TItype i t) = rnf2 i t
    rnf (TIdata is enum) = rnf2 is enum
    rnf (TIstruct ss is) = rnf2 ss is
    rnf (TIabstract) = ()
    rnf (TIatf { atf_class_id = c, atf_param_idxs = ps, atf_target_idx = t }) =
        rnf3 c ps t

instance PPrint StructSubType where
    pPrint _ _ ss = text (show ss)

instance NFData StructSubType where
    rnf SStruct = ()
    rnf SClass = ()
    rnf (SDataCon i nm) = rnf2 i nm
    rnf (SInterface ps) = rnf ps
    rnf (SPolyWrap i con field) = rnf3 i con field

-- Force evaluation of a Ctype
seqCType :: CType -> CType
-- A ground node was built with the gate open, which forced its spine and
-- its leaves' TyCons to WHNF (childWord, leafKey); the walk below would
-- find everything evaluated and return t.
seqCType t | isGroundNode t = ctypeStat SeqCTypeShortcut t
seqCType t@(TVar v) = seq v t
seqCType t@(TCon c) = t
seqCType t@(TAp t1 t2) = seq (seqCType t1) (seq (seqCType t2) t)
seqCType t@(TGen _ _) = t
seqCType t@(TDefMonad _) = t

----
setTypePosition :: Position -> Type -> Type
setTypePosition pos (TVar (TyVar id n k)) = (TVar (TyVar (setIdPosition pos id) n k))
setTypePosition pos (TCon (TyCon id k s)) = (TCon (TyCon (setIdPosition pos id) k s))
setTypePosition pos (TCon (TyNum n _)) = (TCon (TyNum n pos))
setTypePosition pos (TCon (TyStr s _)) = (TCon (TyStr s pos))
setTypePosition pos (TAp f a) = (TAp (setTypePosition pos f) (setTypePosition pos a))
setTypePosition pos (TGen _ n)    = (TGen pos n)
setTypePosition pos (TDefMonad _) = (TDefMonad pos)
