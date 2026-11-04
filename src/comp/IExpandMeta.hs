module IExpandMeta(
        -- sites
        SiteKind(..), SiteKey(..), idSiteKey, primSiteKey, rootSiteKey, boundSiteKey,
        SiteRow(..), SiteRef(..), RecKey(..), SiteTable(..), siteRow, siteTableSize,
        siteIdsFromRows,
        -- frames, binders and cell headers
        Frame, Binder(..), CellMeta, noBinder, headSiteOf, cellDefSite,
        bitCell, bitCellType,
        -- validity
        TypeTable(..), typeIdsFromRows, TypeKey(..), holeFree, bitWidth,
        rawValidity, validityView, validityTiers, meetValidity, Tier(..),
        typeValidity, coercionValidity, constructedValidity, windowValidity,
        refineMeta, RepackKind(..), Repack(..), recordRepack,
        -- the store and the hooks the evaluator calls
        MetaStore, newMetaStore, lookupSite, mintSite, pushFrame, pushRoot,
        swapFrame, swapCellFrame, stampMeta, aliasUnion, readRoots,
        -- the tables after elaboration and the query interface, read by
        -- the rebuild and the dump alone
        MetaTables(..), AliasMap, readMetaTables,
        siteOf, frameOf, binderOf, chainOf, validityOf,
        aliasClassOf, aliasMembers,
        renderSite, renderDump,
        -- the module metadata table, built by the rebuild
        ContextTable(..), emptyContextTable, contextTableSize, internFrame,
        contextChain, contextIdsFromRows, aliasUnions, noCell, DefRecord(..),
        ModuleMeta(..), RepackFact(..), undefKindCode, emptyModuleMeta,
        forceModuleMeta, defNameString, sideFileSchema, renderSideFile,
        -- the ledger the packages carry
        MetaLedger(..), emptyMetaLedger, PassTag(..), MetaEvent(..),
        appendEvent, resolveId, renderLedger,
        -- the census (-metadata-census)
        Census, readCensus, recordUnfolding, recordCompletion, aliasClassCount,
        renderCensus, checkProfile, checkKeptPositions, checkNameHints,
        checkBitGate
        ) where

import Control.Monad(when)
import Data.Bits(bit, (.|.), (.&.), complement, xor, testBit)
import Data.Char(isAlphaNum)
import Data.Data(Data(..), mkNoRepType)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List(intercalate, sortBy)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe(mapMaybe)
import Data.Ord(comparing)
import qualified Data.Set as S
import System.FilePath(takeFileName)

import ErrorUtil(internalError)
import Eval(NFData(..), rnf2, rnf3, rnf4, rnf5)
import FStringCompat(FString, getFString)
import Id(Id, getIdBase, getIdQual, getIdPosition, getIdString)
import IType(IType(..))
import PPrint(PPrint(..), empty, ppString)
import Position(Position(..), noPosition, isPreludePosition)
import PreIds(idBit, idUInt, idInt, idBool)
import Prim(PrimOp)
import Undefined(UndefKind(..))

-- ===============
-- The evaluator metadata store
--
-- The store records, for every heap cell, the lexical site of the
-- expression the cell was created for, the chain of definition
-- unfoldings it was created under (the frame), rooted at a rule, a
-- method or an instantiation, the binder the cell's value was bound
-- to (a parameter, or the index of a field or operand it was moved
-- into), the alias class of cells that hold one value, and for a
-- cell of type Bit n its validity (the one type whose encoding the
-- bits are known to satisfy, or raw, with the tier of that
-- knowledge).  It is written by the
-- evaluator through the hooks below and read only through the query
-- interface: by the rebuild at the end of elaboration, which also
-- builds the module metadata table from it (IExpand.buildModuleMeta;
-- the side-file renderer, renderSideFile, reads that table), and by
-- the -metadata-dump printer.
--
-- Rules that keep the store invisible to the generated output:
--
--  * The store creates no interned string.  Site keys hold FStrings and
--    Positions that already exist (compared by intern id) or a plain
--    String that is never interned; rendering reads existing strings.
--  * Every field is strict; a frame points only at frames already built.
--  * Nothing here allocates a heap cell, forces a cell or reads one.
--  * Under -metadata-scramble every number the store mints (site ids
--    and their fact bits, frame ids, counts, the row bits) is perturbed
--    with a per-run seed where it is created, so an evaluator decision
--    that read the store would change the output of the scramble run.
--    The dump is then suppressed, because the store's content is
--    garbage by design.

-- The kind of expression a site stands for.  SKBound is a boundary
-- constant (a port, a parameter, a clock, a literal, ...) whose
-- identifier the stamp does not read, because the evaluator builds some
-- of those identifiers lazily from strings that may not be interned yet;
-- all such cells share one row.  SKRule, SKMethodRoot and SKInst are
-- the roots of frame chains.  SKBinder and SKField are the binders a
-- cell's value can be bound to: the parameter an argument was heaped
-- for, and the index of the field or operand a value was moved into;
-- such a site appears only in a header's binder field, never as the
-- site a cell is created for.
data SiteKind = SKDef | SKPrim | SKCon | SKMethod | SKForeign | SKLambda
              | SKBound | SKRule | SKMethodRoot | SKInst
              | SKBinder | SKField
        deriving (Eq, Ord, Show, Enum, Bounded)

-- A site key.  Identifier-based keys compare by intern id and position
-- (O(1) per component); a primitive is keyed on its operator alone,
-- because the evaluator builds primitive heads from PreIds whose strings
-- need not have been interned yet and the stamp must not force them; a
-- root is keyed on a name string that is read, never interned; a moved
-- field or operand is keyed on its index.
data SiteKey
  = IdKey !FString !FString !Position !SiteKind  -- base, qualifier, position, kind
  | PrimKey !PrimOp
  | RootKey !String !SiteKind
  | BoundKey
  | FieldKey !Int
        deriving (Eq, Ord)

idSiteKey :: SiteKind -> Id -> SiteKey
idSiteKey k i = IdKey (getIdBase i) (getIdQual i) (getIdPosition i) k

primSiteKey :: PrimOp -> SiteKey
primSiteKey = PrimKey

-- The name is forced here so that the key, and the row built from it,
-- retain no unevaluated structure.
rootSiteKey :: SiteKind -> String -> SiteKey
rootSiteKey k name = length name `seq` RootKey name k

boundSiteKey :: SiteKey
boundSiteKey = BoundKey

siteKeyKind :: SiteKey -> SiteKind
siteKeyKind (IdKey _ _ _ k) = k
siteKeyKind (PrimKey {}) = SKPrim
siteKeyKind (RootKey _ k) = k
siteKeyKind BoundKey = SKBound
siteKeyKind (FieldKey {}) = SKField

-- The binder a cell is created with: none; the parameter an argument
-- is bound to, whose identifier is in weak head normal form (a source
-- binder, or a clone that substitution builds with a bang) so that
-- keying on it interns nothing; or the index of the field or operand a
-- value is moved into when a tuple or a lazy application is evaluated.
data Binder = NoBinder | BoundTo !Id | MovedTo !Int

-- A site row: the key and the facts recorded when the site was minted.
-- sr_useSite says that the reference carried an occurrence position
-- different from the definition's own (an imported definition); it is
-- False for package-local references, which carry the definition's
-- position, and for every kind but SKDef.  sr_recursive says the
-- definition is a recursion point (a nontrivial component of its
-- package's definition graph, or a self-reference); only such sites
-- enter a frame's recursive-site map.
data SiteRow = SiteRow { sr_key :: !SiteKey,
                         sr_useSite :: !Bool,
                         sr_recursive :: !Bool }

-- The id of a minted site and the one fact a push needs (its recursive
-- bit), kept beside the key so that a lookup hit reads nothing else.
data SiteRef = SiteRef {-# UNPACK #-} !Int !Bool

-- st_recKeys maps the site of a recursion point's reference to the key
-- its iterations are counted under, the site of the definition itself
-- (its identifier at no position), and records whether the reference
-- is a use site; a reference at the definition's own position, or at
-- no position, has no entry and is its own key.  See pushFrame for how
-- the two facts decide whether a push continues a loop or nests one.
data SiteTable = SiteTable { st_next :: {-# UNPACK #-} !Int,
                             st_ids :: !(M.Map SiteKey SiteRef),
                             st_rows :: !(IM.IntMap SiteRow),
                             st_recKeys :: !(IM.IntMap RecKey) }

-- the definition key of a recursion point's reference and whether the
-- reference is a use site (its position is not the definition's own)
data RecKey = RecKey {-# UNPACK #-} !Int !Bool

emptySiteTable :: SiteTable
emptySiteTable = SiteTable 1 M.empty IM.empty IM.empty

siteTableSize :: SiteTable -> Int
siteTableSize t = IM.size (st_rows t)

-- Three high bits of a site id carry facts the evaluator's hot paths
-- test without a row lookup.  Bit 58: the site is a definition site.
-- Bit 59: that definition is a recursion point.  Bit 60: the site was
-- borrowed from the cell at the head of an application whose head is a
-- heap reference (the application's own site is not known on the
-- mirror, the node carries no annotation).  Row lookups mask them.
-- The scramble seed (a clock reading below 2^41) does not reach them
-- by xor, so each is set from its fact through perturbBool (insertSite,
-- headSiteOf) and the poison run inverts all three; cellDefSite, the
-- one reader on the evaluator's side, then decides every push the
-- other way, which is store state and must not reach the output.
defSiteBit, recSiteBit, headSiteBit :: Int
defSiteBit = bit 58
recSiteBit = bit 59
headSiteBit = bit 60

siteBits :: Int
siteBits = defSiteBit .|. recSiteBit .|. headSiteBit

siteRow :: SiteTable -> Int -> Maybe SiteRow
siteRow t s = IM.lookup (plainSite s) (st_rows t)

-- A frame: one definition unfolding, or a root.  fr_count is the
-- iteration count of a recursion point (one more than the count of the
-- nearest iteration of the same loop on the chain); fr_rec maps every
-- recursion point on the chain, by definition key, to its nearest
-- iteration: the count, the frame it was pushed under and the site that
-- entered the loop, so a push finds its predecessor in O(log n) without
-- walking the chain.  The map entry records the count and the parent
-- instead of the frame itself, so that a frame never points at itself
-- through its own map.  TopFrame is the register's value before any
-- root frame is pushed: module arguments, the instantiation of the
-- top module's state.
data Frame = TopFrame
           | Frame { fr_id :: {-# UNPACK #-} !Int,
                     fr_site :: {-# UNPACK #-} !Int,
                     fr_count :: {-# UNPACK #-} !Int,
                     fr_parent :: !Frame,
                     fr_rec :: !(IM.IntMap RecEntry) }

-- the count, the parent frame and the entry site of a recursion
-- point's nearest iteration on the chain
data RecEntry = RecEntry {-# UNPACK #-} !Int !Frame {-# UNPACK #-} !Int

frameRec :: Frame -> IM.IntMap RecEntry
frameRec TopFrame = IM.empty
frameRec (Frame { fr_rec = m }) = m

-- compact, so that a derived Show of a heap cell stays printable
instance Show Frame where
    show TopFrame = "top"
    show (Frame { fr_id = i, fr_site = s, fr_count = c }) =
        "frame " ++ show i ++ " site " ++ show s ++ " count " ++ show c

-- The header every heap cell carries: the site of the expression the
-- cell was created for, the frame current when it was created, the
-- site of its binder (noBinder when the cell was created for no
-- parameter, field or operand) and the validity of its value
-- (rawValidity, or a type-window row with tier bits, under the gate
-- bit that says the cell's type admits facts, bitCellType).  The site,
-- frame, binder and gate are written once, at allocation, and copied
-- across every state transition of the cell; the validity is refined
-- when the cell's value is known (refineMeta: a value lands in the
-- cell, a coercion reads it, a boundary declares it) and otherwise
-- copied, and a cell outside the gate keeps it raw.
data CellMeta = CellMeta { cm_site :: {-# UNPACK #-} !Int,
                           cm_frame :: !Frame,
                           cm_binder :: {-# UNPACK #-} !Int,
                           cm_validity :: {-# UNPACK #-} !Int }

-- Site ids start at 1, so 0 is free to mean no binder.
noBinder :: Int
noBinder = 0

instance Show CellMeta where
    show m@(CellMeta s f b _) =
        "CellMeta " ++ show s ++ " (" ++ show f ++ ")" ++
        (if b == noBinder then "" else " binder " ++ show b) ++
        (if bitCell m then " bit" else "") ++
        (if validityOf m == rawValidity then "" else " valid " ++ show (validityOf m))

-- the site of a cell that borrows the site of the cell at its head
{-# INLINE headSiteOf #-}
headSiteOf :: MetaStore -> CellMeta -> Int
headSiteOf st m
  | perturbBool st True = cm_site m .|. headSiteBit
  | otherwise = cm_site m .&. complement headSiteBit

-- The site and recursive bit of a cell that holds a definition's
-- partial application (its site is a definition site, not borrowed
-- from a head), or Nothing for any other cell.  Read when a cell's
-- value is applied, so it tests the bits and touches no table.
{-# INLINE cellDefSite #-}
cellDefSite :: CellMeta -> Maybe (Int, Bool)
cellDefSite m =
    let s = cm_site m
    in  if testBit s 58 && not (testBit s 60)
        then Just (s, testBit s 59)
        else Nothing

-- The store: the site table, the frame counter, the frame register
-- (the frame current for the next allocation), the alias classes, the
-- sites of the root frames pushed so far (most recent first), the
-- scramble seed (0 when scrambling is off), and the census counters
-- with the flag that turns them on.  One store per module elaboration.
data MetaStore = MetaStore { ms_sites :: !(IORef SiteTable),
                             ms_nextFrame :: !(IORef Int),
                             ms_frame :: !(IORef Frame),
                             ms_alias :: !(IORef AliasMap),
                             ms_roots :: !(IORef [Int]),
                             ms_types :: !(IORef TypeTable),
                             ms_repacks :: !(IORef [Repack]),
                             ms_seed :: {-# UNPACK #-} !Int,
                             ms_census :: !Bool,
                             ms_counts :: !(IORef Census) }

newMetaStore :: Int -> Bool -> IO MetaStore
newMetaStore seed census = do
    sites <- newIORef emptySiteTable
    next <- newIORef 1
    reg <- newIORef TopFrame
    alias <- newIORef IM.empty
    roots <- newIORef []
    types <- newIORef emptyTypeTable
    repacks <- newIORef []
    counts <- newIORef emptyCensus
    return (MetaStore sites next reg alias roots types repacks
                      seed census counts)

-- The one scrambling function: identity when the seed is 0.
perturb :: MetaStore -> Int -> Int
perturb st n = n `xor` ms_seed st

perturbBool :: MetaStore -> Bool -> Bool
perturbBool st b = if ms_seed st == 0 then b else not b

-- Look a site up without minting it: its id and recursive bit.
{-# INLINE lookupSite #-}
lookupSite :: MetaStore -> SiteKey -> IO (Maybe (Int, Bool))
lookupSite st key = do
    t <- readIORef (ms_sites st)
    return $! case M.lookup key (st_ids t) of
                Just (SiteRef s r) -> Just (s, r)
                Nothing -> Nothing

-- Look a site up, or mint it.  The facts (use-site, recursive) are
-- forced only when the row is inserted, so a caller may compute them
-- lazily and pay once per site rather than once per allocation.
-- Returns the site id and its recursive bit.
mintSite :: MetaStore -> SiteKey -> (Bool, Bool) -> IO (Int, Bool)
mintSite st key facts = do
    t <- readIORef (ms_sites st)
    case M.lookup key (st_ids t) of
      Just (SiteRef s r) -> return (s, r)
      Nothing -> do
        let (useSite, recursive) = facts
        (s, r, t') <- insertSite st t key useSite recursive
        -- a recursion point's reference counts its iterations under the
        -- definition's own position-free site, minted here when needed
        t'' <- case key of
                 IdKey b q p SKDef | r, p /= noPosition -> do
                   let dkey = IdKey b q noPosition SKDef
                   (dk, _, td) <- case M.lookup dkey (st_ids t') of
                                    Just (SiteRef dk dr) -> return (dk, dr, t')
                                    Nothing -> insertSite st t' dkey False recursive
                   let rk = RecKey (plainSite dk) (perturbBool st useSite)
                   return td { st_recKeys = IM.insert (plainSite s) rk (st_recKeys td) }
                 _ -> return t'
        writeIORef (ms_sites st) $! t''
        return (s, r)

-- the row of a new site: its id (with the fact bits) and recursive bit
insertSite :: MetaStore -> SiteTable -> SiteKey -> Bool -> Bool
           -> IO (Int, Bool, SiteTable)
insertSite st t key useSite recursive = do
    let s0 = perturb st (st_next t)
        r = perturbBool st recursive
        isDef = perturbBool st (siteKeyKind key == SKDef)
        s = s0 .|. (if isDef then defSiteBit else 0)
               .|. (if r then recSiteBit else 0)
        row = SiteRow key (perturbBool st useSite) r
        t' = t { st_next = st_next t + 1,
                 st_ids = M.insert key (SiteRef s r) (st_ids t),
                 st_rows = IM.insert s0 row (st_rows t) }
    return (s, r, t')

plainSite :: Int -> Int
plainSite s = s .&. complement siteBits

-- The key map of a table read back from a .bxref (GenBXref writes the
-- rows alone): a site's id is its row key with the fact bits
-- insertSite sets, from the row's kind and recursive flag, as an
-- unperturbed table has them.
siteIdsFromRows :: IM.IntMap SiteRow -> M.Map SiteKey SiteRef
siteIdsFromRows rows =
    M.fromList [ (sr_key row, SiteRef (siteId s row) (sr_recursive row))
               | (s, row) <- IM.toList rows ]
  where siteId s row =
            s .|. (if siteKeyKind (sr_key row) == SKDef then defSiteBit else 0)
              .|. (if sr_recursive row then recSiteBit else 0)

-- Push a frame for a site under the register's frame and return it
-- (the caller runs the unfolding between two swaps).  A push at a
-- recursion point looks the definition up in the register's map.  It
-- continues the iteration found there when the reference is the one
-- that entered the loop, or when it is the definition's own reference
-- (the self-reference in its body, through which every iteration after
-- the first arrives): the new frame carries the entry site, its parent
-- is the predecessor's parent and its count one more; the frames
-- between the two iterations belong to the previous iteration's body
-- and stay on the cells they stamped.  A reference at a different use
-- site is a different loop of the same definition (two static loops
-- are two primFix sites) and nests a new iteration count under the
-- current frame.  One entry per definition: the inner loop's entry
-- shadows the outer's in the frames under it, and the outer's own
-- frames keep theirs.
pushFrame :: MetaStore -> Int -> Bool -> IO Frame
pushFrame st site recursive = do
    parent <- readIORef (ms_frame st)
    n <- readIORef (ms_nextFrame st)
    writeIORef (ms_nextFrame st) $! n + 1
    let fid = perturb st n
        rec = frameRec parent
        plain = plainSite site
    fr <- if recursive
          then do t <- readIORef (ms_sites st)
                  let RecKey key useSite =
                          IM.findWithDefault (RecKey plain False) plain (st_recKeys t)
                      fresh = let c1 = perturb st 1
                              in  Frame fid site c1 parent
                                        (IM.insert key (RecEntry c1 parent site) rec)
                  return $ case IM.lookup key rec of
                    Just (RecEntry c p entry)
                      | plainSite entry == plain || not useSite ->
                        let c' = perturb st (c + 1)
                        in  Frame fid entry c' p
                                  (IM.insert key (RecEntry c' p entry) (frameRec p))
                    _ -> fresh
          else return (Frame fid site (perturb st 1) parent rec)
    return $! fr

-- Push a root frame: a rule, a method or an instantiation.  A root
-- chains to nothing: the unfoldings that created the rule or the
-- instance are hierarchy, not provenance of the values built under it.
pushRoot :: MetaStore -> Int -> IO Frame
pushRoot st site = do
    n <- readIORef (ms_nextFrame st)
    writeIORef (ms_nextFrame st) $! n + 1
    modifyIORef' (ms_roots st) (site :)
    recordRoot st site
    return $! Frame (perturb st n) site (perturb st 1) TopFrame IM.empty

-- Set the register to a frame and return the frame it held.  The
-- evaluator runs an unfolding between a swap to its frame and a swap
-- back; a save and restore suffices, because the evaluator has no catch
-- sites and an error aborts the compile.
{-# INLINE swapFrame #-}
swapFrame :: MetaStore -> Frame -> IO Frame
swapFrame st fr = do
    old <- readIORef (ms_frame st)
    writeIORef (ms_frame st) fr
    return old

-- The same, to the frame a cell was created in: a demanded cell resumes
-- its evaluation there, so the cells that evaluation allocates belong
-- to the frame that created the demanded one.
{-# INLINE swapCellFrame #-}
swapCellFrame :: MetaStore -> CellMeta -> IO Frame
swapCellFrame st m = swapFrame st (cm_frame m)

-- The header for a cell allocated now at a site, of a type, with its
-- binder (whose site is minted here, on the first cell bound to it).
-- The gate is set from the type (bitCellType); every cell starts raw.
{-# INLINE stampMeta #-}
stampMeta :: MetaStore -> Int -> Binder -> IType -> IO CellMeta
stampMeta st site b t = do
    fr <- readIORef (ms_frame st)
    let gate = if bitCellType t then bitGate else 0
    binder <- case b of
                NoBinder -> return noBinder
                BoundTo i -> mint (idSiteKey SKBinder i)
                MovedTo n -> mint (FieldKey n)
    return $! CellMeta site fr binder gate
  where mint key = fst <$> mintSite st key (False, False)

-- ===============
-- Validity
--
-- The one type fact the store keeps about a bit-shaped value: the
-- validity (must) is the one type whose encoding the bits are known to
-- satisfy, with the tier of that knowledge: by construction (a
-- hole-free type, a literal, a derived pack of valid fields), by
-- declaration (a port of a module, an argument of an instantiation) or
-- by assumption (bits unpacked as a type they were not known to be
-- valid for, which is a repack fact); or rawValidity.  Validity meets
-- at every join (equal types survive, their tiers joined; different
-- types or a raw side give raw).
--
-- The validity word names a row of the type-window table: a type and a
-- window of it (an offset from bit 0 and a width; the whole type is
-- window (0, -1), whose width the type decides).  The window is the
-- field a derived instance extracts from a value valid at the type
-- (windowValidity), which an unpack of that window reads as the field
-- (coercionValidity).  The table is keyed by the program's types,
-- compared by their intern ids (IType's Ord), and the windows the
-- derived instances take from them, so it is bounded by the type
-- declarations and never by the elaborated data; nothing here creates
-- a string.  Validity is census-grade until the valid-unknown
-- redesign and instance lawfulness land, which the side file's schema
-- says.

-- A type window: the type, the offset and the width.
data TypeKey = TypeKey !IType {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        deriving (Eq, Ord)

-- the whole of a value of a type
typeWindow :: IType -> TypeKey
typeWindow t = TypeKey t 0 wholeWidth

wholeWidth :: Int
wholeWidth = -1

data TypeTable = TypeTable { tt_next :: {-# UNPACK #-} !Int,
                             tt_ids :: !(M.Map TypeKey Int),
                             tt_rows :: !(IM.IntMap TypeKey) }

emptyTypeTable :: TypeTable
emptyTypeTable = TypeTable 1 M.empty IM.empty

-- the id map of the table rebuilt from its rows, for a reader of the
-- .bxref file (GenBXref writes the rows alone: a key orders by its
-- type's intern id, so the map's order is not a function of the
-- content)
typeIdsFromRows :: IM.IntMap TypeKey -> M.Map TypeKey Int
typeIdsFromRows rows = M.fromList [ (key, v) | (v, key) <- IM.toList rows ]

-- Look a type window up, or mint it; the id is perturbed under the
-- scramble like every number the store mints.
mintType :: MetaStore -> TypeKey -> IO Int
mintType st key = do
    t <- readIORef (ms_types st)
    case M.lookup key (tt_ids t) of
      Just v -> return v
      Nothing -> do
        let v = perturb st (tt_next t)
            t' = t { tt_next = tt_next t + 1,
                     tt_ids = M.insert key v (tt_ids t),
                     tt_rows = IM.insert v key (tt_rows t) }
        writeIORef (ms_types st) $! t'
        return v

-- A type whose every bit pattern is a valid encoding: Bit, UInt and
-- Int at any width, and Bool.  A struct of such fields is hole-free as
-- well, but deciding that needs the field types, which the evaluator
-- does not have in hand; such a type counts as hole-bearing here, which
-- errs towards recording a repack.  The identifiers compared are
-- Prelude type constructors, interned when the Prelude's .bo was read.
holeFree :: IType -> Bool
holeFree (ITAp (ITCon c _ _) (ITNum _)) = c == idBit || c == idUInt || c == idInt
holeFree (ITCon c _ _) = c == idBool
holeFree _ = False

-- the width of a Bit type, or wholeWidth
bitWidth :: IType -> Int
bitWidth (ITAp (ITCon c _ _) (ITNum n)) | c == idBit = fromInteger n
bitWidth _ = wholeWidth

-- The validity of a value: rawValidity, or the id of a type window
-- with four bits above it: three tier bits (58 by construction, 59 by
-- declaration, 60 by assumption) and bit 57, which says the window's
-- type is a Bit type, whose validity is trivial (every pattern is a
-- Bit n) and gives way to a source type when one becomes known
-- (refineValidity).  The bits are set from their facts through
-- perturbBool so that the scramble inverts them like a site's fact
-- bits; the row id is perturbed when it is minted.
--
-- The gate.  A hint that outlives the evaluator is attached to cells of
-- type Bit n alone, the cells that become wires and defs; a struct,
-- union, interface, function, action or numeric-type cell carries none,
-- and a cell of another primitive type only where a consumer reads the
-- hint for that type (none today).  The gate is one bit of the
-- validity word, set at allocation from the cell's type (stampMeta)
-- and read by refineMeta, the one writer of facts into a header; it is
-- store state that no evaluator decision reads, so the scramble leaves
-- it alone, and validityOf masks it.
--
-- Bool, a data type that is never unpacked and always a one-bit wire,
-- is excluded by the letter of the rule; boolCellsCarryFacts admits a
-- cell of that type to the gate.  A Bool method of a module is a Bit 1
-- port, so its call's cell is a Bit 1 cell declared valid at Bool,
-- gated either way; a cell of the type Bool itself (a chr of bits to
-- Bool, a Bool field) takes no fact today, since a chr writes on the
-- bits side, so the constant changes nothing until a consumer reads
-- such a cell.
bitGate :: Int
bitGate = bit 61

boolCellsCarryFacts :: Bool
boolCellsCarryFacts = False

-- the types whose cells carry facts (the identifiers compared are
-- Prelude type constructors, as in holeFree)
bitCellType :: IType -> Bool
bitCellType (ITAp (ITCon c _ _) _) = c == idBit
bitCellType (ITCon c _ _) = boolCellsCarryFacts && c == idBool
bitCellType _ = False

-- the cell's type admits facts
bitCell :: CellMeta -> Bool
bitCell m = testBit (cm_validity m) 61

rawValidity :: Int
rawValidity = 0

trivialBit :: Int
trivialBit = bit 57

validityBits :: Int
validityBits = trivialBit .|. siteBits

-- the trivial bit of a validity at a type, from the type
trivialBits :: MetaStore -> IType -> Int
trivialBits st t = if perturbBool st (bitWidth t /= wholeWidth) then trivialBit else 0

data Tier = TConstruction | TDeclaration | TAssumption
        deriving (Eq)

tierBit :: Tier -> Int
tierBit TConstruction = bit 58
tierBit TDeclaration = bit 59
tierBit TAssumption = bit 60

tierBits :: MetaStore -> Tier -> Int
tierBits st tier = L.foldl' (.|.) 0
                     [ tierBit t | t <- [TConstruction, TDeclaration, TAssumption],
                                   perturbBool st (t == tier) ]

-- the type window of a validity, without its bits, and the tier bits
validityView :: Int -> Int
validityView v = v .&. complement validityBits

validityTiers :: Int -> Int
validityTiers v = v .&. siteBits

-- the validity is at a Bit type (trivial)
trivialValidity :: Int -> Bool
trivialValidity v = testBit v 57

-- Meet, at a join of two values: equal types survive with their tiers
-- joined; a raw side or different types give raw.
meetValidity :: Int -> Int -> Int
meetValidity a b
  | a == rawValidity || b == rawValidity = rawValidity
  | validityView a == validityView b = a .|. b
  | otherwise = rawValidity

-- Refine, as more becomes known about one value: raw gives way to a
-- type, equal types join their tiers, a trivial Bit type gives way to
-- a source type, and a type already known is otherwise kept.
refineValidity :: Int -> Int -> Int
refineValidity old new
  | old == rawValidity = new
  | new == rawValidity = old
  | validityView old == validityView new = old .|. new
  | trivialValidity old && not (trivialValidity new) = new
  | otherwise = old

-- the validity of a value valid at a type on a tier
typeValidity :: MetaStore -> Tier -> IType -> IO Int
typeValidity st tier t = do
    v <- mintType st (typeWindow t)
    return $! v .|. tierBits st tier .|. trivialBits st t

-- The validity of bits coerced to a type (an unpack, or the conversion
-- of bits to an enumeration), given the validity the bits had and
-- whether they came straight from a boundary (a port or parameter
-- constant, whose type the boundary declares by coercing it): valid at
-- the type by construction when the type is hole-free, by declaration
-- at a boundary, with the bits' own tiers when they were valid at that
-- type or at a window of a valid value (the field a derived instance
-- extracts, windowValidity), and by assumption otherwise, which is a
-- repack (the last component) that the caller records; the type's row
-- in every case, returned as well so that the repack can name it.  A
-- pack reads the same decision for its payload and keeps raw where
-- this says assumption, because packing assumes nothing: the
-- materialisation of the pack decides (constructedValidity).
coercionValidity :: MetaStore -> IType -> Int -> Bool -> IO (Int, Int, Bool)
coercionValidity st t src boundary = do
    v <- mintType st (typeWindow t)
    tt <- readIORef (ms_types st)
    let kept = validityView src == v
        window = src /= rawValidity && not (trivialValidity src) &&
                 case IM.lookup (validityView src) (tt_rows tt) of
                   Just (TypeKey _ _ w) -> w /= wholeWidth
                   Nothing -> False
        (val, repack)
          | holeFree t = (v .|. tierBits st TConstruction .|. trivialBits st t, False)
          | boundary = (v .|. tierBits st TDeclaration, False)
          | kept = (src, False)
          | window = (v .|. validityTiers src, False)
          | otherwise = (v .|. tierBits st TAssumption, True)
    return (v, val, repack)

-- The validity of bits constructed as a type from valid parts (the
-- materialisation of a derived pack whose leaves were all valid): by
-- construction, joined with the tiers of the parts.
constructedValidity :: MetaStore -> IType -> Int -> IO Int
constructedValidity st t tiers = do
    v <- mintType st (typeWindow t)
    return $! v .|. tierBits st TConstruction .|. (tiers .&. siteBits)

-- The validity of bits [m, m + k) of a value with the validity src: the
-- window of the source's validity when the source was valid at a
-- source type, which an unpack of the window reads as the field a
-- derived instance extracts (the evaluator has no field layout to
-- check the window against); raw otherwise, and raw at a trivial Bit
-- type.
windowValidity :: MetaStore -> Int -> Int -> Int -> IO Int
windowValidity st src m k
  | src == rawValidity || trivialValidity src = return rawValidity
  | otherwise = do
      tt <- readIORef (ms_types st)
      case IM.lookup (validityView src) (tt_rows tt) of
        Just (TypeKey ty o w) | w == wholeWidth || m + k <= w -> do
            wv <- mintType st (TypeKey ty (o + m) k)
            return (wv .|. validityTiers src)
        _ -> return rawValidity

-- Refine a header with the validity of its value; Nothing when it does
-- not change, so that a cell whose facts are unchanged keeps its
-- header, and Nothing for a cell outside the gate, which takes no fact
-- whatever lands in it (every writer of facts into a header comes
-- here).
refineMeta :: CellMeta -> Int -> Maybe CellMeta
refineMeta m v
  | not (bitCell m) = Nothing
  | v' == old = Nothing
  | otherwise = Just (m { cm_validity = v' .|. bitGate })
  where old = validityOf m
        v' = refineValidity old v

-- A repack: bits reinterpreted as a type they were not known to be
-- valid for, at an unpack or at the conversion of computed bits to an
-- enumeration (chr): the cell that held the bits (noCell for a value
-- that had none), the validity they had, the row of the type they were
-- read as, and the frame current at the event.
data RepackKind = RKUnpack | RKChr
        deriving (Eq, Show)

data Repack = Repack { rp_kind :: !RepackKind,
                       rp_cell :: {-# UNPACK #-} !Int,
                       rp_from :: {-# UNPACK #-} !Int,
                       rp_to :: {-# UNPACK #-} !Int,
                       rp_frame :: !Frame }

recordRepack :: MetaStore -> RepackKind -> Int -> Int -> Int -> IO ()
recordRepack st kind cell from to = do
    fr <- readIORef (ms_frame st)
    modifyIORef' (ms_repacks st) (Repack kind (perturb st cell) from to fr :)

-- ===============
-- Alias classes
--
-- Two cells hold one value when one takes the value read from the
-- other: a demanded cell whose expression evaluates to a reference, a
-- cell walked to normal form through a reference, a reference re-heaped
-- under an implicit condition, and a cell whose content is itself a
-- reference.  The classes are a union-find over cell pointers: a map
-- from a pointer to its parent, absent for a root, with path
-- compression on find; a cell in no union is its own class and has no
-- entry.  The root of a class is its smallest pointer (the earliest
-- cell), so a class has one representative whatever the order of its
-- unions, and every link goes from a larger pointer to a smaller one,
-- so a find terminates on the perturbed entries of the scramble run
-- as well.
type AliasMap = IM.IntMap Int

-- the root of p's class, compressing the path to it
aliasFind :: MetaStore -> Int -> IO Int
aliasFind st p = do
    m <- readIORef (ms_alias st)
    let root = walk p
        walk x = maybe x walk (IM.lookup x m)
        compress x acc = case IM.lookup x acc of
                           Just y | y /= root -> compress y (IM.insert x root acc)
                           _ -> acc
    case IM.lookup p m of
      Just y | y /= root -> writeIORef (ms_alias st) $! compress p m
      _ -> return ()
    return root

-- Join the classes of p and q: one insert when they differ.
aliasUnion :: MetaStore -> Int -> Int -> IO ()
aliasUnion st p q = do
    rp <- aliasFind st p
    rq <- aliasFind st q
    when (rp /= rq) $ do
      let a = perturb st rp
          b = perturb st rq
      modifyIORef' (ms_alias st) (IM.insert (max a b) (min a b))

-- the root of p's class in a snapshot of the map
aliasClassOf :: AliasMap -> Int -> Int
aliasClassOf m = walk
  where walk x = maybe x walk (IM.lookup x m)

-- the cells in a class with another cell: those with a parent and those
-- that are one
aliasMembers :: AliasMap -> IS.IntSet
aliasMembers m = IS.union (IM.keysSet m) (IS.fromList (IM.elems m))

-- aliasFind on a snapshot: the root of p's class and the map with the
-- path to it compressed.  Every link still goes from a larger pointer
-- to a smaller one, so the walk terminates.
aliasFindIn :: Int -> AliasMap -> (Int, AliasMap)
aliasFindIn p m =
    let walk x path = case IM.lookup x m of
                        Just y -> walk y (x : path)
                        Nothing -> (x, path)
        (root, path) = walk p []
    in  (root, L.foldl' (\ acc x -> IM.insert x root acc) m path)

-- aliasUnion on a snapshot, for each pair in turn: the unions the
-- rebuild makes after elaboration (the cells CSE identifies).
aliasUnions :: [(Int, Int)] -> AliasMap -> AliasMap
aliasUnions pairs m0 = L.foldl' step m0 pairs
  where step m (p, q) =
            let (rp, m1) = aliasFindIn p m
                (rq, m2) = aliasFindIn q m1
            in  if rp == rq then m2 else IM.insert (max rp rq) (min rp rq) m2

-- The tables of the store after elaboration: the site table, the alias
-- classes, the type-window table and the repacks (in event order).
data MetaTables = MetaTables { mt_sites :: !SiteTable,
                               mt_alias :: !AliasMap,
                               mt_types :: !TypeTable,
                               mt_repacks :: ![Repack] }

readMetaTables :: MetaStore -> IO MetaTables
readMetaTables st = do
    sites <- readIORef (ms_sites st)
    alias <- readIORef (ms_alias st)
    types <- readIORef (ms_types st)
    repacks <- readIORef (ms_repacks st)
    return (MetaTables sites alias types (reverse repacks))

-- the sites of the root frames, in push order
readRoots :: MetaStore -> IO [Int]
readRoots st = reverse <$> readIORef (ms_roots st)

-- ===============
-- The query interface

siteOf :: CellMeta -> Int
siteOf = cm_site

frameOf :: CellMeta -> Frame
frameOf = cm_frame

-- the site of the cell's binder, or noBinder
binderOf :: CellMeta -> Int
binderOf = cm_binder

-- the validity of the cell's value (rawValidity, or a type window with
-- tier bits; the gate bit masked)
validityOf :: CellMeta -> Int
validityOf m = cm_validity m .&. complement bitGate

-- (site, count) of every frame from the cell's frame to the root
chainOf :: CellMeta -> [(Int, Int)]
chainOf m = go (cm_frame m)
  where go TopFrame = []
        go (Frame { fr_site = s, fr_count = c, fr_parent = p }) = (s, c) : go p

showSiteKind :: SiteKind -> String
showSiteKind SKDef = "def"
showSiteKind SKPrim = "prim"
showSiteKind SKCon = "con"
showSiteKind SKMethod = "method"
showSiteKind SKForeign = "foreign"
showSiteKind SKLambda = "lambda"
showSiteKind SKBound = "bound"
showSiteKind SKRule = "rule"
showSiteKind SKMethodRoot = "methodroot"
showSiteKind SKInst = "inst"
showSiteKind SKBinder = "binder"
showSiteKind SKField = "field"

-- The position's file is rendered as its base name: the dump is a
-- debugging aid and must not depend on the directory layout.
renderPosition :: Position -> String
renderPosition p
  | p == noPosition = ""
  | otherwise = " [" ++ takeFileName (getFString (pos_file p)) ++ ":" ++
                show (pos_line p) ++ ":" ++ show (pos_column p) ++ "]"

renderName :: FString -> FString -> String
renderName base qual =
    let q = getFString qual
    in  if null q then getFString base else q ++ "." ++ getFString base

-- "<kind> <name> [<file>:<line>:<col>] <flags>", where the flags are
-- h (head-site), u (use-site) and r (recursion point)
renderSite :: SiteTable -> Int -> String
renderSite t s =
    case siteRow t s of
      Nothing -> "site " ++ show s ++ " (no row)"
      Just row ->
        let key = sr_key row
            body = case key of
                     IdKey b q p _ -> renderName b q ++ renderPosition p
                     PrimKey op -> show op
                     RootKey n _ -> n
                     BoundKey -> "-"
                     FieldKey n -> show n
            flags = [ c | (c, on) <- [('h', testBit s 60),
                                      ('u', sr_useSite row),
                                      ('r', sr_recursive row)], on ]
        in  showSiteKind (siteKeyKind key) ++ " " ++ body ++
            (if null flags then "" else " " ++ flags)

-- A type window as "<type>", with " [<offset>+<width>]" for a window
-- that is not the whole type; the type is printed by reading its
-- identifiers' strings.
renderTypeWindow :: TypeTable -> Int -> String
renderTypeWindow tt v =
    case IM.lookup v (tt_rows tt) of
      Nothing -> "type " ++ show v ++ " (no row)"
      Just (TypeKey ty o w) ->
          ppString ty ++
          (if w == wholeWidth then "" else " [" ++ show o ++ "+" ++ show w ++ "]")

-- a validity as "<type window> <letters>" (c by construction, d by
-- declaration, a by assumption, b at a Bit type), or "-" for raw
renderValidity :: TypeTable -> Int -> String
renderValidity tt v
  | v == rawValidity = "-"
  | otherwise = renderTypeWindow tt (validityView v) ++ " " ++
                [ c | (c, b) <- [('c', 58), ('d', 59), ('a', 60), ('b', 57)], testBit v b ]

-- One line per cell, in the order given (the caller passes ascending
-- pointer order): the pointer, the cell's site, its binder when it has
-- one, the root of its alias class when the class has another cell,
-- its validity when it is not raw, then the chain of frames from the
-- creating frame to the root, each as site#count.
renderDump :: MetaTables -> [(Int, CellMeta)] -> String
renderDump (MetaTables t am tt _) cells = unlines (map line cells)
  where line (p, m) =
            "_" ++ show p ++ ": " ++ renderSite t (siteOf m) ++
            binder (binderOf m) ++ alias p ++ validity (validityOf m) ++
            concatMap link (chainOf m)
        binder b | b == noBinder = ""
                 | otherwise = ", " ++ renderSite t b
        alias p | IS.member p members = ", alias _" ++ show (aliasClassOf am p)
                | otherwise = ""
        validity v | v == rawValidity = ""
                   | otherwise = ", valid " ++ renderValidity tt v
        members = aliasMembers am
        link (s, c) = " <- " ++ renderSite t s ++ "#" ++ show c

-- ===============
-- The module metadata table
--
-- Built once by the rebuild (IExpand.buildModuleMeta) from the headers
-- of the cells it keeps, after CSE, and carried on the IModule.  It is
-- read by nothing that orders or chooses: the side file renders it, and
-- -bxref writes it to the module's .bxref (GenBXref); the .ba never
-- carries it.

-- A context is one distinct frame chain, as the triple (parent context,
-- site, count); context 0 is the top, under which no frame was pushed.
-- Contexts are interned bottom-up from the frames of the kept cells,
-- memoized by frame id, so that frames no kept cell references are never
-- visited and identical chains share one id.  This is the only place
-- context ids exist; nothing during evaluation reads or writes them.
-- ct_ops counts the steps of internFrame, one per frame whose chain was
-- looked up, memoized or not: the census's context-table cost counter.
data ContextTable = ContextTable { ct_next :: {-# UNPACK #-} !Int,
                                   ct_ids :: !(M.Map (Int, Int, Int) Int),
                                   ct_rows :: !(IM.IntMap (Int, Int, Int)),
                                   ct_ops :: {-# UNPACK #-} !Int }

emptyContextTable :: ContextTable
emptyContextTable = ContextTable 1 M.empty IM.empty 0

contextTableSize :: ContextTable -> Int
contextTableSize t = IM.size (ct_rows t)

-- Intern the chain of a frame and return its context id; the memo (by
-- frame id) is threaded through the cells of one module.  The recursion
-- is one step per frame of the chain not yet memoized.
internFrame :: (ContextTable, IM.IntMap Int) -> Frame
            -> ((ContextTable, IM.IntMap Int), Int)
internFrame st TopFrame = (st, 0)
internFrame (t0, memo) (Frame { fr_id = fid, fr_site = s, fr_count = c,
                                fr_parent = parent }) =
    let t = t0 { ct_ops = ct_ops t0 + 1 }
    in  case IM.lookup fid memo of
          Just ctx -> ((t, memo), ctx)
          Nothing ->
            let ((t1, memo1), pctx) = internFrame (t, memo) parent
                key = (pctx, s, c)
            in  case M.lookup key (ct_ids t1) of
                  Just ctx -> ((t1, IM.insert fid ctx memo1), ctx)
                  Nothing ->
                    let ctx = ct_next t1
                        t2 = t1 { ct_next = ctx + 1,
                                  ct_ids = M.insert key ctx (ct_ids t1),
                                  ct_rows = IM.insert ctx key (ct_rows t1) }
                    in  ((t2, IM.insert fid ctx memo1), ctx)

-- (site, count) of every frame of a context, from the root
contextChain :: ContextTable -> Int -> [(Int, Int)]
contextChain t = go []
  where go acc 0 = acc
        go acc ctx = case IM.lookup ctx (ct_rows t) of
                       Just (p, s, c) -> go ((s, c) : acc) p
                       Nothing -> internalError ("IExpandMeta.contextChain: " ++
                                                 show ctx)

-- the key map of a context table read back from a .bxref: the inverse
-- of its rows
contextIdsFromRows :: IM.IntMap (Int, Int, Int) -> M.Map (Int, Int, Int) Int
contextIdsFromRows rows = M.fromList [ (key, ctx) | (ctx, key) <- IM.toList rows ]

-- The record of a lifted def: the cell it was lifted from and the cells
-- that stand behind it.  The tiers are derived when the record is
-- rendered: use-site and definition-site from the site row, head-site
-- from the site's bit, shared from dr_shared, merged from dr_merged.
--
-- The record does not hold the def's Id.  The rebuild coins that Id
-- lazily, and forcing it interns its strings; the order in which the
-- passes after elaboration first force the def names is an input to
-- the generated output (identifier ordering is by intern id), so the
-- table must never force one.  It holds the cell's name hint instead,
-- which the evaluator forced when it wrote it, and renders the name
-- the rebuild coins from the hint and the pointer (defNameString).
data DefRecord = DefRecord {
        -- the canonical heap pointer of the cell, and its name hint
        dr_ptr :: {-# UNPACK #-} !Int,
        dr_hint :: !(Maybe Id),
        -- the site of the cell and the context of its frame chain
        dr_site :: {-# UNPACK #-} !Int,
        dr_context :: {-# UNPACK #-} !Int,
        -- every context the def stands for: its own, those of the cells
        -- CSE identified with it and those of the cells folded into its
        -- expression
        dr_contexts :: !IS.IntSet,
        -- the (site, context) of each cell CSE identified with this one,
        -- in pointer order
        dr_merged :: ![(Int, Int)],
        -- the (site, context) of each cell the rebuild inlined into the
        -- def's expression instead of lifting it, in pointer order
        dr_folded :: ![(Int, Int)],
        -- the lowest canonical pointer of a kept cell whose expression
        -- references this cell, or noCell when none does (the cell is
        -- referenced from a rule, a method or an instance alone)
        dr_parent :: {-# UNPACK #-} !Int,
        -- the cell is a definition-cache cell (shared by every unfolding
        -- of its definition)
        dr_shared :: !Bool,
        -- the site of the cell's binder (the parameter its expression
        -- was an argument of, or the index of the field or operand it
        -- was moved into), or noBinder
        dr_binder :: {-# UNPACK #-} !Int,
        -- the root of the cell's alias class after the CSE unions, when
        -- the class holds another cell, or noCell
        dr_alias :: {-# UNPACK #-} !Int,
        -- the validity of the cell's value, met with that of the cells
        -- CSE identified with it
        dr_validity :: {-# UNPACK #-} !Int,
        -- the repacks of bits held by this cell, by a cell CSE
        -- identified with it or by a cell folded into it, in event order
        dr_repacks :: ![RepackFact],
        -- the kinds of the undefined values in the def's expression and
        -- in the cells folded into it, in expression order
        -- (undefKindCode); their origin is the def's site and context
        dr_undefs :: ![Int]
    }

-- A repack as the table records it: the kind, the cell (noCell when the
-- bits had none), the validity the bits had, the row of the type they
-- were read as, and the context of the frame current at the event.
data RepackFact = RepackFact { rf_kind :: !RepackKind,
                               rf_cell :: {-# UNPACK #-} !Int,
                               rf_from :: {-# UNPACK #-} !Int,
                               rf_to :: {-# UNPACK #-} !Int,
                               rf_context :: {-# UNPACK #-} !Int }

instance NFData RepackKind where
    rnf k = k `seq` ()

instance NFData RepackFact where
    rnf (RepackFact x1 x2 x3 x4 x5) = rnf5 x1 x2 x3 x4 x5

-- the code of an undefined value's kind in a record, and its name
undefKindCode :: UndefKind -> Int
undefKindCode UNotUsed = 0
undefKindCode UDontCare = 1
undefKindCode UNoMatch = 2

undefKindName :: Int -> String
undefKindName 0 = "not-used"
undefKindName 1 = "dont-care"
undefKindName 2 = "no-match"
undefKindName n = "kind-" ++ show n

-- Heap pointers start at 0, so -1 is free to mean no cell.
noCell :: Int
noCell = -1

instance NFData DefRecord where
    rnf (DefRecord x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14) =
        rnf5 x1 x2 x3 x4 x5 `seq` rnf5 x6 x7 x8 x9 x10 `seq`
        rnf4 x11 x12 x13 x14

-- The name the rebuild gives the def lifted from a cell: the name hint
-- with its illegal characters dropped, its base extended by the pointer
-- (IExpand.pDef, with "__h" as iExpandPref), rendered as getIdString
-- renders the Id.  Pure string operations over existing strings: nothing
-- is interned.
defNameString :: Int -> Maybe Id -> String
defNameString p hint =
    let suffix = "__h" ++ show p
    in  case hint of
          Nothing -> suffix
          Just i ->
            let legal = filter (\ c -> isAlphaNum c || c == '_')
                base = legal (getFString (getIdBase i)) ++ suffix
                qual = legal (getFString (getIdQual i))
            in  if null qual then base else qual ++ "." ++ base

-- The table: the sites, the contexts, the type windows, the root
-- sites (rules, methods and instantiations, in push order), the def
-- records, keyed by the canonical heap pointer (the number the def's
-- name ends in), every repack of the elaboration in event order, and
-- the events of the passes after elaboration (the ledger below), most
-- recent first, so that a pass appends by consing (appendEvent).
data ModuleMeta = ModuleMeta { mm_sites :: !SiteTable,
                               mm_contexts :: !ContextTable,
                               mm_types :: !TypeTable,
                               mm_roots :: ![Int],
                               mm_defs :: !(IM.IntMap DefRecord),
                               mm_repacks :: ![RepackFact],
                               mm_events :: ![MetaEvent] }

emptyModuleMeta :: ModuleMeta
emptyModuleMeta = ModuleMeta emptySiteTable emptyContextTable emptyTypeTable
                             [] IM.empty [] []

-- compact, so that the derived Show of an IModule stays printable; the
-- events are not counted, so the text does not change with the ledger
instance Show ModuleMeta where
    show mm = "ModuleMeta sites " ++ show (siteTableSize (mm_sites mm)) ++
              " contexts " ++ show (contextTableSize (mm_contexts mm)) ++
              " types " ++ show (IM.size (tt_rows (mm_types mm))) ++
              " roots " ++ show (length (mm_roots mm)) ++
              " defs " ++ show (IM.size (mm_defs mm)) ++
              " repacks " ++ show (length (mm_repacks mm))

instance NFData SiteKey where
    rnf (IdKey b q p k) = rnf4 b q p k
    rnf (PrimKey op) = rnf op
    rnf (RootKey n k) = rnf2 n k
    rnf BoundKey = ()
    rnf (FieldKey n) = rnf n

instance NFData SiteKind where
    rnf k = k `seq` ()

instance NFData SiteRow where
    rnf (SiteRow k u r) = rnf3 k u r

instance NFData SiteRef where
    rnf (SiteRef s r) = rnf2 s r

instance NFData RecKey where
    rnf (RecKey k u) = rnf2 k u

instance NFData SiteTable where
    rnf (SiteTable n ids rows recs) = rnf4 n ids rows recs

instance NFData ContextTable where
    rnf (ContextTable n ids rows ops) = rnf4 n ids rows ops

-- A key in weak head normal form is in normal form: the window is
-- unpacked, and the type is an interned node (IType.internAp forces
-- both children for its key).  The type's interior, shared with the
-- module's own types, is not walked: rnf does not memoise, and a walk
-- of every key's type through the sorts of its constructors was the
-- cost of every deepseq of the table.
instance NFData TypeKey where
    rnf k = k `seq` ()

instance NFData TypeTable where
    rnf (TypeTable n ids rows) = rnf3 n ids rows

-- The table's one deep walk, performed by iExpand when the table is
-- built; the NFData instance below walks the events alone, so that
-- the deepseq of the whole module at every later stage
-- (TopUtils.dump) does not walk the tables again (rnf does not
-- memoise).
forceModuleMeta :: ModuleMeta -> ()
forceModuleMeta (ModuleMeta s c ty r d rp e) =
    rnf3 s c ty `seq` rnf4 r d rp e

instance NFData ModuleMeta where
    -- the tables were walked once by forceModuleMeta; the events are
    -- what the passes after elaboration append, forced where the
    -- package is
    rnf mm = rnf (mm_events mm)

-- ===============
-- The ledger
--
-- The passes between elaboration and Verilog rename, merge, inline,
-- create and drop defs and rules.  Each writes what it did as an event
-- with the ids it had in hand, under its own tag, onto the event list
-- of the module metadata table; its own data structures, counters and
-- orders are untouched, and the events are read by nothing but the
-- resolution below, the dump and the tools.  The ledger rides every
-- package as one field (IModule.imod_meta, APackage.apkg_meta,
-- ASPackage.aspkg_meta): each conversion copies it forward and every
-- other pass keeps it by record update.

-- The pass an event was written by, in driver order: the ISyntax
-- passes after elaboration, the conversion to ASyntax, the ASyntax
-- passes before and after state generation (the ECO backend's keep
-- plan, ECO.Backend.applyBackendPlan, among the latter), and the
-- Verilog backend's expansion of defs into their users.
data PassTag = PTInlineFmt | PTInline | PTTransform | PTSplitIf | PTLift
             | PTParams | PTDropRules
             | PTConv | PTRankMethCalls | PTTaskSplice | PTCleanup
             | PTSchedule | PTAddScheduleDefs | PTNoInline
             | PTAddCFConditionWires | PTAddSchedAssumps | PTRemoveAssumps
             | PTState | PTInlineWires | PTInlineCReg | PTRenameIO
             | PTDropDefs | PTBackendPlan | PTOpt | PTExpand
        deriving (Eq, Ord, Show, Enum, Bounded)

-- One event: what a pass did to def or rule ids.  The ids are the
-- package's at the stage the pass ran (Id on an IModule, AId on an
-- APackage or ASPackage, which is the same type).
data MetaEvent
    = Renamed !PassTag !Id !Id      -- from, to: one id became another
    | Merged !PassTag ![Id] !Id     -- from, into: several defs became one
    | Inlined !PassTag !Id ![Id]    -- from, into: a def went into its users
    | Created !PassTag !Id ![Id]    -- to, from: a def built from others
    | Dropped !PassTag !Id          -- from: a def or rule removed

instance NFData PassTag where
    rnf t = t `seq` ()

instance NFData MetaEvent where
    rnf (Renamed t a b) = rnf3 t a b
    rnf (Merged t as b) = rnf3 t as b
    rnf (Inlined t a bs) = rnf3 t a bs
    rnf (Created t a bs) = rnf3 t a bs
    rnf (Dropped t a) = rnf2 t a

-- The ledger as a package field.  The instances are what keep the field
-- out of the generated output: Eq and Ord are constant, so a comparison
-- of packages (APackage and ASPackage derive Eq) never sees it; Data is
-- a leaf, so a generic traversal never descends into it; NFData walks
-- the events (the table was forced in full at the end of elaboration,
-- forceModuleMeta), so a package's deepseq forces the ledger's events
-- where it forces the package; PPrint prints nothing, so no package
-- dump changes with the ledger (the metadata dump stage prints it on
-- request); Show is the table's compact line, so the expanded module's
-- Show is unchanged.
newtype MetaLedger = MetaLedger { ledgerMeta :: ModuleMeta }

emptyMetaLedger :: MetaLedger
emptyMetaLedger = MetaLedger emptyModuleMeta

instance Eq MetaLedger where
    _ == _ = True

instance Ord MetaLedger where
    compare _ _ = EQ

instance Show MetaLedger where
    show (MetaLedger mm) = show mm

instance Data MetaLedger where
    gfoldl _ z x = z x
    gunfold _ _ _ = internalError "IExpandMeta.MetaLedger: gunfold"
    toConstr _ = internalError "IExpandMeta.MetaLedger: toConstr"
    dataTypeOf _ = mkNoRepType "IExpandMeta.MetaLedger"

instance NFData MetaLedger where
    rnf (MetaLedger mm) = rnf mm

instance PPrint MetaLedger where
    pPrint _ _ _ = empty

-- The one write: a pass appends the event it has in hand.
appendEvent :: MetaEvent -> MetaLedger -> MetaLedger
appendEvent ev (MetaLedger mm) = MetaLedger (mm { mm_events = ev : mm_events mm })

-- Resolve a final id to the ids whose records it stands for, in the
-- order they were found, with the events on the way, oldest first.
-- The events are walked from the most recent: a Renamed to an id in
-- hand replaces it by its source; a Merged into one adds the merged
-- ids beside it; an Inlined into one adds the inlined id; a Created of
-- one replaces it by its sources, when it has any (a def created from
-- nothing stands for itself); a Dropped changes nothing, a dropped id
-- being no final id.  Pure; run by emission and by tools, never by a
-- pass.
resolveId :: MetaLedger -> Id -> ([Id], [MetaEvent])
resolveId (MetaLedger mm) final = go (mm_events mm) (S.singleton final) [final] []
  where
    -- seen: the ids in hand; ids: the same, most recently found first;
    -- evs: the events taken, most recent first
    go [] _ ids evs = (reverse ids, evs)
    go (ev : rest) seen ids evs =
        case ev of
          Renamed _ from to | S.member to seen ->
              taken (add [from] (remove to (seen, ids)))
          Merged _ froms into | S.member into seen ->
              taken (add froms (seen, ids))
          Inlined _ from intos | any (`S.member` seen) intos ->
              taken (add [from] (seen, ids))
          Created _ to froms | S.member to seen, not (null froms) ->
              taken (add froms (remove to (seen, ids)))
          _ -> go rest seen ids evs
      where
        taken (seen', ids') = go rest seen' ids' (ev : evs)
    remove x (seen, ids) = (S.delete x seen, filter (/= x) ids)
    add [] st = st
    add (y : ys) st@(seen, ids)
        | S.member y seen = add ys st
        | otherwise = add ys (S.insert y seen, y : ids)

-- The ledger as the metadata dump stage (-dmetadata) prints it after
-- the named stage: the sizes of the table and the number of events on
-- one line, then the events, oldest first, one per line, the ids
-- rendered by getIdString (which reads existing strings).
renderLedger :: String -> MetaLedger -> String
renderLedger stage (MetaLedger mm) =
    unlines (("ledger after " ++ stage ++ ": " ++
              show (siteTableSize (mm_sites mm)) ++ " sites, " ++
              show (contextTableSize (mm_contexts mm)) ++ " contexts, " ++
              show (length (mm_roots mm)) ++ " roots, " ++
              show (IM.size (mm_defs mm)) ++ " defs, " ++
              show (length (mm_events mm)) ++ " events")
             : map renderEvent (reverse (mm_events mm)))

renderEvent :: MetaEvent -> String
renderEvent ev =
    case ev of
      Renamed t from to -> line "renamed" t (ids [from] ++ " -> " ++ ids [to])
      Merged t froms into -> line "merged" t (ids froms ++ " -> " ++ ids [into])
      Inlined t from intos -> line "inlined" t (ids [from] ++ " -> " ++ ids intos)
      Created t to froms -> line "created" t (ids [to] ++ " <- " ++ ids froms)
      Dropped t from -> line "dropped" t (ids [from])
  where
    line kind t rest = kind ++ " (" ++ drop 2 (show t) ++ "): " ++ rest
    ids = unwords . map getIdString

-- ===============
-- The side file
--
-- One line per root and per lifted def, in pointer order, after the
-- schema line and the module name.  Sites are rendered as in the dump
-- (kind, name, file, line, column, flags) and chains root-first as
-- site#count links, so that the file reads without the tables; the
-- only numbers in it are the pointers in the def names.  A def line:
--
--   def <name> | site <site> | chain <chain> | contexts <n>
--     | merged <n> {<site> @ <chain>}... | folded <n> {<site> @ <chain>}...
--     | parent <cell> | binder <binder> | alias <cell> | tiers <letters or ->
--     | validity <type window> <letters or ->
--     | repacks <n> {<repack>}... | undefined <n> {<kind>}...
--
-- where a cell is the name of the def lifted from it, _<pointer> when
-- no def was (a folded cell; the root of an alias class is the class's
-- earliest cell, lifted or not), or - for none; a binder is a parameter
-- as <name> [<file>:<line>:<col>], a field or operand as field <n>, or -;
--
-- the tier letters are u (use-site), d (definition-site),
-- h (head-site), s (shared) and m (merged); a type window is a type,
-- with [<offset>+<width>] for a window that is not the whole type; a
-- validity's letters are c (by construction), d (by declaration), a
-- (by assumption) and b (at a Bit type, trivially); a repack is
-- "<unpack or chr> <cell> from <validity> to <type> @ <chain>"; an
-- undefined value's kind is not-used, dont-care or no-match.
--
-- Every repack of the module is also listed on a line of its own, as
-- "repack <repack>", since the bits may have had no cell or a cell no
-- def was lifted from.  The line "validity census-grade" says that
-- the validity column is a census until the valid-unknown redesign and
-- instance lawfulness land.

sideFileSchema :: String
sideFileSchema = "bsc-metadata 1"

validityGrade :: String
validityGrade = "validity census-grade"

-- a binder site without its kind word, which the column's label gives
renderBinderSite :: SiteTable -> Int -> String
renderBinderSite t b =
    case siteRow t b of
      Just (SiteRow { sr_key = IdKey base qual p SKBinder }) ->
          renderName base qual ++ renderPosition p
      _ -> renderSite t b

renderChain :: SiteTable -> ContextTable -> Int -> String
renderChain _ _ 0 = "-"
renderChain st ct ctx =
    intercalate " -> " [ renderSite st s ++ "#" ++ show c
                       | (s, c) <- contextChain ct ctx ]

renderSideFile :: String -> ModuleMeta -> String
renderSideFile modName mm =
    unlines ([sideFileSchema, "module " ++ modName, validityGrade] ++
             [ "root " ++ renderSite st s | s <- mm_roots mm ] ++
             map renderDef (IM.elems (mm_defs mm)) ++
             [ "repack " ++ renderRepack r | r <- mm_repacks mm ])
  where
    st = mm_sites mm
    ct = mm_contexts mm
    tt = mm_types mm
    renderDef r =
        intercalate " | "
            [ "def " ++ defName r,
              "site " ++ renderSite st (dr_site r),
              "chain " ++ renderChain st ct (dr_context r),
              "contexts " ++ show (IS.size (dr_contexts r)),
              "merged " ++ renderFacts (dr_merged r),
              "folded " ++ renderFacts (dr_folded r),
              "parent " ++ renderCell (dr_parent r),
              "binder " ++ renderBinder (dr_binder r),
              "alias " ++ renderCell (dr_alias r),
              "tiers " ++ renderTiers r,
              "validity " ++ renderValidity tt (dr_validity r),
              "repacks " ++ show (length (dr_repacks r)) ++
                  concat [ " {" ++ renderRepack rp ++ "}" | rp <- dr_repacks r ],
              "undefined " ++ show (length (dr_undefs r)) ++
                  concat [ " {" ++ undefKindName k ++ "}" | k <- dr_undefs r ] ]
    renderFacts fs =
        show (length fs) ++
        concat [ " {" ++ renderSite st s ++ " @ " ++ renderChain st ct c ++ "}"
               | (s, c) <- fs ]
    renderRepack rp =
        (case rf_kind rp of
           RKUnpack -> "unpack "
           RKChr -> "chr ") ++
        renderCell (rf_cell rp) ++
        " from " ++ renderValidity tt (rf_from rp) ++
        " to " ++ renderTypeWindow tt (rf_to rp) ++
        " @ " ++ renderChain st ct (rf_context rp)
    defName r = defNameString (dr_ptr r) (dr_hint r)
    renderCell p | p == noCell = "-"
                 | otherwise = case IM.lookup p (mm_defs mm) of
                                 Just pr -> defName pr
                                 Nothing -> "_" ++ show p
    renderBinder b | b == noBinder = "-"
                   | otherwise = renderBinderSite st b
    renderTiers r =
        let row = siteRow st (dr_site r)
            isDef = maybe False ((== SKDef) . siteKeyKind . sr_key) row
            use = maybe False sr_useSite row
            letters = [ c | (c, on) <- [('u', use),
                                        ('d', isDef && not use),
                                        ('h', testBit (dr_site r) 60),
                                        ('s', dr_shared r),
                                        ('m', not (null (dr_merged r)))], on ]
        in  if null letters then "-" else letters

-- ===============
-- The census (-metadata-census)
--
-- The counters exist only under -metadata-census: every hook tests
-- ms_census first, so a plain run pays one test per push and allocates
-- nothing here.  The census is suppressed under -metadata-scramble,
-- whose store holds perturbed numbers by design, so the fact bits a
-- walk reads are true facts.  The evaluator prints the census at the
-- end of elaboration (renderCensus) from the module metadata table and
-- the headers of the lifted defs' cells, and runs the cross-checks
-- there (checkProfile and checkNameHints assert, checkKeptPositions
-- reports); the chain walk runs at every push (recordUnfolding,
-- recordCompletion) and counts the loops re-entered and the other
-- sites repeated on a chain.

data Census = Census {
        -- pushes at definition unfoldings (pushDefFrame)
        cs_unfoldings :: {-# UNPACK #-} !Int,
        -- pushes at the application of a cell's value (withCellDefFrame)
        cs_completions :: {-# UNPACK #-} !Int,
        -- root pushes by kind (fromEnum of the SiteKind)
        cs_roots :: !(IM.IntMap Int),
        -- unfolding pushes per site of the reference unfolded
        cs_perSite :: !(IM.IntMap Int),
        -- chains walked
        cs_walks :: {-# UNPACK #-} !Int,
        -- loops re-entered, by the site of the recursion point found on
        -- the chain above a frame of it
        cs_nested :: !(IM.IntMap Nesting),
        -- sites of definitions that are not recursion points found
        -- repeated on the chain of a new frame, by site
        cs_repeats :: !(IM.IntMap Int)
        }

-- A loop re-entered, aggregated over its dynamic instances: the nested
-- instances of the site (frames of it pushed with a count of 1 under a
-- frame of it), the iterations of those instances (every frame of it
-- pushed under one) and the deepest nesting (frames of it on one chain).
data Nesting = Nesting {-# UNPACK #-} !Int {-# UNPACK #-} !Int {-# UNPACK #-} !Int

joinNesting :: Nesting -> Nesting -> Nesting
joinNesting (Nesting i n d) (Nesting i' n' d') = Nesting (i + i') (n + n') (max d d')

emptyCensus :: Census
emptyCensus = Census 0 0 IM.empty IM.empty 0 IM.empty IM.empty

readCensus :: MetaStore -> IO Census
readCensus st = readIORef (ms_counts st)

-- Count an unfolding push, by the site of the reference unfolded (the
-- frame of a continued iteration carries the loop's entry site), and
-- walk its chain.
recordUnfolding :: MetaStore -> Int -> Frame -> IO ()
recordUnfolding st site fr = when (ms_census st) $
    modifyIORef' (ms_counts st) $ \cs ->
        let cs' = cs { cs_unfoldings = cs_unfoldings cs + 1,
                       cs_perSite = IM.insertWith (+) (plainSite site) 1 (cs_perSite cs) }
        in  walkChain fr cs'

-- Count a completion push (the application of a cell's value) and walk
-- its chain.
recordCompletion :: MetaStore -> Frame -> IO ()
recordCompletion st fr = when (ms_census st) $
    modifyIORef' (ms_counts st) $ \cs ->
        walkChain fr (cs { cs_completions = cs_completions cs + 1 })

recordRoot :: MetaStore -> Int -> IO ()
recordRoot st site = when (ms_census st) $ do
    t <- readIORef (ms_sites st)
    let kind = maybe SKBound (siteKeyKind . sr_key) (siteRow t site)
    modifyIORef' (ms_counts st) $ \cs ->
        cs { cs_roots = IM.insertWith (+) (fromEnum kind) 1 (cs_roots cs) }

frameSite :: Frame -> Int
frameSite TopFrame = 0
frameSite (Frame { fr_site = s }) = s

-- Walk the whole chain of a new frame.  Every frame is pushed after
-- its parent (pushFrame takes the register's frame or an ancestor of
-- it as the parent, pushRoot none) and the census runs with the seed
-- 0, so the ids are push numbers and fall toward the root: no frame is
-- on its own chain twice, and a chain that does not fall stops the
-- compile.  A repeated site is counted by what the site is, never
-- asserted.  A recursion point's site above a frame of it is a loop
-- re-entered: the loop's body ran the same loop again, through the
-- same reference, as a nested dynamic instance (a zipWith over a
-- vector of vectors runs the Vector package's zipWith loop inside an
-- iteration of itself when an element is forced).  Between the two
-- frames runs a loop of the same definition through another reference
-- (every static loop is a primFix reference), whose entry shadows the
-- outer instance's in the register's map, so the push nested a fresh
-- count under the current frame (pushFrame): two instances of one
-- site, not two iterations of one loop.  A repeated site whose
-- definition is not a recursion point is the evaluator heaping a
-- definition's arguments under the definition's own frame and
-- applying continuations inside the unfolding that received them, so
-- a definition applied within its own argument (a monadic bind whose
-- continuation binds again, f (f x)) nests under itself without
-- recursing.
walkChain :: Frame -> Census -> Census
walkChain TopFrame cs = cs
walkChain fr@(Frame { fr_id = i, fr_site = s, fr_count = c, fr_parent = p }) cs =
    let site = plainSite s
        -- the frames above that carry the site, each checked against
        -- the id of the frame below it
        above = go i fr 0 p
        go _ _ n TopFrame = n
        go j child n a@(Frame { fr_id = k, fr_site = t, fr_parent = q })
          | k >= j =
              internalError ("IExpandMeta.walkChain: a frame is on the chain of one pushed before it: "
                             ++ show a ++ " above " ++ show child)
          | otherwise = let n' = if plainSite t == site then n + 1 else n
                        in  n' `seq` go k a n' q
        cs' = cs { cs_walks = cs_walks cs + 1 }
    in  if above == 0 then cs'
        else if testBit s 59
        then cs' { cs_nested = IM.insertWith joinNesting site
                                 (Nesting (if c == 1 then 1 else 0) 1 (above + 1))
                                 (cs_nested cs') }
        else cs' { cs_repeats = IM.insertWith (+) site 1 (cs_repeats cs') }

-- A full distribution: every value present with its multiplicity, in
-- ascending order; no buckets.
distribution :: [Int] -> [(Int, Int)]
distribution xs = IM.toAscList (IM.fromListWith (+) [ (x, 1) | x <- xs ])

showDistribution :: [(Int, Int)] -> String
showDistribution d = unwords [ show v ++ ":" ++ show n | (v, n) <- d ]

-- the classes that hold another cell: a cell in such a class has an
-- entry or is the root some entry points at, and a root has no entry,
-- so the classes are the roots the entries point at
aliasClassCount :: AliasMap -> Int
aliasClassCount m = IS.size (IS.fromList (IM.elems m) `IS.difference` IM.keysSet m)

-- The census of one module: the lifted defs, each as its record in the
-- module table with the header of its cell; the roots and frames by
-- kind; the depth and count distributions; the alias classes after the
-- CSE unions; and the cost counters.  The shared and merged facts are
-- the records' (dr_shared, dr_merged), the context-table operations
-- are the steps internFrame took over the kept cells (ct_ops).
renderCensus :: String -> ModuleMeta -> Census -> Int -> Int
             -> [(DefRecord, CellMeta)] -> String
renderCensus modName mm cs cells aliasClasses defs = unlines $
    [ "metadata census for " ++ modName ++ ":",
      "  cells reachable " ++ show cells ++ ", defs lifted " ++ show (length defs)
        ++ ", defs with chains " ++ show (length withChains),
      "  roots pushed: " ++ byKind rootKinds (cs_roots cs),
      "  pushes: unfoldings " ++ show (cs_unfoldings cs)
        ++ ", completions " ++ show (cs_completions cs)
        ++ ", roots " ++ show rootPushes,
      "  frames on chains " ++ show (IM.size frames) ++ ": "
        ++ counts [("use-site", frameUse), ("definition-site", frameDef),
                   ("root", frameRoot)],
      "  defs by site: " ++ counts
        [ ("use-site", nDef True), ("definition-site", nDef False),
          ("prim", nKind SKPrim), ("con", nKind SKCon), ("method", nKind SKMethod),
          ("foreign", nKind SKForeign), ("lambda", nKind SKLambda),
          ("bound", nKind SKBound), ("inst", nKind SKInst) ]
        ++ "; head-site " ++ show (count (testBit60 . dr_site) defs)
        ++ ", shared " ++ show (count dr_shared defs)
        ++ ", merged " ++ show (count (not . null . dr_merged) defs),
      "  depth: " ++ showDistribution (distribution (map (length . chainOf . snd) withChains)),
      "  count: " ++ showDistribution (distribution [ c | Frame { fr_count = c } <- IM.elems frames ]),
      "  alias classes " ++ show aliasClasses,
      "  validity: defs with a must-type " ++ show (length withType)
        ++ " (at a Bit type " ++ show (nTier 57)
        ++ "; by construction " ++ show (nTier 58) ++ ", by declaration " ++ show (nTier 59)
        ++ ", by assumption " ++ show (nTier 60) ++ "), type windows "
        ++ show (IM.size (tt_rows (mm_types mm))),
      "  repacks " ++ show (length (mm_repacks mm))
        ++ " (unpack " ++ show (length [ () | r <- mm_repacks mm, rf_kind r == RKUnpack ])
        ++ ", chr " ++ show (length [ () | r <- mm_repacks mm, rf_kind r == RKChr ])
        ++ "), on lifted defs " ++ show (sum (map (length . dr_repacks . fst) defs))
        ++ "; undefined values on lifted defs " ++ show (sum (map (length . dr_undefs . fst) defs)),
      "  cost: frame pushes " ++ show (cs_unfoldings cs + cs_completions cs + rootPushes)
        ++ ", context-table operations " ++ show (ct_ops ct)
        ++ " for " ++ show (contextTableSize ct) ++ " contexts",
      "  chains walked " ++ show (cs_walks cs)
        ++ ", loops re-entered " ++ show (IM.size (cs_nested cs)) ++ nestings
        ++ ", repeated non-recursion sites " ++ show (sum (IM.elems (cs_repeats cs)))
        ++ repeats ]
  where
    t = mm_sites mm
    ct = mm_contexts mm
    withChains = [ d | d@(_, m) <- defs, not (null (chainOf m)) ]
    withType = [ d | d@(r, _) <- defs, dr_validity r /= rawValidity ]
    nTier b = length [ () | (r, _) <- withType, testBit (dr_validity r) b ]
    rootKinds = [SKRule, SKMethodRoot, SKInst]
    rootPushes = sum (IM.elems (cs_roots cs))
    byKind ks m = counts [ (showSiteKind k, IM.findWithDefault 0 (fromEnum k) m) | k <- ks ]
    counts kvs = intercalate ", " [ k ++ " " ++ show v | (k, v) <- kvs ]
    count f = length . filter (f . fst)
    testBit60 s = testBit s 60
    -- every distinct frame on the chains of the lifted defs
    frames = L.foldl' addChain IM.empty withChains
    addChain m (_, cm) = go (cm_frame cm) m
      where go TopFrame acc = acc
            go fr@(Frame { fr_id = i, fr_parent = p }) acc
              | IM.member i acc = acc
              | otherwise = go p (IM.insert i fr acc)
    frameFacts = [ (siteRow t (frameSite fr), fr) | fr <- IM.elems frames ]
    frameUse = length [ () | (Just r, _) <- frameFacts, isDefRow r, sr_useSite r ]
    frameDef = length [ () | (Just r, _) <- frameFacts, isDefRow r, not (sr_useSite r) ]
    frameRoot = length [ () | (Just r, _) <- frameFacts, siteKeyKind (sr_key r) `elem` rootKinds ]
    isDefRow r = siteKeyKind (sr_key r) == SKDef
    defRows = [ (siteRow t (dr_site r), r) | (r, _) <- defs ]
    nDef u = length [ () | (Just r, _) <- defRows, isDefRow r, sr_useSite r == u ]
    nKind k = length [ () | (Just r, _) <- defRows, siteKeyKind (sr_key r) == k ]
    -- the five most re-entered loops: the nested instances, their
    -- iterations and the deepest nesting
    nestings = case sortBy (comparing (\ (_, Nesting i _ _) -> negate i)) (IM.toList (cs_nested cs)) of
                 [] -> ""
                 ns -> " (" ++ intercalate "; " [ renderSite t s ++ ": " ++ show i ++ " instances, "
                                                  ++ show n ++ " iterations, depth " ++ show d
                                                | (s, Nesting i n d) <- take 5 ns ] ++ ")"
    repeats = case sortBy (comparing (negate . snd)) (IM.toList (cs_repeats cs)) of
                [] -> ""
                rs -> " (" ++ intercalate "; " [ renderSite t s ++ " x" ++ show n
                                               | (s, n) <- take 5 rs ] ++ ")"

-- The unfolding pushes per definition site against the profiling map
-- (IExpandUtils.step counts every unfolding by definition and
-- position): every entry of each must be in the other with the same
-- count.  Returns the mismatches rendered.
checkProfile :: SiteTable -> Census -> [(Id, Position, Int)] -> [String]
checkProfile t cs profile =
    let pushes = [ (key, n) | (s, n) <- IM.toList (cs_perSite cs),
                              Just (SiteRow { sr_key = key }) <- [siteRow t s] ]
        pushMap = M.fromListWith (+) pushes
        profMap = M.fromListWith (+)
                    [ (IdKey (getIdBase i) (getIdQual i) p SKDef, n) | (i, p, n) <- profile ]
        onlyPushes = [ renderKey k ++ ": " ++ show n ++ " frames, no profile entry"
                     | (k, n) <- M.toList (M.difference pushMap profMap) ]
        onlyProf = [ renderKey k ++ ": " ++ show n ++ " unfoldings profiled, no frame"
                   | (k, n) <- M.toList (M.difference profMap pushMap) ]
        differ = [ renderKey k ++ ": " ++ show a ++ " frames, " ++ show b ++ " unfoldings profiled"
                 | (k, (a, b)) <- M.toList (M.intersectionWith (,) pushMap profMap), a /= b ]
    in  onlyPushes ++ onlyProf ++ differ

renderKey :: SiteKey -> String
renderKey (IdKey b q p k) = showSiteKind k ++ " " ++ renderName b q ++ renderPosition p
renderKey (PrimKey op) = "prim " ++ show op
renderKey (RootKey n k) = showSiteKind k ++ " " ++ n
renderKey BoundKey = "bound -"
renderKey (FieldKey n) = "field " ++ show n

-- The name hints against the binders.  A cell heaped as the argument
-- of a lambda takes its parameter as its binder and as its name hint
-- (IExpandUtils.toHeapBinder with BoundTo), and improveCellName keeps
-- the hint when the parameter's position is outside the library, so
-- every such cell is named by its parameter; a hint at a library
-- position is replaced by one inferred from the expression and is not
-- checked.  Returns the number of cells bound to a parameter, the
-- number of those checked, and the cells whose hint is not their
-- parameter, rendered.
checkNameHints :: SiteTable -> [(Int, CellMeta, Maybe Id)] -> (Int, Int, [String])
checkNameHints t cells =
    let bound = [ (p, m, hint, key)
                | (p, m, hint) <- cells,
                  Just (SiteRow { sr_key = key@(IdKey _ _ _ SKBinder) }) <- [siteRow t (binderOf m)] ]
        checked = [ c | c@(_, _, _, IdKey _ _ pos _) <- bound, not (isPreludePosition pos) ]
        hintKey n = idSiteKey SKBinder n
        renderHint = maybe "-" (\ n -> renderName (getIdBase n) (getIdQual n) ++
                                      renderPosition (getIdPosition n))
        bad = [ "_" ++ show p ++ " (" ++ renderSite t (siteOf m) ++ "): binder "
                ++ renderKey key ++ ", name hint " ++ renderHint hint
              | (p, m, hint, key) <- checked, maybe True ((/= key) . hintKey) hint ]
    in  (length bound, length checked, bad)

-- The gate against the facts and the types: a cell with a validity is
-- a gated cell, and the cell of a lifted def (whose type the rebuild
-- has in hand) is gated exactly when the def's type admits facts.
-- Returns the gated cells, the cells with facts, the defs checked and
-- the mismatches rendered.
checkBitGate :: SiteTable -> [(Int, CellMeta, Maybe IType)] -> (Int, Int, Int, [String])
checkBitGate t cells =
    let gated = length [ () | (_, m, _) <- cells, bitCell m ]
        withFacts = [ c | c@(_, m, _) <- cells, validityOf m /= rawValidity ]
        typed = [ c | c@(_, _, Just _) <- cells ]
        bad = [ "_" ++ show p ++ " (" ++ renderSite t (siteOf m) ++ "): facts outside the gate"
              | (p, m, _) <- withFacts, not (bitCell m) ] ++
              [ "_" ++ show p ++ " (" ++ renderSite t (siteOf m) ++ "): gate "
                ++ show (bitCell m) ++ " for the type " ++ ppString ty
              | (p, m, Just ty) <- typed, bitCell m /= bitCellType ty ]
    in  (gated, length withFacts, length typed, bad)

-- The positions kept for method conditions against the chains: a
-- position recorded on a method call (the selections the call was
-- reached through) is on the chain when it is the position of the
-- cell's own site or of the site of a frame on its chain.  Returns the
-- (cell, position) pairs that are not, rendered; the evaluator reports
-- their number.
checkKeptPositions :: SiteTable -> [(Int, CellMeta, [Position])] -> [String]
checkKeptPositions t cells =
    [ "_" ++ show p ++ " (" ++ renderSite t (siteOf m) ++ "): position" ++ renderPosition pos
    | (p, m, poss) <- cells,
      let onChain = mapMaybe (sitePosition t) (siteOf m : map fst (chainOf m)),
      pos <- poss, pos `notElem` onChain ]

sitePosition :: SiteTable -> Int -> Maybe Position
sitePosition t s = case siteRow t s of
                     Just (SiteRow { sr_key = IdKey _ _ p _ }) -> Just p
                     _ -> Nothing
