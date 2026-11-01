module IExpandMeta(
        -- sites
        SiteKind(..), SiteKey, idSiteKey, primSiteKey, rootSiteKey, boundSiteKey,
        SiteRow(..), SiteTable, siteRow, siteTableSize,
        -- frames and cell headers
        Frame, CellMeta, headSiteOf,
        -- the store and the hooks the evaluator calls
        MetaStore, newMetaStore, lookupSite, mintSite, pushFrame, pushRoot,
        swapFrame, swapCellFrame, stampMeta, readSiteTable,
        -- the query interface, read by the rebuild and the dump alone
        siteOf, frameOf, chainOf, renderSite, renderDump
        ) where

import Data.Bits(bit, (.|.), (.&.), complement, xor, testBit)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import System.FilePath(takeFileName)

import FStringCompat(FString, getFString)
import Id(Id, getIdBase, getIdQual, getIdPosition)
import Position(Position(..), noPosition)
import Prim(PrimOp)

-- ===============
-- The evaluator metadata store
--
-- The store records, for every heap cell, the lexical site of the
-- expression the cell was created for and the chain of definition
-- unfoldings it was created under (the frame), rooted at a rule, a
-- method or an instantiation.  It is written by the evaluator through
-- the hooks below and read only through the query interface, by the
-- rebuild at the end of elaboration and by the -metadata-dump printer.
--
-- Rules that keep the store invisible to the generated output:
--
--  * The store creates no interned string.  Site keys hold FStrings and
--    Positions that already exist (compared by intern id) or a plain
--    String that is never interned; rendering reads existing strings.
--  * Every field is strict; a frame points only at frames already built.
--  * Nothing here allocates a heap cell, forces a cell or reads one.
--  * Under -metadata-scramble every number the store mints (site ids,
--    frame ids, counts, the row bits) is perturbed with a per-run seed
--    where it is created, so an evaluator decision that read the store
--    would change the output of the scramble run.  The dump is then
--    suppressed, because the store's content is garbage by design.

-- The kind of expression a site stands for.  SKBound is a boundary
-- constant (a port, a parameter, a clock, a literal, ...) whose
-- identifier the stamp does not read, because the evaluator builds some
-- of those identifiers lazily from strings that may not be interned yet;
-- all such cells share one row.  SKRule, SKMethodRoot and SKInst are
-- the roots of frame chains.
data SiteKind = SKDef | SKPrim | SKCon | SKMethod | SKForeign | SKLambda
              | SKBound | SKRule | SKMethodRoot | SKInst
        deriving (Eq, Ord, Show, Enum, Bounded)

-- A site key.  Identifier-based keys compare by intern id and position
-- (O(1) per component); a primitive is keyed on its operator alone,
-- because the evaluator builds primitive heads from PreIds whose strings
-- need not have been interned yet and the stamp must not force them; a
-- root is keyed on a name string that is read, never interned.
data SiteKey
  = IdKey !FString !FString !Position !SiteKind  -- base, qualifier, position, kind
  | PrimKey !PrimOp
  | RootKey !String !SiteKind
  | BoundKey
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

data SiteTable = SiteTable { st_next :: {-# UNPACK #-} !Int,
                             st_ids :: !(M.Map SiteKey SiteRef),
                             st_rows :: !(IM.IntMap SiteRow) }

emptySiteTable :: SiteTable
emptySiteTable = SiteTable 1 M.empty IM.empty

siteTableSize :: SiteTable -> Int
siteTableSize t = IM.size (st_rows t)

-- A site id whose head-site bit is set names the site of the cell at
-- the head of an application whose head is a heap reference: the
-- application's own site is not known on the mirror (the node carries
-- no annotation), so the cell takes the head cell's site, marked.
-- Row lookups mask the bit.
headSiteBit :: Int
headSiteBit = bit 60

siteRow :: SiteTable -> Int -> Maybe SiteRow
siteRow t s = IM.lookup (s .&. complement headSiteBit) (st_rows t)

-- A frame: one definition unfolding, or a root.  fr_count is the
-- iteration count of a recursion point (one more than the count of the
-- nearest same-site frame on the chain); fr_rec maps every recursion
-- point site on the chain to its count and the frame it was pushed
-- under, so a push at the same site finds its predecessor in O(log n)
-- without walking the chain.  The map entry records the count and the
-- parent instead of the frame itself, so that a frame never points at
-- itself through its own map.  TopFrame is the register's value before
-- any root frame is pushed (module arguments, instantiation of the
-- top module's state).
data Frame = TopFrame
           | Frame { fr_id :: {-# UNPACK #-} !Int,
                     fr_site :: {-# UNPACK #-} !Int,
                     fr_count :: {-# UNPACK #-} !Int,
                     fr_parent :: !Frame,
                     fr_rec :: !(IM.IntMap RecEntry) }

-- the count and the parent frame of a recursion point on the chain
data RecEntry = RecEntry {-# UNPACK #-} !Int !Frame

frameRec :: Frame -> IM.IntMap RecEntry
frameRec TopFrame = IM.empty
frameRec (Frame { fr_rec = m }) = m

-- compact, so that a derived Show of a heap cell stays printable
instance Show Frame where
    show TopFrame = "top"
    show (Frame { fr_id = i, fr_site = s, fr_count = c }) =
        "frame " ++ show i ++ " site " ++ show s ++ " count " ++ show c

-- The header every heap cell carries: the site of the expression the
-- cell was created for and the frame current when it was created.
-- It is written once, at allocation, and copied across every state
-- transition of the cell.
data CellMeta = CellMeta { cm_site :: {-# UNPACK #-} !Int,
                           cm_frame :: !Frame }

instance Show CellMeta where
    show (CellMeta s f) = "CellMeta " ++ show s ++ " (" ++ show f ++ ")"

{-# INLINE headSiteOf #-}
headSiteOf :: CellMeta -> Int
headSiteOf m = cm_site m .|. headSiteBit

-- The store: the site table, the frame counter, the frame register
-- (the frame current for the next allocation) and the scramble seed
-- (0 when scrambling is off).  One store per module elaboration.
data MetaStore = MetaStore { ms_sites :: !(IORef SiteTable),
                             ms_nextFrame :: !(IORef Int),
                             ms_frame :: !(IORef Frame),
                             ms_seed :: {-# UNPACK #-} !Int }

newMetaStore :: Int -> IO MetaStore
newMetaStore seed = do
    sites <- newIORef emptySiteTable
    next <- newIORef 1
    reg <- newIORef TopFrame
    return (MetaStore sites next reg seed)

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
            s = perturb st (st_next t)
            r = perturbBool st recursive
            row = SiteRow key (perturbBool st useSite) r
            t' = SiteTable { st_next = st_next t + 1,
                             st_ids = M.insert key (SiteRef s r) (st_ids t),
                             st_rows = IM.insert s row (st_rows t) }
        writeIORef (ms_sites st) $! t'
        return (s, r)

-- Push a frame for a site under the register's frame and return it
-- (the caller runs the unfolding under it with withFrame).  A push at
-- a recursion point whose site is already on the chain continues that
-- iteration: the new frame's parent is the predecessor's parent and its
-- count one more; the frames between the two iterations belong to the
-- previous iteration's body and stay on the cells they stamped.
pushFrame :: MetaStore -> Int -> Bool -> IO Frame
pushFrame st site recursive = do
    parent <- readIORef (ms_frame st)
    n <- readIORef (ms_nextFrame st)
    writeIORef (ms_nextFrame st) $! n + 1
    let fid = perturb st n
        rec = frameRec parent
        fr = case (recursive, IM.lookup site rec) of
               (True, Just (RecEntry c p)) ->
                   let c' = perturb st (c + 1)
                   in  Frame fid site c' p
                             (IM.insert site (RecEntry c' p) (frameRec p))
               (True, Nothing) ->
                   let c1 = perturb st 1
                   in  Frame fid site c1 parent
                             (IM.insert site (RecEntry c1 parent) rec)
               (False, _) -> Frame fid site (perturb st 1) parent rec
    return $! fr

-- Push a root frame: a rule, a method or an instantiation.  A root
-- chains to nothing: the unfoldings that created the rule or the
-- instance are hierarchy, not provenance of the values built under it.
pushRoot :: MetaStore -> Int -> IO Frame
pushRoot st site = do
    n <- readIORef (ms_nextFrame st)
    writeIORef (ms_nextFrame st) $! n + 1
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

-- The header for a cell allocated now at a site.
{-# INLINE stampMeta #-}
stampMeta :: MetaStore -> Int -> IO CellMeta
stampMeta st site = do
    fr <- readIORef (ms_frame st)
    return $! CellMeta site fr

readSiteTable :: MetaStore -> IO SiteTable
readSiteTable st = readIORef (ms_sites st)

-- ===============
-- The query interface

siteOf :: CellMeta -> Int
siteOf = cm_site

frameOf :: CellMeta -> Frame
frameOf = cm_frame

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
            flags = [ c | (c, on) <- [('h', testBit s 60),
                                      ('u', sr_useSite row),
                                      ('r', sr_recursive row)], on ]
        in  showSiteKind (siteKeyKind key) ++ " " ++ body ++
            (if null flags then "" else " " ++ flags)

-- One line per cell, in the order given (the caller passes ascending
-- pointer order): the pointer, the cell's site, then the chain of
-- frames from the creating frame to the root, each as site#count.
renderDump :: SiteTable -> [(Int, CellMeta)] -> String
renderDump t cells = unlines (map line cells)
  where line (p, m) =
            "_" ++ show p ++ ": " ++ renderSite t (siteOf m) ++
            concatMap link (chainOf m)
        link (s, c) = " <- " ++ renderSite t s ++ "#" ++ show c
