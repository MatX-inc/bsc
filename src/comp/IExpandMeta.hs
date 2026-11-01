module IExpandMeta(
        -- sites
        SiteKind(..), SiteKey, idSiteKey, primSiteKey, rootSiteKey, boundSiteKey,
        SiteRow(..), SiteTable, siteRow, siteTableSize,
        -- frames, binders and cell headers
        Frame, Binder(..), CellMeta, noBinder, headSiteOf, cellDefSite,
        -- the store and the hooks the evaluator calls
        MetaStore, newMetaStore, lookupSite, mintSite, pushFrame, pushRoot,
        swapFrame, swapCellFrame, stampMeta, aliasUnion, readRoots,
        -- the tables after elaboration and the query interface, read by
        -- the rebuild and the dump alone
        MetaTables(..), AliasMap, readMetaTables,
        siteOf, frameOf, binderOf, chainOf, aliasClassOf, aliasMembers,
        renderSite, renderDump,
        -- the module metadata table, built by the rebuild
        ContextTable, emptyContextTable, contextTableSize, internFrame,
        contextChain, aliasUnions, noCell, DefRecord(..), ModuleMeta(..),
        emptyModuleMeta, defNameString, sideFileSchema, renderSideFile
        ) where

import Control.Monad(when)
import Data.Bits(bit, (.|.), (.&.), complement, xor, testBit)
import Data.Char(isAlphaNum)
import Data.IORef
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List(intercalate)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import System.FilePath(takeFileName)

import ErrorUtil(internalError)
import Eval(NFData(..), rnf2, rnf3, rnf4, rnf11)
import FStringCompat(FString, getFString)
import Id(Id, getIdBase, getIdQual, getIdPosition)
import Position(Position(..), noPosition)
import Prim(PrimOp)

-- ===============
-- The evaluator metadata store
--
-- The store records, for every heap cell, the lexical site of the
-- expression the cell was created for, the chain of definition
-- unfoldings it was created under (the frame), rooted at a rule, a
-- method or an instantiation, the binder the cell's value was bound
-- to (a parameter, or the index of a field or operand it was moved
-- into), and the alias class of cells that hold one value.  It is
-- written by the evaluator through the hooks below and read only
-- through the query interface: by the rebuild at the end of
-- elaboration, which also builds the module metadata table from it
-- (IExpand.buildModuleMeta; the side-file renderer, renderSideFile,
-- reads that table), and by the -metadata-dump printer.
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
-- cell was created for, the frame current when it was created, and the
-- site of its binder (noBinder when the cell was created for no
-- parameter, field or operand).  It is written once, at allocation,
-- and copied across every state transition of the cell.
data CellMeta = CellMeta { cm_site :: {-# UNPACK #-} !Int,
                           cm_frame :: !Frame,
                           cm_binder :: {-# UNPACK #-} !Int }

-- Site ids start at 1, so 0 is free to mean no binder.
noBinder :: Int
noBinder = 0

instance Show CellMeta where
    show (CellMeta s f b) = "CellMeta " ++ show s ++ " (" ++ show f ++ ")" ++
                            (if b == noBinder then "" else " binder " ++ show b)

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
-- sites of the root frames pushed so far (most recent first) and the
-- scramble seed (0 when scrambling is off).  One store per module
-- elaboration.
data MetaStore = MetaStore { ms_sites :: !(IORef SiteTable),
                             ms_nextFrame :: !(IORef Int),
                             ms_frame :: !(IORef Frame),
                             ms_alias :: !(IORef AliasMap),
                             ms_roots :: !(IORef [Int]),
                             ms_seed :: {-# UNPACK #-} !Int }

newMetaStore :: Int -> IO MetaStore
newMetaStore seed = do
    sites <- newIORef emptySiteTable
    next <- newIORef 1
    reg <- newIORef TopFrame
    alias <- newIORef IM.empty
    roots <- newIORef []
    return (MetaStore sites next reg alias roots seed)

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

-- The header for a cell allocated now at a site, with its binder (whose
-- site is minted here, on the first cell bound to it).
{-# INLINE stampMeta #-}
stampMeta :: MetaStore -> Int -> Binder -> IO CellMeta
stampMeta st site b = do
    fr <- readIORef (ms_frame st)
    binder <- case b of
                NoBinder -> return noBinder
                BoundTo i -> mint (idSiteKey SKBinder i)
                MovedTo n -> mint (FieldKey n)
    return $! CellMeta site fr binder
  where mint key = fst <$> mintSite st key (False, False)

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

-- The tables of the store after elaboration: the site table and the
-- alias classes.
data MetaTables = MetaTables { mt_sites :: !SiteTable,
                               mt_alias :: !AliasMap }

readMetaTables :: MetaStore -> IO MetaTables
readMetaTables st = do
    sites <- readIORef (ms_sites st)
    alias <- readIORef (ms_alias st)
    return (MetaTables sites alias)

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

-- One line per cell, in the order given (the caller passes ascending
-- pointer order): the pointer, the cell's site, its binder when it has
-- one, the root of its alias class when the class has another cell,
-- then the chain of frames from the creating frame to the root, each
-- as site#count.
renderDump :: MetaTables -> [(Int, CellMeta)] -> String
renderDump (MetaTables t am) cells = unlines (map line cells)
  where line (p, m) =
            "_" ++ show p ++ ": " ++ renderSite t (siteOf m) ++
            binder (binderOf m) ++ alias p ++
            concatMap link (chainOf m)
        binder b | b == noBinder = ""
                 | otherwise = ", " ++ renderSite t b
        alias p | IS.member p members = ", alias _" ++ show (aliasClassOf am p)
                | otherwise = ""
        members = aliasMembers am
        link (s, c) = " <- " ++ renderSite t s ++ "#" ++ show c

-- ===============
-- The module metadata table
--
-- Built once by the rebuild (IExpand.buildModuleMeta) from the headers
-- of the cells it keeps, after CSE, and carried on the IModule.  It is
-- read by nothing that orders or chooses: the side file renders it, and
-- it is never serialized.

-- A context is one distinct frame chain, as the triple (parent context,
-- site, count); context 0 is the top, under which no frame was pushed.
-- Contexts are interned bottom-up from the frames of the kept cells,
-- memoized by frame id, so that frames no kept cell references are never
-- visited and identical chains share one id.  This is the only place
-- context ids exist; nothing during evaluation reads or writes them.
data ContextTable = ContextTable { ct_next :: {-# UNPACK #-} !Int,
                                   ct_ids :: !(M.Map (Int, Int, Int) Int),
                                   ct_rows :: !(IM.IntMap (Int, Int, Int)) }

emptyContextTable :: ContextTable
emptyContextTable = ContextTable 1 M.empty IM.empty

contextTableSize :: ContextTable -> Int
contextTableSize t = IM.size (ct_rows t)

-- Intern the chain of a frame and return its context id; the memo (by
-- frame id) is threaded through the cells of one module.  The recursion
-- is one step per frame of the chain not yet memoized.
internFrame :: (ContextTable, IM.IntMap Int) -> Frame
            -> ((ContextTable, IM.IntMap Int), Int)
internFrame st TopFrame = (st, 0)
internFrame st@(_, memo) (Frame { fr_id = fid, fr_site = s, fr_count = c,
                                  fr_parent = parent }) =
    case IM.lookup fid memo of
      Just ctx -> (st, ctx)
      Nothing ->
        let ((t1, memo1), pctx) = internFrame st parent
            key = (pctx, s, c)
        in  case M.lookup key (ct_ids t1) of
              Just ctx -> ((t1, IM.insert fid ctx memo1), ctx)
              Nothing ->
                let ctx = ct_next t1
                    t2 = ContextTable (ctx + 1)
                                      (M.insert key ctx (ct_ids t1))
                                      (IM.insert ctx key (ct_rows t1))
                in  ((t2, IM.insert fid ctx memo1), ctx)

-- (site, count) of every frame of a context, from the root
contextChain :: ContextTable -> Int -> [(Int, Int)]
contextChain t = go []
  where go acc 0 = acc
        go acc ctx = case IM.lookup ctx (ct_rows t) of
                       Just (p, s, c) -> go ((s, c) : acc) p
                       Nothing -> internalError ("IExpandMeta.contextChain: " ++
                                                 show ctx)

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
        dr_alias :: {-# UNPACK #-} !Int
    }

-- Heap pointers start at 0, so -1 is free to mean no cell.
noCell :: Int
noCell = -1

instance NFData DefRecord where
    rnf (DefRecord x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11) =
        rnf11 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11

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

-- The table: the sites, the contexts, the root sites (rules, methods and
-- instantiations, in push order) and the def records, keyed by the
-- canonical heap pointer (the number the def's name ends in).
data ModuleMeta = ModuleMeta { mm_sites :: !SiteTable,
                               mm_contexts :: !ContextTable,
                               mm_roots :: ![Int],
                               mm_defs :: !(IM.IntMap DefRecord) }

emptyModuleMeta :: ModuleMeta
emptyModuleMeta = ModuleMeta emptySiteTable emptyContextTable [] IM.empty

-- compact, so that the derived Show of an IModule stays printable
instance Show ModuleMeta where
    show mm = "ModuleMeta sites " ++ show (siteTableSize (mm_sites mm)) ++
              " contexts " ++ show (contextTableSize (mm_contexts mm)) ++
              " roots " ++ show (length (mm_roots mm)) ++
              " defs " ++ show (IM.size (mm_defs mm))

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
    rnf (ContextTable n ids rows) = rnf3 n ids rows

instance NFData ModuleMeta where
    rnf (ModuleMeta s c r d) = rnf4 s c r d

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
--
-- where a cell is the name of the def lifted from it, _<pointer> when
-- no def was (a folded cell; the root of an alias class is the class's
-- earliest cell, lifted or not), or - for none; a binder is a parameter
-- as <name> [<file>:<line>:<col>], a field or operand as field <n>, or -;
--
-- and the tier letters are u (use-site), d (definition-site),
-- h (head-site), s (shared) and m (merged).

sideFileSchema :: String
sideFileSchema = "bsc-metadata 1"

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
    unlines ([sideFileSchema, "module " ++ modName] ++
             [ "root " ++ renderSite st s | s <- mm_roots mm ] ++
             map renderDef (IM.elems (mm_defs mm)))
  where
    st = mm_sites mm
    ct = mm_contexts mm
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
              "tiers " ++ renderTiers r ]
    renderFacts fs =
        show (length fs) ++
        concat [ " {" ++ renderSite st s ++ " @ " ++ renderChain st ct c ++ "}"
               | (s, c) <- fs ]
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
