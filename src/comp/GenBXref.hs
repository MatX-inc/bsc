{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenBXref(BXrefFile(..), BXrefStage(..), bxrefStageName,
                genBXrefFile, readBXrefFile, checkIdentTable) where

import Error(internalError, ErrMsg(..), ErrorHandle, bsErrorUnsafe)
import Position(Position, noPosition)
import Version(bscVersionStr)

import BinData
import IExpandMeta(ModuleMeta(..), SiteTable(..), SiteKey(..), SiteKind,
                   SiteRow(..), RecKey(..), ContextTable(..), DefRecord(..),
                   MetaEvent(..), PassTag, siteIdsFromRows, contextIdsFromRows,
                   IdentRecord(..), IdentKind, IdentOrigin(..), renderIdent)
import FileIOUtil(writeBinaryFileCatch)

import Data.Array(Array, listArray, (!))
import Data.Bits((.&.), (.|.), shiftL, shiftR)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

-- ===============
-- The .bxref file
--
-- <module>.bxref, written beside the module's .ba under -bxref, is the
-- module's cross-reference: the evaluator's metadata table
-- (IExpandMeta.ModuleMeta: the sites, the contexts, the roots and one
-- record per lifted def), the ledger of what the passes since
-- elaboration did to the def and rule ids, as the ledger stood when
-- the module's code was generated, and the identifier records of the
-- Verilog backend's join (VerilogMeta: each identifier of the
-- generated module with what it stands for).  It is an artifact of its own: the
-- .ba never carries the table or the ledger, and nothing in a compile
-- reads a .bxref; its readers are the tools (dumpbxref).  The file is
-- the format tag below, then one BinData container holding a
-- BXrefFile, written with the -remap-path-prefix remap the .ba's
-- positions get; the identifier records go into the container as the
-- identifier table below, which stores what they repeat once.

-- .bxref file tag -- change this whenever the .bxref format changes
-- See also GenABin.header and GenBin.header
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-bxref-20261102-4"

headerBS :: B.ByteString
headerBS = B.pack header

-- The stage the ledger had reached when the file was written: the end
-- of the Verilog backend (the final ASPackage, after finalCleanup); the
-- final APackage, which the Bluesim backend takes; or the package
-- handed to the scheduler, when scheduling failed.
data BXrefStage = BXVerilog | BXBluesim | BXSchedErr
        deriving (Eq, Show, Enum, Bounded)

-- the stage's name in the file's text renderings
bxrefStageName :: BXrefStage -> String
bxrefStageName BXVerilog = "verilog"
bxrefStageName BXBluesim = "bluesim"
bxrefStageName BXSchedErr = "schederr"

data BXrefFile = BXrefFile {
        -- the compiler that wrote the file (bscVersionStr True, as a
        -- .ba's ab_version); the reader accepts no other
        bx_version :: String,
        -- the module's name
        bx_module :: String,
        bx_stage :: BXrefStage,
        -- the table and the ledger
        bx_meta :: ModuleMeta,
        -- the identifier records of the generated module, in
        -- declaration order; empty at the bluesim and schederr stages,
        -- where no Verilog identifier exists
        bx_idents :: [IdentRecord]
    }

genBXrefFile :: ErrorHandle -> (Position -> Position) -> String -> BXrefFile
             -> IO ()
genBXrefFile errh remapP fn bxfile =
    writeBinaryFileCatch errh fn (header ++ encodeWith remapP bxfile)

-- Decoded strictly, as a .ba is (GenABin.readABinFile): the tools read
-- the whole file.  A file with another tag, or written by another
-- compiler build, is rejected as a .ba is (ABinUtil.decodeABin): the
-- metadata is produced and consumed by one compiler.
readBXrefFile :: ErrorHandle -> String -> B.ByteString -> BXrefFile
readBXrefFile errh nm s =
    let hlen = B.length headerBS
        bxfile = decodeStrict (B.drop hlen s)
    in  if B.take hlen s /= headerBS || bx_version bxfile /= bscVersionStr True
        then bsErrorUnsafe errh [(noPosition, EBinFileVerMismatch nm)]
        else bxfile

-- ----------
-- Bin instances of the table and the ledger.  They live here rather
-- than beside their types because BinData imports ISyntax, which
-- carries the ledger.  An IntMap or IntSet is written as its ascending
-- list and an enumeration as its index, checked against its bounds
-- when read, so the bytes are a function of the content.  The two
-- inverse maps of the tables (st_ids, ct_ids) are not written: both
-- are rebuilt from the rows on read (IExpandMeta.siteIdsFromRows,
-- contextIdsFromRows), and SiteKey orders by intern id, so writing
-- st_ids would make the bytes depend on the live intern table.

instance Bin BXrefFile where
    writeBytes (BXrefFile v m s mm is) =
        section "BXrefFile" $
        do toBin v
           toBin m
           toBin s
           toBin mm
           toBin (encodeIdents is)
    readBytes = do v <- fromBin
                   m <- fromBin
                   s <- fromBin
                   mm <- fromBin
                   is <- fmap decodeIdents fromBin
                   return (BXrefFile v m s mm is)

instance Bin BXrefStage where
    writeBytes s = putI (fromEnum s)
    readBytes = fmap toEnumOrErr getI

instance Bin ModuleMeta where
    writeBytes (ModuleMeta s c r d e) =
        do toBin s
           toBin c
           toBin r
           toBin (IM.toAscList d)
           toBin e
    readBytes = do s <- fromBin
                   c <- fromBin
                   r <- fromBin
                   d <- fromBin
                   e <- fromBin
                   return (ModuleMeta s c r (IM.fromDistinctAscList d) e)

instance Bin SiteTable where
    writeBytes (SiteTable n _ rows recs) =
        do toBin n
           toBin (IM.toAscList rows)
           toBin (IM.toAscList recs)
    readBytes = do n <- fromBin
                   rows <- fromBin
                   recs <- fromBin
                   let rowMap = IM.fromDistinctAscList rows
                   return (SiteTable n (siteIdsFromRows rowMap) rowMap
                                     (IM.fromDistinctAscList recs))

instance Bin SiteKey where
    writeBytes (IdKey b q p k) = do putI 0; toBin b; toBin q; toBin p; toBin k
    writeBytes (PrimKey op) = do putI 1; toBin op
    writeBytes (RootKey n k) = do putI 2; toBin n; toBin k
    writeBytes BoundKey = putI 3
    writeBytes (FieldKey n) = do putI 4; toBin n
    readBytes = do i <- getI
                   case i of
                     0 -> do b <- fromBin; q <- fromBin; p <- fromBin; k <- fromBin
                             return (IdKey b q p k)
                     1 -> fmap PrimKey fromBin
                     2 -> do n <- fromBin; k <- fromBin
                             return (RootKey n k)
                     3 -> return BoundKey
                     4 -> fmap FieldKey fromBin
                     n -> internalError ("GenBXref.Bin(SiteKey).readBytes: " ++ show n)

instance Bin SiteKind where
    writeBytes k = putI (fromEnum k)
    readBytes = fmap toEnumOrErr getI

instance Bin SiteRow where
    writeBytes (SiteRow k u r) = do toBin k; toBin u; toBin r
    readBytes = do k <- fromBin; u <- fromBin; r <- fromBin
                   return (SiteRow k u r)

instance Bin RecKey where
    writeBytes (RecKey k u) = do toBin k; toBin u
    readBytes = do k <- fromBin; u <- fromBin
                   return (RecKey k u)

instance Bin ContextTable where
    writeBytes (ContextTable n _ rows ops) =
        do toBin n
           toBin (IM.toAscList rows)
           toBin ops
    readBytes = do n <- fromBin
                   rows <- fromBin
                   ops <- fromBin
                   let rowMap = IM.fromDistinctAscList rows
                   return (ContextTable n (contextIdsFromRows rowMap) rowMap ops)

instance Bin DefRecord where
    writeBytes (DefRecord p h s c cs m f par sh b a) =
        do toBin p
           toBin h
           toBin s
           toBin c
           toBin (IS.toAscList cs)
           toBin m
           toBin f
           toBin par
           toBin sh
           toBin b
           toBin a
    readBytes = do p <- fromBin
                   h <- fromBin
                   s <- fromBin
                   c <- fromBin
                   cs <- fromBin
                   m <- fromBin
                   f <- fromBin
                   par <- fromBin
                   sh <- fromBin
                   b <- fromBin
                   a <- fromBin
                   return (DefRecord p h s c (IS.fromDistinctAscList cs) m f par sh b a)

instance Bin PassTag where
    writeBytes t = putI (fromEnum t)
    readBytes = fmap toEnumOrErr getI

instance Bin MetaEvent where
    writeBytes (Renamed t a b) = do putI 0; toBin t; toBin a; toBin b
    writeBytes (Merged t as b) = do putI 1; toBin t; toBin as; toBin b
    writeBytes (Inlined t a bs) = do putI 2; toBin t; toBin a; toBin bs
    writeBytes (Created t a bs) = do putI 3; toBin t; toBin a; toBin bs
    writeBytes (Dropped t a) = do putI 4; toBin t; toBin a
    readBytes = do i <- getI
                   case i of
                     0 -> do t <- fromBin; a <- fromBin; b <- fromBin
                             return (Renamed t a b)
                     1 -> do t <- fromBin; as <- fromBin; b <- fromBin
                             return (Merged t as b)
                     2 -> do t <- fromBin; a <- fromBin; bs <- fromBin
                             return (Inlined t a bs)
                     3 -> do t <- fromBin; a <- fromBin; bs <- fromBin
                             return (Created t a bs)
                     4 -> do t <- fromBin; a <- fromBin
                             return (Dropped t a)
                     n -> internalError ("GenBXref.Bin(MetaEvent).readBytes: " ++ show n)

-- ----------
-- The identifier table
--
-- The records are the bulk of the file.  A history holds the position
-- of every event between the evaluator's defs and the identifier, and
-- a def inlined into many identifiers repeats its events in each of
-- their histories (the forward-belt module's 3737 histories hold 5.6
-- million positions of 46448 distinct events).  The events a pass
-- emits for one lineage are consecutive, so a history is a few runs
-- of consecutive positions, and the histories of the identifiers a
-- def reaches share their oldest runs.  The file writes bx_idents as
-- a table that stores each shared part once:
--
--   chains   one node per distinct run: the node of the run before it
--            in the same history (0 for the oldest), the gap from that
--            node's last position and the number of positions; a
--            record holds the node of its newest run, and the oldest
--            runs two histories share are one node;
--   origins  the distinct origin lists, each once, as stored;
--   records  per identifier its spelling, its id when the two differ,
--            its kinds, the index of its origin list and its node.
--
-- Nodes and lists are numbered in first-occurrence order of one walk
-- over the records in declaration order, each history oldest first,
-- so a parent precedes its children and the numbering, like the
-- bytes, is a function of the records alone; the walk's map keys are
-- Ints, Strings and PassTags, no FString, and the walk runs inside the
-- writer, under -bxref, after the Verilog is on disk.  Counts and
-- indices are LEB128 varints (putVarint), a node names its parent by
-- the distance back (1 for the node written before it, which it is for
-- nearly all), and the def and root pointers of an origin list are the
-- zigzag-coded differences from the pointer before them.  The reader
-- builds the nodes and the origin lists once; a record's history is
-- expanded from its chain when the record is demanded, and the records
-- of one origin list share the one decoded list.

-- a run of consecutive positions: the node of the run before it in
-- the history (0 for none), the gap from that node's last position
-- (from -1 for none) to the run's first, and the number of positions
data ChainNode = ChainNode !Int !Int !Int

-- an identifier: its spelling, its id when the two differ, its kinds,
-- its origin list and its chain node
data IdentEntry = IdentEntry !String !(Maybe String) ![IdentKind] !Int !Int

-- the nodes, 1 and up, each after its parent (0 is the empty
-- history); the distinct origin lists, 0 and up; the records
data IdentTable = IdentTable [ChainNode] [[IdentOrigin]] [IdentEntry]

-- The state of the encoder's walk: the nodes by (parent, gap,
-- length) and the lists by content, each with the next index and the
-- entries so far, newest first.  A key is in a map once by
-- construction (it is looked up before it is inserted).
data EncState = EncState {
        es_nodes :: !(M.Map (Int, Int, Int) Int),
        es_nodeCount :: !Int,
        es_nodesRev :: [ChainNode],
        es_lists :: !(M.Map [IdentOrigin] Int),
        es_listCount :: !Int,
        es_listsRev :: [[IdentOrigin]],
        es_entriesRev :: [IdentEntry]
    }

encodeIdents :: [IdentRecord] -> IdentTable
encodeIdents rs =
    IdentTable (reverse (es_nodesRev st)) (reverse (es_listsRev st))
               (reverse (es_entriesRev st))
  where
    st = L.foldl' record (EncState M.empty 0 [] M.empty 0 [] []) rs
    record s r =
        let (o, s1) = originList (ir_origins r) s
            (c, s2) = chain (ir_ident r) (ir_history r) s1
            i = if ir_id r == ir_ident r then Nothing else Just (ir_id r)
            e = IdentEntry (ir_ident r) i (ir_kinds r) o c
        in  s2 { es_entriesRev = e : es_entriesRev s2 }
    originList os s =
        case M.lookup os (es_lists s) of
          Just o -> (o, s)
          Nothing -> let o = es_listCount s
                     in  (o, s { es_lists = M.insert os o (es_lists s),
                                 es_listCount = o + 1,
                                 es_listsRev = os : es_listsRev s })
    -- the runs of a history oldest first, each under the node of the
    -- run before it; the node of the last run is the record's
    chain ident h s0 = go 0 (-1) (historyRuns ident h) s0
      where
        go parent _ [] s = (parent, s)
        go parent lastPos ((first, len) : more) s =
            let key = (parent, first - lastPos - 1, len)
                end = first + len - 1
            in  case M.lookup key (es_nodes s) of
                  Just c -> go c end more s
                  Nothing ->
                      let c = es_nodeCount s + 1
                          (p, g, l) = key
                          node = ChainNode p g l
                          s' = node `seq`
                               s { es_nodes = M.insert key c (es_nodes s),
                                   es_nodeCount = c,
                                   es_nodesRev = node : es_nodesRev s }
                      in  go c end more s'

-- the maximal runs of consecutive positions of a history, as (first,
-- length); resolveIds builds a history strictly ascending, so any
-- other order is a broken record
historyRuns :: String -> [Int] -> [(Int, Int)]
historyRuns _ [] = []
historyRuns ident (p : ps) = go p 1 ps
  where
    go first len (q : qs)
        | q == first + len = go first (len + 1) qs
        | q > first + len = (first, len) : go q 1 qs
        | otherwise = internalError ("GenBXref.historyRuns: the history of " ++
                                     ident ++ " is not ascending at " ++ show q)
    go first len [] = [(first, len)]

-- The records of a table, each built when it is demanded: its history
-- from the node array (the chain from its node to the root, then the
-- positions of each run, oldest first), its origins the one decoded
-- list of its index.  Each node's parent precedes it (checked when
-- the nodes are read), so a chain ends.
decodeIdents :: IdentTable -> [IdentRecord]
decodeIdents (IdentTable cs os rs) = map entry rs
  where
    nodeCount = length cs
    nodes = listArray (1, nodeCount) cs :: Array Int ChainNode
    listCount = length os
    lists = listArray (0, listCount - 1) os :: Array Int [IdentOrigin]
    entry (IdentEntry s mi ks o c) =
        IdentRecord { ir_ident = s, ir_id = fromMaybe s mi, ir_kinds = ks,
                      ir_origins = originList o, ir_history = history c }
    originList o
        | o >= 0 && o < listCount = lists ! o
        | otherwise = internalError ("GenBXref.decodeIdents: origin list " ++
                                     show o ++ " of " ++ show listCount)
    history c
        | c == 0 = []
        | c > 0 && c <= nodeCount = expand (-1) (chain c [])
        | otherwise = internalError ("GenBXref.decodeIdents: chain node " ++
                                     show c ++ " of " ++ show nodeCount)
    chain 0 acc = acc
    chain c acc = let node@(ChainNode p _ _) = nodes ! c in chain p (node : acc)
    expand _ [] = []
    expand lastPos (ChainNode _ g l : more) =
        let first = lastPos + g + 1
            end = first + l - 1
        in  [first .. end] ++ expand end more

-- The records through the table's encoding and back (-metadata-check):
-- the records that differ from what is read, one line each with both
-- renderings, and the counts when they differ.  The table holds
-- strings and numbers, no FString, so the decode interns nothing.
checkIdentTable :: ModuleMeta -> [IdentRecord] -> [String]
checkIdentTable mm rs =
    [ renderIdent mm r ++ "\n  read back as " ++ renderIdent mm r'
    | (r, r') <- zip rs rs', not (same r r') ] ++
    [ show (length rs) ++ " records written, " ++ show (length rs') ++ " read"
    | length rs /= length rs' ]
  where
    rs' = decodeIdents (decodeStrict (B.pack (encode (encodeIdents rs))))
    same r r' = ir_ident r == ir_ident r' && ir_id r == ir_id r' &&
                ir_kinds r == ir_kinds r' && ir_origins r == ir_origins r' &&
                ir_history r == ir_history r'

instance Bin IdentTable where
    writeBytes (IdentTable cs os rs) =
        do putVarint (length cs)
           sequence_ (zipWith putChainNode [1 ..] cs)
           putVarint (length os)
           mapM_ putOriginList os
           putVarint (length rs)
           mapM_ putIdentEntry rs
    readBytes = do n <- getVarint
                   cs <- getChainNodes n
                   m <- getVarint
                   os <- getMany m getOriginList
                   k <- getVarint
                   rs <- getMany k getIdentEntry
                   return (IdentTable cs os rs)

-- a node: the distance back to its parent (1 and up; the parent of
-- the first node is 0), the gap, the length less one
putChainNode :: Int -> ChainNode -> Out ()
putChainNode c (ChainNode p g l) =
    do putVarint (c - p)
       putVarint g
       putVarint (l - 1)

-- the nodes 1 to n, each checked to name a node before it
getChainNodes :: Int -> In [ChainNode]
getChainNodes n = go 1 []
  where
    go c acc | c > n = return (reverse acc)
             | otherwise =
                 do d <- getVarint
                    g <- getVarint
                    l <- getVarint
                    let p = c - d
                    if d < 1 || p < 0
                      then internalError ("GenBXref.getChainNodes: node " ++ show c ++
                                          " under " ++ show p)
                      else go (c + 1) (ChainNode p g (l + 1) : acc)

-- an origin list: its length, then each origin as a tag byte and its
-- payload; a def or root pointer is the zigzag-coded difference from
-- the previous def or root pointer of the list (from 0 before the
-- first)
putOriginList :: [IdentOrigin] -> Out ()
putOriginList os = do putVarint (length os)
                      go 0 os
  where
    go _ [] = return ()
    go prev (o : more) =
        case o of
          ODef p -> do putI 0; putVarint (zigzag (p - prev)); go p more
          ORoot s -> do putI 1; putVarint (zigzag (s - prev)); go s more
          ORule n -> do putI 2; toBin n; go prev more
          OPass t n -> do putI 3; toBin t; toBin n; go prev more
          OUnresolved n -> do putI 4; toBin n; go prev more

getOriginList :: In [IdentOrigin]
getOriginList = do n <- getVarint
                   go n 0 []
  where
    go k prev acc
        | k <= 0 = return (reverse acc)
        | otherwise =
            do i <- getI
               case i of
                 0 -> do d <- getVarint
                         let p = prev + unzigzag d
                         go (k - 1) p (ODef p : acc)
                 1 -> do d <- getVarint
                         let s = prev + unzigzag d
                         go (k - 1) s (ORoot s : acc)
                 2 -> do n <- fromBin
                         go (k - 1) prev (ORule n : acc)
                 3 -> do t <- fromBin
                         n <- fromBin
                         go (k - 1) prev (OPass t n : acc)
                 4 -> do n <- fromBin
                         go (k - 1) prev (OUnresolved n : acc)
                 m -> internalError ("GenBXref.getOriginList: " ++ show m)

-- an entry: the spelling, a byte 0 (the id is the spelling) or 1 and
-- the id, the kinds counted, the origin list, the node
putIdentEntry :: IdentEntry -> Out ()
putIdentEntry (IdentEntry s mi ks o c) =
    do toBin s
       case mi of
         Nothing -> putI 0
         Just i -> do putI 1; toBin i
       putVarint (length ks)
       mapM_ toBin ks
       putVarint o
       putVarint c

getIdentEntry :: In IdentEntry
getIdentEntry =
    do s <- fromBin
       f <- getI
       mi <- case f of
               0 -> return Nothing
               1 -> fmap Just fromBin
               m -> internalError ("GenBXref.getIdentEntry: " ++ show m)
       n <- getVarint
       ks <- getMany n fromBin
       o <- getVarint
       c <- getVarint
       return (IdentEntry s mi ks o c)

-- n values in order, accumulated and reversed so that a long list
-- does not grow the stack (as BinData's list reader does)
getMany :: Int -> In a -> In [a]
getMany n0 r = go n0 []
  where go n acc | n <= 0 = return (reverse acc)
                 | otherwise = do x <- r
                                  go (n - 1) (x : acc)

-- An unsigned LEB128 varint: seven bits per byte, low bits first, the
-- high bit set on every byte but the last; one byte up to 127, two up
-- to 16383.  The counts, indices and differences of the table are
-- non-negative by construction; a negative one is a broken table.
putVarint :: Int -> Out ()
putVarint n
    | n < 0 = internalError ("GenBXref.putVarint: " ++ show n)
    | otherwise = putBs (bytes n)
  where
    bytes v | v < 128 = [fromIntegral v]
            | otherwise = (fromIntegral (v .&. 127) .|. 128) : bytes (v `shiftR` 7)

getVarint :: In Int
getVarint = go 0 0
  where
    go acc sh =
        do b <- getB
           let acc' = acc .|. (fromIntegral (b .&. 127) `shiftL` sh)
           if b < 128
             then return acc'
             else if sh >= 56
                  then internalError "GenBXref.getVarint: more than nine bytes"
                  else go acc' (sh + 7)

-- a signed difference as a non-negative varint: 0, -1, 1, -2, .. as
-- 0, 1, 2, 3, ..
zigzag :: Int -> Int
zigzag d = if d >= 0 then 2 * d else -2 * d - 1

unzigzag :: Int -> Int
unzigzag z = if even z then z `div` 2 else negate (z `div` 2) - 1

instance Bin IdentKind where
    writeBytes k = putI (fromEnum k)
    readBytes = fmap toEnumOrErr getI

-- an enumeration's index back to the value, within its bounds
toEnumOrErr :: (Enum a, Bounded a, Show a) => Int -> a
toEnumOrErr i =
    let r = toEnum i
        lo = minBound `asTypeOf` r
        hi = maxBound `asTypeOf` r
    in  if i >= fromEnum lo && i <= fromEnum hi
        then r
        else internalError ("GenBXref.toEnumOrErr: " ++ show i ++ " outside " ++
                            show lo ++ ".." ++ show hi)
