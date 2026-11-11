{-# LANGUAGE BangPatterns #-}
-- The capture field of a .ba file: provenance facts about the elaborated
-- module, recorded by every compile and read back by bluetcl and bluehs.
--
-- The record holds only Ints, Bools and plain Strings, so writing it never
-- reaches the shared Id/Position/Type/IType tables or the string table, and
-- reading it interns nothing.  It must not import FStringCompat, Id,
-- Position, IType or anything that defines them; CaptureFreeze builds it.
--
-- Keys are ordinals into the APackage written in the same .ba
-- (apkg_inputs, apkg_state_instances, apkg_local_defs, apkg_interface,
-- apkg_rules), so a record can never name the wrong definition.  Every
-- string, file and site index is 1-based; 0 means "none".
module Capture(
               Capture(..), CapFile(..), CapPackage(..), CapSites(..),
               DefRef(..), DefRec(..), NameRec(..), NameRef(..),
               InnerRec(..), Host(..), ObsRec(..), CapType(..),
               NetCap(..), NetName(..), Origin(..), NetLink(..),
               capVersion, emptyCapture,
               innerKindInner, innerKindConst, innerKindDead, innerKindUnattached,
               viaNames, stageNames, obsKindNames, boolOpNames, linkRoleNames,
               captureHeader, captureSummary, captureDump
              ) where

import Data.List(intercalate)
import qualified Data.ByteString.Lazy as B
import Eval(NFData(..), rnf2, rnf3, rnf4, rnf6)
import BinData(Bin(..), putI, getI, encodeWithIO)
import ErrorUtil(internalError)

-- Layout version; each step that changes the layout bumps it and the .ba
-- header tag (GenABin.header).
--   1: names (step 1.1)
--   2: type events (step 1.2): cap_types, dr_obs, ir_obs
--   3: declared types (step 1.4): the array-elem, array-of, dynsel-result
--      and bool-by observations
--   4: boundary links (step 1.5): nc_links
capVersion :: Int
capVersion = 4

data Capture = Capture
  { cap_version  :: !Int
  , cap_mode     :: !Int           -- perturbation bitmask; 0 = real facts
  , cap_strings  :: [String]       -- string k is entry k-1
  , cap_files    :: [CapFile]
  , cap_packages :: [CapPackage]   -- the imports, with their .bo hashes
  , cap_sites    :: CapSites
  , cap_types    :: [CapType]      -- type k is entry k-1; children point back
  , cap_defs     :: [DefRec]       -- apkg space, ascending key
  , cap_inner    :: [InnerRec]     -- names that left the module before amod_final
  , cap_stats    :: [(Int, Int)]   -- counters (stage * 100 + kind, count)
  , cap_net      :: Maybe NetCap   -- Verilog backend only
  }

data CapFile = CapFile { cf_path :: !Int, cf_stdlib :: !Bool }

data CapPackage = CapPackage { cp_name :: !Int, cp_bo_hash :: !Int }

-- column store; row k is site k
data CapSites = CapSites { cs_file :: [Int], cs_line :: [Int], cs_col :: [Int] }

data DefRef = DRDef !Int          -- apkg_local_defs
            | DRInput !Int        -- apkg_inputs
            | DRInst !Int !Int    -- apkg_state_instances; argument (-1 = the instance)
            | DRIfc !Int !Int     -- apkg_interface; 0 = the method, k = its k-th input port
            | DRRule !Int         -- apkg_rules
  deriving (Eq, Ord)

-- every name merged into a definition (its own name is in the apkg), and
-- the types observed on it
data DefRec = DefRec { dr_key :: !DefRef, dr_names :: [NameRec], dr_obs :: [ObsRec] }

data NameRec = NameRec { nr_name  :: !NameRef
                       , nr_via   :: !Int    -- how it merged (viaNames)
                       , nr_stage :: !Int    -- where (stageNames)
                       , nr_site  :: !Int }  -- the name's binder site

-- A name, spelled by the reader:
--   NId q b          qualifier and base strings of an Id
--   NHeap p (Just (q, b))  IExpand cell p named q::b: legalized, "__h<p>" added
--   NHeap p Nothing        "__h<p>" (also any Id spelled exactly so)
--   NGen c n         a generated name "__" ++ [c] ++ show n: c = 'i', 'j'
--                    (ITransform CSE "__iN"/"__jN": an unpicked number, or
--                    any Id spelled exactly so) or 'd' (AConv CSE)
data NameRef = NId !Int !Int
             | NHeap !Int !(Maybe (Int, Int))
             | NGen !Int !Int

-- A definition that left the module: inlined into its hosts (kind inner),
-- replaced by a constant, or dropped as dead.  ir_names starts with its own
-- name at that point.
data InnerRec = InnerRec { ir_kind  :: !Int
                         , ir_via   :: !Int
                         , ir_stage :: !Int
                         , ir_names :: [NameRec]
                         , ir_hosts :: [Host]
                         , ir_obs   :: [ObsRec] }

innerKindInner, innerKindConst, innerKindDead, innerKindUnattached :: Int
innerKindInner = 0
innerKindConst = 1
innerKindDead  = 2
-- a record whose definition is not in the stored package (a writer gap)
innerKindUnattached = 3

data Host = HDef !DefRef      -- a definition in the stored apkg
          | HInner !Int       -- cap_inner (or nc_inner) entry, 1-based
          | HNet !Int         -- nc_names entry, 1-based
          | HName !NameRef    -- a host that is none of these (a later name)

-- A type observation (step 1.2): IExpand saw the def's value (or the value
-- of a cell holding the same value, or of one eqPtrs merged into it) at a
-- type (obsKindNames):
--   pack-hold, pack-cancel, pack-undet, pack-merge: the bits are a pack
--     from or_type (held, cancelled against an unpack, applied at once to
--     a don't-care, or merged by an if of two held packs);
--   unpack-hold, unpack-cancel, unpack-undet, unpack-const, unpack-merge:
--     the bits are unpacked to or_type;
--   repack: the bits of a pack from or_type2, unpacked to or_type;
--   move: the value is argument or_lo (of type or_type) of a constructor
--     or tuple of type or_type2 in cell or_link;
--   win-concat: bits [or_lo, or_lo+or_width) of the def are cell or_link;
--   win-select: the def is bits [or_lo, or_lo+or_width) of cell or_link;
--   win-zeroext, win-signext: the def extends cell or_link (or_width bits);
--   array-elem: the def is element or_lo of an or_width-element array of
--     or_type in the value of cell or_link (a def's);
--   array-of: the def's value holds an or_width-element array of or_type;
--   dynsel-result: the def is a dynamic select, by an or_width-bit index, of
--     an element of type or_type;
--   bool-by: the def is a Bool, built by the operator or_lo (boolOpNames).
-- Cells are IExpand heap pointers: a def's own is the number in its
-- "__h<p>" name, and NHeap names give the cells merged into it.
data ObsRec = ObsRec { or_kind  :: !Int
                     , or_tier  :: !Int   -- 0 the def's own cell, 1 a cell eqPtrs
                                          -- merged into it, 2 a cell linked to one
                                          -- of those (materialised or copied)
                     , or_type  :: !Int   -- cap_types, 0 = unknown
                     , or_type2 :: !Int
                     , or_lo    :: !Int
                     , or_width :: !Int
                     , or_src   :: !Int   -- the cell the event named
                     , or_link  :: !Int   -- the other cell, -1 = none
                     , or_count :: !Int } -- identical events folded

-- A type, post-order: children are earlier entries (1-based).  Names are
-- strings; kinds, sorts, fields and widths are the reader's, from the
-- packages' .bo files.
data CapType = CTCon !Int !Int          -- qualifier, base
             | CTAp !Int !Int           -- function, argument
             | CTNum !Integer
             | CTStr !Int
             | CTVar !Int               -- base
             | CTForAll !Int !Int       -- binder base, body

-- Verilog backend: the Verilog identifiers and where they came from
data NetCap = NetCap { nc_names   :: [NetName]
                     , nc_inner   :: [InnerRec]   -- after amod_final
                     , nc_dropped :: [(DefRef, Int)]  -- apkg defs dropped, by stage
                     , nc_links   :: [NetLink] }  -- boundary links (step 1.5)

-- A boundary link (step 1.5): definition nl_def carries the bits of a port
-- of state instance nl_inst, in role nl_role (linkRoleNames):
--   arm n, sel n, presel n: arm n of the mux AState built for argument
--     nl_argN (split port nl_portM) of method nl_meth (copy nl_ino), its
--     select, or the select's rule part (n from 1);
--   portdef: the definition of that port, n = 0 a single input, 1 a mux,
--     2 a priority mux;
--   enable n, preenable n: the method's EN definition (n = 0) or its n-th
--     use's term (enable) or rule part (preenable);
--   rwire-data, rwire-has: an inlined RWire's (or BypassWire's) wget and
--     whas definitions;
--   creg-read n, creg-write n: an inlined CReg's port n read result, or the
--     write argument InlineCReg made for an unused port n;
--   reg-din: the D_IN of the register InlineCReg made, after its n ports.
-- Ports are from 1; nl_portM = 0 is an argument with one port; nl_inst is
-- an apkg_state_instances ordinal (-1 none); nl_meth is a string (0: the
-- role names the port); nl_ino = -1 is a method with one copy.
data NetLink = NetLink { nl_def    :: !Int   -- an nc_names entry (from 1), or minus an
                                             -- nc_inner entry (a def that left)
                       , nl_inst   :: !Int
                       , nl_meth   :: !Int
                       , nl_argN   :: !Int
                       , nl_portM  :: !Int
                       , nl_role   :: !Int
                       , nl_n      :: !Int
                       , nl_ino    :: !Int }

data NetName = NetName { nn_vname :: !Int, nn_from :: [Origin] }

data Origin = OApkg !DefRef !Int     -- an apkg item, and the via code
            | OName !NameRef !Int    -- a name merged after amod_final
            | OInout !Int            -- the net's name before renameInoutPorts
            | OSvStd !Int            -- the name before renameSVStdIdents
            | ODollar !Int           -- the name before removeDollarsFromVerilog

emptyCapture :: Int -> Capture
emptyCapture mode = Capture capVersion mode [] [] [] (CapSites [] [] []) [] [] [] [] Nothing

-- -----
-- Enumerations (the reader's spelling of via and stage codes)

viaNames :: [(Int, String)]
viaNames =
  [ (0, "self"), (1, "eqptrs1"), (2, "eqptrs2"), (3, "pdef-atom")
  , (4, "inline-alias"), (5, "inline"), (6, "atom")
  , (7, "cse"), (8, "cse-name"), (9, "drop")
  , (10, "alias"), (11, "rename"), (12, "param")
  , (13, "join"), (14, "created"), (15, "aopt-copy") ]

obsKindNames :: [(Int, String)]
obsKindNames =
  [ (1, "pack-hold"), (2, "pack-cancel"), (3, "pack-undet"), (4, "unpack-hold")
  , (5, "unpack-cancel"), (6, "unpack-undet"), (7, "unpack-const"), (8, "repack")
  , (9, "pack-merge"), (10, "unpack-merge"), (11, "move")
  , (12, "win-concat"), (13, "win-select"), (14, "win-zeroext"), (15, "win-signext")
  , (17, "array-elem"), (18, "array-of"), (19, "dynsel-result"), (20, "bool-by") ]

-- NetLink roles (nl_role)
linkRoleNames :: [(Int, String)]
linkRoleNames =
  [ (0, "arm"), (1, "sel"), (2, "presel"), (3, "portdef"), (4, "enable")
  , (5, "rwire-data"), (6, "rwire-has"), (7, "creg-read"), (8, "creg-write")
  , (9, "reg-din"), (10, "preenable") ]

-- bool-by's operators (or_lo)
boolOpNames :: [(Int, String)]
boolOpNames =
  [ (1, "eq"), (2, "ult"), (3, "ule"), (4, "slt"), (5, "sle"), (6, "bnot")
  , (7, "band"), (8, "bor"), (9, "is") ]

stageNames :: [(Int, String)]
stageNames =
  [ (1, "iexpand"), (2, "inlinefmt"), (3, "inline"), (4, "transform-i")
  , (5, "transform-i-inline"), (6, "lift"), (7, "transform-j")
  , (8, "transform-j-inline"), (9, "iparams"), (10, "idroprules")
  , (11, "aconv")
  , (20, "astate"), (21, "inlinewires"), (22, "inlinecreg"), (23, "renameio")
  , (24, "adropdefs"), (25, "aopt"), (26, "synthesize"), (27, "veriquirks")
  , (28, "finalcleanup"), (29, "verilog") ]

-- -----
-- NFData

instance NFData Capture where
  rnf (Capture v m ss fs ps st ts ds is xs n) =
    v `seq` m `seq` rnf ss `seq` rnf fs `seq` rnf ps `seq` rnf st `seq` rnf ts `seq`
    rnf ds `seq` rnf is `seq` rnf xs `seq` rnf n
instance NFData CapFile where rnf (CapFile p l) = rnf2 p l
instance NFData CapPackage where rnf (CapPackage n h) = rnf2 n h
instance NFData CapSites where rnf (CapSites f l c) = rnf3 f l c
instance NFData DefRef where rnf r = r `seq` ()
instance NFData DefRec where rnf (DefRec k ns os) = rnf3 k ns os
instance NFData ObsRec where rnf o = o `seq` ()
instance NFData CapType where rnf t = t `seq` ()
instance NFData NameRec where rnf (NameRec n v s t) = rnf4 n v s t
instance NFData NameRef where
  rnf (NId q b) = rnf2 q b
  rnf (NHeap p m) = rnf2 p m
  rnf (NGen c n) = rnf2 c n
instance NFData InnerRec where rnf (InnerRec k v s ns hs os) = rnf6 k v s ns hs os
instance NFData Host where
  rnf (HDef r) = rnf r
  rnf (HInner k) = rnf k
  rnf (HNet k) = rnf k
  rnf (HName n) = rnf n
instance NFData NetCap where rnf (NetCap ns is ds ls) = rnf4 ns is ds ls
instance NFData NetLink where rnf l = l `seq` ()
instance NFData NetName where rnf (NetName v os) = rnf2 v os
instance NFData Origin where
  rnf (OApkg r v) = rnf2 r v
  rnf (OName n v) = rnf2 n v
  rnf (OInout s) = rnf s
  rnf (OSvStd s) = rnf s
  rnf (ODollar s) = rnf s

-- -----
-- Bin

instance Bin Capture where
  writeBytes (Capture v m ss fs ps st ts ds is xs n) = do
    toBin v; toBin m; toBin ss; toBin fs; toBin ps; toBin st; toBin ts
    toBin ds; toBin is; toBin xs; toBin n
  readBytes = do
    v <- fromBin
    if v /= capVersion
      then internalError ("Capture: layout version " ++ show v ++
                          ", this reader knows " ++ show capVersion)
      else do m <- fromBin; ss <- fromBin; fs <- fromBin; ps <- fromBin
              st <- fromBin; ts <- fromBin; ds <- fromBin; is <- fromBin
              xs <- fromBin; n <- fromBin
              return (Capture v m ss fs ps st ts ds is xs n)

instance Bin CapFile where
  writeBytes (CapFile p l) = toBin p >> toBin l
  readBytes = CapFile <$> fromBin <*> fromBin

instance Bin CapPackage where
  writeBytes (CapPackage n h) = toBin n >> toBin h
  readBytes = CapPackage <$> fromBin <*> fromBin

instance Bin CapSites where
  writeBytes (CapSites f l c) = toBin f >> toBin l >> toBin c
  readBytes = CapSites <$> fromBin <*> fromBin <*> fromBin

instance Bin DefRef where
  writeBytes (DRDef k)    = putI 0 >> toBin k
  writeBytes (DRInput k)  = putI 1 >> toBin k
  writeBytes (DRInst k a) = putI 2 >> toBin k >> toBin a
  writeBytes (DRIfc k a)  = putI 3 >> toBin k >> toBin a
  writeBytes (DRRule k)   = putI 4 >> toBin k
  readBytes = do
    t <- getI
    k <- fromBin
    case t of
      0 -> return (DRDef k)
      1 -> return (DRInput k)
      2 -> DRInst k <$> fromBin
      3 -> DRIfc k <$> fromBin
      4 -> return (DRRule k)
      _ -> internalError ("Capture.DefRef: tag " ++ show t)

instance Bin DefRec where
  writeBytes (DefRec k ns os) = toBin k >> toBin ns >> toBin os
  readBytes = DefRec <$> fromBin <*> fromBin <*> fromBin

instance Bin ObsRec where
  writeBytes (ObsRec k t ty ty2 lo w src l n) = do
    toBin k; toBin t; toBin ty; toBin ty2; toBin lo; toBin w; toBin src; toBin l; toBin n
  readBytes = ObsRec <$> fromBin <*> fromBin <*> fromBin <*> fromBin <*> fromBin
                     <*> fromBin <*> fromBin <*> fromBin <*> fromBin

instance Bin CapType where
  writeBytes (CTCon q b) = putI 0 >> toBin q >> toBin b
  writeBytes (CTAp f a) = putI 1 >> toBin f >> toBin a
  writeBytes (CTNum n) = putI 2 >> toBin n
  writeBytes (CTStr s) = putI 3 >> toBin s
  writeBytes (CTVar v) = putI 4 >> toBin v
  writeBytes (CTForAll v t) = putI 5 >> toBin v >> toBin t
  readBytes = do
    t <- getI
    case t of
      0 -> CTCon <$> fromBin <*> fromBin
      1 -> CTAp <$> fromBin <*> fromBin
      2 -> CTNum <$> fromBin
      3 -> CTStr <$> fromBin
      4 -> CTVar <$> fromBin
      5 -> CTForAll <$> fromBin <*> fromBin
      _ -> internalError ("Capture.CapType: tag " ++ show t)

instance Bin NameRec where
  writeBytes (NameRec n v s t) = toBin n >> toBin v >> toBin s >> toBin t
  readBytes = NameRec <$> fromBin <*> fromBin <*> fromBin <*> fromBin

instance Bin NameRef where
  writeBytes (NId q b) = putI 0 >> toBin q >> toBin b
  writeBytes (NHeap p m) = putI 1 >> toBin p >> toBin m
  writeBytes (NGen c n) = putI 2 >> toBin c >> toBin n
  readBytes = do
    t <- getI
    case t of
      0 -> NId <$> fromBin <*> fromBin
      1 -> NHeap <$> fromBin <*> fromBin
      2 -> NGen <$> fromBin <*> fromBin
      _ -> internalError ("Capture.NameRef: tag " ++ show t)

instance Bin InnerRec where
  writeBytes (InnerRec k v s ns hs os) =
    toBin k >> toBin v >> toBin s >> toBin ns >> toBin hs >> toBin os
  readBytes = InnerRec <$> fromBin <*> fromBin <*> fromBin <*> fromBin <*> fromBin
                       <*> fromBin

instance Bin Host where
  writeBytes (HDef r) = putI 0 >> toBin r
  writeBytes (HInner k) = putI 1 >> toBin k
  writeBytes (HNet k) = putI 2 >> toBin k
  writeBytes (HName n) = putI 3 >> toBin n
  readBytes = do
    t <- getI
    case t of
      0 -> HDef <$> fromBin
      1 -> HInner <$> fromBin
      2 -> HNet <$> fromBin
      3 -> HName <$> fromBin
      _ -> internalError ("Capture.Host: tag " ++ show t)

instance Bin NetCap where
  writeBytes (NetCap ns is ds ls) = toBin ns >> toBin is >> toBin ds >> toBin ls
  readBytes = NetCap <$> fromBin <*> fromBin <*> fromBin <*> fromBin

instance Bin NetLink where
  writeBytes (NetLink d i m a p r n c) = do
    toBin d; toBin i; toBin m; toBin a; toBin p; toBin r; toBin n; toBin c
  readBytes = NetLink <$> fromBin <*> fromBin <*> fromBin <*> fromBin <*> fromBin
                      <*> fromBin <*> fromBin <*> fromBin

instance Bin NetName where
  writeBytes (NetName v os) = toBin v >> toBin os
  readBytes = NetName <$> fromBin <*> fromBin

instance Bin Origin where
  writeBytes (OApkg r v) = putI 0 >> toBin r >> toBin v
  writeBytes (OName n v) = putI 1 >> toBin n >> toBin v
  writeBytes (OInout s) = putI 2 >> toBin s
  writeBytes (OSvStd s) = putI 3 >> toBin s
  writeBytes (ODollar s) = putI 4 >> toBin s
  readBytes = do
    t <- getI
    case t of
      0 -> OApkg <$> fromBin <*> fromBin
      1 -> OName <$> fromBin <*> fromBin
      2 -> OInout <$> fromBin
      3 -> OSvStd <$> fromBin
      4 -> ODollar <$> fromBin
      _ -> internalError ("Capture.Origin: tag " ++ show t)

-- -----
-- Printing (dumpba)

captureHeader :: Capture -> String
captureHeader c =
  "version " ++ show (cap_version c) ++ ", mode " ++ show (cap_mode c) ++
  ", " ++ show (length (cap_defs c)) ++ " defs, " ++
  show (length (cap_inner c)) ++ " inner" ++
  maybe "" (\n -> ", " ++ show (length (nc_names n)) ++ " nets") (cap_net c)

-- One line per section: entries and encoded bytes.
captureSummary :: Capture -> IO [String]
captureSummary c = do
  let net = cap_net c
      obs = concatMap dr_obs (cap_defs c) ++ concatMap ir_obs (cap_inner c)
  sections <- sequence
    [ row "strings" (length (cap_strings c)) (cap_strings c)
    , row "files" (length (cap_files c)) (cap_files c)
    , row "packages" (length (cap_packages c)) (cap_packages c)
    , row "sites" (length (cs_line (cap_sites c))) (cap_sites c)
    , row "types" (length (cap_types c)) (cap_types c)
    , row "defs" (length (cap_defs c)) (cap_defs c)
    , row "obs" (length obs) obs
    , row "inner" (length (cap_inner c)) (cap_inner c)
    , row "stats" (length (cap_stats c)) (cap_stats c)
    , row "net.names" (maybe 0 (length . nc_names) net) (fmap nc_names net)
    , row "net.inner" (maybe 0 (length . nc_inner) net) (fmap nc_inner net)
    , row "net.dropped" (maybe 0 (length . nc_dropped) net) (fmap nc_dropped net)
    , row "net.links" (maybe 0 (length . nc_links) net) (fmap nc_links net)
    ]
  total <- B.length <$> encodeWithIO (\_ f -> f) c
  return (("capture: version " ++ show (cap_version c) ++ ", mode " ++
           show (cap_mode c) ++ ", " ++ show total ++ " bytes") : sections)
 where
  row :: Bin a => String -> Int -> a -> IO String
  row nm n x = do
    b <- B.length <$> encodeWithIO (\_ f -> f) x
    return ("  " ++ nm ++ ": " ++ show n ++ " entries, " ++ show b ++ " bytes")

-- Every record, with names spelled from the string table.
captureDump :: Capture -> [String]
captureDump c =
  [ "capture version " ++ show (cap_version c) ++ " mode " ++ show (cap_mode c) ] ++
  [ "package " ++ str n ++ " " ++ str h | CapPackage n h <- cap_packages c ] ++
  [ "type " ++ show i ++ " " ++ ctype t | (i, t) <- zip [1 :: Int ..] (cap_types c) ] ++
  concat [ ("def " ++ ref k ++ ": " ++ names ns) : map (("  obs " ++) . obs) os
         | DefRec k ns os <- cap_defs c ] ++
  concat [ ("inner " ++ show i ++ " " ++ inner r) : map (("  obs " ++) . obs) (ir_obs r)
         | (i, r) <- zip [1 :: Int ..] (cap_inner c) ] ++
  [ "stat " ++ show k ++ " " ++ show n | (k, n) <- cap_stats c ] ++
  case cap_net c of
    Nothing -> []
    Just nc ->
      [ "net " ++ show i ++ " " ++ str v ++ " <- " ++
        intercalate ", " (map origin os)
      | (i, NetName v os) <- zip [1 :: Int ..] (nc_names nc) ] ++
      [ "net.inner " ++ show i ++ " " ++ inner r | (i, r) <- zip [1 :: Int ..] (nc_inner nc) ] ++
      [ "net.dropped " ++ ref k ++ " " ++ stage s | (k, s) <- nc_dropped nc ] ++
      [ "net.link " ++ (if d < 0 then "inner " ++ show (negate d) else "net " ++ show d) ++
        " " ++ lkp linkRoleNames r ++ " " ++ show n ++
        " inst#" ++ show i ++ (if m == 0 then "" else " " ++ str m) ++
        (if c >= 0 then " copy " ++ show c else "") ++
        (if a > 0 then " arg " ++ show a else "") ++ (if p > 0 then " port " ++ show p else "")
      | NetLink d i m a p r n c <- nc_links nc ]
 where
  strs = cap_strings c
  nstrs = length strs
  str 0 = "\"\""
  str k | k > 0 && k <= nstrs = show (strs !! (k - 1))
        | otherwise = "<bad string " ++ show k ++ ">"
  site 0 = "-"
  site k = let CapSites fs ls cs = cap_sites c
               f = fs !! (k - 1)
               path = case drop (f - 1) (cap_files c) of
                        (CapFile p _ : _) | f > 0 -> str p
                        _ -> "-"
           in  path ++ ":" ++ show (ls !! (k - 1)) ++ ":" ++ show (cs !! (k - 1))
  lkp tbl k = maybe (show k) id (lookup k tbl)
  via = lkp viaNames
  stage = lkp stageNames
  ref (DRDef k) = "def#" ++ show k
  ref (DRInput k) = "input#" ++ show k
  ref (DRInst k a) = "inst#" ++ show k ++ (if a < 0 then "" else "." ++ show a)
  ref (DRIfc k a) = "ifc#" ++ show k ++ (if a == 0 then "" else "." ++ show a)
  ref (DRRule k) = "rule#" ++ show k
  nref (NId q b) = (if q == 0 then "" else str q ++ "::") ++ str b
  nref (NHeap p Nothing) = "heap " ++ show p
  nref (NHeap p (Just (q, b))) = "heap " ++ show p ++ " " ++ nref (NId q b)
  nref (NGen t n) = "gen " ++ [toEnum t] ++ show n
  names ns = intercalate "; " [ nref n ++ " (" ++ via v ++ "@" ++ stage s ++ " " ++ site t ++ ")"
                              | NameRec n v s t <- ns ]
  host (HDef r) = ref r
  host (HInner k) = "inner " ++ show k
  host (HNet k) = "net " ++ show k
  host (HName n) = nref n
  kind k | k == innerKindInner = "inner"
         | k == innerKindConst = "const"
         | k == innerKindDead = "dead"
         | k == innerKindUnattached = "unattached"
         | otherwise = show k
  ctype (CTCon q b) = "con " ++ (if q == 0 then "" else str q ++ "::") ++ str b
  ctype (CTAp f a) = "ap " ++ show f ++ " " ++ show a
  ctype (CTNum n) = "num " ++ show n
  ctype (CTStr t) = "str " ++ str t
  ctype (CTVar v) = "var " ++ str v
  ctype (CTForAll v t) = "forall " ++ str v ++ " " ++ show t
  tyref 0 = "?"
  tyref k = "t" ++ show k
  obs (ObsRec k t ty ty2 lo w src l n) =
    lkp obsKindNames k ++ " tier " ++ show t ++ " " ++ tyref ty ++
    (if ty2 /= 0 then " from " ++ tyref ty2 else "") ++
    (if k == 20 then " op " ++ lkp boolOpNames lo
     else if lo >= 0 then " lo " ++ show lo else "") ++
    (if w >= 0 then " width " ++ show w else "") ++
    " cell " ++ show src ++ (if l >= 0 then " link " ++ show l else "") ++
    (if n /= 1 then " x" ++ show n else "")
  inner (InnerRec k v s ns hs _) =
    kind k ++ " " ++ via v ++ "@" ++ stage s ++ ": " ++ names ns ++
    (if null hs then "" else " in " ++ intercalate ", " (map host hs))
  origin (OApkg r v) = ref r ++ " (" ++ via v ++ ")"
  origin (OName n v) = nref n ++ " (" ++ via v ++ ")"
  origin (OInout s) = "inout " ++ str s
  origin (OSvStd s) = "svstd " ++ str s
  origin (ODollar s) = "dollar " ++ str s
