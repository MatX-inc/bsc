-- The live capture: provenance facts carried through the compile as the
-- last field of IModule, APackage and ASPackage (Cap CapTable), frozen
-- into the .ba by CaptureFreeze.
--
-- Forcing rule.  The field is the last one of each record, and each
-- record's rnf forces it last, so a stage's table is forced by that
-- stage's dump (which always deepseqs) right after production has forced
-- the stage's own module.  The table may therefore hold only values that
-- production has forced by then: Ints, Ids of definitions, ports and
-- instances that some stage's module held, and cell names (hc_name, which
-- IExpand deepseqs when it makes a cell).  It never builds a name
-- (mkFString, mkId..., legalizeNameInfo): an unpicked generated name is
-- kept as its number.  Nothing in the compiler reads the table.
module CaptureLive(
                   Cap(..), capOf,
                   CapTable(..), emptyCapTable,
                   NetLive(..), NetNameL(..), NetFrom(..),
                   NameRefL(..), NameFact(..), HostL(..), Gone(..),
                   ObsL(..), CapTy(..), LinkL(..), isNameFact,
                   capMerge, capMergeFacts, capGone, capGoneFacts, capCopied, capDead,
                   capRename, capStat, capNamesOf, capRecs, capFromRecs,
                   -- boundary links
                   capLinks, boundaryLinks, methLink, instLink,
                   linkArm, linkSel, linkPreSel, linkPortDef, linkEnable,
                   linkRWireData, linkRWireHas, linkCRegRead, linkCRegWrite,
                   linkRegDIn, linkPreEnable,
                   -- stages
                   stIExpand, stInlineFmt, stInline, stTransI, stTransIInline,
                   stLift, stTransJ, stTransJInline, stIParams, stIDropRules,
                   stAConv, stAState, stInlineWires, stInlineCReg, stRenameIO,
                   stADropDefs, stAOpt, stSynthesize, stVeriQuirks, stFinalCleanup,
                   stVerilog,
                   -- vias
                   viaSelf, viaEqPtrs1, viaEqPtrs2, viaPDefAtom, viaInlineAlias,
                   viaInline, viaAtom, viaCse, viaCseName, viaDrop, viaAlias,
                   viaRename, viaParam, viaJoin, viaCreated, viaCopy,
                   kindInner, kindConst, kindDead,
                   -- perturbation
                   perturbing, perturbMask,
                   ptHeader, ptNames, ptNetnames,
                   ptEvents, ptCopylinks, ptMoves, ptWindows, ptDecl, ptBoundary,
                   rotate
                  ) where

import qualified Data.Map.Strict as M
import Data.Bits((.&.), (.|.), shiftL)
import Data.List(foldl')
import Data.Maybe(fromMaybe)

import Eval(NFData(..), rnf2, rnf3, rnf5)
import PPrint(PPrint(..), empty)
import Data.Char(isDigit)
import FStringCompat(getFString)
import Id(Id, getIdQual, getIdBase)
import IType(IType)
import IOUtil(capturePerturbEnv)

-- -----
-- The carrier

-- Capture-only payload: always equal, prints nothing, never serialized by
-- the record that holds it.
newtype Cap a = Cap a

capOf :: Cap a -> a
capOf (Cap a) = a

instance Eq (Cap a) where
  _ == _ = True
instance Ord (Cap a) where
  compare _ _ = EQ
instance Show (Cap a) where
  show _ = ""
instance PPrint (Cap a) where
  pPrint _ _ _ = empty
instance NFData a => NFData (Cap a) where
  rnf (Cap a) = rnf a

-- -----
-- The table

-- A name: an IExpand cell (its pointer and its raw name, before
-- legalization), the Id of a definition some stage held, or a generated
-- name never spelled by the compile (the CSE prefix letter and number).
data NameRefL = HeapName !Int (Maybe Id)
              | DefName Id
              | CseNo !Char !Int

-- A fact on a record: a name merged into it, a type observation (ObsFact,
-- from step 1.2) or a boundary link (LinkFact, from step 1.5), which the
-- same operations carry through every merge.
data NameFact = NameFact { nf_ref :: !NameRefL, nf_via :: !Int, nf_stage :: !Int }
              | ObsFact !ObsL
              | LinkFact !LinkL

isNameFact :: NameFact -> Bool
isNameFact (NameFact {}) = True
isNameFact _ = False

-- A type observation (CaptureEvents' kinds): made at IExpand's rebuild from
-- the evaluator's event log, on the def whose cell the event names, or read
-- off a def's own value (windows, declared types).  The types are ones the
-- evaluator had evaluated, or the module's dump had, so fully built (an
-- interned IType's children are evaluated at construction).
data ObsL = ObsL { ol_kind  :: !Int
                 , ol_tier  :: !Int     -- 0 the def's own cell, 1 a cell eqPtrs merged
                                        -- into it, 2 a cell linked to one of those
                 , ol_type  :: !CapTy   -- the observed type
                 , ol_type2 :: !CapTy   -- a repack's pack type; a move's parent type
                 , ol_lo    :: !Int     -- a window's offset; a move's argument index;
                                        -- an array element's index; a bool-by's operator
                 , ol_width :: !Int     -- a window's width; an array's size; a dynamic
                                        -- select's index width (-1: none)
                 , ol_src   :: !Int     -- the cell the event named (heap pointer)
                 , ol_link  :: !Int     -- the other cell: a payload, a move's parent,
                                        -- a window's cell, an array element's holder
                                        -- (-1: none)
                 , ol_count :: !Int }   -- identical events folded

data CapTy = NoTy | Ty IType

-- A boundary link (step 1.5): the definition holding it carries the bits of
-- a port of a state instance of the stored package, in a role (the link*
-- codes): an arm, select or preselect of the mux AState builds for a method
-- argument's port, that port's definition, an enable or an enable term, or
-- the definition InlineWires or InlineCReg makes for an inlined RWire's or
-- CReg's port.  The instance and method are the ones the pass read (a
-- method call's, or the instance's own name); the reader types the link
-- through the instance's port types.  A merge carries it like any fact:
-- the survivor carries the same bits.
data LinkL = LinkL { ll_role  :: !Int
                   , ll_n     :: !Int        -- arm, select or enable term (from 1;
                                             -- 0 the EN definition); portdef: 0 one
                                             -- input, 1 mux, 2 priority mux; CReg port
                   , ll_inst  :: !Id
                   , ll_meth  :: !(Maybe Id) -- Nothing: the role names the port
                   , ll_ino   :: !Int        -- copy of a multi-ported method, -1 none
                   , ll_argN  :: !Int        -- argument, from 1; 0 none
                   , ll_portM :: !Int }      -- port of a split argument, from 1;
                                             -- 0 not split

-- Where an inlined definition went: a definition, rule, method or
-- instance of that stage, by Id, or an instance argument.
data HostL = HLId Id | HLInst Id !Int

-- A name that left the module: inlined (with its hosts), replaced by a
-- constant, or dead.  g_names starts with its own name.
data Gone = Gone { g_kind :: !Int, g_via :: !Int, g_stage :: !Int,
                   g_names :: [NameFact], g_hosts :: [HostL] }

data CapTable = CapTable {
    -- the names merged into each current definition, port or instance,
    -- newest first (capNamesOf and capRecs give them in merge order), so
    -- that a merge costs the merged names, not the survivor's
    ct_recs  :: !(M.Map Id [NameFact]),
    -- newest first
    ct_gone  :: [Gone],
    -- counters, by stage * 100 + kind
    ct_stats :: !(M.Map Int Int)
  }

emptyCapTable :: CapTable
emptyCapTable = CapTable M.empty [] M.empty

-- The Verilog backend's net names (CaptureLedger): each identifier the
-- main module declares, with the package names it stands for (before
-- dollar removal) and its earlier spellings.  nl_table is the last
-- ASPackage's table.
data NetLive = NetLive { nl_names :: [NetNameL], nl_table :: CapTable }
data NetNameL = NetNameL { nnl_vname :: String, nnl_ids :: [Id], nnl_from :: [NetFrom] }
data NetFrom = NFInout String     -- the net renameInoutPorts renamed to this port
             | NFSvStd String     -- the name before renameSVStdIdents
             | NFDollar Id        -- the name before removeDollarsFromVerilog

instance NFData NetLive where
  rnf (NetLive ns t) = rnf2 ns t
instance NFData NetNameL where
  rnf (NetNameL v is fs) = rnf3 v is fs
instance NFData NetFrom where
  rnf (NFInout s) = rnf s
  rnf (NFSvStd s) = rnf s
  rnf (NFDollar i) = rnf i

instance NFData NameRefL where
  rnf (HeapName p n) = rnf2 p n
  rnf (DefName i) = rnf i
  rnf (CseNo c n) = rnf2 c n
instance NFData NameFact where
  rnf (NameFact r v s) = rnf3 r v s
  rnf (ObsFact o) = rnf o
  rnf (LinkFact l) = rnf l
-- the Ids are ones the pass that made the link had evaluated (it spelled
-- them in the names of the definitions it made), so evaluating them is
-- enough
instance NFData LinkL where
  rnf (LinkL _ _ i m _ _ _) = i `seq` maybe () (`seq` ()) m
-- the types are fully built (see ObsL), so evaluating them is enough
instance NFData ObsL where
  rnf (ObsL _ _ t1 t2 _ _ _ _ _) = rnf t1 `seq` rnf t2
instance NFData CapTy where
  rnf NoTy = ()
  rnf (Ty t) = t `seq` ()
instance NFData HostL where
  rnf (HLId i) = rnf i
  rnf (HLInst i k) = rnf2 i k
instance NFData Gone where
  rnf (Gone k v s ns hs) = rnf5 k v s ns hs
instance NFData CapTable where
  rnf (CapTable rs gs ss) = rnf3 rs gs ss

-- -----
-- Operations

capNamesOf :: Id -> CapTable -> [NameFact]
capNamesOf i t = reverse (M.findWithDefault [] i (ct_recs t))

-- Every record, its names in merge order.
capRecs :: CapTable -> [(Id, [NameFact])]
capRecs t = [ (i, reverse ns) | (i, ns) <- M.toList (ct_recs t) ]

-- A table of the given records (names in merge order).
capFromRecs :: [(Id, [NameFact])] -> CapTable
capFromRecs rs = emptyCapTable { ct_recs = M.fromList [ (i, reverse ns) | (i, ns) <- rs ] }

-- Merge each of the definitions into the survivor: its own name and every
-- name already merged into it.
capMerge :: Int -> Int -> Id -> [Id] -> CapTable -> CapTable
capMerge stage via surv vs t = foldl' one t vs
  where one t' v | v == surv = t'
        one t' v =
          let ns = NameFact (DefName v) via stage : capNamesOf v t'
              recs = M.delete v (ct_recs t')
          in  t' { ct_recs = addNames surv ns recs }

-- Add facts (names that were never definitions of this table) to a
-- definition's record.
capMergeFacts :: Id -> [NameFact] -> CapTable -> CapTable
capMergeFacts _ [] t = t
capMergeFacts surv ns t = t { ct_recs = addNames surv ns (ct_recs t) }

-- ns in merge order onto the newest-first record (perturbed: each merged
-- list lands reversed)
addNames :: Id -> [NameFact] -> M.Map Id [NameFact] -> M.Map Id [NameFact]
addNames k ns m = M.alter (Just . add . fromMaybe []) k m
  where add old | perturbing ptNames = ns ++ old
                | otherwise = foldl' (flip (:)) old ns

-- The definition was copied into the hosts and stays in the module (a
-- parameter argument iParams inlined): its names so far, as an inner
-- record of those hosts.
capCopied :: Int -> Int -> Id -> [HostL] -> CapTable -> CapTable
capCopied stage via v hosts t =
  let ns = NameFact (DefName v) via stage : capNamesOf v t
  in  t { ct_gone = Gone kindInner via stage ns hosts : ct_gone t }

-- The definition left the module.
capGone :: Int -> Int -> Int -> Id -> [HostL] -> CapTable -> CapTable
capGone kind stage via v hosts t =
  -- read now: left lazy, each record keeps the pre-delete table alive
  -- until the stage's dump
  let old = capNamesOf v t
      ns = NameFact (DefName v) via stage : old
  in  old `seq` t { ct_recs = M.delete v (ct_recs t),
                    ct_gone = Gone kind via stage ns hosts : ct_gone t }

-- A def nothing used left the module: recorded with its names, except a
-- generated def (an unqualified "__h<p>", "__i<n>" or "__j<n>") with no
-- other name, which is only counted (stage * 100 + statDeadGen), with any
-- type observations: such a name was never in any output and stands for
-- no source name.
capDead :: Int -> Id -> CapTable -> CapTable
capDead stage v t
  | not (any isNameFact (M.findWithDefault [] v (ct_recs t))) && generatedId v =
      capStat stage statDeadGen 1 (t { ct_recs = M.delete v (ct_recs t) })
  | otherwise = capGone kindDead stage viaDrop v [] t

generatedId :: Id -> Bool
generatedId i = null (getFString (getIdQual i)) && gen (getFString (getIdBase i))
  where gen ('_' : '_' : c : ds) = (c == 'h' || c == 'i' || c == 'j') &&
                                   not (null ds) && all isDigit ds
        gen _ = False

statDeadGen :: Int
statDeadGen = 50

-- Names that left the module without having been a definition here.
capGoneFacts :: Int -> Int -> Int -> [NameFact] -> [HostL] -> CapTable -> CapTable
capGoneFacts _ _ _ [] _ t = t
capGoneFacts kind stage via ns hosts t =
  t { ct_gone = Gone kind via stage ns hosts : ct_gone t }

-- The definition was renamed: its record moves to the new name, which
-- gains the old one.
capRename :: Int -> Int -> Id -> Id -> CapTable -> CapTable
capRename stage via old new t = capMerge stage via new [old] t

-- Boundary links onto the definitions that carry them.
capLinks :: [(Id, LinkL)] -> CapTable -> CapTable
capLinks ls t = foldl' (\t' (i, l) -> capMergeFacts i [LinkFact l] t') t ls

-- A link on a port of a method argument (AState).
methLink :: Int -> Int -> Id -> Id -> Maybe Integer -> Integer -> Maybe Integer -> LinkL
methLink role n o m ino argN portM =
  LinkL role n o (Just m) (maybe (-1) fromInteger ino) (fromInteger argN)
        (maybe 0 fromInteger portM)

-- A link on the port its role names (InlineWires, InlineCReg).
instLink :: Int -> Int -> Id -> LinkL
instLink role n o = LinkL role n o Nothing (-1) 0 0

-- The links of each mux (or inlined instance), in order.  Perturbed
-- (boundary): each group's links name the next group's port, an arm's
-- number moves to the next arm, and select and portdef swap roles.
boundaryLinks :: [[(Id, LinkL)]] -> [(Id, LinkL)]
boundaryLinks gs0
  | perturbing ptBoundary = concat (zipWith pert gs (rotate gs))
  | otherwise = concat gs0
  where
    gs = filter (not . null) gs0
    pert g next =
      let arms = length [ () | (_, l) <- g, ll_role l == linkArm ]
          LinkL _ _ o m ino a p = snd (head next)
          one l = let l' = l { ll_inst = o, ll_meth = m, ll_ino = ino,
                               ll_argN = a, ll_portM = p }
                  in  case ll_role l of
                        r | r == linkArm -> l' { ll_n = ll_n l `mod` max 1 arms + 1 }
                          | r == linkSel -> l' { ll_role = linkPortDef }
                          | r == linkPortDef -> l' { ll_role = linkSel }
                          | r == linkCRegRead -> l' { ll_n = (ll_n l + 1) `mod` 5 }
                          | otherwise -> l'
      in  [ (i, one l) | (i, l) <- g ]

-- Link roles (Capture.linkRoleNames spells them)
linkArm, linkSel, linkPreSel, linkPortDef, linkEnable, linkRWireData,
  linkRWireHas, linkCRegRead, linkCRegWrite, linkRegDIn, linkPreEnable :: Int
linkArm = 0
linkSel = 1
linkPreSel = 2
linkPortDef = 3
linkEnable = 4
linkRWireData = 5
linkRWireHas = 6
linkCRegRead = 7
linkCRegWrite = 8
linkRegDIn = 9
linkPreEnable = 10

capStat :: Int -> Int -> Int -> CapTable -> CapTable
capStat _ _ 0 t = t
capStat stage kind n t =
  t { ct_stats = M.insertWith (+) (stage * 100 + kind) n (ct_stats t) }

-- -----
-- Codes (Capture.stageNames and Capture.viaNames spell them)

stIExpand, stInlineFmt, stInline, stTransI, stTransIInline, stLift,
  stTransJ, stTransJInline, stIParams, stIDropRules, stAConv,
  stAState, stInlineWires, stInlineCReg, stRenameIO, stADropDefs, stAOpt,
  stSynthesize, stVeriQuirks, stFinalCleanup, stVerilog :: Int
stIExpand = 1
stInlineFmt = 2
stInline = 3
stTransI = 4
stTransIInline = 5
stLift = 6
stTransJ = 7
stTransJInline = 8
stIParams = 9
stIDropRules = 10
stAConv = 11
stAState = 20
stInlineWires = 21
stInlineCReg = 22
stRenameIO = 23
stADropDefs = 24
stAOpt = 25
stSynthesize = 26
stVeriQuirks = 27
stFinalCleanup = 28
stVerilog = 29

viaSelf, viaEqPtrs1, viaEqPtrs2, viaPDefAtom, viaInlineAlias, viaInline,
  viaAtom, viaCse, viaCseName, viaDrop, viaAlias, viaRename, viaParam,
  viaJoin, viaCreated, viaCopy :: Int
viaSelf = 0
viaEqPtrs1 = 1
viaEqPtrs2 = 2
viaPDefAtom = 3
viaInlineAlias = 4
viaInline = 5
viaAtom = 6
viaCse = 7
viaCseName = 8
viaDrop = 9
viaAlias = 10
viaRename = 11
viaParam = 12
viaJoin = 13
viaCreated = 14
viaCopy = 15

kindInner, kindConst, kindDead :: Int
kindInner = 0
kindConst = 1
kindDead = 2

-- -----
-- Perturbation (BSC_CAPTURE_PERTURB=<token,...>): each token makes the
-- facts of one kind wrong at the place they are recorded, so that a
-- compiler reader of them would change some output.  A perturbation only
-- permutes values already in hand.

-- bit numbers, in the order of the spec's token table
ptHeader, ptNames, ptNetnames, ptEvents, ptCopylinks, ptMoves, ptWindows,
  ptDecl, ptBoundary :: Int
ptHeader = 0
ptNames = 1
ptNetnames = 2
ptEvents = 3
ptCopylinks = 4
ptMoves = 5
ptWindows = 6
ptDecl = 7
ptBoundary = 9

perturbTokens :: [(String, Int)]
perturbTokens =
  [ ("header", 0), ("names", 1), ("netnames", 2), ("events", 3)
  , ("copylinks", 4), ("moves", 5), ("windows", 6), ("decl", 7)
  , ("opnds", 8), ("boundary", 9), ("usepos", 10), ("arraypos", 11)
  , ("sites", 12), ("evalsites", 13), ("usites", 14), ("passsites", 15)
  , ("asites", 16), ("lits", 17), ("vtrees", 18), ("atrees", 19)
  , ("ctx", 20), ("frames", 21), ("cellframes", 22), ("refpos", 23)
  , ("nodeframes", 24), ("frametable", 25), ("undet", 26), ("closures", 27) ]

-- Tokens not known to this compiler are ignored.
perturbMask :: Int
perturbMask = foldl' (.|.) 0
                [ 1 `shiftL` b | tok <- splitComma capturePerturbEnv,
                                 Just b <- [lookup tok perturbTokens] ]
  where splitComma s = case break (== ',') s of
                         (a, []) -> [a]
                         (a, _:rest) -> a : splitComma rest

perturbing :: Int -> Bool
perturbing b = perturbMask .&. (1 `shiftL` b) /= 0

-- left rotation by one
rotate :: [a] -> [a]
rotate [] = []
rotate (x:xs) = xs ++ [x]
