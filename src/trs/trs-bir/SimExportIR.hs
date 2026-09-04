{-# LANGUAGE OverloadedStrings #-}

-- | BIR export: serialize the post-scheduling simulation system for the
-- TRS backend (src/trs).
--
-- The format is specified in src/trs/BIR.md and defined operationally
-- by the Rust types in src/trs/crates/trs-ir; @trs ir dump@
-- round-trips the output.  The input is the same 'SimSystem' that
-- 'simMakeCBlocks' consumes today (post 'simExpand' / 'simPackageOpt'),
-- so schedule merging and package optimization stay in this compiler.
--
-- Wire conventions match ciborium's serde defaults on the Rust side:
-- structs are CBOR maps keyed by field name, tuples and Vecs are arrays,
-- @Option@ is null-or-value, unit enum variants are strings, and payload
-- variants are single-entry maps ({variant: payload}).
--
-- STATUS (P0, in progress): exports the design skeleton plus module
-- bodies — clock domains, resets, inputs, instances (with method-order
-- pairs), defs, rules, and interface methods, with full expression and
-- action trees.  Not yet exported: segmented schedules, compositions,
-- ME inhibitors, content hashes.  Unhandled IR constructs fail loudly
-- ('internalError') rather than exporting wrong data.
module SimExportIR
    ( birVersion
    , writeModuleBir
    , writeForeignBir
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.State.Strict (State, runState, gets, modify)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word32)

import qualified Codec.CBOR.Encoding as C
import qualified Codec.CBOR.Write as CW

import Data.Char (isDigit)
import Data.List (foldl', nub, sort, sortBy, stripPrefix)
import Data.Maybe (mapMaybe, isJust)

import ErrorUtil (internalError)
import Util (stableOrdNub)
import Id (Id, getIdBaseString, getIdQualString, isSignedId,
           mkEnableId, mkIdCanFire, mkIdWillFire, mkRdyId, cmpIdByName)
import IntLit (IntLit(..))
import PPrint (ppReadable)
import Prim (PrimOp(..))
import SCC (tsort)
import Pragma (RulePragma(..), isAlwaysEn)
import Wires (ClockDomain(..), ResetId, writeResetId, WireProps(..), wpResets)
import VModInfo (vName, getVNameString, VWireInfo(..), VClockInfo(..),
                 VResetInfo(..), VArgInfo(..), vRst, lookupOutputClockWires,
                 lookupInputClockWires)
import AUses (MethodId(..))
import AScheduleInfo (AScheduleInfo(..), ADynSched(..), Conflicts(..), RuleRelationDB(..),
                      RuleRelationInfo(..))
import ASyntax (getInstArgs)
import ASyntaxUtil (aVars, tupleElemRange, argInputPorts)
import SimCCBlock (SimCCFnStmt(..), isOkId)
import SimMakeCBlocks (cvtActions, mkAVMethTmpId)
import SimPrimitiveModules (primMap, tickElem, tickIsPos, tickIsNeg,
                            getPrimDomainInfo)
import SimDomainInfo (DomainInfo(..))
import ForeignFunctions (ForeignFunction(..), ForeignType(..))
import ASyntax
import SimPackage

-- | Bumped on any change to the encoded shape; must equal BIR_VERSION in
-- trs-ir/src/lib.rs.
birVersion :: Word32
birVersion = 14

-- ===============
-- String interning
--
-- All identifiers in BIR are indices into one design-wide string table.
-- Encoders run in a state monad accumulating the table; the table is
-- emitted after all bodies are encoded (CBOR encodings are values, so
-- assembly order is independent of write order).

data StrTable = StrTable !(M.Map String Word32) ![String] !Word32

-- | The string table, which spans the design, and the output-clock
-- wires of the one module being encoded (see 'outClockOscs').
data EncState = EncState !StrTable !(M.Map AId (String, String))

type EncM = State EncState

emptyState :: EncState
emptyState = EncState (StrTable M.empty [] 0) M.empty

str :: String -> EncM Word32
str s = do
    EncState (StrTable m rev n) oscs <- gets id
    case M.lookup s m of
      Just i  -> return i
      Nothing -> do
        modify (\_ -> EncState (StrTable (M.insert s n m) (s : rev) (n + 1)) oscs)
        return n

strE :: String -> EncM C.Encoding
strE s = encW32 <$> str s

idE :: Id -> EncM C.Encoding
idE = strE . getIdBaseString

tableStrings :: EncState -> [String]
tableStrings (EncState (StrTable _ rev _) _) = reverse rev

-- ===============
-- Encoding helpers (ciborium/serde conventions)

-- A struct is a map keyed by field name.
encStruct :: [(String, C.Encoding)] -> C.Encoding
encStruct fields =
    C.encodeMapLen (fromIntegral (length fields))
    <> mconcat [ encStr k <> v | (k, v) <- fields ]

-- A unit enum variant is its name.
encUnitVariant :: String -> C.Encoding
encUnitVariant = encStr

-- A payload-carrying enum variant is {name: payload}.
encVariant :: String -> C.Encoding -> C.Encoding
encVariant name payload = C.encodeMapLen 1 <> encStr name <> payload

-- Vec<T> and tuples are arrays.
encList :: [C.Encoding] -> C.Encoding
encList xs = C.encodeListLen (fromIntegral (length xs)) <> mconcat xs

encPair :: C.Encoding -> C.Encoding -> C.Encoding
encPair a b = C.encodeListLen 2 <> a <> b

-- Option<T> is null or the value.
encMaybe :: (a -> C.Encoding) -> Maybe a -> C.Encoding
encMaybe _ Nothing  = C.encodeNull
encMaybe f (Just x) = f x

encW32 :: Word32 -> C.Encoding
encW32 = C.encodeWord32

encBool :: Bool -> C.Encoding
encBool = C.encodeBool

encStr :: String -> C.Encoding
encStr = C.encodeString . T.pack

-- ===============
-- SimSystem -> BIR

-- | Encode a 'SimSystem' as a BIR design document.
-- | magic(8) + BIR_VERSION little-endian(4), ahead of the CBOR body.
--
-- A reader has to settle the version before it decodes anything: a
-- schema change makes the body fail to deserialize, and a version
-- carried inside the body can only be reported after that failure has
-- already happened.  The last magic byte is the header's own format.
-- Mirrors BIR_MAGIC / BIR_HEADER in trs-ir.
birHeader :: L.ByteString
birHeader = L.pack (magic ++ leWord32 birVersion)
  where
    magic = [0x54, 0x52, 0x53, 0x42, 0x49, 0x52, 0x00, 0x01]  -- "TRSBIR\0\1"
    leWord32 w = [ fromIntegral ((w `shiftR` k) .&. 0xff)
                 | k <- [0, 8, 16, 24] ]

-- | A .bir: the 12-byte header, then a CBOR struct whose string table
-- is whatever the body interned while it was being encoded.
birFile :: EncM [(String, C.Encoding)] -> L.ByteString
birFile action = birHeader <> CW.toLazyByteString (encStruct fields')
  where
    (fields, finalTbl) = runState action emptyState
    fields' = [ (k, if k == "strings"
                    then encList (map encStr (tableStrings finalTbl))
                    else v)
              | (k, v) <- fields ]

-- | Write one synthesized module's .bir file.
--
-- The unit is the synthesis boundary, because that is the unit bsc
-- already has: one @(* synthesize *)@, one .ba, one module in the BIR.
-- A non-boundary module is not here to write: elaboration already
-- inlined it into the boundary that instantiated it long before this
-- point, leaving its primitives behind under prefixed names.
--
-- The file holds that one boundary and nothing else: not the
-- boundaries it instantiates, not the signatures of the imports it
-- calls, and nothing design-level.  Those describe a design, and one
-- module is not one -- `trs link` derives them from the set of files
-- it is given.
writeModuleBir :: FilePath -> Bool -> [AId] -> [String] -> String
               -> SimSystem -> IO ()
writeModuleBir path keepF elabs ffcalls modName ssys =
    L.writeFile path $ birFile $
        encModuleFields keepF elabs ffcalls modName ssys

-- | Write one foreign function's .bir file.
--
-- bsc writes a .ba per @import "BDPI"@ declaration just as it writes
-- one per synthesized module, and this is the same reduction applied
-- to it: the signature, declared once, however many modules call it.
writeForeignBir :: FilePath -> String -> ForeignFunction -> IO ()
writeForeignBir path linkname ff = L.writeFile path $ birFile $ do
    ffEnc <- encForeignFunc (linkname, ff)
    return
      [ ("strings", mempty)   -- placeholder, replaced by birFile
      , ("uses_wave_tasks", encBool False)
      , ("body", encStruct [("Foreign", ffEnc)])
      ]

encModuleFields :: Bool -> [AId] -> [String] -> String
                -> SimSystem -> EncM [(String, C.Encoding)]
encModuleFields keepF elabs ffcalls modName ssys = do
    -- the one boundary this export writes
    let allPkgs = M.elems (ssys_packages ssys)
        pkg = case [ p | p <- allPkgs
                       , getIdBaseString (sp_name p) == modName ] of
                [p] -> p
                _   -> internalError ("no such module to export: " ++ modName)

        -- bsc derives a default clock and reset for the module an
        -- export was rooted at, and this is always that module: they
        -- are its own pragmas.  A link reads whichever module turns
        -- out to be the design's top.
        defaults = (ssys_default_clk ssys, ssys_default_rst ssys)

    -- Externally tagged, as serde writes an enum.
    modEnc <- encModule (analyzeModule pkg) elabs keepF ffcalls defaults pkg
    return
      [ ("strings", mempty)   -- placeholder, replaced by birFile
      , ("uses_wave_tasks", encBool (designUsesWaveTasks [pkg]))
      , ("body", encStruct [("Fragment", modEnc)])
      ]

-- ===============
-- Module-local schedule analysis (BIR.md section 4)
--
-- A module's own schedule order (asi_sched_order) contains its rule nodes
-- AND its interface-method nodes; the method positions are the only points
-- where the outside world can interleave (the merge fuses method nodes
-- into calling parent rules).  Cutting at method positions yields the
-- per-module-type segments; the design-level composition then references
-- (instance, segment).

data Seg = Seg { seg_nodes :: [SchedNode], seg_cut :: [String] }

data ModSchedInfo = ModSchedInfo
    { msi_domains :: [(Int, [Seg])]           -- per clock domain
      -- node key -> ((domain, segment), position within segment)
    , msi_segIdx  :: M.Map String ((Int, Int), Int)
    , msi_execPos :: M.Map String Int         -- rule name -> local exec pos
    , msi_taskRules   :: S.Set String  -- rules with system/foreign tasks
    , msi_finishRules :: S.Set String  -- rules calling $finish/$fatal/$stop
      -- rule name -> its position in the module's emitted rule list; a
      -- rule reference never leaves its module, so it travels as a
      -- position rather than a name
    , msi_ruleIx  :: M.Map String Int
      -- method name -> its position in the module's emitted method
      -- list; bsc's scheduler ranks methods alongside rules, so a
      -- schedule node or conflict entry may name either
    , msi_methIx  :: M.Map String Int
    }

-- Segment lookup key for a schedule node ("S:rule" / "E:rule"), local
-- (unqualified) name.
nodeKey :: SchedNode -> String
nodeKey (Sched i) = "S:" ++ getIdBaseString i
nodeKey (Exec i) = "E:" ++ getIdBaseString i

-- | The wave-recording system tasks, as the trs runtime dispatches them.
waveTaskNames :: S.Set String
waveTaskNames = S.fromList
    [ "$dumpfile", "$dumpvars", "$dumpon", "$dumpoff"
    , "$dumpall", "$dumplimit", "$dumpflush" ]

-- | The name a task-bearing action calls, if it is one.
taskNameOf :: AAction -> Maybe String
taskNameOf (AFCall { afcall_fun = f }) = Just f
taskNameOf (ATaskAction { ataskact_fun = f }) = Just f
taskNameOf _ = Nothing

-- | Does this design record waveforms?
--
-- The runtime cannot answer this for itself: rule bodies reach it
-- deferred, and the string table it could search holds every interned
-- string, so a $display of the text "$dumpvars" reads the same as a
-- call to it.  Here the actions are plain data and the question is
-- exact.
designUsesWaveTasks :: [SimPackage] -> Bool
designUsesWaveTasks pkgs = or
    [ maybe False (`S.member` waveTaskNames) (taskNameOf a)
    | p <- pkgs
    , r <- sp_rules p ++ concatMap aIfaceRules (sp_interface p)
    , a <- arule_actions r ]

analyzeModule :: SimPackage -> ModSchedInfo
analyzeModule pkg =
    let asi = sp_schedule pkg
        order = asi_sched_order asi

        methodNames = S.fromList
            [ getIdBaseString (aif_name f) | f <- sp_interface pkg ]

        ruleDom :: M.Map String Int
        ruleDom = M.fromList
            [ (getIdBaseString (arule_id r), domOf (arule_wprops r))
            | r <- sp_rules pkg ]
        domOf wp = case wpClockDomain wp of
                     Just (ClockDomain n) -> n
                     Nothing -> 0

        execPos = M.fromList
            [ (getIdBaseString i, p)
            | (Exec i, p) <- zip order [(0 :: Int) ..] ]

        doms = nub (M.elems ruleDom)

        -- Rules with observable task effects: a $finish/$fatal in one rule
        -- makes the relative Exec order with any other task-bearing rule
        -- observable (bk_finished suppresses later output in the same
        -- instant), so encComposition pins those orders to bsc's flat
        -- order; finish rules also get singleton segments below
        actTaskName (AFCall { afcall_fun = f }) = Just f
        actTaskName (ATaskAction { ataskact_fun = f }) = Just f
        actTaskName _ = Nothing
        taskRules = S.fromList
            [ getIdBaseString (arule_id r)
            | r <- sp_rules pkg
            , any (isJust . actTaskName) (arule_actions r) ]
        finishRules = S.fromList
            [ getIdBaseString (arule_id r)
            | r <- sp_rules pkg
            , any (\a -> actTaskName a `elem` [Just "$finish", Just "$fatal",
                                               Just "$stop"])
                  (arule_actions r) ]

        -- Split this domain's rule nodes into segments: cut at interface
        -- method positions, ONE SEGMENT PER RULE NODE.  Per-node segments
        -- make the composed order reproduce bsc's flat merged order
        -- exactly (encComposition's Kahn sort breaks ties by first
        -- appearance in that order): CF rules carry no ordering edge, yet
        -- their Exec order is observable through unguarded primitives
        -- (mkUGFIFOF warn/drop, bsc.lib/getput) and task output, and a
        -- multi-node segment cannot interleave another instance's Exec
        -- between its own nodes.  This also keeps every ME/finish
        -- endpoint independently placeable — including design-level ME
        -- pairs derived through child methods (combineSchedDRDB), which
        -- interlocked multi-node segments into a cycle
        -- (bsc.interra/libraries/SRAMFile) — so the projected unit graph
        -- is trivially acyclic.
        segsFor :: Int -> [Seg]
        segsFor d =
            let step (segs, cut) node =
                    let base = getIdBaseString (getSchedNodeId node)
                    in  if base `S.member` methodNames
                        then (segs, nub (cut ++ [base]))
                        else if M.lookup base ruleDom /= Just d
                        then (segs, cut)
                        else let closed = if null cut
                                          then segs
                                          else segs ++ [Seg [] cut]
                             in  (closed ++ [Seg [node] []], [])
                (segs, cut) = foldl' step ([], []) order
            in  if null cut && not (null segs)
                then segs
                else segs ++ [Seg [] cut]

        domSegs = [ (d, segsFor d) | d <- doms ]

        -- keyed per node, not per rule: a method cut can fall between a
        -- rule's Sched and Exec, putting them in different segments
        segIdx = M.fromList
            [ (nodeKey n, ((d, i), j))
            | (d, segs) <- domSegs
            , (i, seg) <- zip [(0 :: Int) ..] segs
            , (j, n) <- zip [(0 :: Int) ..] (seg_nodes seg) ]

        -- a rule reference never leaves its module, so it travels as a
        -- position in the list encModule emits
        ruleIx = M.fromList
                   (zip (map (getIdBaseString . arule_id) (sp_rules pkg)) [0 ..])

        -- encMethod emits one entry per value/action/actionvalue face,
        -- in interface order, and nothing for clocks, resets or inouts
        methIx = M.fromList
                   (zip [ getIdBaseString (aif_name f)
                        | f <- sp_interface pkg, isMethodIfc f ] [0 ..])
    in
        ModSchedInfo { msi_domains = domSegs
                     , msi_segIdx = segIdx
                     , msi_execPos = execPos
                     , msi_taskRules = taskRules
                     , msi_finishRules = finishRules
                     , msi_ruleIx = ruleIx
                     , msi_methIx = methIx }

-- ===============
-- Modules

encModule :: ModSchedInfo
          -> [AId] -> Bool -> [String] -> (Maybe String, Maybe String)
          -> SimPackage -> EncM C.Encoding
encModule msi elab_ids keepF ffcalls (defClk, defRst) pkg = do
    nameId <- idE (sp_name pkg)
    -- the modules this fragment reaches across its boundary
    let externNames = externsOf pkg
        externIx = M.fromList (zip externNames [0 ..])
    externIds <- mapM str externNames
    -- the imports this module calls, named rather than described: the
    -- signature is in a file of its own and the link brings the two
    -- together
    ffcallIds <- mapM str ffcalls
    setOutClockOscs (M.elems (sp_state_instances pkg))
    domsEnc <- mapM encClockDomain (sp_clock_domains pkg)
    rstsEnc <- mapM encReset (sp_reset_list pkg)
    insEnc0 <- concat <$> mapM encInput (sp_inputs pkg)
    -- A method with actions is driven by an enable port unless it is
    -- always enabled, in which case bsc ties it high and drops it.  The
    -- port is the module's, so the module lists it: the reader reaches a
    -- method's enable through the id on the method, never by spelling
    -- the name again.
    enInsEnc <- sequence
      [ do n <- idE (mkEnableId (aif_name f))
           return (encPortRaw n 1 "MethodEnable")
      | f <- sp_interface pkg
      , hasEnablePort pkg f ]
    let insEnc = insEnc0 ++ enInsEnc
    -- construction order matters for load-time output (RegFileLoad gap
    -- warnings): match the C++ backend's alphabetization (raw_avis)
    -- Constructed alphabetically, to match the C++ backend, but the
    -- order the module elaborated them is what tick accumulation
    -- follows, so each instance carries its place in it.
    let elab = M.fromList (zip elab_ids [0 :: Int ..])
        avis = sortBy (\a b -> avi_vname a `cmpIdByName` avi_vname b)
                      (M.elems (sp_state_instances pkg))
    instsEnc0 <- mapM (encInstance externIx elab
                                   (sp_method_order_map pkg)) avis
    -- noinline functions instantiate as argument-less modules whose one
    -- value method computes the function
    niEnc <- mapM (\(iname, mname) -> do
                     nmEnc <- strE iname
                     kEnc <- encVariant "Module" <$> strE mname
                     return $ encStruct
                       [ ("name", nmEnc)
                       , ("kind", kEnc)
                       , ("args", encList [])
                       , ("method_order", encList [])
                       , ("port_counts", encList [])
                       ])
                  (sp_noinline_instances pkg)
    let instsEnc = instsEnc0 ++ niEnc
    -- interface ActionValue return defs join the def table so the
    -- Def-reference results resolve on the backend side
    let av_defs = [ d | AIActionValue { aif_value = d } <- sp_interface pkg
                  , not (M.member (adef_objid d) (sp_local_defs pkg)) ]
    defsEnc <- mapM encDef (M.elems (sp_local_defs pkg) ++ av_defs)
    rulesEnc <- mapM (encRule msi pkg) (sp_rules pkg)
    -- a method's sibling RDY method and WILL_FIRE def are relations the
    -- module's own tables settle; naming them here spares the runtime
    -- rebuilding them from name text
    let sibs = Siblings
          { sibIfc = S.fromList (map (getIdBaseString . aif_name)
                                     (sp_interface pkg))
          , sibDefs = S.fromList (map (getIdBaseString . adef_objid)
                                      (M.elems (sp_local_defs pkg) ++ av_defs))
          }
    methodsEnc <- concat <$> mapM (encMethod sibs pkg) (sp_interface pkg)
    schedEnc <- encSchedule msi pkg
    -- interface output clocks: external port name -> the internal osc
    -- wire being re-exported (constant = noClock, never ticks)
    -- the clocks this module takes in, and the ports they arrive on.
    -- inputs carries the ports; only this says which clock they are.
    inClksEnc <- sequence
      [ do n <- idE clkId
           o <- strE (getVNameString osc)
           g <- traverse (strE . getVNameString) mgate
           return $ encStruct
             [ ("name", n), ("osc", o), ("gate", encMaybe id g) ]
      | (clkId, Just (osc, gatePort)) <- input_clocks (wClk (sp_external_wires pkg))
      , let mgate = case gatePort of
                      Right gv -> Just gv
                      Left _   -> Nothing ]
    let oclks = output_clocks (wClk (sp_external_wires pkg))
        oclkPortName n = case lookup n oclks of
                           Just (Just (vn, _)) -> getVNameString vn
                           _ -> "CLK_" ++ getIdBaseString n
        constZero = encVariant "Const" $ encStruct
                      [ ("width", encW32 1), ("limbs", encList [encW32 0]) ]
    ifcClksEnc <- sequence
      [ do pn <- str (oclkPortName (aif_name f))
           oscEnc <- case aclock_osc (aif_clock f) of
                       p@(ASPort {}) -> encOsc p
                       _ -> return constZero
           return (encPair (encW32 pn) oscEnc)
      | f@(AIClock {}) <- sp_interface pkg ]
    -- interface output clock GATES, keyed by the clock's interface
    -- method name (what AMGate references): a parent rule that calls a
    -- method clocked by a child's gated clock reads this through
    -- Expr::Gate (Bug 1677 lifts the gate into the rule condition)
    ifcClkGatesEnc <- sequence
      [ do gn <- str (getIdBaseString (aif_name f))
           gateEnc <- encGate (aclock_gate (aif_clock f))
           return (encPair (encW32 gn) gateEnc)
      | f@(AIClock {}) <- sp_interface pkg ]
    -- interface output resets: external port name -> the internal reset
    -- wire being re-exported (parents refer to it as "<inst>$<port>")
    let orsts = output_resets (wRst (sp_external_wires pkg))
        orstPortName n = case lookup n orsts of
                           Just (Just vn, _) -> getVNameString vn
                           _ -> getIdBaseString n
        rstWireName i = if null (getIdQualString i)
                        then getIdBaseString i
                        else getIdQualString i ++ "$" ++ getIdBaseString i
    ifcRstsEnc <- sequence
      [ do pn <- str (orstPortName (aif_name f))
           wn <- case areset_wire (aif_reset f) of
                   ASPort _ i -> str (rstWireName i)
                   ASDef _ i  -> str (rstWireName i)
                   _          -> str ""
           return (encPair (encW32 pn) (encW32 wn))
      | f@(AIReset {}) <- sp_interface pkg ]
    defClkId <- traverse str defClk
    defRstId <- traverse str defRst
    return $ encStruct
      [ ("name", nameId)
      , ("externs", encList [ encStruct [("module", encW32 i)]
                            | i <- externIds ])
      , ("foreign_calls", encList (map encW32 ffcallIds))
      , ("content_hash", encList (replicate 32 (C.encodeWord8 0))) -- P0 TODO
      , ("keep_fires", encBool keepF)
      , ("default_clock", encMaybe encW32 defClkId)
      , ("default_reset", encMaybe encW32 defRstId)
      , ("clock_domains", encList domsEnc)
      , ("resets", encList rstsEnc)
      , ("inputs", encList insEnc)
      , ("input_clocks", encList inClksEnc)
      , ("ifc_clocks", encList ifcClksEnc)
      , ("ifc_clock_gates", encList ifcClkGatesEnc)
      , ("ifc_resets", encList ifcRstsEnc)
      , ("instances", encList instsEnc)
      , ("defs", encList defsEnc)
      , ("rules", encList rulesEnc)
      , ("methods", encList methodsEnc)
      , ("schedule", schedEnc)
      ]

encSchedule :: ModSchedInfo -> SimPackage
            -> EncM C.Encoding
encSchedule msi pkg = do
    domsEnc <- mapM (encModSched msi) (msi_domains msi)
    let esposito = case asch_scheduler (asi_schedule (sp_schedule pkg)) of
                     [ASchedEsposito pairs] -> pairs
                     scheds -> concat [ ps | ASchedEsposito ps <- scheds ]
    -- the scheduler ranks methods alongside rules, so these are names
    let conflictsEnc =
          [ encPair (encSchedEntity msi r)
                    (encList (map (encSchedEntity msi) blockers))
          | (r, blockers) <- esposito ]
    -- ordering facts the design-level merge needs and cannot recover
    -- from the segments alone; sorted, so the fragment's bytes do not
    -- depend on set iteration order
    let refsOf = map (encW32 . fromIntegral) . sort
                 . map (ruleIxOf msi) . S.toList
        taskEnc = refsOf (msi_taskRules msi)
        finishEnc = refsOf (msi_finishRules msi)
    -- the design-level merge's own inputs: this module's schedule graph
    -- and the disjointness it may reorder across.  The segments above
    -- are what the merge produces from them.
    let asi = sp_schedule pkg
        graphEnc =
          [ encPair (encSchedNodeE msi n) (encList (map (encSchedNodeE msi) ns))
          | (n, ns) <- asi_sched_graph asi ]
        disjEnc =
          [ encPair (encSchedEntity msi r)
                    (encList (map (encSchedEntity msi) (S.toList ds)))
          | (r, ds) <- M.toList
                         (exclRulesDBToDisjRulesDB (asi_exclusive_rules_db asi)) ]
    -- the only thing the merge reads out of the rule-relation database:
    -- Exec pairs ordered solely because two foreign calls had to be put
    -- in some order, which it may drop to break a cycle
        RuleRelationDB _ rrmap = asi_rule_relation_db asi
        isFFuncOnly i = case i of
          RuleRelationInfo Nothing Nothing Nothing Nothing Nothing
                           (Just CFFuncArbitraryChoice) -> True
          _ -> False
        ffuncEnc = [ encPair (encSchedEntity msi a) (encSchedEntity msi b)
                   | ((a, b), i) <- M.toList rrmap, isFFuncOnly i ]
    -- a flagged call names a local instance and a method of whatever
    -- `between` stays a name because bsc leaves it unqualified
    -- (see DynSched::between)
    let avis' = sortBy (\a b -> avi_vname a `cmpIdByName` avi_vname b)
                       (M.elems (sp_state_instances pkg))
        instIx = M.fromList
          (zip (map (getIdBaseString . avi_vname) avis') [0 :: Int ..])
        -- The method is named, not positioned: a position would be one
        -- in the CHILD's method list, and this module is exported
        -- without reading its children.  The link resolves it.
        encSubMeth (MethodId obj meth) = do
          let o = getIdBaseString obj
          mn <- strE (getIdBaseString meth)
          case M.lookup o instIx of
            Just i -> return $ encStruct
                        [ ("instance", encW32 (fromIntegral i))
                        , ("name", mn) ]
            Nothing -> internalError
                         ("SimExportIR: flagged call on " ++ o
                          ++ ", which is not an instance of this module")
    let encDyn d@(ADynSched {}) = do
          gE <- encExpr (ads_guardE d)
          gLE <- traverse encExpr (ads_guardL d)
          btw <- mapM (strE . getIdBaseString) (ads_between d)
          methsEnc <- mapM (\(a, b) -> encPair <$> encSubMeth a
                                               <*> encSubMeth b)
                           (ads_meths d)
          return $ encVariant "Pair" $ encStruct
            [ ("rule_e", encRuleRefName msi (getIdBaseString (ads_ruleE d)))
            , ("guard_e", gE)
            , ("rule_l", encRuleRefName msi (getIdBaseString (ads_ruleL d)))
            , ("guard_l", encMaybe id gLE)
            , ("meths", encList methsEnc)
            , ("between", encList btw)
            ]
        encDyn d@(ADynSchedSelf {}) = do
          gE <- encExpr (adss_guard d)
          btw <- mapM (strE . getIdBaseString) (adss_between d)
          earlyEnc <- encSubMeth (adss_early d)
          lateEnc <- encSubMeth (adss_late d)
          return $ encVariant "SelfCall" $ encStruct
            [ ("rule", encRuleRefName msi (getIdBaseString (adss_rule d)))
            , ("guard", gE)
            , ("early", earlyEnc)
            , ("late", lateEnc)
            , ("between", encList btw)
            ]
    dynEnc <- mapM encDyn (asi_dyn_scheds asi)
    return $ encStruct
      [ ("domains", encList domsEnc)
      , ("conflicts", encList conflictsEnc)
      , ("task_rules", encList taskEnc)
      , ("finish_rules", encList finishEnc)
      , ("sched_graph", encList graphEnc)
      , ("disjoint_rules", encList disjEnc)
      , ("ffunc_edges", encList ffuncEnc)
      , ("dyn_scheds", encList dynEnc)
      ]

encModSched :: ModSchedInfo -> (Int, [Seg]) -> EncM C.Encoding
encModSched msi (d, segs) = do
    segsEnc <- mapM (encSeg msi) segs
    return $ encStruct
      [ ("domain", encW32 (fromIntegral d))
      , ("posedge", encBool True)   -- P0 TODO: negedge-triggered domains
      , ("segments", encList segsEnc)
      -- P0 TODO: per-module tick order (composition carries ticks for now)
      , ("ticks", encList [])
      ]

encSeg :: ModSchedInfo -> Seg -> EncM C.Encoding
encSeg msi seg = do
    nodesEnc <- mapM (encSchedNode msi) (seg_nodes seg)
    cutEnc <- mapM strE (seg_cut seg)
    return $ encStruct
      [ ("nodes", encList nodesEnc)
      , ("cut", encList cutEnc)
      ]

-- | A schedule node, pure: the graph has no strings left to intern.
encSchedNodeE :: ModSchedInfo -> SchedNode -> C.Encoding
encSchedNodeE msi (Sched i) = encVariant "Sched" (encSchedEntity msi i)
encSchedNodeE msi (Exec i) = encVariant "Exec" (encSchedEntity msi i)

encSchedNode :: ModSchedInfo -> SchedNode -> EncM C.Encoding
encSchedNode msi (Sched i) = return (encVariant "Sched" (encSchedEntity msi i))
encSchedNode msi (Exec i) = return (encVariant "Exec" (encSchedEntity msi i))

-- | A rule as its position in the module's rule list.  The segment
-- builder admits only this module's own rules, so a name that is not
-- there is an exporter bug rather than something to skip.
ruleIxOf :: ModSchedInfo -> String -> Int
ruleIxOf msi n =
    M.findWithDefault
      (internalError ("SimExportIR: no rule " ++ show n
                      ++ " in its own module's rule list"))
      n (msi_ruleIx msi)

-- | The interface faces that become methods, in the order encMethod
-- emits them.
isMethodIfc :: AIFace -> Bool
isMethodIfc f = case f of
    AIDef {}         -> True
    AIAction {}      -> True
    AIActionValue {} -> True
    _                -> False

-- | What a schedule node or conflict entry names: a rule of this module
-- or one of its interface methods.  Anything else is an exporter bug.
encSchedEntity :: ModSchedInfo -> Id -> C.Encoding
encSchedEntity msi i =
    let n = getIdBaseString i
    in  case (M.lookup n (msi_ruleIx msi), M.lookup n (msi_methIx msi)) of
          (Just k, _) -> encVariant "Rule" (encW32 (fromIntegral k))
          (_, Just k) -> encVariant "Method" (encW32 (fromIntegral k))
          _ -> internalError
                 ("SimExportIR: " ++ show n
                  ++ " is neither a rule nor a method of its module")

encRuleRefName :: ModSchedInfo -> String -> C.Encoding
encRuleRefName msi n =
    case M.lookup n (msi_ruleIx msi) of
      Just k  -> encW32 (fromIntegral k)
      Nothing -> internalError
                   ("SimExportIR: no rule " ++ show n
                    ++ " in its own module's rule list")

-- | Every wire in this module that carries a submodule's output clock,
-- paired with the submodule and the port it leaves the submodule on.
--
-- The wire's own name has the instance spliced into it, and by the time
-- a clock domain names it the two halves are one string.  Resolving it
-- here, where the instance is still a thing rather than a substring, is
-- what lets the format name the submodule directly.
outClockOscs :: [AVInst] -> M.Map AId (String, String)
outClockOscs avis = M.fromList
    [ (osc_wire, (getIdBaseString (avi_vname avi), getVNameString port))
    | avi <- avis
    , (clk_id, osc_wire, _) <- getOutputClockWires avi
    , let (port, _) = lookupOutputClockWires clk_id (avi_vmi avi) ]

-- | Record the output-clock wires of the module about to be encoded.
setOutClockOscs :: [AVInst] -> EncM ()
setOutClockOscs avis =
    modify (\(EncState t _) -> EncState t (outClockOscs avis))

-- | A clock's oscillator, naming the submodule that exports it where
-- there is one.
encOsc :: AExpr -> EncM C.Encoding
encOsc e@(ASPort _ i) = do
    EncState _ oscs <- gets id
    case M.lookup i oscs of
      Just (inst, port) -> do instE <- strE inst
                              clkE <- strE port
                              return $ encVariant "ClockOut" $ encStruct
                                [ ("instance", instE), ("clock", clkE) ]
      Nothing | not (null (getIdQualString i)) -> encClockOut i
              | otherwise -> encExpr e
encOsc e = encExpr e

encClockDomain :: AClockDomain -> EncM C.Encoding
encClockDomain (ClockDomain n, clocks) = do
    clksEnc <- mapM (\c -> encPair <$> encOsc (aclock_osc c)
                                   <*> encGate (aclock_gate c))
                    clocks
    return $ encStruct
      [ ("id", encW32 (fromIntegral n))
      , ("clocks", encList clksEnc)
      ]

encReset :: (ResetId, AReset) -> EncM C.Encoding
encReset (rid, rst) = do
    wireEnc <- encExpr (areset_wire rst)
    return $ encStruct
      [ ("id", encW32 (fromIntegral (writeResetId rid)))
      , ("wire", wireEnc)
      ]

-- BDPI import signatures; the C ABI is fixed by toCtype/mkFFDecl:
-- narrow by value, wide/poly as unsigned int* limb pointers, strings as
-- char*, wide/poly returns via a first-argument out-pointer.
encForeignFunc :: (String, ForeignFunction) -> EncM C.Encoding
encForeignFunc (linkname, FF fname rt ats) = do
    nm <- strE linkname
    cn <- idE fname
    argsEnc <- mapM encForeignType ats
    retEnc <- encForeignType rt
    return $ encStruct
      [ ("name", nm)
      , ("c_name", cn)
      , ("ret", retEnc)
      , ("args", encList argsEnc)
      ]

encForeignType :: ForeignType -> EncM C.Encoding
encForeignType Void = return $ encUnitVariant "Void"
encForeignType (Narrow n) = return $ encVariant "Bits" (encW32 (fromIntegral n))
encForeignType (Wide n) = return $ encVariant "Wide" (encW32 (fromIntegral n))
encForeignType Polymorphic = return $ encUnitVariant "Poly"
encForeignType StringPtr = return $ encUnitVariant "CString"

encInput :: AAbstractInput -> EncM [C.Encoding]
encInput (AAI_Port (i, t)) = (: []) <$> encPort (i, t) "MethodArg"
encInput (AAI_Clock osc mgate) = do
    n <- idE osc
    -- a gated input clock also has a gate wire (e.g. CLK_GATE_gclk),
    -- referenced by rule guards; it follows its Clock port so the
    -- backend can bind both from one Clock{osc,gate} instantiation arg
    gateEnc <- traverse (\g -> do gn <- idE g
                                  return (encPortRaw gn 1 "ClockGate"))
                        mgate
    return (encPortRaw n 1 "Clock" : maybe [] (: []) gateEnc)
encInput (AAI_Reset r) = do
    n <- idE r
    return [encPortRaw n 1 "Reset"]
encInput (AAI_Inout {}) =
    internalError "SimExportIR.encInput: Inout not supported by Bluesim"

encPort :: (Id, AType) -> String -> EncM C.Encoding
encPort it kind = encPortOf it kind Nothing

-- | A port, with the bare name of the method argument it carries when it
-- is one.  bsc composes an argument's port name as <method>_<arg>; this
-- is the one place that knows it, so the reader never takes the name
-- apart to find the argument.
encPortOf :: (Id, AType) -> String -> Maybe Id -> EncM C.Encoding
encPortOf (i, t) kind mmeth = do
    n <- idE i
    baseEnc <- traverse strE (mmeth >>= argBaseName i)
    return $ encPortRawBase n (aTypeWidth t) kind (encMaybe id baseEnc)

-- | The argument's own name: the port name with its method's prefix off.
argBaseName :: Id -> Id -> Maybe String
argBaseName arg meth =
    stripPrefix (getIdBaseString meth ++ "_") (getIdBaseString arg)

encPortRaw :: C.Encoding -> Word32 -> String -> C.Encoding
encPortRaw nameEnc w kind =
    encPortRawBase nameEnc w kind (encMaybe id Nothing)

encPortRawBase :: C.Encoding -> Word32 -> String -> C.Encoding -> C.Encoding
encPortRawBase nameEnc w kind baseEnc =
    encStruct
      [ ("name", nameEnc)
      , ("width", encW32 w)
      , ("kind", encUnitVariant kind)
      , ("base", baseEnc)
      ]

-- | The synthesized modules a fragment instantiates, in first-use
-- order.  A cross-boundary reference names a position in this list, so
-- the module name is written once however many times it is used.
externsOf :: SimPackage -> [String]
externsOf pkg =
    stableOrdNub [ getVNameString (vName (avi_vmi avi))
                 | avi <- M.elems (sp_state_instances pkg)
                 , not (avi_user_import avi) ]

encInstance :: M.Map String Int -> M.Map AId Int
            -> MethodOrderMap -> AVInst -> EncM C.Encoding
encInstance externIx elab mom avi = do
    nameId <- idE (avi_vname avi)
    -- the instance's clock wiring, as VArgInfo describes it: which
    -- argument carries which named clock, and whether that clock has an
    -- input reset (what makes its ticks reset ticks).  Carried per
    -- instance because an imported Verilog module's wiring is declared,
    -- not looked up in a table of known primitives.
    let rstClks = nub [ c | (_, (Just _, Just c))
                              <- input_resets (vRst (avi_vmi avi)) ]
        clkArgs = [ (k, argId) | (k, (ClockArg argId, _)) <- zip [0 :: Int ..]
                                                                 (getInstArgs avi) ]
    -- which edges this port ticks on, from the primitive table: the one
    -- place that knows it, so nothing downstream has to look a module up
    -- by name to find out
    let modName' = getVNameString (vName (avi_vmi avi))
        tickSpecs = case [ l | (nm, _, _, l) <- primMap, nm == modName' ] of
                      (l : _) -> l
                      []      -> []
        ticksFor p = case [ td | td <- tickSpecs, tickElem td == p ] of
                       (td : _) | tickIsPos td && tickIsNeg td -> "Both"
                                | tickIsPos td -> "Pos"
                                | otherwise    -> "Neg"
                       [] -> "Never"
    clkArgsEnc <- mapM (\(k, argId) -> do
                          n <- idE argId
                          return $ encStruct
                            [ ("name", n)
                            , ("arg", encW32 (fromIntegral k))
                            , ("has_reset", encBool (argId `elem` rstClks))
                            , ("ticks",
                               encUnitVariant (ticksFor (getIdBaseString argId)))
                            ])
                       clkArgs
    -- What kind of thing this instantiates is recorded by elaboration,
    -- not inferred here: `avi_user_import` is the flag bsc's own
    -- hierarchy walk partitions on to decide which instances have a
    -- .ba to go find (ABinUtil.getABIHierarchy).  A foreign module is
    -- a `module verilog` -- bsc's primitives are written that way in
    -- the standard library too, so the two are one kind at source and
    -- the name is what tells them apart to whoever implements them.
    let modName = getVNameString (vName (avi_vmi avi))
    kindEnc <-
      if avi_user_import avi
        -- P0 TODO: map primitives to their structured kinds (Reg, Fifo,
        -- ...) instead of Other; the structured mapping lands with codegen.
        then do mEnc <- strE modName
                return $ encVariant "Prim"
                           (encVariant "Other" (encStruct [("name", mEnc)]))
        else case M.lookup modName externIx of
               Just k -> return (encVariant "Module" (encW32 (fromIntegral k)))
               Nothing -> internalError
                            ("SimExportIR: " ++ show modName
                             ++ " instantiated but not in the externs list")
    argsEnc <- mapM encExpr (avi_iargs avi)
    -- name-sorted: the set is (AId, AId) pairs and AId's Ord follows
    -- run/context-dependent interned-FString order — the encoded list
    -- is a constraint RELATION, so canonical order is free (and .bir
    -- bytes must not shift when batch vs -c/-e compilation changes
    -- interning order)
    let morder = sortBy (\(a, b) (c, d) ->
                           (a `cmpIdByName` c) <> (b `cmpIdByName` d))
                        (S.toList (M.findWithDefault S.empty (avi_vname avi) mom))
    morderEnc <- mapM (\(a, b) -> encPair <$> idE a <*> idE b) morder
    portsEnc <- mapM (\(m, n) -> encPair <$> idE m
                                         <*> pure (encW32 (fromIntegral n)))
                     (avi_iarray avi)
    -- A primitive has no fragment of its own, so its clock domains --
    -- a divider's slow output, a crossing register's two sides -- have
    -- to be carried by the module that instantiates it, the same three
    -- facts a submodule's fragment would state for itself.
    primClksEnc <- case getPrimDomainInfo avi modName of
      Nothing -> return C.encodeNull
      Just (avi', doms, outs) -> do
        let vmi' = avi_vmi avi'
        insEnc <- sequence
          [ do n <- idE clk_id
               o <- strE (getVNameString osc)
               g <- traverse (strE . getVNameString) mgate
               return $ encStruct
                 [ ("name", n), ("osc", o), ("gate", encMaybe id g) ]
          | (ClockArg clk_id, _) <- getInstArgs avi'
          , Just (osc, mgate) <- [lookupInputClockWires clk_id vmi'] ]
        domsEnc <- mapM encClockDomain doms
        outsEnc <- sequence
          [ do p <- strE (getVNameString port)
               o <- encOsc (aclock_osc aclk)
               return (encPair p o)
          | AIClock clk_id aclk _ <- outs
          , let (port, _) = lookupOutputClockWires clk_id vmi' ]
        return $ encStruct
          [ ("inputs", encList insEnc)
          , ("domains", encList domsEnc)
          , ("outputs", encList outsEnc)
          ]
    return $ encStruct
      [ ("name", nameId)
      , ("kind", kindEnc)
      , ("clock_args", encList clkArgsEnc)
      , ("elab_order",
         encW32 (fromIntegral (M.findWithDefault 0 (avi_vname avi) elab)))
      , ("prim_clocks", primClksEnc)
      , ("args", encList argsEnc)
      , ("method_order", encList morderEnc)
      , ("port_counts", encList portsEnc)
      ]

-- ActionValue results are read through the synthetic temp def that the
-- corresponding AvAction statement latches -- never by re-invoking the
-- method (mirrors substAV, SimMakeCBlocks.hs:1481-1482).
substAV :: AExpr -> AExpr
substAV (AMethValue ty obj meth) = ASDef ty (mkAVMethTmpId obj meth)
substAV (APrim i ty op es) = APrim i ty op (map substAV es)
substAV (AMethCall ty obj meth es) = AMethCall ty obj meth (map substAV es)
substAV (AFunCall ty i f isC es) = AFunCall ty i f isC (map substAV es)
substAV e = e

encDef :: ADef -> EncM C.Encoding
encDef (ADef i t e0 _props) = do
    let e = substAV e0
    nameId <- idE i
    exprEnc <- encExpr e
    let base = getIdBaseString i
        isCF = take 9 base == "CAN_FIRE_"
        isWF = take 10 base == "WILL_FIRE_"
    return $ encStruct
      [ ("name", nameId)
      , ("width", encW32 (aTypeWidth t))
      , ("expr", exprEnc)
      , ("props", encStruct
          [ ("can_fire", encBool isCF)
          , ("will_fire", encBool isWF)
          , ("signed", encBool False)   -- P0 TODO: from id props
          -- Whether bsc will show this name at all: three Id
          -- properties the front end sets, so nothing downstream can
          -- work it out from the name.  Which defs a debug session can
          -- actually name is decided over the linked design, from this
          -- and an analysis of the module (trs_ir::sym).
          , ("nameable", encBool (isOkId i))
          ])
      ]

-- The exact def/action interleaving that Bluesim executes: reuse the
-- backend's own linearization (tsortActionsAndDefs via cvtActions) and
-- encode its statement list.
bodyStmts :: SimPackage -> Id -> WireProps -> Maybe ADef -> [AAction]
          -> [SimCCFnStmt]
bodyStmts pkg rid wprops mretdef acts =
    let reset_ids = [ ae_objid (areset_wire rst)
                    | n <- wpResets wprops
                    , Just rst <- [lookup n (sp_reset_list pkg)] ]
        -- an ActionValue return def joins the linearization so its reads
        -- are positioned by the method-order edges (a deq-then-return-
        -- first method must read first before the deq; cvtIFace does the
        -- same, SimMakeCBlocks.hs:560-575)
        defmap = case mretdef of
                   Just d -> M.insert (adef_objid d) d (sp_local_defs pkg)
                   Nothing -> sp_local_defs pkg
        closure seen [] = seen
        closure seen (i : rest)
          | i `S.member` seen = closure seen rest
          | otherwise = case M.lookup i defmap of
              -- method-argument ports and state ids are not defs; they
              -- must not reach cvtActions' findDef
              Nothing -> closure seen rest
              Just (ADef _ _ e _) ->
                  closure (S.insert i seen) (aVars e ++ rest)
        other_defs = case mretdef of
                       Just d -> closure S.empty [adef_objid d]
                       Nothing -> S.empty
    in  cvtActions (sp_name pkg) rid defmap
                   (sp_method_order_map pkg) other_defs acts reset_ids

type SignedOracle = AId -> Bool

encStmt :: SignedOracle -> SimCCFnStmt -> EncM C.Encoding
encStmt _ (SFSDef _ (_, i) (Just e)) = encDefStmt i e
encStmt _ (SFSDef _ _ Nothing) =
    -- declaration only (e.g. a task temp); the Task action fills it
    return mempty
encStmt _ (SFSAssign _ i e) = encDefStmt i e
encStmt sgn (SFSAction act) = encVariant "Action" <$> encAction sgn act
encStmt sgn (SFSAssignAction _ i act _) = do
    dE <- idE i
    aE <- encAction sgn act
    return $ encVariant "AvAction" (encStruct [("def", dE), ("action", aE)])
encStmt sgn (SFSCond c ts es) = do
    cE <- encExpr c
    tE <- encStmts sgn ts
    eE <- encStmts sgn es
    return $ encVariant "Cond"
               (encStruct [("cond", cE), ("then_", tE), ("else_", eE)])
encStmt _ s = internalError ("SimExportIR.encStmt: " ++ ppReadable s)

-- The statement's own expression is authoritative (it carries the
-- tsort's ActionValue substitutions, which the def table may not after
-- inlining re-embedded calls); substAV catches AMethValue forms the
-- tsort leaves for the reader.
encDefStmt :: AId -> AExpr -> EncM C.Encoding
encDefStmt i e = do
    nameE <- idE i
    exprE <- encExpr (substAV e)
    return $ encVariant "Def"
               (encStruct [("name", nameE), ("expr", exprE)])

-- mempty markers from declaration-only stmts must not appear in the list
encStmts :: SignedOracle -> [SimCCFnStmt] -> EncM C.Encoding
encStmts sgn stmts = do
    let keep (SFSDef _ _ Nothing) = False
        keep _ = True
    encList <$> mapM (encStmt sgn) (filter keep stmts)

-- Signed display for a system-task argument: encodeArgs's "-" prefix
-- checks exactly the referenced Id's sign property
-- (ForeignFunctions.hs:258).  Checking the def's own id as well
-- over-flags and widens columns Bluesim prints unsigned (found by the
-- sweep regressing when it was tried).
mkSignedOracle :: SimPackage -> SignedOracle
mkSignedOracle _ = isSignedId

encRule :: ModSchedInfo -> SimPackage -> ARule -> EncM C.Encoding
encRule msi pkg r = do
    nameId <- idE (arule_id r)
    -- The predicate is a reference to the CAN_FIRE def after
    -- aAddScheduleDefs; recover the def names.
    let cfId = case arule_pred r of
                 ASDef _ i -> i
                 _         -> mkIdCanFire (arule_id r)
    cf <- idE cfId
    wf <- idE (mkIdWillFire (arule_id r))
    bodyEnc <- encStmts (mkSignedOracle pkg)
                        (bodyStmts pkg (arule_id r) (arule_wprops r)
                                   Nothing (arule_actions r))
    let dom = case wpClockDomain (arule_wprops r) of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
        crossing = RPclockCrossingRule `elem` arule_pragmas r
    -- ME inhibitors are all composition-level (cross_inhibits): the
    -- executed order is instance-specific, so a module-shared list
    -- cannot express them (and the pragma-asserted pairs must not
    -- inhibit at all when every Sched runs before its partners' Execs)
    let inhibitsEnc = [] :: [C.Encoding]
    return $ encStruct
      [ ("name", nameId)
      , ("can_fire", cf)
      , ("will_fire", wf)
      , ("body", bodyEnc)
      , ("clock_domain", encW32 dom)
      , ("crossing", encBool crossing)
      , ("me_inhibits", encList inhibitsEnc)
      ]

-- The module-local name tables a method's sibling references resolve
-- against: its interface entries and its defs.
data Siblings = Siblings { sibIfc :: S.Set String, sibDefs :: S.Set String }

-- The sibling RDY method and WILL_FIRE def of a method, each named only
-- when the module actually has one.  Only a method with actions has a
-- WILL_FIRE def (cvtIFace wf_stmts).
encSiblings :: Siblings -> Id -> Bool -> EncM (C.Encoding, C.Encoding)
encSiblings sibs name hasActions = do
    let present set i =
            if getIdBaseString i `S.member` set then Just i else Nothing
    rdyEnc <- traverse idE (present (sibIfc sibs) (mkRdyId name))
    wfEnc <- traverse idE
               (if hasActions
                then present (sibDefs sibs) (mkIdWillFire name)
                else Nothing)
    return (encMaybe id rdyEnc, encMaybe id wfEnc)

-- bsc's interface ready predicate can name the pre-block-conversion
-- RDY_<m> signal, which is not a def and so resolves to nothing on its
-- own; the backend reads the method's CAN_FIRE def in that position
-- (mkGCD.cxx: PORT_RDY_result = DEF_CAN_FIRE_result).  Emit the
-- reference that resolves, rather than one the reader has to repair.
resolveReady :: Siblings -> Id -> AExpr -> AExpr
resolveReady sibs name e@(ASDef t i)
  | not (getIdBaseString i `S.member` sibDefs sibs)
  , cf <- mkIdCanFire name
  , getIdBaseString cf `S.member` sibDefs sibs
  = ASDef t cf
  | otherwise = e
resolveReady _ _ e = e

-- Interface methods.  Clock/reset/inout interface entries carry no
-- executable content (they are in the clock/reset lists); skip them.
encMethod :: Siblings -> SimPackage -> AIFace -> EncM [C.Encoding]
encMethod sibs pkg (AIDef name inputs props pred_ (ADef _ t e _) _ _) = do
    m <- encMethodStruct sibs pkg name "Value" (concat inputs) (Just pred_) [] (Just (t, e))
                         props
    return [m]
encMethod sibs pkg (AIAction inputs props pred_ name body _) = do
    m <- encMethodStruct sibs pkg name "Action" (concat inputs) (Just pred_)
                         (concatMap arule_actions body) Nothing props
    return [m]
encMethod sibs pkg (AIActionValue inputs props pred_ name body retdef _) = do
    m <- encMethodStructAV sibs pkg name (concat inputs) (Just pred_)
                           (concatMap arule_actions body) retdef props
    return [m]
encMethod _ _ (AIClock {}) = return []
encMethod _ _ (AIReset {}) = return []
encMethod _ _ (AIInout {}) = return []

-- ActionValue methods: the return def is linearized with the body and
-- the result is a reference to it.
encMethodStructAV :: Siblings -> SimPackage -> Id -> [AInput] -> Maybe APred
                  -> [AAction] -> ADef -> WireProps -> EncM C.Encoding
encMethodStructAV sibs pkg name inputs mpred body retdef props = do
    nameId <- idE name
    (rdyEnc, wfEnc) <- encSiblings sibs name True
    enEnc <- encEnableId pkg name True
    argsEnc <- mapM (\it -> encPortOf it "MethodArg" (Just name)) inputs
    readyEnc <- traverse (encExpr . resolveReady sibs name) mpred
    bodyEnc <- encStmts (mkSignedOracle pkg)
                        (bodyStmts pkg name props (Just retdef) body)
    let ADef ret_id rt _ _ = retdef
    resultEnc <- encExpr (ASDef rt ret_id)
    let dom = case wpClockDomain props of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
    return $ encStruct
      [ ("name", nameId)
      , ("kind", encUnitVariant "ActionValue")
      , ("args", encList argsEnc)
      , ("ready", encMaybe id readyEnc)
      , ("body", bodyEnc)
      , ("result", resultEnc)
      , ("clock_domain", encW32 dom)
      , ("always_enabled", encBool (isAlwaysEn (sp_pps pkg) name))
      , ("rdy", rdyEnc)
      , ("will_fire", wfEnc)
      , ("en", enEnc)
      ]

-- | The id of a method's enable port, or nothing where it has none.
-- Must agree with `hasEnablePort`, which decides whether the module
-- lists the port at all.
encEnableId :: SimPackage -> Id -> Bool -> EncM C.Encoding
encEnableId pkg name hasActions = do
    e <- traverse idE (if hasActions && not (isAlwaysEn (sp_pps pkg) name)
                       then Just (mkEnableId name)
                       else Nothing)
    return (encMaybe id e)

-- | Whether a method is driven by an enable port: it takes actions, and
-- is not always enabled -- bsc ties those high and emits no port.
hasEnablePort :: SimPackage -> AIFace -> Bool
hasEnablePort pkg f = case f of
    AIAction {}      -> notAlways
    AIActionValue {} -> notAlways
    _                -> False
  where notAlways = not (isAlwaysEn (sp_pps pkg) (aif_name f))

encMethodStruct :: Siblings -> SimPackage -> Id -> String -> [AInput]
                -> Maybe APred -> [AAction] -> Maybe (AType, AExpr)
                -> WireProps -> EncM C.Encoding
encMethodStruct sibs pkg name kind inputs mpred body mresult props = do
    nameId <- idE name
    (rdyEnc, wfEnc) <- encSiblings sibs name (kind /= "Value")
    enEnc <- encEnableId pkg name (kind /= "Value")
    argsEnc <- mapM (\it -> encPortOf it "MethodArg" (Just name)) inputs
    readyEnc <- traverse (encExpr . resolveReady sibs name) mpred
    bodyEnc <- encStmts (mkSignedOracle pkg)
                        (bodyStmts pkg name props Nothing body)
    resultEnc <- traverse (encExpr . snd) mresult
    let dom = case wpClockDomain props of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
        -- the runtime RDY check only matters for methods with actions
        ae = kind /= "Value" && isAlwaysEn (sp_pps pkg) name
    return $ encStruct
      [ ("name", nameId)
      , ("kind", encUnitVariant kind)
      , ("args", encList argsEnc)
      , ("ready", encMaybe id readyEnc)
      , ("body", bodyEnc)
      , ("result", encMaybe id resultEnc)
      , ("clock_domain", encW32 dom)
      , ("always_enabled", encBool ae)
      , ("rdy", rdyEnc)
      , ("will_fire", wfEnc)
      , ("en", enEnc)
      ]

-- ===============
-- Expressions

aTypeWidth :: AType -> Word32
aTypeWidth (ATBit n) = fromIntegral n
aTypeWidth (ATString _) = 0
aTypeWidth ATReal = 64
-- a tuple is a wide bit vector, first element in the MSBs
aTypeWidth (ATTuple ts) = sum (map aTypeWidth ts)
aTypeWidth t = internalError ("SimExportIR.aTypeWidth: " ++ ppReadable t)

-- An Integer as little-endian 32-bit limbs (matching WideData layout).
toLimbs :: Word32 -> Integer -> [Word32]
toLimbs w v =
    let nlimbs = max 1 ((fromIntegral w + 31) `div` 32)
        limb k = fromIntegral ((v `shiftR` (32 * k)) .&. 0xFFFFFFFF)
    in  map limb [0 .. nlimbs - 1]

-- The port a method call addresses.  bsc encodes it in the method name
-- of a multi-ported primitive ("port0__read", "port1__write" -- the
-- CReg family); the number is data the runtime schedules on, so it is
-- resolved here, at the one place that knows bsc's naming, instead of
-- being re-parsed out of the name on every call.  Single-ported
-- methods are port 0.
methPortNum :: Id -> Word32
methPortNum i =
    case getIdBaseString i of
      'p':'o':'r':'t':rest
        | (ds@(_:_), '_':'_':_) <- span isDigit rest -> read ds
      _ -> 0

-- | The gate of a clock, as an expression.  A gate qualified by an
-- instance is that submodule's output clock gate, so it is emitted as
-- the reference it is rather than as a port name with the instance
-- spliced into it -- the reader reaches the submodule directly instead
-- of taking a name apart to find it.
-- | A submodule's output clock, from an osc qualified by the instance
-- that exports it.
encClockOut :: Id -> EncM C.Encoding
encClockOut i =
    let qual = getIdQualString i
    in  if '.' `elem` qual
          then internalError (pathQualified "output clock" qual (getIdBaseString i))
          else do instE <- strE qual
                  clkE <- strE (getIdBaseString i)
                  return $ encVariant "ClockOut" $ encStruct
                    [ ("instance", instE), ("clock", clkE) ]

-- | A clock reference the format cannot spell.
--
-- `Expr::Gate` and `Expr::ClockOut` name one instance, and the reader
-- resolves them by asking that instance -- for a submodule, by reading
-- the gate it records for the clock it exports, which may itself be a
-- reference one level further down.  A submodule that exports a clock
-- it gates internally reaches the exporter already flattened to a wire
-- of the grandchild ("s.gc$CLK_GATE_OUT"), and there is nothing to hand
-- the reader that it can follow: the merge derives the reference the
-- reader wants, but the composition bsc hands over does not carry it.
-- The whole construct is unsupported rather than half-supported --
-- encoding the flattened form would turn this into a panic at run time.
pathQualified :: String -> String -> String -> String
pathQualified what qual base =
    "SimExportIR: " ++ what ++ " qualified by a path, not an instance: "
    ++ show qual ++ "$" ++ base
    ++ "\n  (a submodule exporting a clock it gates internally)"

encGate :: AExpr -> EncM C.Encoding
encGate g = case g of
    ASPort _ i | not (null (getIdQualString i)) ->
        let qual = getIdQualString i
        in  if '.' `elem` qual
              then internalError (pathQualified "gate" qual (getIdBaseString i))
              else do instE <- strE qual
                      clkE <- strE (getIdBaseString i)
                      return $ encVariant "Gate" $ encStruct
                        [ ("instance", instE), ("clock", clkE) ]
    _ -> encExpr g

encExpr :: AExpr -> EncM C.Encoding
encExpr (ASInt _ t lit) =
    let w = aTypeWidth t
    in  return $ encVariant "Const" $ encStruct
          [ ("width", encW32 w)
          , ("limbs", encList (map encW32 (toLimbs w (ilValue lit))))
          ]
encExpr (ASReal _ _ d) = return $ encVariant "Real" (C.encodeDouble d)
encExpr (ASDef _ i) = encVariant "Def" <$> idE i
encExpr (ASPort _ i) = encVariant "Port" <$> idE i
encExpr (ASParam _ i) = encVariant "Param" <$> idE i
encExpr (ASStr _ _ s) = encVariant "Str" <$> strE s
encExpr (AMethCall t obj meth args) = do
    o <- idE obj
    m <- idE meth
    -- one value PER PORT, matching the callee's flattened (concat
    -- inputs) method-input defs: split-port args (ATuple / tuple-typed
    -- exprs) expand exactly as the C++ backend's call sites do
    argsEnc <- mapM encExpr (concatMap argInputPorts args)
    return $ encVariant "MethCall" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("instance", o)
      , ("method", m)
      , ("port", encW32 (methPortNum meth))
      , ("args", encList argsEnc)
      ]
encExpr (AMethValue t obj meth) = do
    o <- idE obj
    m <- idE meth
    return $ encVariant "MethValue" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("instance", o)
      , ("method", m)
      ]
encExpr (ATaskValue t _ _ _ cookie) =
    return $ encVariant "TaskValue" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("cookie", encW32 (fromIntegral cookie))
      ]
encExpr (AFunCall t _ fun _ args) = do
    f <- strE fun
    argsEnc <- mapM encExpr args
    return $ encVariant "ForeignCall" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("func", f)
      , ("args", encList argsEnc)
      ]
encExpr (AMGate _ obj clk) = do
    o <- idE obj
    c <- idE clk
    return $ encVariant "Gate" $ encStruct
      [ ("instance", o)
      , ("clock", c)
      ]
encExpr (ASClock _ clk) = do
    oscEnc <- encOsc (aclock_osc clk)
    gateEnc <- encGate (aclock_gate clk)
    return $ encVariant "Clock" $ encStruct
      [ ("osc", oscEnc)
      , ("gate", gateEnc)
      ]
encExpr (ASReset _ rst) = do
    wireEnc <- encExpr (areset_wire rst)
    return $ encVariant "Reset" $ encStruct
      [ ("wire", wireEnc)
      ]
encExpr (APrim _ _ PrimResetUnassertedVal []) =
    -- the value of an unasserted reset wire (active-low convention: 1)
    return $ encVariant "Const" $ encStruct
      [ ("width", encW32 1)
      , ("limbs", encList [encW32 1])
      ]
encExpr (APrim _ t PrimIf [c, x, y]) = do
    cEnc <- encExpr c
    xEnc <- encExpr x
    yEnc <- encExpr y
    return $ encVariant "If" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("cond", cEnc)
      , ("then_", xEnc)
      , ("else_", yEnc)
      ]
encExpr (APrim _ t PrimCase (scrut : dflt : arms)) = do
    sEnc <- encExpr scrut
    dEnc <- encExpr dflt
    armsEnc <- encCaseArms arms
    return $ encVariant "Case" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("scrutinee", sEnc)
      , ("arms", encList armsEnc)
      , ("default", dEnc)
      ]
encExpr (APrim _ t op args) = do
    argsEnc <- mapM encExpr args
    return $ encVariant "Prim" $ encStruct
      [ ("op", encUnitVariant (primOpName op))
      , ("width", encW32 (aTypeWidth t))
      , ("args", encList argsEnc)
      ]
-- SplitPorts tuples (multi-output methods) are laid out as wide bit
-- vectors with the first element in the most-significant bits
-- (Verilog {e1,...,en}) — the identical lowering the C++ backend
-- performs (SimCCBlock aExprToCExpr ATuple/ATupleSel), so exporting
-- the lowered form is byte-parity-correct by construction.  Encoded
-- as the existing Concat/Extract prims: no BIR change.
encExpr (ATuple t es) = do
    argsEnc <- mapM encExpr es
    return $ encVariant "Prim" $ encStruct
      [ ("op", encUnitVariant "Concat")
      , ("width", encW32 (aTypeWidth t))
      , ("args", encList argsEnc)
      ]
encExpr (ATupleSel t e idx) = do
    -- idx is 1-based (see AConv/AState); tupleElemRange gives the
    -- element's [hi:lo] over the concatenated layout
    eEnc <- encExpr e
    let (hi, lo) = tupleElemRange (ae_type e) idx
        ixEnc v = encVariant "Const" $ encStruct
          [ ("width", encW32 32)
          , ("limbs", encList [encW32 (fromIntegral v)])
          ]
    return $ encVariant "Prim" $ encStruct
      [ ("op", encUnitVariant "Extract")
      , ("width", encW32 (aTypeWidth t))
      , ("args", encList [eEnc, ixEnc hi, ixEnc lo])
      ]
encExpr e = internalError ("SimExportIR.encExpr: " ++ ppReadable e)

encCaseArms :: [AExpr] -> EncM [C.Encoding]
encCaseArms [] = return []
encCaseArms (ASInt _ _ lit : v : rest) = do
    vEnc <- encExpr v
    restEnc <- encCaseArms rest
    return (encPair (C.encodeWord64 (fromIntegral (ilValue lit))) vEnc
            : restEnc)
encCaseArms es =
    internalError ("SimExportIR.encCaseArms: " ++ ppReadable es)

primOpName :: PrimOp -> String
primOpName PrimStringConcat = "StringConcat"
primOpName PrimAdd = "Add"
primOpName PrimSub = "Sub"
primOpName PrimAnd = "And"
primOpName PrimOr = "Or"
primOpName PrimXor = "Xor"
primOpName PrimMul = "Mul"
primOpName PrimQuot = "Quot"
primOpName PrimRem = "Rem"
primOpName PrimSL = "Shl"
primOpName PrimSRL = "Lshr"
primOpName PrimSRA = "Ashr"
primOpName PrimInv = "Not"
primOpName PrimNeg = "Neg"
primOpName PrimEQ = "Eq"
primOpName PrimEQ3 = "Eq"   -- Bluesim is 2-state; === is ==
primOpName PrimULE = "Ule"
primOpName PrimULT = "Ult"
primOpName PrimSLE = "Sle"
primOpName PrimSLT = "Slt"
primOpName PrimSignExt = "SignExt"
primOpName PrimZeroExt = "ZeroExt"
primOpName PrimExtract = "Extract"
primOpName PrimConcat = "Concat"
primOpName PrimBNot = "Not"
primOpName PrimBAnd = "And"
primOpName PrimBOr = "Or"
primOpName PrimArrayDynSelect = "Select"
primOpName op = internalError ("SimExportIR.primOpName: " ++ show op)

-- ===============
-- Actions

encAction :: SignedOracle -> AAction -> EncM C.Encoding
encAction _ (ACall obj meth (cond : args)) = do
    o <- idE obj
    m <- idE meth
    condEnc <- encExpr cond
    -- per-port arg expansion: see encExpr (AMethCall ...)
    argsEnc <- mapM encExpr (concatMap argInputPorts args)
    return $ encVariant "MethCall" $ encStruct
      [ ("instance", o)
      , ("method", m)
      , ("port", encW32 (methPortNum meth))
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      ]
encAction sgn (AFCall _ fun _ (cond : args) assump) = do
    f <- strE fun
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "Foreign" $ encStruct
      [ ("func", f)
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      , ("signed", encList (map (encBool . argSigned sgn) args))
      , ("assumption", encBool assump)
      ]
encAction sgn (ATaskAction _ fun _ cookie (cond : args) mtemp mty assump) = do
    f <- strE fun
    tempEnc <- traverse idE mtemp
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "Task" $ encStruct
      [ ("func", f)
      , ("cookie", encW32 (fromIntegral cookie))
      , ("temp", encMaybe id tempEnc)
      , ("width", encW32 (aTypeWidth mty))
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      , ("signed", encList (map (encBool . argSigned sgn) args))
      , ("assumption", encBool assump)
      ]
encAction _ a = internalError ("SimExportIR.encAction: " ++ ppReadable a)

-- Signed-display flag for a system-task argument: matches encodeArgs's
-- "-" prefix rule (ForeignFunctions.hs:256-262), extended with the def
-- table (the sign property may be on the def rather than the reference).
argSigned :: SignedOracle -> AExpr -> Bool
argSigned sgn (ASDef _ aid) = sgn aid
argSigned _ _ = False
