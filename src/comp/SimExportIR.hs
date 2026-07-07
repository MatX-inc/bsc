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
-- ME inhibitors, foreign-function signatures, content hashes.  Unhandled
-- IR constructs fail loudly ('internalError') rather than exporting
-- wrong data.
module SimExportIR
    ( birVersion
    , simSystemToBir
    , writeBirFile
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

import Data.List (foldl', nub)
import Data.Maybe (mapMaybe)

import ErrorUtil (internalError)
import Id (Id, getIdBaseString, getIdQualString, isSignedId,
           mkIdCanFire, mkIdWillFire)
import IntLit (IntLit(..))
import PPrint (ppReadable)
import Prim (PrimOp(..))
import Pragma (RulePragma(..))
import Wires (ClockDomain(..), ResetId, writeResetId, WireProps(..), wpResets)
import VModInfo (vName, getVNameString)
import AScheduleInfo (AScheduleInfo(..), SchedNode(..), getSchedNodeId)
import ASyntaxUtil (aVars)
import SimCCBlock (SimCCFnStmt(..))
import SimMakeCBlocks (cvtActions)
import SimDomainInfo (DomainInfo(..))
import ASyntax
import SimPackage

-- | Bumped on any change to the encoded shape; must equal BIR_VERSION in
-- trs-ir/src/lib.rs.
birVersion :: Word32
birVersion = 1

-- ===============
-- String interning
--
-- All identifiers in BIR are indices into one design-wide string table.
-- Encoders run in a state monad accumulating the table; the table is
-- emitted after all bodies are encoded (CBOR encodings are values, so
-- assembly order is independent of write order).

data StrTable = StrTable !(M.Map String Word32) ![String] !Word32

type EncM = State StrTable

emptyStrTable :: StrTable
emptyStrTable = StrTable M.empty [] 0

str :: String -> EncM Word32
str s = do
    StrTable m rev n <- gets id
    case M.lookup s m of
      Just i  -> return i
      Nothing -> do
        modify (\_ -> StrTable (M.insert s n m) (s : rev) (n + 1))
        return n

strE :: String -> EncM C.Encoding
strE s = encW32 <$> str s

idE :: Id -> EncM C.Encoding
idE = strE . getIdBaseString

tableStrings :: StrTable -> [String]
tableStrings (StrTable _ rev _) = reverse rev

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
simSystemToBir :: SimSystem -> L.ByteString
simSystemToBir ssys = CW.toLazyByteString (encDesign ssys)

-- | Write the design's .bir file.
writeBirFile :: FilePath -> SimSystem -> IO ()
writeBirFile path ssys = L.writeFile path (simSystemToBir ssys)

encDesign :: SimSystem -> C.Encoding
encDesign ssys =
    let pkgs = M.elems (ssys_packages ssys)
        pkgNames = S.fromList (map (getIdBaseString . sp_name) pkgs)
        instmap = M.toList (ssys_instmap ssys)

        -- per-module schedule analysis (segments, exec order, disjointness)
        msis = M.fromList [ (getIdBaseString (sp_name p), analyzeModule p)
                          | p <- pkgs ]
        segmaps = M.map msi_segIdx msis
        instToMod = ssys_instmap ssys

        action :: EncM [(String, C.Encoding)]
        action = do
          topId <- str (getIdBaseString (ssys_top ssys))
          modsEnc <- mapM (\p -> encModule pkgNames
                                   (msis M.! getIdBaseString (sp_name p)) p)
                          pkgs
          instEnc <- mapM (\(p, m) -> encPair <$> strE p <*> strE m) instmap
          compsEnc <- mapM (encComposition instToMod segmaps)
                           (ssys_schedules ssys)
          clkId <- traverse str (ssys_default_clk ssys)
          rstId <- traverse str (ssys_default_rst ssys)
          return
            [ ("version", encW32 birVersion)
            , ("strings", mempty)   -- placeholder, replaced below
            , ("top", encW32 topId)
            , ("modules", encList modsEnc)
            , ("instance_map", encList instEnc)
            , ("compositions", encList compsEnc)
            , ("foreign_funcs", encList [])  -- P0 TODO: from ssys_ffuncmap
            , ("default_clock", encMaybe encW32 clkId)
            , ("default_reset", encMaybe encW32 rstId)
            ]

        (fields, finalTbl) = runState action emptyStrTable
        strsEnc = encList (map encStr (tableStrings finalTbl))
        fields' = [ (k, if k == "strings" then strsEnc else v)
                  | (k, v) <- fields ]
    in  encStruct fields'

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
    , msi_segIdx  :: M.Map String Int         -- node key -> segment index
    , msi_execPos :: M.Map String Int         -- rule name -> local exec pos
    , msi_disj    :: M.Map String (S.Set String) -- rule -> disjoint rules
    }

-- Segment lookup key for a schedule node ("S:rule" / "E:rule"), local
-- (unqualified) name.
nodeKey :: SchedNode -> String
nodeKey (Sched i) = "S:" ++ getIdBaseString i
nodeKey (Exec i) = "E:" ++ getIdBaseString i

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

        disj0 = exclRulesDBToDisjRulesDB (asi_exclusive_rules_db asi)
        isRule s = M.member s ruleDom
        disj = M.fromList
            [ (rs, S.filter isRule (S.map getIdBaseString ds))
            | (r, ds) <- M.toList disj0
            , let rs = getIdBaseString r
            , isRule rs ]

        doms = nub (M.elems ruleDom)

        -- Split this domain's rule nodes into segments at method positions.
        segsFor :: Int -> [Seg]
        segsFor d =
            let step (segs, nodes, cut) node =
                    let base = getIdBaseString (getSchedNodeId node)
                    in  if base `S.member` methodNames
                        then (segs, nodes, nub (cut ++ [base]))
                        else if M.lookup base ruleDom == Just d
                        then if null cut
                             then (segs, nodes ++ [node], [])
                             else (segs ++ [Seg nodes cut], [node], [])
                        else (segs, nodes, cut)
                (segs, nodes, cut) = foldl' step ([], [], []) order
            in  if null nodes && null cut && not (null segs)
                then segs
                else segs ++ [Seg nodes cut]

        domSegs = [ (d, segsFor d) | d <- doms ]

        -- keyed per node, not per rule: a method cut can fall between a
        -- rule's Sched and Exec, putting them in different segments
        segIdx = M.fromList
            [ (nodeKey n, i)
            | (_, segs) <- domSegs
            , (i, seg) <- zip [(0 :: Int) ..] segs
            , n <- seg_nodes seg ]
    in
        ModSchedInfo { msi_domains = domSegs
                     , msi_segIdx = segIdx
                     , msi_execPos = execPos
                     , msi_disj = disj }

-- ===============
-- Compositions

-- Qualified rule path: "inst.path.RL_rule" (the merge stores the instance
-- path in the Id qualifier, qualifyChildId).
qualPath :: Id -> String
qualPath i = case getIdQualString i of
               ""  -> getIdBaseString i
               q   -> q ++ "." ++ getIdBaseString i

encComposition :: M.Map String String -> M.Map String (M.Map String Int)
               -> SimSchedule -> EncM C.Encoding
encComposition instToMod segmaps ss = do
    let order = ss_sched_order ss

        -- resolve a merged node to (instance path, segment index);
        -- top-module method nodes resolve to Nothing and are skipped
        resolve node =
            let i = getSchedNodeId node
                inst = getIdQualString i
                key = case node of
                        Sched _ -> "S:" ++ getIdBaseString i
                        Exec _ -> "E:" ++ getIdBaseString i
                modName = case M.lookup inst instToMod of
                            Just m -> m
                            Nothing -> internalError
                              ("SimExportIR: unknown instance " ++ show inst)
                segmap = M.findWithDefault M.empty modName segmaps
            in  (,) inst <$> M.lookup key segmap

        -- The flat merged order freely interleaves Sched and Exec nodes of
        -- different instances, so it cannot be collapsed into segment runs
        -- directly.  Instead, project the merged constraint graph onto
        -- (instance, segment) units and topologically sort those; the
        -- BIR.md section 4 argument (cross-instance constraints only
        -- attach at method cut points) makes this projection acyclic.
        units = nub (mapMaybe resolve order)
        firstPos = M.fromList
            (reverse [ (u, p)
                     | (p, Just u) <- zip [(0 :: Int) ..] (map resolve order) ])

        unitEdges = S.fromList
            [ (pu, nu)
            | (n, preds) <- ss_sched_graph ss
            , Just nu <- [resolve n]
            , p <- preds
            , Just pu <- [resolve p]
            , pu /= nu ]

        -- Kahn's algorithm; ties broken by first appearance in the flat
        -- order so the output tracks bsc's own choice.
        succsOf u = [ b | (a, b) <- S.toList unitEdges, a == u ]
        indeg0 = M.fromListWith (+)
                   ([ (u, 0 :: Int) | u <- units ]
                    ++ [ (b, 1) | (_, b) <- S.toList unitEdges ])
        pickNext ready = case ready of
            [] -> Nothing
            _  -> Just (snd (minimum
                    [ (M.findWithDefault maxBound u firstPos, u)
                    | u <- ready ]))
        kahn indeg done
            | Just u <- pickNext
                [ v | (v, d) <- M.toList indeg, d == 0 ] =
                let indeg' = M.delete u indeg
                    indeg'' = foldl' (\m v -> M.adjust (subtract 1) v m)
                                     indeg' (succsOf u)
                in  kahn indeg'' (u : done)
            | M.null indeg = reverse done
            | otherwise = internalError
                ("SimExportIR: cyclic segment graph; a module boundary is "
                 ++ "interleaved below method granularity: "
                 ++ show (M.keys indeg))
        entries = kahn indeg0 []

        dups = length entries /= S.size (S.fromList entries)

        execPos = M.fromList [ (qualPath i, p)
                             | (Exec i, p) <- zip order [(0 :: Int) ..] ]

        crossPairs =
            [ (qualPath r, qualPath d)
            | (r, ds) <- M.toList (ss_disjoint_rules_db ss)
            , d <- S.toList ds
            , getIdQualString r /= getIdQualString d
            , let pr = M.lookup (qualPath r) execPos
            , let pd = M.lookup (qualPath d) execPos
            , maybe False id ((<) <$> pr <*> pd) ]

        ticks = [ (getIdQualString prim, getIdBaseString prim,
                   getIdBaseString port)
                | di <- M.elems (ss_domain_info_map ss)
                , (prim, (port, _)) <- di_prims di ]

    if dups
      then internalError ("SimExportIR: non-contiguous segment interleaving; "
                          ++ "composition needs graph-based derivation")
      else do
        clkId <- str (oscName (ss_clock ss))
        entriesEnc <- mapM (\(inst, seg) -> do
                              instE <- strE inst
                              return $ encStruct
                                [ ("instance", instE)
                                , ("segment", encW32 (fromIntegral seg))
                                ])
                           entries
        ticksEnc <- mapM (\(inst, prim, port) -> do
                            iE <- strE inst
                            pE <- strE prim
                            oE <- strE port
                            return $ encStruct
                              [ ("instance", iE), ("prim", pE), ("port", oE) ])
                         ticks
        earlyEnc <- mapM (strE . qualPath) (ss_early_rules ss)
        crossEnc <- mapM (\(a, b) -> encPair <$> strE a <*> strE b) crossPairs
        return $ encStruct
          [ ("clock", encW32 clkId)
          , ("posedge", encBool (ss_posedge ss))
          , ("entries", encList entriesEnc)
          , ("ticks", encList ticksEnc)
          , ("early", encList earlyEnc)
          , ("cross_inhibits", encList crossEnc)
          ]

oscName :: AClock -> String
oscName clk = case aclock_osc clk of
                ASPort _ i -> getIdBaseString i
                ASDef _ i -> getIdBaseString i
                e -> ppReadable e

-- ===============
-- Modules

encModule :: S.Set String -> ModSchedInfo -> SimPackage -> EncM C.Encoding
encModule pkgNames msi pkg = do
    nameId <- idE (sp_name pkg)
    domsEnc <- mapM encClockDomain (sp_clock_domains pkg)
    rstsEnc <- mapM encReset (sp_reset_list pkg)
    insEnc <- mapM encInput (sp_inputs pkg)
    instsEnc <- mapM (encInstance pkgNames (sp_method_order_map pkg))
                     (M.elems (sp_state_instances pkg))
    defsEnc <- mapM encDef (M.elems (sp_local_defs pkg))
    rulesEnc <- mapM (encRule msi pkg) (sp_rules pkg)
    methodsEnc <- concat <$> mapM (encMethod pkg) (sp_interface pkg)
    schedEnc <- encSchedule msi pkg
    return $ encStruct
      [ ("name", nameId)
      , ("content_hash", encList (replicate 32 (C.encodeWord8 0))) -- P0 TODO
      , ("clock_domains", encList domsEnc)
      , ("resets", encList rstsEnc)
      , ("inputs", encList insEnc)
      , ("instances", encList instsEnc)
      , ("defs", encList defsEnc)
      , ("rules", encList rulesEnc)
      , ("methods", encList methodsEnc)
      , ("schedule", schedEnc)
      ]

encSchedule :: ModSchedInfo -> SimPackage -> EncM C.Encoding
encSchedule msi pkg = do
    domsEnc <- mapM encModSched (msi_domains msi)
    let esposito = case asch_scheduler (asi_schedule (sp_schedule pkg)) of
                     [ASchedEsposito pairs] -> pairs
                     scheds -> concat [ ps | ASchedEsposito ps <- scheds ]
    conflictsEnc <- mapM (\(r, blockers) -> do
                            rE <- idE r
                            bsE <- mapM idE blockers
                            return (encPair rE (encList bsE)))
                         esposito
    disjEnc <- mapM (\(r, ds) -> do
                       rE <- strE r
                       dsE <- mapM strE (S.toList ds)
                       return (encPair rE (encList dsE)))
                    (M.toList (msi_disj msi))
    return $ encStruct
      [ ("domains", encList domsEnc)
      , ("conflicts", encList conflictsEnc)
      , ("disjoint", encList disjEnc)
      ]

encModSched :: (Int, [Seg]) -> EncM C.Encoding
encModSched (d, segs) = do
    segsEnc <- mapM encSeg segs
    return $ encStruct
      [ ("domain", encW32 (fromIntegral d))
      , ("posedge", encBool True)   -- P0 TODO: negedge-triggered domains
      , ("segments", encList segsEnc)
      -- P0 TODO: per-module tick order (composition carries ticks for now)
      , ("ticks", encList [])
      ]

encSeg :: Seg -> EncM C.Encoding
encSeg seg = do
    nodesEnc <- mapM encSchedNode (seg_nodes seg)
    cutEnc <- mapM strE (seg_cut seg)
    return $ encStruct
      [ ("nodes", encList nodesEnc)
      , ("cut", encList cutEnc)
      ]

encSchedNode :: SchedNode -> EncM C.Encoding
encSchedNode (Sched i) = encVariant "Sched" <$> idE i
encSchedNode (Exec i) = encVariant "Exec" <$> idE i

encClockDomain :: AClockDomain -> EncM C.Encoding
encClockDomain (ClockDomain n, clocks) = do
    clksEnc <- mapM (\c -> encPair <$> encExpr (aclock_osc c)
                                   <*> encExpr (aclock_gate c))
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

encInput :: AAbstractInput -> EncM C.Encoding
encInput (AAI_Port (i, t)) = encPort (i, t) "MethodArg"
encInput (AAI_Clock osc _mgate) = do
    n <- idE osc
    return $ encPortRaw n 1 "Clock"
encInput (AAI_Reset r) = do
    n <- idE r
    return $ encPortRaw n 1 "Reset"
encInput (AAI_Inout {}) =
    internalError "SimExportIR.encInput: Inout not supported by Bluesim"

encPort :: (Id, AType) -> String -> EncM C.Encoding
encPort (i, t) kind = do
    n <- idE i
    return $ encPortRaw n (aTypeWidth t) kind

encPortRaw :: C.Encoding -> Word32 -> String -> C.Encoding
encPortRaw nameEnc w kind =
    encStruct
      [ ("name", nameEnc)
      , ("width", encW32 w)
      , ("kind", encUnitVariant kind)
      ]

encInstance :: S.Set String -> MethodOrderMap -> AVInst -> EncM C.Encoding
encInstance pkgNames mom avi = do
    nameId <- idE (avi_vname avi)
    let modName = getVNameString (vName (avi_vmi avi))
    kindEnc <-
      if modName `S.member` pkgNames
        then encVariant "Module" <$> strE modName
        -- P0 TODO: map primitives to their structured kinds (Reg, Fifo,
        -- ...) instead of Other; the structured mapping lands with codegen.
        else do mEnc <- strE modName
                return $ encVariant "Prim"
                           (encVariant "Other" (encStruct [("name", mEnc)]))
    argsEnc <- mapM encExpr (avi_iargs avi)
    let morder = S.toList (M.findWithDefault S.empty (avi_vname avi) mom)
    morderEnc <- mapM (\(a, b) -> encPair <$> idE a <*> idE b) morder
    portsEnc <- mapM (\(m, n) -> encPair <$> idE m
                                         <*> pure (encW32 (fromIntegral n)))
                     (avi_iarray avi)
    return $ encStruct
      [ ("name", nameId)
      , ("kind", kindEnc)
      , ("args", encList argsEnc)
      , ("method_order", encList morderEnc)
      , ("port_counts", encList portsEnc)
      ]

encDef :: ADef -> EncM C.Encoding
encDef (ADef i t e _props) = do
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
          ])
      ]

-- The exact def/action interleaving that Bluesim executes: reuse the
-- backend's own linearization (tsortActionsAndDefs via cvtActions) and
-- encode its statement list.
bodyStmts :: SimPackage -> Id -> WireProps -> S.Set AId -> [AAction]
          -> [SimCCFnStmt]
bodyStmts pkg rid wprops other_defs acts =
    let reset_ids = [ ae_objid (areset_wire rst)
                    | n <- wpResets wprops
                    , Just rst <- [lookup n (sp_reset_list pkg)] ]
    in  cvtActions (sp_name pkg) rid (sp_local_defs pkg)
                   (sp_method_order_map pkg) other_defs acts reset_ids

encStmt :: SimCCFnStmt -> EncM C.Encoding
encStmt (SFSDef _ (_, i) (Just _)) = encVariant "Def" <$> idE i
encStmt (SFSDef _ _ Nothing) =
    -- declaration only (e.g. a task temp); the Task action fills it
    return mempty
encStmt (SFSAssign _ i _) = encVariant "Def" <$> idE i
encStmt (SFSAction act) = encVariant "Action" <$> encAction act
encStmt (SFSAssignAction _ i act _) = do
    dE <- idE i
    aE <- encAction act
    return $ encVariant "AvAction" (encStruct [("def", dE), ("action", aE)])
encStmt (SFSCond c ts es) = do
    cE <- encExpr c
    tE <- encStmts ts
    eE <- encStmts es
    return $ encVariant "Cond"
               (encStruct [("cond", cE), ("then_", tE), ("else_", eE)])
encStmt s = internalError ("SimExportIR.encStmt: " ++ ppReadable s)

-- mempty markers from declaration-only stmts must not appear in the list
encStmts :: [SimCCFnStmt] -> EncM C.Encoding
encStmts stmts = do
    let keep (SFSDef _ _ Nothing) = False
        keep _ = True
    encList <$> mapM encStmt (filter keep stmts)

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
    bodyEnc <- encStmts (bodyStmts pkg (arule_id r) (arule_wprops r)
                                   S.empty (arule_actions r))
    let dom = case wpClockDomain (arule_wprops r) of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
        crossing = RPclockCrossingRule `elem` arule_pragmas r
        -- disjoint rules of this module executing earlier in the module's
        -- own order inhibit this rule (destructive-execution patch;
        -- cross-module pairs live in the composition)
        base = getIdBaseString (arule_id r)
        myPos = M.findWithDefault maxBound base (msi_execPos msi)
        earlier r' = M.findWithDefault maxBound r' (msi_execPos msi) < myPos
        inhibits = filter earlier
                     (S.toList (M.findWithDefault S.empty base (msi_disj msi)))
    inhibitsEnc <- mapM strE inhibits
    return $ encStruct
      [ ("name", nameId)
      , ("can_fire", cf)
      , ("will_fire", wf)
      , ("body", bodyEnc)
      , ("clock_domain", encW32 dom)
      , ("crossing", encBool crossing)
      , ("me_inhibits", encList inhibitsEnc)
      ]

-- Interface methods.  Clock/reset/inout interface entries carry no
-- executable content (they are in the clock/reset lists); skip them.
encMethod :: SimPackage -> AIFace -> EncM [C.Encoding]
encMethod pkg (AIDef name inputs props pred_ (ADef _ t e _) _ _) = do
    m <- encMethodStruct pkg name "Value" (concat inputs) (Just pred_) [] (Just (t, e))
                         props
    return [m]
encMethod pkg (AIAction inputs props pred_ name body _) = do
    m <- encMethodStruct pkg name "Action" (concat inputs) (Just pred_)
                         (concatMap arule_actions body) Nothing props
    return [m]
encMethod pkg (AIActionValue inputs props pred_ name body (ADef _ t e _) _) = do
    m <- encMethodStruct pkg name "ActionValue" (concat inputs) (Just pred_)
                         (concatMap arule_actions body) (Just (t, e)) props
    return [m]
encMethod _ (AIClock {}) = return []
encMethod _ (AIReset {}) = return []
encMethod _ (AIInout {}) = return []

encMethodStruct :: SimPackage -> Id -> String -> [AInput] -> Maybe APred
                -> [AAction] -> Maybe (AType, AExpr) -> WireProps
                -> EncM C.Encoding
encMethodStruct pkg name kind inputs mpred body mresult props = do
    nameId <- idE name
    argsEnc <- mapM (\it -> encPort it "MethodArg") inputs
    readyEnc <- traverse encExpr mpred
    -- defs the result expression needs must be computed with the body
    -- (an ActionValue's return can depend on the body's effects order)
    let result_defs = case mresult of
          Just (_, e) -> S.fromList
              [ i | i <- aVars e, i `M.member` sp_local_defs pkg ]
          Nothing -> S.empty
    bodyEnc <- encStmts (bodyStmts pkg name props result_defs body)
    resultEnc <- traverse (encExpr . snd) mresult
    let dom = case wpClockDomain props of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
    return $ encStruct
      [ ("name", nameId)
      , ("kind", encUnitVariant kind)
      , ("args", encList argsEnc)
      , ("ready", encMaybe id readyEnc)
      , ("body", bodyEnc)
      , ("result", encMaybe id resultEnc)
      , ("clock_domain", encW32 dom)
      ]

-- ===============
-- Expressions

aTypeWidth :: AType -> Word32
aTypeWidth (ATBit n) = fromIntegral n
aTypeWidth (ATString _) = 0
aTypeWidth t = internalError ("SimExportIR.aTypeWidth: " ++ ppReadable t)

-- An Integer as little-endian 32-bit limbs (matching WideData layout).
toLimbs :: Word32 -> Integer -> [Word32]
toLimbs w v =
    let nlimbs = max 1 ((fromIntegral w + 31) `div` 32)
        limb k = fromIntegral ((v `shiftR` (32 * k)) .&. 0xFFFFFFFF)
    in  map limb [0 .. nlimbs - 1]

encExpr :: AExpr -> EncM C.Encoding
encExpr (ASInt _ t lit) =
    let w = aTypeWidth t
    in  return $ encVariant "Const" $ encStruct
          [ ("width", encW32 w)
          , ("limbs", encList (map encW32 (toLimbs w (ilValue lit))))
          ]
encExpr (ASDef _ i) = encVariant "Def" <$> idE i
encExpr (ASPort _ i) = encVariant "Port" <$> idE i
encExpr (ASParam _ i) = encVariant "Param" <$> idE i
encExpr (ASStr _ _ s) = encVariant "Str" <$> strE s
encExpr (AMethCall t obj meth args) = do
    o <- idE obj
    m <- idE meth
    argsEnc <- mapM encExpr args
    return $ encVariant "MethCall" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("instance", o)
      , ("method", m)
      , ("port", encW32 0)   -- P0 TODO: multi-port assignment
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
    oscEnc <- encExpr (aclock_osc clk)
    gateEnc <- encExpr (aclock_gate clk)
    return $ encVariant "Clock" $ encStruct
      [ ("osc", oscEnc)
      , ("gate", gateEnc)
      ]
encExpr (ASReset _ rst) = do
    wireEnc <- encExpr (areset_wire rst)
    return $ encVariant "Reset" $ encStruct
      [ ("wire", wireEnc)
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

encAction :: AAction -> EncM C.Encoding
encAction (ACall obj meth (cond : args)) = do
    o <- idE obj
    m <- idE meth
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "MethCall" $ encStruct
      [ ("instance", o)
      , ("method", m)
      , ("port", encW32 0)   -- P0 TODO: multi-port assignment
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      ]
encAction (AFCall _ fun _ (cond : args) _) = do
    f <- strE fun
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "Foreign" $ encStruct
      [ ("func", f)
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      , ("signed", encList (map (encBool . argSigned) args))
      ]
encAction (ATaskAction _ fun _ cookie (cond : args) mtemp mty _) = do
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
      , ("signed", encList (map (encBool . argSigned) args))
      ]
encAction a = internalError ("SimExportIR.encAction: " ++ ppReadable a)

-- Signed-display flag for a system-task argument: matches encodeArgs's
-- "-" prefix rule (ForeignFunctions.hs:256-262).
argSigned :: AExpr -> Bool
argSigned (ASDef _ aid) = isSignedId aid
argSigned _ = False
