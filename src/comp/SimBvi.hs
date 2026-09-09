-- | BVI-import contracts for the trs backend (design of record: the KB
-- draft \"KB: BVI-via-Verilator design (trs)\", v4).
--
-- 'deriveBvi' turns a foreign 'AVInst' (an @import \"BVI\"@ instance)
-- into the contract the BIR carries ('InstanceKind::Bvi' on the Rust
-- side, trs-ir\/src\/bvi.rs): the physical port table, logical methods
-- with their port bindings, input clocks\/resets, typed parameters, and
-- declared + synthesized combinational paths.  It simultaneously runs
-- the contract-local refusal suite of design §7 -- everything the
-- shadow-vector execution model cannot replay exactly is refused with a
-- specific tag, never silently accepted.
--
-- 'checkBviPackage' adds the parent-schedule checks that need more than
-- the instance itself: the self-SBR ActionValue atomic-read condition
-- (a consumer must be the schedule-last group member or pairwise
-- exclusive with every later member -- the same exclusivity information
-- the G0117 warning consults) and the refusal of that pattern under
-- dynamic scheduling.
--
-- Classic Bluesim behavior is untouched: nothing here runs unless the
-- foreign-import gate in 'ABinUtil.getABIHierarchy' was lifted, which
-- happens only for @genTrs@.
module SimBvi (
    BviInfo(..), BviPortI(..), BviMethodI(..), BviClockI(..),
    BviResetI(..), BviParamValueI(..),
    BviDirI(..), BviKindI(..), BviMethodKindI(..),
    bviPropReg, bviPropInhigh,
    RefKindI(..),
    deriveBvi,
    checkBviPackage,
    isBviImport,
    spHasBvi
) where

import Data.List (nub, nubBy, isPrefixOf, intercalate, partition)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Numeric (showHex)
import qualified Data.Map as M
import qualified Data.Set as S

import Error (EMsg, ErrMsg(..))
import Position (getPosition)
import Id (Id, getIdBaseString, getIdString)
import IntLit (IntLit(..))
import PPrint (ppReadable)
import ErrorUtil (internalError)
import VModInfo (VModInfo(..), VFieldInfo(..), VArgInfo(..), VName(..),
                 VeriPortProp(..), VPort, getVNameString,
                 VClockInfo(..), VResetInfo(..), VPathInfo(..))
import SchedInfo (SchedInfo(..), MethodConflictInfo(..))
import AScheduleInfo (AScheduleInfo(..), SchedNode(..),
                      areRulesExclusive)
import AUses (MethodId(..), UniqueUse(..), MethodUsesMap)
import ASyntax
import ASyntaxUtil (aVars, findAExprs, aType)
import SimPrimitiveModules (isPrimitiveModule)
import SimPackage

-- Port property bits; must match BVI_PROP_* in trs-ir/src/bvi.rs.
bviPropReg, bviPropInhigh :: Integer
bviPropReg = 1
bviPropInhigh = 2

-- ===============
-- The derived contract (mirrors trs-ir/src/bvi.rs one-to-one; the
-- encoder in SimExportIR walks this record)

data BviDirI = BviInput | BviOutput
  deriving (Eq, Show)

data BviKindI = KClock | KClockGate | KReset | KEnable | KRdy
              | KMethodArg | KMethodResult | KConstArg
  deriving (Eq, Show)

data BviMethodKindI = MKValue | MKAction | MKActionValue
  deriving (Eq, Show)

data BviPortI = BviPortI {
      bp_name  :: String,   -- physical Verilog port name
      bp_width :: Integer,
      bp_dir   :: BviDirI,
      bp_kind  :: BviKindI,
      bp_props :: Integer   -- bviProp* bits
    } deriving (Show)

data BviMethodI = BviMethodI {
      bm_name     :: String,
      bm_kind     :: BviMethodKindI,
      bm_clock    :: Maybe Int,  -- index into bi_clocks
      bm_args     :: [Int],      -- port indices, argument order
      bm_results  :: [Int],
      bm_enable   :: Maybe Int,
      bm_rdy      :: Maybe Int,
      bm_self_sbr :: Bool
    } deriving (Show)

data BviClockI = BviClockI {
      bc_name :: String,
      bc_osc  :: Int,
      bc_gate :: Maybe Int,
      bc_tick :: String   -- tick-port name (merged same-AClock groups)
    } deriving (Show)

data BviResetI = BviResetI {
      br_name       :: String,
      br_port       :: Int,
      br_active_low :: Bool
    } deriving (Show)

data BviParamValueI = PVIntSigned Integer Integer  -- width, value
                    | PVBits Integer String        -- width, hex digits
                    | PVStr String
                    | PVReal Double
                    -- a parameter FORWARDED across a synthesis
                    -- boundary: instantiation-arg index + the
                    -- serialization class from the arg's type; the
                    -- interpreter resolves it in the parent context
                    | PVFromArg Int Integer RefKindI  -- arg, width, kind
  deriving (Show)

data RefKindI = RKBits | RKReal | RKStr
  deriving (Show, Eq)

data BviInfo = BviInfo {
      bi_verilog_name :: String,
      bi_ports        :: [BviPortI],
      bi_methods      :: [BviMethodI],
      bi_clocks       :: [BviClockI],
      bi_resets       :: [BviResetI],
      -- output resets: (logical name, output port index)
      bi_out_resets   :: [(String, Int)],
      -- output clocks: (logical name, oscillator output port index)
      bi_out_clocks   :: [(String, Int)],
      bi_params       :: [(String, BviParamValueI)],
      bi_const_args   :: [(Int, BviParamValueI)],
      bi_paths        :: [(Int, Int)],
      -- self-SBR ActionValue method names: the parent-schedule
      -- atomic-read check in 'checkBviPackage' keys off these
      bi_sbr_av       :: [Id]
    } deriving (Show)

-- ===============
-- Small helpers

vpName :: VPort -> String
vpName = getVNameString . fst

vpProps :: VPort -> [VeriPortProp]
vpProps = snd

rdyPrefix :: String
rdyPrefix = "RDY_"

isRdyField :: VFieldInfo -> Bool
isRdyField (Method { vf_name = n }) =
    rdyPrefix `isPrefixOf` getIdBaseString n
isRdyField _ = False

-- constant-expression parameter/port-argument values (post-iParams
-- inlining these are literals; anything else is a refusal)
constVal :: AExpr -> Maybe BviParamValueI
constVal (ASInt _ (ATBit w) il) =
    let v = ilValue il
    in  Just $ if v < 0
               then if w <= 64 && v >= (- (2 ^ (63 :: Integer)))
                    then PVIntSigned w v
                    -- negative but too wide for i64: two's-complement
                    -- into the declared width, carried as sized hex
                    else PVBits w (showHex (v + 2 ^ w) "")
               else PVBits w (showHex v "")
constVal (ASStr _ _ s)  = Just (PVStr s)
constVal (ASReal _ _ d) = Just (PVReal d)
constVal _ = Nothing

-- collect ActionValue result reads (AMethValue nodes) inside an
-- expression -- the recursion mirrors exprForeignCalls
collectAVReads :: AExpr -> [(AId, AId)]
collectAVReads (AMethValue _ obj meth) = [(obj, meth)]
collectAVReads (APrim _ _ _ es)           = concatMap collectAVReads es
collectAVReads (AMethCall _ _ _ es)       = concatMap collectAVReads es
collectAVReads (AFunCall _ _ _ _ es)      = concatMap collectAVReads es
collectAVReads (ANoInlineFunCall _ _ _ es) = concatMap collectAVReads es
collectAVReads (ATuple _ es)              = concatMap collectAVReads es
collectAVReads (ATupleSel _ e _)          = collectAVReads e
collectAVReads _ = []

-- ===============
-- Contract derivation + contract-local refusals

deriveBvi :: AVInst -> Either [String] BviInfo
deriveBvi avi =
    let vmi = avi_vmi avi
        modName = getVNameString (vName vmi)
        clkinfo = vClk vmi
        rstinfo = vRst vmi
        mci = methodConflictInfo (vSched vmi)

        -- vFields aligns 1:1 with avi_meth_types
        -- (BackendNamingConventions.hs:395 relies on the same zip)
        fields_and_types = zip (vFields vmi) (avi_meth_types avi)
        meth_fields = [ (f, ts) | (f@(Method {}), ts) <- fields_and_types ]
        (rdy_fields, real_fields) = partition (isRdyField . fst) meth_fields

        -- RDY_x pseudo-methods (CVParser.lhs:1511 creates one per
        -- ready() clause: zero args, no EN, one output = the RDY port)
        rdy_map = M.fromList
            [ (drop (length rdyPrefix) (getIdBaseString (vf_name f)), f)
            | (f, _) <- rdy_fields ]

        -- ----------
        -- refusals visible before the port table is built

        inout_args   = [ getVNameString vn | (InoutArg vn _ _) <- vArgs vmi ]
        inout_fields = [ getIdBaseString (vf_name f)
                       | f@(Inout {}) <- vFields vmi ]
        -- output clocks (v1.2): ordinary output ports sampled by the
        -- runtime at the commit point; edges route through the
        -- interpreter's dynamic-clock network (the ClockDiv mechanism).
        -- Gated and portless output clocks stay refused.
        out_clk_list = [ (i, osc, gate)
                       | (i, Just (osc, gate)) <- output_clocks clkinfo ]
        -- output resets: an ordinary output port sampled by the runtime
        -- and routed into the interpreter's derived-reset network (one
        -- per import in v1.2; portless outputs have nothing to sample)
        out_rst_list = [ (i, vn)
                       | (i, (Just vn, _)) <- output_resets rstinfo ]

        early_refusals =
            [ "output clock '" ++ getIdBaseString i ++
              "' has no oscillator port (portless output clocks are " ++
              "not observable)"
            | (i, Nothing) <- output_clocks clkinfo ] ++
            [ "output clock '" ++ getIdBaseString i ++
              "' is gated (gated output clocks are not supported)"
            | (i, _, Just _) <- out_clk_list ] ++
            [ "output reset '" ++ getIdBaseString i ++
              "' has no output port (portless output resets are not " ++
              "observable)"
            | (i, (Nothing, _)) <- output_resets rstinfo ] ++
            [ "more than one output reset (only a single output reset " ++
              "per import is supported)"
            | length out_rst_list > 1 ] ++
            [ "inout '" ++ x ++ "' (inouts are not supported)"
            | x <- inout_args ++ inout_fields ] ++
            [ "method '" ++ getIdBaseString (vf_name f) ++
              "' has multiplicity " ++ show (vf_mult f) ++
              " (only single-ported methods are supported)"
            | (f, _) <- real_fields, vf_mult f > 1 ] ++
            [ "ready method '" ++ getIdBaseString (vf_name f) ++
              "' has no matching base method"
            | (f, _) <- rdy_fields
            , let base = drop (length rdyPrefix) (getIdBaseString (vf_name f))
            , base `notElem`
                  [ getIdBaseString (vf_name g) | (g, _) <- real_fields ] ] ++
            [ "ready method '" ++ getIdBaseString (vf_name f) ++
              "' does not have exactly one output port"
            | (f, _) <- rdy_fields
            , length (vf_outputs f) /= 1 ||
              not (null (concat (vf_inputs f))) || isJust (vf_enable f) ]

        -- ----------
        -- input clocks: only ported ones enter the contract; the
        -- tick-port name merges clock args bound to the same parent
        -- AClock (same oscillator AND gate), so coincident edges arrive
        -- as one QualifiedTick and commit in one batched eval
        clk_args = [ (arg_id, getClk clk_expr)
                   | (ClockArg arg_id, clk_expr) <- getInstArgs avi ]
        getClk (ASClock _ aclk) = aclk
        getClk e = internalError ("SimBvi.getClk: " ++ ppReadable e)
        tick_groups = nubBy (\a b -> snd a == snd b) clk_args
        tickNameFor arg_id =
            case [ getIdBaseString g
                 | (g, ac) <- tick_groups
                 , Just ac2 <- [lookup arg_id clk_args], ac == ac2 ] of
              (n : _) -> n
              [] -> getIdBaseString arg_id  -- unconnected (noClock) arg

        ported_clks = [ (i, osc, gate)
                      | (i, Just (osc, gate)) <- input_clocks clkinfo ]
        portless_clks = [ i | (i, Nothing) <- input_clocks clkinfo ]

        -- ----------
        -- input resets (portless input resets are legal; nothing to drive)
        ported_rsts = [ (i, vn) | (i, (Just vn, _)) <- input_resets rstinfo ]

        -- ----------
        -- params and constant Port args, aligned with avi_iargs (the
        -- arg INDEX is the position in vArgs; forwarded parameters are
        -- resolved from Instance.args at instantiation)
        param_args = [ (i, getVNameString vn, e)
                     | (i, (Param vn, e)) <- zip [0 ..] (getInstArgs avi) ]
        port_args  = [ (vp, e) | (Port vp _ _, e) <- getInstArgs avi ]

        -- a non-literal parameter is a closed function of the
        -- enclosing module's own parameters (bsc's iParams inlined
        -- everything else during elaboration); classify its
        -- serialization from the argument's type
        fwdParam i e = case aType e of
            ATReal      -> Just (PVFromArg i 64 RKReal)
            ATString _  -> Just (PVFromArg i 0 RKStr)
            ATBit w     -> Just (PVFromArg i w RKBits)
            _           -> Nothing

        param_refusals =
            [ "parameter '" ++ n ++ "' has an unsupported forwarded " ++
              "type: " ++ ppReadable (aType e)
            | (i, n, e) <- param_args, isNothing (constVal e)
            , isNothing (fwdParam i e) ] ++
            [ "port argument '" ++ vpName vp ++
              "' is not a compile-time constant (dynamic port arguments " ++
              "are not supported): " ++ ppReadable e
            | (vp, e) <- port_args, isNothing (constVal e) ] ++
            [ "port argument '" ++ vpName vp ++ "' is marked (*reg*)"
            | (vp, _) <- port_args, VPreg `elem` vpProps vp ]

        params = [ (n, v)
                 | (i, n, e) <- param_args
                 , Just v <- [case constVal e of
                                Just v -> Just v
                                Nothing -> fwdParam i e] ]

        -- ----------
        -- the physical port table
        -- order: clock osc/gate, reset, const args, then per-method
        -- args/EN/results, then RDY ports

        clk_ports =
            concat [ (getVNameString osc, 1, BviInput, KClock, 0) :
                     [ (getVNameString g, 1, BviInput, KClockGate, 0)
                     | Right g <- [gate] ]
                   | (_, osc, gate) <- ported_clks ]
        rst_ports = [ (getVNameString vn, 1, BviInput, KReset, 0)
                    | (_, vn) <- ported_rsts ]
        orst_ports = [ (getVNameString vn, 1, BviOutput, KReset, 0)
                     | (_, vn) <- out_rst_list ]
        oclk_ports = [ (getVNameString osc, 1, BviOutput, KClock, 0)
                     | (_, osc, _) <- out_clk_list ]
        carg_ports = [ (vpName vp, aTypeWidthI (aType e), BviInput,
                        KConstArg, 0)
                     | (vp, e) <- port_args ]

        aTypeWidthI (ATBit n) = n
        aTypeWidthI t = internalError ("SimBvi.aTypeWidthI: " ++ ppReadable t)

        -- per real method: (field, argports, en, results) with widths
        meth_port_details =
            [ let args = concat (vf_inputs f)
                  arg_tys = concat argTss
                  en = vf_enable f
                  outs = vf_outputs f
              in (f, zip args arg_tys, en, zip outs resTys)
            | (f, (argTss, _enTy, resTys)) <- real_fields ]

        meth_arg_group_refusals =
            [ "method '" ++ getIdBaseString (vf_name f) ++
              "' has a multi-port argument (not expressible in BVI)"
            | (f, (argTss, _, _)) <- real_fields
            , any ((/= 1) . length) (vf_inputs f) ||
              length (concat (vf_inputs f)) /= length (concat argTss) ]

        vpreg_refusals =
            [ "argument port '" ++ vpName vp ++ "' of method '" ++
              getIdBaseString (vf_name f) ++ "' is marked (*reg*)" ++
              " ((*reg*) input args are not supported)"
            | (f, aps, _, _) <- meth_port_details, (vp, _) <- aps
            , VPreg `elem` vpProps vp ]

        meth_ports =
            concat [ [ (vpName vp, aTypeWidthI t, BviInput, KMethodArg, 0)
                     | (vp, t) <- aps ] ++
                     [ (vpName vp, 1, BviInput, KEnable,
                        if VPinhigh `elem` vpProps vp then bviPropInhigh
                        else 0)
                     | Just vp <- [en] ] ++
                     [ (vpName vp, aTypeWidthI t, BviOutput, KMethodResult, 0)
                     | (vp, t) <- outs ]
                   | (_, aps, en, outs) <- meth_port_details ]

        rdy_ports = [ (vpName vp, 1, BviOutput, KRdy, 0)
                    | (f, _) <- rdy_fields
                    , vp <- take 1 (vf_outputs f) ]

        all_port_tuples =
            clk_ports ++ rst_ports ++ orst_ports ++ oclk_ports ++
            carg_ports ++ meth_ports ++ rdy_ports
        ports = [ BviPortI n w d k pr | (n, w, d, k, pr) <- all_port_tuples ]

        -- physical names must be unique: a shared output port is exactly
        -- the aliasing that hides undeclared cross-method paths; shared
        -- inputs are equally unreplayable (one shadow slot, two writers)
        alias_refusals =
            [ "physical port '" ++ n ++
              "' is shared by more than one role/method " ++
              "(aliased ports are not supported)"
            | (n, k) <- M.toList (M.fromListWith (+)
                            [ (bp_name p, 1 :: Int) | p <- ports ])
            , k > 1 ]

        port_idx = M.fromList (zip (map bp_name ports) [0 ..])
        idxOf n = M.findWithDefault
                    (internalError ("SimBvi.idxOf: " ++ n)) n port_idx

        -- ----------
        -- contract clocks/resets (indices resolved against the table)
        clocks = [ BviClockI (getIdBaseString i)
                             (idxOf (getVNameString osc))
                             (case gate of
                                Right g -> Just (idxOf (getVNameString g))
                                Left _  -> Nothing)
                             (tickNameFor i)
                 | (i, osc, gate) <- ported_clks ]
        clock_idx = M.fromList
            [ (getIdBaseString i, k)
            | ((i, _, _), k) <- zip ported_clks [0 ..] ]

        resets = [ BviResetI (getIdBaseString i) (idxOf (getVNameString vn))
                             True  -- bsc input resets are active-low
                 | (i, vn) <- ported_rsts ]

        -- (logical name, port index); the interpreter finds the parent
        -- module's derived-reset node by the "<inst>$<port>" wire name
        out_resets = [ (getIdBaseString i, idxOf (getVNameString vn))
                     | (i, vn) <- out_rst_list ]
        out_clocks = [ (getIdBaseString i, idxOf (getVNameString osc))
                     | (i, osc, _) <- out_clk_list ]

        const_args = [ (idxOf (vpName vp), v)
                     | (vp, e) <- port_args, Just v <- [constVal e] ]

        -- ----------
        -- methods
        sbr_pairs = [ (getIdBaseString a, getIdBaseString b) | (a, b) <- sSBR mci ]
        sb_pairs  = [ (getIdBaseString a, getIdBaseString b) | (a, b) <- sSB mci ]
        cf_self   = S.fromList [ getIdBaseString a
                               | (a, b) <- sCF mci, a == b ]
        sbr_self  = S.fromList [ a | (a, b) <- sbr_pairs, a == b ]
        ordered_before = S.fromList (sb_pairs ++ sbr_pairs)

        mkMethod (f, aps, en, outs) =
            let mname = getIdBaseString (vf_name f)
                kind = case (en, outs) of
                         (Just _, [])     -> MKAction
                         (Just _, _)      -> MKActionValue
                         (Nothing, _ : _) -> MKValue
                         (Nothing, [])    -> MKValue  -- refused below
                mclk = do c <- vf_clock f
                          M.lookup (getIdBaseString c) clock_idx
                self_sbr = mname `S.member` sbr_self
            in BviMethodI {
                   bm_name = mname,
                   bm_kind = kind,
                   bm_clock = mclk,
                   bm_args = [ idxOf (vpName vp) | (vp, _) <- aps ],
                   bm_results = [ idxOf (vpName vp) | (vp, _) <- outs ],
                   bm_enable = case en of
                                 Just vp -> Just (idxOf (vpName vp))
                                 Nothing -> Nothing,
                   bm_rdy = case M.lookup mname rdy_map of
                              Just rf -> Just (idxOf
                                          (vpName (head (vf_outputs rf))))
                              Nothing -> Nothing,
                   bm_self_sbr = self_sbr
                 }
        methods = map mkMethod meth_port_details

        method_refusals =
            [ "method '" ++ getIdBaseString (vf_name f) ++
              "' has neither outputs nor an enable"
            | (f, _, Nothing, []) <- meth_port_details ] ++
            [ "method '" ++ getIdBaseString (vf_name f) ++
              "' returns through " ++ show (length outs) ++
              " output ports (multi-port results are not supported yet)"
            | (f, _, _, outs) <- meth_port_details, length outs > 1 ] ++
            [ "Action/ActionValue method '" ++
              getIdBaseString (vf_name f) ++ "' is clockless " ++
              "(no edge exists to commit its effects)"
            | (f, _, Just _, _) <- meth_port_details
            , case vf_clock f of
                Nothing -> True
                Just c  -> let cn = getIdBaseString c
                           in  cn `elem` map getIdBaseString portless_clks
                               || not (M.member cn clock_idx) ] ++
            [ "self-CF Action method '" ++ getIdBaseString (vf_name f) ++
              "' with arguments (unordered coincident writers " ++
              "are not replayable)"
            | (f, aps, Just _, _) <- meth_port_details
            , not (null aps)
            , getIdBaseString (vf_name f) `S.member` cf_self ]

        -- ----------
        -- paths: declared clauses + synthesized implicit
        -- arg -> own-result paths of every value/ActionValue method
        VPathInfo declared = vPath vmi
        unknown_path_ports =
            nub [ getVNameString vn
                | (a, b) <- declared, vn <- [a, b]
                , not (M.member (getVNameString vn) port_idx) ]
        path_port_refusals =
            [ "path names port '" ++ n ++ "' which is not in the " ++
              "physical port table"
            | n <- unknown_path_ports ]

        declared_paths =
            [ (idxOf (getVNameString a), idxOf (getVNameString b))
            | (a, b) <- declared
            , M.member (getVNameString a) port_idx
            , M.member (getVNameString b) port_idx ]
        implicit_paths =
            nub [ (a, r) | m <- methods, bm_kind m /= MKAction
                         , a <- bm_args m, r <- bm_results m ]
        all_paths = nub (declared_paths ++ implicit_paths)

        -- port index -> (owning method name, role)
        owner_map :: M.Map Int (String, BviKindI)
        owner_map = M.fromList $ concat
            [ [ (a, (bm_name m, KMethodArg)) | a <- bm_args m ] ++
              [ (r, (bm_name m, KMethodResult)) | r <- bm_results m ] ++
              [ (e, (bm_name m, KEnable)) | Just e <- [bm_enable m] ] ++
              [ (r, (bm_name m, KRdy)) | Just r <- [bm_rdy m] ]
            | m <- methods ]
        kind_of i = bp_kind (ports !! i)
        name_of i = bp_name (ports !! i)
        meth_kind_of mn = head ([ bm_kind m | m <- methods, bm_name m == mn ]
                                ++ [MKValue])

        pathRefusal (src, dst) =
            let dst_owner = M.lookup dst owner_map
                src_owner = M.lookup src owner_map
            in case (kind_of src, dst_owner) of
                 (_, Nothing) ->
                     Just ("path targets output port '" ++ name_of dst ++
                           "' which no method owns")
                 (KClock, _) ->
                     Just ("path from clock oscillator port '" ++
                           name_of src ++ "' (transient mid-cycle levels " ++
                           "are not replayable)")
                 -- level inputs settled before any observation: no
                 -- ordering requirement
                 (KClockGate, _) -> Nothing
                 (KReset, _)     -> Nothing
                 (KConstArg, _)  -> Nothing
                 (_, Just (ym, _)) ->
                   case src_owner of
                     Nothing -> Just ("path from unowned input port '" ++
                                      name_of src ++ "'")
                     Just (xm, xrole)
                       | xm == ym -> Nothing
                       | xrole == KMethodArg && meth_kind_of xm == MKValue ->
                           Just ("path from value-method argument '" ++
                                 name_of src ++ "' (" ++ xm ++ ") to '" ++
                                 name_of dst ++ "' (" ++ ym ++ "): value " ++
                                 "arguments have no selection event and " ++
                                 "cannot source cross-method paths")
                       | (xm, ym) `S.member` ordered_before -> Nothing
                       | otherwise ->
                           Just ("path '" ++ name_of src ++ "' -> '" ++
                                 name_of dst ++ "': influencer method '" ++
                                 xm ++ "' is not scheduled SB/SBR before " ++
                                 "reader method '" ++ ym ++ "' (unordered " ++
                                 "or reversed paths are not replayable)")

        path_refusals = mapMaybe pathRefusal all_paths

        refusals = early_refusals ++ param_refusals ++
                   meth_arg_group_refusals ++ vpreg_refusals ++
                   alias_refusals ++ method_refusals ++
                   path_port_refusals ++ path_refusals

        sbr_av = [ vf_name f
                 | (f, _, Just _, _ : _) <- meth_port_details
                 , getIdBaseString (vf_name f) `S.member` sbr_self ]

    in if null refusals
       then Right (BviInfo {
                bi_verilog_name = modName,
                bi_ports = ports,
                bi_methods = methods,
                bi_clocks = clocks,
                bi_resets = resets,
                bi_out_resets = out_resets,
                bi_out_clocks = out_clocks,
                bi_params = params,
                bi_const_args = const_args,
                bi_paths = all_paths,
                bi_sbr_av = sbr_av })
       else Left refusals

-- ===============
-- Package-level checks (need the parent's schedule)

-- A USER foreign import: the Prelude's own primitives (RegN, FIFO2,
-- BRAM, ...) are also import-BVI instances with avi_user_import set,
-- but they are native trs prims, never Verilator contracts.
isBviImport :: AVInst -> Bool
isBviImport avi =
    avi_user_import avi &&
    not (isPrimitiveModule (getVNameString (vName (avi_vmi avi))))

spHasBvi :: SimPackage -> Bool
spHasBvi pkg = any isBviImport (M.elems (sp_state_instances pkg))

-- All BVI refusals for one package: contract-local ones from
-- 'deriveBvi' plus the self-SBR ActionValue atomic-read condition.
checkBviPackage :: SimPackage -> [EMsg]
checkBviPackage pkg =
    concat [ checkOne avi
           | avi <- M.elems (sp_state_instances pkg), isBviImport avi ]
  where
    asi = sp_schedule pkg
    edb = asi_exclusive_rules_db asi
    uses :: MethodUsesMap
    uses = asi_method_uses_map asi
    pos_map = M.fromList
        (zip [ i | Exec i <- asi_sched_order asi ] [(0 :: Int) ..])
    defmap = sp_local_defs pkg
    dyn_sched = not (null (asi_dyn_scheds asi))

    refuse avi tags =
        [ (getPosition (avi_vname avi),
           EGeneric ("trs cannot import Verilog module '" ++
                     getVNameString (vName (avi_vmi avi)) ++
                     "' (instance '" ++ getIdString (avi_vname avi) ++
                     "') via BVI:\n  " ++ intercalate "\n  " tags)) ]

    checkOne avi =
        case deriveBvi avi of
          Left tags -> refuse avi tags
          Right info ->
              case concatMap (checkSbrAv avi) (bi_sbr_av info) of
                [] -> []
                tags -> refuse avi tags

    -- rules of the parent, with their transitive def closure, that read
    -- the AV result of instance.meth
    consumers :: AId -> AId -> [AId]
    consumers inst meth =
        [ arule_id r | r <- sp_rules pkg, ruleReads r ]
      where
        hit e = [ () | (o, m) <- collectAVReads e
                     , getIdString o == getIdString inst
                     , getIdBaseString m == getIdBaseString meth ]
        defExpr i = M.lookup i defmap
        ruleReads r =
            let seed = nub (aVars r)
                closure seen [] = seen
                closure seen (i : rest)
                  | i `S.member` seen = closure seen rest
                  | otherwise =
                      case defExpr i of
                        Nothing -> closure seen rest
                        Just d -> closure (S.insert i seen)
                                          (nub (aVars d) ++ rest)
                reached = S.toList (closure S.empty seed)
                exprs = [ e | i <- reached
                            , Just (ADef _ _ e _) <- [defExpr i] ]
            in not (null (findAExprs hit r)) ||
               any (not . null . hit) exprs

    checkSbrAv avi meth =
        let inst = avi_vname avi
            mname = getIdBaseString meth
            -- every rule that CALLS the method (action use)
            callers = case [ us | (MethodId o m, us) <- M.toList uses
                                , getIdString o == getIdString inst
                                , getIdBaseString m == mname ] of
                        [] -> []
                        uss -> nub [ rid
                                   | us <- uss
                                   , (UUAction _, (_, body, _)) <- us
                                   , rid <- body ]
            cons = consumers inst meth
            posOf r = M.lookup r pos_map
            missing_pos = [ r | r <- callers ++ cons, posOf r == Nothing ]
            later_of r = [ r' | r' <- callers, r' /= r
                              , case (posOf r, posOf r') of
                                  (Just a, Just b) -> b > a
                                  _ -> False ]
            bad_pairs = [ (r, r') | r <- cons, r' <- later_of r
                                  , not (areRulesExclusive edb r r') ]
        in if null cons
           then []
           else if dyn_sched
           then [ "self-SBR ActionValue method '" ++ mname ++
                  "' has its result consumed under -sched-dynamic " ++
                  "(schedule-last is not a static fact there)" ]
           else
             [ "self-SBR ActionValue method '" ++ mname ++
               "' is called from '" ++ getIdString r ++
               "' which is not in the executed schedule order " ++
               "(cannot establish the atomic-read condition)"
             | r <- nub missing_pos ] ++
             [ "self-SBR ActionValue method '" ++ mname ++
               "': rule '" ++ getIdString r ++ "' consumes the result " ++
               "but rule '" ++ getIdString r' ++ "' can also call the " ++
               "method later in the same cycle -- the read is not " ++
               "atomic with the last call (make the rules mutually " ++
               "exclusive, or consume from the schedule-last caller)"
             | (r, r') <- bad_pairs ]
