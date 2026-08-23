module SimBlocksToC ( simBlocksToC
                    , SBMap
                    , mkSchedName
                    ) where

import Data.List(nub, genericLength, mapAccumL)
import Data.Maybe(catMaybes)
import qualified Data.Set as S
import Control.Monad.State(runState)
import System.Time -- XXX: in old-time package
import qualified Data.Map as M

import ErrorUtil(internalError)
import Flags
import Id(getIdString)
import IntLit(ilValue)
import SimCCBlock
import ASyntax
import ASyntaxUtil(exprForeignCalls,actionForeignCalls,aSize)
import ForeignFunctions
import FileNameUtil(mkCxxName, mkHName)
import CCSyntax
import VModInfo(vName_to_id)
import PPrint(ppReadable) -- hiding (int, char)
import Util(concatMapM, mapFst)
import SimFileUtils(codeGenOptionDescr)
import TopUtils(TimeInfo(..))
import Version(versionname)
import BuildVersion(buildVersion)

-- import Debug.Trace

-- Create many .cxx and .h files from the entire list of SimCCBlocks
-- and SimCCScheds.  The blocks are grouped by module, the schedules
-- cut across all modules.
simBlocksToC :: Flags -> TimeInfo -> SBId ->
                (Maybe String) -> (Maybe String) ->
                SBMap -> ForeignFuncMap ->
                [String] -> [SimCCBlock] -> [SimCCSched] ->
                [SimCCClockGroup] -> SimCCGateInfo ->
                (String -> String -> IO String) -> IO [String]
simBlocksToC flags time top_block def_clk def_rst
             sb_map ff_map reused mod_blocks scheds
             clk_groups gate_info writeFileC = do
    let wdef_mod_map =
            M.fromList
            [ (sb_id sb, wide_defs) |
                  sb <- mod_blocks,
                  let defs = sb_publicDefs sb ++ sb_privateDefs sb,
                  let ports = [ (t,vName_to_id vn) | (t,_,vn) <- sb_methodPorts sb ],
                  let wide_defs = map snd $ filter isWideDef (defs ++ ports) ]
        wdef_inst_map =
            M.fromList
            [ (inst, wide_defs) |
                  (inst, mod) <- mkInstanceMap sb_map top_block,
                  let wide_defs = M.findWithDefault [] mod wdef_mod_map ]

    let cvtModBlock = convertModuleBlock flags sb_map ff_map wdef_mod_map reused top_block
    module_names <- concatMapM (cvtModBlock writeFileC) mod_blocks
    schedule_names <- convertSchedules flags time top_block def_clk def_rst sb_map ff_map
                                       wdef_inst_map scheds clk_groups gate_info writeFileC
    return $ module_names ++ schedule_names

-- Given a top block Id, it makes a list of pairs of (inst,mod)
-- qualified with "top"
mkInstanceMap :: SBMap -> SBId -> [(String, SBId)]
mkInstanceMap sb_map i =
  let addInstQual i "" = i
      addInstQual i s  = i ++ "." ++ s
      mkMap :: SBId -> [(String, SBId)]
      mkMap i =
          case (M.lookup i sb_map) of
              Nothing -> [("",i)]
              Just sb ->
                  let mkSubs (mod, inst, _) =
                          mapFst (addInstQual (getIdString inst)) (mkMap mod)
                  in  [("",i)] ++ concatMap mkSubs (sb_state sb)
  in  mapFst (addInstQual "top") (mkMap i)

-- Test if a SimCCBlock or SimCCFn calls a foreign function
fnCallsForeignFn :: SimCCFn -> Bool
fnCallsForeignFn fn = (any makesForeignCall (sf_body fn))
  where makesForeignCall (SFSDef _ _ Nothing)      = False
        makesForeignCall (SFSDef _ _ (Just expr))  =
          not (null (exprForeignCalls expr))
        makesForeignCall (SFSAssign _ _ expr)    =
          not (null (exprForeignCalls expr))
        makesForeignCall (SFSAction act)         =
          not (null (actionForeignCalls act))
        makesForeignCall (SFSAssignAction _ _ act _) =
          not (null (actionForeignCalls act))
        makesForeignCall (SFSRuleExec _)         = False
        makesForeignCall (SFSCond expr ts fs)    =
          (not (null (exprForeignCalls expr))) ||
          (any makesForeignCall (ts ++ fs))
        makesForeignCall (SFSMethodCall _ _ args)  =
          not (null (concatMap exprForeignCalls args))
        makesForeignCall (SFSFunctionCall _ _ args) =
          not (null (concatMap exprForeignCalls args))
        makesForeignCall (SFSResets stmts)       =
          (any makesForeignCall stmts)
        makesForeignCall (SFSReturn Nothing)     = False
        makesForeignCall (SFSReturn (Just expr)) =
          not (null (exprForeignCalls expr))
        makesForeignCall (SFSOutputReset _ expr) =
          not (null (exprForeignCalls expr))

blockCallsForeignFn :: SimCCBlock -> Bool
blockCallsForeignFn block =
    (any fnCallsForeignFn (get_rule_fns block))   ||
    (any fnCallsForeignFn (get_method_fns block)) ||
    (any fnCallsForeignFn (sb_resets block))

modName :: SimCCBlock -> String
modName sb = fst (sb_naming_fn sb [])

schedCallsForeignFn :: SimCCSched -> Bool
schedCallsForeignFn sched = fnCallsForeignFn (sched_fn sched)

-- Convert the block for a module into .cxx and .h files
convertModuleBlock :: Flags -> SBMap -> ForeignFuncMap ->
                       M.Map SBId [AId] -> [String] -> SBId ->
                       (String -> String -> IO String)-> SimCCBlock ->
                       IO [String]
convertModuleBlock flags sb_map ff_map wdef_mod_map reused top_blk writeFileC sb = do
    let name = sb_name sb
        wide_defs = M.findWithDefault [] (sb_id sb) wdef_mod_map
        wdef_inst_map = M.fromList [("", wide_defs)]
        uses_foreign_fn = blockCallsForeignFn sb
        is_top = (sb_id sb) == top_blk

        -- list of subblocks that need to be included
        include_ids = nub (map (\(id,_,_)->id) (sb_state sb))

        -- class declaration (for the H file)
        class_decl = simCCBlockToClassDeclaration sb_map sb

        -- method definitions (for the CXX file)
        (method_defs, state) =
            runState (simCCBlockToClassDefinition sb_map sb)
                     (initialState ff_map wdef_inst_map (unSpecTo flags))
        lit_defs = mkLiteralDecls (nub (literals state))
        str_defs = mkStringDecls (M.toList (str_map state)) (str_objs state)
        class_defs = lit_defs ++ str_defs ++ method_defs
    if (name `elem` reused)
    then return [] -- don't generate any files for reused blocks
    else mkCxxAndH flags sb_map name uses_foreign_fn is_top
                   ( include_ids
                   , [class_decl]
                   , class_defs
                   )
                   writeFileC

-- Convert the schedule and reset functions into .cxx and .h files
convertSchedules :: Flags -> TimeInfo -> SBId ->
                    (Maybe String) -> (Maybe String) ->
                    SBMap -> ForeignFuncMap -> M.Map String [AId] ->
                    [SimCCSched] -> [SimCCClockGroup] -> SimCCGateInfo ->
                    (String -> String -> IO String) -> IO [String]
convertSchedules flags creation_time top_id def_clk def_rst sb_map ff_map
                 wdef_map scheds clk_groups gate_info writeFileC = do
    let ids      = []
        top_blk  = lookupSB sb_map top_id
        top_inst = (modName top_blk) ++ "_instance"

        model_includes = [ cpp_include "bs_model.h"
                         , cpp_include $ (modName top_blk) ++ ".h" ]

        -- declaration of the model class, named for the top module
        model_name = pfxModel ++ (modName top_blk)
        inst = mkVar ((modName top_blk) ++ "_instance")
        inst_decl = [ comment "Top-level module instance" $
                      private $
                      [ decl $ ptr . (moduleType top_blk []) $ inst ] ]
        sim_hdl_decl = [ comment "Handle to the simulation kernel" $
                         private $
                         [ decl $ userType "tSimStateHdl" $ mkVar "sim_hdl" ] ]
        ctor_decl = [ comment "Constructor" $
                      public $
                      [ decl $ ctor (mkVar model_name) [] ] ]
        kernel_fns_decl =
            [ comment "Functions required by the kernel" $
              public $
              [ decl $ function void (mkVar "create_model")
                           [ (userType "tSimStateHdl") (mkVar "simHdl")
                           , bool (mkVar "master") ]
              , decl $ function void (mkVar "destroy_model") []
              , decl $ function void (mkVar "reset_model")
                           [ bool (mkVar "asserted") ]
              , decl $ function void (mkVar "get_version")
                           [ (ptr . ptr . constant . char) (mkVar "name")
                           , (ptr . ptr . constant . char) (mkVar "build") ]
              , decl $ function (userType "time_t") (mkVar "get_creation_time") []
              , decl $ function (ptr . void) (mkVar "get_instance") []
              , decl $ function (userType "tUInt32")
                           (mkVar "get_max_event_queue_depth") []
              , decl $ function (userType "tUInt32")
                           (mkVar "get_num_state_elements") []
              , decl $ function (ptr . constant . (userType "tBkStateInfo"))
                           (mkVar "get_state_element")
                           [ (userType "tUInt32") (mkVar "n") ]
              , decl $ function (userType "tUInt64") (mkVar "get_state_bytes") []
              , decl $ function (userType "tUInt32")
                           (mkVar "get_num_input_ports") []
              , decl $ function (ptr . constant . (userType "tBkPortInfo"))
                           (mkVar "get_input_port")
                           [ (userType "tUInt32") (mkVar "n") ]
              , decl $ function (userType "tUInt64") (mkVar "get_input_bytes") []
              , decl $ function (userType "tUInt32")
                           (mkVar "get_num_output_ports") []
              , decl $ function (ptr . constant . (userType "tBkPortInfo"))
                           (mkVar "get_output_port")
                           [ (userType "tUInt32") (mkVar "n") ]
              , decl $ function (userType "tUInt64") (mkVar "get_output_bytes") []
              ]
            ]
        class_decl =
            [ comment ("Class declaration for a model of " ++
                       modName top_blk) $
              c_class model_name (Just "Model") $
                      concat [ inst_decl
                             , sim_hdl_decl
                             , ctor_decl
                             , kernel_fns_decl
                             ]
            ]

        -- abstract the model constructor
        new_fn_proto =
            function (ptr . void) (mkVar ("new_" ++ model_name)) []
        new_fn_decl =
            [ comment "Function for creating a new model" $
              externC [decl $ new_fn_proto]
            ]

        new_fn_def =
            [ comment "Function for creating a new model" $
              define new_fn_proto
                (block
                 [ decl $ (ptr . userType model_name) $
                            (mkVar "model") `assign`
                                (new (classType model_name) (Just []))
                 , ret $ Just (cCast (ptrType voidType) (var "model")) ]
                 )
            ]

        -- model constructor
        mkScopedVar s = mkVar (model_name ++ "::" ++ s)
        ctor_def = [ comment "Constructor" $
                     define (ctor (mkScopedVar model_name) [])
                            (block $ [ inst `assign` mkNULL ])
                   ]

        -- definitions of the schedule functions
        (sch_fn_lists,state) =
            runState (mapM (simCCScheduleToFunctionDefinition top_blk) scheds)
                     (initialState ff_map wdef_map (unSpecTo flags))
        sched_fns = [comment "Schedule functions" (blankLines 1)] ++
                    concat sch_fn_lists

        -- wide literals used in the methods
        meth_lits = mkLiteralDecls (nub (literals state))

        str_lits = mkStringDecls (M.toList (str_map state)) (str_objs state)

        -- include files needed for kernel callbacks, etc.
        uses_foreign = any schedCallsForeignFn scheds
        kernel_includes =
                  [ cpp_system_include "cstdlib"
                  , cpp_system_include "time.h"
                  , cpp_include "bluesim_kernel_api.h"
                  , cpp_include "bs_reset.h"
                  , blankLines 1 ]

        -- calls for declaring clocks
        declare_clk_name grp =
          let name = mkClkName (aclock_osc (grp_canonical grp))
          in  stmt $ (var "bk_get_or_define_clock") `cCall` [ var "sim_hdl", mkStr name ]

        -- calls for setting up clock waveforms
        set_waveform clk_name init_val has_init first_edge hi lo =
          let val_enum = if (init_val /= (0 :: Integer))
                         then var "CLK_HIGH"
                         else var "CLK_LOW"
          in stmt $ (var "bk_alter_clock") `cCall`
                      [ var "sim_hdl"
                      , (var "bk_get_clock_by_name") `cCall` [ var "sim_hdl", mkStr clk_name ]
                      , val_enum
                      , mkBool has_init
                      , mkUInt64 first_edge
                      , mkUInt64 lo
                      , mkUInt64 hi
                      ]

        -- calls for setting up initial reset
        setup_reset = stmt $ (var "bk_use_default_reset") `cCall` [ var "sim_hdl" ]

        -- calls for setting clock names
        set_clk_name grp =
          let name = mkClkName (aclock_osc (grp_canonical grp))
          in map (helper name) (grp_instances grp)
          where helper nm (aid,dom) =
                  let fn   = mkSetClkFnName dom
                      -- given an Id which has an empty base and possibly an
                      -- instance for the qualifier, make the instance name
                      s = concatMap (++".") (fst (adjustInstQuals aid))
                      meth = if (null s) then fn else s ++ fn
                  in stmt $ (var top_inst) `cArrow` meth `cCall` [ mkStr nm ]

        -- calls for registering clock schedules
        -- Note: the argument list must match the
        -- declaration in sim/bluesim_kernel_api.h and
        -- implementation in sim/kernel.cxx
        register_clk sched =
          let clk        = sched_clock sched
              clk_name   = mkClkName clk
              is_posedge = sched_posedge sched
              dir_enum   = if is_posedge
                           then var "POSEDGE"
                           else var "NEGEDGE"
              after_edge_sched =
                  case (sched_after_fn sched) of
                    Nothing -> mkNULL
                    (Just f) -> var (mkSchedName (clk, is_posedge, True))
          in stmt $ (var "bk_set_clock_event_fn") `cCall`
                      [ var "sim_hdl"
                      , (var "bk_get_clock_by_name") `cCall` [ var "sim_hdl", mkStr clk_name ]
                      , (var (mkSchedName (clk, is_posedge, False)))
                      , after_edge_sched
                      , (classType "tEdgeDirection") `cCast` dir_enum
                      ]

        -- calls for setting clock gate pointers
        set_gate_ptr (inst, ginfo) =
            map (uncurry (mkGateAssign top_inst inst)) ginfo

        gate_lits =
            let any_true = any (any ((== (Left True)) . snd) . snd) gate_info
                any_false = any (any ((== (Left False)) . snd) . snd) gate_info
            in
                if (any_true || any_false)
                then [ comment "Constant gate declarations" (blankLines 0)]
                     ++ (if any_true then [mkGateConst True] else [])
                     ++ (if any_false then [mkGateConst False] else [])
                else []

        -- ----------------
        -- The design's maximum event-queue depth: an upper bound on the
        -- number of events the generated model can have live in the
        -- kernel's event queue at any one time, ASSUMING NO HOST CALLS
        -- THAT ENQUEUE EVENTS (bk_quit_at, host-scheduled UI events,
        -- host-triggered clock edges, cycle dumping, host-defined
        -- clocks).  It is exposed through the Model virtual
        -- get_max_event_queue_depth() and the bk_max_event_queue_depth()
        -- kernel accessor, so that an embedder can size the fixed
        -- event-queue capacity it passes to bk_sync_init().
        --
        -- The formula, derived from the enqueue sites in the kernel and
        -- the Bluesim primitives (kernel.cxx, reset.cxx,
        -- bs_prim_mod_clockgen.h, bs_prim_mod_clockmux.h,
        -- bs_prim_mod_resets.h):
        --
        --   max = 3 + C*(5 + 2*C) + 6*R
        --
        -- where C = number of clock domains the model registers (one
        -- per SimCCClockGroup, each declared with bk_get_or_define_clock
        -- in create_model) and R = number of primitive instances whose
        -- Bluesim implementation schedules deferred reset events
        -- (reset_init / reset_at_end_of_timeslice).
        --
        -- Per-source worst-case live-event counts:
        --
        --  * 2: the default reset waveform (bk_use_default_reset from
        --    create_model when master): one assert event at time 0 and
        --    one deassert event at time 2, both non-recurring.
        --
        --  * 1: the UI yield event.  $stop/$finish/$fatal and a reached
        --    bk_quit_after_edge limit schedule one yield event at the
        --    current time; it is deduplicated per target time, so the
        --    model on its own never holds more than one.
        --
        --  * 5 + 2*C per clock:
        --      - A clock with a periodic waveform holds at most 5 live
        --        events (setup_clock_edges): posedge + negedge events,
        --        their two post-edge combinational events, and possibly
        --        a time-0 initial edge.  Recurring events are re-added
        --        only after being popped, so re-scheduling is net zero.
        --      - A primitive-driven (aperiodic) clock (MakeClock,
        --        ClockInverter, ClockDivider, ClockSelect, ClockMux)
        --        holds at most 1 initial edge event
        --        (bk_enqueue_initial_clock_edge) plus 2 events per
        --        pending bk_trigger_clock_edge pair.  Pending pairs for
        --        one clock alternate direction (a primitive only
        --        triggers an edge that changes its recorded clock
        --        value), so a new pair requires one more edge execution
        --        of the DRIVING clock at the same instant before the
        --        pending pairs run.  A clock executes at most
        --        1 + (its own pending pairs) edge events at one
        --        instant, so along a chain of derived clocks the
        --        pending-pair bound grows by at most 1 per level; with
        --        at most C clocks in a chain and a root holding at most
        --        2 pending-pair-equivalents (a periodic clock's edge
        --        events at one instant, degenerate zero-width phases
        --        included), pending pairs per clock are bounded by
        --        2 + C, giving 1 + 2*(2 + C) = 5 + 2*C live events.
        --        Since 5 <= 5 + 2*C, this uniform per-clock bound
        --        covers both kinds.
        --
        --  * 6 per reset-scheduling primitive instance: deferred reset
        --    events are scheduled at the current instant
        --    (PG_INITIAL/PG_AFTER_LOGIC) and consumed within it; a
        --    primitive schedules at most one event per entry-point
        --    invocation, and the largest primitive (ClockSelect) has 5
        --    entry points that can schedule in one instant (aClk, bClk,
        --    xclk ticks, the select method and its reset), rounded up
        --    to 6 for margin (e.g. InitialReset's create_model-time
        --    reset_init coinciding with its first deassert).
        --
        -- VCD and state-dump events no longer exist in this runtime,
        -- and generated module code itself never schedules events; the
        -- above sources are exhaustive.
        num_clocks = genericLength clk_groups :: Integer
        prim_name_map = M.fromList [ (sb_id pb, sb_name pb) | pb <- primBlocks ]
        reset_event_prims = [ "MakeReset", "MakeResetA", "MakeReset0"
                            , "SyncReset", "SyncResetA", "InitialReset"
                            , "ResetMux", "ClockSelect", "UngatedClockSelect"
                            ]
        num_reset_prims =
            genericLength [ ()
                          | (_, mid) <- mkInstanceMap sb_map top_id
                          , maybe False (`elem` reset_event_prims)
                                  (M.lookup mid prim_name_map)
                          ] :: Integer
        max_event_queue_depth =
            3 + (num_clocks * (5 + 2 * num_clocks)) + (6 * num_reset_prims)

        get_max_depth_decl = function (userType "tUInt32")
                                 (mkScopedVar "get_max_event_queue_depth") []
        gmqd_def =
          define get_max_depth_decl
                 (comment ("3 (reset waveform + UI yield) + " ++
                           (show num_clocks) ++ " clocks * (5 + 2*" ++
                           (show num_clocks) ++ ") + " ++
                           (show num_reset_prims) ++
                           " reset primitives * 6; see SimBlocksToC")
                          (ret (Just (mkUInt32 max_event_queue_depth))))

        -- ----------------
        -- Non-allocating introspection: static descriptor tables for
        -- the design's state elements and the top module's input and
        -- output ports, with a flat layout (byte offsets in planned
        -- contiguous areas).  The tables are 'static const' data --
        -- walking them through the Model virtuals (and the bk_*
        -- kernel accessors over them) allocates nothing and works
        -- before create_model().  The ordering, alignment and layout
        -- contract is documented in bluesim_introspection.h; the
        -- collection and layout code is at the bottom of this file.
        state_elems = collectStateElements sb_map top_id
        (state_places, state_bytes) =
            layoutArea [ (b, e) | (_, _, b, e) <- state_elems ]
        state_rows =
            [ mkInitBraces [ mkStr nm
                           , var (stateKindCName k)
                           , mkUInt32 b
                           , mkUInt64 e
                           , mkUInt64 off
                           , mkUInt64 sz
                           ]
            | ((nm, k, b, e), (off, sz)) <- zip state_elems state_places ]

        ifc_ports    = sb_ifcPorts top_blk
        input_ports  = [ (nm, aSize t) | (True,  t, nm) <- ifc_ports ]
        output_ports = [ (nm, aSize t) | (False, t, nm) <- ifc_ports ]
        (input_places, input_bytes) =
            layoutArea [ (b, 1) | (_, b) <- input_ports ]
        (output_places, output_bytes) =
            layoutArea [ (b, 1) | (_, b) <- output_ports ]
        port_rows ports places =
            [ mkInitBraces [ mkStr nm
                           , mkUInt32 b
                           , mkUInt64 off
                           , mkUInt64 sz
                           ]
            | ((nm, b), (off, sz)) <- zip ports places ]

        desc_table ty nm rows =
            -- C++ has no zero-length arrays; an empty table is
            -- simply not emitted and its walkers return NULL/0
            if null rows
            then []
            else [ static $ constant . array . (userType ty) $
                       (mkVar nm) `assign` (mkInitBraces rows) ]
        desc_tables =
            (desc_table "tBkStateInfo" "bk_state_elements" state_rows) ++
            (desc_table "tBkPortInfo" "bk_input_ports" (port_rows input_ports input_places)) ++
            (desc_table "tBkPortInfo" "bk_output_ports" (port_rows output_ports output_places))
        desc_defs =
            if (null desc_tables)
            then []
            else [ comment ("Introspection descriptor tables " ++
                            "(static; see bluesim_introspection.h)")
                           (blankLines 0) ] ++ desc_tables

        mk_num_def fn_name n =
            define (function (userType "tUInt32") (mkScopedVar fn_name) [])
                   (block [ ret (Just (mkUInt32 n)) ])
        mk_bytes_def fn_name n =
            define (function (userType "tUInt64") (mkScopedVar fn_name) [])
                   (block [ ret (Just (mkUInt64 n)) ])
        mk_elem_def fn_name ty arr n =
            define (function (ptr . constant . (userType ty))
                             (mkScopedVar fn_name)
                             [ (userType "tUInt32") (mkVar "n") ])
                   (block (if (n == (0 :: Integer))
                           then [ ret (Just mkNULL) ]
                           else [ if_cond ((var "n") `cGe` (mkUInt32 n))
                                          (ret (Just mkNULL))
                                          Nothing
                                , ret (Just (cAddr (cIndex (var arr)
                                                           (var "n"))))
                                ]))
        num_state_elems  = genericLength state_elems
        num_input_ports  = genericLength input_ports
        num_output_ports = genericLength output_ports
        introspect_methods =
            [ comment ("State element and top-module port introspection " ++
                       "(see bluesim_introspection.h)")
                      (blankLines 0)
            , mk_num_def "get_num_state_elements" num_state_elems
            , mk_elem_def "get_state_element" "tBkStateInfo"
                          "bk_state_elements" num_state_elems
            , mk_bytes_def "get_state_bytes" state_bytes
            , mk_num_def "get_num_input_ports" num_input_ports
            , mk_elem_def "get_input_port" "tBkPortInfo"
                          "bk_input_ports" num_input_ports
            , mk_bytes_def "get_input_bytes" input_bytes
            , mk_num_def "get_num_output_ports" num_output_ports
            , mk_elem_def "get_output_port" "tBkPortInfo"
                          "bk_output_ports" num_output_ports
            , mk_bytes_def "get_output_bytes" output_bytes
            ]

        -- functions for creating, destroying and resetting the model
        create_model_decl  = function void (mkScopedVar "create_model")
                                 [ (userType "tSimStateHdl") (mkVar "simHdl")
                                 , bool (mkVar "master") ]
        destroy_model_decl = function void (mkScopedVar "destroy_model") []
        reset_model_decl   = function void (mkScopedVar "reset_model")
                                 [ bool $ mkVar "asserted" ]
        get_instance_decl  = function (ptr . void) (mkScopedVar "get_instance") []

        newInst sb   = let inst = mkVar ((modName sb) ++ "_instance")
                           new_expr = new (classType (pfxMod ++ (modName sb)))
                                          (Just [var "sim_hdl", mkStr "top", mkNULL])
                       in inst `assign` new_expr
        deleteInst sb = let nm = (modName sb) ++ "_instance"
                        in [ stmt $ delete (var nm)
                           , (mkVar nm) `assign` mkNULL
                           ]
        resetInst sb = let nm = (modName sb) ++ "_instance"
                           arg = cTernary (var "asserted") (mkBit 0) (mkBit 1)
                       in [ stmt $ ((var nm) `cArrow` meth) `cCall` [arg]
                          | rst <- sb_inputResets sb
                          , let meth = mkResetFnName rst
                          ]

        setup_clk_and_rst = (case def_clk of
                               (Just name) -> [ set_waveform name
                                                             0
                                                             False
                                                             0
                                                             5
                                                             5
                                              ]
                               Nothing     -> []) ++
                            (case def_rst of
                               (Just _) -> [ setup_reset ]
                               Nothing  -> [])
        create_model_def =
          define create_model_decl
                 (block $ -- record the sim state handle
                          [ (mkVar "sim_hdl") `assign` (var "simHdl") ] ++
                          -- clear reset counters
                          [ stmt $ (var "init_reset_request_counters") `cCall`
                                     [ var "sim_hdl" ] ] ++
                          -- allocate top module instance
                          [ newInst top_blk ] ++
                          -- declare clock names (which creates handles)
                          (map declare_clk_name clk_groups) ++
                          -- if master, setup default clock and reset
                          [ if_cond (var "master")
                                    (block setup_clk_and_rst)
                                    Nothing ] ++
                          -- register schedule callbacks
                          (map register_clk scheds) ++
                          -- tell mods what their clocks are
                          (concatMap set_clk_name clk_groups) ++
                          -- tell mods what their clock gates are
                          concatMap set_gate_ptr gate_info
                 )

        destroy_model_def =
          define destroy_model_decl
                 (block $ -- delete top module instance
                          (deleteInst top_blk)
                 )

        reset_model_def =
          define reset_model_decl
                 (block $ -- call reset functions for top module instance
                          (resetInst top_blk)
                 )

        get_instance_def =
          let inst_name = (modName top_blk) ++ "_instance"
          in define get_instance_decl
                    (block $ -- return pointer to model instance
                             [ret (Just (var inst_name))]
                    )

        model_methods = [comment "Model creation/destruction functions" (blankLines 1)] ++
                        [ create_model_def
                        , destroy_model_def
                        , reset_model_def
                        , get_instance_def
                        , comment "Maximum event-queue depth assuming no host calls that enqueue events" gmqd_def
                        ] ++ introspect_methods


        -- functions for getting the version information and creation time
        get_version = function void
                               (mkScopedVar "get_version")
                               [ ptr . ptr . constant . char $ (mkVar "name")
                               , ptr . ptr . constant . char $ (mkVar "build")
                               ]
        mk_version_str s = let vs = if showVersion flags then s else ""
                           in  if vs == "" then mkNULL else mkStr vs
        model_version_name  = mk_version_str versionname
        model_version_build = mk_version_str buildVersion
        gv_def =
          define get_version
                 (block [ stmt (cDeref (var "name"))  `assign` model_version_name
                        , stmt (cDeref (var "build")) `assign` model_version_build
                        ])

        get_creation_time = function (userType "time_t")
                                     (mkScopedVar "get_creation_time")
                                     []
        (TimeInfo _ clock_time@(TOD t _)) = if (timeStamps flags)
                                            then creation_time
                                            else TimeInfo 0 (TOD 0 0)
        time_str = calendarTimeToString (toUTCTime clock_time)
        gct_def = define get_creation_time
                         (comment time_str (ret (Just (mkUInt64 t))))
        version_methods = [ comment "Fill in version numbers" gv_def
                          , comment "Get the model creation time" gct_def
                          ]

        fname = "model_" ++ (modName top_blk)

    mkCxxAndH flags sb_map fname uses_foreign False
              ( ids
              , (model_includes ++ class_decl ++ new_fn_decl)
              , (kernel_includes ++
                 meth_lits ++
                 str_lits ++
                 gate_lits ++
                 desc_defs ++
                 ctor_def ++
                 new_fn_def ++
                 sched_fns ++
                 model_methods ++
                 version_methods
                )
              )
              writeFileC

-- ----------------
-- Non-allocating introspection descriptors
--
-- The generated model carries three 'static const' descriptor
-- tables: the design's state elements (the Bluesim primitive
-- instances of the whole module tree, with dotted instance names)
-- and the top module's input and output ports.  Along with the
-- descriptors the code generator defines a flat layout, assigning
-- each element a byte offset in a planned contiguous area (one area
-- for state, one for inputs, one for outputs).  The ordering,
-- alignment and layout rules are the documented contract in
-- src/bluesim/bluesim_introspection.h; keep this code and that
-- header in sync.

-- The kind of a state element: which Bluesim primitive family
-- implements it.  Mirrors tBkStateKind in bluesim_introspection.h.
data StateKind = SkReg | SkWire | SkRegFile | SkBRAM | SkFifo
               | SkProbe | SkCounter | SkSync | SkClock | SkReset
  deriving (Eq)

stateKindCName :: StateKind -> String
stateKindCName SkReg     = "BK_STATE_REG"
stateKindCName SkWire    = "BK_STATE_WIRE"
stateKindCName SkRegFile = "BK_STATE_REGFILE"
stateKindCName SkBRAM    = "BK_STATE_BRAM"
stateKindCName SkFifo    = "BK_STATE_FIFO"
stateKindCName SkProbe   = "BK_STATE_PROBE"
stateKindCName SkCounter = "BK_STATE_COUNTER"
stateKindCName SkSync    = "BK_STATE_SYNC"
stateKindCName SkClock   = "BK_STATE_CLOCK"
stateKindCName SkReset   = "BK_STATE_RESET"

-- One state element: dotted instance name, kind, bit width of one
-- entry, number of entries.
type StateElem = (String, StateKind, Integer, Integer)

-- Enumerate the state elements of the design: a depth-first
-- pre-order walk of the module instance tree from the top block,
-- taking the sub-instances of each module in sb_state order (the
-- code generator records them alphabetically by instance name).
-- Only Bluesim primitive instances are state elements; a generated
-- submodule contributes its subtree at its position.  Names are
-- dotted instance names rooted at "top", matching the runtime name
-- of the top-module instance.
collectStateElements :: SBMap -> SBId -> [StateElem]
collectStateElements sb_map top_id =
  let prim_map = M.fromList [ (sb_id pb, pb) | pb <- primBlocks ]
      -- note: the sb_map handed to simBlocksToC contains the
      -- primitive blocks too (with an empty sb_state), so the
      -- primitive test must come first
      walk pfx sbid args =
        case (M.lookup sbid prim_map) of
          Just pb -> [ classifyPrim pfx pb args ]
          Nothing ->
            case (M.lookup sbid sb_map) of
              Just sb -> concat [ walk (pfx ++ "." ++ getIdString inst)
                                       sub_id sub_args
                                | (sub_id, inst, sub_args) <- sb_state sb ]
              -- neither a generated module nor a Bluesim primitive
              -- (e.g. a noinline function): no state to describe
              Nothing -> []
  in  walk "top" top_id []

-- Classify one primitive instance: map its BSV primitive name to a
-- state kind and extract its geometry (entry bit width and entry
-- count) from its instantiation arguments.  Width and geometry
-- arguments are always compile-time literals (the naming functions
-- in SimPrimitiveModules already require this for widths).
classifyPrim :: String -> SimCCBlock -> [AExpr] -> StateElem
classifyPrim inst pb args =
  let prim = sb_name pb
      -- the naming fn normalizes the argument list to the C++
      -- constructor's shape; for every sized primitive the entry
      -- width is then the first argument, and for the FIFO families
      -- the depth is the second
      norm_args = snd (sb_naming_fn pb args)
      geomNat what e =
        case e of
          (ASInt _ _ il) -> ilValue il
          _ -> internalError ("SimBlocksToC.classifyPrim: non-constant " ++
                              what ++ " argument of " ++ prim ++
                              " instance " ++ inst ++ ": " ++ ppReadable e)
      argNat what as n =
        case (drop n as) of
          (e:_) -> geomNat what e
          []    -> internalError ("SimBlocksToC.classifyPrim: missing " ++
                                  what ++ " argument of " ++ prim ++
                                  " instance " ++ inst)
      width    = argNat "width" norm_args 0
      -- raw argument positions (Verilog parameter order, clocks and
      -- resets already dropped) for the memory primitives
      dataW  n = argNat "data width" args n
      loA    n = argNat "low address" args n
      hiA    n = argNat "high address" args n
      addrW  n = argNat "address width" args n
      memSz  n = argNat "memory size" args n
      reg_prims   = [ "RegN", "RegUN", "RegA"
                    , "CRegN5", "CRegUN5", "CRegA5"
                    , "CrossingRegN", "CrossingRegUN", "CrossingRegA"
                    , "ConfigRegN", "ConfigRegUN", "ConfigRegA"
                    , "RegTwoN", "RegTwoUN", "RegTwoA"
                    , "RevertReg", "RegAligned"
                    ]
      wire_prims  = [ "RWire", "RWire0", "BypassWire", "BypassWire0"
                    , "CrossingBypassWire", "BypassCrossingWire"
                    ]
      fifo_prims  = [ "FIFO1", "FIFO10", "FIFO2", "FIFO20"
                    , "SizedFIFO", "SizedFIFO0"
                    , "FIFOL1", "FIFOL10", "FIFOL2", "FIFOL20"
                    , "SizedFIFOL", "SizedFIFOL0"
                    , "SyncFIFO", "SyncFIFO0", "SyncFIFO1", "SyncFIFO10"
                    , "SyncFIFOLevel", "SyncFIFOLevel0"
                    ]
      probe_prims = [ "Probe", "ProbeWire" ]
      sync1_prims = [ "SyncBit05", "SyncBit1", "SyncBit15", "SyncBit"
                    , "SyncPulse", "SyncHandshake"
                    ]
      syncN_prims = [ "SyncRegister", "LatchCrossingReg" ]
      clock_prims = [ "ClockGen", "MakeClock", "GatedClock"
                    , "ClockInverter", "GatedClockInverter"
                    , "ClockDiv", "GatedClockDiv"
                    , "ClockSelect", "UngatedClockSelect"
                    , "ClockMux", "UngatedClockMux"
                    ]
      reset_prims = [ "MakeReset", "MakeResetA", "MakeReset0"
                    , "SyncReset", "SyncResetA", "SyncReset0"
                    , "InitialReset", "ResetMux", "ResetEither"
                    , "ResetToBool"
                    ]
      (kind, bits, entries)
        | prim `elem` reg_prims   = (SkReg,     width, 1)
        | prim `elem` wire_prims  = (SkWire,    width, 1)
        | prim `elem` fifo_prims  = (SkFifo,    width,
                                     argNat "depth" norm_args 1)
        | prim `elem` probe_prims = (SkProbe,   width, 1)
        | prim == "Counter"       = (SkCounter, width, 1)
        | prim `elem` sync1_prims = (SkSync,    1,     1)
        | prim `elem` syncN_prims = (SkSync,    width, 1)
        | prim `elem` clock_prims = (SkClock,   1,     1)
        | prim `elem` reset_prims = (SkReset,   1,     1)
        -- RegFile: ADDR_WIDTH, DATA_WIDTH, LO, HI
        | prim == "RegFile"       = (SkRegFile, dataW 1, (hiA 3) - (loA 2) + 1)
        -- RegFileLoad: FILE, ADDR_WIDTH, DATA_WIDTH, LO, HI, BINARY
        | prim == "RegFileLoad"   = (SkRegFile, dataW 2, (hiA 4) - (loA 3) + 1)
        -- DualPortRam: ADDR_WIDTH, DATA_WIDTH (2^ADDR_WIDTH entries)
        | prim == "DualPortRam"   = (SkRegFile, dataW 1, 2 ^ (addrW 0))
        -- BRAM1/BRAM2: PIPELINED, ADDR_WIDTH, DATA_WIDTH, MEMSIZE
        | prim `elem` ["BRAM1","BRAM2"] = (SkBRAM, dataW 2, memSz 3)
        -- BRAM1Load/BRAM2Load: FILE, PIPELINED, ADDR_WIDTH,
        --                      DATA_WIDTH, MEMSIZE, BINARY
        | prim `elem` ["BRAM1Load","BRAM2Load"] = (SkBRAM, dataW 3, memSz 4)
        -- BRAM1BE/BRAM2BE: PIPELINED, ADDR_WIDTH, DATA_WIDTH,
        --                  CHUNKSIZE, WE_WIDTH, MEMSIZE
        | prim `elem` ["BRAM1BE","BRAM2BE"] = (SkBRAM, dataW 2, memSz 5)
        -- BRAM1BELoad/BRAM2BELoad: FILE, PIPELINED, ADDR_WIDTH,
        --                          DATA_WIDTH, CHUNKSIZE, WE_WIDTH,
        --                          MEMSIZE, BINARY
        | prim `elem` ["BRAM1BELoad","BRAM2BELoad"] = (SkBRAM, dataW 3, memSz 6)
        | otherwise =
            internalError ("SimBlocksToC.classifyPrim: unclassified " ++
                           "Bluesim primitive '" ++ prim ++ "' (instance " ++
                           inst ++ "); assign it a tBkStateKind here")
  in  (inst, kind, bits, entries)

-- The storage unit (in bytes) of one entry of a given bit width, and
-- its required alignment.  These are the documented rules of
-- bluesim_introspection.h: 1/4/8 bytes for up to 8/32/64 bits (as
-- tUInt8/tUInt32/tUInt64), and a 4-byte-aligned array of 32-bit
-- words for wide data.
entryUnitBytes :: Integer -> Integer
entryUnitBytes b | b <= 8    = 1
                 | b <= 32   = 4
                 | b <= 64   = 8
                 | otherwise = 4 * ((b + 31) `div` 32)

entryAlignBytes :: Integer -> Integer
entryAlignBytes b | b <= 8    = 1
                  | b <= 32   = 4
                  | b <= 64   = 8
                  | otherwise = 4

-- Lay out one area: walk the elements (entry bit width, entry count)
-- in table order with a running offset starting at 0, rounding up to
-- each element's alignment and advancing by its size.  Returns the
-- (offset, size) of each element and the total area size, which is
-- the final offset rounded up to a multiple of 8 so the area itself
-- can be placed at any 8-byte-aligned address.
layoutArea :: [(Integer, Integer)] -> ([(Integer, Integer)], Integer)
layoutArea elems =
  let alignUp x a = ((x + a - 1) `div` a) * a
      step off (b, e) =
        let off' = alignUp off (entryAlignBytes b)
            sz   = e * (entryUnitBytes b)
        in  (off' + sz, (off', sz))
      (end, places) = mapAccumL step 0 elems
  in  (places, alignUp end 8)

-- Some literals cannot be written inline in the generated C, so they are
-- declared as separate variables at the beginning of the file.
mkLiteralDecls :: [(ASize,Integer)] -> [CCFragment]
mkLiteralDecls [] = [blankLines 0]
mkLiteralDecls lits = [comment "Literal declarations" (blankLines 0)]
                      ++ (concatMap mkLitDecl lits)
                      ++ [blankLines 1]
  where mkLitDecl (sz,val) =
           let name = mkLiteralName sz val
               arr_name = name ++ "_arr";
               arr_words = [ mkUInt32 ((val `div` (2^n))
                                            `mod` (2^(32::Integer)))
                           | n <- [0,32..(sz-1)] ]
               initializer = mkInitBraces arr_words
               arr_decl = constant . array . unsigned . int $
                            (mkVar arr_name) `assign` initializer
               lit_var = constant $
                            (mkVar name) `ofType` (bitsType 65 CTunsigned)
               lit = construct lit_var [mkUInt32 sz, var arr_name]
           in [static $ arr_decl, static $ lit]


-- String literals are declared as plain file-scope char arrays:
-- constant-initialized data, so loading a model runs no global
-- constructors and makes no allocator calls for them.  They may have
-- embedded null characters, so consumers do not measure them with
-- strlen(): the generated calls carry the length in their argument
-- descriptor string or pass it explicitly.  A literal used as a
-- string value additionally gets a tStr leaf object (see bs_str.h)
-- carrying the array and its byte count; its constexpr constructor
-- makes it constant-initialized as well.
mkStringDecls :: [(String,Integer)] -> S.Set String -> [CCFragment]
mkStringDecls [] _ = [blankLines 0]
mkStringDecls lits objs = [comment "String declarations" (blankLines 0)]
                           ++ (concatMap mkStrDecl lits)
                           ++ [blankLines 1]
  where mkStrDecl (s,n) =
           let name = mkStringLiteralName n
               str_var = constant . array . CCSyntax.char $
                           (mkVar name) `assign` (mkStr s)
               obj = construct (constant $ (userType "tStr")
                                    (mkVar (mkStringObjName n)))
                               [ var name
                               , mkUInt32 (genericLength s) ]
           in (static $ str_var) :
              (if (s `S.member` objs) then [static $ obj] else [])

-- Create one .cxx and one .h file, given a list of
-- referenced blocks, class declarations and method definitions.
mkCxxAndH :: Flags -> SBMap -> String -> Bool -> Bool ->
             ([SBId],[CCFragment],[CCFragment]) ->
             (String -> String -> IO String) -> IO [String]
mkCxxAndH flags sb_map name include_foreign is_top (ids,decls,meths) writeFileC = do
  let c_file_name = mkCxxName Nothing "" name
      h_file_name = mkHName Nothing "" name
      c_includes  = [ cpp_include "bluesim_primitives.h"
                    , cpp_include h_file_name ]
      foreign_includes = if include_foreign
                         then [cpp_include "imported_BDPI_functions.h"]
                         else []
      state_files = (catMaybes (nub (map (idToHFile sb_map) ids)))
      h_includes  = [ cpp_include "bluesim_types.h"
                    , cpp_include "bs_module.h"
                    , cpp_include "bluesim_primitives.h"
                    ] ++
                    (map cpp_include state_files)
      c_fragments = c_includes ++ foreign_includes ++ [blankLines 1] ++ meths
      h_fragments = h_includes ++ [blankLines 1] ++ decls
      c_file_contents = ppReadable $ program c_fragments
      code_gen_option_comment = literal_comment [codeGenOptionDescr flags is_top]
      h_file_contents = ppReadable $ program [ code_gen_option_comment
                                             , protect_header name h_fragments
                                             ]
  h_name_rel <- writeFileC h_file_name h_file_contents
  c_name_rel <- writeFileC c_file_name c_file_contents
  return [ h_name_rel, c_name_rel ]
  where idToHFile sb_map id =
          if isPrimBlock id
          then Nothing
          else Just (sb_name (lookupSB sb_map id) ++ ".h")

-- Extract the name from an expression naming a clock port
mkClkName :: AExpr -> String
mkClkName expr = case expr of
                   ASPort _ i -> getIdString i
                   _ -> internalError ("mkClkName: " ++ ppReadable expr)

-- Make a name for a schedule function given the clock expression,
-- edge direction and whether this is for the edge or after it
mkSchedName :: (AExpr,Bool,Bool) -> String
mkSchedName (expr, is_posedge, after_edge) =
  let dir = (if after_edge then "_after" else "") ++
            (if is_posedge then "_posedge_" else "_negedge_")
      clk_name  = escapeClkName (mkClkName expr)
  in "schedule" ++ dir ++ clk_name

-- remove chars which can't be in a C Id, like '.'
escapeClkName :: String -> String
escapeClkName "" = ""
escapeClkName ('.':cs) = ('_' : escapeClkName cs)
escapeClkName (c:cs)   = (c   : escapeClkName cs)

-- ----------------
