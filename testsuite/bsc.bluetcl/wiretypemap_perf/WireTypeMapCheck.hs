-- Direct tests of the production analysis, including ambiguous wire names
-- that cannot be expressed through ordinary source-level port declarations.
-- See run.sh for the correctness and optional scaling commands.
module Main where

import Control.DeepSeq(force)
import Control.Exception(evaluate)
import Control.Monad(unless)
import qualified Data.Map as M
import qualified Data.Set as S
import System.CPUTime(getCPUTime)
import System.Environment(getArgs)
import Text.Printf(printf)
import Text.Read(readMaybe)

import ASyntax
import Id(mk_homeless_id)
import ISyntax(IType)
import ISyntaxUtil(itBool, itBitN, itClock)
import SchedInfo(SchedInfo(..), MethodConflictInfo(..))
import VModInfo
import WireAnalysis(getWireTypeMap, getWireTypeCandidates)
import Wires(emptyWireProps)

clocks :: VClockInfo
clocks = ClockInfo [] [] [] []

resets :: VResetInfo
resets = ResetInfo [] []

package :: [(String, IType)] -> [AVInst] -> APackage
package external instances = APackage {
    apkg_name = mk_homeless_id "test",
    apkg_is_wrapped = False,
    apkg_backend = Nothing,
    apkg_size_params = [],
    apkg_inputs = [],
    apkg_clock_domains = [],
    apkg_external_wires = WireInfo clocks resets [],
    apkg_external_wire_types = M.fromList [(VName n, t) | (n, t) <- external],
    apkg_expr_types = [],
    apkg_final_wire_names = Nothing,
    apkg_reset_list = [],
    apkg_state_instances = instances,
    apkg_local_defs = [],
    apkg_rules = [],
    apkg_schedule_pragmas = [],
    apkg_interface = [],
    apkg_inst_comments = [],
    apkg_inst_tree = M.empty,
    apkg_proof_obligations = []
    }

instanceWithType :: String -> IType -> AVInst
instanceWithType name typ =
    AVInst (mk_homeless_id name) (ATBit 1) False []
        (M.singleton (VName "OUT") typ)
        (mkVModInfo (VName "TestPrimitive") clocks resets [] []
            (SchedInfo (MethodConflictInfo [] [] [] [] [] [] []) [] [] [])
            (VPathInfo [])) [] []

checkPairs :: IO ()
checkPairs = do
    -- The first pair is repeated across external and submodule entries.
    -- The final instance repeats an earlier instance. Bool and Bit#(1)
    -- must both survive under a$OUT and a.OUT, despite their equal width.
    let input = package [("z", itBool), ("a$OUT", itBool)]
            [ instanceWithType "a" itBool,
              instanceWithType "a" (itBitN 1),
              instanceWithType "b" (itBitN 1),
              instanceWithType "a" itBool ]
        expected =
            [ ("a$OUT", itBool), ("z", itBool), ("a.OUT", itBool),
              ("a$OUT", itBitN 1), ("a.OUT", itBitN 1),
              ("b$OUT", itBitN 1), ("b.OUT", itBitN 1) ]
        actual = getWireTypeMap input
    unless (actual == expected) $
        error ("wrong candidate order or deduplication: " ++ show actual)
    unless (null (getWireTypeMap (package [] []))) $
        error "empty package has candidate wires"
    putStrLn "PASS: stable pair deduplication, category order, distinct same-name types"


-- A0's split results must join evidence to exactly the selected port,
-- both on a submodule call and on the enclosing module's interface.
checkSplitOutputs :: IO ()
checkSplitOutputs = do
    let iid = mk_homeless_id "child"
        mid = mk_homeless_id "get"
        bt = ATBit 1
        tup = ATTuple [bt,bt]
        fi = Method mid Nothing Nothing 1 [] [(VName "LEFT",[]),(VName "RIGHT",[])] Nothing
        vmi = mkVModInfo (VName "TestPrimitive") clocks resets [] [fi]
                  (SchedInfo (MethodConflictInfo [(mid,mid)] [] [] [] [] [] []) [] [] [])
                  (VPathInfo [])
        inst = (instanceWithType "child" (itBitN 1)) {
                    avi_port_types = M.fromList [(VName "LEFT",itBitN 1),(VName "RIGHT",itBitN 1)],
                    avi_vmi = vmi }
        left = ATupleSel bt (AMethCall tup iid mid []) 1
        right = ATupleSel bt (AMethCall tup iid mid []) 2
        intf = AIDef mid [] emptyWireProps left
                   (ADef mid tup (ATuple tup [left,right]) []) fi []
        input = (package [("LEFT",itBitN 1),("RIGHT",itBitN 1)] [inst]) { apkg_interface = [intf],
                  apkg_expr_types = [(left,S.singleton itBool)] }
        actual = M.fromList (getWireTypeCandidates input)
    unless (all (\n -> maybe False (itBool `elem`) (M.lookup n actual))
                ["child$LEFT","child.LEFT","LEFT"]) $
        error ("split output lost its observation: " ++ show actual)
    unless (all (\n -> maybe True (itBool `notElem`) (M.lookup n actual))
                ["child$RIGHT","child.RIGHT","RIGHT","get"]) $
        error ("split output observation escaped to another port: " ++ show actual)
    putStrLn "PASS: split result evidence follows only its physical output port"


-- A removed old name must not contaminate another wire that receives that
-- spelling during stable renumbering. Staged elaboration exposes boundaries
-- but withholds internal names until an emitted-name binding exists.
checkEmittedNames :: IO ()
checkEmittedNames = do
    let bt = ATBit 1
        old = "bits__h112"
        target = "bits__h1"
        port = ASPort bt (mk_homeless_id "input")
        other = ASPort bt (mk_homeless_id "other")
        raw = (package [("input",itBitN 1)] []) {
                apkg_local_defs = [ADef (mk_homeless_id old) bt port [],
                                   ADef (mk_homeless_id target) bt other []],
                apkg_expr_types = [(port,S.singleton itBool),(other,S.singleton itClock)] }
        final = raw { apkg_final_wire_names = Just (M.fromList [(old,target),("input","input")]) }
        mapped = M.fromList (getWireTypeCandidates final)
        staged = M.fromList (getWireTypeCandidates raw)
    unless (M.lookup target mapped == Just [itBool] && M.notMember old mapped) $
        error ("stale-name collision contaminated final type: " ++ show mapped)
    unless (M.notMember old staged && M.notMember target staged &&
            maybe False (itBool `elem`) (M.lookup "input" staged)) $
        error ("unbound internal name escaped staged fallback: " ++ show staged)
    let identity = raw { apkg_final_wire_names = Just (M.singleton old old) }
    unless (M.lookup old (M.fromList (getWireTypeCandidates identity)) == Just [itBool]) $
        error "identity emitted-name map lost an internal type"
    let dollars = (package [("x$OUT",itBitN 1)] []) {
                    apkg_expr_types = [(ASPort bt (mk_homeless_id "x$OUT"),S.singleton itBool)],
                    apkg_final_wire_names = Just (M.singleton "x$OUT" "x_OUT") }
    unless (M.lookup "x_OUT" (M.fromList (getWireTypeMap dollars)) == Just itBool) $
        error "renamed declared wire did not retain its type evidence"
    putStrLn "PASS: emitted names, stale-name exclusion, identity, staged fallback, dollar aliases"

benchmark :: Int -> IO ()
benchmark count = do
    let input = package []
            [ instanceWithType ("wire" ++ show i) (itBitN 32)
            | i <- [1..count] ]
    -- Exclude package construction; force the entire result inside the
    -- timed interval. This measures analysis, not Tcl/type rendering.
    _ <- evaluate (force input)
    start <- getCPUTime
    result <- evaluate (force (getWireTypeMap input))
    end <- getCPUTime
    unless (length result == 2 * count) $
        error "incorrect benchmark candidate count"
    printf "%d instances, %d entries: %.6f CPU seconds\n"
        count (length result) (fromIntegral (end - start) / 1e12 :: Double)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [] -> checkPairs >> checkSplitOutputs >> checkEmittedNames
        [arg] | Just count <- readMaybe arg, count > 0 -> benchmark count
        _ -> error "usage: WireTypeMapCheck [positive-instance-count]"
