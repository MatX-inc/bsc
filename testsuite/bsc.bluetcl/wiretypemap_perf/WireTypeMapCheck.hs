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
import ISyntaxUtil(itBool, itBitN)
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
                  (SchedInfo (MethodConflictInfo [] [] [] [] [] [] []) [] [] [])
                  (VPathInfo [])
        inst = (instanceWithType "child" (itBitN 1)) {
                    avi_port_types = M.fromList [(VName "LEFT",itBitN 1),(VName "RIGHT",itBitN 1)],
                    avi_vmi = vmi }
        left = ATupleSel bt (AMethCall tup iid mid []) 1
        right = ATupleSel bt (AMethCall tup iid mid []) 2
        intf = AIDef mid [] emptyWireProps left
                   (ADef mid tup (ATuple tup [left,right]) []) fi []
        input = (package [] [inst]) { apkg_interface = [intf],
                  apkg_expr_types = [(left,S.singleton itBool)] }
        actual = M.fromList (getWireTypeCandidates input)
    unless (all (\n -> maybe False (itBool `elem`) (M.lookup n actual))
                ["child$LEFT","child.LEFT","LEFT"]) $
        error ("split output lost its observation: " ++ show actual)
    unless (all (\n -> maybe True (itBool `notElem`) (M.lookup n actual))
                ["child$RIGHT","child.RIGHT","RIGHT","get"]) $
        error ("split output observation escaped to another port: " ++ show actual)
    putStrLn "PASS: split result evidence follows only its physical output port"

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
        [] -> checkPairs >> checkSplitOutputs
        [arg] | Just count <- readMaybe arg, count > 0 -> benchmark count
        _ -> error "usage: WireTypeMapCheck [positive-instance-count]"
