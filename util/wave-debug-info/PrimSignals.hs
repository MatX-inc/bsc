-- The signals a primitive instance puts in a waveform dump, spelled as
-- the selected backend spells them.
--
-- Bluesim dumps a register, wire, CReg or counter as one signal named
-- by the instance and carrying its contents, a probe as `<inst>$PROBE`,
-- and every primitive but the wires also as a scope of the instance's
-- name holding its ports (a register's D_IN, EN and Q_OUT under
-- -wave-include-internals; a FIFO's ENQ, D_IN, DEQ, D_OUT, ...).
--
-- Verilog inlines registers, wires, CRegs and probes into the parent
-- module.  An inlined register is the wire `<inst>` with its ports
-- `<inst>$D_IN` and `<inst>$EN`; an inlined wire or CReg is inlined
-- before methods become ports, so its wires are named by method:
-- `<inst>$wget`, `<inst>$whas`, `<inst>$port0__read`,
-- `<inst>$port0__write_1`, `<inst>$EN_port0__write`; a probe leaves
-- its ports `<inst>$PROBE` and `<inst>$PROBE_VALID`.  Every other
-- primitive is a module instance, a scope of its own, whose ports are
-- also wires `<inst>$<PORT>` of the parent.
--
-- WireAnalysis (bluetcl's wiretypemap) lists every name either
-- backend's dump might use for these ports, to type a dump's signals
-- by name; this module gives each port its one path in the selected
-- backend's dump.
module PrimSignals(PrimPort(..), primSignals) where

import Data.Maybe(listToMaybe)
import qualified Data.Map as M

import Flags(Flags(..))
import Backend(Backend(..))
import Id(getIdBaseString)
import IType(IType)
import ISyntaxUtil(itBool)
import VModInfo
import ASyntax(AVInst(..))
import BackendNamingConventions(isRegInst, isClockCrossingRegInst,
                                isRWire, isRWire0, isBypassWire, isBypassWire0,
                                isClockCrossingBypassWire,
                                isCRegInst, cregReadStr, qoutPortStr, rwireGetStr)

data PrimPort = PrimPort { pp_method :: String   -- the method the port serves
                         , pp_port :: String     -- the port's name in the dump
                         , pp_role :: String     -- "arg", "result" or "enable"
                         , pp_type :: Maybe IType
                         , pp_path :: [String]   -- its path, relative to the parent module's scope
                         }

-- The path, relative to the parent module's scope, of the signal that
-- carries the instance's contents, when the dump has one (a FIFO has
-- only its scope), and the instance's ports
primSignals :: Flags -> AVInst -> (Maybe [String], [PrimPort])
primSignals flags avi =
    case backend flags of
      Just Verilog -> verilogSignals flags avi
      _ -> bluesimSignals avi

inst :: AVInst -> String
inst = getIdBaseString . avi_vname

isPrim :: String -> AVInst -> Bool
isPrim name avi = vName (avi_vmi avi) == VName name

isWire :: AVInst -> Bool
isWire avi = isRWire avi || isRWire0 avi || isBypassWire avi || isBypassWire0 avi

-- The instance's method ports: each argument, result and enable, with
-- the method it serves and the type recorded for it.  An enable tied
-- high (an always-enabled method) has no wire.
methodPorts :: AVInst -> [PrimPort]
methodPorts avi =
    concat [ [ port m vn "arg" | args <- vf_inputs f, (vn, _) <- args ] ++
             [ port m vn "result" | (vn, _) <- vf_outputs f ] ++
             [ (port m vn "enable") { pp_type = Just itBool }
             | Just (vn, props) <- [vf_enable f], VPinhigh `notElem` props ]
           | f@(Method {}) <- vFields (avi_vmi avi)
           , let m = getIdBaseString (vf_name f) ]
  where port m vn role =
            PrimPort m (getVNameString vn) role (M.lookup vn (avi_port_types avi)) []

bluesimSignals :: AVInst -> (Maybe [String], [PrimPort])
bluesimSignals avi
  | isPrim "Probe" avi = (Just [i ++ "$PROBE"], [])
  | isWire avi = (Just [i], [])
  | isRegInst avi || isCRegInst avi || isPrim "Counter" avi = (Just [i], under)
  | otherwise = (Nothing, under)
  where i = inst avi
        under = [ p { pp_path = [i, pp_port p] } | p <- methodPorts avi ]

verilogSignals :: Flags -> AVInst -> (Maybe [String], [PrimPort])
verilogSignals flags avi
  | removeReg flags && isRegInst avi
    && (not (isClockCrossingRegInst avi) || removeCross flags) =
      (Just [i], [ byPort p | p <- methodPorts avi, pp_port p /= qoutPortStr ])
  | isWire avi && (not (isClockCrossingBypassWire avi) || removeCross flags) =
      -- inlining substitutes the setter away and keeps the results; the
      -- contents are wget's when the wire has one
      let results = [ byMethod p | p <- methodPorts avi, pp_role p == "result" ]
          wget = [ p | p <- results, pp_method p == rwireGetStr ]
      in  (fmap pp_path (listToMaybe (wget ++ results)), results)
  | isCRegInst avi = (Just [i ++ sep ++ cregReadStr 0], map byMethod (methodPorts avi))
  | isPrim "Probe" avi = (Just [i ++ sep ++ "PROBE"], map byPort (methodPorts avi))
  | otherwise = (Nothing, map byPort (methodPorts avi))
  where i = inst avi
        sep = if removeVerilogDollar flags then "_" else "$"
        byPort p = p { pp_path = [i ++ sep ++ pp_port p] }
        -- a CReg's write method takes one argument
        byMethod p = let name = case pp_role p of
                                  "result" -> pp_method p
                                  "arg" -> pp_method p ++ "_1"
                                  _ -> "EN_" ++ pp_method p
                     in  p { pp_port = name, pp_path = [i ++ sep ++ name] }
