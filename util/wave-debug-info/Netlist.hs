-- The nets of a module's generated Verilog, named as the netlist names
-- them: bsc's own back end run on the elaboration (aState and the passes
-- after it, the Verilog generator and its stable renumbering), under the
-- flags the .ba recorded, as bsc does when it regenerates Verilog from
-- a .ba.  Every definition of the final package whose name the netlist
-- has, other than the rule fires, method ports and state element ports
-- the signals describe already, is a net.  Each is reported with the
-- rules and methods whose logic it is part of, from the elaboration:
-- the definitions a rule's condition and body reach, and a method's
-- ready, result and body; a definition the back end minted belongs to
-- whatever the definitions it references belong to (a rule's fire or a
-- method's enable counting as that rule or method), or failing that to
-- whatever references it.
module Netlist(Netlist(..), Net(..), Owner(..), verilogNetlist) where

import Control.Monad(unless)
import Data.Data(Data, gmapQ)
import Data.Typeable(cast)
import qualified Data.Map as M
import qualified Data.Set as S

import Error(ErrorHandle, bsWarning, bsError)
import Flags(Flags(..))
import Id(Id, getIdString, unQualId)
import ASyntax
import ASyntaxUtil(aVars)
import ABin(ABinModInfo(..))
import AState(aState)
import InlineWires(aInlineWires)
import InlineCReg(aInlineCReg)
import ARenameIO(aRenameIO)
import ADropDefs(aDropDefs)
import AOpt(aOpt)
import Synthesize(aSynthesize)
import AVeriQuirks(aVeriQuirks)
import VFinalCleanup(finalCleanup)
import AVerilog(aVerilog)
import VStableRenumber(stableRenumberVProgram)
import Verilog(VId(..))
import ForeignFunctions(ForeignFuncMap)
import BackendNamingConventions(createVerilogNameMapForAVInst)
import FStringCompat(getFString)

-- The rule or method whose logic a net is part of.  The part is
-- "condition" or "body" for a rule; "result", "ready" or "body" for a
-- method.
data Owner = Owner { ow_kind :: String, ow_id :: Id, ow_part :: String }
  deriving (Eq, Ord)

data Net = Net { net_name :: String       -- as the netlist spells it
               , net_id :: AId            -- the definition, for its position
               , net_width :: Maybe Integer
               , net_owners :: [Owner] }

newtype Netlist = Netlist { nl_nets :: [Net] }  -- in package order

verilogNetlist :: ErrorHandle -> Flags -> ForeignFuncMap -> ABinModInfo -> IO Netlist
verilogNetlist errh flags0 ffmap abmi = do
    -- codegen flags are the compile's own (recorded with the module's
    -- pragmas applied); only the reporting ones follow this run
    let flags = (abmi_flags abmi) { verbosity = verbosity flags0 }
        pps = abmi_pps abmi
    asmod <- aState errh flags pps (abmi_aschedinfo abmi) (abmi_apkg abmi)
    let (asNoWires, wws, wes)
            | removeRWire flags = aInlineWires flags asmod
            | otherwise = (asmod, [], [])
    unless (null wws) $ bsWarning errh wws
    unless (null wes) $ bsError errh wes
    let asNoCReg | removeCReg flags = aInlineCReg asNoWires
                 | otherwise = asNoWires
    aomod <- aOpt errh flags (aDropDefs (aRenameIO flags asNoCReg))
    let final = finalCleanup flags (aVeriQuirks flags (aSynthesize flags aomod))
    vprog0 <- aVerilog errh flags pps final ffmap
    let vprog | stableVerilog flags = stableRenumberVProgram vprog0
              | otherwise = vprog0
        -- the renumbering is a substitution; both programs list their
        -- identifiers in one order
        renamed = M.fromList (zip (map vidStr (vids vprog0)) (map vidStr (vids vprog)))
        inNetlist = S.fromList (map vidStr (vids vprog))
        spell n = M.findWithDefault n n renamed
        -- a tuple-typed definition is one wire per element, named by the
        -- element's index (AVerilogUtil.tupleElemVId)
        wires d = case adef_type d of
            ATTuple ts -> [ (spell (n ++ "_" ++ show k), width t) | (k, t) <- zip [1 :: Integer ..] ts ]
            t -> [ (spell n, width t) ]
          where n = str (adef_objid d)

        -- the definitions the signals describe already: rule fires, method
        -- ports, state element ports and the ports of inlined primitives
        sinfo = aspkg_signal_info asmod
        fires = S.fromList [ str i | (_, sigs) <- aspsi_rule_sched sinfo, i <- sigs ]
        methodPorts = S.fromList $ concat
            [ [ str i | Just i <- [aspm_mrdyid m] ] ++ [ str i | i <- aspm_resultids m ]
            | m <- aspsi_methods sinfo ]
        statePorts = S.fromList $
            [ getFString b | v <- aspkg_state_instances asmod
                           , (_, b) <- createVerilogNameMapForAVInst flags v ] ++
            map str (aspkg_inlined_ports asNoCReg)
        described n = n `S.member` fires || n `S.member` methodPorts || n `S.member` statePorts

        -- ownership from the elaboration: the definitions each rule's
        -- condition and body reach, and each method's ready, result and
        -- body.  A definition the elaboration did not have (minted by the
        -- back end) belongs to whatever the definitions it references
        -- belong to, a rule's fire or a method's enable counting as that
        -- rule or method; one that references none of those belongs to
        -- whatever references it.
        apkg = abmi_apkg abmi
        localDefs = M.fromList [ (adef_objid d, d) | d <- apkg_local_defs apkg ]
        closure es = go S.empty (concatMap aVars es)
          where go seen [] = seen
                go seen (i : is)
                  | i `S.member` seen = go seen is
                  | Just d <- M.lookup i localDefs = go (S.insert i seen) (aVars (adef_expr d) ++ is)
                  | otherwise = go seen is
        claim o es = [ (i, S.singleton o) | i <- S.toList (closure es) ]
        ruleParts rid r =
            claim (Owner "rule" rid "condition") [arule_pred r] ++
            claim (Owner "rule" rid "body") (concatMap aact_args (arule_actions r))
        methodParts f = case f of
            AIDef { aif_name = m, aif_pred = p, aif_value = v } ->
                claim (Owner "method" m "ready") [p] ++ claim (Owner "method" m "result") [adef_expr v]
            AIAction { aif_name = m, aif_pred = p, aif_body = rs } ->
                claim (Owner "method" m "ready") [p] ++ body m rs
            AIActionValue { aif_name = m, aif_pred = p, aif_body = rs, aif_value = v } ->
                claim (Owner "method" m "ready") [p] ++ body m rs ++
                claim (Owner "method" m "result") [adef_expr v]
            _ -> []
          where body m rs = concat [ claim (Owner "method" m "body") (arule_pred r : concatMap aact_args (arule_actions r)) | r <- rs ]
        elaborated = M.fromListWith S.union $
            concat [ ruleParts (arule_id r) r | r <- apkg_rules apkg ] ++
            concatMap methodParts (apkg_interface apkg)

        defs = aspkg_values final
        defMap = M.fromList [ (adef_objid d, d) | d <- defs ]
        canFire = M.fromList [ (c, r) | (r, [c, _]) <- aspsi_rule_sched sinfo ]
        willFire = M.fromList [ (w, r) | (r, [_, w]) <- aspsi_rule_sched sinfo ]
        enables = M.fromList [ (e, aspm_name m) | m <- aspsi_methods sinfo, Just e <- [aspm_menableid m] ]
        attributed :: M.Map AId (S.Set Owner)
        attributed = M.map attribute defMap
        attribute d = case M.lookup (adef_objid d) elaborated of
            Just os -> os
            Nothing -> S.unions (map through (aVars (adef_expr d)))
        through i
          | Just r <- M.lookup i willFire = S.singleton (Owner "rule" r "body")
          | Just r <- M.lookup i canFire = S.singleton (Owner "rule" r "condition")
          | Just m <- M.lookup i enables = S.singleton (Owner "method" m "body")
          | otherwise = M.findWithDefault S.empty i attributed
        referrers = M.fromListWith (++) [ (i, [adef_objid d]) | d <- defs, i <- aVars (adef_expr d) ]
        owners d = let i = adef_objid d
                       own = M.findWithDefault S.empty i attributed
                   in S.toList $ if S.null own
                                 then S.unions [ M.findWithDefault S.empty j attributed
                                               | j <- M.findWithDefault [] i referrers ]
                                 else own

        nets = [ Net s (adef_objid d) w (owners d)
               | d <- defs, not (described (str (adef_objid d)))
               , (s, w) <- wires d, s `S.member` inNetlist ]
    return (Netlist nets)
  where
    str = getIdString . unQualId
    width (ATBit n) = Just n
    width _ = Nothing
    vidStr (VId s _ _) = s

-- every identifier of a Verilog program, in traversal order
vids :: Data a => a -> [VId]
vids x = case cast x of
           Just v -> [v]
           Nothing -> concat (gmapQ vids x)
