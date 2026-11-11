-- The capture's net-name ledger (CaptureLive.NetLive): each identifier the
-- generated Verilog module declares, bound to the package names it stands
-- for through the renames the backend actually made (renameInoutPorts,
-- renameSVStdIdents, removeDollarsFromVerilog), never by spelling.
--
-- Reads the stored program (deepseq'd by the DFverilogDollar dump), the
-- dollar map whose every value names an identifier of that program, and
-- the rename lists aVerilog returned; it builds no name.
module CaptureLedger(netLedger) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(foldl')

import Id(Id)
import Verilog
import ASyntax(ASPackage(..), ADef(..))
import CaptureLive

netLedger :: VProgram -> M.Map Id Id -> ([(VId, VId)], [(String, String)]) ->
             ASPackage -> NetLive
netLedger (VProgram mods _ _) dollar_map (inouts, svstd) aspkg =
    let decls = case mods of
                  (m : _) -> modDecls m
                  [] -> []
        undollar = M.fromList [ (new, old) | (old, new) <- M.toList dollar_map ]
        svmap = M.fromList [ (new, old) | (old, new) <- svstd ]
        inoutmap = M.fromList [ (getVIdString new, old) | (new, old) <- inouts ]
        entry (VId s i _) =
            let (pre, fd) = case M.lookup i undollar of
                              Just old -> (old, [NFDollar old])
                              Nothing -> (i, [])
                fs = [ NFSvStd o | Just o <- [M.lookup s svmap] ]
                (io_ids, fi) = case M.lookup s inoutmap of
                                 Just (VId o oi _) -> ([oi], [NFInout o])
                                 Nothing -> ([], [])
            in  NetNameL s (pre : io_ids) (fd ++ fs ++ fi)
        names = map entry (dedup S.empty decls)
        dedup _ [] = []
        dedup seen (v@(VId s _ _) : vs)
            | S.member s seen = dedup seen vs
            | otherwise = v : dedup (S.insert s seen) vs
        -- perturbed: every net gets the next net's origins
        names' | perturbing ptNetnames =
                   zipWith (\n o -> n { nnl_ids = nnl_ids o, nnl_from = nnl_from o })
                           names (rotate names)
               | otherwise = names
        -- defs aVerilog does not declare (it drops the unused ones)
        declared = S.fromList (concatMap nnl_ids names)
        undeclared = [ i | ADef i _ _ _ <- aspkg_values aspkg,
                           not (S.member i declared) ]
        tbl = foldl' (\t i -> capGone kindDead stVerilog viaDrop i [] t)
                     (capOf (aspkg_capture aspkg)) undeclared
    in  NetLive names' tbl

-- The identifiers a module declares as nets: its name, ports, variables,
-- instance names and register groups (not a submodule's own names).
modDecls :: VModule -> [VId]
modDecls vm =
    vm_name vm :
    [ v | (args, _) <- vm_ports vm, a <- args, v <- argVIds a ] ++
    concatMap item (vm_body vm)
  where
    argVIds (VAInput i _) = [i]
    argVIds (VAInout i _ _) = [i]
    argVIds (VAOutput i _) = [i]
    argVIds (VAParameter i _ _ _) = [i]
    item (VMDecl (VVDecl _ _ vs)) = map vvName vs
    item (VMDecl (VVDWire _ v _)) = [vvName v]
    item (VMInst { vi_inst_name = inm }) = [inm]
    item (VMComment _ it) = item it
    item (VMRegGroup i _ _ it) = i : item it
    item (VMGroup _ itss) = concatMap (concatMap item) itss
    item _ = []
