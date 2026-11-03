{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module VerilogMeta(IdentJoin(..), verilogIdents) where

import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char(isDigit)

import Id(Id, getIdString)
import ASyntax
import Verilog
import VModInfo(VName(..), VPort, VeriPortProp(..))
import Pragma(PProp(..))
import IExpandMeta

-- ===============
-- The join of the generated module's identifiers to the ledger
--
-- After the Verilog text is written, the identifiers the printer
-- declared are read off the VProgram the way the reserved-identifier
-- scan reads them (AVerilog.vModuleDeclVIds): the module's ports and
-- parameters, the wires and regs its body declares, its instances and
-- its register groups, each a VId carrying its spelling and the id of
-- the final ASPackage it was printed from.  Each id is resolved
-- through the ledger (IExpandMeta.resolveIds) and every origin is
-- classified against the metadata table and the package: a lifted
-- def's record, a method, instance or rule root, an id a pass made
-- from nothing.  Nothing here is read by the backend: the walk runs
-- under -bxref or -metadata-check alone, after the Verilog files are
-- on disk, over existing strings and ids, and interns nothing.

-- the result of the join
data IdentJoin = IdentJoin {
        -- one record per identifier, in declaration order
        ij_records :: [IdentRecord],
        -- the records that stand for nothing and are not in the
        -- allowed set (the -metadata-check closure's violations)
        ij_unresolved :: [IdentRecord],
        -- the ids resolved, for the census cross-check
        ij_ids :: [Id]
    }

-- The join over the module named after the package (under PPbitBlast,
-- the unblasted wrapper, whose body is the blasted wires and the
-- instance of the blasted module; the blasted module itself is not
-- walked).  The ports are the Verilog port properties of the module
-- (VIOProps.getIOProps), the renames those of -remove-dollar
-- (VVerilogDollar.dollarRenames), by the identifier's string.
verilogIdents :: [PProp] -> ASPackage -> [VPort] -> M.Map String String
              -> VProgram -> IdentJoin
verilogIdents pps aspack ports renames (VProgram mods _ _) =
    IdentJoin { ij_records = records,
                ij_unresolved = filter violation records,
                ij_ids = ids }
  where
    modName = getIdString (aspkg_name aspack)
    decls = concat [ moduleDecls vm | vm <- mods,
                                      getVIdString (vm_name vm) == modName ]
    -- one entry per spelling, first occurrence first, with its kinds in
    -- the module's order (a port re-declared in the body has both)
    (ordered, ids) = distinctDecls ruleSignals decls
    ruleSignals = S.fromList [ i | (_, is) <- aspsi_rule_sched signalInfo, i <- is ]
    signalInfo = aspkg_signal_info aspack
    ledger = aspkg_meta aspack
    resolved = resolveIds ledger ids
    ctx = joinContext aspack ledger
    records = [ IdentRecord { ir_ident = M.findWithDefault s s renames,
                              ir_id = getIdString i,
                              ir_kinds = ks,
                              ir_origins = origins i,
                              ir_history = history i }
              | (s, i, ks) <- ordered ]
    -- sorted and distinct: a method's ready port and the method itself
    -- both classify to the method's root
    origins i = case M.lookup i resolved of
                  Just (os, _) -> S.toAscList (S.fromList (map (classify ctx) os))
                  Nothing -> [OUnresolved (getIdString i)]
    history i = maybe [] snd (M.lookup i resolved)
    -- the identifiers that may stand for nothing: clock, gate and reset
    -- ports and ports nothing reads (by their port properties; a reset
    -- no register uses is an unused port), module parameters, the
    -- ports and wire of a scan chain (AVerilog.createScanArgs,
    -- createScanDefs) and, under PPbitBlast, the wrapper's body
    allowed = S.fromList
        ([ n | (VName n, props) <- ports,
               any (`elem` [VPclock, VPclockgate, VPreset, VPunused]) props ] ++
         map (getIdString . fst) (aspkg_parameters aspack) ++
         (if any isScan pps then ["scan_en", "scan_in", "scan_out"] else []))
    isScan (PPscanInsert _) = True
    isScan _ = False
    bitBlast = PPbitBlast `elem` pps
    isPort k = k `elem` [IKInput, IKOutput, IKInout, IKParameter]
    violation r = identUnresolved r && not (S.member (ir_id r) allowed) &&
                  not (bitBlast && not (any isPort (ir_kinds r)))

-- The identifiers a module declares, in order: its ports and
-- parameters, then, per body item, the names it declares.  A spine
-- walk, as AVerilog.vModuleDeclVIds, without the module's own name,
-- the instantiated modules' names and their port and parameter
-- formals (which belong to the instantiated module), functions and
-- DPI imports (external names).
moduleDecls :: VModule -> [(IdentKind, VId)]
moduleDecls vm =
    [ d | (vargs, _) <- vm_ports vm, va <- vargs, d <- argDecls va ] ++
    concatMap itemDecls (vm_body vm)
  where
    argDecls (VAInput i _) = [(IKInput, i)]
    argDecls (VAInout i mi _) = (IKInout, i) : [ (IKWire, n) | Just n <- [mi] ]
    argDecls (VAOutput i _) = [(IKOutput, i)]
    argDecls (VAParameter i _ _ _) = [(IKParameter, i)]
    itemDecls :: VMItem -> [(IdentKind, VId)]
    itemDecls (VMDecl d) = declDecls d
    itemDecls (VMInst { vi_inst_name = i }) = [(IKInstance, i)]
    itemDecls (VMAssign _ _) = []
    itemDecls (VMStmt {}) = []
    itemDecls (VMComment _ it) = itemDecls it
    itemDecls (VMRegGroup i _ _ it) = (IKReg, i) : itemDecls it
    itemDecls (VMGroup _ itss) = concatMap (concatMap itemDecls) itss
    itemDecls (VMFunction _) = []
    itemDecls (VMDPI _) = []
    declDecls (VVDecl t _ vs) = [ (declKind t, vvName v) | v <- vs ]
    declDecls (VVDWire _ v _) = [(IKWire, vvName v)]
    declKind VDReg = IKReg
    declKind VDWire = IKWire
    declKind VDInput = IKInput
    declKind VDInout = IKInout
    declKind VDOutput = IKOutput

-- the declarations by spelling, first occurrence first, each with its
-- id and its distinct kinds in order (a wire that is a rule's firing
-- signal is a rule signal), and the distinct ids in the same order
distinctDecls :: S.Set Id -> [(IdentKind, VId)] -> ([(String, Id, [IdentKind])], [Id])
distinctDecls ruleSignals decls =
    ([ (s, i, reverse ks) | s <- reverse order, Just (i, ks) <- [M.lookup s table] ],
     reverse idList)
  where
    (table, order, _, idList) = L.foldl' add (M.empty, [], S.empty, []) decls
    add (t, o, seen, is) (k, VId s i _) =
        let k' = if k == IKWire && S.member i ruleSignals then IKRule else k
        in  case M.lookup s t of
              Just (i0, ks) | k' `elem` ks -> (t, o, seen, is)
                            | otherwise -> (M.insert s (i0, k' : ks) t, o, seen, is)
              Nothing -> (M.insert s (i, [k']) t, s : o,
                          S.insert i seen,
                          if S.member i seen then is else i : is)

-- What the classification of an origin reads: the table, its roots by
-- kind and name (for a root whose id the package carries at another
-- position than the evaluator pushed it), the instantiation roots by
-- the name the evaluator gave each instance, the method each method
-- port belongs to, the instances by name and the ids created from
-- nothing.
data JoinContext = JoinContext {
        jc_meta :: ModuleMeta,
        jc_rootsByName :: M.Map (SiteKind, String) [Int],
        jc_instanceRoots :: M.Map String Int,
        jc_methodOf :: M.Map Id Id,
        jc_instances :: M.Map String Id,
        jc_created :: M.Map Id PassTag
    }

joinContext :: ASPackage -> MetaLedger -> JoinContext
joinContext aspack ledger@(MetaLedger mm) =
    JoinContext {
        jc_meta = mm,
        -- distinct sites: a root pushed once per iteration of a loop
        -- (an instance made in a static loop) is one site, listed by
        -- every push
        jc_rootsByName =
            M.map (S.toAscList . S.fromList)
                (M.fromListWith (++)
                     [ ((siteKeyKind key, name), [site])
                     | site <- mm_roots mm,
                       Just (SiteRow { sr_key = key }) <- [siteRow (mm_sites mm) site],
                       Just name <- [rootName key] ]),
        jc_instanceRoots =
            instanceNames [ (renderName b q, site)
                          | site <- mm_roots mm,
                            Just (SiteRow { sr_key = IdKey b q _ SKInst })
                                <- [siteRow (mm_sites mm) site] ],
        jc_methodOf =
            M.fromList [ (port, aspm_name m)
                       | m <- aspsi_methods (aspkg_signal_info aspack),
                         port <- maybe [] (: []) (aspm_mrdyid m) ++
                                 maybe [] (: []) (aspm_menableid m) ++
                                 aspm_resultids m ++ aspm_inputs m ],
        jc_instances =
            M.fromList [ (getIdString (avi_vname inst), avi_vname inst)
                       | inst <- aspkg_state_instances aspack ],
        jc_created = createdFromNothing ledger }
  where
    rootName (IdKey b q _ _) = Just (renderName b q)
    rootName (RootKey n _) = Just n
    rootName _ = Nothing

-- The name the evaluator gave each instance, with its root.  An
-- instantiation pushes its root under the name hint (IExpand.newState)
-- and then names the instance by uniqueStateName (IExpandUtils): the
-- first use of a hint keeps it, a later use takes the first unused
-- <hint>_<n>, n counted from 1 per hint, and a name so made is itself
-- in use after that; ids compare by name, so uses of a hint at
-- different positions share one count.  Nothing between the push and
-- the naming instantiates (the arguments of newState are evaluated
-- before it runs), so the roots in push order are the hints in naming
-- order, and the same walk over them, keyed on the rendered names
-- (getIdString of the instance's id), gives each name its root; a hint
-- pushed once per iteration of a loop gives every iteration's name the
-- one site.  The names are distinct by construction, so an insert
-- never replaces one.
instanceNames :: [(String, Int)] -> M.Map String Int
instanceNames = go M.empty M.empty
  where
    go _ named [] = named
    go used named ((hint, site) : rest) =
        case M.lookup hint used of
          Nothing -> go (M.insert hint (1 :: Int) used) (M.insert hint site named) rest
          Just n ->
              let (name, n') = firstUnused n
                  firstUnused k = let cand = hint ++ "_" ++ show k
                             in  if M.member cand used then firstUnused (k + 1) else (cand, k)
              in  go (M.insert name 1 (M.insert hint (n' + 1) used))
                     (M.insert name site named) rest

-- The origin an id of the final package stands for.  A def lifted by
-- the rebuild is named <hint>__h<pointer> (IExpandMeta.defNameString),
-- and the pointer is the key of its record.  A method is a root of
-- the table, looked up by the key the evaluator pushed (its id,
-- position included), then by kind and name; an instance by the name
-- the evaluator gave it (instanceNames; the root was pushed under the
-- name hint, which a repeated hint's instances do not carry); a
-- method's ports (its ready, enable, result and argument ports)
-- resolve to the method.  A rule
-- id is RL_<prefix><name> for a root keyed on the name
-- alone (IExpand pushes rootSiteKey SKRule on the evaluated name), so
-- a rule resolves to the one root its id ends in, or to its own id.
-- An id a pass created from nothing resolves to that pass.  A port of
-- a submodule instance, <instance>$<port>, resolves to the instance.
classify :: JoinContext -> Id -> IdentOrigin
classify jc i
    | Just p <- defPointer s, IM.member p (mm_defs mm) = ODef p
    | Just r <- rootSite SKMethodRoot i = ORoot r
    | Just r <- rootSite SKInst i = ORoot r
    | Just m <- M.lookup i (jc_methodOf jc), Just r <- rootSite SKMethodRoot m = ORoot r
    | Just rest <- L.stripPrefix "RL_" s =
        case M.lookup (SKRule, rest) (jc_rootsByName jc) of
          Just [r] -> ORoot r
          _ -> case [ r | ((SKRule, n), [r]) <- M.toList (jc_rootsByName jc),
                          ('_' : n) `L.isSuffixOf` rest ] of
                 [r] -> ORoot r
                 _ -> ORule s
    | Just t <- M.lookup i (jc_created jc) = OPass t s
    | Just inst <- instanceOf s, Just r <- rootSite SKInst inst = ORoot r
    | otherwise = OUnresolved s
  where
    s = getIdString i
    mm = jc_meta jc
    rootSite SKInst x = M.lookup (getIdString x) (jc_instanceRoots jc)
    rootSite k x =
        case M.lookup (idSiteKey k x) (st_ids (mm_sites mm)) of
          Just (SiteRef site _) -> Just site
          Nothing -> rootByName k (getIdString x)
    rootByName k n = case M.lookup (k, n) (jc_rootsByName jc) of
                       Just [site] -> Just site
                       _ -> Nothing
    instanceOf str =
        case break (== '$') (reverse str) of
          (_, '$' : rest) -> M.lookup (reverse rest) (jc_instances jc)
          _ -> Nothing

-- the pointer a lifted def's name ends in: the digits after the last
-- "__h", when the whole tail is digits
defPointer :: String -> Maybe Int
defPointer s =
    case [ drop 3 t | t <- L.tails s, "__h" `L.isPrefixOf` t ] of
      [] -> Nothing
      tails -> let digits = last tails
               in  if not (null digits) && all isDigit digits
                   then Just (read digits)
                   else Nothing
