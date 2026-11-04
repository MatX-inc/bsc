-- | The file's first sections (mke2e.py lines 974-1625): header and local
-- lemmas, child interfaces, kinds and uses tables, site tables, guards and firing.
module Lean.Sched.Emit.Hdr
    ( emitHeader, emitUses, emitSites, emitGuards, schedLean, usesTab, usesUs, mtaskNote
    ) where

import Control.Monad (forM_, when, unless)
import Data.Array ((!), listArray)
import Data.Bits (shiftL, (.|.))
import Data.List (intercalate, stripPrefix)
import Data.Maybe (fromMaybe)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (null, (!), findWithDefault, fromList, keys, lookup, member, fromListWith)
import qualified Data.Set as S (member, empty, fromList, insert, toAscList)
import qualified Data.Sequence as Sq (fromList, index)

import Lean.Print (andterm, cut, fmt, nat, stab, tab, tabOr, tup, w, W)
import Lean.Sched.Emit.Ctx (cfHw, condHw, ex, hasCf, hwGuard, guardRho, ihelper, isUncond, kd, lport, mxKind, rdyHw, rho,
    site, sitePort, step, EC(ecRDC, ecName, ecLCP, ecCfgLib, ecNM, ecNR, ecU, ecNT, ecKinds, ecM, ecNS,
    ecMultiBlk, ecChkG, ecLitc, ecGroups, ecWXM, ecMet, ecDomNote, ecExport, ecSynth))
import Lean.Sched.Export (Export(exNS, exMeths, exModule), SynthKid(..), XMeth(..), exConfB, exPrecB, exSbrB, isSynthKind, portFn)
import Lean.Sched.Kinds (cfgTypes, KindDef(kdAlwaysRdy, kdIface, kdUsesc, kdPorts, kdLPorts, kdCodeRel,
    kdShare, kdHasEn, kdMType), PM(PT, PS), Sched)
import Lean.Sched.Model (DView(dvKids, dvKidOrder), MCheck(ckLst, ckHead, ckHeads), MCf(cfRule, cfLine), MPort,
    MSite(sPort, sKind, sArgs, sCond, sArg), MStep(tGuard, tRes, tSites, tName), MX(MFalse, MTrue, MZ, MChkI),
    Model(mDisj, mMTasks, mDV, mByk, mRules, mBlk, mChecks, mCfS))

mtaskNote :: EC -> String
mtaskNote ec = case mMTasks (ecM ec) of
    [] -> ""
    ts -> "\n   Task calls inside interface methods (bsc G0020) are left out of task agreement: "
          ++ intercalate ", " [ a ++ " (" ++ show n ++ ")" | (a, n) <- ts ] ++ "."

asmNote :: EC -> String
asmNote ec
    | null (mChecks (ecM ec)) && not (hasCf ec) = "\n   Assumed user assertions (bsc's run-time checks): none" ++ (if null kidAsm then "." else " of its own." ++ kidAsm)
    | otherwise = "\n   Assumed user assertions (bsc's run-time checks, in `asm`): " ++ show (length (ecGroups ec))
                  ++ " mutually_exclusive group(s) of " ++ (case ecGroups ec of { [] -> "0"; gs -> intercalate ", " (map (show . length) gs) })
                  ++ " rules (`grpT`) and " ++ show (length (ecLitc ec)) ++ " single check(s) (`chkHead`, `chkLst`)"
                  ++ (if nj == 0 then "" else ",\n   " ++ show nj ++ " of them from line(s) with several rules on their first side (one check per rule of\n\
\   that side; together exactly bsc's line)")
                  ++ (if not (hasCf ec) then "." else ";\n   " ++ show (length (mCfS (ecM ec))) ++ " conflict_free check(s) (`cfR`, `cfT`), each instrumentation wire read as its one\n\
\   writer's argument; a fact the solver confirms only with them assumed is stated under `asmP`.")
                  ++ kidAsm
  where kidAsm = case [ exModule (skExport sk) | (_, sk) <- ecSynth ec, skOff sk == Nothing, "user assertions (in asm)" `elem` skAssume sk ] of
            [] -> ""
            ms -> "\n   Its synthesized children's files assume their own (in their `asm`): " ++ intercalate ", " ms ++ "."
        nj = length [ () | c <- ecLitc ec, length (ckHeads (mChecks (ecM ec) !! c)) > 1 ]

schedLean :: Sched -> String -> String
schedLean sl ctor = "[" ++ intercalate ",\n   " [ "([" ++ intercalate ", " (map (ctor ++) ps) ++ "], ." ++ r ++ ", ["
                                                   ++ intercalate ", " (map (ctor ++) qs) ++ "])" | (ps, r, qs) <- sl ] ++ "]"

kidSched :: EC -> String -> Sched
kidSched ec typ = kidSchedOf ec [typ]

-- | The schedule of the first child whose Verilog module is one of these.
kidSchedOf :: EC -> [String] -> Sched
kidSchedOf ec typs = case [ sl | k <- dvKidOrder dv, Just (t, _, sl) <- [M.lookup k (dvKids dv)], t `elem` typs ] of
    (x : _) -> x
    [] -> []
  where dv = mDV (ecM ec)

hasF2 :: EC -> Bool
hasF2 ec = "f2" `elem` ecKinds ec || "sf" `elem` ecKinds ec

emitHeader :: EC -> W ()
emitHeader ec = do
    let name = ecName ec
        kinds = ecKinds ec
        m = ecM ec
    w (fmt "/- %s: the modular obligation of one corpus module, emitted by schedlean.\n\
\   Structure (children, steps, sites, users, exclusions) is the design's; data and\n\
\   guards are opaque operators over their real fan-in (see e2e/README.md).%s%s%s -/\n\
\%s\n\
\open Modular Emit Scan\n\
\open scoped List\n\
\\n\
\set_option autoImplicit false\n\
\set_option linter.unusedVariables false\n\
\set_option linter.unusedSimpArgs false\n\
\set_option linter.deprecated false\n\
\set_option maxRecDepth 100000\n\
\set_option maxHeartbeats 0\n\
\\n\
\namespace %s\n\
\noncomputable section\n\
\\n\
\/-- The module's operators: constants, opaque data and Boolean operators (by number), tuples,\n\
\    and the design's Bool reading of a one-bit value. -/\n\
\structure Ops (V : Type) where\n\
\  z : V\n\
\  tup : V → V → V\n\
\  k : Nat → V\n\
\  f : Nat → V → V\n\
\  g : Nat → V → Bool\n\
\  ofBool : Bool → V\n\
\  toB : V → Bool := fun _ => false\n"
       [name, mtaskNote ec, asmNote ec, ecDomNote ec, (if not (null (ecLCP ec)) || ecCfgLib ec then "import LastCaller" else "import Scan")
                                                     ++ concat [ "\nimport " ++ ns | ns <- childImports ec ], name])
    w localLemmas
    w "/-! ## Child interfaces: the library's, and assumed kinds from the dump's schedules -/\n"
    when ("fifo" `elem` kinds) $ w (fmt fifoText [schedLean (kidSched ec "FIFOL1") "."])
    when ("cfg" `elem` kinds) $ w (fmt cfgLibText [schedLean (kidSchedOf ec cfgTypes) "."])
    when ("rf" `elem` kinds) $ w rfText
    forM_ [ (k, sk) | (k, sk) <- ecSynth ec, skOff sk /= Nothing ] $ \(k, sk) -> do
        let kdef = kd ec k
            ps = kdPorts kdef
            n = length ps
            (cp, cs, cc) = fromMaybe (error "schedlean: a synthesized leaf without code relations") (kdCodeRel kdef)
            code = M.fromList (zip ps [0 :: Int ..])
            rel f = [ [ b | b <- [0 .. n - 1], f a b ] | a <- [0 .. n - 1] ]
            rdyOf = [ maybe [] pure (M.lookup (PS ("RDY_" ++ pname p)) code) | p <- ps ]
        w (fmt "/-- %s: %s, an assumed synthesized leaf (design §3.1, %s): its ports bsc's methods as the parent\n\
\    sees them, action methods C with themselves, each RDY_m an always-ready value port aliased to m's ready. -/"
               [k, exModule (skExport sk), maybe "" id (skOff sk)])
        w (finAnn ("sA_" ++ k) n (rel cp) (rel cs) (rel cc) (map (kdShare kdef) ps)
                  (map (kdHasEn kdef) ps) (map (kdAlwaysRdy kdef) ps) (Just rdyOf))
        w (fmt "abbrev sX_%s : Iface := ⟨Fin %d, inferInstance, sA_%s⟩\n\
\def %s (i : Nat) : Fin %d := ⟨i %% %d, Nat.mod_lt _ (by decide)⟩\n\
\theorem %s_val (m : Fin %d) : %s m.val = m := Fin.ext (Nat.mod_eq_of_lt m.isLt)\n"
               [k, show n, k, portFn k, show n, show n, portFn k, show n, portFn k])
    when (hasF2 ec) $ w ""
    let nT = ecNT ec
        kdOf = kd ec
    when (nT > 0) $
        w ("/-- The module's task statements, numbered in rank order (trust point: bsc's Verilog order). -/\n"
           ++ "abbrev nT : Nat := " ++ show nT ++ "\ndef tP (t : Nat) : Fin nT := ⟨t % nT, Nat.mod_lt _ (by decide)⟩\n")
    w ("inductive Kind where\n  | " ++ intercalate " | " kinds ++ "\n  deriving DecidableEq\n")
    w ("abbrev nk : Kind → Nat\n" ++ concat [ "  | ." ++ k ++ " => " ++ show (length (mByk m M.! k)) ++ "\n" | k <- kinds ])
    w ("abbrev KI : Kind → Iface\n" ++ concat [ "  | ." ++ k ++ " => " ++ fromMaybe "" (kdIface (kdOf k)) ++ "\n" | k <- kinds ])
    w "abbrev I := Σ κ : Kind, Fin (nk κ)\nabbrev CI : I → Iface := fun j => KI j.1\nabbrev OC (V : Type) := (j : I) → Outs (CI j).M V\n"
    forM_ kinds $ \k -> w (fmt "def %s (i : Nat) : I := ⟨.%s, ⟨i %% nk .%s, Nat.mod_lt _ (by decide)⟩⟩" [ihelper k, k, k])
    case ecExport ec of
        Just ex | ecMet ec -> do
            let xs = exMeths ex
                n = length xs
                rel f = [ [ b | b <- [0 .. n - 1], f a b ] | a <- [0 .. n - 1] ]
            w (fmt "\nabbrev nM : Nat := %d\n\
\/-- The module's exported methods, as a synthesized child (design §3.1), in order: %s.\n\
\    bsc's exported relations as bits by method (prec: SB, SBR; sbr: SBR; conf: C, or none; action\n\
\    methods C with themselves); value methods without an enable, one argument wire (`share`);\n\
\    `alwaysRdy` where bsc builds the method always ready (its wrapper assumes RDY = 1). -/" [show n, intercalate ", " (map xmName xs)])
            w (finAnn "xAnn" n (rel (exPrecB ex)) (rel (exSbrB ex)) (rel (exConfB ex)) (map xmValue xs) (map (not . xmValue) xs) (map xmAR xs) Nothing)
            w (fmt (fromMaybe (error "schedlean: metText does not start with metHead") (stripPrefix metHead metText)) [show (ecNR ec), show (ecNS ec)])
        _ | not (ecMet ec) -> w (fmt noMetText [show (ecNR ec), show (ecNS ec)])
          | otherwise -> w (fmt metText [show (ecNM ec), show (ecNR ec), show (ecNS ec)])
    w buildersText
    unless (null (ecSynth ec)) $ w finBuilders
    when ("cfg" `elem` kinds) $ w cfgBuilders
    when ("fifo" `elem` kinds) $ w fifoBuilders
    when (hasF2 ec) $ w f2Builders
    when ("rf" `elem` kinds) $ w rfBuilders

-- | The files of the synthesized children on path A (design.md §3.1).
childImports :: EC -> [String]
childImports ec = foldl (\acc x -> if x `elem` acc then acc else acc ++ [x]) [] [ exNS (skExport sk) | (_, sk) <- ecSynth ec, skOff sk == Nothing ]

pname :: PM -> String
pname (PS p) = p
pname (PT t) = show t

-- | An annotation over @Fin n@ as bits by port: prec, sbr, conf, each by rows and by columns (a pair related
-- where both say so, for the row-mask kind check), share, hasEn, alwaysRdy, and rdyRet when given.
finAnn :: String -> Int -> [[Int]] -> [[Int]] -> [[Int]] -> [Bool] -> [Bool] -> [Bool] -> Maybe [[Int]] -> String
finAnn nm n prec sbr conf share en rdy rr =
    concat [ "def " ++ nm ++ s ++ "T : Tab Nat :=\n  " ++ tabOr "0" (map (show . bitsOf) t) ++ "\n"
           | (s, t) <- concat [ [(r, x), (r ++ "C", cols x)] | (r, x) <- [("Prec", prec), ("Sbr", sbr), ("Conf", conf)] ]
                       ++ maybe [] (\x -> [("RdyRet", x)]) rr ]
    ++ concat [ "def " ++ nm ++ s ++ "B : Nat := " ++ show (bitsOf [ i | (i, True) <- zip [0 ..] t ]) ++ "\n"
              | (s, t) <- [("Share", share), ("En", en), ("Rdy", rdy)] ]
    ++ "def " ++ nm ++ " : Ann (Fin " ++ show n ++ ") where\n"
    ++ concat [ "  " ++ f ++ " p q := (" ++ nm ++ s ++ "T.get p.val).testBit q.val && (" ++ nm ++ s ++ "CT.get q.val).testBit p.val\n"
              | (f, s) <- [("prec", "Prec"), ("sbr", "Sbr"), ("conf", "Conf")] ]
    ++ "  share m := " ++ nm ++ "ShareB.testBit m.val\n  hasEn m := " ++ nm ++ "EnB.testBit m.val\n  alwaysEn _ := false\n"
    ++ "  alwaysRdy m := " ++ nm ++ "RdyB.testBit m.val\n"
    ++ maybe "" (const ("  rdyRet a p := (" ++ nm ++ "RdyRetT.get a.val).testBit p.val\n")) rr
  where bitsOf is = foldl (\acc i -> acc .|. (1 `shiftL` i)) (0 :: Integer) is
        -- each column's rows, from the rows (one pass over the pairs)
        cols x = let m = M.fromListWith (flip (++)) [ (b, [a]) | (a, bs) <- zip [0 :: Int ..] x, b <- bs ]
                 in [ M.findWithDefault [] b m | b <- [0 .. n - 1] ]

usesTab :: EC -> MPort -> String
usesTab ec p =
    let us = M.findWithDefault [] p (ecU ec)
        by = M.fromListWith (flip (++)) [ (k, [i]) | (k, i) <- us ]
        ks = M.keys by
        kv = Sq.fromList ks
        go lo hi | hi - lo == 1 = let k = Sq.index kv lo in "(.leaf (" ++ show k ++ ", [" ++ intercalate ", " (map show (by M.! k)) ++ "]))"
                 | otherwise = let mid = (lo + hi) `div` 2 in "(.node " ++ show (Sq.index kv mid) ++ " " ++ go lo mid ++ " " ++ go mid hi ++ ")"
    in if null ks then "(.leaf (0, []))" else go 0 (length ks)

usesUs :: EC -> MPort -> String
usesUs ec p = "[" ++ intercalate ", " (map show (S.toAscList (S.fromList (map fst (M.findWithDefault [] p (ecU ec)))))) ++ "]"

emitUses :: EC -> W ()
emitUses ec = do
    let kinds = ecKinds ec
        m = ecM ec
        nT = ecNT ec
        kdOf = kd ec
        lps k = intercalate ", " (fromMaybe [] (kdLPorts (kdOf k)))
        plist k = if isSynthKind k then "List.finRange " ++ drop 4 (mty k) else "[" ++ lps k ++ "]"
        mty k = fromMaybe "" (kdMType (kdOf k))
    forM_ kinds $ \k -> if k == "tasks"
        then do
            stab "tusesT" "Tab (Tab (Nat × List Nat))" [ usesTab ec ("tasks", 0, PT t) | t <- [0 .. nT - 1] ]
            stab "tusT" "Tab (List Nat)" [ usesUs ec ("tasks", 0, PT t) | t <- [0 .. nT - 1] ]
            w "def uses_tasks : Tab (Uses (KI .tasks).M) := .leaf ⟨fun t => tusesT.get t.val, fun t => tusT.get t.val⟩"
        else if isSynthKind k then stab ("uses_" ++ k) ("Tab (Uses (KI ." ++ k ++ ").M)")
                  [ "finU " ++ tabOr ".leaf (0, [])" [ usesTab ec (k, i, pm) | pm <- kdPorts (kdOf k) ] ++ "\n  "
                    ++ tabOr "[]" [ usesUs ec (k, i, pm) | pm <- kdPorts (kdOf k) ]
                  | i <- [0 .. length (mByk m M.! k) - 1] ]
        else stab ("uses_" ++ k) ("Tab (Uses (KI ." ++ k ++ ").M)")
                  [ fromMaybe "" (kdUsesc (kdOf k)) ++ " " ++ unwords [ usesTab ec (k, i, pm) | pm <- kdPorts (kdOf k) ]
                    ++ " " ++ unwords [ usesUs ec (k, i, pm) | pm <- kdPorts (kdOf k) ]
                  | i <- [0 .. length (mByk m M.! k) - 1] ]
    w ("def usesK : (κ : Kind) → Tab (Uses (KI κ).M)\n" ++ concat [ "  | ." ++ k ++ " => uses_" ++ k ++ "\n" | k <- kinds ])
    w (if not (ecMet ec) then usesNoMet else usesMet)
    forM_ kinds $ \k -> if k == "tasks"
        then w "theorem usOK_tasks' : Tab.allB (fun t T => usersT T == tusT.get t) tusesT 0 nT = true := by decide +kernel"
        else do
            w (fmt "theorem usOK_%s : Tab.allB (fun _ U => (%s : List (KI .%s).M).all (fun m => usersT (U.uses m) == U.us m)) uses_%s 0 (nk .%s) = true := by\n  decide +kernel"
                   [k, plist k, k, k, k])
            if k == "reg"
                then w "theorem portsU_reg : ∀ p : CRegM 1, p ∈ [Reg.read, Reg.write]\n  | .rd ⟨0, _⟩ => by simp\n  | .wr ⟨0, _⟩ => by simp"
                else if isSynthKind k
                -- a synthesized kind's ports are List.finRange (no list written or decided)
                then w (fmt "theorem portsU_%s : ∀ p : %s, p ∈ List.finRange %s := List.mem_finRange" [k, mty k, drop 4 (mty k)])
                else w (fmt "theorem portsU_%s : ∀ p : (KI .%s).M, p ∈ [%s] := by intro p; cases p <;> simp" [k, k, lps k])
    w ("/-- The users list is the uses table's (decided per kind). -/\n\
\theorem usersT_eq (p : Σ j : I, (CI j).M) : usersT (usesAt p) = usL p := by\n\
\  obtain ⟨⟨κ, i, hi⟩, m⟩ := p\n\
\  cases κ\n" ++ concat [ if k == "tasks"
                              then "  · have h := Tab.allB_spec _ tusesT 0 nT usOK_tasks' m.val (Nat.zero_le _) m.isLt\n\
\    have hi0 : i = 0 := by simp at hi; omega\n\
\    subst hi0\n\
\    exact beq_iff_eq.mp h\n"
                              else fmt "  · have h := List.all_eq_true.mp (Tab.allB_spec _ uses_%s 0 (nk .%s) usOK_%s i (Nat.zero_le _) hi) m (portsU_%s m)\n\
\    exact beq_iff_eq.mp h\n" [k, k, k, k] | k <- kinds ])
    w (if not (ecMet ec) then memUsersNoMet else memUsersMet)
    cut

sfLeaf :: EC -> Int -> MSite -> String
sfLeaf ec k st
    | sKind st == "task" = fmt "fun V (ops : Ops V) (E : EqC V) (x : V) => ((fun ρ => %s, fun ρ => %s) : SF V)"
                               [ex ec k (sCond st) rho "x", tup [ ex ec k a rho "x" | a <- fromMaybe [] (sArgs st) ]]
    | sCond st == MTrue && sArg st == MZ = "rdSF"
    | otherwise = fmt "fun V (ops : Ops V) (E : EqC V) (x : V) => ((fun ρ => %s, fun ρ => %s) : SF V)"
                      [ex ec k (sCond st) rho "x", ex ec k (sArg st) rho "x"]

resLeaf :: EC -> Int -> String
resLeaf ec k =
    let r = step ec k
        resv = ex ec k (tRes r) rho "x"
    in case tGuard r of
        Just g | mxKind g `notElem` ["check", "false"] ->
            fmt "fun V (ops : Ops V) (E : EqC V) (x : V) (ρ : Nat → V) => bif %s then some (%s) else none" [guardRho ec k, resv]
        _ -> fmt "fun V (ops : Ops V) (E : EqC V) (x : V) (ρ : Nat → V) => some (%s)" [resv]

emitSites :: EC -> W ()
emitSites ec = do
    let nS = ecNS ec
        k0 = case ecKinds ec of
            (k : _) -> k
            [] -> error "schedlean: IndexError: a step without sites in a module without children (not emitted yet)"
        sites k = F.toList (tSites (step ec k))
    w "/-! ## Steps as numbered site tables (rules, then methods) -/\n"
    forM_ [0 .. nS - 1] $ \k -> do
        let keys = case sites k of
                [] -> ["⟨" ++ ihelper k0 ++ " 0, " ++ head (fromMaybe [] (kdLPorts (kd ec k0))) ++ "⟩"]
                ss -> [ lport ec (sPort st) | st <- ss ]
        stab ("key_" ++ show k) "Tab (Σ j : I, (CI j).M)" keys
    stab "keyT" "Tab (Tab (Σ j : I, (CI j).M))" [ "key_" ++ show k | k <- [0 .. nS - 1] ]
    stab "kT" "Tab Nat" [ nat (length (sites k)) | k <- [0 .. nS - 1] ]
    w "abbrev kR (k : Nat) : Nat := kT.get k\n"
    w "section\nvariable {V : Type}\n/-- An unconditional site that passes the dummy argument (reads, calls without data). -/\n\
\def rdSF (V : Type) (ops : Ops V) (E : EqC V) (x : V) : SF V := (fun _ => true, fun _ => ops.z)\nend\n"
    forM_ [0 .. nS - 1] $ \k ->
        stab ("sf_" ++ show k) "Tab ((V : Type) → Ops V → EqC V → V → SF V)" (case sites k of { [] -> ["rdSF"]; ss -> map (sfLeaf ec k) ss })
    stab "sfT" "Tab (Tab ((V : Type) → Ops V → EqC V → V → SF V))" [ "sf_" ++ show k | k <- [0 .. nS - 1] ]
    w "\nsection\nvariable {V : Type}\n/-- Step `k`'s sites at argument `x` (a method's argument; rules pass `z`). -/\n\
\def siteS (ops : Ops V) (E : EqC V) (k : Nat) (x : V) : Nat → Site I (fun j => (CI j).M) V :=\n\
\  mkSite (keyT.get k).get (fun i => (sfT.get k).get i V ops E x)\n\
\abbrev siteR (ops : Ops V) (E : EqC V) (k : Nat) : Nat → Site I (fun j => (CI j).M) V := siteS ops E k ops.z\nend\n"
    w "/-- Each step's result: its explicit guard, read at its position (run-time check rules: `some z`);\n\
\    a method returns its result expression. -/"
    stab "resT" "Tab ((V : Type) → Ops V → EqC V → V → (Nat → V) → Option V)" [ resLeaf ec k | k <- [0 .. nS - 1] ]
    cut

gKind :: EC -> Int -> String
gKind ec k = maybe "true" mxKind (tGuard (step ec k))

emitGuards :: EC -> W ()
emitGuards ec = do
    let m = ecM ec
        nS = ecNS ec
        rdyG (k, _, p) = not (kdAlwaysRdy (kd ec k) p)
        uc = [ foldl (.|.) (0 :: Integer) [ 1 `shiftL` i | (i, s) <- zip [0 ..] (F.toList (tSites r)), not (isUncond s), rdyG (sPort s) ] | r <- mRules m ]
        needuc = any (/= 0) uc
        multi = ecMultiBlk ec
        blk = mBlk m
        gk = gKind ec
        guarded k = gk k `notElem` ["check", "false"]
    w "/-! ## The hardware: guards, firing, run-time checks -/\n"
    when needuc $ stab "ucT" "Tab Nat" (map show uc)
    w ("def alwaysRdyB (p : Σ j : I, (CI j).M) : Bool := (CI p.1).ann.alwaysRdy p.2\n\nsection\nvariable {V : Type}\n"
       ++ (if needuc then "/-- The ready signals of a step's unconditional guarded sites, ANDed into its CAN_FIRE (a conditional\n\
\    guarded call's readiness is in its guard `G`, a readiness read's comes from the child's alias). -/\n\
\@[irreducible] def RD (oc : OC V) (k : Nat) : Bool :=\n\
\  (List.range' 0 (kR k)).all (fun i => alwaysRdyB ((keyT.get k).get i) || (ucT.get k).testBit i ||\n\
\    (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2)\n"
           else "/-- The ready signals of a rule's guarded calls, ANDed into its CAN_FIRE (calls are unconditional). -/\n\
\@[irreducible] def RD (oc : OC V) (k : Nat) : Bool :=\n\
\  (List.range' 0 (kR k)).all (fun i => alwaysRdyB ((keyT.get k).get i) || (oc ((keyT.get k).get i).1).rdy ((keyT.get k).get i).2)\n"))
    forM_ [0 .. nS - 1] $ \k -> when (guarded k) $
        w (fmt "def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := %s" [show k, gDef ec k])
    w "def bTrue (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := true"
    let visit (done, order) k
            | S.member k done = (done, order)
            | otherwise = let (d', o') = foldl visit (done, order) (M.findWithDefault [] k blk)
                          in (S.insert k d', o' ++ [k])
        (_, order) = foldl visit (S.empty, []) [ k | k <- [0 .. nS - 1], guarded k ]
    forM_ order $ \k -> do
        when (M.member k blk) $
            if multi
                then w (fmt "def B_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := !(orL [%s])"
                            [show k, intercalate ", " [ "WFd_" ++ show b ++ " V ops E oc" | b <- blk M.! k ]])
                else w (fmt "def B_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := !(WFd_%d V ops E oc)" [show k, show (head (blk M.! k))])
        let bname = if M.member k blk then "B_" ++ show k else "bTrue"
        w (fmt "def WFd_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := (RD oc %d && G_%d V ops E oc) && %s V ops E oc"
               [show k, show k, show k, bname])
    w "end\n"
    w "/-- bsc's `mutually_exclusive` groups whose checks are suffixes (`SufAsm`), in bsc's order. -/"
    stab "grpT" "Tab (List Nat)" (case ecGroups ec of { [] -> ["[]"]; gs -> [ "[" ++ intercalate ", " (map show g) ++ "]" | g <- gs ] })
    w "section\nvariable {V : Type}\n"
    let chkG = ecChkG ec
        checks = listArray (0, length (mChecks m) - 1) (mChecks m)
    unless (M.null chkG) $ do
        w "def bFalse (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := false"
        stab "wfdT" "Tab ((V : Type) → Ops V → EqC V → OC V → Bool)" [ if guarded k then "WFd_" ++ show k else "bFalse" | k <- [0 .. nS - 1] ]
    forM_ [0 .. nS - 1] $ \k -> case tGuard (step ec k) of
        Just (MChkI c) | Just (g, j) <- M.lookup c chkG ->
            w (fmt "def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := WFd_%d V ops E oc && orL (((grpT.get %d).drop %d).map (fun y => (wfdT.get y) V ops E oc))"
                   [show k, show (ckHead (checks ! c)), show g, show (j + 1)])
        Just (MChkI c) | length (ckHeads (checks ! c)) > 1 -> let ch = checks ! c
                          in w (fmt "def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := orL [%s] && orL [%s]"
                                    [show k, intercalate ", " [ "WFd_" ++ show y ++ " V ops E oc" | y <- ckHeads ch ]
                                    , intercalate ", " [ "WFd_" ++ show y ++ " V ops E oc" | y <- ckLst ch ]])
        Just (MChkI c) -> let ch = checks ! c
                          in w (fmt "def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := WFd_%d V ops E oc && orL [%s]"
                                    [show k, show (ckHead ch), intercalate ", " [ "WFd_" ++ show y ++ " V ops E oc" | y <- ckLst ch ]])
        Just MFalse -> w (fmt "def G_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool := false" [show k])
        _ -> return ()
    w "end\n"
    stab "gT" "Tab ((V : Type) → Ops V → EqC V → OC V → Bool)" [ "G_" ++ show k | k <- [0 .. nS - 1] ]
    w ("def bT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  " ++ tab [ if M.member k blk then "B_" ++ show k else "bTrue" | k <- [0 .. nS - 1] ])
    let litc = ecLitc ec
        disj = mDisj m
    stab "chkHead" "Tab Nat" (case litc of { [] -> ["0"]; _ -> [ show (ckHead (checks ! c)) | c <- litc ] })
    stab "chkLst" "Tab (List Nat)" (case litc of { [] -> ["[]"]; _ -> [ "[" ++ intercalate ", " (map show (ckLst (checks ! c))) ++ "]" | c <- litc ] })
    w ("abbrev nC : Nat := " ++ show (length litc))
    w ("abbrev nG : Nat := " ++ show (length (ecGroups ec)))
    stab "dAT" "Tab Nat" (case disj of { [] -> ["0"]; _ -> [ show a | (a, _) <- disj ] })
    stab "dBT" "Tab Nat" (case disj of { [] -> ["0"]; _ -> [ show b | (_, b) <- disj ] })
    w ("abbrev nD : Nat := " ++ show (length disj))
    let cfs = mCfS m
        joinedN = length [ () | c <- litc, length (ckHeads (checks ! c)) > 1 ]
    w (fmt cfWfText [ecWXM ec] ++ (if not (ecMet ec) then stepOnNoMet else stepOnMet) ++ cfText ec ++ asmTextOf (joinedN > 0) (not (null cfs)))
    -- each conflict_free check from asm, for the task statement that reports it
    unless (null cfs) $
        w ("section\nvariable {V : Type}\n/-- A conflict_free check's report: its step never makes it, and its enable is false where\n\
\    the check passes (`asm`). -/\n\
\theorem en_tcf {a b : Bool} (h : (a && b) = false) : (a && b) = true ↔ a = true ∧ false = true := by\n\
\  rw [h]; exact ⟨fun h' => absurd h' Bool.false_ne_true, fun h' => absurd h'.2 Bool.false_ne_true⟩\n"
           ++ concat [ fmt "theorem cfA_%d (ops : Ops V) (E : EqC V) (oc : OC V) (hA : asmP ops E oc) :\n\
\    (WF ops E oc %d && cfL_%d V ops E oc) = false := hA.2.2 %d rfl\n" [show c, show (cfRule cf), show c, show c]
                     | (c, cf) <- zip [0 :: Int ..] cfs ]
           ++ "end\n")
    cut

-- | bsc's conflict_free checks as the file states them: each one's line and rule (empty without any).
cfText :: EC -> String
cfText ec = case mCfS (ecM ec) of
    [] -> ""
    cfs -> "/-! bsc's `conflict_free` checks (design §5): check `c` in rule `cfR.get c` is `!(WF R && cfL_c)`, `cfL_c`\n\
\    its line with each instrumentation wire read as its one unconditional writer's argument (a condition\n\
\    over that writer's reads; trust point 3). -/\n"
           ++ concat [ fmt "/-- In %s, bsc's line `%s`. -/\ndef cfL_%d (V : Type) (ops : Ops V) (E : EqC V) (oc : OC V) : Bool :=\n  %s\n"
                           [tName (step ec (cfRule cf)), unwords (words (cfLine cf)), show c, cfHw ec cf]
                     | (c, cf) <- zip [0 :: Int ..] cfs ]
           ++ "def cfR : Tab Nat :=\n  " ++ tab (map (show . cfRule) cfs) ++ "\n"
           ++ "def cfT : Tab ((V : Type) → Ops V → EqC V → OC V → Bool) :=\n  " ++ tab [ "cfL_" ++ show c | c <- [0 .. length cfs - 1] ] ++ "\n"
           ++ "abbrev nF : Nat := " ++ show (length cfs) ++ "\n"


-- | CAN_FIRE's guard part: the explicit guard, and bsc's lifted readiness of conditional guarded calls.
gDef :: EC -> Int -> String
gDef ec k =
    let base = hwGuard ec k
        rdc = ecRDC ec ! k
        parts = [ "(!(" ++ condHw ec k (site ec k i) ++ ") || " ++ rdyHw ec (sitePort ec k i) ++ ")" | i <- rdc ]
    in if null rdc then base else "(" ++ base ++ ") && (" ++ andterm (parts ++ ["true"]) ++ ")"

localLemmas :: String
localLemmas = "section\n\
\variable {V : Type}\n\
\/-- A split use of conditional reads: equal selections, and equal values where selected. -/\n\
\theorem split_val {α : Type} {b b' : Bool} {x1 x2 y1 y2 : α} (hb : b = b') (h1 : b' = true → x1 = y1)\n\
\    (h2 : x2 = y2) : (bif b then x1 else x2) = (bif b' then y1 else y2) := by\n\
\  subst hb; cases b <;> simp_all\n\
\/-- Urgency with several blockers: `a && !(b₁ || …)` excludes each `bᵢ`. -/\n\
\theorem blk_excl_or {a b : Bool} {l : List Bool} {p : Nat} (h : (a && !orL l) = true) (hl : l[p]? = some b)\n\
\    (hb : b = true) : False := by\n\
\  have : orL l = true := orL_true.mpr ⟨b, List.mem_of_getElem? hl, hb⟩\n\
\  rw [this] at h; simp at h\n\
\theorem iff_of_eqB {a b : Bool} (h : a = b) : a = true ↔ b = true := by rw [h]\n\
\/-- A conditional read's site value, its condition rewritten. -/\n\
\theorem ite_bcongr {α : Type} {c c' : Bool} (h : c = c') (a b : α) :\n\
\    (if c = true then a else b) = (if c' = true then a else b) := by rw [h]\n\
\/-- An R term over its own read's site value: the readiness fact gives the invariance. -/\n\
\theorem inv_self {G : V → Bool} {c : Bool} {p : V} (h : ∀ t, G (if c = true then p else t) = true → c = true)\n\
\    (t : V) : G (if c = true then p else t) = G p := by\n\
\  cases c with\n\
\  | true => rfl\n\
\  | false =>\n\
\    have h1 : G t ≠ true := fun e => absurd (h t e) (by decide)\n\
\    have h2 : G p ≠ true := fun e => absurd (h p e) (by decide)\n\
\    simp only [Bool.false_eq_true, ite_false]\n\
\    cases ht : G t <;> cases hp : G p <;> simp_all\n\
\theorem cond_congr {α : Type} {b b' : Bool} {x x' y y' : α} (hb : b = b') (hx : x = x') (hy : y = y') :\n\
\    (bif b then x else y) = (bif b' then x' else y') := by subst hb hx hy; rfl\n\
\/-- One user's enable entry, from its firing and its call condition. -/\n\
\theorem en_cnd {a c : Bool} {On C : Prop} (hOn : On ↔ a = true) (hC : a = true → (C ↔ c = true)) :\n\
\    (a && c) = true ↔ On ∧ C := by\n\
\  rw [Bool.and_eq_true]\n\
\  constructor\n\
\  · intro h; exact ⟨hOn.mpr h.1, (hC h.1).mpr h.2⟩\n\
\  · intro h; exact ⟨hOn.mp h.1, (hC (hOn.mp h.1)).mp h.2⟩\n\
\theorem en_unc {a : Bool} {On C : Prop} (hOn : On ↔ a = true) (hC : a = true → C) : a = true ↔ On ∧ C :=\n\
\  ⟨fun h => ⟨hOn.mpr h, hC h⟩, fun h => hOn.mp h.1⟩\n\
\/-- A port's enable over explicit per-user entries. -/\n\
\theorem en_pairs (P : Parent V) (fire : ((j : P.I) → Outs (P.CI j).M V) → Wires P.X.M V → P.PR → Bool)\n\
\    (oc : (j : P.I) → Outs (P.CI j).M V) (w : Wires P.X.M V) (j : P.I) (cm : (P.CI j).M) :\n\
\    ∀ (l : List (Step P × Bool)), AllL (fun e => e.2 = true ↔ On P fire oc w e.1 ∧ Calls P (trS P oc w e.1) j cm) l →\n\
\      (orL (l.map Prod.snd) = true ↔ ∃ s ∈ l.map Prod.fst, On P fire oc w s ∧ Calls P (trS P oc w s) j cm)\n\
\  | [], _ => by simp [orL]\n\
\  | e :: l, ⟨h1, h2⟩ => by\n\
\    have h1' : e.2 = true ↔ On P fire oc w e.1 ∧ Calls P (trS P oc w e.1) j cm := h1\n\
\    simp only [List.map_cons, orL, Bool.or_eq_true, List.mem_cons, exists_eq_or_imp]\n\
\    rw [h1', en_pairs P fire oc w j cm l h2]\n\
\theorem mem_callsOf_inv {I : Type} {CM : I → Type} (ρ : Nat → V) : ∀ (ss : List (Site I CM V)) (c : Σ j, CM j × V),\n\
\    c ∈ callsOf ρ ss → ∃ s ∈ ss, s.cond ρ = true ∧ c = ⟨s.j, (s.m, s.arg ρ)⟩\n\
\  | [], c, h => by simp [callsOf] at h\n\
\  | s :: ss, c, h => by\n\
\    simp only [callsOf, List.mem_append] at h\n\
\    rcases h with h | h\n\
\    · by_cases hc : s.cond ρ = true\n\
\      · rw [if_pos hc] at h; simp only [List.mem_singleton] at h; exact ⟨s, List.mem_cons_self .., hc, h⟩\n\
\      · rw [if_neg hc] at h; simp at h\n\
\    · obtain ⟨s', hs', h1, h2⟩ := mem_callsOf_inv ρ ss c h\n\
\      exact ⟨s', List.mem_cons_of_mem _ hs', h1, h2⟩\n\
\end\n"

fifoText :: String
fifoText = "/-- FIFOL1's ports. -/\n\
\inductive FL where\n\
\  | enq | deq | first | notFull | i_notFull | notEmpty | i_notEmpty | clear\n\
\  deriving DecidableEq\n\
\\n\
\/-- `FIFOL1`'s schedule, as the dump prints it. -/\n\
\def flSched : List (List FL × Rel × List FL) :=\n\
\  %s\n\
\\n\
\/-- FIFOL1 (assumed leaf): its schedule; `deq` has paths to `enq`'s ready and the full flags. -/\n\
\def flAnn : Ann FL :=\n\
\  { Ann.ofSched flSched (fun m => match m with | .enq | .deq | .clear => false | _ => true)\n\
\      (fun m => match m with | .enq | .deq | .clear => true | _ => false) (fun _ => false)\n\
\      (fun m => match m with | .enq | .deq | .first => false | _ => true) with\n\
\    path := fun p q => match p, q with\n\
\      | .deq, .enq | .deq, .notFull | .deq, .i_notFull | .deq, .clear | .enq, .clear => true\n\
\      | _, _ => false }\n\
\\n\
\abbrev FL.X : Iface := ⟨FL, inferInstance, flAnn⟩\n"

cfgLibText :: String
cfgLibText = "open LC (CfgM)\n\
\\n\
\/-- ConfigRegN: LastCaller's model, its schedule checked against this instance's. -/\n\
\def cfgAnn : Ann CfgM :=\n\
\  Ann.ofSched %s (fun m => m == .read) (fun m => m == .write) (fun _ => false) (fun _ => true)\n\
\theorem cfgAnn_lib : cfgAnn = LC.cfgAnn := rfl\n\
\\n\
\abbrev Cfg.X : Iface := ⟨CfgM, inferInstance, cfgAnn⟩\n"

rfText :: String
rfText = "/-- RegFile's ports: `upd`, and one port per copy of `sub` (bsc's port copies). -/\n\
\inductive RFM where\n\
\  | upd | s0 | s1 | s2 | s3 | s4\n\
\  deriving DecidableEq\n\
\def RFM.isS : RFM → Bool\n\
\  | .upd => false\n\
\  | _ => true\n\
\/-- RegFile (assumed leaf): `sub CF sub, sub SB upd, upd C upd`; a copy's calls share their\n\
\    address; the address reaches the copy's result combinationally. -/\n\
\def rfAnn : Ann RFM where\n\
\  prec p q := p.isS && !q.isS\n\
\  sbr _ _ := false\n\
\  conf p q := !p.isS && !q.isS\n\
\  share m := m.isS\n\
\  hasEn m := !m.isS\n\
\  alwaysEn _ := false\n\
\  alwaysRdy _ := true\n\
\  path p q := p.isS && decide (p = q)\n\
\abbrev RF.X : Iface := ⟨RFM, inferInstance, rfAnn⟩\n"

noMetText :: String
noMetText = "\n\
\def xAnn : Ann Empty where\n\
\  prec e _ := e.elim\n\
\  sbr e _ := e.elim\n\
\  conf e _ := e.elim\n\
\  share e := e.elim\n\
\  hasEn e := e.elim\n\
\  alwaysEn e := e.elim\n\
\  alwaysRdy e := e.elim\n\
\abbrev X : Iface := ⟨Empty, inferInstance, xAnn⟩\n\
\\n\
\abbrev nR : Nat := %d\n\
\abbrev nS : Nat := %d\n\
\abbrev St := Empty ⊕ Fin nR\n\
\/-- Rule `k` (rules are numbered in schedule order). -/\n\
\def rR (k : Nat) : Fin nR := ⟨k %% nR, Nat.mod_lt _ (by decide)⟩\n\
\theorem rR_val (r : Fin nR) : rR r.val = r := Fin.ext (Nat.mod_eq_of_lt r.isLt)\n\
\theorem rlt (r : Fin nR) : r.val < nS := Nat.lt_of_lt_of_le r.isLt (by decide)\n"

-- | metText's lines up to and including X (an exported module states its own xAnn instead).
metHead :: String
metHead = "\n\
\abbrev nM : Nat := %d\n\
\/-- The module's exported methods (dump order): pairwise CF, each conflicting with itself; enabled,\n\
\    with RDY outputs. -/\n\
\def xAnn : Ann (Fin nM) where\n\
\  prec _ _ := false\n\
\  sbr _ _ := false\n\
\  conf p q := decide (p = q)\n\
\  share _ := false\n\
\  hasEn _ := true\n\
\  alwaysEn _ := false\n\
\  alwaysRdy _ := false\n"

finBuilders :: String
finBuilders = "/-- A synthesized child's wires and uses, by method index in balanced tables (design §3.1). -/\n\
\def finW {V : Type} {n : Nat} (en : Tab Bool) (d : Tab V) : Wires (Fin n) V :=\n\
\  ⟨fun m => en.get m.val, fun m => d.get m.val⟩\n\
\def finU {n : Nat} (t : Tab (Tab (Nat × List Nat))) (l : Tab (List Nat)) : Uses (Fin n) :=\n\
\  ⟨fun m => t.get m.val, fun m => l.get m.val⟩\n"

metText :: String
metText = "\n\
\abbrev nM : Nat := %d\n\
\/-- The module's exported methods (dump order): pairwise CF, each conflicting with itself; enabled,\n\
\    with RDY outputs. -/\n\
\def xAnn : Ann (Fin nM) where\n\
\  prec _ _ := false\n\
\  sbr _ _ := false\n\
\  conf p q := decide (p = q)\n\
\  share _ := false\n\
\  hasEn _ := true\n\
\  alwaysEn _ := false\n\
\  alwaysRdy _ := false\n\
\abbrev X : Iface := ⟨Fin nM, inferInstance, xAnn⟩\n\
\\n\
\abbrev nR : Nat := %d\n\
\abbrev nS : Nat := %d\n\
\abbrev St := Fin nM ⊕ Fin nR\n\
\/-- Rule `k` (rules are numbered in schedule order); method `m` is step `nR + m`. -/\n\
\def rR (k : Nat) : Fin nR := ⟨k %% nR, Nat.mod_lt _ (by decide)⟩\n\
\theorem rR_val (r : Fin nR) : rR r.val = r := Fin.ext (Nat.mod_eq_of_lt r.isLt)\n\
\def mM (k : Nat) : Fin nM := ⟨k %% nM, Nat.mod_lt _ (by decide)⟩\n\
\theorem mM_val (m : Fin nM) : mM m.val = m := Fin.ext (Nat.mod_eq_of_lt m.isLt)\n\
\theorem rlt (r : Fin nR) : r.val < nS := Nat.lt_of_lt_of_le r.isLt (by decide)\n\
\theorem mlt (m : Fin nM) : nR + m.val < nS := Nat.lt_of_lt_of_le (Nat.add_lt_add_left m.isLt nR) (by decide)\n\
\/-- A step's table index: rules first, then methods. -/\n\
\def tix : St → Nat\n\
\  | .inl m => nR + m.val\n\
\  | .inr r => r.val\n\
\def stOf (t : Nat) : St := bif Nat.blt t nR then .inr (rR t) else .inl (mM (t - nR))\n\
\theorem stOf_tix (s : St) : stOf (tix s) = s := by\n\
\  cases s with\n\
\  | inl m =>\n\
\    have h : Nat.blt (nR + m.val) nR = false := Bool.eq_false_iff.mpr (fun h' => absurd (Nat.blt_eq.mp h') (Nat.not_lt.mpr (Nat.le_add_right nR m.val)))\n\
\    simp only [stOf, tix, h, cond_false, Nat.add_sub_cancel_left, mM_val]\n\
\  | inr r =>\n\
\    have h : Nat.blt r.val nR = true := Nat.blt_eq.mpr r.isLt\n\
\    simp only [stOf, tix, h, cond_true, rR_val]\n\
\/-- A method is called: enabled (as `called`, before the parent exists). -/\n\
\def calledM {V : Type} (w : Wires (Fin nM) V) (m : Fin nM) : Bool := (xAnn.hasEn m && w.en m) || xAnn.alwaysEn m\n"

buildersText :: String
buildersText = "/-! ## Per-kind builders, port splits, uses -/\n\
\\n\
\def regW {V : Type} (en : Bool) (d z : V) : Wires (CRegM 1) V :=\n\
\  ⟨fun m => match m with | .wr _ => en | .rd _ => false, fun m => match m with | .wr _ => d | .rd _ => z⟩\n\
\def wireW {V : Type} (en : Bool) (d z : V) : Wires WireM V :=\n\
\  ⟨fun m => match m with | .wset => en | _ => false, fun m => match m with | .wset => d | _ => z⟩\n\
\def wire0W {V : Type} (en : Bool) (z : V) : Wires Wire0M V :=\n\
\  ⟨fun m => match m with | .wset => en | _ => false, fun _ => z⟩\n\
\theorem fin1 (i : Fin 1) : i = 0 := by obtain ⟨k, hk⟩ := i; congr; omega\n\
\theorem regAll {Q : CRegM 1 → Prop} (hr : Q Reg.read) (hw : Q Reg.write) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm with\n\
\  | rd i => rw [fin1 i]; exact hr\n\
\  | wr i => rw [fin1 i]; exact hw\n\
\theorem wireAll {Q : WireM → Prop} (hs : Q .wset) (hg : Q .wget) (hh : Q .whas) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\theorem reg_nopath (p q : CRegM 1) : (cregAnn 1).path p q = false := by\n\
\  cases p <;> cases q <;> rename_i a b <;> rw [fin1 a, fin1 b] <;> rfl\n\
\theorem wire0All {Q : Wire0M → Prop} (hs : Q .wset) (hh : Q .whas) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\\n\
\/-- A port's uses: a table keyed by step (its sites per step), and its users as a list. -/\n\
\structure Uses (M : Type) where\n\
\  uses : M → Tab (Nat × List Nat)\n\
\  us : M → List Nat\n\
\def regU (rd wr : Tab (Nat × List Nat)) (rd' wr' : List Nat) : Uses (CRegM 1) :=\n\
\  ⟨fun m => match m with | .rd _ => rd | .wr _ => wr, fun m => match m with | .rd _ => rd' | .wr _ => wr'⟩\n\
\def wireU (s g h : Tab (Nat × List Nat)) (s' g' h' : List Nat) : Uses WireM :=\n\
\  ⟨fun m => match m with | .wset => s | .wget => g | .whas => h, fun m => match m with | .wset => s' | .wget => g' | .whas => h'⟩\n\
\def wire0U (s h : Tab (Nat × List Nat)) (s' h' : List Nat) : Uses Wire0M :=\n\
\  ⟨fun m => match m with | .wset => s | .whas => h, fun m => match m with | .wset => s' | .whas => h'⟩\n"

cfgBuilders :: String
cfgBuilders = "def cfgW {V : Type} (en : Bool) (d z : V) : Wires CfgM V :=\n\
\  ⟨fun m => match m with | .write => en | .read => false, fun m => match m with | .write => d | .read => z⟩\n\
\theorem cfgAll {Q : CfgM → Prop} (hr : Q .read) (hw : Q .write) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\def cfgU (rd wr : Tab (Nat × List Nat)) (rd' wr' : List Nat) : Uses CfgM :=\n\
\  ⟨fun m => match m with | .read => rd | .write => wr, fun m => match m with | .read => rd' | .write => wr'⟩\n"

fifoBuilders :: String
fifoBuilders = "def fifoW {V : Type} (eE dE cE : Bool) (d z : V) : Wires FL V :=\n\
\  ⟨fun m => match m with | .enq => eE | .deq => dE | .clear => cE | _ => false,\n\
\   fun m => match m with | .enq => d | _ => z⟩\n\
\theorem flAll {Q : FL → Prop} (h1 : Q .enq) (h2 : Q .deq) (h3 : Q .first) (h4 : Q .notFull) (h5 : Q .i_notFull)\n\
\    (h6 : Q .notEmpty) (h7 : Q .i_notEmpty) (h8 : Q .clear) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\def fifoU (enq deq first nf inf ne ine clr : Tab (Nat × List Nat)) (enq' deq' first' nf' inf' ne' ine' clr' : List Nat) : Uses FL :=\n\
\  ⟨fun m => match m with\n\
\    | .enq => enq | .deq => deq | .first => first | .notFull => nf | .i_notFull => inf\n\
\    | .notEmpty => ne | .i_notEmpty => ine | .clear => clr,\n\
\   fun m => match m with\n\
\    | .enq => enq' | .deq => deq' | .first => first' | .notFull => nf' | .i_notFull => inf'\n\
\    | .notEmpty => ne' | .i_notEmpty => ine' | .clear => clr'⟩\n"

f2Builders :: String
f2Builders = "def f2W {V : Type} (eE dE cE : Bool) (d z : V) : Wires FifoM V :=\n\
\  ⟨fun m => match m with | .enq => eE | .deq => dE | .clear => cE | _ => false,\n\
\   fun m => match m with | .enq => d | _ => z⟩\n\
\theorem f2All {Q : FifoM → Prop} (h1 : Q .enq) (h2 : Q .deq) (h3 : Q .first) (h4 : Q .notEmpty) (h5 : Q .notFull)\n\
\    (h6 : Q .i_notEmpty) (h7 : Q .i_notFull) (h8 : Q .clear) : ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\def f2U (enq deq first ne nf ine inf clr : Tab (Nat × List Nat)) (enq' deq' first' ne' nf' ine' inf' clr' : List Nat) : Uses FifoM :=\n\
\  ⟨fun m => match m with\n\
\    | .enq => enq | .deq => deq | .first => first | .notEmpty => ne | .notFull => nf\n\
\    | .i_notEmpty => ine | .i_notFull => inf | .clear => clr,\n\
\   fun m => match m with\n\
\    | .enq => enq' | .deq => deq' | .first => first' | .notEmpty => ne' | .notFull => nf'\n\
\    | .i_notEmpty => ine' | .i_notFull => inf' | .clear => clr'⟩\n"

rfBuilders :: String
rfBuilders = "def rfW {V : Type} (eU : Bool) (dU a0 a1 a2 a3 a4 : V) : Wires RFM V :=\n\
\  ⟨fun m => match m with | .upd => eU | _ => false,\n\
\   fun m => match m with | .upd => dU | .s0 => a0 | .s1 => a1 | .s2 => a2 | .s3 => a3 | .s4 => a4⟩\n\
\theorem rfAll {Q : RFM → Prop} (h0 : Q .upd) (h1 : Q .s0) (h2 : Q .s1) (h3 : Q .s2) (h4 : Q .s3) (h5 : Q .s4) :\n\
\    ∀ cm, Q cm := by\n\
\  intro cm; cases cm <;> assumption\n\
\def rfU (upd a0 a1 a2 a3 a4 : Tab (Nat × List Nat)) (upd' a0' a1' a2' a3' a4' : List Nat) : Uses RFM :=\n\
\  ⟨fun m => match m with | .upd => upd | .s0 => a0 | .s1 => a1 | .s2 => a2 | .s3 => a3 | .s4 => a4,\n\
\   fun m => match m with | .upd => upd' | .s0 => a0' | .s1 => a1' | .s2 => a2' | .s3 => a3' | .s4 => a4'⟩\n"

usesNoMet :: String
usesNoMet = "/-- A port's uses: a table keyed by rule, so a rule's sites on the port are a lookup (no walk). -/\n\
\def usesAt (p : Σ j : I, (CI j).M) : Tab (Nat × List Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2\n\
\def usL (p : Σ j : I, (CI j).M) : List Nat := ((usesK p.1.1).get p.1.2.val).us p.2\n\
\def users (j : I) (cm : (CI j).M) : List St := (usL ⟨j, cm⟩).map (fun k => .inr (rR k))\n\
\def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := match s with\n\
\  | .inl e => e.elim\n\
\  | .inr r => idxT (usesAt p) r.val\n"

usesMet :: String
usesMet = "/-- A port's uses: a table keyed by step index, so a step's sites on the port are a lookup. -/\n\
\def usesAt (p : Σ j : I, (CI j).M) : Tab (Nat × List Nat) := ((usesK p.1.1).get p.1.2.val).uses p.2\n\
\def usL (p : Σ j : I, (CI j).M) : List Nat := ((usesK p.1.1).get p.1.2.val).us p.2\n\
\def users (j : I) (cm : (CI j).M) : List St := (usL ⟨j, cm⟩).map stOf\n\
\def idx (s : St) (p : Σ j : I, (CI j).M) : List Nat := idxT (usesAt p) (tix s)\n"

memUsersNoMet :: String
memUsersNoMet = "theorem mem_users {r : Fin nR} {p : Σ j : I, (CI j).M} {i : Nat} (h : i ∈ idx (.inr r) p) : (.inr r : St) ∈ users p.1 p.2 := by\n\
\  have h1 := mem_usersT h\n\
\  rw [usersT_eq] at h1\n\
\  have h' := List.mem_map_of_mem (f := fun k => (.inr (rR k) : St)) h1\n\
\  rw [rR_val] at h'\n\
\  exact h'\n\
\/-- A fact for each user of a port, from a balanced proof over its uses table (no list proof). -/\n\
\theorem users_all {j : I} {cm : (CI j).M} {Q : St → Prop}\n\
\    (h : Tab.All (fun e => e.2.isEmpty = false → Q (.inr (rR e.1))) (usesAt ⟨j, cm⟩)) : ∀ s ∈ users j cm, Q s := by\n\
\  intro s hs\n\
\  obtain ⟨k, hk, rfl⟩ := List.mem_map.mp hs\n\
\  rw [← usersT_eq] at hk\n\
\  exact usersT_all (Q := fun k => Q (.inr (rR k))) h k hk\n"

memUsersMet :: String
memUsersMet = "theorem mem_users {s : St} {p : Σ j : I, (CI j).M} {i : Nat} (h : i ∈ idx s p) : s ∈ users p.1 p.2 := by\n\
\  have h1 := mem_usersT h\n\
\  rw [usersT_eq] at h1\n\
\  have h' := List.mem_map_of_mem (f := stOf) h1\n\
\  rw [stOf_tix] at h'\n\
\  exact h'\n\
\theorem users_all {j : I} {cm : (CI j).M} {Q : St → Prop}\n\
\    (h : Tab.All (fun e => e.2.isEmpty = false → Q (stOf e.1)) (usesAt ⟨j, cm⟩)) : ∀ s ∈ users j cm, Q s := by\n\
\  intro s hs\n\
\  obtain ⟨k, hk, rfl⟩ := List.mem_map.mp hs\n\
\  rw [← usersT_eq] at hk\n\
\  exact usersT_all (Q := fun k => Q (stOf k)) h k hk\n"

cfWfText :: String
cfWfText = "\n\
\section\n\
\variable {V : Type}\n\
\/-- CAN_FIRE: the guarded calls' ready signals and the step's guard. -/\n\
\def CF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := RD oc k && (gT.get k) V ops E oc\n\
\/-- WILL_FIRE: CAN_FIRE and not blocked by a more urgent rule (a method's is its CAN_FIRE). -/\n\
\def WF (ops : Ops V) (E : EqC V) (oc : OC V) (k : Nat) : Bool := CF ops E oc k && (bT.get k) V ops E oc\n\
\def fire (ops : Ops V) (E : EqC V) : OC V → %s → Fin nR → Bool := fun oc _ r => WF ops E oc r.val\n"

stepOnNoMet :: String
stepOnNoMet = "def stepOn (ops : Ops V) (E : EqC V) (oc : OC V) : St → Bool\n\
\  | .inl e => e.elim\n\
\  | .inr r => WF ops E oc r.val\n"

stepOnMet :: String
stepOnMet = "def stepOn (ops : Ops V) (E : EqC V) (oc : OC V) (w : Wires (Fin nM) V) : St → Bool\n\
\  | .inl m => calledM w m\n\
\  | .inr r => WF ops E oc r.val\n"

-- | asmP and its accessors: with lines of several rules on their first side (@joined@), and with
-- conflict_free checks (@cf@, a third conjunct); with neither, round 9a's text.
asmTextOf :: Bool -> Bool -> String
asmTextOf joined cf = "/-- The module's assertions: bsc's run-time checks of `mutually_exclusive`, each `!(WF h && (WF y || …))`\n\
\    exactly as bsc's line (one rule on its first side), a group's checks as one `SufAsm`"
    ++ (if joined then ";\n    a line `(WF h₁ || …) && (WF y || …)` as one such check per `hᵢ` (together exactly the line)" else "")
    ++ (if cf then ";\n    and bsc's `conflict_free` checks, each `!(WF R && cfL_c)`" else "")
    ++ ". -/\n\
\def asmP (ops : Ops V) (E : EqC V) (oc : OC V) : Prop :=\n\
\  (∀ g, Nat.blt g nG = true → SufAsm (WF ops E oc) (grpT.get g)) ∧\n"
    ++ (if not cf then "  ∀ c, Nat.blt c nC = true → (WF ops E oc (chkHead.get c) && orL ((chkLst.get c).map (WF ops E oc))) = false\n"
        else "  (∀ c, Nat.blt c nC = true → (WF ops E oc (chkHead.get c) && orL ((chkLst.get c).map (WF ops E oc))) = false) ∧\n\
\  ∀ c, Nat.blt c nF = true → (WF ops E oc (cfR.get c) && (cfT.get c) V ops E oc) = false\n")
    ++ asmRest

asmRest :: String
asmRest = "\
\theorem cf_of_wf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : WF ops E oc k = true) : CF ops E oc k = true :=\n\
\  ((Bool.and_eq_true _ _).mp h).1\n\
\theorem rd_of_cf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : CF ops E oc k = true) : RD oc k = true :=\n\
\  ((Bool.and_eq_true _ _).mp h).1\n\
\theorem g_of_cf {ops : Ops V} {E : EqC V} {oc : OC V} {k : Nat} (h : CF ops E oc k = true) : (gT.get k) V ops E oc = true :=\n\
\  ((Bool.and_eq_true _ _).mp h).2\n\
\\n\
\end\n"
