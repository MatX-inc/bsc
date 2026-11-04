-- | What the emitter's sections share: the module's plan (model, scan, facts,
-- muxes) and the renderings of a step's expressions (mke2e.py lines 662-876).
module Lean.Sched.Emit.Ctx
    ( EC(..), PInfo(..), HwEdit(..), VCtx(..), vctx, site, step, kd, ihelper, lport, lportOf, ocread, sitePort, hierForm, exported
    , hasCf, asmChk, cfHw
    , isUncond, isCond, ex, rterm, rho, hw, xhw, condHw, argHw, guardHw, hwGuard, guardRho, svHw, rsv, yof
    , val, evt, evst, evct, condVal, argVal, rdyHw, excl, entry, entryOn, fsrc, mxKind
    ) where

import Data.Array (Array, (!))
import Data.Char (toUpper)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M (Map, (!), empty, findWithDefault, lookup, insert)
import qualified Data.Set as S (Set)
import qualified Data.Sequence as Sq (index)

import Lean.Print (andc, andget, andterm, tup, tupcong)
import Lean.Sched.Export (Export, SynthKid)
import Lean.Sched.Kinds (kdCode, KindDef(kdLPorts, kdLPortA), PM(PT))
import Lean.Sched.Model (mxRepr, CfC(CfWF, CfX), InvRef(InvFact, InvSelf), MCf(cfConj), MPort, MSite(sArg, sPort, sR,
    sYsv, sInv, sFact, sCKind, sCond, sKind, sArgs), MStep(tGuard, tSites), MX(..), Model(mMEFind, mK, mBlk, mDFind, mCfS))
import Lean.Sched.Scan (ScanDesign, Ent)

-- | A port's arms: its users, how its argument is chosen ('none', 'const', 'single', 'mux', 'lc'),
-- and for a mux its number, members in mux order and positions.
data PInfo = PInfo { piUsers :: [(Int, Int)], piArgk :: String, piQ :: Int, piMus :: [(Int, Int)]
                   , piMargs :: [MSite], piMpos :: M.Map Int Int }

-- | One change to the hardware the file states (a mutant, "Lean.Sched.Mutate"); the steps'
-- sites, and so the obligation's other side, stay as they are.
data HwEdit = NoEdit
    | EnDrop MPort Int              -- ^ the port's enable leaves out its user n
    | EnForce MPort                 -- ^ the port's enable is on in every cycle
    | TaskEn Int Bool               -- ^ task statement t enabled never (False) or always (True)
    | ArgWrap MPort Int             -- ^ user n's argument under a fresh operator
    | TaskArgWrap Int               -- ^ task statement t's arguments under a fresh operator
    | MuxSwap MPort Int             -- ^ mux members n and n+1 exchange their arguments
    | GuardRead Int Int (Maybe Int) -- ^ step k's guard reads site j' (Nothing: a constant) for read j
    | LcReverse MPort               -- ^ a last-caller mux's arms reversed: the first caller wins
    | CondDrop MPort Int            -- ^ the port's enable takes user n without its call condition
    deriving (Eq)

data EC = EC
    { ecM :: Model, ecName :: String
    , ecRules :: Array Int MStep
    , ecNR, ecNM, ecNS, ecNT :: Int, ecMet :: Bool
    , ecKinds :: [String]
    , ecGroups :: [[Int]], ecChkG :: M.Map Int (Int, Int), ecLitc :: [Int], ecLiti :: M.Map Int Int
    , ecLCP :: [MPort], ecVB :: M.Map MPort Int, ecKV :: Int, ecCfgLib :: Bool, ecVirt :: [[Ent]]
    , ecNSlots, ecD :: Int
    , ecNeed, ecEsc :: [Integer], ecDn :: ScanDesign
    , ecESC :: [(Int, Int, [(Int, Int)])]
    , ecU :: M.Map MPort [(Int, Int)]
    , ecRDC :: Array Int [Int]
    , ecFactOk :: Array Int Bool, ecFactK :: M.Map Int Int
    , ecEFacts :: [(Int, Int, Int, Int, Maybe String, Maybe String)]
    , ecCFacts :: [(Int, Int, Int, Int, Int, Maybe String, Maybe String)]
    , ecCFIdx :: M.Map (Int, Int, Int) (Int, Maybe String)
    , ecMuxes :: [(MPort, [(Int, Int)], M.Map Int (Maybe Int, S.Set Int))]
    , ecSelmux :: M.Map Int Bool, ecMgrp :: M.Map Int (Int, [Int])
    , ecLCMux :: [(MPort, [(Int, Int)])]
    , ecPortInfo :: M.Map MPort PInfo
    , ecMultiBlk :: Bool
    , ecHasUnconf :: Bool, ecHyp, ecHya :: String
    , ecWXM, ecXM :: String
    , ecMann :: Maybe ([Integer], [Integer], [Integer], [Integer], [Integer])
    , ecEdit :: HwEdit
    , ecDomNote :: String          -- ^ the note on gated clocks and resets (Lean.Sched.Domain.domainNote)
    , ecExport :: Maybe Export     -- ^ written as a synthesized child: its exported interface (design.md §3.1)
    , ecSynth :: [(String, SynthKid)]  -- ^ its synthesized children's kinds, in kind order
    }

-- | The built module in the hierarchy form: implK gives ModuleU, with Subs (a child, or a parent).
hierForm :: EC -> Bool
hierForm ec = exported ec || not (null (ecSynth ec))

-- | The file states bsc's conflict_free checks: asmP's third conjunct (the checks' part is then .2.1).
hasCf :: EC -> Bool
hasCf ec = not (null (mCfS (ecM ec)))

-- | asmP's part for the single mutually_exclusive checks, from h : asmP ops E oc.
asmChk :: EC -> String -> String
asmChk ec h = h ++ (if hasCf ec then ".2.1" else ".2")

-- | conflict_free check c's line in the hardware: its conjuncts, each over its own step's reads.
cfHw :: EC -> MCf -> String
cfHw ec cf = foldr1 (\a b -> a ++ " && " ++ b) (map one (cfConj cf))
  where one (CfWF k) = "WF ops E oc " ++ show k
        one (CfX k e) = ex ec k e (hw ec k) (xhw ec k)

-- | Written as a synthesized child (design.md §3.1).
exported :: EC -> Bool
exported ec = isJust (ecExport ec)

step :: EC -> Int -> MStep
step ec k = ecRules ec ! k

site :: EC -> Int -> Int -> MSite
site ec k i = Sq.index (tSites (step ec k)) i

kd :: EC -> String -> KindDef
kd ec k = mK (ecM ec) M.! k

ihelper :: String -> String
ihelper (c : r) = 'i' : toUpper c : r
ihelper [] = "i"

lportOf :: EC -> String -> PM -> String
lportOf ec k m = case kdLPortA kdef of
    Just a -> a ! kdCode kdef m
    Nothing -> fromMaybe [] (kdLPorts kdef) !! kdCode kdef m
  where kdef = kd ec k

lport :: EC -> MPort -> String
lport ec (k, i, m) = case m of
    PT t | k == "tasks" -> "⟨iTasks 0, tP " ++ show t ++ "⟩"
    _ -> "⟨" ++ ihelper k ++ " " ++ show i ++ ", " ++ lportOf ec k m ++ "⟩"

ocread :: EC -> MPort -> String
ocread ec (k, i, m) = "(oc (" ++ ihelper k ++ " " ++ show i ++ ")).ret " ++ lportOf ec k m

rdyHw :: EC -> MPort -> String
rdyHw ec (k, i, m) = "(oc (" ++ ihelper k ++ " " ++ show i ++ ")).rdy " ++ lportOf ec k m

sitePort :: EC -> Int -> Int -> MPort
sitePort ec k i = sPort (site ec k i)

isUncond :: MSite -> Bool
isUncond s = sCond s == MTrue

isCond :: EC -> Int -> Int -> Bool
isCond ec k j = isJust (sR (site ec k j))

-- | The first element of the prototype's expression tuple.
mxKind :: MX -> String
mxKind x = case x of
    MZ -> "z"; MK _ -> "k"; MXarg -> "x"; MF _ _ -> "f"; MG _ _ -> "g"; MGK _ -> "gk"; MSplit {} -> "split"
    MTrue -> "true"; MFalse -> "false"; MLit _ -> "lit"; MChkI _ -> "check"; MChk -> "chk"; MTup -> "tup"; MCfT _ -> "cf"

rOf :: EC -> Int -> Int -> (Int, [Int])
rOf ec k i = case sR (site ec k i) of
    Just (MG n js) -> (n, js)
    _ -> error ("schedcheck: site " ++ show (k, i) ++ " has no R")

-- | The reach term of conditional read site i of step k.
rterm :: EC -> Int -> Int -> (Int -> String) -> String
rterm ec k i at = let (n, js) = rOf ec k i in "ops.g " ++ show n ++ " (" ++ tup (map at js) ++ ")"

yof :: EC -> Int -> Int -> [Int]
yof ec k i = fromMaybe [] (sYsv (site ec k i))

-- | Render e over `at` (a function from a read site index to its term).
ex :: EC -> Int -> MX -> (Int -> String) -> String -> String
ex ec k e at xarg = case e of
    MZ -> "ops.z"
    MK n -> "ops.k " ++ show n
    MXarg -> xarg
    MTrue -> "true"
    MFalse -> "false"
    MGK n -> "ops.g " ++ show n ++ " ops.z"
    MLit j -> "ops.toB (" ++ at j ++ ")"
    MF n js -> "ops.f " ++ show n ++ " (" ++ tup (map at js) ++ ")"
    MG n js -> "ops.g " ++ show n ++ " (" ++ tup (map at js) ++ ")"
    MSplit is a b ->
        let rs = case is of { [i] -> rterm ec k i at; _ -> andterm [ rterm ec k i at | i <- is ] }
        in "(bif " ++ rs ++ " then " ++ ex ec k a at xarg ++ " else " ++ ex ec k b at xarg ++ ")"
    _ -> error ("schedcheck: ex: " ++ mxRepr e)

rho :: Int -> String
rho j = "ρ " ++ show j

hw :: EC -> Int -> Int -> String
hw ec k j = ocread ec (sitePort ec k j)

xhw :: EC -> Int -> String
xhw ec k = if k >= ecNR ec then "w.arg (mM " ++ show (k - ecNR ec) ++ ")" else "ops.z"

condHw :: EC -> Int -> MSite -> String
condHw ec k st = ex ec k (sCond st) (hw ec k) (xhw ec k)

argHw :: EC -> Int -> MSite -> String
argHw ec k st
    | sKind st == "task" = tup [ ex ec k a (hw ec k) (xhw ec k) | a <- fromMaybe [] (sArgs st) ]
    | otherwise = ex ec k (sArg st) (hw ec k) (xhw ec k)

guardOf :: EC -> Int -> MX
guardOf ec k = fromMaybe MTrue (tGuard (step ec k))

guardHw :: EC -> Int -> String
guardHw ec k = ex ec k (guardOf ec k) (hw ec k) (xhw ec k)

-- | The guard in CAN_FIRE (the hardware's), with a 'GuardRead' edit applied.
hwGuard :: EC -> Int -> String
hwGuard ec k = case ecEdit ec of
    GuardRead k' j r | k' == k ->
        let at i = if i /= j then hw ec k i else maybe "ops.z" (hw ec k) r
        in ex ec k (guardOf ec k) at (xhw ec k)
    _ -> guardHw ec k

guardRho :: EC -> Int -> String
guardRho ec k = ex ec k (guardOf ec k) rho "x"

-- | Site j's value in the model, over the hardware: its port where its condition holds, else t.
svHw :: EC -> Int -> Int -> String -> String
svHw ec k j t = "(if " ++ condHw ec k (site ec k j) ++ " = true then " ++ ocread ec (sitePort ec k j) ++ " else " ++ t ++ ")"

-- | R over the site values of its conditional reads, the skipped value of read j named tn j.
rsv :: EC -> Int -> Int -> (Int -> String) -> String
rsv ec k i tn = let (n, js) = rOf ec k i; y = yof ec k i
                in "ops.g " ++ show n ++ " (" ++ tup [ if j `elem` y then svHw ec k j (tn j) else ocread ec (sitePort ec k j) | j <- js ] ++ ")"

-- | The names in scope where a value proof is rendered.
data VCtx = VCtx { cV, cX, cHf :: String }

vctx :: String -> VCtx
vctx v = VCtx v "ops.z" "hf"

fsrc :: EC -> Int -> String
fsrc ec n = if ecFactOk ec ! n then "hy" else "hr"

-- | R at the site values (skipped values c.v) equals R at the ports, under c.hf.
rinv :: EC -> Int -> Int -> VCtx -> Maybe String
rinv ec k i c =
    let s = site ec k i
        y = yof ec k i
        vj j = "(" ++ cV c ++ " " ++ show j ++ ")"
    in if null y then Nothing else Just $ case sInv s of
        Just InvSelf ->
            let (n, js) = rOf ec k i
                g = "fun y => ops.g " ++ show n ++ " (" ++ tup [ if j == i then "y" else ocread ec (sitePort ec k j) | j <- js ] ++ ")"
                f = fromMaybe (error "schedcheck: inv_self without a fact") (sFact s)
            in "inv_self (G := " ++ g ++ ") (fun t => " ++ fsrc ec f ++ ".rdy_" ++ show f ++ " oc t " ++ cHf c ++ ") " ++ vj i
        Just (InvFact n) -> fsrc ec n ++ ".inv_" ++ show n ++ " oc " ++ unwords (map vj y) ++ " " ++ cHf c
        _ -> error "schedcheck: a Y without an invariance"

rval :: EC -> Int -> Int -> VCtx -> String
rval ec k i c =
    let (n, js) = rOf ec k i
        y = yof ec k i
        base = "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong [ if j `elem` y then evst ec k j c else evt ec k j c | j <- js ] ++ ")"
    in case rinv ec k i c of
        Nothing -> base
        Just iv -> "(" ++ base ++ ").trans (" ++ iv ++ ")"

evt, evst :: EC -> Int -> Int -> VCtx -> String
evt _ k j c = "ev_" ++ show k ++ "_" ++ show j ++ " ops E " ++ cV c ++ " oc " ++ cX c
evst _ k j c = "evs_" ++ show k ++ "_" ++ show j ++ " ops E " ++ cV c ++ " oc " ++ cX c

evct :: EC -> Int -> Int -> VCtx -> String -> String
evct _ k j c h = "evc_" ++ show k ++ "_" ++ show j ++ " ops E " ++ cV c ++ " oc " ++ cX c ++ " (" ++ h ++ ")"

-- | The ready literal from R, under hf: site i's readiness fact (over the site values).
factTerm :: EC -> Int -> Int -> VCtx -> String -> String
factTerm ec k i c hR =
    let s = site ec k i
        f = fromMaybe (error "schedcheck: a readiness site without a fact") (sFact s)
        y = yof ec k i
    in if null y then fsrc ec f ++ ".rdy_" ++ show f ++ " oc " ++ cHf c ++ " " ++ hR
       else fsrc ec f ++ ".rdy_" ++ show f ++ " oc " ++ unwords [ "(" ++ cV c ++ " " ++ show j ++ ")" | j <- y ] ++ " " ++ cHf c
            ++ " ((" ++ fromMaybe "" (rinv ec k i c) ++ ").trans " ++ hR ++ ")"

-- | A proof that e at the step's environment equals e at the hardware.
val :: EC -> Int -> MX -> VCtx -> Maybe (M.Map Int String) -> String
val ec k e c condOk = case e of
    MZ -> "rfl"; MK _ -> "rfl"; MXarg -> "rfl"; MTrue -> "rfl"; MFalse -> "rfl"; MGK _ -> "rfl"
    MLit j -> "congrArg ops.toB (" ++ rd j ++ ")"
    MF n js -> "congrArg (ops.f " ++ show n ++ ") (" ++ tupcong (map rd js) ++ ")"
    MG n js -> "congrArg (ops.g " ++ show n ++ ") (" ++ tupcong (map rd js) ++ ")"
    MSplit crs a b ->
        let rv = andc [ rval ec k i c | i <- crs ]
            ok = foldl (\m (n, i) -> let hR = andget "hR" (length crs) n
                                         s = site ec k i
                                     in M.insert i (if sCKind s == Just "reach" then hR else factTerm ec k i c hR) m)
                       (fromMaybe M.empty condOk) (zip [0 ..] crs)
        in "split_val (" ++ rv ++ ") (fun hR => " ++ val ec k a c (Just ok) ++ ") (" ++ val ec k b c condOk ++ ")"
    _ -> error ("schedcheck: val: " ++ mxRepr e)
  where
    rd j | isCond ec k j = case condOk >>= M.lookup j of
             Just h -> evct ec k j c h
             Nothing -> error ("schedcheck: conditional read " ++ show j ++ " of step " ++ show k ++ " used outside its reach")
         | otherwise = evt ec k j c

condVal :: EC -> Int -> MSite -> VCtx -> String
condVal ec k st c = val ec k (sCond st) c Nothing

argVal :: EC -> Int -> MSite -> VCtx -> String
argVal ec k st c
    | sKind st == "task" = tupcong [ val ec k a c Nothing | a <- fromMaybe [] (sArgs st) ]
    | otherwise = val ec k (sArg st) c Nothing

-- | A user's enable entry: it fires (a method: is called), and its call condition.
entry :: EC -> Int -> MSite -> String
entry ec k st =
    let on = entryOn ec k
    in if isUncond st then on else "(" ++ on ++ " && " ++ condHw ec k st ++ ")"

-- | A user's firing in an enable entry.
entryOn :: EC -> Int -> String
entryOn ec k = if k >= ecNR ec then "calledM w (mM " ++ show (k - ecNR ec) ++ ")" else "WF ops E oc " ++ show k

-- | Proof of False from h1 : WF x = true, h2 : WF y = true.
excl :: EC -> Int -> Int -> String -> String -> String -> String -> String
excl ec x y hA hy h1 h2 =
    let m = ecM ec
        blkOf a = M.findWithDefault [] a (mBlk m)
        idx v l = length (takeWhile (/= v) l)
    in if y `elem` blkOf x then blk False (idx y (blkOf x))
       else if x `elem` blkOf y then blk True (idx x (blkOf y))
       else case M.lookup (x, y) (mDFind m) of
           Just (d, fl) -> let (a, b) = sw fl in "dX ops E " ++ hy ++ " oc " ++ show d ++ " rfl " ++ a ++ " " ++ b
           Nothing -> case M.lookup (x, y) (mMEFind m) of
               Just (c, p, fl) ->
                   let (a, b) = sw fl
                   in case M.lookup c (ecChkG ec) of
                        Just (g, j) -> "meG ops E oc " ++ hA ++ " " ++ show g ++ " " ++ show j ++ " " ++ show (j + 1 + p)
                                       ++ " rfl rfl rfl rfl " ++ a ++ " " ++ b
                        Nothing -> "meX ops E oc " ++ hA ++ " " ++ show (ecLiti ec M.! c) ++ " " ++ show p ++ " rfl rfl " ++ a ++ " " ++ b
               Nothing -> error ("schedcheck: no exclusion between steps " ++ show (x, y))
  where
    sw fl = if not fl then (h1, h2) else (h2, h1)
    blk fl p = let (a, b) = sw fl
               in if ecMultiBlk ec then "blk_excl_or " ++ a ++ " (p := " ++ show p ++ ") rfl " ++ b else "blk_excl " ++ a ++ " " ++ b

