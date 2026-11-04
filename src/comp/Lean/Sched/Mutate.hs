-- | --mutants: changes to the hardware a modular file states, made only where some cycle the
-- hypotheses allow shows them (so each mutant's obligation is false and its file must fail).
module Lean.Sched.Mutate
    ( Mutant(..), Change(..), candidates, choose, renumberTasks
    ) where

import Data.List (nub)
import Data.Maybe (isNothing, listToMaybe)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (lookup, (!))
import qualified Data.Set as S (empty, insert, member, fromList)

import Lean.Sched.Conflicts (mayFollow)
import Lean.Sched.Emit.Arms (tuser)
import Lean.Sched.Emit.Ctx (argHw, kd, site, step, EC(ecKinds, ecM, ecNR, ecNS, ecNT, ecPortInfo), HwEdit(..),
    PInfo(piArgk, piMus, piUsers))
import Lean.Sched.Kinds (KindDef(kdHasEn, kdPorts), PM(..))
import Lean.Sched.Model (exclSrc, CfC(..), MCf(cfRule, cfConj), MSite(sCond, sHw, sKind, sPort), MStep(tGuard, tName,
    tSites, tTasks), MX(..), Model(mByk, mRules, mCfS))
import Lean.Sched.Types (Call(clPort), Child(chConf), Design(dChildren, dStepMap), Port(ptInst, ptMeth),
    Step(stCalls, stReads))

-- | A mutant: its kind, what it changes, and the change.
data Mutant = Mutant { muKind :: String, muWhat :: String, muChange :: Change }

-- | A change to the rendered hardware, or task statements t and t+1 numbered the other way
-- round (the Verilog order of their calls; the steps' sites follow, so only the order changes).
data Change = Hw HwEdit | Renumber Int

-- | Each kind's mutants, in kind order (enable, argument, mux, guard, CF order, last caller,
-- conflict_free condition); a kind's picks alternate among its lists (enable: drops, then forces).
candidates :: Design -> EC -> [[[Mutant]]]
candidates d ec = [[drops, forces], [args], [muxes], [guards], [orders], [lcs], [cfDrops]]
  where
    m = ecM ec
    nR = ecNR ec
    nT = ecNT ec
    name k = tName (step ec k)
    guardOf k = tGuard (step ec k)
    -- a step that can fire: not a run-time check, not guarded false
    canFire k = case guardOf k of { Just MFalse -> False; Just (MChkI _) -> False; Just MChk -> False; _ -> True }
    -- a step that can be off: a method, or a rule with an opaque guard
    canBeOff k = k >= nR || maybe False (`notElem` [MTrue]) (guardOf k)
    live (k, si) = canFire k && sCond (site ec k si) /= MFalse
    offable (k, si) = canBeOff k || sCond (site ec k si) /= MTrue
    tu = tuser ec
    taskUser t = let (k, si, _) = tu M.! t in (k, si)
    taskLive t = let (k, si, st) = tu M.! t in live (k, si) && sHw st /= Just MChk
    isCfT h = case h of { Just (MCfT _) -> True; _ -> False }
    ports = [ (p, pinfo) | k <- ecKinds ec, k /= "tasks", i <- [0 .. length (mByk m M.! k) - 1], pm <- kdPorts (kd ec k)
                         , let p = (k, i, pm), Just pinfo <- [M.lookup p (ecPortInfo ec)], not (null (piUsers pinfo)) ]
    enPorts = [ x | x@((k, _, pm), _) <- ports, kdHasEn (kd ec k) pm ]
    pname (k, i, pm) = (mByk m M.! k !! i) ++ "." ++ (case pm of { PS s -> s; PT t -> show t })
    argText (k, si) = argHw ec k (site ec k si)
    noExcl a b = isNothing (exclSrc m a b) && isNothing (exclSrc m b a)
    mk kind what ch = Mutant kind what (Hw ch)
    drops = [ mk "enable drop" ("task statement " ++ show t ++ " (" ++ name (fst (taskUser t)) ++ ") never enabled") (TaskEn t False)
            | t <- [0 .. nT - 1], taskLive t ]
         ++ [ mk "enable drop" ("the enable of " ++ pname p ++ " leaves out " ++ name k) (EnDrop p n)
            | (p, pinfo) <- enPorts, (n, (k, si)) <- zip [0 ..] (piUsers pinfo), live (k, si) ]
    forces = [ mk "enable force" ("task statement " ++ show t ++ " (" ++ name (fst (taskUser t)) ++ ") enabled in every cycle") (TaskEn t True)
             | t <- [0 .. nT - 1], taskLive t, offable (taskUser t) ]
          -- a conflict_free check's report on (never made where asm holds; dropping it or its message is not seen)
          ++ [ mk "enable force" ("task statement " ++ show t ++ " (" ++ name k ++ "'s conflict_free report) enabled in every cycle") (TaskEn t True)
             | t <- [0 .. nT - 1], let (k, _, st) = tu M.! t, canFire k, isCfT (sHw st) ]
          ++ [ mk "enable force" ("the enable of " ++ pname p ++ " on in every cycle") (EnForce p)
             | (p, pinfo) <- enPorts, all offable (piUsers pinfo) ]
    args = [ mk "argument change" ("task statement " ++ show t ++ "'s arguments (" ++ name (fst (taskUser t)) ++ ") under a fresh operator") (TaskArgWrap t)
           | t <- [0 .. nT - 1], taskLive t ]
        ++ [ mk "argument change" ("the argument of " ++ pname p ++ " under a fresh operator") (ArgWrap p 0)
           | (p, pinfo) <- ports, piArgk pinfo `elem` ["const", "single"], any live (piUsers pinfo) ]
        ++ [ mk "argument change" ("the argument " ++ name k ++ " gives " ++ pname p ++ " under a fresh operator") (ArgWrap p n)
           | (p, pinfo) <- ports, piArgk pinfo `elem` ["mux", "lc"], (n, (k, si)) <- zip [0 ..] (piMus pinfo), live (k, si) ]
    muxes = [ mk "mux priority swap" ("the mux of " ++ pname p ++ ": " ++ name ka ++ " and " ++ name kb ++ " exchange arguments") (MuxSwap p n)
            | (p, pinfo) <- ports, piArgk pinfo == "mux"
            , (n, (a@(ka, _), b@(kb, _))) <- zip [0 ..] (zip (piMus pinfo) (drop 1 (piMus pinfo)))
            , live a || live b, argText a /= argText b ]
    guards = [ mk "guard read change" ("the guard of " ++ name k ++ " reads " ++ maybe "a constant" (sname k) r ++ " for " ++ sname k j) (GuardRead k j r)
             | k <- [0 .. ecNS ec - 1], canFire k, observable k, Just js <- [guardOf k >>= mxReads], j <- nub js
             , let r = listToMaybe [ i | (i, s) <- zip [0 ..] (sitesOf k), sKind s == "read", inst (sPort s) /= inst (sPort (site ec k j)) ] ]
    sitesOf k = F.toList (tSites (step ec k))
    sname k j = pname (sPort (site ec k j))
    inst (k, i, _) = (k, i)
    -- a change to a step is visible if it has a method's RDY, a call with an enable, or a task
    observable k = k >= nR || or [ sKind s == "task" || (sKind s == "call" && kdHasEn (kd ec kk) pm)
                                 | s <- sitesOf k, let (kk, _, pm) = sPort s ]
    orders = [ Mutant "CF-order-only" ("task statements " ++ show t ++ " (" ++ name a ++ ") and " ++ show (t + 1) ++ " (" ++ name b ++ ") in the other order") (Renumber t)
             | t <- [0 .. nT - 2], let a = fst (taskUser t); b = fst (taskUser (t + 1))
             , a < b, b < nR, taskLive t, taskLive (t + 1), noExcl a b, forced d (name a) (name b) ]
    -- a call condition left out of its port's enable only, where a conflict_free check names that caller and
    -- another: the two may both call in the hardware, while the check's line (asm) still reads the condition
    cfNamed = [ S.fromList (cfRule cf : [ j | c <- cfConj cf, j <- case c of { CfWF x -> [x]; CfX x _ -> [x] } ]) | cf <- mCfS m ]
    cfDrops = [ mk "conflict_free condition drop" ("the enable of " ++ pname p ++ " takes " ++ name k ++ " without its call condition") (CondDrop p n)
              | (p, pinfo) <- enPorts, let us = piUsers pinfo, (n, (k, si)) <- zip [0 ..] us, live (k, si), sCond (site ec k si) /= MTrue
              , or [ S.member k ns && S.member k' ns | (k', _) <- us, k' /= k, ns <- cfNamed ] ]
    lcs = [ mk "last-caller order reversal" ("the last-caller mux of " ++ pname p ++ ": the first of its " ++ show (length mus) ++ " callers wins") (LcReverse p)
          | (p, pinfo) <- ports, piArgk pinfo == "lc", let mus = piMus pinfo
          , or [ live x && live y && noExcl ka kb && argText x /= argText y && forced d (name (min ka kb)) (name (max ka kb))
               | (i, x@(ka, _)) <- zip [0 :: Int ..] mus, y@(kb, _) <- drop (i + 1) mus ] ]

-- | The read sites an opaque guard reads.
mxReads :: MX -> Maybe [Int]
mxReads x = case x of
    MG _ js | not (null js) -> Just js
    MF _ js | not (null js) -> Just js
    MLit j -> Just [j]
    _ -> Nothing

-- | Must step @a@ precede step @b@ in one cycle: some use of @b@ may not be followed, in a later
-- step, by a use of @a@ on the same child?
forced :: Design -> String -> String -> Bool
forced d a b = case (M.lookup a (dStepMap d), M.lookup b (dStepMap d)) of
    (Just sa, Just sb) -> or [ not (mayFollow (chConf c) (ptMeth pb) (ptMeth pa))
                             | pa <- uses sa, pb <- uses sb, ptInst pa == ptInst pb, Just c <- [M.lookup (ptInst pa) (dChildren d)] ]
    _ -> False
  where uses s = map fst (stReads s) ++ map clPort (stCalls s)

-- | N mutants: kinds in turn, then each kind's lists in turn, each list's picks spread over it
-- ('spreadOrder'); the first N are the same for every larger N.
choose :: Int -> [[[Mutant]]] -> [Mutant]
choose n kinds = roundRobin [ roundRobin (zipWith pick (deal c (map length ls)) ls) | (c, ls) <- zip (deal n (map (sum . map length) kinds)) kinds ]
  where pick c xs = take c (map (xs !!) (spreadOrder (length xs)))

-- | 0 .. l-1 in van der Corput order: the first, then halves, quarters, ... of the list.
spreadOrder :: Int -> [Int]
spreadOrder l = go S.empty [ (rev j * l) `div` p | j <- [0 .. p - 1] ]
  where
    b = length (takeWhile (< l) (iterate (* 2) 1))
    p = 2 ^ b :: Int
    rev j = foldl (\acc i -> acc * 2 + (j `div` (2 ^ i)) `mod` 2) 0 [0 .. b - 1]
    go _ [] = []
    go seen (x : xs) | S.member x seen = go seen xs
                     | otherwise = x : go (S.insert x seen) xs

-- | Hand out n picks one at a time to the bins in turn, each up to its capacity.
deal :: Int -> [Int] -> [Int]
deal n caps = go n (map (const 0) caps)
  where
    go k cs | k <= 0 || and (zipWith (>=) cs caps) = cs
            | otherwise = let (k', cs') = pass k (zip cs caps) in go k' cs'
    pass k [] = (k, [])
    pass k ((c, cap) : r) | k > 0 && c < cap = let (k', r') = pass (k - 1) r in (k', c + 1 : r')
                          | otherwise = let (k', r') = pass k r in (k', c : r')

roundRobin :: [[a]] -> [a]
roundRobin xss = case [ (y, ys) | (y : ys) <- xss ] of
    [] -> []
    ps -> map fst ps ++ roundRobin (map snd ps)

-- | Task statements t and t+1 exchange numbers in every step's sites and task list.
renumberTasks :: Int -> Model -> Model
renumberTasks t m = m { mRules = map stepR (mRules m) }
  where
    sw x | x == t = t + 1
         | x == t + 1 = t
         | otherwise = x
    stepR st = st { tSites = fmap siteR (tSites st), tTasks = map sw (tTasks st) }
    siteR s = case sPort s of
        ("tasks", i, PT x) -> s { sPort = ("tasks", i, PT (sw x)) }
        _ -> s
