{-# LANGUAGE LambdaCase, MagicHash #-}
-- | --show-model: the extracted model as text.
module Lean.Sched.Show where

import ASyntax (ADef(adef_expr, adef_objid))
import Data.List (intercalate)
import Id (getIdString)
import PPrint (ppString)
import qualified Data.Map.Strict as M ((!), lookup, toList)
import qualified Data.Set as S (member, fromList, toList)

import Lean.Sched.BscFacts (Candidate(..), DisjointFact(dfWhy, dfStep1, dfStep2))
import Lean.Sched.Types (portLabel, Call(clArgs, clPort, clCond), Child(chWires, chInst, chModule, chSched,
    chMethods, chOrder), Design(dWarnings, dModule, dPackage, dSource, dInputOrder, dInputs, dOutputs,
    dChildren, dChildOrder, dSteps, dHwTasks, dUseDefs, dOrders, dDisjoint, dBlocks, dUserME, dExportedC,
    dExportedCF, dUnrealized, dExportedSB, dEnables, dReady, dUnspec), HwTask(htCond, htTag, htName, htId),
    Meth(mtCopies, mtName, mtKind, mtDeps), MethKind(ValueMethod, ActionMethod, ActionValueMethod),
    Order(orSteps, orName), Output(ouMask, ouName, ouSeq), Port(ptResults, ptKind, ptCopy, ptEnable, ptArgs),
    Site(siPort, siWithin, siUse), Step(stTaskCalls, stName, stDescr, stFire, stCanFire, stGuard, stReads,
    stSites, stTaskSites, stCalls, stChecks, stOutputs), TaskCall(tcArgs, tcTag, tcName, tcCheck, tcCond))

-- | The extracted model, for checking phase 1 on its own (@-dump-design@),
-- with the candidate disjointness facts and those the solver confirmed.
-- The condition shown with each read ("hint: used when") is bsc's use
-- condition, only a hint for which facts to ask for; the reads out of order
-- are the sites, whatever the hints say.
dumpDesign :: Design -> [Candidate] -> [DisjointFact] -> [String]
dumpDesign d cands facts =
    [ "module " ++ dModule d ++ " (package " ++ dPackage d ++ "), from " ++ dSource d
    , "inputs:  " ++ unwords [ n ++ " :: " ++ ppString t | n <- dInputOrder d, Just t <- [M.lookup n (dInputs d)] ]
    , "outputs: " ++ unwords [ n ++ " :: " ++ ppString t | (n, t) <- dOutputs d ]
    , "", "children:" ]
    ++ concat [ ("  " ++ chInst c ++ " : " ++ chModule c ++ "   " ++ unwords (words (chSched c)))
                : [ "    " ++ mtName mt ++ " (" ++ kindName (mtKind mt) ++ ")"
                    ++ (if null (mtDeps mt) then "" else ", sees same-cycle " ++ intercalate ", " (mtDeps mt))
                    ++ concat [ "\n      copy " ++ maybe "-" show (ptCopy p) ++ ":"
                                ++ maybe "" (" enable " ++) (ptEnable p)
                                ++ concat [ " arg " ++ a ++ " :: " ++ ppString t | (a, t) <- ptArgs p ]
                                ++ concat [ " result " ++ r ++ " :: " ++ ppString t | (r, t) <- ptResults p ]
                              | p <- mtCopies mt ]
                  | mt <- map (chMethods c M.!) (chOrder c) ]
                ++ [ "    wire: " ++ o ++ " = " ++ i ++ " within the cycle" | (o, i) <- chWires c ]
              | c <- map (dChildren d M.!) (dChildOrder d) ]
    ++ [ "", "execution order:" ]
    ++ concat [ [ "  " ++ show k ++ ". " ++ stName st ++ "   [" ++ stDescr st ++ "]"
                , "       fires on " ++ stFire st ++ ", can fire " ++ stCanFire st ]
                ++ [ "       guard " ++ ppString g | Just g <- [stGuard st] ]
                ++ [ "       reads " ++ portLabel p ++ "  (hint: used when " ++ ppString c ++ ")" | (p, c) <- stReads st ]
                ++ [ "       reads " ++ portLabel (siPort s) ++ " out of order: " ++ siteText s | s <- stSites st ]
                ++ [ "       a system task (when " ++ ppString c ++ ") reads " ++ portLabel (siPort s)
                     ++ " out of order: " ++ siteText s
                   | (c, _, ss) <- stTaskSites st, s <- ss ]
                ++ [ "       calls " ++ portLabel (clPort c) ++ " when " ++ ppString (clCond c)
                     ++ " with " ++ intercalate ", " (map ppString (clArgs c)) | c <- stCalls st ]
                ++ [ "       check " ++ ppString c | c <- stChecks st ]
                ++ [ "       output " ++ ouName o ++ " = " ++ ppString (ouSeq o)
                     ++ (if null (ouMask o) then "" else "  (when " ++ unwords (map show (ouMask o)) ++ ")")
                   | o <- stOutputs st ]
                ++ [ "       task #" ++ show (tcTag tc) ++ " " ++ tcName tc ++ (if tcCheck tc then " (run-time check)" else "")
                     ++ " when " ++ ppString (tcCond tc)
                     ++ (if null (tcArgs tc) then "" else " with " ++ intercalate ", " (map ppString (tcArgs tc)))
                   | tc <- stTaskCalls st ]
              | (k, st) <- zip [1 :: Int ..] (dSteps d) ]
    ++ (if null (dHwTasks d) then [] else
        "" : "system-task calls of the hardware, in the order of its always block:"
        : [ "  " ++ maybe "matches none" (\k -> "task #" ++ show k) (htTag h) ++ ": " ++ htName h ++ " [" ++ htId h ++ "]"
            ++ " when " ++ ppString (htCond h)
          | h <- dHwTasks d ])
    ++ [ "", "use conditions (recomputed):" ]
    ++ [ "  " ++ getIdString (adef_objid u) ++ " = " ++ ppString (adef_expr u) | u <- dUseDefs d ]
    ++ [ "", "orders checked:" ]
    ++ [ "  " ++ (if null (orName o) then "schedule" else orName o) ++ ": " ++ unwords (orSteps o)
       | o <- dOrders d ]
    ++ [ "", "disjoint guards (bsc):     " ++ pairsText (dDisjoint d)
       , "urgency blocking (r, by):  " ++ pairsText (S.toList (dBlocks d))
       , "mutually_exclusive (user): " ++ pairsText (dUserME d)
       , "exported C:                " ++ pairsText (dExportedC d)
       , "exported CF:               " ++ pairsText (dExportedCF d)
       , "unrealizable CF swaps:     " ++ pairsText (dUnrealized d)
       , "exported SB/SBR:           " ++ unwords [ a ++ " " ++ r ++ " " ++ b | (a, r, b) <- dExportedSB d ]
       , "enables (method, EN):      " ++ pairsText (M.toList (dEnables d))
       , "ready (method, RDY):       " ++ pairsText (M.toList (dReady d))
       , "unspecified values:        " ++ dUnspec d
       , "", "disjointness bsc relied on but did not record:" ]
    ++ concatMap candLines cands
    ++ [ "warning: " ++ w | w <- dWarnings d ]
  where
    kindName k = case k of { ValueMethod -> "value"; ActionMethod -> "action"; ActionValueMethod -> "actionvalue" }
    siteText s
        | siWithin s = "with its own call of " ++ portLabel (siUse s)
        | ptKind (siUse s) == ValueMethod = "after a read of " ++ portLabel (siUse s)
        | otherwise = "after a call of " ++ portLabel (siUse s)
    pairsText ps = if null ps then "-" else unwords [ "(" ++ a ++ ", " ++ b ++ ")" | (a, b) <- ps ]
    factKey f = (dfStep1 f, dfStep2 f, dfWhy f)
    confirmed = S.fromList (map factKey facts)
    -- (a chained fact not confirmed is followed by the pairs asked instead)
    candLines c = case c of
        Single f -> [factLine "  " f]
        Chained f fbs -> factLine "  " f : (if S.member (factKey f) confirmed then [] else map (factLine "    ") fbs)
    factLine ind f = ind ++ (if S.member (factKey f) confirmed then "confirmed " else "UNCONFIRMED ")
                     ++ dfStep1 f ++ " / " ++ dfStep2 f ++ ": " ++ dfWhy f
