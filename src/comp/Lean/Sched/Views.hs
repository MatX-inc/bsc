-- | --show-model's views (parity oracles and tests): sites, the model and scan, the gate's facts and
-- shadow classes, the CF-swap and schedule tests, and bsc's solver on logged queries.
module Lean.Sched.Views
    ( showView, viewNames, solverCheck
    ) where

import ABin (ABinModInfo(..))
import Control.Exception (evaluate)
import Control.Monad (forM)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Error (ErrorHandle)
import Flags (Flags)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (elems, lookup, size, fromList)
import qualified Data.Set as S (difference, fromList, size, toList)

import Lean.Sched.BscFacts (Candidate(..), DisjointFact(..), invertedFacts)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Facts (jsonDumps, GateFacts(gfEsc, gfScan, gfRecord, gfLCP))
import Lean.Sched.Kinds (parseSched, schedMap)
import Lean.Sched.Model (bitLength, mportRepr, pyStrErr, showModel, MSite(sPort), MStep(tSites), Model(mD,
    mRules, mNSlots))
import Lean.Sched.Run (gateModel, prepare, GateModel(gmFacts, gmModel), Input(inCands, inTimer, inDesign,
    inABMI, inName, inSolver), Prep(pPrims, pSolver, pDV, pSites))
import Lean.Sched.Shadow (shadowReport, ShadowOut(..))
import Lean.Sched.Sites (Sites(..), analyseSites, annOf, showSites)
import Lean.Sched.Time (phase, phaseNote)
import Lean.Sched.Types (Design(..), Step(..), Child(..))
import Lean.Solver (Verdict(..), newSolver, solveCNF, readDimacs)

viewNames :: [String]
viewNames = ["sites", "model", "facts", "inverted-check", "sched-check"]

-- | One view's lines (Left: no bsc library).
showView :: Input -> String -> IO (Either String [String])
showView inp what = case what of
    "sites" -> do
        let sv = inSolver inp
            ap = memAPkg (abmi_apkg (inABMI inp))
        r <- phase t "sites" $ analyseSites sv d ap (abmi_aschedinfo (inABMI inp))
        case r of
            Left err -> return (Right ["error " ++ err])
            Right st -> do
                ls <- showSites d ap st
                phaseNote t ("sites: " ++ show (sxQueries st) ++ " solver queries")
                return (Right ls)
    "model" -> withModel $ \_ gm -> case gmModel gm of
        Left e -> return ["error model: " ++ take 200 (pyStrErr e)]
        Right m -> do
            let cr = crashLines m
                gf = gmFacts gm
                -- the last-caller ports' virtual slots counted, as the prototype's model then has them
                ns = mNSlots m + length (gfLCP gf)
                scan = case gfScan gf of
                    Right (need, esc) | null cr -> [ "scan " ++ show i ++ " need=" ++ bitList n ++ " esc=" ++ bitList e
                                                   | (i, n, e) <- zip3 [0 :: Int ..] need esc ]
                    Left err | null cr -> ["scan error " ++ err]
                    _ -> []
            return (cr ++ showModel m { mNSlots = ns, mD = max 1 (bitLength (ns - 1)) } ++ scan)
    "facts" -> withModel $ \p gm -> do
        let cr = either (const []) crashLines (gmModel gm)
            gf = gmFacts gm
            fl = if not (null cr) then [] else
                    [ "gate " ++ k ++ " " ++ jsonDumps v | (k, v) <- gfRecord gf ]
                    ++ [ "lcp " ++ mportRepr q | q <- gfLCP gf ]
                    ++ [ "esc " ++ unwords [show a, show b, show c, show e] ++ " " ++ fromMaybe "None" h ++ " " ++ fromMaybe "None" x
                       | (a, b, c, e, h, x) <- gfEsc gf ]
        sh <- phase t "shadow" $ shadowReport (pSolver p) d (pDV p) (pSites p) (pPrims p) (inName inp)
                  >>= \x -> evaluate (length (concat (shLines x))) >> return x
        return (cr ++ fl ++ map ("shadow " ++) (shLines sh))
    "inverted-check" -> do
        -- test: the CF-swap facts asked pair by pair equal those of every order
        let pos = M.fromList (zip (map stName (dSteps d)) [0 :: Int ..])
            key f = (dfStep1 f, dfStep2 f, dfWhy f)
            allInv = S.fromList [ key f | Single f <- inCands inp, Just a <- [M.lookup (dfStep1 f) pos]
                                        , Just b <- [M.lookup (dfStep2 f) pos], a > b ]
            names = map stName (dSteps d)
            mine = S.fromList [ key f | (i, y) <- zip [0 :: Int ..] names, x <- take i names, f <- invertedFacts d y x ]
        return (Right (("inverted: orders " ++ show (S.size allInv) ++ ", pairwise " ++ show (S.size mine)
                        ++ (if allInv == mine then " same" else " DIFFER"))
                       : [ "  only orders " ++ show k | k <- S.toList (S.difference allInv mine) ]
                       ++ [ "  only pairwise " ++ show k | k <- S.toList (S.difference mine allInv) ]))
    "sched-check" -> do
        -- test: each child's statements, read from bsc's records, equal the parse of its printed schedule
        let bad = [ chInst c | c <- M.elems (dChildren d), let t = unwords (words (chSched c))
                             , chStmts c /= parseSched (if take 1 t == "[" then init (drop 1 t) else t) ]
            -- Kinds reads a pair's first statement, Sites.annOf its last: they agree when none is related twice
            twice = [ chInst c | c <- M.elems (dChildren d), schedMap (chStmts c) /= annOf (chStmts c) ]
        return (Right [ "sched: " ++ show (M.size (dChildren d)) ++ " children, " ++ show (length bad) ++ " differ " ++ unwords (take 5 bad)
                      , "sched: " ++ show (length twice) ++ " relate a pair twice " ++ unwords (take 5 twice) ])
    _ -> return (Right [])
  where
    t = inTimer inp
    d = inDesign inp
    withModel k = do
        r <- prepare inp
        case r of
            Left (Left err) -> return (Right ["sites error " ++ err])
            Left (Right err) -> return (Left err)
            Right p -> Right <$> (gateModel inp p >>= k p)

-- | The prototype's crash on a step with two sites on one port.
crashLines :: Model -> [String]
crashLines m = [ "crash AssertionError: two sites of one step on one port"
               | any (\r -> let ps = map sPort (F.toList (tSites r)) in S.size (S.fromList ps) /= length ps) (mRules m) ]

bitList :: Integer -> String
bitList x = "[" ++ intercalate ", " (go (0 :: Int) x) ++ "]"
  where go _ 0 = []
        go i y = (if odd y then (show i :) else id) (go (i + 1) (y `div` 2))

-- | bsc's solver on the prototype's logged queries: every verdict must agree.
solverCheck :: ErrorHandle -> Flags -> FilePath -> IO [String]
solverCheck errh flags dir = do
    sv <- newSolver errh flags
    ls <- lines <$> readFile (dir ++ "/verdicts.log")
    rs <- forM [ (q, rc) | l <- ls, [q, rc] <- [words l] ] $ \(q, rc) -> do
        cls <- readDimacs <$> readFile (dir ++ "/" ++ q ++ ".cnf")
        v <- solveCNF sv (q ++ "_") cls
        let want = case rc of { "10" -> Just Sat; "20" -> Just Unsat; _ -> Nothing }
        return (q, rc, maybe True (== v) want, v)
    let n = length rs
        bad = [ "MISMATCH " ++ q ++ " cadical " ++ rc ++ " bsc " ++ show v | (q, rc, False, v) <- rs ]
        cnt w = length [ () | (_, _, _, v) <- rs, v == w ]
    return (bad ++ ["solver-check: " ++ show n ++ " queries, " ++ show (length bad) ++ " mismatches; bsc unsat "
                    ++ show (cnt Unsat) ++ ", sat " ++ show (cnt Sat) ++ ", unknown " ++ show (cnt Unknown)])
