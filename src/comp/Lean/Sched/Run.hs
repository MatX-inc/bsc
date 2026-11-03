-- | One module on the modular path: its inputs, then the stages every run shares
-- (site conditions, the model's view, the model, the gate's facts).
module Lean.Sched.Run
    ( Input(..), Prep(..), prepare, GateModel(..), gateModel
    ) where

import ABin (ABinModInfo(..))
import Control.Exception (evaluate)
import Error (ErrorHandle)
import Flags (Flags)
import qualified Data.Set as S (Set)

import Lean.Sched.Bounds (Bounds)
import Lean.Sched.BscFacts (Candidate, DisjointFact, BscLookup, newBscLookup)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Facts (gateFacts, newFactShare, FactShare, GateFacts(gfRecord))
import Lean.Sched.Kinds (libSets, ifcTypes)
import Lean.Sched.Model (buildModel, mkDView, showModel, DView, Model, PyErr)
import Lean.Sched.Sites (Sites, analyseSites)
import Lean.Sched.Time (phase, Timer)
import Lean.Sched.Tree (APkg)
import Lean.Sched.Types (Design)
import Lean.Solver (Solver, newSolver)

-- | What the driver extracted from one @.ba@.
data Input = Input
    { inDesign :: Design
    , inCands  :: [Candidate]
    , inFacts  :: [DisjointFact]
    , inMore   :: [DisjointFact] -> IO [DisjointFact]
    , inABMI   :: ABinModInfo
    , inBounds :: Bounds
    , inName   :: String
    , inFlags  :: Flags
    , inErrh   :: ErrorHandle
    , inTimer  :: Timer
    }

-- | The shared stages' results: solver, package view, sites, the model's view, bsc's library
-- primitives and the lookup of bsc's facts.
data Prep = Prep { pSolver :: Solver, pAP :: APkg, pSites :: Sites, pDV :: DView, pPrims :: S.Set String, pBL :: BscLookup }

-- | Site conditions and the model's view (Left: the sites' failure, or no bsc library).
prepare :: Input -> IO (Either (Either String String) Prep)
prepare inp = do
    sv <- newSolver (inErrh inp) (inFlags inp)
    let apkg = abmi_apkg (inABMI inp)
        ap = memAPkg apkg
    r <- phase (inTimer inp) "sites" $ analyseSites sv (inDesign inp) ap (abmi_aschedinfo (inABMI inp))
    case r of
        Left err -> return (Left (Left err))
        Right st -> do
            lib <- libSets (inFlags inp)
            case lib of
                Left err -> return (Left (Right err))
                Right (prims, pkgs) -> do
                    bl <- newBscLookup (inDesign inp) (inCands inp) (inFacts inp) (inMore inp)
                    return (Right (Prep sv ap st (mkDView (inDesign inp) ap (ifcTypes apkg) pkgs) prims bl))

-- | The gate's model and facts, with the share a file's plan of the same model reuses.
data GateModel = GateModel { gmModel :: Either PyErr Model, gmFacts :: GateFacts, gmShare :: FactShare }

gateModel :: Input -> Prep -> IO GateModel
gateModel inp p = do
    let t = inTimer inp
    m <- phase t "model" $ buildModel True (pDV p) (pSites p) >>= \x ->
             evaluate (either (const ()) (\mm -> length (showModel mm) `seq` ()) x) >> return x
    share <- newFactShare (pSolver p) (pSites p)
    gf <- phase t "facts" $ gateFacts (pSolver p) share (inDesign inp) (pDV p) (pSites p) (pBL p) (inBounds inp) (inName inp) m
              >>= \g -> evaluate (length (show (map fst (gfRecord g)))) >> return g
    return (GateModel m gf share)
