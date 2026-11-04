-- | One module on the modular path: its inputs, then the stages every run shares
-- (site conditions, the model's view, the model, the gate's facts).
module Lean.Sched.Run
    ( Input(..), Prep(..), prepare, GateModel(..), gateModel, HierIn(..), noHier
    ) where

import ABin (ABinModInfo(..))
import Control.Exception (evaluate)
import Error (ErrorHandle)
import Flags (Flags)
import qualified Data.Map.Strict as M (Map, empty)
import qualified Data.Set as S (Set)

import Lean.Sched.Bounds (Bounds)
import Lean.Sched.BscFacts (Candidate, DisjointFact, BscLookup, newBscLookup)
import Lean.Sched.Domain (Domain, domainChecks, domainOf)
import Lean.Sched.Export (Export, SynthKid)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Facts (gateFacts, newFactShare, FactShare, GateFacts(gfRecord))
import Lean.Sched.Kinds (libSets, ifcTypes)
import Lean.Sched.Model (buildModel, mkDView, showModel, DView(dvExport, dvSynthK), Model, PyErr)
import Lean.Sched.Sites (Sites, analyseSites)
import Lean.Sched.Time (phase, Timer)
import Lean.Sched.Tree (APkg)
import Lean.Sched.Types (Design)
import Lean.Solver (Solver)

-- | What the driver extracted from one @.ba@.
data Input = Input
    { inDesign :: Design
    , inCands  :: [Candidate]
    , inFacts  :: [DisjointFact]
    , inMore   :: [DisjointFact] -> IO [DisjointFact]
    , inSolver :: Solver
    , inABMI   :: ABinModInfo
    , inBounds :: Bounds
    , inName   :: String
    , inFlags  :: Flags
    , inErrh   :: ErrorHandle
    , inTimer  :: Timer
    , inHier   :: HierIn
    }

-- | Its place in a hierarchy (Lean.Sched.Hier): its export when it is written as a synthesized
-- child, and its synthesized children by instance.
data HierIn = HierIn { hiExport :: Maybe Export, hiKids :: M.Map String SynthKid }

noHier :: HierIn
noHier = HierIn Nothing M.empty

-- | The shared stages' results: solver, package view, sites, the model's view, bsc's library
-- primitives, the lookup of bsc's facts, and the clocks with why their gates are not modelled.
data Prep = Prep { pSolver :: Solver, pAP :: APkg, pSites :: Sites, pDV :: DView, pPrims :: S.Set String, pBL :: BscLookup
                 , pDomain :: Domain, pDomainWhy :: [String] }

-- | Site conditions and the model's view (Left: the sites' failure, or no bsc library).
prepare :: Input -> IO (Either (Either String String) Prep)
prepare inp = do
    let sv = inSolver inp
        apkg = abmi_apkg (inABMI inp)
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
                    let dom = domainOf apkg
                    why <- phase (inTimer inp) "domain" $ domainChecks dom (inDesign inp) (inMore inp)
                    let dv = (mkDView (inDesign inp) ap (ifcTypes apkg) pkgs) { dvExport = hiExport (inHier inp), dvSynthK = hiKids (inHier inp) }
                    return (Right (Prep sv ap st dv prims bl dom why))

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
