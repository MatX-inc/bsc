{-# LANGUAGE DeriveGeneric, DeriveAnyClass, StandaloneDeriving #-}
-- | The module's clocks and resets: bsc's clock domains (one per family), its gated clocks and
-- its resets. One domain is checked; each gate is a condition on the state its clock drives.
module Lean.Sched.Domain
    ( Domain(..), Clk(..), GateSrc(..), domainOf, multiDomain, multiDomainVerdict, domainLine, domainNote, domainChecks
    ) where

import ASyntax (aTBool, AAbstractInput(AAI_Clock), AClock(AClock), ADef(adef_expr, adef_objid),
    AExpr(APrim, ASDef, ASInt, ASPort, AMGate, ASClock, ASReset), AReset(AReset), APackage(apkg_clock_domains, apkg_reset_list,
    apkg_inputs, apkg_state_instances), AVInst(avi_vname, avi_vmi, avi_iargs))
import ASyntaxUtil (aAnd, aNot)
import Control.DeepSeq (NFData)
import Data.Containers.ListUtils (nubOrd)
import Data.List (intercalate)
import GHC.Generics (Generic)
import Id (getIdBaseString, getIdString)
import IntLit (IntLit(ilValue))
import PPrint (ppString)
import Prim (PrimOp(PrimBAnd))
import VModInfo (VArgInfo(ClockArg, ResetArg), VClockInfo(input_clocks, output_clocks), VFieldInfo(Method, vf_name, vf_clock), vArgs,
    vClk, vFields, getVNameString)
import qualified Data.Map.Lazy as ML (findWithDefault, map)
import qualified Data.Map.Strict as M (Map, elems, findWithDefault, fromList, fromListWith, lookup, map, member)
import qualified Data.Set as S (fromList, member)

import Lean.Sched.BscFacts (DisjointFact(..))
import Lean.Sched.Types (Call(clCond, clPort), Design(dSteps, dSeqDefs, dReady, dReadyDef), Port(ptInst, ptMeth),
    Step(stCalls, stGuard, stKind, stMethod, stName), StepKind(RuleStep, ActionMethodStep))

-- | A clock: its oscillator and its gate, Nothing when the gate is the constant 1.
data Clk = Clk { ckOsc :: String, ckGate :: Maybe GateSrc }
    deriving (Eq, Show)

-- | A gate: an input port of the module (its name and expression), or another signal (its name,
-- and what it is: a child's output gate, a constant 0).
data GateSrc = GateInput String AExpr | GateOther String String
    deriving (Eq, Show)

-- | bsc's clock domains (each a family) with their clocks, the resets with the instances each resets,
-- each child method's gated clock (by instance and method), and the instances taking a gate port.
data Domain = Domain
    { dmDomains  :: [[Clk]]
    , dmResets   :: [String]
    , dmResetOf  :: M.Map String [String]
    , dmMethGate :: M.Map (String, String) Clk
    , dmSeesGate :: [(String, String)]   -- ^ (instance, oscillator) of a gated clock it has a gate port for
    }
    deriving (Eq, Show)

deriving instance Generic Clk
instance NFData Clk
deriving instance Generic GateSrc
instance NFData GateSrc
deriving instance Generic Domain
instance NFData Domain

-- | The module's clocks, from the APackage, grouped into domains as bsc groups them.
domainOf :: APackage -> Domain
domainOf apkg = Domain
    { dmDomains = [ map clk cs | (_, cs) <- apkg_clock_domains apkg ]
    , dmResets  = [ wire w | (_, AReset w) <- apkg_reset_list apkg ]
    , dmResetOf = M.map reverse (M.fromListWith (++) [ (wire w, [getIdString (avi_vname v)]) | v <- apkg_state_instances apkg
                                                     , (ResetArg _, ASReset _ (AReset w)) <- zip (vArgs (avi_vmi v)) (avi_iargs v) ])
    , dmMethGate = M.fromList
          [ ((getIdString (avi_vname v), getIdBaseString (vf_name f)), c)
          | v <- apkg_state_instances apkg, let cs = instClocks v
          , f@Method {} <- vFields (avi_vmi v), Just ci <- [vfClock f], Just (c, _) <- [M.lookup (getIdString ci) cs]
          , ckGate c /= Nothing ]
    , dmSeesGate = [ (getIdString (avi_vname v), ckOsc c) | v <- apkg_state_instances apkg
                   , (c, True) <- M.elems (instClocks v), ckGate c /= Nothing ]
    }
  where
    inputGates = S.fromList [ getIdString g | AAI_Clock _ (Just g) <- apkg_inputs apkg ]
    -- each instance's output clocks' gate ports, for naming a gate a child drives
    outGates = M.fromList [ ((getIdString (avi_vname v), getIdString c), getIdString (avi_vname v) ++ "$" ++ getVNameString gp)
                          | v <- apkg_state_instances apkg, (c, Just (_, Just (gp, _))) <- output_clocks (vClk (avi_vmi v)) ]
    -- each clock argument of an instance, and whether its Verilog has a port for the gate
    instClocks v = let vmi = avi_vmi v
                       ports = M.fromList [ (getIdString i, either (const False) (const True) g)
                                          | (i, Just (_, g)) <- input_clocks (vClk vmi) ]
                   in M.fromList [ (getIdString i, (clk ac, M.lookup (getIdString i) ports == Just True))
                                 | (ClockArg i, ASClock _ ac) <- zip (vArgs vmi) (avi_iargs v) ]
    vfClock f = case f of { Method {} -> vf_clock f; _ -> Nothing }
    clk (AClock osc gate) = Clk (wire osc) (gateOf gate)
    gateOf g = case g of
        ASInt _ _ il | ilValue il == 1 -> Nothing
        ASPort _ i | S.member (getIdString i) inputGates -> Just (GateInput (getIdString i) g)
        AMGate _ o c -> let n = M.findWithDefault (getIdString o ++ "." ++ getIdString c ++ ".gate") (getIdString o, getIdString c) outGates
                        in Just (GateOther n ("the gate of instance " ++ getIdString o ++ "'s output clock " ++ getIdString c))
        _ -> Just (GateOther (trim (ppString g)) "not an input port")
    wire e = case e of
        ASPort _ i -> getIdString i
        _ -> trim (ppString e)
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')

-- | More than one clock domain: the module is not checked (no fallback); its domains' clocks.
multiDomain :: Domain -> Maybe String
multiDomain d
    | length (dmDomains d) <= 1 = Nothing
    | otherwise = Just (show (length (dmDomains d)) ++ " clock domains: " ++ intercalate "; " [ unwords (map ckOsc cs) | cs <- dmDomains d ])

-- | The verdict on a module of several clock domains, neither path A nor B.
multiDomainVerdict :: String
multiDomainVerdict = "not checked: multi-domain"

gatedClocks :: Domain -> [Clk]
gatedClocks d = [ c | cs <- dmDomains d, c <- cs, ckGate c /= Nothing ]

-- | The gate's line on clocks and resets: only for several clocks (gated or not) or several resets.
domainLine :: Domain -> [String]
domainLine d
    | length (concat (dmDomains d)) <= 1 && null (gatedClocks d) && length (dmResets d) <= 1 = []
    | otherwise = [ "domain clocks " ++ unwords [ ckOsc c ++ maybe "" (\g -> "(gate " ++ gateName g ++ ")") (ckGate c)
                                                | cs <- dmDomains d, c <- cs ]
                    ++ "; resets " ++ (if null (dmResets d) then "none" else unwords (map rst (dmResets d))) ]
  where rst r = r ++ (if length (dmResets d) > 1 then "(" ++ unwords (M.findWithDefault [] r (dmResetOf d)) ++ ")" else "")

-- | The top theorem's note on several clocks, gated clocks and resets (empty for one clock and one reset).
domainNote :: Domain -> String
domainNote d = concatMap ("\n   " ++) (gates ++ family ++ resets)
  where
    gs = [ (ckOsc c, g) | c <- gatedClocks d, Just (GateInput g _) <- [ckGate c] ]
    gates | null gs = []
          | otherwise =
              [ "Gated clocks of the module's one clock family: " ++ intercalate ", " [ o ++ " (gate input " ++ g ++ ")" | (o, g) <- gs ] ++ "."
              , "bsc ANDs a clock's gate into every action call on the state it clocks (schedlean checks that each such"
              , "call's enable implies it), so with the gate off that state is not called and holds. A gate is not part"
              , "of the interface: a guard reading it is an operator of its other reads, covered for every `ops`, so for"
              , "each cycle's gate." ]
    clocks = concat (dmDomains d)
    family | length clocks <= 1 = []
           | otherwise =
               [ "Trust point: the module's clocks (" ++ intercalate ", " (map ckOsc clocks) ++ ") share one oscillator's edges, as bsc's"
               , "clock_family attribute (or a BVI import's same_family) asserts; schedlean does not check it." ]
    resets | length (dmResets d) <= 1 = []
           | otherwise = [ "The module has " ++ show (length (dmResets d)) ++ " resets (" ++ intercalate ", " (dmResets d)
                           ++ "): the theorem covers the cycles", "outside reset, with every reset deasserted." ]

-- | Why the gated clocks are not modelled (none: checked): a gate not an input, an instance taking a gate,
-- or a gated call its guard and condition do not imply the gate for (bsc ANDs it in, IExpand.handleMethod).
domainChecks :: Domain -> Design -> ([DisjointFact] -> IO [DisjointFact]) -> IO [String]
domainChecks dom d more = do
    let notInput = [ "the gate " ++ n ++ " of clock " ++ ckOsc c ++ " is " ++ why ++ ", not an input of the module (not modelled)"
                   | c <- gatedClocks dom, Just (GateOther n why) <- [ckGate c] ]
        sees = [ "instance " ++ i ++ " takes the gate of clock " ++ o ++ " as a port (its cycles with the gate off are not modelled)"
               | (i, o) <- dmSeesGate dom ]
        gates = nubOrd [ ge | c <- gatedClocks dom, Just (GateInput _ ge) <- [ckGate c] ]
        -- the gates among each definition's conjuncts (through definitions), found once per definition
        defGates = ML.map (gatesIn . adef_expr) (dSeqDefs d)
        gatesIn e = case e of
            APrim _ _ PrimBAnd xs -> nubOrd (concatMap gatesIn xs)
            ASDef _ i -> ML.findWithDefault [] (getIdString i) defGates
            _ -> filter (== e) gates
        -- each step's gated calls, with the gates among its premise's conjuncts
        calls = [ (st, c, ck, g, ge, inPrem)
                | st <- dSteps d
                , let gc = [ (c, ck, g, ge) | c <- stCalls st, Just ck <- [M.lookup (ptInst (clPort c), ptMeth (clPort c)) (dmMethGate dom)]
                                            , Just (GateInput g ge) <- [ckGate ck] ]
                      inPrem = if null gc then [] else maybe [] gatesIn (premise st)
                , (c, ck, g, ge) <- gc ]
        premise st = case stKind st of
            RuleStep -> stGuard st
            ActionMethodStep | Just m <- stMethod st, M.member m (dReady d), Just r <- M.lookup m (dReadyDef d)
                             , Just df <- M.lookup r (dSeqDefs d) -> Just (ASDef aTBool (adef_objid df))
            _ -> Nothing
        lhs st c = maybe (clCond c) (\p -> aAnd p (clCond c)) (premise st)
        label st c ck = ptInst (clPort c) ++ "." ++ ptMeth (clPort c) ++ " in " ++ stName st ++ " (clock " ++ ckOsc ck ++ ")"
        open = zip [0 :: Int ..] [ (st, c, ck, g, ge) | (st, c, ck, g, ge, inPrem) <- calls, ge `notElem` inPrem
                                                      , ge `notElem` gatesIn (clCond c) ]
        -- (each fact's text numbered, to know which ones the solver confirms)
        facts = [ DisjointFact (stName st) (stName st) (lhs st c) (aNot ge) (show k) Nothing | (k, (st, c, _, _, ge)) <- open ]
    ok <- if null notInput && null sees then more facts else return []
    let okSet = S.fromList (map dfWhy ok)
        bad = [ "the call of " ++ label st c ck ++ " may be enabled with the gate " ++ g ++ " off"
              | (k, (st, c, ck, g, _)) <- open, not (S.member (show k) okSet) ]
    return (notInput ++ sees ++ (if null notInput && null sees then bad else []))

gateName :: GateSrc -> String
gateName g = case g of { GateInput n _ -> n; GateOther n _ -> n }
