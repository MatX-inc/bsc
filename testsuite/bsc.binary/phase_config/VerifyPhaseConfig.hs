{-# LANGUAGE ExistentialQuantification #-}

-- Run with ./test.sh after building bsc-core. These are dependency-boundary
-- tests: irrelevant settings must be forgotten, while settings consumed by
-- more than one phase must reach every consumer.
module Main (main) where

import Control.Monad (forM_, unless)
import Control.Exception (ErrorCall, displayException, try)
import Data.List (isInfixOf)
import System.Exit (die)

import Backend (Backend(..))
import qualified Error as E
import qualified Flags as F
import FlagsDecode (defaultFlags, showFlagsRaw)
import qualified PhaseConfig as PC
import qualified PhaseConfigLegacy as PL
import Position (noPosition)

data Projection = forall a. (Eq a, Show a, PL.PhaseFlags a) =>
    Projection String (F.Flags -> PC.PhaseConfig a)

projections :: [Projection]
projections =
    [ Projection "parse" PC.parseConfig
    , Projection "reduce" PC.reduceConfig
    , Projection "typecheck" PC.typecheckConfig
    , Projection "internal" PC.internalConfig
    , Projection "elaborate" PC.elabConfig
    , Projection "schedule" PC.schedConfig
    , Projection "materialize" PC.materializeConfig
    , Projection "Verilog generation" PC.verilogGenConfig
    , Projection "Bluesim generation" PC.bluesimGenConfig
    , Projection "host compilation" PC.hostCompileConfig
    , Projection "Verilog linking" PC.verilogLinkConfig
    , Projection "Bluesim linking" PC.bluesimLinkConfig
    , Projection "foreign generation" PC.foreignGenConfig
    ]

assert :: String -> Bool -> IO ()
assert label ok = unless ok (die ("FAIL: " ++ label))

same :: (Eq a, Show a) => String -> a -> a -> IO ()
same label expected actual = unless (expected == actual) $
    die ("FAIL: " ++ label ++ "\nexpected: " ++ show expected ++
         "\nactual:   " ++ show actual)

different :: Eq a => String -> a -> a -> IO ()
different label before after = assert label (before /= after)

key :: PC.PhaseConfig a -> (PC.RunFlags, a)
key config = (PC.phaseRunFlags config, PC.phaseFlags config)

base :: F.Flags
base = defaultFlags "/phase-config-test/toolchain"

-- Nondefaults from each major consumer expose a lost field in a legacy
-- adapter. This does not duplicate the complete projection field lists.
configured :: F.Flags
configured = base
    { F.backend = Just Verilog
    , F.cpp = True
    , F.cppFlags = ["-traditional-cpp"]
    , F.defines = ["PHASE_TEST=1"]
    , F.usePrelude = False
    , F.allowIncoherentMatches = True
    , F.maxTIStackDepth = 311
    , F.inlineISyntax = False
    , F.liftDicts = False
    , F.expandIf = True
    , F.aggImpConds = False
    , F.biasMethodScheduling = True
    , F.relaxMethodEarliness = False
    , F.removeStarvedRules = True
    , F.satBackend = F.SAT_STP
    , F.stableVerilog = not (F.stableVerilog base)
    , F.unSpecTo = "0"
    , F.keepFires = True
    , F.systemVerilogOutput = True
    , F.semanticPortsComment = True
    , F.removeReg = False
    , F.blockCodegen = True
    , F.genSysC = True
    , F.resetName = "RESET_PHASE_TEST"
    , F.dumpFormats = ["fst"]
    , F.cDebug = True
    , F.cIncPath = ["/phase-config-test/include"]
    , F.cFlags = ["-O1"]
    , F.cxxFlags = ["-O2"]
    , F.parallelSimLink = 3
    , F.cLibPath = ["/phase-config-test/lib"]
    , F.cLibs = ["phase_test"]
    , F.linkFlags = ["-Wl,--as-needed"]
    , F.vFlags = ["-g2012"]
    , F.vsim = Just "iverilog"
    , F.useDPI = True
    , F.bdir = Just "phase-bdir"
    , F.vdir = Just "phase-vdir"
    , F.cdir = Just "phase-cdir"
    , F.fdir = Just "phase-fdir"
    , F.oFile = "phase-output"
    , F.remapPathPrefix = [("/phase-config-test", "/source")]
    , F.promoteWarnings = F.SomeMsgs ["S0080"]
    , F.verbosity = F.ExtraVerbose
    }

checkCommon :: Projection -> IO ()
checkCommon (Projection name project) = do
    let original = project base
        verbose = project (base { F.verbosity = F.ExtraVerbose })
        dumped = project (base { F.dumps = [(F.DFschedule, Just "phase.dump")] })
        stopped = project (base { F.kill = Just (F.DFschedule, Nothing) })
        promoted = project (base { F.promoteWarnings = F.SomeMsgs ["S0080"] })
        demoted = project (base { F.demoteErrors = F.SomeMsgs ["T0020"] })
        suppressed = project (base { F.suppressWarnings = F.SomeMsgs ["S0080"] })
        config = project configured
    same (name ++ ": verbosity leaves semantic key unchanged")
        (key original) (key verbose)
    different (name ++ ": verbosity reaches runtime options")
        (PC.phaseRunOptions original) (PC.phaseRunOptions verbose)
    same (name ++ ": ordinary dumps leave primary-artifact key unchanged")
        (key original) (key dumped)
    different (name ++ ": ordinary dumps reach runtime options")
        (PC.phaseRunOptions original) (PC.phaseRunOptions dumped)
    different (name ++ ": stop-after-stage changes failure/output policy")
        (PC.phaseRunFlags original) (PC.phaseRunFlags stopped)
    forM_ [("promotion", promoted), ("demotion", demoted),
           ("suppression", suppressed)] $ \(policy, changed) -> do
        different (name ++ ": warning " ++ policy ++ " is keyed")
            (PC.phaseRunFlags original) (PC.phaseRunFlags changed)
        same (name ++ ": warning " ++ policy ++ " is not a phase setting")
            (PC.phaseFlags original) (PC.phaseFlags changed)
    same (name ++ ": legacy adapter preserves effective phase configuration")
        config (project (PL.legacyFlags config))
    same (name ++ ": legacy adapter preserves runtime verbosity")
        F.ExtraVerbose (F.verbosity (PL.legacyFlags verbose))

-- Use observable warning behavior rather than exposing ErrorHandle internals.
-- Promoted warnings throw ErrorCall with their diagnostic; they do not print
-- the error before throwing. Suppress the summary too, so expected warnings
-- and failures keep this regression's output quiet.
checkDiagnosticScope :: IO ()
checkDiagnosticScope = do
    errh <- E.initErrorHandle
    let phaseWarning = (noPosition, E.WPoisonedDefFile "phase-scope-probe.bo")
        callerWarning = (noPosition, E.WUnusedDef "caller-scope-probe")
        phaseTag = E.getErrMsgTag (snd phaseWarning)
        summaryTag = E.getErrMsgTag (E.WSuppressedWarnings 1)
        caller = base
            { F.promoteWarnings = F.AllMsgs
            , F.suppressWarnings = F.SomeMsgs [phaseTag, summaryTag]
            }
        phase = PL.legacyFlags $ PC.schedConfig $ base
            { F.promoteWarnings = F.SomeMsgs [phaseTag] }
        expectPromoted label warning action = do
            result <- try action :: IO (Either ErrorCall ())
            case result of
                Left diagnostic -> assert label $
                    E.getErrMsgTag (snd warning) `isInfixOf` displayException diagnostic
                Right () -> die ("FAIL: " ++ label ++ " did not promote warning")
    E.setErrorHandleFlags errh caller
    expectPromoted "phase-local warning promotion" phaseWarning $
        E.withErrorHandleFlags errh phase $
            E.bsWarning errh [phaseWarning]
    -- The preceding phase exited by exception. Its suppression and promotion
    -- policies must both have been replaced by the caller's policies.
    E.bsWarning errh [phaseWarning]
    expectPromoted "caller promotion restored after phase exception" callerWarning $
        E.bsWarning errh [callerWarning]

-- Typechecking compiler-generated reflection expressions has a fixed policy.
-- Only the solver configuration comes from the invocation; package-level
-- language and error-recovery switches must not leak into this nested check.
checkInternalTypecheckerPolicy :: IO ()
checkInternalTypecheckerPolicy = do
    let internal = PL.internalTypecheckFlags . PC.typeSolverFlags
        expected = internal base
        changedPolicies =
            [ ("incoherent instances", base { F.allowIncoherentMatches = True })
            , ("let generalization", base { F.letGen = True })
            , ("poison recovery", base { F.enablePoisonPills = True })
            , ("combined package policies", base
                  { F.allowIncoherentMatches = True
                  , F.letGen = True
                  , F.enablePoisonPills = True
                  })
            ]
    forM_ changedPolicies $ \(policy, changed) -> do
        same ("nested typecheck ignores " ++ policy ++ " in solver inputs")
            (PC.typeSolverFlags base) (PC.typeSolverFlags changed)
        same ("nested typecheck ignores " ++ policy ++ " in effective flags")
            (showFlagsRaw expected) (showFlagsRaw (internal changed))

    let solverInput = base
            { F.maxTIStackDepth = 317
            , F.useProvisoSAT = not (F.useProvisoSAT base)
            , F.satBackend = if F.satBackend base == F.SAT_STP
                             then F.SAT_Yices else F.SAT_STP
            }
        restored = internal solverInput
    same "nested typecheck preserves solver stack limit"
        (F.maxTIStackDepth solverInput) (F.maxTIStackDepth restored)
    same "nested typecheck preserves proviso SAT setting"
        (F.useProvisoSAT solverInput) (F.useProvisoSAT restored)
    same "nested typecheck preserves SAT backend"
        (F.satBackend solverInput) (F.satBackend restored)
    same "nested typecheck fixes incoherent matches to false"
        False (F.allowIncoherentMatches restored)
    same "nested typecheck fixes let generalization to false"
        False (F.letGen restored)
    same "nested typecheck fixes poison recovery to false"
        False (F.enablePoisonPills restored)
    same "nested typecheck does not inherit the invocation's toolchain root"
        "" (F.bluespecDir restored)

    let letGeneralization = base { F.letGen = not (F.letGen base) }
    same "let generalization is not an elaboration dependency"
        (PC.elabConfig base) (PC.elabConfig letGeneralization)
    different "let generalization remains a package typecheck dependency"
        (PC.typecheckConfig base) (PC.typecheckConfig letGeneralization)

main :: IO ()
main = do
    mapM_ checkCommon projections
    checkDiagnosticScope
    checkInternalTypecheckerPolicy

    let dialect = base { F.systemVerilogOutput = True }
    different "Verilog dialect reaches Verilog generation"
        (PC.verilogGenConfig base) (PC.verilogGenConfig dialect)
    same "Verilog dialect does not affect parsing"
        (PC.parseConfig base) (PC.parseConfig dialect)
    same "Verilog dialect does not affect scheduling"
        (PC.schedConfig base) (PC.schedConfig dialect)
    same "Verilog dialect does not affect C++ generation"
        (PC.bluesimGenConfig base) (PC.bluesimGenConfig dialect)

    let hostFlags = base { F.cxxFlags = ["-O3", "-fno-exceptions"] }
    different "host compiler arguments reach host compilation"
        (PC.hostCompileConfig base) (PC.hostCompileConfig hostFlags)
    same "host compiler arguments do not affect generated C++"
        (PC.bluesimGenConfig base) (PC.bluesimGenConfig hostFlags)
    same "host compiler arguments do not affect scheduling"
        (PC.schedConfig base) (PC.schedConfig hostFlags)
    same "host compiler arguments do not affect parsing"
        (PC.parseConfig base) (PC.parseConfig hostFlags)

    let solver = base { F.satBackend = F.SAT_STP }
    different "SAT selection reaches typechecking"
        (PC.typecheckConfig base) (PC.typecheckConfig solver)
    different "SAT selection reaches scheduling"
        (PC.schedConfig base) (PC.schedConfig solver)
    different "SAT selection reaches Verilog optimization"
        (PC.verilogGenConfig base) (PC.verilogGenConfig solver)
    same "SAT selection does not affect parsing"
        (PC.parseConfig base) (PC.parseConfig solver)

    let stable = base { F.stableVerilog = not (F.stableVerilog base) }
    different "stable naming reaches elaboration"
        (PC.elabConfig base) (PC.elabConfig stable)
    different "stable naming reaches scheduling"
        (PC.schedConfig base) (PC.schedConfig stable)
    different "stable naming reaches materialization"
        (PC.materializeConfig base) (PC.materializeConfig stable)
    different "stable naming reaches Verilog generation"
        (PC.verilogGenConfig base) (PC.verilogGenConfig stable)

    -- A poisoned, irrelevant input is stronger than comparing two Boolean
    -- settings: using the old Flags as the reconstruction baseline fails.
    let scheduleInput = base
            { F.biasMethodScheduling = True
            , F.systemVerilogOutput = error "scheduler retained Verilog dialect"
            , F.cxxFlags = error "scheduler retained host compiler arguments"
            }
        restored = PL.legacyFlags (PC.schedConfig scheduleInput)
    same "scheduler adapter preserves its requested policy"
        True (F.biasMethodScheduling restored)
    same "scheduler adapter resets unrelated dialect to fixed default"
        (F.systemVerilogOutput base) (F.systemVerilogOutput restored)
    same "scheduler adapter resets unrelated host arguments to fixed default"
        (F.cxxFlags base) (F.cxxFlags restored)
    putStrLn "PASS: phase configuration dependency boundaries"
