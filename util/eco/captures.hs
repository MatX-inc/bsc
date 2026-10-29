-- bluehs captures.hs [--objective whole|translated-raw|translated-normalized] [--refinements standard] [--structure preserve|ordinary|compare] [--invariants export|defer] [--vacant-state] [--state-view] [--historical-shell] [--inactive-helpers] FLOW.config
-- FLOW.config is a typed CaptureFlow record; all proposals and proofs are saved.
module Main where
import ECOASTFlow
import ECOASTRefinements
import ECOASTTranslatedObjective
import ECOASTSearch (changedLines, SearchOptions(..), defaultCandidateProvider, combineCandidateProviders)
import ECOASTVacancy (vacancyProvider)
import ECOASTStateView (stateViewProvider)
import ECOASTShell (shellProvider)
import ECOASTInactive (inactiveProvider)
import Control.Monad (when)
import System.Directory (doesPathExist, createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath ((</>))
import Text.Read (readMaybe)

usage = "usage: bluehs captures.hs [--objective whole|translated-raw|translated-normalized] [--refinements standard] [--structure preserve|ordinary|compare] [--invariants export|defer] [--vacant-state] [--state-view] [--historical-shell] [--inactive-helpers] FLOW.config"

parseOptions structure invariants vacancy view shell inactive refinements args = case args of
    [path] -> return (structure,invariants,vacancy,view,shell,inactive,refinements,path)
    "--structure":mode:rest | mode `elem` ["preserve","ordinary","compare"] -> parseOptions mode invariants vacancy view shell inactive refinements rest
    "--invariants":mode:rest | mode `elem` ["export","defer"] ->
        parseOptions structure (if mode=="export" then ExportInvariants else DeferInvariants) vacancy view shell inactive refinements rest
    "--vacant-state":rest -> parseOptions structure invariants True view shell inactive refinements rest
    "--state-view":rest -> parseOptions structure invariants vacancy True shell inactive refinements rest
    "--historical-shell":rest -> parseOptions structure invariants vacancy view True inactive refinements rest
    "--inactive-helpers":rest -> parseOptions structure invariants vacancy view shell True refinements rest
    "--refinements":"standard":rest -> parseOptions structure invariants vacancy view shell inactive True rest
    _ -> die usage

main = do
    allArgs <- getArgs
    (objectiveMode,args) <- case allArgs of
      "--objective":mode:rest | mode `elem` ["whole","translated-raw","translated-normalized"] -> return (mode,rest)
      "--objective":_ -> die usage
      _ -> return ("whole",allArgs)
    (mode,invariants,vacancy,view,shell,inactive,refinements,path) <- parseOptions "preserve" ExportInvariants False False False False False args
    source <- readFile path
    config <- maybe (die "invalid CaptureFlow configuration") return (readMaybe source)
    let optional = [vacancyProvider | vacancy && not refinements] ++ [stateViewProvider | view && not refinements] ++ [shellProvider | shell] ++ [inactiveProvider | inactive]
        provider = combineCandidateProviders (optional ++ [if refinements then standardRefinementProvider else defaultCandidateProvider])
        options = (if refinements then enableStandardRefinements else id) (cfSearch config)
        extended = options {soFeatures=soFeatures options ++ [f | (f,enabled) <- [("F21",vacancy),("F23",view),("F22",shell),("F22Inactive",inactive)], enabled, f `notElem` soFeatures options]}
        configured policy directory = config
            {cfSearch=applyStructurePolicy policy extended,cfOutput=directory}
        run selected = do
          result <- case objectiveMode of
            "whole" -> runCaptureFlowWithProvider provider invariants selected
            choice -> runCaptureFlowWithCost
              (translatedCostProvider (if choice=="translated-raw" then TranslatedRaw else TranslatedNormalized)
                  [cfImplementationManifest selected]) provider invariants selected
          writeFile (cfOutput selected </> "effective.config") (show selected ++ "\n")
          return result
    case mode of
      "compare" -> do
        let root=cfOutput config
        exists <- doesPathExist root
        when exists (die "comparison output directory exists; use a fresh path")
        createDirectoryIfMissing True root
        historical <- run (configured PreserveStructure (root </> "preserve"))
        ordinary <- run (configured OrdinaryStructure (root </> "ordinary"))
        writeFile (root </> "structure-comparison.report") (show [("preserve",historical),("ordinary",ordinary)] ++ "\n")
        writeFile (root </> "summary.txt") (unlines
          ["scope=F09 case skeleton and F10 equal-arm conditional restoration; other features unchanged",
           "preserve_changed_lines=" ++ show (changedLines (frSelectedCost historical)),
           "ordinary_changed_lines=" ++ show (changedLines (frSelectedCost ordinary)),
           "downstream_preference=unmeasured; both outputs retained"])
        print [("preserve",historical),("ordinary",ordinary)]
      _ -> run (configured (if mode=="ordinary" then OrdinaryStructure else PreserveStructure) (cfOutput config)) >>= print
