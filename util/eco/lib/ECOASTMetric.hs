-- A second reporting metric over the native AST, never a search objective or
-- a proof. Actual emitted bytes remain the raw OLD-relative comparison.
module ECOASTMetric
    ( MetricInput(..), MetricSource(..), MetricReport(..), normalizeProgram, stripInformation
    , MetricInputs, withMetricInputs, metricInputPrograms, measureMetricInputs
    , measureAST, metricReportJSON, renderMetricReport, normalizeHeader
    , costJSON, identityJSON
    ) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesPathExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import BinData (decodeStrict)
import GenABin ()
import ECO.VerilogInfo
import ECOBind (unchanged)
import ECOJSON (EvidenceJSON(..), renderEvidenceJSON)
import ECO.Interface (fixedPorts)
import ECOASTCost (LineCost(..), lineCost, changedLines)
import ECO.Replay (FileIdentity(..), fileIdentity)
import PPrint
import Verilog

-- A candidate is bound to a checked capture's renderer and to actual RTL
-- bytes. A bare AST without its capture context is intentionally unsupported.
data MetricInput = Snapshot FilePath | ContextAST FilePath FilePath FilePath
    deriving (Eq, Read, Show)

-- The identities a report binds for each side: the capture snapshot, or the
-- candidate AST and its actual RTL under a capture's renderer context.
data MetricSource = SnapshotSource FileIdentity
                  | CandidateSource FileIdentity FileIdentity FileIdentity
    deriving (Eq, Show)

sourceIdentities :: MetricSource -> [FileIdentity]
sourceIdentities (SnapshotSource context) = [context]
sourceIdentities (CandidateSource context ast rtl) = [context,ast,rtl]

data MetricReport = MetricReport
    { metricOld :: MetricSource, metricNew :: MetricSource
    , metricRaw :: LineCost, metricNormalized :: LineCost
    , metricOldRTL :: FileIdentity, metricNewRTL :: FileIdentity
    , metricEvidence :: [FileIdentity]
    } deriving (Show)

-- Retain complete comment fields containing known tool/directive markers.
-- This is a documented metric convention, not a semantic classification of
-- every possible third-party annotation language.
protected :: [String] -> Bool
protected = any marked
  where
    marked s = any (\marker -> isInfixOf marker (map toLower s)) markers
    markers = ["synopsys","synthesis","pragma","translate_","full_case","parallel_case",
               "verilator","verible","lint","coverage","attribute","(*","*)",[toEnum 96],
               "synplify","altera","xilinx","quartus","cadence","vivado",
               "dont_touch","dont touch","keep_hierarchy","mark_debug","async_reg"]

keepComments comments = if protected comments then comments else []

stripInformation :: VProgram -> VProgram
stripInformation (VProgram modules dpi comments) =
    VProgram (map moduleInfo modules) dpi (keepComments comments)
  where
    moduleInfo m = m { vm_comments=keepComments (vm_comments m)
                    , vm_ports=[(args,keepComments cs) | (args,cs)<-vm_ports m]
                    , vm_body=map item (vm_body m) }
    item (VMComment cs x) = case keepComments cs of
        [] -> item x
        ys -> VMComment ys (item x)
    item (VMRegGroup _ _ cs x) = case keepComments cs of
        [] -> item x
        ys -> VMComment ys (item x)
    item (VMGroup off groups) = VMGroup off (map (map item) groups)
    item x = x

trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace
normalizedLines = unlines . filter (not . null) . map trim . lines

-- Preserve statement/arm boundaries, declarations (including their partition)
-- and item order. Only prettyprinter soft wrapping and blank space disappear.
normalizeProgram :: VProgram -> String
normalizeProgram = normalizedLines . pretty 1000000000 1000000000 .
                   pPrint PDReadable 0 . stripInformation

-- The native header is an authenticated compiler comment field, not parsed RTL.
-- Keep it whole if it contains a recognized directive/tool marker.
normalizeHeader header =
    if protected (lines header) then normalizedLines header else ""

need value message = unless value (ioError (userError ("ECO metric: " ++ message)))
bytes = TE.encodeUtf8 . T.pack

-- Immutable, authenticated values shared by whole-file and translated metrics.
-- Constructors are private: callers cannot substitute an AST or renderer after
-- authentication. This is operation-local context, never proof authority.
data Loaded = Loaded MetricSource VerilogInfo VProgram FileIdentity
data MetricInputs = MetricInputs Loaded Loaded

-- Keep both projections on the same decoded inputs. Recheck identities and
-- native bindings after the callback, including when it only inspects the ASTs.
withMetricInputs :: MetricInput -> MetricInput -> (MetricInputs -> IO a) -> IO a
withMetricInputs old new action = do
    a <- load old
    b <- load new
    let inputs = MetricInputs a b
    result <- action inputs
    validateMetricInputs inputs
    return result

metricInputPrograms :: MetricInputs -> ((VerilogInfo,VProgram),(VerilogInfo,VProgram))
metricInputPrograms (MetricInputs (Loaded _ a ap _) (Loaded _ b bp _)) =
    ((a,ap),(b,bp))

validateMetricInputs :: MetricInputs -> IO ()
validateMetricInputs (MetricInputs a b) = mapM_ validate [a,b]
  where
    validate (Loaded source snapshot _ raw) = do
        mapM_ unchanged (nub (raw:sourceIdentities source))
        validateVerilogInfo snapshot

load :: MetricInput -> IO Loaded
load input = do
    snapshot <- readVerilogInfo (case input of Snapshot p -> p; ContextAST p _ _ -> p)
    exact <- verilogRenderingIsExact snapshot
    need exact "captured output does not exactly render its typed AST; filtered or unbound normalization is unsupported"
    (source,program,raw) <- case input of
      Snapshot context -> do
        identity <- fileIdentity context
        return (SnapshotSource identity,viProgram snapshot,aeOutput (viEnvelope snapshot))
      ContextAST context path rtl -> do
        [contextId,astId,actual] <- mapM fileIdentity [context,path,rtl]
        serialized <- BS.readFile path
        let candidate = decodeStrict serialized :: VProgram
        need (fixedPorts candidate == fixedPorts (viProgram snapshot))
             "candidate changed its renderer context's module ports or parameters"
        actualBytes <- BS.readFile (fiPath actual)
        need (bytes (renderVerilogText snapshot candidate) == actualBytes)
             "candidate AST and actual RTL bytes do not agree under the authenticated renderer"
        return (CandidateSource contextId astId actual,candidate,actual)
    mapM_ unchanged (sourceIdentities source)
    validateVerilogInfo snapshot
    return (Loaded source snapshot program raw)

-- Compare each automatic/direct result separately to the SAME OLD input.
-- ContextAST integrity is not an equivalence proof; report qualification means
-- only that the counted AST and actual bytes agree in an authenticated context.
measureAST :: FilePath -> MetricInput -> MetricInput -> IO MetricReport
measureAST output old new = withMetricInputs old new (measureMetricInputs output)

-- Revalidate on entry as well as exit, so retaining an opaque context between
-- calls cannot turn it into an unchecked cache of stale input bindings.
measureMetricInputs :: FilePath -> MetricInputs -> IO MetricReport
measureMetricInputs output inputs@(MetricInputs (Loaded old a ap ar) (Loaded new b bp br)) = do
    exists <- doesPathExist output
    need (not exists) "output directory exists; use a fresh path"
    validateMetricInputs inputs
    createDirectoryIfMissing True output
    let left=output </> "old.normalized.v"
        right=output </> "new.normalized.v"
    writeFile left (normalizeHeader (viHeader a) ++ normalizeProgram ap)
    writeFile right (normalizeHeader (viHeader b) ++ normalizeProgram bp)
    raw <- lineCost (fiPath ar) (fiPath br)
    normalized <- lineCost left right
    validateMetricInputs inputs
    generated <- mapM fileIdentity [left,right]
    let result=MetricReport old new raw normalized ar br
                 (nub (sourceIdentities old++sourceIdentities new++[ar,br]++generated))
    writeFile (output </> "report.json") (renderMetricReport result ++ "\n")
    return result

-- Policy identifiers name the count conventions documented in README.md (Measure);
-- the raw whole-file identifier is the search objective's own.
metricReportJSON :: MetricReport -> EvidenceJSON
metricReportJSON r = JObject
    [("schema",JNumber "2"),("equivalence_proof_performed",JBool False)
    ,("old_input",source (metricOld r)),("new_input",source (metricNew r))
    ,("old_rtl",identityJSON (metricOldRTL r)),("new_rtl",identityJSON (metricNewRTL r))
    ,("raw",costJSON "whole-file-raw-v1" (metricRaw r))
    ,("normalized",costJSON "whole-file-normalized-v1" (metricNormalized r))
    ,("evidence",JArray (map identityJSON (metricEvidence r)))]
  where
    source (SnapshotSource context) = JObject [("kind",JString "snapshot"),("context",identityJSON context)]
    source (CandidateSource context ast rtl) = JObject
        [("kind",JString "candidate"),("context",identityJSON context),("ast",identityJSON ast),("rtl",identityJSON rtl)]

identityJSON :: FileIdentity -> EvidenceJSON
identityJSON (FileIdentity path digest) = JObject [("path",JString path),("sha256",JString digest)]

costJSON :: String -> LineCost -> EvidenceJSON
costJSON policy c = JObject [("policy",JString policy),("added",number (addedLines c)),
                             ("deleted",number (deletedLines c)),("changed",number (changedLines c))]
  where number = JNumber . show

renderMetricReport :: MetricReport -> String
renderMetricReport = renderEvidenceJSON . metricReportJSON
