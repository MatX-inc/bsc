-- A second reporting metric over the native AST, never a search objective or
-- a proof. Actual emitted bytes remain the raw OLD-relative comparison.
module ECOASTMetric
    ( MetricInput(..), MetricReport(..), normalizeProgram, stripInformation
    , MetricInputs, withMetricInputs, metricInputPrograms, measureMetricInputs
    , measureAST, metricReportJSON, renderMetricReport, normalizeHeader
    ) where

import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.Char (isSpace, toLower)
import Data.List (isInfixOf, nub)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (doesPathExist, createDirectoryIfMissing)
import System.FilePath ((</>))
import BinData (decode)
import GenABin ()
import ECOAST
import ECOASTInvariant (EvidenceJSON(..), renderEvidenceJSON)
import ECO.Interface (fixedPorts)
import ECOASTCost (LineCost(..), lineCost, changedLines)
import ECO.Replay (FileIdentity(..), fileIdentity)
import PPrint
import Verilog

-- A candidate is bound to a checked capture's renderer and to actual RTL
-- bytes. A bare AST without its capture context is intentionally unsupported.
data MetricInput = Snapshot FilePath | ContextAST FilePath FilePath FilePath
    deriving (Eq, Read, Show)

data MetricReport = MetricReport
    { metricOld :: MetricInput, metricNew :: MetricInput
    , metricRaw :: LineCost, metricNormalized :: LineCost
    , metricOldRTL :: FileIdentity, metricNewRTL :: FileIdentity
    , metricRendering :: ASTRenderingPolicy
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
    item (VMInst moduleName instanceName (Left parameters) ports) =
        VMInst moduleName instanceName (Left (map parameter parameters)) ports
    item x = x
    parameter (Just comment,value) | not (protected [comment]) = (Nothing,value)
    parameter pair = pair

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
data Loaded = Loaded MetricInput VerilogInfo VProgram FileIdentity [FileIdentity]
data MetricInputs = MetricInputs Loaded Loaded

-- Keep both projections on the same decoded inputs. Recheck identities and
-- native bindings after the callback, including when it only inspects the ASTs.
withMetricInputs :: MetricInput -> MetricInput -> (MetricInputs -> IO a) -> IO a
withMetricInputs old new action = do
    a@(Loaded _ left _ _ _) <- load old
    b@(Loaded _ right _ _ _) <- load new
    need (astRenderingPolicy left == astRenderingPolicy right)
         "renderer policies differ; no qualified normalized comparison"
    let inputs = MetricInputs a b
    result <- action inputs
    validateMetricInputs inputs
    return result

metricInputPrograms :: MetricInputs -> ((VerilogInfo,VProgram),(VerilogInfo,VProgram))
metricInputPrograms (MetricInputs (Loaded _ a ap _ _) (Loaded _ b bp _ _)) =
    ((a,ap),(b,bp))

validateMetricInputs :: MetricInputs -> IO ()
validateMetricInputs (MetricInputs a b) = mapM_ validate [a,b]
  where
    validate (Loaded _ snapshot _ raw identities) = do
        mapM_ unchanged (nub (raw:identities))
        validateVerilogInfo snapshot

load :: MetricInput -> IO Loaded
load input = do
    let (context,extra) = case input of
          Snapshot p -> (p,[])
          ContextAST p ast rtl -> (p,[ast,rtl])
    initial <- mapM fileIdentity (context:extra)
    snapshot <- readVerilogInfo context
    exact <- verilogRenderingIsExact snapshot
    need exact "captured output does not exactly render its typed AST; filtered or unbound normalization is unsupported"
    (program,raw) <- case input of
      Snapshot _ -> return (viProgram snapshot,aeOutput (viEnvelope snapshot))
      ContextAST _ path rtl -> do
        serialized <- BS.readFile path
        let candidate = decode serialized :: VProgram
        need (fixedPorts candidate == fixedPorts (viProgram snapshot))
             "candidate changed its renderer context's module ports or parameters"
        actual <- fileIdentity rtl
        actualBytes <- BS.readFile (fiPath actual)
        need (bytes (renderVerilogText snapshot candidate) == actualBytes)
             "candidate AST and actual RTL bytes do not agree under the authenticated renderer"
        return (candidate,actual)
    mapM_ unchanged initial
    validateVerilogInfo snapshot
    return (Loaded input snapshot program raw initial)

unchanged expected = do
    actual <- fileIdentity (fiPath expected)
    need (actual == expected) ("input changed: " ++ fiPath expected)

-- Compare each automatic/direct result separately to the SAME OLD input.
-- ContextAST integrity is not an equivalence proof; report qualification means
-- only that the counted AST and actual bytes agree in an authenticated context.
measureAST :: FilePath -> MetricInput -> MetricInput -> IO MetricReport
measureAST output old new = withMetricInputs old new (measureMetricInputs output)

-- Revalidate on entry as well as exit, so retaining an opaque context between
-- calls cannot turn it into an unchecked cache of stale input bindings.
measureMetricInputs :: FilePath -> MetricInputs -> IO MetricReport
measureMetricInputs output inputs@(MetricInputs (Loaded old a ap ar ai) (Loaded new b bp br bi)) = do
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
    let result=MetricReport old new raw normalized ar br (astRenderingPolicy a)
                 (nub (ai++bi++[ar,br]++generated))
    writeFile (output </> "report.json") (renderMetricReport result ++ "\n")
    return result

metricReportJSON :: MetricReport -> EvidenceJSON
metricReportJSON r = JObject
    [("schema",JNumber "1"),("qualified",JBool True),("equivalence_proof_performed",JBool False)
    ,("baseline_policy",JString "Compare the same OLD input independently to ordinary, automatic and direct outputs; automatic-versus-direct is informational only.")
    ,("old_input",JString (show (metricOld r))),("new_input",JString (show (metricNew r)))
    ,("old_rtl",file (metricOldRTL r)),("new_rtl",file (metricNewRTL r))
    ,("raw",cost (metricRaw r)),("normalized",cost (metricNormalized r))
    ,("algorithm",JString "Git Myers added+deleted lines")
    ,("renderer",JString (show (metricRendering r)))
    ,("normalization",JString "Canonical native PPrint with soft wrapping disabled; trim indentation/trailing space and omit blank lines. Strip informational compiler/AST comment fields, retain complete fields containing listed directive/tool markers and all native macro, translate-off and case flags. Preserve names, operators, literals, declaration partition and member order.")
    ,("scope",JString "Normalized lines are a separate textual metric, not raw lines minus a subset, equivalence, source intent, timing, physical ECO cost or a global minimum. The known-marker comment convention is not a universal third-party annotation semantics.")
    ,("evidence",JArray (map file (metricEvidence r)))]
  where
    file (FileIdentity path digest) = JObject [("path",JString path),("sha256",JString digest)]
    number = JNumber . show
    cost c = JObject [("added",number (addedLines c)),("deleted",number (deletedLines c)),
                      ("changed",number (changedLines c))]

renderMetricReport :: MetricReport -> String
renderMetricReport = renderEvidenceJSON . metricReportJSON
