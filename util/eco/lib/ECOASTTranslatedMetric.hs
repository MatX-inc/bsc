{-# LANGUAGE ScopedTypeVariables #-}
-- Reporting projection only. This does not alter candidates or relax proof.
module ECOASTTranslatedMetric
  ( TranslationProjection(..), projectTranslatedProgram
  , TranslatedMetricReport(..), measureTranslatedAST, renderTranslatedMetricReport
  ) where
import Control.Monad(unless,forM_)
import qualified Data.ByteString as B
import Data.Char(toLower)
import Data.Data
import Data.List(isInfixOf)
import System.Directory(doesPathExist,createDirectory)
import System.FilePath((</>))
import BinData(encode)
import GenABin()
import Verilog
import ECOAST
import ECOASTMetric
import ECOASTCost(LineCost(..),lineCost,changedLines)
import ECO.Replay
import ECOASTInvariant(EvidenceJSON(..),renderEvidenceJSON)

data TranslationProjection = TranslationProjection
  { removedItems :: Int, removedInitialBlocks :: Int
  , removedAssertions :: Int, removedTaskCalls :: Int
  , nativeOffWrappers :: Int, nestedOffWrappers :: Int
  } deriving (Eq,Read,Show)
zero = TranslationProjection 0 0 0 0 0 0
plus (TranslationProjection a b c d e n) (TranslationProjection f g h i j m) =
  TranslationProjection (a+f) (b+g) (c+h) (d+i) (e+j) (n+m)
wrapper = zero { nativeOffWrappers=1 }
collect :: forall b a. (Typeable b,Data a)=>a->[b]
collect x=maybe [] (:[]) (cast x) ++ concat(gmapQ collect x)
removed x = TranslationProjection 1 (length [()|Vinitial{}<-ss])
  (length [()|VAssert{}<-ss]) (length [()|VTask{}<-ss]) 0 0
  where ss=collect x::[VStmt]

-- Unknown textual switch directives cannot be interpreted using native flags.
-- Inspect comment fields, never parse emitted RTL or quoted expressions.
checkComments :: [String] -> Either String ()
checkComments cs
  | any (\s->any (`isInfixOf` map toLower s) markers) cs =
      Left "translated metric: textual synthesis switch in a comment/header is unsupported; use native translate flags"
  | otherwise = Right ()
  where markers=["translate_off","translate_on","translate off","translate on",
                 "synthesis off","synthesis on","synopsys off","synopsys on"]

-- Native Verilog PPrint emits off/on pairs, not lexically nested regions.
-- A nested off/on pair therefore enables the remaining outer body. Preserve
-- that ordered toggle semantics, including wrapper comments emitted before
-- their child. The result is a metric artifact, not replacement RTL.
projectTranslatedProgram :: VProgram -> Either String (VProgram,TranslationProjection)
projectTranslatedProgram (VProgram modules dpi cs) = do
  checkComments cs
  -- VAssert embeds an unquoted template in the native printer. It must not
  -- smuggle an unmodeled switch even when the surrounding item is disabled.
  mapM_ checkComments [[a,b]|VAssert _ (VEString a:VEString b:_)<-(collect modules::[VStmt])]
  pairs<-mapM projectModule modules
  let count=foldr plus zero(map snd pairs)
      nested=sum[sum(map(nestedItem 0)(vm_body m))|m<-modules]
  pure(VProgram (map fst pairs) dpi cs,count{nestedOffWrappers=nested})
  where
    nestedItem depth (VMGroup off gs)=
      (if off && depth>0 then 1 else 0)+sum(map(nestedItem(depth+if off then 1 else 0))(concat gs))
    nestedItem depth (VMStmt True _)=if depth>0 then 1 else 0
    nestedItem depth (VMComment _ x)=nestedItem depth x
    nestedItem depth (VMRegGroup _ _ _ x)=nestedItem depth x
    nestedItem _ _=0
    projectModule m = do
      checkComments(vm_comments m)
      mapM_ (checkComments.snd) (vm_ports m)
      (_,body,counts)<-items True(vm_body m)
      pure(m{vm_body=body},counts)
    items enabled []=Right(enabled,[],zero)
    items enabled (x:xs)=do
      (next,kept,a)<-item enabled x
      (final,rest,b)<-items next xs
      pure(final,kept++rest,plus a b)
    groups enabled []=Right(enabled,[],zero)
    groups enabled (x:xs)=do
      (next,kept,a)<-items enabled x
      (final,rest,b)<-groups next xs
      pure(final,kept:rest,plus a b)
    item enabled (VMGroup off gs)=do
      (next,kept,count)<-groups (if off then False else enabled) gs
      let body=if all null kept then [] else [VMGroup False kept]
      pure(if off then True else next,body,plus count(if off then wrapper else zero))
    item _ x@(VMStmt True _) = Right(True,[],plus (removed x) wrapper)
    item enabled (VMComment cs child)=do
      checkComments cs
      (next,kept,count)<-item enabled child
      -- A leading live informational comment is itself translated. Preserve
      -- it even if its disabled child disappears; normalized strips it later.
      let attach []=if enabled && not(null cs) then [VMComment cs(VMGroup False [])] else []
          attach [x]=[if enabled then VMComment cs x else x]
          attach xs=if enabled then [VMComment cs(VMGroup False[xs])] else xs
      pure(next,attach kept,count)
    item enabled (VMRegGroup n def cs child)=do
      checkComments cs
      (next,kept,count)<-item enabled child
      let attach []=if enabled then [VMRegGroup n def cs(VMGroup False [])] else []
          attach [x]=[if enabled then VMRegGroup n def cs x else x]
          attach xs=if enabled then [VMRegGroup n def cs(VMGroup False[xs])] else xs
      pure(next,attach kept,count)
    item True x=Right(True,[x],zero)
    item False x=Right(False,[],removed x)

data TranslatedMetricReport = TranslatedMetricReport
  { translatedWhole :: MetricReport
  , translatedRaw :: LineCost, translatedNormalized :: LineCost
  , translatedOldProjection :: TranslationProjection
  , translatedNewProjection :: TranslationProjection
  , translatedEvidence :: [FileIdentity]
  } deriving Show

need ok message=unless ok(ioError(userError("translated metric: "++message)))
unchanged expected=do
  actual<-fileIdentity(fiPath expected)
  need(actual==expected)("input changed: "++fiPath expected)

measureTranslatedAST :: FilePath -> MetricInput -> MetricInput -> IO TranslatedMetricReport
measureTranslatedAST out old new=do
  exists<-doesPathExist out
  need(not exists)"output directory exists; use a fresh path"
  createDirectory out
  withMetricInputs old new $ \inputs -> measure inputs
  where
   measure inputs=do
    -- Whole-file and translated reports share one authenticated decode. The
    -- enclosing operation rechecks all input identities and native bindings.
    whole<-measureMetricInputs(out</>"whole")inputs
    let ((a,ap),(b,bp))=metricInputPrograms inputs
    mapM_ (either (ioError.userError) pure . checkComments . lines . viHeader) [a,b]
    (at,ac)<-either (ioError.userError) pure(projectTranslatedProgram ap)
    (bt,bc)<-either (ioError.userError) pure(projectTranslatedProgram bp)
    let rawA=out</>"old.translated.v";rawB=out</>"new.translated.v"
        normA=out</>"old.translated.normalized.v";normB=out</>"new.translated.normalized.v"
        astA=out</>"old.translated.ast";astB=out</>"new.translated.ast"
    writeFile rawA(renderVerilogText a at);writeFile rawB(renderVerilogText b bt)
    writeFile normA(normalizeHeader(viHeader a)++normalizeProgram at);writeFile normB(normalizeHeader(viHeader b)++normalizeProgram bt)
    B.writeFile astA(B.pack(encode at));B.writeFile astB(B.pack(encode bt))
    raw<-lineCost rawA rawB;normalized<-lineCost normA normB
    forM_ (metricEvidence whole++[metricOldRTL whole,metricNewRTL whole]) unchanged
    validateVerilogInfo a;validateVerilogInfo b
    files<-mapM fileIdentity[rawA,rawB,normA,normB,astA,astB,out</>"whole/report.json"]
    let report=TranslatedMetricReport whole raw normalized ac bc files
    writeFile(out</>"report.json")(renderTranslatedMetricReport report++"\n")
    pure report

renderTranslatedMetricReport :: TranslatedMetricReport -> String
renderTranslatedMetricReport r=renderEvidenceJSON(JObject
  [("schema",JNumber"1"),("qualified",JBool True),("equivalence_proof_performed",JBool False)
  ,("metric_scope",JString"native synthesis-translated projection")
  ,("raw",cost(translatedRaw r)),("normalized",cost(translatedNormalized r))
  ,("whole_file",metricReportJSON(translatedWhole r))
  ,("old_projection",projection(translatedOldProjection r)),("new_projection",projection(translatedNewProjection r))
  ,("policy",JString"Ordered native VMGroup/VMStmt translate_off/on flags only; nested wrappers follow native printer toggle order. Reject textual synthesis switches in comment/header fields. Preserve active names, expressions, declarations and declaration partition/order; keep active helper cones even when used only by excluded assertions. Assertions and initialization disappear only where their native emitted region is off.")
  ,("macro_policy",JString"Do not evaluate arbitrary preprocessor branches or macros. Generated guards owned by an excluded native item disappear with that item; active initial-block guards, reset polarity, assignment delay and parameter-string policy remain. No BSV_NO_INITIAL_BLOCKS value is assumed.")
  ,("raw_policy",JString"Re-render projected typed AST with the authenticated native wrapping/header policy, including active comments. Projection is not a deployed RTL file or a literal text-splice operation; blank separators around removed items may be regenerated. Normalized projection additionally excludes informational comment fields/formatting under the existing convention; declaration grouping stays material.")
  ,("evidence",JArray(map identity(translatedEvidence r)))])
  where
    num=JNumber . show
    cost x=JObject[("added",num(addedLines x)),("deleted",num(deletedLines x)),("changed",num(changedLines x))]
    projection x=JObject[("removed_items",num(removedItems x)),("removed_initial_blocks",num(removedInitialBlocks x)),("removed_assertions",num(removedAssertions x)),("removed_task_calls",num(removedTaskCalls x)),("native_off_wrappers",num(nativeOffWrappers x)),("nested_off_wrappers",num(nestedOffWrappers x))]
    identity i=JObject[("path",JString(fiPath i)),("sha256",JString(fiSHA256 i))]
