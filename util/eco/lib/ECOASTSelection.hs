-- Optional external policy over recorded accepted search prefixes. This utility
-- measures their costs, exposes Pareto tradeoffs, and independently
-- replays selected prefixes. No RTL parsing or candidate rewriting.
module ECOASTSelection (SelectionPolicy(..), CostPoint(..), paretoPoints, choosePoint, selectPrefixes, selectPrefixesWithMetric) where
import ECOAST
import ECOASTMetric
import ECOASTSearch
import ECOASTProof
import ECOASTProofContext
import ECO.Replay(FileIdentity(..),fileIdentity)
import ECOASTFlow(CaptureFlow(..))
import ECO.Interface(fixedPorts)
import ECOASTReplay
import Verilog
import BinData(encode,decode)
import GenABin()
import qualified Data.ByteString as B
import Control.Monad
import Data.List(minimumBy,intercalate,nubBy)
import Data.Ord(comparing)
import Data.Char(ord)
import Numeric(showHex)
import System.Directory
import System.FilePath

data SelectionPolicy=RawFirst|NormalizedFirst deriving(Eq,Read,Show)
data CostPoint=CostPoint{pointKey::String,pointRaw::Int,pointNormalized::Int}deriving(Eq,Show)
paretoPoints ps=[p|p<-ps,not(any(`dominates`p)ps)]where
 dominates a b=pointRaw a<=pointRaw b&&pointNormalized a<=pointNormalized b&&
   (pointRaw a<pointRaw b||pointNormalized a<pointNormalized b)
choosePoint policy ps=minimumBy(comparing score)(paretoPoints ps)where
 score p=case policy of RawFirst->(pointRaw p,pointNormalized p,pointKey p)
                        NormalizedFirst->(pointNormalized p,pointRaw p,pointKey p)
data Candidate=Candidate
 { candidatePoint::CostPoint,candidateFlow::FilePath,candidateBranch::String,candidateLength::Int
 , candidateSourcePlan::FileIdentity,candidateRTL::FilePath
 , candidateDigest::String }
need b s=unless b(fail("prefix selector: "++s))
unchanged f=fileIdentity(fiPath f) >>= \x->need(x==f)("input changed: "++fiPath f)
json s='"':concatMap esc s++"\""where
 esc '"'="\\\"";esc '\\'="\\\\";esc '\n'="\\n";esc '\r'="\\r";esc '\t'="\\t"
 esc c|ord c<32=let h=showHex(ord c)""in"\\u"++replicate(4-length h)'0'++h
      |otherwise=[c]
object xs="{"++intercalate","[json k++":"++v|(k,v)<-xs]++"}"
array xs="["++intercalate","xs++"]"
pointJSON p=object[("key",json(pointKey p)),("raw",show(pointRaw p)),("normalized",show(pointNormalized p))]
candidateJSON c=object[("point",pointJSON(candidatePoint c)),("flow",json(candidateFlow c)),("branch",json(candidateBranch c)),
 ("accepted_steps",show(candidateLength c)),("ast_sha256",json(candidateDigest c)),("actual_rtl",json(candidateRTL c))]
-- A bundle member is untrusted input; require containment before reading it.
member bundle relative expected=do
 need(isRelative relative&&".."`notElem`splitDirectories relative)"artifact path escapes bundle"
 root<-canonicalizePath bundle
 path<-canonicalizePath(root</>relative)
 need(take(length(addTrailingPathSeparator root))path==addTrailingPathSeparator root)"artifact symlink escapes bundle"
 identity<-fileIdentity path
 need(fiSHA256 identity==expected)"artifact digest changed"
 return path

selectPrefixes :: ProofDomain -> FilePath -> Int -> FilePath -> [FilePath] -> IO ()
selectPrefixes = selectPrefixesWithMetric "whole-file" [] $ \dir a b -> do
 r<-measureAST dir a b
 pure(metricRaw r,metricNormalized r)

-- Metric policy changes selection only; semantic prefix plans and complete RTL
-- remain unchanged and are independently replayed before a selection is used.
selectPrefixesWithMetric :: String -> [FilePath] ->
 (FilePath -> MetricInput -> MetricInput -> IO (LineCost,LineCost)) ->
 ProofDomain -> FilePath -> Int -> FilePath -> [FilePath] -> IO ()
selectPrefixesWithMetric metricPolicy metricSources measure domain yosys seconds output flows=do
 sourceIDs<-mapM fileIdentity metricSources
 need(not(null flows)&&length flows<=16&&seconds>0)"need 1..16 flows and positive proof timeout"
 exists<-doesPathExist output;need(not exists)"fresh output directory required"
 createDirectory output
 solver<-fileIdentity yosys
 configs<-forM flows $ \flow->do
  effective<-doesFileExist(flow</>"effective.config")
  configuration<-fileIdentity(if effective then flow</>"effective.config" else flow++".config")
  cfg<-read <$> readFile(fiPath configuration)
  need(soDomain(cfSearch cfg)==domain)"flow proof domain differs from explicit requested domain"
  oldCap<-fileIdentity(cfOldCapture cfg);newCap<-fileIdentity(cfNewCapture cfg)
  unchanged configuration
  return(flow,cfg,oldCap,newCap,configuration)
 need(length(nubBy(==)[(cfModule c,fiSHA256 o,fiSHA256 n)|(_,c,o,n,_)<-configs])==1)"flows do not share the same OLD and NEW captures"
 (_,snapshots,flowCandidates)<-foldM (\(baseline,snapshots,accumulated) (index,(flow,cfg,_,_,_))->do
  let base=output</>("flow-"++show index);modul=cfModule cfg
      oldPath=flow</>"adapted-old"</>modul<.>"eco-ast"
      newPath=flow</>"adapted-new"</>modul<.>"eco-ast"
  createDirectory base
  old<-readVerilogInfo oldPath;ordinary<-readVerilogInfo newPath
  need(aeModule(viEnvelope old)==modul&&aeModule(viEnvelope ordinary)==modul)"snapshot module differs from configured module"
  need(fixedPorts(viProgram old)==fixedPorts(viProgram ordinary))"OLD and NEW fixed ports differ"
  _<-verifyVerilogInfo old(base</>"old-control")
  _<-verifyVerilogInfo ordinary(base</>"new-control")
  let oldASTPath=base</>"old-authenticated.ast";newASTPath=base</>"new-authenticated.ast"
  B.writeFile oldASTPath(B.pack(encode(viProgram old)))
  B.writeFile newASTPath(B.pack(encode(viProgram ordinary)))
  oldAST<-fileIdentity oldASTPath;newAST<-fileIdentity newASTPath
  let actualBaseline=(fiSHA256 oldAST,fiSHA256 newAST,
        astRenderingPolicy old,astRenderingPolicy ordinary,
        fiSHA256(aeOutput(viEnvelope old)),fiSHA256(aeOutput(viEnvelope ordinary)),
        show(childPinsFromInstances(viInstances old)),show(childPinsFromInstances(viInstances ordinary)))
  case baseline of Nothing->pure()
                   Just expected->need(expected==actualBaseline)"authenticated OLD/ordinary NEW AST, renderer, RTL or child metadata differs across flows"
  branches<-forM["ordinary","kept"] $ \branch->do
   let source=flow</>(branch++"-search")</>"search.plan"
   available<-doesFileExist source
   if not available then return([],[])else do
    context<-if branch=="ordinary"then return ordinary else readVerilogInfo(flow</>"kept"</>modul<.>"eco-ast")
    need(aeModule(viEnvelope context)==modul&&fixedPorts(viProgram context)==fixedPorts(viProgram ordinary))"branch context module/ports differ"
    _<-verifyVerilogInfo context(base</>(branch++"-control"))
    planIdentity<-fileIdentity source
    plan<-read <$> readFile source
    need(planOptions plan==cfSearch cfg&&planSchema plan==2)"plan options/schema do not match flow"
    need(length(planSteps plan)<=256)"accepted-prefix budget exceeds 256 steps per branch"
    let contextBytes=B.pack(encode(viProgram context));tmp=base</>(branch++"-input.ast")
    B.writeFile tmp contextBytes
    input<-fileIdentity tmp;rendered<-fileIdentity(fiPath(aeOutput(viEnvelope context)))
    need(fiSHA256 input==planInput plan&&fiSHA256 rendered==planRenderer plan)"plan input or renderer identity differs from capture"
    -- Validate the complete recorded chain before any cost is trusted.
    finalHash<-foldM(\previous step->do
      need(stepBefore step==previous&&stepAfter step==stepArtifactHash step)"broken accepted-step hash chain"
      _<-member(takeDirectory source)(stepArtifact step)(stepArtifactHash step)
      _<-member(takeDirectory source)(stepContractsArtifact step)(stepContractsHash step)
      return(stepAfter step))(planInput plan)(planSteps plan)
    need(finalHash==planOutput plan)"plan final hash differs from accepted chain"
    prefixes<-forM[0..length(planSteps plan)] $ \count->do
     let key=show index++"-"++branch++"-"++show count;dir=base</>(branch++"-prefix-"++show count)
         steps=take count(planSteps plan);digest=if null steps then planInput plan else stepAfter(last steps)
     createDirectory dir
     program<-if null steps then return(viProgram context)else do
       path<-member(takeDirectory source)(stepArtifact(last steps))(stepArtifactHash(last steps))
       decode <$> B.readFile path
     let ast=dir</>"candidate.ast";rtl=dir</>modul<.>"v"
     B.writeFile ast(B.pack(encode program))
     emitVerilog context rtl program
     (rawCost,normalizedCost)<-measure(dir</>"metric")(Snapshot oldPath)
       (ContextAST(if branch=="ordinary"then newPath else flow</>"kept"</>modul<.>"eco-ast")ast rtl)
     return(Candidate(CostPoint key(changedLines rawCost)(changedLines normalizedCost))flow branch count planIdentity rtl digest)
    pure([context],prefixes)
  let loaded=concatMap fst branches
  pure(Just actualBaseline,reverse loaded++(old:ordinary:snapshots),concatMap snd branches:accumulated)
  ) (Nothing,[],[]) (zip[0::Int ..]configs)
 let candidates=concat(reverse flowCandidates)
     points=map candidatePoint candidates;frontier=paretoPoints points
 need(not(null candidates))"no recorded accepted prefixes"
 writeFile(output</>"frontier.json")(object[("schema","1"),("proof_domain",json(show domain)),
   ("selection_metric",json metricPolicy),("metric_sources",json(show sourceIDs)),("search_objective",json"Recorded source plans remain unchanged"),("selection_scope",json"Recorded accepted prefixes only; no global minimum claim. Costs are authenticated typed-AST/emitted-byte metrics, not proof receipts."),
   ("candidates",array(map candidateJSON candidates)),("pareto",array(map pointJSON frontier))]++"\n")
 forM_[RawFirst,NormalizedFirst] $ \policy->do
  let chosen=choosePoint policy points;c=head[x|x<-candidates,candidatePoint x==chosen]
      destination=output</>show policy
  verified<-replayFlowPrefix domain yosys seconds(candidateFlow c)(candidateBranch c)(candidateLength c)(candidateSourcePlan c)destination
  let context=replayContext verified;rtl=fiPath(replayRTL verified)
  a<-B.readFile rtl;b<-B.readFile(candidateRTL c);need(a==b)"replayed output differs from measured prefix"
  let oldPath=candidateFlow c</>"adapted-old"</>aeModule(viEnvelope context)<.>"eco-ast"
      contextPath=candidateFlow c</>(if candidateBranch c=="ordinary" then "adapted-new" else "kept")</>aeModule(viEnvelope context)<.>"eco-ast"
  (freshRaw,freshNormalized)<-measure(destination</>"selected-metric") (Snapshot oldPath)
    (ContextAST contextPath(fiPath(replayAST verified))rtl)
  need(changedLines freshRaw==pointRaw chosen&&changedLines freshNormalized==pointNormalized chosen)
    "selected metric changed after independent replay"
  validateReplay verified
  unchanged(candidateSourcePlan c)
  writeFile(destination</>"selection.json")(object[("schema","1"),("policy",json(show policy)),("selected",candidateJSON c),
    ("fresh_independent_replay_passed","true"),("output",json rtl)]++"\n")
  putStrLn(show policy++" "++show(pointRaw chosen)++"/"++show(pointNormalized chosen)++" "++pointKey chosen)
 mapM_ (unchanged.candidateSourcePlan)candidates
 mapM_(\(_,_,oldCap,newCap,cfg)->mapM_ unchanged[oldCap,newCap,cfg])configs
 mapM_ validateVerilogInfo snapshots
 unchanged solver
 mapM_ unchanged sourceIDs
 writeFile(output</>"completed.json")(object[("schema","1"),("input_bindings_rechecked","true"),
  ("selected_snapshots_revalidated","true"),("policies",array(map(json.show)[RawFirst,NormalizedFirst]))]++"\n")
