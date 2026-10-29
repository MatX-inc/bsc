-- Optional external policy over recorded accepted search prefixes. This utility
-- measures their costs, exposes Pareto tradeoffs, and independently
-- replays selected prefixes. No RTL parsing or candidate rewriting.
module ECOASTSelection (SelectionPolicy(..), CostPoint(..), paretoPoints, choosePoint, selectPrefixes, selectPrefixesWithMetric) where
import ECO.VerilogInfo
import ECOBind
import ECOASTMetric
import ECOASTSearch
import ECOASTProofContext
import ECO.Replay(FileIdentity(..),fileIdentity)
import ECOASTReplay
import ECOJSON(EvidenceJSON(..),renderEvidenceJSON)
import BinData(encode,decode)
import GenABin()
import qualified Data.ByteString as B
import Control.Monad
import Data.List(minimumBy,nub)
import Data.Ord(comparing)
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
need b s=unless b(failWith"prefix selector"s)
json=JString;num=JNumber . show
object xs=renderEvidenceJSON(JObject xs)++"\n"
pointJSON p=JObject[("key",json(pointKey p)),("raw",num(pointRaw p)),("normalized",num(pointNormalized p))]
candidateJSON c=JObject[("point",pointJSON(candidatePoint c)),("flow",json(candidateFlow c)),("branch",json(candidateBranch c)),
 ("accepted_steps",num(candidateLength c)),("ast_sha256",json(candidateDigest c)),("actual_rtl",json(candidateRTL c))]

selectPrefixes :: TrustRoots -> FilePath -> [FilePath] -> IO ()
selectPrefixes = selectPrefixesWithMetric "whole-file" [] $ \dir a b -> do
 r<-measureAST dir a b
 pure(metricRaw r,metricNormalized r)

-- Metric policy changes selection only; semantic prefix plans and complete RTL
-- remain unchanged and are independently replayed before a selection is used.
selectPrefixesWithMetric :: String -> [FilePath] ->
 (FilePath -> MetricInput -> MetricInput -> IO (LineCost,LineCost)) ->
 TrustRoots -> FilePath -> [FilePath] -> IO ()
selectPrefixesWithMetric metricPolicy metricSources measure roots output flows=do
 sourceIDs<-mapM fileIdentity metricSources
 need(not(null flows)&&length flows<=16)"need 1..16 flows"
 exists<-doesPathExist output;need(not exists)"fresh output directory required"
 createDirectory output
 solver<-fileIdentity(trSolver roots)
 loaded<-forM(zip[0::Int ..]flows)$ \(index,flow)->do
  let base=output</>("flow-"++show index)
  createDirectory base
  l<-loadFlow roots flow base
  -- Prefixes of different flows are comparable only over the same
  -- authenticated OLD/ordinary NEW ASTs, RTL and child metadata.
  let old=lfOld l;ordinary=lfOrdinary l
      oldASTPath=base</>"old-authenticated.ast";newASTPath=base</>"new-authenticated.ast"
  B.writeFile oldASTPath(B.pack(encode(viProgram old)))
  B.writeFile newASTPath(B.pack(encode(viProgram ordinary)))
  oldAST<-fileIdentity oldASTPath;newAST<-fileIdentity newASTPath
  let baseline=(cfModule(lfConfig l),fiSHA256 oldAST,fiSHA256 newAST,
        fiSHA256(aeOutput(viEnvelope old)),fiSHA256(aeOutput(viEnvelope ordinary)),
        show(childPinsFromInstances(viInstances old)),show(childPinsFromInstances(viInstances ordinary)))
  pure(index,base,l,baseline)
 need(length(nub[baseline|(_,_,_,baseline)<-loaded])==1)"authenticated OLD/ordinary NEW AST, RTL or child metadata differs across flows"
 candidates<-fmap concat$forM loaded$ \(index,base,l,_)->fmap concat$forM(lfPlans l)$ \(branch,planIdentity,plan,_)->do
  let modul=cfModule(lfConfig l);flow=lfFlow l
      oldPath=flow</>"adapted-old"</>"capture"</>modul<.>"eco-ast"
      contextPath=if branch=="ordinary"then flow</>"adapted-new"</>"capture"</>modul<.>"eco-ast"else flow</>"kept"</>modul<.>"eco-ast"
      bundle=takeDirectory(fiPath planIdentity)
  context<-if branch=="ordinary"then pure(lfOrdinary l)else maybe(failWith"prefix selector""kept plan without kept context")pure(lfKept l)
  forM[0..length(planSteps plan)]$ \count->do
   let key=show index++"-"++branch++"-"++show count;dir=base</>(branch++"-prefix-"++show count)
       steps=take count(planSteps plan);digest=if null steps then planInput plan else stepAfter(last steps)
   createDirectory dir
   program<-if null steps then pure(viProgram context)else do
     path<-boundMember bundle(stepArtifact(last steps))(stepArtifactHash(last steps))
     decode<$>B.readFile path
   let ast=dir</>"candidate.ast";rtl=dir</>modul<.>"v"
   B.writeFile ast(B.pack(encode program))
   emitVerilog context rtl program
   (rawCost,normalizedCost)<-measure(dir</>"metric")(Snapshot oldPath)(ContextAST contextPath ast rtl)
   pure(Candidate(CostPoint key(changedLines rawCost)(changedLines normalizedCost))flow branch count planIdentity rtl digest)
 let points=map candidatePoint candidates;frontier=paretoPoints points
 need(not(null candidates))"no recorded accepted prefixes"
 writeFile(output</>"frontier.json")(object[("schema",JNumber"1"),("proof_domain",json(show(trDomain roots))),
   ("selection_metric",json metricPolicy),("metric_sources",json(show sourceIDs)),("search_objective",json"Recorded source plans remain unchanged"),("selection_scope",json"Recorded accepted prefixes only; no global minimum claim. Costs are authenticated typed-AST/emitted-byte metrics, not proof receipts."),
   ("candidates",JArray(map candidateJSON candidates)),("pareto",JArray(map pointJSON frontier))])
 forM_[RawFirst,NormalizedFirst] $ \policy->do
  let chosen=choosePoint policy points;c=head[x|x<-candidates,candidatePoint x==chosen]
      destination=output</>show policy
  verified<-replayFlowPrefix roots(candidateFlow c)(candidateBranch c)(candidateLength c)(candidateSourcePlan c)destination
  let context=replayContext verified;rtl=fiPath(replayRTL verified);modul=aeModule(viEnvelope context)
  a<-B.readFile rtl;b<-B.readFile(candidateRTL c);need(a==b)"replayed output differs from measured prefix"
  let oldPath=candidateFlow c</>"adapted-old"</>"capture"</>modul<.>"eco-ast"
      contextPath=candidateFlow c</>(if candidateBranch c=="ordinary" then "adapted-new"</>"capture" else "kept")</>modul<.>"eco-ast"
  (freshRaw,freshNormalized)<-measure(destination</>"selected-metric") (Snapshot oldPath)
    (ContextAST contextPath(fiPath(replayAST verified))rtl)
  need(changedLines freshRaw==pointRaw chosen&&changedLines freshNormalized==pointNormalized chosen)
    "selected metric changed after independent replay"
  validateReplay verified
  unchanged(candidateSourcePlan c)
  writeFile(destination</>"selection.json")(object[("schema",JNumber"1"),("policy",json(show policy)),("selected",candidateJSON c),
    ("fresh_independent_replay_passed",JBool True),("output",json rtl)])
  putStrLn(show policy++" "++show(pointRaw chosen)++"/"++show(pointNormalized chosen)++" "++pointKey chosen)
 mapM_ (unchanged.candidateSourcePlan)candidates
 forM_ loaded$ \(_,_,l,_)->do
  mapM_ unchanged(lfBindings l)
  mapM_ validateVerilogInfo(lfOld l:lfOrdinary l:maybe[]pure(lfKept l))
 unchanged solver
 mapM_ unchanged sourceIDs
 writeFile(output</>"completed.json")(object[("schema",JNumber"1"),("input_bindings_rechecked",JBool True),
  ("selected_snapshots_revalidated",JBool True),("policies",JArray(map(json.show)[RawFirst,NormalizedFirst]))])
