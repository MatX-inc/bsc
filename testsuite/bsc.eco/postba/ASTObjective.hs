module Main where
import Control.Exception(IOException,try,finally)
import Control.Monad(unless,forM_)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Data.List(isInfixOf)
import System.Directory
import System.Environment(getArgs)
import System.FilePath
import Verilog
import PPrint(pp80)
import ECOASTCost
import ECOASTSearch
import ECOASTProof
import ECOASTTransform(ASTCandidate(..))
import ECOASTTranslatedMetric(projectTranslatedProgram)
import ECOASTMetric(normalizeProgram)
v=mkVId
e=VEVar . v
assign n x=VMAssign(VLId(v n))x
wire n=VMDecl(VVDecl VDWire Nothing[VVar(v n)])
program name notes=VProgram[VModule(v"M")[] [([VAInput(v"a")Nothing,VAOutput(v"o")Nothing],[])]
 [wire name,assign name(e"a"),assign"o"(e name),VMComment notes(VMStmt True(VTask(v"$display")[e"a"]))]][][]
check s b=unless b(fail s)>>putStrLn("PASS: "++s)
reject s action=do
 r<-try(action>>pure())::IO(Either IOException())
 check s(either(const True)(const False)r)
runner opts dir alpha cs=runASTProofWithContracts[][]cs(soYosys opts)(soProofSeconds opts)dir(soDomain opts)alpha
translated sources=CostProvider "test-translated-normalized-v1" sources render render
 where render p=normalizeProgram.fst <$> projectTranslatedProgram p
provider candidate alpha=CandidateProvider ["FObjective"] [] $ \_ _ _ _ current ->
 if current==candidate then [] else [ASTCandidate"FObjective""directed native AST candidate"candidate[]False alpha []]
main=do
 [out]<-getArgs
 createDirectory out
 let source=out</>"policy-input.txt"
 writeFile source"policy-v1\n"
 let objective=translated[source]
     old=program"historical"["old comment"]
     ordinary=program"fresh"["new comment"]
     assertion p text=case p of VProgram[m]d c->VProgram[m{vm_body=vm_body m++[VMStmt True(VAssert(VEEposedge(e"a"))[VEString"property",VEString text,e"a"])]}]d c
     assertionOld=assertion old "old assertion"
     assertionOrdinary=assertion ordinary "new assertion"
     assertionOnly=assertion ordinary "old assertion"
     improved=program"historical"["debug "++show i|i<-[1::Int ..40]]
     options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["FObjective"],soRounds=2,soCandidates=4}
     request opts label o n=defaultSearchRequest opts (out</>label)
       (SearchInput o pp80) (SearchInput n pp80)
     search label p o n rs=optimizeAST (request options label o n)
       {searchObjective=objective,searchProvider=p,searchProof=runner,searchRenames=rs}
     replay label policy= replayASTPlanWithCost policy old runner options pp80(out</>"improved/search.plan")(out</>label)ordinary
 rejected<-search"assertion-only"(provider assertionOnly[])assertionOld assertionOrdinary[]
 check"assertion-only textual improvement is not selected"(null(planSteps(srPlan rejected))&&srProgram rejected==assertionOrdinary)
 result<-search"improved"(provider improved[("historical","fresh")])old ordinary[]
 check"translated improvement accepted even when whole-file worse"
  (changedLines(srObjectiveCost result)<changedLines(srOrdinaryObjectiveCost result)&&changedLines(srCost result)>changedLines(srOrdinaryCost result))
 check"complete emitted candidate includes excluded debug comments" . isInfixOf "debug 40" =<< readFile(out</>"improved/selected.v")
 check"semantic plan remains schema2 with full-file costs"(planSchema(srPlan result)==2&&stepAfterCost(head(planSteps(srPlan result)))==srCost result)
 (again,selected,baseline)<-replay"replay"objective
 check"saved objective independently reproves and rescored same candidate"(again==improved&&selected==srObjectiveCost result&&baseline==srOrdinaryObjectiveCost result)
 -- Caller-specified mode is independent of mutable saved envelope.
 reject"changed expected objective rejected"(replay"wrong-mode"objective{costPolicy="different"})
 let envelopePath=out</>"improved/objective.plan"
 envelopeBytes<-B.readFile envelopePath
 let tamperEnvelope label edit=do
       envelope<-read . C.unpack <$> B.readFile envelopePath
       writeFile envelopePath(show(edit envelope)++"\n")
       reject label(replay label objective) `finally` B.writeFile envelopePath envelopeBytes
 tamperEnvelope"tampered-policy"(\x->x{costPolicyIdentity="different"})
 tamperEnvelope"tampered-plan-hash"(\x->x{costSemanticPlanHash="bad"})
 tamperEnvelope"tampered-old-ast"(\x->x{costOldASTHash="bad"})
 tamperEnvelope"tampered-score"(\x->x{costObservations=[p{costValue=LineCost 99 99}|p<-costObservations x]})
 let witness=out</>"improved"</>costWitness(last(costObservations(srCostEnvelope result)))
 witnessBytes<-B.readFile witness
 B.appendFile witness(B.pack[10])
 reject"changed score witness rejected"(replay"changed-witness"objective) `finally` B.writeFile witness witnessBytes
 let oldWitness=out</>"improved/objective/old.score.v"
 oldBytes<-B.readFile oldWitness
 B.appendFile oldWitness(B.pack[10])
 reject"changed OLD score witness rejected"(replay"changed-old-witness"objective) `finally` B.writeFile oldWitness oldBytes
 appendFile source"changed\n"
 reject"changed policy source rejected"(replay"changed-source"objective) `finally` writeFile source"policy-v1\n"
 let differentOld=program"different"["old comment"]
 reject"different OLD baseline rejected"(replayASTPlanWithCost objective differentOld runner options pp80(out</>"improved/search.plan")(out</>"changed-old")ordinary)
 let nameOptions=options{soFeatures=["F01"]}
 named<-optimizeAST (request nameOptions "named" old ordinary)
   {searchObjective=objective,searchProof=runner,searchRenames=[("fresh","historical")]}
 check"initial naming uses translated objective"(changedLines(srObjectiveCost named)==0&&not(srRolledBack named))
 rolled<-optimizeAST (request nameOptions "rollback" old old)
   {searchObjective=objective,searchProof=runner,searchRenames=[("historical","fresh")]}
 check"worsening translated initial names roll back"(srRolledBack rolled&&null(planSteps(srPlan rolled))&&changedLines(srObjectiveCost rolled)==0)
 (rollback,_,_)<-replayASTPlanWithCost objective old runner nameOptions pp80(out</>"rollback/search.plan")(out</>"rollback-replay")old
 check"rollback objective envelope replays"(rollback==old)
 -- An unsafe observable change remains blocked irrespective of its score.
 let unsafe=case old of VProgram[m]ds cs->VProgram[m{vm_body=wire"historical":assign"historical"(VEConst 0):drop 2(vm_body m)}]ds cs
 blocked<-search"unsafe"(provider unsafe[("historical","fresh")])old ordinary[]
 check"objective cannot bypass semantic proof"(srProgram blocked==ordinary)
 putStrLn"PASS: objective controls complete"
