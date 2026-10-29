module Main where
import ECOASTPipelineChain
import ECOASTProof
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..))
import Verilog
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
v=mkVId
e=VEVar . v
r=Just(VEConst 7,VEConst 0)
decl t n=VMDecl(VVDecl t r[VVar(v n)])
assign n x=VMAssign(VLId(v n))x
state n input=[VMRegGroup(v n)"RegUN"[](VMGroup False[[decl VDReg n,decl VDWire("input_"++n)]]),assign("input_"++n)input,VMStmt False(Valways(VAt(VEEposedge(e"clock"))(VAssignA(VLId(v n))(e("input_"++n)))))]
modul ns=VModule(v"Demo")[][([VAInput(v"clock")Nothing,VAInput(v"input")r,VAOutput(v"out")r],[])]
 (concat[state n rhs|(n,rhs)<-zip ns(e"input":map e ns)]++[assign"out"(e(last ns))])
prog m=VProgram[m][][]
main=do
 [ys,out]<-getArgs;createDirectory out
 let check s b=unless b(fail s)>>putStrLn("PASS "++s)
     old=modul["a","b","c","d"];current=modul["a","b"]
     proposals=pipelineChainProposals 16 old current
 check "shrinking complete chain found"(length proposals==1)
 let p=head proposals
 check "no suffix convention in tail mapping"(pipelineForwardNames p==[("a","c"),("input_a","input_c"),("b","d"),("input_b","input_d")])
 proof<-runASTProof ys 30(out</>"shrink")StrictFourState[(b,a)|(a,b)<-pipelineForwardNames p](prog current)(prog(pipelineModule p))
 check "strict shrinking-chain alpha proof"(proofPassed proof)
 let growth=pipelineChainProposals 16 current old
 check "growing chain uses a complete injective rotation"(length growth==1&&length(pipelineForwardNames(head growth))==8)
 growProof<-runASTProof ys 30(out</>"grow")StrictFourState[(b,a)|(a,b)<-pipelineForwardNames(head growth)](prog old)(prog(pipelineModule(head growth)))
 check "strict growing-chain alpha proof"(proofPassed growProof)
 let changedInput=current{vm_body=map(\x->case x of VMAssign(VLId i)_|getVIdString i=="input_a"->assign"input_a"(VEWConst defaultVId 8 10 1);_->x)(vm_body current)}
 check "different input role has no proposal"(null(pipelineChainProposals 16 old changedInput))
 let collision=current{vm_body=decl VDWire "c":vm_body current}
 check "occupied historical target outside chain rejected"(null(pipelineChainProposals 16 old collision))
 let cycle=current{vm_body=map(\x->case x of VMAssign(VLId i)_|getVIdString i=="input_a"->assign"input_a"(e"b");_->x)(vm_body current)}
 check "cyclic state graph has no complete chain"(null(pipelineChainProposals 16 old cycle))
 let branching=old{vm_body=vm_body old++state"branch"(e"b")}
 check "branching chain is not treated as unique"(null(pipelineChainProposals 16 branching current))
 check "same length chain has no rotation"(null(pipelineChainProposals 16 old old))
 check "single remaining stage can inherit historical tail"(length(pipelineChainProposals 16 old(modul["a"]))==1)
 let directive=old{vm_body=VMComment["synthesis keep"] (head(vm_body old)):tail(vm_body old)}
 check "directive bearing AST rejected"(null(pipelineChainProposals 16 directive current))
 let reasons a b=[reason|(_,_,reason)<-pipelinePairOutcomes(pipelineChainAudit 16 a b)]
 check "occupied target has an explicit reason"("occupied-target-or-noninjective-map" `elem` reasons old collision)
 check "unmatched input role has an explicit reason"("unmatched-head-input-role" `elem` reasons old current{vm_body=map(\x->case x of VMAssign(VLId i)_|getVIdString i=="input_a"->assign"input_a"(e"differentInput");_->x)(vm_body current)})
 let ambiguous=old{vm_body=vm_body old++state"extraA"(e"input")++state"extraB"(e"extraA")}
 check "multiple historical matches are explicit"("multiple-OLD-chain-matches" `elem`reasons ambiguous current)
 check "branch exclusion is explicit"(any((=="branch-in-state-transfer-graph").snd)(pipelineOldStateIssues(pipelineChainAudit 16 branching current)))
 check "cycle exclusion is explicit"(any((=="cycle-in-state-transfer-graph").snd)(pipelineCurrentStateIssues(pipelineChainAudit 16 old cycle)))
 let hierarchy=current{vm_body=vm_body current++[assign"probe"(e"c.child")]}
 check "hierarchical target is excluded explicitly"(null(pipelineChainProposals 16 old hierarchy)&&"hierarchical-source-or-target-identity" `elem`reasons old hierarchy)
 let limited=pipelineChainAudit 0 old current
 check "proposal budget is explicit"(pipelineEligible limited==1&&pipelineEmitted limited==0&&pipelineBudgetLimited limited)

 let dual first second=(modul first){
       vm_ports=[([VAInput(v "clock")Nothing,VAInput(v "input")r,VAInput(v "other")r,VAOutput(v "out")r,VAOutput(v "out2")r],[])],
       vm_body=vm_body(modul first)++concat[state n rhs|(n,rhs)<-zip second(e "other":map e second)]++[assign "out2"(e(last second))]}
     oldDual=dual ["a","b","c","d"] ["u","v","w","x"]
     newDual=dual ["a","b"] ["u","v"]
     generated budget a b=cpGenerate pipelineChainProvider [] ["F25"] budget(prog a)(prog b)
     signature p=(prog(pipelineModule p),[(b,a)|(a,b)<-pipelineForwardNames p])
     actual c=(acProgram c,acStateMap c)
 forM_[-1,0,1,2,3,16]$ \budget->do
   let expected=pipelineChainProposals budget oldDual newDual
       cs=generated budget oldDual newDual
       audit=show(pipelineChainAudit budget oldDual newDual)
   check ("provider order and joint budget "++show budget)(map actual cs==map signature expected)
   check ("provider metadata keeps complete audit "++show budget)
     (all(\c->all(\(_,fields)->lookup "audit" fields==Just audit)(acOperations c))cs)
 check "two independent chain shifts retain singles then joint"
   (map(length . pipelineOldStates)(pipelineChainProposals 16 oldDual newDual)==[4,4,8])
 let bothInvalid=directive{vm_ports=[]}
 check "global guards retain rejection precedence"
   (all(\(_,_,why)->why=="fixed-module-or-ports-differ")
     (pipelinePairOutcomes(pipelineChainAudit 16 bothInvalid current)))
