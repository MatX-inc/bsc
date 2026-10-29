module Main where
import ECOASTHelperReuse
import ECOASTProof
import ECOASTSearch
import PPrint(pp80)
import Verilog
import qualified Data.Generics as G
import Control.Monad
import Data.Either(isLeft)
import Data.IORef
import Data.List(isInfixOf)
import System.Environment
import System.Directory
import System.FilePath
v=mkVId;e=VEVar . v
r n=Just(VEConst(n-1),VEConst 0)
d n bits ty=VMDecl(VVDecl ty(r bits)[VVar(v n)])
a n x=VMAssign(VLId(v n))x
plus x y=mkVEOp x VAdd y
set n x=VAssignA(VLId(v n))x
clock=VEEposedge(e "clock")
p xs=VMStmt False(Valways(VAt clock(VSeq xs)))
i xs=VMStmt True(Vinitial(VSeq[VAssign(VLId(v n))(VEWConst defaultVId 8 10 0)|n<-xs]))
prog body=VProgram[VModule(v "Demo")[][([VAInput(v "clock")Nothing,VAInput(v "a")(r 8),VAInput(v "b")(r 8),VAInput(v "c")(r 8),VAOutput(v "out")(r 8)],[])]body][][]
old=prog[d "stage" 8 VDReg,d "result" 8 VDReg,d "stage_next" 8 VDWire,d "result_next" 8 VDWire,d "historical" 8 VDWire,a "stage_next"(plus(e "a")(e "b")),a "historical"(plus(e "stage")(e "c")),a "result_next"(e "historical"),a "out"(e "result"),p[set "stage"(e "stage_next"),set "result"(e "result_next")],i["stage","result"]]
current=prog[d "result" 8 VDReg,d "result_next" 8 VDWire,a "result_next"(plus(plus(e "a")(e "b"))(e "c")),a "out"(e "result"),p[set "result"(e "result_next")],i["result"]]
moduleMap f(VProgram[m]ds cs)=VProgram[f m]ds cs
append x=moduleMap(\m->m{vm_body=vm_body m++x})
replace n rhs=moduleMap(\m->m{vm_body=map(\x->case x of VMAssign(VLId q)_|getVIdString q==n->a n rhs;_->x)(vm_body m)})
getOne a b=case helperReuseCandidates a b of Right[x]->Right x;Left x->Left x;_->Left"expected exactly one proposal"
main=do
 [ys,out]<-getArgs
 createDirectory out
 counter<-newIORef(0::Int)
 let check label b=unless b(fail label)>>modifyIORef' counter(+1)>>putStrLn("PASS "++label)
 h<-either fail return(getOne old current)
 check "removed state is recorded as evidence"("removedState" `isInfixOf`show(reuseOperations h))
 check "existing OLD helper is reused as live logic"("stage_next" `isInfixOf`show(reuseOperations h))
 x<-runASTProof ys 30(out</>"live")StrictFourState(reuseAlpha h)current(reuseLive h)
 check "strict CURRENT to reconstructed live proof"(proofPassed x)
 let wrong=replace "historical"(e "c")(reuseLive h)
 z<-runASTProof ys 30(out</>"wrong-live")StrictFourState(reuseAlpha h)current wrong
 check "wrong reconstructed function rejects"(not(proofPassed z))
 check "no removed state returns reason"(isLeft(getOne current current))
 check "new state excluded"(isLeft(getOne old(append[d "added" 8 VDReg,p[set "added"(e "a")]]current)))
 check "duplicate declarations reject"(isLeft(getOne(append[d "stage_next" 8 VDWire]old)current))
 check "duplicate drivers reject"(isLeft(getOne(append[a "stage_next"(e "b")]old)current))
 check "module directive rejects"(isLeft(getOne(moduleMap(\m->m{vm_comments=["synthesis keep"]})old)current))
 let foreignItem=VMFunction(VFunction(v "foreign")(r 8)[](VAssign(VLId(v "foreign"))(e "a")))
 check "opaque function rejects"(isLeft(getOne(append[foreignItem]old)current))
 let comb body=VMStmt False(Valways(VAt(VEE(e "a"))body))
 check "effectful process rejects"(isLeft(getOne(append[d "impure" 8 VDReg,comb(VSeq[VAssign(VLId(v "impure"))(e "a"),VTask(v "$display")[e "a"]])]old)current))
 check "missing sensitivity rejects"(isLeft(getOne(append[d "bad" 8 VDReg,comb(VAssign(VLId(v "bad"))(e "b"))]old)current))
 check "partial procedural assignment rejects"(isLeft(getOne(append[d "bad" 8 VDReg,comb(VAssign(VLSub(VLId(v "bad"))(VEConst 0))(VEWConst defaultVId 1 10 0))]old)current))
 check "combinational self read rejects"(isLeft(getOne(append[d "bad" 8 VDReg,VMStmt False(Valways(VAt(VEE(e "bad"))(VAssign(VLId(v "bad"))(e "bad"))))]old)current))
 let trunc=moduleMap(\m->m{vm_body=map(\x->case x of VMDecl(VVDecl t _ [VVar q])|getVIdString q=="stage_next"->d "stage_next" 4 t;_->x)(vm_body m)})old
 ht<-either fail return(getOne trunc current)
 check "truncating OLD boundary is reported, never expanded as exact"("unknown/unequal declaration-to-RHS width" `isInfixOf`show(reuseOperations ht))
 let unknown=moduleMap(\m->m{vm_body=map(\x->case x of VMDecl(VVDecl t _ [VVar q])|getVIdString q=="stage_next"->VMDecl(VVDecl t(Just(e "dynamic",VEConst 0))[VVar q]);_->x)(vm_body m)})old
 hu<-either fail return(getOne unknown current)
 check "unknown width excluded from bypass evidence"(not("(\"stage\",\"stage_next\")" `isInfixOf`show(reuseOperations hu)))
 let chained=replace "result_next"(e "chain0")(append(concat[[d("chain"++show n)8 VDWire,a("chain"++show n)(if n==70 then plus(e "a")(e "b")else e("chain"++show(n+1))) ]|n<-[0::Int ..70]])current)
 check "finite expansion depth budget enforced"(case getOne old chained of Left why->"budget" `isInfixOf`why;_->False)
 let tooMany=append[d("extra"++show n)8 VDWire|n<-[0::Int ..4096]]old
 check "finite declaration budget enforced"(case getOne tooMany current of Left why->"budget" `isInfixOf`why;_->False)
 let cycleC=replace "result_next"(e "loop0")(append[d "loop0" 8 VDWire,d "loop1" 8 VDWire,a "loop0"(e "loop1"),a "loop1"(e "loop0")]current)
 check "CURRENT expansion cycle explicitly rejected"(case getOne old cycleC of Left why->"cyclic" `isInfixOf`why;_->False)
 check "two scalar procedural writers reject"(isLeft(getOne(append[d "twice" 8 VDReg,comb(VAssign(VLId(v "twice"))(e "a")),comb(VAssign(VLId(v "twice"))(e "a"))]old)current))
 check "continuous plus procedural writer rejects"(isLeft(getOne(append[d "twice" 8 VDReg,a "twice"(e "a"),comb(VAssign(VLId(v "twice"))(e "a"))]old)current))
 check "inconsistent port/body width rejects"(isLeft(getOne(append[d "out" 4 VDWire]old)current))
 let latchOld=moduleMap(\m->m{vm_body=map(\x->case x of VMDecl(VVDecl _ _ [VVar q])|getVIdString q=="stage_next"->d "stage_next" 8 VDReg;VMAssign(VLId q)_|getVIdString q=="stage_next"->VMStmt False(Valways(VAt(VEEOr(VEE(e "a"))(VEE(e "b")))(Vcase(e "a")[VCaseArm[VEWConst defaultVId 8 10 0](Vif(e "b")(VAssign(VLId q)(e "b"))),VDefault(VAssign(VLId q)(e "b"))]False False)));_->x)(vm_body m)})old
 hl<-either fail return(getOne latchOld current)
 check "unused partial process is removed from the live projection"(not("stage_next" `isInfixOf`show(reuseLive hl)))
 let scaffold=append[d "ordinary_unused" 8 VDWire,a "ordinary_unused"(e "a")]
     obsolete=append[d "OLD_unused" 8 VDWire,a "OLD_unused"(e "a")]
 hs<-either fail return(getOne(obsolete(scaffold old))(scaffold current))
 check "unchanged CURRENT helper scaffold stays in normal form"("ordinary_unused" `isInfixOf`show(reuseLive hs))
 check "OLD-only disconnected helper is deleted by live policy"(not("OLD_unused" `isInfixOf`show(reuseLive hs)))
 let options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["F26LiveHelpers"],soYosys=ys}
 result<-optimizeAST (defaultSearchRequest options (out</>"search")
   (SearchInput old pp80 []) (SearchInput current pp80 [])) {searchProvider=helperReuseProvider}
 check "live provider participates in standard cost selection"(changedLines(srCost result)<changedLines(srOrdinaryCost result)&&not(null(planSteps(srPlan result))))
 replayed<-replayASTPlan [] options pp80(out</>"search/search.plan")(out</>"replay")current
 check "standard saved replay rechecks live projection without provider"(replayed==srProgram result)
 check "selected AST removes the obsolete stage"(not("VId \"stage\"" `isInfixOf`show replayed))
 n<-readIORef counter
 writeFile(out</>"summary.txt")("PASS "++show n++" directed checks\n")
