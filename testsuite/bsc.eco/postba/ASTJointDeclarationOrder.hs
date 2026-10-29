module Main where
import Control.Monad(unless,forM_)
import Data.Either(isRight,isLeft)
import ECOASTJointDeclarationOrder
import ECOASTFinishOrder(firstQualified)
import ECOASTValueNames(renameValues)
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch
import ECOASTProof
import BinData(encode)
import GenABin()
import PPrint(pp80)
import System.Environment
import System.FilePath
import Verilog
v=mkVId
e=VEVar . v
r w=Just(VEConst(w-1),VEConst 0)
d t w ns=VMDecl(VVDecl t(r w)(map(VVar . v)ns))
a n rhs=VMAssign(VLId(v n))rhs
base xs=VProgram[VModule(v"m")[][([VAInput(v"x")(r 8),VAInput(v"clock")Nothing,VAOutput(v"y")(r 8)],[])]xs][][]
group n wire reversed=VMRegGroup(v n)"RegUN"["CURRENT wrapper"](VMGroup False
 [if reversed then [d VDWire 8[wire],d VDReg 8[n]]else[d VDReg 8[n],d VDWire 8[wire]]])
state n wire=VMStmt False(Valways(VAt(VEEposedge(e"clock"))(VAssignA(VLId(v n))(e wire))))
old=base[group"discard""carry"False,group"q""operand"False,a"carry"(e"x"),a"operand"(e"discard"),state"discard""carry",state"q""operand",a"y"(e"q")]
current=base[group"q""carry"True,a"carry"(e"x"),state"q""carry",a"y"(e"q")]
expected=renameValues[("carry","operand")](base[group"q""carry"False,a"carry"(e"x"),state"q""carry",a"y"(e"q")])
body f(VProgram[m]ds cs)=VProgram[m{vm_body=f(vm_body m)}]ds cs
append xs=body(++xs)
changeGroup f=body(map(\x->case x of VMRegGroup n t c b->VMRegGroup n t c(f b);_->x))
changeDecl f=changeGroup(\x->case x of VMGroup b gs->VMGroup b(map(map f)gs);_->x)
no before after=null(jointDeclarationOrderCandidates 1 before after)
check label ok=unless ok(error label)
main=do
 [out,self,ys]<-getArgs
 let generated=jointDeclarationOrderCandidates 1 old current
     candidate=head generated
     pairs=[("carry","operand")]
     unsafe x=no old(append[x]current)
     duplicate=append[d VDWire 8["carry"]]current
     protected=changeGroup(\x->VMComment["synthesis keep"]x)current
     ambiguous=changeGroup(\x->case x of VMGroup b [row]->VMGroup b[row++[d VDWire 8["other"]]];_->x)
     ambiguousOld=changeGroup(\x->case x of VMGroup b [row]->VMGroup b[row++[d VDWire 8["old_other"]]];_->x)
     extraRead=VMStmt True(Vinitial(VTask(v"$display")[e"carry"]))
     wireWidth x=case x of VMDecl(VVDecl VDWire _ vs)->VMDecl(VVDecl VDWire(r 7)vs);_->x
     partition=changeDecl(\x->case x of VMDecl(VVDecl VDReg q vs)->VMDecl(VVDecl VDReg q(vs++[VVar(v"extra")]));_->x)
     off=changeGroup(\x->case x of VMGroup _ gs->VMGroup True gs;_->x)
     array x=case x of VMDecl(VVDecl VDWire q [VVar i])->VMDecl(VVDecl VDWire q[VArray(VEConst 3,VEConst 0)i]);_->x
     initializer x=case x of VMDecl(VVDecl VDWire q [i])->VMDecl(VVDWire q i(e"x"));_->x
     strict p c=isRight(checkAST StrictFourState(acStateMap c)p(acProgram c))
     namesOnly=renameValues pairs current
     independentOld=append[group"other""fresh"False,a"fresh"(e"x"),state"other""fresh"]old
     independentNew=append[group"other""unsafe"True,a"unsafe"(e"x"),state"other""unsafe",VMStmt True(Vinitial(VTask(v"unsafe")[]))]current
     changedState=body(map(\x->case x of
       VMStmt{}->VMStmt False(Valways(VAt(VEEposedge(e"clock"))(VAssignA(VLId(v"q"))(VEWConst defaultVId 8 10 0))))
       _->x))expected
     tests=
      [("joint candidate derives wire role without suffix",length generated==1&&encode(acProgram candidate)==encode expected)
       ,("internal order only",let cs=jointDeclarationOrderCandidates 1 old namesOnly in length cs==1&&null(acStateMap(head cs))&&encode(acProgram(head cs))==encode expected)
      ,("unsafe group leaves independent safe group eligible",let cs=jointDeclarationOrderCandidates 1 independentOld independentNew in length cs==1&&acStateMap(head cs)==[("operand","carry")]&&lookup "declined-register-groups"(acOperations(head cs))/=Just[])
      ,("candidate maps only wire",acStateMap candidate==[("operand","carry")])
      ,("strict four-state alpha and order identity",strict current candidate)
      ,("serialized preservation guard",jointDeclarationOrderOnly pairs current expected)
      ,("fixed point",no old expected)
      ,("no isolated steps emitted",all((=="JointDeclarationOrder").acFeature)generated)
      ,("empty budget",null(jointDeclarationOrderCandidates 0 old current)&&null(jointDeclarationOrderCandidates(-1)old current))
      ,("disabled family",null(cpGenerate jointDeclarationOrderProvider[][]1 old current))
      ,("duplicate source declaration",no old duplicate)
      ,("duplicate OLD declaration",no(append[d VDWire 8["operand"]]old)current)
      ,("occupied target declaration",unsafe(d VDWire 8["operand"]))
      ,("occupied target expression",unsafe(a"unused"(e"operand")))
      ,("nonunique wire role",no(ambiguousOld old)(ambiguous current))
      ,("wire width mismatch",no old(changeDecl wireWidth current))
      ,("CURRENT array barrier",no old(changeDecl array current))
      ,("CURRENT initializer barrier",no old(changeDecl initializer current))
      ,("OLD array barrier",no(changeDecl array old)current)
      ,("OLD initializer barrier",no(changeDecl initializer old)current)
      ,("CURRENT translate-off barrier",no old(off current))
      ,("OLD translate-off barrier",no(off old)current)
      ,("directive comment barrier",no old protected)
      ,("module directive",no old(case current of VProgram[m]ds cs->VProgram[m{vm_comments=["pragma keep"]}]ds cs))
      ,("group type mismatch",no old(body(map(\x->case x of VMRegGroup n _ c b->VMRegGroup n"Different"c b;_->x))current))
      ,("missing canonical state",no old(changeDecl(\x->case x of VMDecl(VVDecl VDReg q _)->d VDReg 8["another"];_->x)current))
      ,("multiple state members",no old(partition current))
      ,("procedural writer",unsafe(VMStmt True(Vinitial(VAssign(VLId(v"carry"))(e"x")))))
      ,("duplicate continuous writer",unsafe(a"carry"(e"x")))
       ,("indexed identity",unsafe(a"unused"(VEIndex(v"carry")(VEConst 0))))
      ,("annotation identity preserved",let p=body(map(\x->case x of VMAssign(VLId i)_|i==v"carry"->a"carry"(VEOp(v"carry")(e"x")VAnd(e"x"));_->x))current;cs=jointDeclarationOrderCandidates 1 old p in length cs==1&&jointDeclarationOrderOnly pairs p(acProgram(head cs)))
      ,("event reference",unsafe(VMStmt False(Valways(VAt(VEEposedge(e"carry"))(VSeq[])))))
      ,("hierarchical reference",unsafe(a"unused"(e"carry.bit")))
      ,("task identity",unsafe(VMStmt True(Vinitial(VTask(v"carry")[]))))
      ,("instance identity",unsafe(VMInst(v"Child")(v"carry")(Right [])[]))
      ,("port collision",let expose(VProgram[m]ds cs)=VProgram[m{vm_ports=vm_ports m++[([VAOutput(v"carry")(r 8)],[])]}]ds cs in no(expose old)(expose current))
      ,("fixed interface",no old(case current of VProgram[m]ds cs->VProgram[m{vm_ports=[]}]ds cs))
      ,("effect value reads renamed",let p=append[extraRead]current;cs=jointDeclarationOrderCandidates 1 old p in length cs==1&&strict p(head cs))
      ,("mutated value rejected by guard",not(jointDeclarationOrderOnly pairs current(body(map(\x->case x of VMAssign(VLId i)_|i==v"operand"->a"operand"(VEConst 0);_->x))expected)))
      ,("mutated width rejected by guard",not(jointDeclarationOrderOnly pairs current(changeDecl wireWidth expected)))
      ,("changed partition rejected by guard",not(jointDeclarationOrderOnly pairs current(partition expected)))
      ,("changed wrapper rejected by guard",not(jointDeclarationOrderOnly pairs current(changeGroup(VMComment["extra"])expected)))
      ,("noninjective map rejected",not(jointDeclarationOrderOnly[("carry","operand"),("carry","other")]current expected))
      ,("changed state transition rejected",isLeft(checkAST StrictFourState[]current changedState))]
 forM_ tests(uncurry check)
 let opts=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["JointDeclarationOrder"],soYosys=ys}
     provider=jointDeclarationOrderProvider{cpSources=self:cpSources jointDeclarationOrderProvider}
 result<-optimizeAST (defaultSearchRequest opts (out</>"search")
   (SearchInput old pp80) (SearchInput current pp80)) {searchProvider=provider}
 check"search selects joint candidate"(encode(srProgram result)==encode expected)
 replay<-replayASTPlan opts pp80(out</>"search/search.plan")(out</>"replay")current
 check"saved plan independently replayed"(encode replay==encode expected)
 fallback<-firstQualified[base[],expected] $ \i p->pure$
  if isRight(checkAST StrictFourState[("operand","carry")]current p)then Just(i,p)else Nothing
 check"failed best proof tries valid next candidate"(case fallback of Just(2,p)->encode p==encode expected;_->False)
 putStrLn("PASS: "++show(length tests+3)++" joint name/order guard, strict proof, fallback and replay checks")
