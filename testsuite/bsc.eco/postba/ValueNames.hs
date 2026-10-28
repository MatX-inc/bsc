module Main where
import ECOASTValueNames
import ECOASTSearch
import ECOASTTransform
import ECOASTProof
import Verilog
import qualified Data.Generics as G
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
import Data.IORef
v=mkVId;e=VEVar . v
r n=if n==1 then Nothing else Just(VEConst(n-1),VEConst 0)
decl n w=VMDecl(VVDecl VDWire(r w)[VVar(v n)])
assign n x=VMAssign(VLId(v n))x
p xs=VProgram[VModule(v "M")[] [([VAInput(v "a")(r 8),VAInput(v "b")(r 8),VAInput(v "CLK")Nothing,VAOutput(v "o")(r 8)],[])]xs][][]
rhs=VEOp defaultVId(e "a")VAdd(e "b")
body n=[decl n 8,assign n rhs,assign "o"(e n)]
effect n=VMStmt True(Valways(VAt(VEEnegedge(e "CLK"))(Vif(e n)(VTask(v "$display")[VEString "fresh",e n]))))
old=p(body "historic"++[effect "historic"])
new=p(body "fresh"++[effect "fresh"])
props a b=cpGenerate valueNamesProvider[]["ValueNames"]8 a b
append x(VProgram[m]ds cs)=VProgram[m{vm_body=vm_body m++[x]}]ds cs
main=do
 [ys,out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label b=unless b(fail label)>>modifyIORef' count(+1)>>putStrLn("PASS "++label)
 check "compound pure value names proposed through effect arguments"(length(props old new)==1)
 let c=head(props old new)
 check "exact inverse correspondence"(acStateMap c==[("historic","fresh")])
 q<-runASTProof ys 30(out</>"effect-positive")StrictFourState(acStateMap c)new(acProgram c)
 check "unchanged strict effect inventory proves alpha"(proofPassed q)
 check "whole candidate is exact OLD for unchanged value"(acProgram c==old)
 let annotationCurrent=p[decl "fresh" 8,assign "fresh"(VEOp(v "fresh")(e "a")VAdd(e "b")),assign "o"(e "fresh"),effect "fresh"]
     annotationCandidate=head(props old annotationCurrent)
     VProgram [am] _ _=acProgram annotationCandidate
 check "operator annotation metadata not renamed" ([i|VMAssign _ (VEOp i _ _ _)<-vm_body am]==[v "fresh"])
 annotationProof<-runASTProof ys 30(out</>"metadata-positive")StrictFourState(acStateMap annotationCandidate)annotationCurrent(acProgram annotationCandidate)
 check "metadata-preserving effect alpha strictly proved"(proofPassed annotationProof)
 let bad=p[decl "historic" 8,assign "historic"(e "a"),assign "o"(e "historic"),effect "historic"]
 b<-runASTProof ys 30(out</>"changed-rhs-negative")StrictFourState(acStateMap c)new bad
 check "changed value rejected independently"(not(proofPassed b))
 b2<-runASTProof ys 30(out</>"missing-alpha-negative")StrictFourState[]new(acProgram c)
 check "effect name change without inverse mapping rejected"(not(proofPassed b2))
 let reject label item=check label (null (props old (append item new)))
 reject "event value use excluded" $ VMStmt False $ Valways $ VAt (VEEposedge(e "fresh")) (VTask(v "$display")[])
 reject "assert event value excluded" $ VMStmt True $ VAssert (VEE(e "fresh")) [e "a"]
 reject "callable identifier collision excluded" $ VMStmt True $ Vinitial $ VTask(v "historic")[e "fresh"]
 let call n=p[decl n 8,assign n(VEFctCall(v "f")[e "a"])]
 check "call expression excluded from RHS evidence" (null(props (call "historic") (call "fresh")))
 reject "procedural writer excluded" $ VMStmt False $ Vinitial $ VAssign(VLId(v "fresh"))(e "a")
 reject "partial continuous writer excluded" $ VMAssign(VLSub(VLId(v "fresh"))(VEConst 0))(VEConst 0)
 reject "duplicate definition excluded" $ assign "fresh" rhs
 reject "duplicate declaration excluded" $ decl "fresh" 8
 check "width mismatch excluded" (null(props (p[decl "historic" 4,assign "historic" rhs]) (p[decl "fresh" 8,assign "fresh" rhs])))
 reject "occupied target excluded" $ decl "historic" 8
 let duplicate=append(assign "fresh2" rhs)(append(decl "fresh2" 8)new)
 check "ambiguous RHS owners excluded" (null(props old duplicate))
 check "ambiguity reason recorded" (not(null(vnaRejected(valueNamesAudit old duplicate))))
 let off=p[VMGroup True[body "fresh"]]
 check "translate-off driver excluded" (null(props old off))
 reject "directive comment excluded" $ VMComment["synthesis keep"](assign "z"(e "fresh"))
 reject "function scope excluded" $ VMFunction $ VFunction(v "f")(r 8)[](VAssign(VLId(v "f"))(e "fresh"))
 let dpi=VDPI(v "f")VDT_void[]
     globalDpi=case new of VProgram ms _ cs->VProgram ms[dpi]cs
 check "program DPI scope excluded" (null(props old globalDpi))
 reject "instance connection excluded" $ VMInst(v "Child")(v "child")(Right [])[(v "I",Just(e "fresh"))]
 reject "dumpvars nonvalue use excluded" $ VMStmt True $ Vdumpvars 0[v "fresh"]
 reject "array binder excluded" $ VMDecl $ VVDecl VDWire(r 8)[VArray(VEConst 1,VEConst 0)(v "fresh")]
 reject "initialized wire scope excluded" $ VMDecl $ VVDWire(r 8)(VVar(v "z"))(e "fresh")
 reject "range metadata excluded" $ VMDecl $ VVDecl VDWire(Just(e "fresh",VEConst 0))[VVar(v "z")]
 let addPort a(VProgram[m]ds cs)=VProgram[m{vm_ports=vm_ports m++[([a],[])]}]ds cs
     portReject label a=check label(null(props(addPort a old)(addPort a new)))
 portReject "inout identity reserved" (VAInout(v "historic")Nothing(Just(r 8)))
 portReject "parameter identity reserved" (VAParameter(v "historic")(r 8)(VEConst 0))
 portReject "parameter default value reference reserved" (VAParameter(v "param")(r 8)(e "fresh"))
 portReject "port range reference reserved" (VAInput(v "bus")(Just(e "fresh",VEConst 0)))
 reject "body port width conflict rejected" $ decl "a" 4
 check "zero budget"(null(cpGenerate valueNamesProvider[]["ValueNames"]0 old new))
 check "explicit naming opt-in required"(null(cpGenerate valueNamesProvider[]["ValueNamesLayout"]8 old new))
 let joint=cpGenerate valueNamesProvider[]["ValueNames","ValueNamesLayout"]8 old new
 check "joint layout bounded"(not(null joint)&&length joint<=5)
 check "joint alpha preserved"(all((==acStateMap c).acStateMap)joint)
 check "budget caps expanded candidates"(length(cpGenerate valueNamesProvider[]["ValueNames","ValueNamesLayout"]1 old new)==1)
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS "++show n++" checks\n")
