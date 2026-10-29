module Main where
import ECOASTCorrespondence
import ECOASTSearch
import ECOASTTransform
import ECOASTProof
import Verilog
import Control.Monad
import System.Environment
import System.Directory
import System.FilePath
import Data.IORef
import Data.List(sort)
v=mkVId;e=VEVar . v
range n=if n==1 then Nothing else Just(VEConst(n-1),VEConst 0)
decl n bits=VMDecl(VVDecl VDWire(range bits)[VVar(v n)])
assign n r=VMAssign(VLId(v n))r
prog body=VProgram[VModule(v "M")[] [([VAInput(v "a")(range 8),VAInput(v "b")(range 8),VAInput(v "c")Nothing,VAOutput(v "o")(range 8)],[])] body][][]
role n bits val=prog[decl n bits,assign n val,assign "o"(VEIf(e "c")(e n)(VEWConst defaultVId 8 10 0))]
old=role "historical" 4(VESelect(e "a")(VEConst 3)(VEConst 0))
current=role "grown" 8(e "a")
props feats a b=cpGenerate correspondenceProvider[] feats 10 a b
main=do
 [ys,out]<-getArgs
 createDirectory out
 counter<-newIORef(0::Int)
 let check msg b=unless b(fail msg)>>modifyIORef' counter(+1)>>putStrLn("PASS "++msg)
     widthProps=props["F24WidthRole"]
     exactProps=props["F24ExactLeaf"]
 check "unique width growth role"(length(widthProps old current)==1)
 let grown=head(widthProps old current)
 p<-runASTProof ys 20(out</>"width-positive")StrictFourState(acStateMap grown)current(acProgram grown)
 check "strict proof preserves NEW width/value"(proofPassed p)
 bad<-runASTProof ys 20(out</>"wrong-old-value")StrictFourState(acStateMap grown)current old
 check "restoring old truncated value is rejected"(not(proofPassed bad))
 check "unchanged width is not this family"(null(widthProps(role "historical" 8(e "a"))current))
 check "width shrink is excluded"(null(widthProps current old))
 let occupied=prog[decl "grown" 8,decl "historical" 4,assign "grown"(e "a"),assign "historical"(VESelect(e "b")(VEConst 3)(VEConst 0)),assign "o"(VEIf(e "c")(e "grown")(VEWConst defaultVId 8 10 0))]
 check "occupied historical target rejected"(null(widthProps old occupied))
 let ambiguousOld=prog[decl "x" 4,decl "y" 4,assign "x"(VESelect(e "a")(VEConst 3)(VEConst 0)),assign "y"(VESelect(e "b")(VEConst 3)(VEConst 0)),assign "o"(VEIf(e "c")(e "x")(e "y"))]
     ambiguousNew=prog[decl "z" 8,assign "z"(e "a"),assign "o"(VEIf(e "c")(e "z")(e "z"))]
 check "ambiguous owner roles rejected"(null(widthProps ambiguousOld ambiguousNew))
 check "ambiguity is reported"(not(null(caRejected(correspondenceAudit 2 ambiguousOld ambiguousNew))))
 let swapOld=prog[decl "x" 8,decl "y" 8,assign "x"(e "a"),assign "y"(e "b"),assign "o"(VEIf(e "c")(e "x")(e "y"))]
     swapNew=prog[decl "x" 8,decl "y" 8,assign "x"(e "b"),assign "y"(e "a"),assign "o"(VEIf(e "c")(e "y")(e "x"))]
 check "exact typed permutation proposed"(length(exactProps swapOld swapNew)==1)
 let swap=head(exactProps swapOld swapNew)
 check "permutation is bijective"(length(acStateMap swap)==2)
 q<-runASTProof ys 20(out</>"permutation-positive")StrictFourState(acStateMap swap)swapNew(acProgram swap)
 check "strict permutation proof"(proofPassed q)
 check "exact permutation restores old assignment roles"(sort(acStateMap swap)==sort[("x","y"),("y","x")])
 let dup=prog[decl "x" 8,decl "y" 8,assign "x"(e "a"),assign "y"(e "a"),assign "o"(e "x")]
 check "duplicate exact expressions are not guessed"(null(exactProps swapOld dup))
 check "exact ambiguity reason"(not(null(caRejected(correspondenceAudit 2 swapOld dup))))
 let displacement=prog[decl "x" 8,decl "fresh" 8,assign "x"(e "b"),assign "fresh"(e "a"),assign "o"(e "fresh")]
     one=prog[decl "x" 8,assign "x"(e "a"),assign "o"(e "x")]
 check "occupied eligible target moves into vacated name"(length(exactProps one displacement)==1&&length(acStateMap(head(exactProps one displacement)))==2)
 let displaced=head(exactProps one displacement)
 z<-runASTProof ys 20(out</>"displacement-positive")StrictFourState(acStateMap displaced)displacement(acProgram displaced)
 check "displacement strictly proved"(proofPassed z)
 let impure=prog[decl "x" 8,decl "fresh" 8,assign "fresh"(e "a"),VMStmt False(Vinitial(VAssign(VLId(v "x"))(e "b"))),assign "o"(e "fresh")]
 check "occupied state target rejected"(null(exactProps one impure))
 let duplicate=prog[decl "grown" 8,decl "grown" 8,assign "grown"(e "a"),assign "o"(VEIf(e "c")(e "grown")(VEWConst defaultVId 8 10 0))]
 check "duplicate declarations reject width role"(null(widthProps old duplicate))
 check "zero budget is explicit"(null(cpGenerate correspondenceProvider[]["F24WidthRole"]0 old current)&&caBudgetLimited(correspondenceAudit 0 old current))
 check "disabled feature emits nothing"(null(props[]old current))
 let select i=VESelect1(e "a")(VEConst i)
     bit n i=prog[decl n 1,assign n(select i),assign "o"(VERepeat(VEConst 8)(e n))]
 check "different bit positions are not exact evidence"(null(exactProps(bit "x" 0)(bit "y" 1)))
 check "out of bounds selectors excluded"(null(exactProps(bit "x" 9)(bit "y" 9)))
 let macro n=prog[decl n 8,assign n(VEMacro "UNKNOWN"),assign "o"(e n)]
 check "macro expressions are not exact evidence"(null(exactProps(macro "x")(macro "y")))
 let guarded=case current of VProgram[m]ds cs->VProgram[m{vm_body=[VMGroup True[vm_body m]]}]ds cs
 check "translate-off definitions excluded"(null(widthProps old guarded))
 let extraWriter=case current of VProgram[m]ds cs->VProgram[m{vm_body=vm_body m++[VMStmt False(Vinitial(VAssign(VLId(v "grown"))(e "a")))]}]ds cs
 check "extra procedural writer excluded"(null(widthProps old extraWriter))
 let mapModule f (VProgram ms ds cs)=VProgram(map f ms)ds cs
     append x=mapModule(\m->m{vm_body=vm_body m++[x]})current
     programComment(VProgram ms ds _)=VProgram ms ds["synthesis keep"]
     moduleComment=mapModule(\m->m{vm_comments=["pragma keep"]})
     portComment=mapModule(\m->m{vm_ports=[(as,["attribute keep"] )|(as,_)<-vm_ports m]})
     wrapperComment=mapModule(\m->m{vm_body=[VMComment["verilator lint_off"](VMGroup False[vm_body m])]})
     regComment=mapModule(\m->m{vm_body=[VMRegGroup(v "r")"Reg"["synopsys keep"](VMGroup False[vm_body m])]})
 forM_[("program",programComment),("module",moduleComment),("port",portComment),("wrapper",wrapperComment),("register group",regComment)] $ \(label,decorate) -> do
  check(label++" directive rejects NEW proposal")(null(widthProps old(decorate current)))
  check(label++" directive rejects OLD proposal")(null(widthProps(decorate old)current))
 check "directive rejection explained"(not(null(caRejected(correspondenceAudit 10 old(programComment current)))))
 let function=VMFunction(VFunction(v "f")(range 8)[](VAssign(VLId(v "f"))(e "grown")))
     dpi=VDPI(v "foreign_fn")VDT_void[(v "grown",False,VDT_int)]
     globalDpi=case current of VProgram ms _ cs->VProgram ms[dpi]cs
 check "function reference reserves external wire"(null(widthProps old(append function)))
 check "program DPI reserves mentioned wire"(null(widthProps old globalDpi))
 check "translate-off group read reserves external wire"(null(widthProps old(append(VMGroup True[[assign "o"(e "grown")]]))))
 check "translate-off statement read reserves external wire"(null(widthProps old(append(VMStmt True(Vinitial(VTask(v "$display")[e "grown"]))))))
 let joint=props["F24ExactLeaf","F24Layout","F04"]swapOld swapNew
     bundled=[x|x<-joint,acFeature x=="F24Layout"]
 check "bounded joint naming/layout proposed"(not(null bundled)&&length bundled<=4)
 check "joint retains name correspondence"(all((==acStateMap swap).acStateMap)bundled)
 jp<-runASTProof ys 20(out</>"joint-layout-positive")StrictFourState(acStateMap(head bundled))swapNew(acProgram(head bundled))
 check "joint layout strictly proved against CURRENT"(proofPassed jp)
 check "combined provider preserves explicit joint opt-in"(any((=="F24Layout").acFeature)(cpGenerate(combineCandidateProviders[correspondenceProvider,defaultCandidateProvider])[]["F24ExactLeaf","F24Layout","F04"]10 swapOld swapNew))
 check "joint family independently opt-in"(all((/="F24Layout").acFeature)(props["F24ExactLeaf","F04"]swapOld swapNew))
 check "joint cannot bypass disabled naming family"(null(props["F24Layout","F04"]swapOld swapNew))
 check "joint bounded by caller candidate budget"(length(cpGenerate correspondenceProvider[]["F24ExactLeaf","F24Layout","F04"]1 swapOld swapNew)==1)
 n<-readIORef counter
 writeFile(out</>"summary.txt")("PASS "++show n++" checks\n")
