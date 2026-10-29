module Main where
import ECOASTShell
import ECOASTInactive
import Verilog
import ECOASTTransform
import qualified Data.Generics as G
import Control.Monad
import Data.IORef
import System.Environment(getArgs)
import System.Directory
import System.FilePath
v=mkVId
e=VEVar . v
lit x=VEWConst defaultVId 1 2 x
decl ns=VMDecl(VVDecl VDWire Nothing[VVar(v n)|n<-ns])
assign n rhs=VMAssign(VLId(v n))rhs
modul body=VModule(v "M")[] [([VAInput(v "a")Nothing,VAOutput(v "o")Nothing],[])]body
prog m=VProgram[m][][]
old=modul[decl["before","unused","after"],assign "before"(e "a"),assign "unused"(lit 1),assign "after"(lit 1),assign "o"(e "before")]
current=modul[decl["before","after"],assign "before"(VEUnOp defaultVId VNot(e "a")),assign "after"(lit 1),assign "o"(e "before")]
protected x=VMGroup True[[VMStmt False(Vinitial(VAssign(VLId(v "opaque_state"))(lit x)))]]
stateDecl=VMDecl(VVDecl VDReg Nothing[VVar(v "opaque_state")])
withOpaque m x=m{vm_body=vm_body m++[stateDecl,protected x]}
main=do
 [out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label yes=do unless yes(fail label);modifyIORef' count(+1);putStrLn("PASS "++label)
     none label a b=do
      check(label++" shell")(null(shellCandidates a b))
      check(label++" inactive")(null(inactiveCandidates a b))
     alter f=prog(f old)
 none "program directive" (VProgram[old][]["synthesis keep"])(prog current)
 none "bare full_case directive" (VProgram[old][]["full_case"])(prog current)
 none "bare parallel_case directive" (VProgram[old][]["parallel_case"])(prog current)
 none "module directive" (alter(\m->m{vm_comments=["pragma translate_off"]}))(prog current)
 none "port directive" (alter(\m->m{vm_ports=[(as,["synopsys keep"])|(as,_)<-vm_ports m]}))(prog current)
 none "wrapper directive" (alter(\m->m{vm_body=VMComment["synthesis keep"](VMGroup False[vm_body m]):[]}))(prog current)
 none "register-group directive" (alter(\m->m{vm_body=VMRegGroup(v "r")"Reg"["attribute keep"](VMGroup False[vm_body m]):[]}))(prog current)
 none "CURRENT program directive" (prog old)(VProgram[current][]["verilator lint_off"])
 let protectedCandidates=shellCandidates(prog(withOpaque old 1))(prog(withOpaque current 1))
 check "equal protected subtree may remain opaque"(not(null protectedCandidates)&&all(\c->protected 1`elem`G.listify(const True)(acProgram c))protectedCandidates)
 check "changed protected body refused"(null(shellCandidates(prog(withOpaque old 1))(prog(withOpaque current 0))))
 let flagged=(withOpaque current 1){vm_body=vm_body current++[stateDecl,VMGroup False[[VMStmt False(Vinitial(VAssign(VLId(v "opaque_state"))(lit 1)))]]]}
 check "translate-off flag changes refused"(null(shellCandidates(prog(withOpaque old 1))(prog flagged)))
 let hiddenOld=old{vm_body=[decl["before","unused","after"],assign "before"(e "a"),VMGroup True[[assign "unused"(lit 1)]],assign "after"(lit 1),assign "o"(e "before")]}
 check "translated-off helper never restored as active"(null(inactiveCandidates(prog hiddenOld)(prog current)))
 let hiddenDecl=current{vm_body=VMGroup True[[decl["before","after"]]]:tail(vm_body current)}
 check "translated-off declaration region never expanded"(null(inactiveCandidates(prog old)(prog hiddenDecl)))
 let instanceOld=old{vm_body=VMInst(v "Cell")(v "child")[][]:vm_body old}
     instanceNew=current{vm_body=VMInst(v "Cell")(v "child")[][]:vm_body current}
 check "opaque instance outside shell rule scope"(null(shellCandidates(prog instanceOld)(prog instanceNew)))
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS directedchecks="++show n++"\n")
 putStrLn("PASS directedchecks="++show n)
