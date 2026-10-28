module Main where
import ECOASTInactive
import Verilog
import ECOASTProof
import ECOASTTransform
import Control.Monad
import Data.IORef
import qualified Data.Generics as G
import System.Environment(getArgs)
import System.Directory
import System.FilePath
v=mkVId
e=VEVar . v
lit=VEWConst defaultVId 1 2 1
decl names=VMDecl(VVDecl VDWire Nothing[VVar(v n)|n<-names])
assign n=VMAssign(VLId(v n))lit
modul body=VModule(v "M")[] [([VAInput(v "x")Nothing,VAOutput(v "o")Nothing],[])]body
prog m=VProgram[m][][]
old=modul[decl["before","unused","after"],assign "before",VMComment["old helper"](assign "unused"),assign "after",VMAssign(VLId(v "o"))(e "before")]
current=modul[decl["before","after"],assign "before",assign "after",VMAssign(VLId(v "o"))(e "before")]
main=do
 [ys,out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label condition=do unless condition(fail label);modifyIORef' count(+1);putStrLn("PASS "++label)
     cs=inactiveCandidates(prog old)(prog current)
 check "one historical inactive helper proposal"(length cs==1)
 let candidate=head cs
 check "declaration grouping restored"(decl["before","unused","after"]`elem`G.listify(const True)(acProgram candidate))
 check "original assignment and comment placement restored"(acProgram candidate==prog old)
 result<-runASTProof ys 20(out</>"proof")StrictFourState[](prog current)(acProgram candidate)
 writeFile(out</>"proof.json")(renderProofResult result)
 check "independent strict proof"(proofPassed result)
 check "provider disabled by default"(null(inactiveCandidates(prog current)(prog current)))
 let collide=current{vm_body=VMAssign(VLId(v "o"))(e "unused"):vm_body current}
 check "reference collision is not inactive"(null(inactiveCandidates(prog old)(prog collide)))
 let effects=old{vm_body=decl["before","unused","after"]:assign "before":VMAssign(VLId(v "unused"))(VEFctCall(v "$random")[]):drop 3(vm_body old)}
 check "effectful missing helper refused"(null(inactiveCandidates(prog effects)(prog current)))
 let signalOld=old{vm_body=decl["before","unused","after"]:assign "before":VMAssign(VLId(v "unused"))(e "x"):drop 3(vm_body old)}
 check "nonconstant helper outside initial scope refused"(null(inactiveCandidates(prog signalOld)(prog current)))
 let annotated=old{vm_body=decl["before","unused","after"]:assign "before":VMComment["synthesis keep"](assign "unused"):drop 3(vm_body old)}
 -- Directive wrapper must not be imported. The pure assignment may still be
 -- proposed independently; its proof and normal CURRENT directives apply.
 check "tool directive not imported"(all(\c->not("synthesis keep"`elem`[s|VMComment ss _<-G.listify(const True)(acProgram c),s<-ss]))(inactiveCandidates(prog annotated)(prog current)))
 check "changed ports refused"(null(inactiveCandidates(prog old)(prog current{vm_ports=[]})))
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS directedchecks="++show n++"\n")
 putStrLn("PASS directedchecks="++show n)
