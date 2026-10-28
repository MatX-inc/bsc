module Main where
import ECOASTShell
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
op o a b=VEOp defaultVId a o b
decl ty s=VMDecl(VVDecl ty Nothing[VVar(v s)])
assign s=VMAssign(VLId(v s))
modul body=VModule(v "M")[] [([VAInput(v "a")Nothing,VAInput(v "b")Nothing,VAInput(v "enable")Nothing,VAOutput(v "o")Nothing],[])]body
prog m=VProgram[m][][]
old=modul[decl VDWire "prior",decl VDWire "unused",assign "prior"(op VLAnd(e "a")(e "b")),assign "unused"(e "a"),assign "o"(op VLAnd(e "prior")(e "enable"))]
current=modul[decl VDWire "current",assign "current"(op VLOr(e "a")(e "b")),assign "o"(op VLAnd(e "enable")(e "current"))]
main=do
 [ys,out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label condition=do unless condition(fail label);modifyIORef' count(+1);putStrLn("PASS "++label)
     cs=shellCandidates(prog old)(prog current)
     proof label domain reference candidate relation expected=do
      result<-runASTProof ys 20(out</>label)domain relation reference candidate
      writeFile(out</>label<.>"json")(renderProofResult result++"\n")
      check label(proofPassed result==expected)
 check "exact and historical-Boolean candidates emitted"(length cs==2)
 let strict=head cs;binary=cs!!1
 check "optional historical form is explicitly Binary only"(not(acBinaryOnly strict)&&acBinaryOnly binary)
 check "historical helper now computes current OR expression"(assign "prior"(op VLOr(e "a")(e "b"))`elem`G.listify(const True)(acProgram binary))
 check "historical pure inactive helper is retained"(assign "unused"(e "a")`elem`G.listify(const True)(acProgram binary))
 check "historical Boolean consumer order retained"(assign "o"(op VLAnd(e "prior")(e "enable"))`elem`G.listify(const True)(acProgram binary))
 proof "strict-exact-current" StrictFourState(prog current)(acProgram strict)(acStateMap strict)True
 proof "binary-historical-consumer" Binary(prog current)(acProgram binary)(acStateMap binary)True
 proof "old-function-negative" Binary(prog current)(prog old)[]False
 check "port change refused"(null(shellCandidates(prog old)(prog current{vm_ports=vm_ports current++[([VAInput(v "other")Nothing],[])]})))
 check "new state refused"(null(shellCandidates(prog old)(prog current{vm_body=decl VDReg "new_state":vm_body current})))
 let withEffect=old{vm_body=[assign "unused"(VEFctCall(v "$random")[])]++drop 4(vm_body old)}
 check "side-effectful historical expression refused"(null(shellCandidates(prog withEffect)(prog current)))
 let changed= current{vm_body=VMStmt False(Vinitial(VAssign(VLId(v "other"))(VEConst 0))):vm_body current}
 check "changed non-combinational scaffold refused"(null(shellCandidates(prog old)(prog changed)))
 let ambiguousOld=modul[decl VDWire "p",decl VDWire "q",assign "p"(e "a"),assign "q"(e "b"),assign "o"(op VLAnd(e "p")(e "q"))]
     ambiguousNew=modul[decl VDWire "x",decl VDWire "y",assign "x"(op VLOr(e "a")(e "b")),assign "y"(op VNE(e "a")(e "b")),assign "o"(op VLAnd(e "x")(e "y"))]
     aps=shellCandidates(prog ambiguousOld)(prog ambiguousNew)
 check "ambiguous two-hole roles do not overwrite old predicates"(all(\c->assign "p"(e "a")`elem`G.listify(const True)(acProgram c)&&assign "q"(e "b")`elem`G.listify(const True)(acProgram c))aps)
 forM_(zip[0::Int ..]aps) $ \(i,c)->proof("ambiguous-safe-cone-"++show i)Binary(prog ambiguousNew)(acProgram c)(acStateMap c)True
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS directedchecks="++show n++"; no environmental assumptions\n")
 putStrLn("PASS directedchecks="++show n)
