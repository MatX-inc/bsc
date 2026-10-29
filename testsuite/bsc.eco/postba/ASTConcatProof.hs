-- Exact typed concat-field projection is an independent checker identity,
-- not a candidate-supplied certificate or a don't-care assumption.
module Main where
import Control.Monad (unless,forM_)
import Data.Either (isRight)
import ECOASTProof
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import PPrint (pp80)
import System.FilePath ((</>))
import Verilog

v=mkVId
e=VEVar . v
range w=Just (VEConst (w-1),VEConst 0)
decl ty w n=VMDecl (VVDecl ty (range w) [VVar (v n)])
assign n x=VMAssign (VLId (v n)) x
slice x hi lo=VESelect x (VEConst hi) (VEConst lo)
base w body=VProgram [VModule (v "m") []
  [([VAInput (v "a") (range 3),VAInput (v "b") Nothing,
     VAInput (v "clk") Nothing,VAOutput (v "o") (range w)],[])] body] [] []
packed width extra rhs=base 3 ([decl VDWire width "pack",
  assign "pack" (VEConcat [e "b",e "a"])]++extra++[assign "o" rhs])
reference=packed 4 [] (slice (e "pack") 2 0)
expected=packed 4 [] (e "a")
check label ok=unless ok (error label)
strict a b=isRight (checkAST StrictFourState [] a b)
seqp rhs=VMStmt False (Valways (VAt (VEEposedge (e "clk"))
  (VAssignA (VLId (v "q")) rhs)))
main=do
 [yosys,out,iverilog,vvp]<-getArgs
 createDirectoryIfMissing True out
 let upper rhs=base 1 [decl VDWire 4 "pack",assign "pack" (VEConcat [e "b",e "a"]),assign "o" rhs]
     state rhs=packed 4 [decl VDReg 3 "q",seqp rhs] (e "q")
     tests=
       [("whole lower field",strict reference expected)
       ,("whole upper field",strict (upper (VESelect1 (e "pack") (VEConst 3))) (upper (e "b")))
       ,("unchanged sequential RHS gains complete cone proof",strict (state (slice (e "pack") 2 0)) (state (e "a")))
       ,("wrong field rejected",not (strict reference (packed 4 [] (e "b"))))
       ,("partial field not complete",not (strict (packed 4 [] (slice (e "pack") 2 1)) expected))
       ,("cross-field slice rejected",not (strict (packed 4 [] (slice (e "pack") 3 1)) expected))
       ,("out of bounds rejected",not (strict (packed 4 [] (slice (e "pack") 4 2)) expected))
       ,("truncated concat not normalized",not (strict (packed 3 [] (slice (e "pack") 2 0)) (packed 3 [] (e "a"))))
       ,("extended concat not normalized",not (strict (packed 5 [] (slice (e "pack") 2 0)) (packed 5 [] (e "a"))))
       ,("duplicate driver rejected",not (strict (packed 4 [assign "pack" (e "a")] (slice (e "pack") 2 0)) expected))
       ,("cyclic driver rejected",not (strict (base 3 [decl VDWire 4 "pack",assign "pack" (VEConcat [e "b",slice (e "pack") 2 0]),assign "o" (slice (e "pack") 2 0)]) expected))
       ,("unknown bits are not don't cares",not (strict (base 3 [assign "o" (VEConcat [VETriConst [Vx],VETriConst [Vz,V0]])]) (base 3 [assign "o" (VETriConst [V0,Vz,V0])])))]
 forM_ tests (uncurry check)
 let vectors=sequence (replicate 4 [V0,V1,Vx,Vz])
 forM_ vectors $ \bits->do
   let high=VETriConst (take 1 bits);low=VETriConst (drop 1 bits)
       literal rhs=base 3 [decl VDWire 4 "pack",assign "pack" (VEConcat [high,low]),assign "o" rhs]
   check ("literal 0/1/X/Z projection "++show bits) (strict (literal (slice (e "pack") 2 0)) (literal low))
 actual<-runASTProof yosys 30 (out</>"actual") Binary [] reference expected
 check "binary-domain structural proof" (proofPassed actual)
 wrong<-runASTProof yosys 30 (out</>"wrong") Binary [] reference (packed 4 [] (slice (e "pack") 3 1))
 check "actual cross-field mismatch" (proofStatus wrong=="rejected")
 -- Execute the independently interpreted emitted Verilog, rather than
 -- treating checker normalization or literal AST equality as an X/Z oracle.
 let rename n (VProgram [m] ds cs)=VProgram [m {vm_name=v n}] ds cs
     value bits=pp80 (VETriConst bits)
     source candidate=pp80 (rename "reference" reference)++"\n"++pp80 (rename "candidate" candidate)++unlines
       (["module tb;", "reg [3:0] bits; wire [2:0] left_result, right_result;",
         "reference r(.a(bits[2:0]), .b(bits[3]), .clk(1'b0), .o(left_result));",
         "candidate c(.a(bits[2:0]), .b(bits[3]), .clk(1'b0), .o(right_result));",
         "initial begin"]++
        ["bits = "++value bits++"; #1; if (left_result !== right_result) $fatal(1, \"mismatch\");"|bits<-vectors]++
        ["$display(\"PASS: 256 emitted-Verilog four-state vectors\"); $finish; end", "endmodule"])
     simulate label candidate=do
       let rtl=out</>(label++".v");exe=out</>(label++".sim")
       writeFile rtl (source candidate)
       (compiled,cout,cerr)<-readProcessWithExitCode iverilog ["-g2012","-s","tb","-o",exe,rtl] ""
       writeFile (out</>(label++".compile.log")) (cout++cerr)
       check (label++" compiles") (compiled==ExitSuccess)
       (ran,sout,serr)<-readProcessWithExitCode vvp [exe] ""
       writeFile (out</>(label++".run.log")) (sout++serr)
       pure ran
 positive<-simulate "four-state-positive" expected
 check "independent emitted-Verilog 0/1/X/Z projection" (positive==ExitSuccess)
 negative<-simulate "four-state-negative" (packed 4 [] (slice (e "pack") 3 1))
 check "independent emitted-Verilog wrong-field rejection" (negative/=ExitSuccess)
 putStrLn ("PASS: "++show (length tests+4)++" concat-field proof controls; "++show (length vectors)++" four-state literal vectors")
