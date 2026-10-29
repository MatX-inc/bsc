-- Logical AND/OR order is a four-state identity. Keep coercion, typed widths,
-- and duplicate operand multiplicity; do not infer two-state Boolean laws.
module Main where
import Control.Monad (forM_,unless)
import Data.Either (isRight)
import Data.List (isInfixOf)
import ECOASTProof
import PPrint (pp80)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (readProcessWithExitCode)
import Verilog

v=mkVId
e=VEVar . v
lit w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
op k a b=VEOp (v "") a k b
decl w n=VMDecl (VVDecl VDWire (range w) [VVar (v n)])
assign n x=VMAssign (VLId (v n)) x
program aw bw cw ow body=VProgram [VModule (v "m") []
  [([VAInput (v "a") (range aw),VAInput (v "b") (range bw),
     VAInput (v "c") (range cw),VAOutput (v "o") (range ow)],[])] body] [] []
pair aw bw cw ow a b=(program aw bw cw ow [assign "o" a],program aw bw cw ow [assign "o" b])
strict a b=isRight (checkAST StrictFourState [] a b)
check label ok=unless ok (error label)

directed=do
 let a=e "a";b=e "b";c=e "c"
     positive=concat
       [let left=op k (op k a b) c;right=op k c (op k b a)
            prove aw bw cw ow=uncurry strict (pair aw bw cw ow left right)
            deep reverseOperands=program 2 2 2 1
              (concat [[decl 1 ("h"++show n),assign ("h"++show n)
                (if n==0 then op k (if reverseOperands then b else a) (if reverseOperands then a else b)
                 else op k (e("h"++show(n-1))) (e("h"++show(n-1))))]|n<-[0::Int ..40]]++
                [assign "o" (op k (e "h40") c)])
        in [(show k++" associative",uncurry strict (pair 1 1 1 1 left (op k a (op k b c))))
           ,(show k++" order",prove 1 1 1 1)
           ,(show k++" wide truth operands",prove 2 2 2 1)
           ,(show k++" mixed truth widths",prove 1 2 3 1)
           ,(show k++" extended logical result",prove 2 2 2 8)
           ,(show k++" duplicate occurrences reordered",uncurry strict (pair 1 1 1 1
               (op k (op k a b) a) (op k a (op k a b))))
           ,(show k++" duplicate occurrences not discarded",not (uncurry strict (pair 1 1 1 1
               (op k (op k a b) a) (op k a b))))
           ,(show k++" raw Z still operator-coerced",uncurry strict (pair 1 1 1 1
               (op k (op k (VETriConst [Vz]) a) b) (op k b (op k a (VETriConst [Vz])))))
           ,(show k++" shared forty-level duplicate DAG",strict (deep False) (deep True))]
       |k<-[VLAnd,VLOr]]
     test a b=uncurry strict (pair 2 2 2 1 a b)
     widened=program 2 2 2 1 [decl 2 "predicate",assign "predicate" (op VLAnd a b),
       assign "o" (op VLAnd (e "predicate") c)]
     negatives=
       [("operator kind retained",not(test (op VLAnd (op VLAnd a b) c) (op VLOr c (op VLOr b a))))
       ,("mixed operators not flattened",not(test (op VLAnd (op VLOr a b) c) (op VLOr a (op VLAnd b c))))
       ,("bitwise AND not logical AND",not(test (op VAnd a b) (op VLAnd a b)))
       ,("bitwise OR not logical OR",not(test (op VOr a b) (op VLOr a b)))
       ,("no distributivity rule",not(test (op VLAnd a (op VLOr b c)) (op VLOr (op VLAnd a b) (op VLAnd a c))))
       ,("resize is a barrier",not(strict widened (program 2 2 2 1 [assign "o" (op VLAnd (op VLAnd a b) c)])))
       ,("logical zero cannot erase raw Z",not(uncurry strict (pair 1 1 1 1 (op VLOr (lit 1 0) a) a)))
       ,("logical one cannot erase raw Z",not(uncurry strict (pair 1 1 1 1 (op VLAnd (lit 1 1) a) a)))
       ,("duplicated raw Z cannot become raw leaf",not(uncurry strict (pair 1 1 1 1 (op VLAnd a a) a)))
       ,("changed leaf retained",not(test (op VLAnd (op VLAnd a b) c) (op VLAnd a (op VLAnd b b))))]
 forM_ (positive++negatives) (uncurry check)
 putStrLn("PASS: "++show(length positive+length negatives)++" strict logical AC controls, including compact shared DAGs")

oracle out iverilog vvp=do
 createDirectoryIfMissing True out
 let a=e "a";b=e "b";c=e "c"
     positive=concat
       [[(show k++"-one-bit",1,1,1,1,op k (op k a b) c,op k c (op k b a),True)
        ,(show k++"-wide-truth",2,2,2,1,op k (op k a b) c,op k c (op k b a),True)
        ,(show k++"-mixed-width",1,2,3,1,op k (op k a b) c,op k c (op k b a),True)
        ,(show k++"-wide-result",1,1,1,8,op k (op k a b) c,op k c (op k b a),True)
        ,(show k++"-duplicates",1,1,1,1,op k (op k a b) a,op k a (op k a b),True)
        ,(show k++"-literal-Z",1,1,1,1,op k (op k (VETriConst [Vz]) a) b,
             op k b (op k a (VETriConst [Vz])),True)]|k<-[VLAnd,VLOr]]
     negative=
       [("wrong-kind",1,1,1,1,op VLAnd (op VLAnd a b) c,op VLOr c (op VLOr b a),False)
       ,("mixed-kind",1,1,1,1,op VLAnd (op VLOr a b) c,op VLOr a (op VLAnd b c),False)
       ,("bitwise-and",2,2,1,1,op VAnd a b,op VLAnd a b,False)
       ,("bitwise-or",2,2,1,1,op VOr a b,op VLOr a b,False)
       ,("zero-or-raw-Z",1,1,1,1,op VLOr (lit 1 0) a,a,False)
       ,("one-and-raw-Z",1,1,1,1,op VLAnd (lit 1 1) a,a,False)
       ,("duplicate-raw-Z",1,1,1,1,op VLAnd a a,a,False)]
     rename n (VProgram [m] ds cs)=VProgram [m{vm_name=v n}] ds cs
 forM_ (positive++negative)$ \(label,aw,bw,cw,ow,left,right,expectEqual)->do
   let vectors=sequence(replicate(fromInteger(aw+bw+cw))[V0,V1,Vx,Vz])
       (l,r)=pair aw bw cw ow left right
       connections=".a(bits["++show(aw-1)++":0]),.b(bits["++show(aw+bw-1)++":"++show aw++"]),.c(bits["++show(aw+bw+cw-1)++":"++show(aw+bw)++"])"
       source=pp80(rename "reference" l)++"\n"++pp80(rename "candidate" r)++unlines
         (["module tb;","reg ["++show(aw+bw+cw-1)++":0] bits;wire ["++show(ow-1)++":0] left_result,right_result;",
           "reference r("++connections++",.o(left_result));","candidate c("++connections++",.o(right_result));",
           "initial begin"]++
          ["bits="++pp80(VETriConst bits)++";#1;if(left_result !== right_result)$fatal(1,\"mismatch\");"|bits<-vectors]++
          ["$display(\"PASS: "++show(length vectors)++" four-state vectors\");$finish;end","endmodule"])
       rtl=out</>(label++".v");exe=out</>(label++".sim")
   check(label++" checker result")(strict l r==expectEqual)
   writeFile rtl source
   (compiled,cout,cerr)<-readProcessWithExitCode iverilog ["-g2012","-s","tb","-o",exe,rtl] ""
   writeFile(out</>(label++".compile.log"))(cout++cerr)
   check(label++" compiles")(compiled==ExitSuccess)
   (ran,sout,serr)<-readProcessWithExitCode vvp [exe] ""
   writeFile(out</>(label++".run.log"))(sout++serr)
   check(label++" emitted-Verilog result")(if expectEqual then ran==ExitSuccess else ran/=ExitSuccess && "mismatch" `isInfixOf` sout)
 let vectors=sum[4^(aw+bw+cw)|(_,aw,bw,cw,_,_,_,_)<-positive]::Integer
 putStrLn("PASS: "++show vectors++" exhaustive emitted-Verilog X/Z vectors across "++show(length positive)++
   " positive logical AC cases; "++show(length negative)++" independent negative witnesses")

main=do
 args<-getArgs
 case args of
   ["directed"]->directed
   ["oracle",out,iverilog,vvp]->oracle out iverilog vvp
   _->error "ASTLogicalAC directed | oracle OUT IVERILOG VVP"
