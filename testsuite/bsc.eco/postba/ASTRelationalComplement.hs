-- Four-state identities over typed comparison nodes. No binary assumptions,
-- candidate-supplied certificates, private design sources, or RTL parsing.
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
c w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
op a k b=VEOp (v "") a k b
neg=VEUnOp (v "") VNot
decl w n=VMDecl (VVDecl VDWire (range w) [VVar (v n)])
assign n x=VMAssign (VLId (v n)) x
slice x hi lo=VESelect x (VEConst hi) (VEConst lo)
program aw bw ow body=VProgram [VModule (v "m") []
  [([VAInput (v "a") (range aw),VAInput (v "b") (range bw),
     VAOutput (v "o") (range ow)],[])] body] [] []
pair aw bw ow a b=(program aw bw ow [assign "o" a],program aw bw ow [assign "o" b])
strict a b=isRight (checkAST StrictFourState [] a b)
check label ok=unless ok (error label)
relations=[(VULT,VUGE),(VULE,VUGT),(VUGT,VULE),(VUGE,VULT),(VEQ,VNE),(VNE,VEQ)]

directed=do
 let controls=concat
       [let x=op (e "a") relation (e "b")
            y=op (e "a") complement (e "b")
            prove aw bw ow=uncurry strict (pair aw bw ow (neg x) y)
            aliased=program 16 16 1
              [decl 16 "left",assign "left" (e "a"),decl 1 "predicate",
               assign "predicate" (op (e "left") relation (e "b")),assign "o" (neg (e "predicate"))]
            plain=program 16 16 1 [assign "o" y]
            context=op (op (e "a") VAdd (c 2 1)) relation (e "b")
            contextComplement=op (op (e "a") VAdd (c 2 1)) complement (e "b")
        in [(show relation++" direct",prove 2 2 1)
           ,(show relation++" reverse proof",uncurry (flip strict) (pair 2 2 1 (neg x) y))
           ,(show relation++" mixed operand widths",prove 2 3 1)
           ,(show relation++" wide result context",prove 2 2 8)
           ,(show relation++" exact operand/predicate aliases",strict aliased plain)
           ,(show relation++" double logical complement",uncurry strict (pair 2 2 1 (neg (neg x)) x))
           ,(show relation++" context-sized arithmetic",uncurry strict (pair 2 3 1 (neg context) contextComplement))
           ,(show relation++" literal X/Z operands",uncurry strict (pair 2 2 1
               (neg (op (VETriConst [Vx,Vz]) relation (e "b")))
               (op (VETriConst [Vx,Vz]) complement (e "b"))))]
       |(relation,complement)<-relations]
     le=op (e "a") VULE (e "b")
     gt=op (e "a") VUGT (e "b")
     test a b=uncurry strict (pair 2 2 1 a b)
     signed x=VEFctCall (v "$signed") [x]
     overflowRef=program 2 3 1 [decl 2 "narrow_sum",assign "narrow_sum" (op (e "a") VAdd (c 2 1)),
       assign "o" (neg (op (e "narrow_sum") VULE (e "b")))]
     overflowBad=program 2 3 1 [assign "o" (op (op (e "a") VAdd (c 2 1)) VUGT (e "b"))]
     extendedPredicate=program 2 2 1 [decl 2 "wide",assign "wide" le,assign "o" (neg (e "wide"))]
     bitwisePredicate=program 2 2 2 [decl 2 "wide",assign "wide" le,
       assign "o" (VEUnOp (v "") VInv (e "wide"))]
     sliced off z=slice (e z) (off+15) off
     tupleBody alias=concat
       [[decl 1 ("p"++show off),assign ("p"++show off) (op (sliced off "a") VULE (sliced off "b"))]
       |off<-[48,32,16,0],alias]++
       [assign "o" (VEConcat (concat
         [[if alias then e ("p"++show off) else op (sliced off "a") VULE (sliced off "b"),
           op (sliced off "a") VEQ (sliced off "b"),
           if alias then neg (e ("p"++show off)) else op (sliced off "a") VUGT (sliced off "b")]
         |off<-[48,32,16,0]]))]
     boundaries=
       [("four unsigned sixteen-bit tuple fields",strict (program 64 64 12 (tupleBody True)) (program 64 64 12 (tupleBody False)))
       ,("wrong complement rejected",not (test (neg le) (op (e "a") VUGE (e "b"))))
       ,("changed operand rejected",not (test (neg le) (op (e "b") VUGT (e "a"))))
       ,("changed slice rejected",not (test (neg le) (op (slice (e "a") 0 0) VUGT (e "b"))))
       ,("changed constant width rejected",not (test (neg (op (e "a") VULE (c 1 2))) (op (e "a") VUGT (c 2 2))))
       ,("truncation before comparison retained",not (strict overflowRef overflowBad))
       ,("resize of predicate not silently removed",not (strict extendedPredicate (program 2 2 1 [assign "o" gt])))
       ,("wide bitwise inverse is not logical inverse",not (strict bitwisePredicate (program 2 2 2 [assign "o" gt])))
       ,("ordinary one-bit Z leaf is not double-negation identity",not (uncurry strict (pair 1 1 1 (neg (neg (e "a"))) (e "a"))))
       ,("relational disjunction is not inequality",not (test (op (op (e "a") VULT (e "b")) VLOr gt) (op (e "a") VNE (e "b"))))
       ,("known mismatch with an X bit",test
           (neg (op (VETriConst [V0,Vx]) VEQ (VETriConst [V1,Vx])))
           (op (VETriConst [V0,Vx]) VNE (VETriConst [V1,Vx])))
       ,("known mismatch with a Z bit",test
           (neg (op (VETriConst [V0,Vz]) VEQ (VETriConst [V1,Vz])))
           (op (VETriConst [V0,Vz]) VNE (VETriConst [V1,Vz])))
       ,("ordinary equality is not case inequality",not (test (neg (op (e "a") VEQ (e "b"))) (op (e "a") VNE3 (e "b"))))
       ,("case equality not ordinary equality",not (test (neg (op (e "a") VEQ3 (e "b"))) (op (e "a") VNE (e "b"))))
       ,("signed casts remain unsupported",not (test (neg (op (signed (e "a")) VULE (signed (e "b")))) (op (signed (e "a")) VUGT (signed (e "b")))))
       ,("unsized positive remains unsupported",not (test (neg (op (e "a") VULE (VEConst 1))) (op (e "a") VUGT (VEConst 1))))
       ,("unsized negative remains unsupported",not (test (neg (op (e "a") VULE (VEConst (-1)))) (op (e "a") VUGT (VEConst (-1)))))
       ,("zero-width malformed operand not normalized",not (test (neg (op (VETriConst []) VULE (VETriConst []))) (op (VETriConst []) VUGT (VETriConst []))))]
 forM_ (controls++boundaries) (uncurry check)
 putStrLn ("PASS: "++show (length controls+length boundaries)++" strict relational-complement controls")

oracle out iverilog vvp=do
 createDirectoryIfMissing True out
 let good=concat
       [[(show relation++"-equal",2,2,1,[assign "o" (neg (op (e "a") relation (e "b")))],
                                            [assign "o" (op (e "a") complement (e "b"))],True)
        ,(show relation++"-mixed",2,3,1,[assign "o" (neg (op (e "a") relation (e "b")))],
                                            [assign "o" (op (e "a") complement (e "b"))],True)
        ,(show relation++"-wide-result",2,2,8,[assign "o" (neg (op (e "a") relation (e "b")))],
                                                  [assign "o" (op (e "a") complement (e "b"))],True)
        ,(show relation++"-arithmetic-context",2,3,1,
           [assign "o" (neg (op (op (e "a") VAdd (c 2 1)) relation (e "b")))],
           [assign "o" (op (op (e "a") VAdd (c 2 1)) complement (e "b"))],True)]
       |(relation,complement)<-relations]
     bad=
       [("wrong-complement",2,2,1,[assign "o" (neg (op (e "a") VULE (e "b")))],
           [assign "o" (op (e "a") VUGE (e "b"))],False)
       ,("overflow-context",2,3,1,[decl 2 "narrow_sum",assign "narrow_sum" (op (e "a") VAdd (c 2 1)),
           assign "o" (neg (op (e "narrow_sum") VULE (e "b")))],
           [assign "o" (op (op (e "a") VAdd (c 2 1)) VUGT (e "b"))],False)
       ,("wide-bitwise",2,2,2,[decl 2 "wide",assign "wide" (op (e "a") VULE (e "b")),
           assign "o" (VEUnOp (v "") VInv (e "wide"))],
           [assign "o" (op (e "a") VUGT (e "b"))],False)
       ,("inequality-disjunction",2,2,1,[assign "o" (op (op (e "a") VULT (e "b")) VLOr (op (e "a") VUGT (e "b")))],
           [assign "o" (op (e "a") VNE (e "b"))],False)
       ,("ordinary-vs-case-inequality",2,2,1,[assign "o" (neg (op (e "a") VEQ (e "b")))],
           [assign "o" (op (e "a") VNE3 (e "b"))],False)]
     rename n (VProgram [m] ds cs)=VProgram [m {vm_name=v n}] ds cs
 forM_ (good++bad) $ \(label,aw,bw,ow,left,right,expectEqual)->do
   let vectors=sequence (replicate (fromInteger (aw+bw)) [V0,V1,Vx,Vz])
       l=program aw bw ow left;r=program aw bw ow right
       source=pp80 (rename "reference" l)++"\n"++pp80 (rename "candidate" r)++unlines
         (["module tb;", "reg ["++show (aw+bw-1)++":0] bits; wire ["++show (ow-1)++":0] left_result, right_result;",
           "reference r(.a(bits["++show (aw-1)++":0]), .b(bits["++show (aw+bw-1)++":"++show aw++"]), .o(left_result));",
           "candidate c(.a(bits["++show (aw-1)++":0]), .b(bits["++show (aw+bw-1)++":"++show aw++"]), .o(right_result));",
           "initial begin"]++
          ["bits="++pp80 (VETriConst bits)++"; #1; if (left_result !== right_result) $fatal(1, \"mismatch\");"|bits<-vectors]++
          ["$display(\"PASS: "++show (length vectors)++" four-state vectors\"); $finish; end", "endmodule"])
       rtl=out</>(label++".v");exe=out</>(label++".sim")
   check (label++" checker agrees with oracle expectation") (strict l r==expectEqual)
   writeFile rtl source
   (compiled,cout,cerr)<-readProcessWithExitCode iverilog ["-g2012","-s","tb","-o",exe,rtl] ""
   writeFile (out</>(label++".compile.log")) (cout++cerr)
   check (label++" compiles") (compiled==ExitSuccess)
   (ran,sout,serr)<-readProcessWithExitCode vvp [exe] ""
   writeFile (out</>(label++".run.log")) (sout++serr)
   check (label++" emitted-Verilog result")
     (if expectEqual then ran==ExitSuccess else ran/=ExitSuccess && "mismatch" `isInfixOf` sout)
 let vectorCount=sum [4^(aw+bw) | (_,aw,bw,_,_,_,_)<-good] :: Integer
 putStrLn ("PASS: "++show vectorCount++" exhaustive emitted-Verilog X/Z vectors across "++show (length good)++
   " positive comparison/context cases; "++show (length bad)++" independent negative witnesses")

main=do
 args<-getArgs
 case args of
   ["directed"]->directed
   ["oracle",out,iverilog,vvp]->oracle out iverilog vvp
   _->error "ASTRelationalComplement directed | oracle OUT IVERILOG VVP"
