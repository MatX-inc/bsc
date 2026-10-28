-- A missing-default process is a history boundary, never a don't-care mux.
-- Exact duplicates may share that boundary from common Verilog initialization.
module Main where
import Control.Monad (unless,forM_)
import Data.Either (isRight)
import Data.List (isInfixOf)
import ECOASTProof
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.Process (readProcessWithExitCode)
import System.FilePath ((</>))
import PPrint (pp80)
import Verilog

v=mkVId
e=VEVar . v
c w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
decl w n=VMDecl (VVDecl VDReg (range w) [VVar (v n)])
wire w n=VMDecl (VVDecl VDWire (range w) [VVar (v n)])
assign n x=VMAssign (VLId (v n)) x
as n x=VAssign (VLId (v n)) x
base body=VProgram [VModule (v "m") [] [([VAInput (v "a") (range 8),
  VAInput (v "b") (range 8),VAInput (v "sel") (range 2),
  VAInput (v "clk") Nothing,VAOutput (v "o") (range 16)],[])] body] [] []
event=VEEOr (VEE (e "sel")) (VEEOr (VEE (e "a")) (VEE (e "b")))
body n=Vcase (e "sel") [VCaseArm [c 2 k] (as n x) |
  (k,x)<-[(0,e "a"),(1,e "b"),(2,c 8 85),(3,c 8 170)]] False False
process ev s=VMStmt False (Valways (VAt ev s))
p n=process event (body n)
reference=base [decl 8 "q",p "q",assign "o" (VEConcat [e "q",e "q"])]
cloneWith reg proc extra=base ([decl 8 "q",p "q",reg,proc]++extra++
  [assign "o" (VEConcat [e "q",e "r"])])
clone=cloneWith (decl 8 "r") (p "r") []
check label ok=unless ok (error label)
strict a b=isRight (checkAST StrictFourState [] a b)
changeBody f (VProgram [m] ds cs)=VProgram [m {vm_body=f (vm_body m)}] ds cs
renameTop n (VProgram [m] ds cs)=VProgram [m {vm_name=v n}] ds cs
initial n val=VMStmt False (Vinitial (as n val))
main=do
 [out,iverilog,vvp]<-getArgs
 createDirectoryIfMissing True out
 let rejects=not . strict reference
     changed rhs=process event (Vcase (e "sel")
       [VCaseArm [c 2 0] (as "r" rhs),VCaseArm [c 2 1] (as "r" (e "b"))] False False)
     extraDefault=process event (Vcase (e "sel")
       ([VCaseArm [c 2 k] (as "r" x) |(k,x)<-[(0,e "a"),(1,e "b"),(2,c 8 85),(3,c 8 170)]]++
        [VDefault (as "r" (c 8 0))]) False False)
     aliasNames=base [decl 8 "q",p "q",decl 8 "r",wire 2 "alias_sel",
       assign "alias_sel" (e "sel"),process
       (VEEOr (VEE (e "alias_sel")) (VEEOr (VEE (e "a")) (VEE (e "b"))))
       (Vcase (e "alias_sel") [VCaseArm [c 2 k] (as "r" x) |
        (k,x)<-[(0,e "a"),(1,e "b"),(2,c 8 85),(3,c 8 170)]] False False),
       assign "o" (VEConcat [e "q",e "r"])]
     withCone operand duplicates=base ([wire 8 "incoming",assign "incoming" operand]++
       concat [[decl 8 n,process (VEEOr (VEE (e "sel")) (VEE (e "incoming")))
          (Vif (e "sel") (as n (e "incoming")))] |n<-if duplicates then ["q","r"] else ["q"]]++
       [assign "o" (VEConcat [e "q",e (if duplicates then "r" else "q")])])
     seqWriter=VMStmt False (Valways (VAt (VEEposedge (e "clk")) (VAssignA (VLId (v "r")) (e "a"))))
     indirect=cloneWith (decl 8 "r") (changed (e "feedback")) [wire 8 "feedback",assign "feedback" (e "r")]
     throughRetained duplicates=base ([decl 8 "retained",process event (Vif (e "sel") (as "retained" (e "a")))]++
       concat [[decl 8 n,process (VEEOr (VEE (e "sel")) (VEE (e "retained")))
         (Vif (e "sel") (as n (e "retained")))] | n<-if duplicates then ["q","r"] else ["q"]]++
       [assign "o" (VEConcat [e "q",e (if duplicates then "r" else "q")])])
     initialClone=cloneWith (decl 8 "r") (p "r") [initial "r" (c 8 0)]
     partial=process event (VAssign (VLSub (VLId (v "r")) (c 1 0)) (c 1 0))
     portReg=case clone of VProgram [m] ds cs->VProgram [m {vm_ports=vm_ports m++[([VAOutput (v "r") (range 8)],[])]}] ds cs
     portRef=case reference of VProgram [m] ds cs->VProgram [m {vm_ports=vm_ports m++[([VAOutput (v "r") (range 8)],[])],vm_body=vm_body m++[assign "r" (e "q")]}] ds cs
     withCall=changeBody (++[decl 8 "dummy",initial "dummy" (VEFctCall (v "foreign_call") [])])
     withNestedCall=changeBody (++[decl 8 "dummy",VMGroup True [[initial "dummy" (VEFctCall (v "foreign_call") [])]]])
     withDPI (VProgram ms _ cs)=VProgram ms [VDPI (v "foreign_call") VDT_void []] cs
     withOrdinaryState=changeBody (++[decl 8 "state",initial "state" (c 8 0),
       process (VEEposedge (e "clk")) (Vifelse (e "sel")
         (VAssignA (VLId (v "state")) (e "a")) (VAssignA (VLId (v "state")) (e "b")))])
     tests=
       [("exact incomplete clone",strict reference clone)
       ,("exact incomplete clone reverse",strict clone reference)
       ,("reordered opaque duplicates",strict reference (changeBody reverse clone))
       ,("exact input aliases retain common history",strict reference aliasNames)
       ,("shared upstream cone is proved",strict (withCone (e "a") False) (withCone (e "a") True))
       ,("changed upstream cone cannot become a free cut",not (strict (withCone (e "a") False) (withCone (e "b") True)))
       ,("changed value rejected",rejects (cloneWith (decl 8 "r") (changed (e "b")) []))
       ,("changed selector rejected",rejects (cloneWith (decl 8 "r") (process event (Vif (e "a") (as "r" (e "b")))) []))
       ,("default changes history and is rejected",rejects (cloneWith (decl 8 "r") extraDefault []))
       ,("width mismatch rejected",rejects (cloneWith (decl 16 "r") (p "r") []))
       ,("missing sensitivity rejected",rejects (cloneWith (decl 8 "r") (process (VEE (e "sel")) (body "r")) []))
       ,("different event order rejected conservatively",rejects (cloneWith (decl 8 "r") (process
           (VEEOr (VEE (e "a")) (VEEOr (VEE (e "sel")) (VEE (e "b")))) (body "r")) []))
       ,("extra event rejected conservatively",rejects (cloneWith (decl 8 "r") (process (VEEOr event (VEE (e "clk"))) (body "r")) []))
       ,("edge event rejected",rejects (cloneWith (decl 8 "r") (process (VEEposedge (e "clk")) (body "r")) []))
       ,("initial conflict rejected",rejects initialClone)
       ,("translate-off initial still rejected",rejects (cloneWith (decl 8 "r") (p "r") [VMGroup True [[initial "r" (c 8 0)]]]))
       ,("extra sequential writer rejected",rejects (cloneWith (decl 8 "r") (p "r") [seqWriter]))
       ,("extra continuous writer rejected",rejects (cloneWith (decl 8 "r") (p "r") [assign "r" (e "a")]))
       ,("partial extra writer rejected",rejects (cloneWith (decl 8 "r") (p "r") [partial]))
       ,("duplicate same-destination writer rejected",rejects (cloneWith (decl 8 "r") (p "r") [p "r"]))
       ,("direct self feedback rejected",rejects (cloneWith (decl 8 "r") (changed (e "r")) []))
       ,("indirect self feedback rejected",rejects indirect)
       ,("other history dependency rejected",not (strict (throughRetained False) (throughRetained True)))
       ,("task effect rejected",rejects (cloneWith (decl 8 "r") (process event (VSeq [body "r",VTask (v "$display") [e "a"]])) []))
       ,("function call rejected",rejects (cloneWith (decl 8 "r") (changed (VEFctCall (v "$random") [])) []))
       ,("unchanged task cannot supply hidden history",not (strict
          (changeBody (++[VMStmt True (Vinitial (VTask (v "$display") [VEString "r"]))]) reference)
          (changeBody (++[VMStmt True (Vinitial (VTask (v "$display") [VEString "r"]))]) clone)))
       ,("opaque foreign call initializer cannot hide history effects",not (strict (withCall reference) (withCall clone)))
       ,("nested opaque foreign call cannot hide history effects",not (strict (withNestedCall reference) (withNestedCall clone)))
       ,("program DPI scope excludes a new history relation",not (strict (withDPI reference) (withDPI clone)))
       ,("opaque call identity without merge stays supported",strict (withCall reference) (withCall reference))
       ,("DPI identity without merge stays supported",strict (withDPI reference) (withDPI reference))
       ,("ordinary sequential controls do not prevent safe history merge",strict (withOrdinaryState reference) (withOrdinaryState clone))
       ,("unchanged zero-delay effects rejected conservatively",not (strict
          (changeBody (++[VMStmt True (Vinitial VZeroDelay)]) reference)
          (changeBody (++[VMStmt True (Vinitial VZeroDelay)]) clone)))
       ,("array destination rejected",rejects (cloneWith (VMDecl (VVDecl VDReg (range 8) [VArray (VEConst 1,VEConst 0) (v "r")])) (p "r") []))
       ,("port cannot become hidden history alias",not (strict portRef portReg))
       ,("multiple destinations rejected",rejects (cloneWith (decl 8 "r") (process event (VSeq [body "r",as "q" (e "a")])) []))]
 forM_ tests (uncurry check)
 case checkAST StrictFourState [] reference clone of
   Left x->error (show x)
   Right proof->do
     check "history precondition reported" (lookup "commonInitialHistoryAliases" (proofInventory proof)==Just 1)
     check "specific alias relation reported" ("history-alias:candidate:r=q" `elem` proofStructuralRoots proof)
     check "no solver residual or miter" (null (proofResidualRoots proof) && maybe True (const False) (proofMiter proof))
     let rendered=renderProofResult (ProofResult True "proved" [] (Just proof) Nothing [])
     check "serialized history scope is explicit" ("common-Verilog-initialization-and-history" `isInfixOf` rendered)
     case checkAST StrictFourState [] reference reference of
       Left x->error (show x)
       Right identityProof->check "identity does not claim new history induction"
         (not ("common-Verilog-initialization-and-history" `isInfixOf`
           renderProofResult (ProofResult True "proved" [] (Just identityProof) Nothing [])))
     writeFile (out</>"history-proof.txt") (show proof++"\n")
     writeFile (out</>"history-proof.json") (rendered++"\n")
 -- Independent actual Verilog checks exercise both default-X initialization
 -- and repeated known -> X/Z selector transitions, with unknown payload data.
 let source candidate=pp80 (renameTop "reference" reference)++"\n"++pp80 (renameTop "candidate" candidate)++unlines
       ["module tb; reg [7:0] a,b; reg [1:0] sel; wire [15:0] x,y; integer h,n,d,count;",
        "reference r(.a(a),.b(b),.sel(sel),.clk(1'b0),.o(x));",
        "candidate c(.a(a),.b(b),.sel(sel),.clk(1'b0),.o(y));",
        "function automatic f(input integer n); case(n) 0:f=0;1:f=1;2:f=1'bx;3:f=1'bz;endcase endfunction",
        "initial begin a=8'h5a; b=8'ha5; sel=2'bxx; count=0; #1;",
        "if(x !== y) $fatal(1,\"initial history mismatch\");",
        "for(h=0;h<4;h=h+1) for(n=0;n<16;n=n+1) for(d=0;d<16;d=d+1) begin",
        "sel=h; a=8'h5a; b=8'ha5; #1; if(x !== y) $fatal(1,\"known history mismatch\");",
        "sel={f(n/4),f(n%4)}; a={8{f(d/4)}}; b={8{f(d%4)}}; #1;",
        "if(x !== y) $fatal(1,\"four-state transition mismatch\"); count=count+1; end",
        "$display(\"PASS: %0d history/data transitions\",count); $finish; end endmodule"]
     simulate label candidate=do
       let rtl=out</>(label++".v");exe=out</>(label++".sim")
       writeFile rtl (source candidate)
       (compiled,cout,cerr)<-readProcessWithExitCode iverilog (["-g2012","-s","tb","-o",exe,rtl]++
         ["-DBSV_NO_INITIAL_BLOCKS" | label/="changed-initial"]) ""
       writeFile (out</>(label++".compile.log")) (cout++cerr)
       check (label++" compiles") (compiled==ExitSuccess)
       (ran,sout,serr)<-readProcessWithExitCode vvp [exe] ""
       writeFile (out</>(label++".run.log")) (sout++serr)
       return ran
 positive<-simulate "common-history" clone
 check "independent four-state history oracle" (positive==ExitSuccess)
 negative<-simulate "changed-default" (cloneWith (decl 8 "r") extraDefault [])
 check "history-changing default has actual witness" (negative/=ExitSuccess)
 -- Enable only this deliberately conflicting explicit initializer.  The
 -- positive and changed-default oracles disable standard initial blocks.
 initialBad<-simulate "changed-initial" initialClone
 check "independent initial states are not assumed equal" (initialBad/=ExitSuccess)
 putStrLn ("PASS: "++show (length tests+8)++" duplicate-history controls and 1024 emitted-Verilog transitions")
