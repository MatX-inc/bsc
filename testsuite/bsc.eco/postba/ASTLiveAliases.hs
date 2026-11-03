module Main where
import Control.Monad (unless,forM_)
import Data.Either (isRight)
import Data.List (sort)
import ECOASTLiveAliases
import ECOASTSearch
import ECOASTProof
import ECOASTTransform (ASTCandidate(..))
import ECOASTPreservation (transparentFlat)
import PPrint (pp80)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Verilog

v=mkVId
e=VEVar . v
range w=Just (VEConst (w-1),VEConst 0)
decl t w n=VMDecl (VVDecl t (range w) [VVar (v n)])
a n rhs=VMAssign (VLId (v n)) rhs
field=VESelect (e "view") (VEConst 7) (VEConst 0)
valid=VESelect1 (e "view") (VEConst 8)
seqp n rhs=VMStmt False (Valways (VAt (VEEposedge (e "clk")) (VAssignA (VLId (v n)) rhs)))
base extra dataAlias validAlias dataOutput validOutput=VProgram [VModule (v "m") []
 [([VAInput (v "clk") Nothing,VAInput (v "x") (range 8),VAInput (v "valid") Nothing,
    VAOutput (v "dataOut") (range 8),VAOutput (v "validOut") Nothing],[])]
 ([decl VDReg 8 "d0",decl VDReg 1 "v0"]++[x|x@(VMDecl _)<-extra]++[seqp "d0" (e "x"),seqp "v0" (e "valid"),
   decl VDWire 8 "dataAlias",decl VDWire 1 "validAlias",decl VDWire 9 "view",
   a "dataAlias" dataAlias,a "validAlias" validAlias,a "view" (VEConcat [e "validAlias",e "dataAlias"]),
   a "dataOut" dataOutput,a "validOut" validOutput]++[x|x<-extra,case x of VMDecl _->False;_->True])] [] []
newState dataInput validInput=
 [decl VDReg 8 "d1",decl VDReg 8 "d2",decl VDReg 1 "v1",decl VDReg 1 "v2",
  decl VDWire 8 "d2DIN",decl VDWire 1 "v2DIN",a "d2DIN" dataInput,a "v2DIN" validInput,
  seqp "d1" (e "d0"),seqp "d2" (e "d2DIN"),seqp "v1" (e "v0"),seqp "v2" (e "v2DIN")]
old=base [] (e "d0") (e "v0") field valid
current=base (newState field valid) (e "d1") (e "v1") (e "d2") (e "v2")
expected=base (newState (e "d1") (e "v1")) (e "d2") (e "v2") field valid
wrong=base (newState field valid) (e "d2") (e "v2") field valid
change f (VProgram [m] ds cs)=VProgram [m {vm_body=map f (vm_body m)}] ds cs
replace n rhs=change (\x->case x of VMAssign (VLId i) _|getVIdString i==n->a n rhs;_->x)
strict x y=isRight (checkAST StrictFourState [] x y)
check label ok=unless ok (error label)
main=do
 [out,self,providerSource,ys]<-getArgs
 let generated=liveAliasCandidates 32 old current
     proposals=map acProgram generated
     noCandidates before after=null (liveAliasCandidates 32 before after)
     noncontinuous (VProgram [m] ds cs)=(vm_name m,vm_ports m,ds,cs,[x|x<-transparentFlat (vm_body m),case x of VMAssign{}->False;_->True])
     owners (VProgram [m] _ _)=sort [getVIdString i|VMAssign (VLId i) _<-transparentFlat (vm_body m)]
     changedPorts=case current of VProgram [m] ds cs->VProgram [m {vm_ports=[]}] ds cs
     tests=
       [("all and individual proposals",length generated==3)
       ,("combined generic proposal matches expected",head proposals==expected)
       ,("all generated proposals independently strict",all (strict current) proposals)
       ,("no declaration state control initialization or port changes",all ((==noncontinuous current).noncontinuous) proposals)
       ,("no helpers added or deleted",all ((==owners current).owners) proposals)
       ,("fixed port guard",noCandidates old changedPorts)
       ,("wrong-width current output has no data proposal",length (liveAliasCandidates 32 old (replace "dataOut" (e "v2") current))==1)
       ,("changed packing has no proposals",noCandidates old (replace "view" (VEConcat [e "dataAlias",e "validAlias"]) current))
       ,("old aliases still unchanged do not move",noCandidates old (base (newState field valid) (e "d0") (e "v0") (e "d2") (e "v2")))
       ,("no rerouting opportunity declines",noCandidates old (base (newState (e "d1") (e "v1")) (e "d1") (e "v1") (e "d2") (e "v2")))
       ,("already restored is fixed point",noCandidates old expected)
       ,("directive comments block proposals",noCandidates old (case current of VProgram [m] ds cs->VProgram [m {vm_comments=["synthesis keep"]}] ds cs))
       ,("zero proposal budget",null (liveAliasCandidates 0 old current))
       ,("broken consumer reconnection is not strict",not (strict current wrong))]
 forM_ tests (uncurry check)
 let provider=liveAliasProvider [self,providerSource]
     opts=defaultSearchOptions {soDomain=StrictFourState,soFeatures=["LiveAliases"],soYosys=ys}
 check "disabled feature does not propose" (null (cpGenerate provider [] 32 old current))
 result<-optimizeAST (defaultSearchRequest opts (out</>"search")
   (SearchInput old pp80 []) (SearchInput current pp80 [])) {searchProvider=provider}
 check "search selects combined candidate" (srProgram result==expected)
 check "four changed lines removed" (changedLines (srOrdinaryCost result)-changedLines (srCost result)==4)
 replay<-replayASTPlan [] opts pp80 (out</>"search/search.plan") (out</>"replay") current
 check "saved plan reproves without provider" (replay==expected)
 negative<-runASTProof ys 30 (out</>"wrong-reroute") Binary [] current wrong
 check "actual solver rejects wrong reconnection" (proofStatus negative=="rejected")
 putStrLn ("PASS: "++show (length tests+5)++" live-alias candidate, independent proof, cost and replay checks")
