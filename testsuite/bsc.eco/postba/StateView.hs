module Main where
import ECOASTStateView
import ECOASTProof
import Verilog
import Control.Monad
import Data.IORef
import System.Environment
import System.Directory
import System.FilePath
v=mkVId
e=VEVar . v
range n=if n==1 then Nothing else Just(VEConst(n-1),VEConst 0)
lit n x=VEWConst defaultVId n 10 x
decl ty n s=VMDecl(VVDecl ty(range n)[VVar(v s)])
assign n x=VMAssign(VLId(v n))x
state n cls w input=[VMRegGroup(v n)cls[](VMGroup False[[decl VDReg w n,decl VDWire w("data_"++n)]]),assign("data_"++n)input,VMStmt False(Valways(VAt(VEEposedge(e "clock"))(Vifelse(e "reset")(VAssignA(VLId(v n))(lit w 0))(VAssignA(VLId(v n))(e("data_"++n))))))]
modul body=VModule(v "Demo")[][([VAInput(v "clock")Nothing,VAInput(v "reset")Nothing,VAInput(v "a")(range 3),VAInput(v "b")(range 5),VAOutput(v "out")(range 8)],[])]body
old=modul(state "packed" "ResetReg" 8(VEConcat[e "a",e "b"])++[assign "out"(e "packed")])
new=modul(state "left" "NewReset" 3(e "a")++state "right" "NewReset" 5(e "b")++[decl VDWire 8 "view",assign "view"(VEConcat[e "left",e "right"]),assign "out"(e "view")])
prog m=VProgram[m][][]
append m xs=m{vm_body=vm_body m++xs}
replaceDef n r m=m{vm_body=map f(vm_body m)} where
 f(VMAssign(VLId i)_)|getVIdString i==n=assign n r
 f x=x
main=do
 [ys,out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label ok=do unless ok(fail label);modifyIORef' count(+1);putStrLn("PASS "++label)
     proposals=stateViewProposals 16
     none label a b=check label(null(proposals a b))
     ps=proposals old new
 check "one typed input-packing proposal"(length ps==1)
 let p=head ps;cand=svModule p
 check "aggregate only renamed"(svSource p=="view"&&svHistorical p=="packed"&&svParts p==["left","right"])
 let states m=[s|VMStmt _ s<-vm_body m]
 check "all NEW state processes retained exactly"(states cand==states new)
 check "zero budget explicitly accounted"(null(stateViewProposals 0 old new)&&svaEligible(stateViewAudit 0 old new)==1&&svaBudgetLimited(stateViewAudit 0 old new))
 none "wrong component ordering" old(replaceDef "view"(VEConcat[e "right",e "left"])new)
 none "same width wrong input role" old(replaceDef "data_left"(VESelect(e "b")(VEConst 2)(VEConst 0))new)
 none "missing component transfer" old(replaceDef "data_left"(e "unknown")new)
 none "missing historical transfer"(replaceDef "data_packed"(e "unknown")old)new
 none "target name occupied" old(append new[decl VDWire 8 "packed"])
 none "source protected by translate-off use" old(append new[VMGroup True[[assign "debug"(e "view")]]])
 none "source protected by opaque statement" old(append new[VMStmt True(Vinitial(VAssign(VLId(v "debug"))(e "view")))])
 none "source is a port" old(new{vm_ports=vm_ports new++[([VAOutput(v "view")(range 8)],[])]})
 none "fixed ports changed" old(new{vm_ports=vm_ports new++[([VAInput(v "extra")Nothing],[])]})
 none "different module" old(new{vm_name=v "Different"})
 none "hierarchical source reference" old(append new[assign "debug"(e "view.child")])
 none "hierarchical target reference" old(append new[assign "debug"(e "packed.child")])
 none "duplicate source declaration" old(append new[decl VDWire 8 "view"])
 none "duplicate source definition" old(append new[assign "view"(VEConcat[e "left",e "right"])])
 none "partial state view" old(replaceDef "view"(VEConcat[VESelect(e "left")(VEConst 1)(VEConst 0),e "right"])new)
 none "repeated state component" old(replaceDef "view"(VEConcat[e "left",e "left"])new)
 none "wire instead of state component" old(append(replaceDef "view"(VEConcat[e "notState",e "right"])new)[decl VDWire 3 "notState",assign "notState"(e "a")])
 let badWidth newItem=case newItem of VMDecl(VVDecl VDWire _ [VVar i])|getVIdString i=="view"->decl VDWire 7 "view";_->newItem
 none "aggregate width mismatch" old(new{vm_body=map badWidth(vm_body new)})
 none "directive comment" old(new{vm_comments=["synthesis keep"]})
 none "old directive comment"(old{vm_comments=["synopsys full_case"]})new
 none "cyclic input helper" old(replaceDef "data_left"(e "data_left")new)
 none "old group is not canonical"(old{vm_body=map removeGroup(vm_body old)})new
 let two=append old(state "anotherPacked" "ResetReg" 8(VEConcat[e "a",e "b"]))
 check "multiple naming alternatives accounted without claiming uniqueness"(length(proposals two new)==2&&svaEligible(stateViewAudit 1 two new)==2&&svaBudgetLimited(stateViewAudit 1 two new))
 let differentReset=new{vm_body=map changeReset(vm_body new)}
     changeReset(VMStmt False s)=VMStmt False(change s)
     changeReset x=x
     change(Valways x)=Valways(change x)
     change(VAt ev x)=VAt ev(change x)
     change(Vifelse c (VAssignA l@(VLId i) _) b)|getVIdString i=="right"=Vifelse c(VAssignA l(lit 5 7))b
     change x=x
     differentProposal=proposals old differentReset
 check "different NEW reset value is retained, not compared to OLD"(length differentProposal==1&&states(svModule(head differentProposal))==states differentReset)
 let alpha=[("packed","view")]
 forM_ [("strict NEW preserved",alpha,cand,True),
        ("wire-only rename needs no state assumption",[],cand,True),
        ("changed aggregate behavior rejected",alpha,replaceDef "packed"(VEConcat[e "right",e "left"])cand,False),
        ("changed NEW state transfer rejected",alpha,replaceDef "data_left"(lit 3 0)cand,False),
        ("wrong relation rejected",[("packed","left")],cand,False)] $ \(label,relation,c,expected)->do
  result<-runASTProof ys 20(out</>filter(/=' ')label)StrictFourState relation(prog new)(prog c)
  writeFile(out</>filter(/=' ')label<.>"json")(renderProofResult result)
  check label(proofPassed result==expected)
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS directedchecks="++show n++"\n")
 putStrLn("PASS directedchecks="++show n)
 where removeGroup(VMRegGroup i cls cs x)=VMRegGroup(v "wrongGroup")cls cs x
       removeGroup x=x
