module Main where
import ECOASTIdentity
import Verilog
import ECOASTProof
import Control.Monad
import Data.List(sort)
import Data.IORef
import System.Environment(getArgs)
import System.Directory
import System.FilePath
v=mkVId
e=VEVar . v
width n=if n==1 then Nothing else Just(VEConst(n-1),VEConst 0)
lit n x=VEWConst defaultVId n 10 x
decl ty n s=VMDecl(VVDecl ty(width n)[VVar(v s)])
assign n rhs=VMAssign(VLId(v n))rhs
stage n cls bits rhs=[VMRegGroup(v n)cls[](VMGroup False [[decl VDReg bits n,decl VDWire bits("operand_"++n),decl VDWire 1("permission_"++n)]]),assign("operand_"++n)rhs,assign("permission_"++n)(e "enable"),VMStmt False(Valways(VAt(VEEposedge(e "CLK"))(Vif(e("permission_"++n))(VAssignA(VLId(v n))(e("operand_"++n)))))),VMStmt False(Vinitial(VAssign(VLId(v n))(lit bits 0)))]
modul body=VModule(v "M")[] [([VAInput(v "CLK")Nothing,VAInput(v "enable")Nothing,VAInput(v "a")(width 8),VAInput(v "b")(width 8),VAOutput(v "o")(width 8)],[])]body
prog m=VProgram[m][][]
append m xs=m{vm_body=vm_body m++xs}
old=modul(stage "terminal" "Reg" 8(e "a")++[assign "o"(e "terminal")])
current=modul(stage "survivor" "Reg" 8(e "b")++[assign "o"(e "survivor")])
proposals=stateRoleAlternatives 8
main=do
 [ys,out]<-getArgs
 createDirectory out
 count<-newIORef(0::Int)
 let check label condition=do unless condition(fail label);modifyIORef' count(+1);putStrLn("PASS "++label)
     none label a b=check label(null(proposals a b))
 let ps=proposals old current
 check "one complete group with vacant historical names"(length ps==1)
 let candidate=head ps;forward=srpForwardNames candidate
 check "all populated names travel together without suffix assumptions"(sort forward==sort[("survivor","terminal"),("operand_survivor","operand_terminal"),("permission_survivor","permission_terminal")])
 check "zero budget"(null(stateRoleAlternatives 0 old current))
 none "historical state name occupied by local wire" old(append current[decl VDWire 8 "terminal"])
 none "historical populated member occupied" old(append current[decl VDWire 8 "operand_terminal"])
 none "historical name occupied by port" old(current{vm_ports=vm_ports current++[([VAInput(v "terminal")(width 8)],[])]})
 none "hierarchical target reference" old(append current[assign "debug"(e "terminal.q")])
 none "hierarchical source reference" old(append current[assign "debug"(e "survivor.q")])
 none "duplicate source declaration" old(append current[decl VDWire 8 "operand_survivor"])
 none "incompatible state class" old(modul(stage "survivor" "OtherReg" 8(e "b")++[assign "o"(e "survivor")]))
 none "incompatible state width" old(modul(stage "survivor" "Reg" 7(e "b")++[assign "o"(e "survivor")]))
 let morePort m=m{vm_ports=vm_ports m++[([VAOutput(v "p")(width 8)],[])]}
 let ambiguousOld=morePort(append old(stage "another" "Reg" 8(e "a")++[assign "p"(e "another")]))
     ambiguousNew=morePort(append current[assign "p"(e "survivor")])
     ambiguity=vacancyAudit 1 ambiguousOld ambiguousNew
 check "ambiguous roles enumerate both alternatives"(length(proposals ambiguousOld ambiguousNew)==2)
 check "ambiguity is explicit rather than claimed unique"(all(\(_,_,kind,_)->kind=="enumerated-alternative")(vaPairOutcomes ambiguity))
 check "candidate budget accounts for unselected alternative"(vaEligible ambiguity==2&&vaEmitted ambiguity==1&&vaBudgetLimited ambiguity)
 let buriedOld=old{vm_body=init(vm_body old)++[decl VDWire 8 "old_cone",assign "old_cone"(VEOp defaultVId(e "terminal") VAdd (e "a")),assign "o"(e "old_cone")]}
     buriedNew=current{vm_body=init(vm_body current)++[decl VDWire 8 "new_cone",assign "new_cone"(VEOp defaultVId(e "survivor") VAdd (e "b")),assign "o"(e "new_cone")]}
     buried=proposals buriedOld buriedNew
 check "buried changed consumer needs no exact output vote"(length buried==1&&null(srpOutputConsumers(head buried)))
 let multiple=append buriedNew(stage "second" "Reg" 8(e "a"))
 check "multiple compatible CURRENT groups enumerate alternatives"(length(proposals buriedOld multiple)==2)
 forM_(zip[0::Int ..](proposals buriedOld multiple)) $ \(i,alternative)->do
  let relation=[(b,a)|(a,b)<-srpForwardNames alternative]
  result<-runASTProof ys 20(out</>("multiple-alternative-"++show i))StrictFourState relation(prog multiple)(prog(srpModule alternative))
  writeFile(out</>("multiple-alternative-"++show i++".json"))(renderProofResult result)
  check("multiple compatible alternative independently proved "++show i)(proofPassed result)
 let alpha=[(b,a)|(a,b)<-forward]
 forM_ [("strict-current-function-preserved",alpha,prog(srpModule candidate),True),
        ("missing-state-relation-rejected",[],prog(srpModule candidate),False),
        ("old-function-rejected",alpha,prog old,False)] $ \(label,relation,cand,expected)->do
  result<-runASTProof ys 20(out</>label)StrictFourState relation(prog current)cand
  writeFile(out</>label<.>"json")(renderProofResult result++"\n")
  check label(proofPassed result==expected)
 let packed=modul(stage "packed" "ResetReg" 9(VEConcat[lit 1 1,e "a"])++[assign "o"(VESelect(e "packed")(VEConst 7)(VEConst 0))])
     split=modul(stage "payload" "NoResetReg" 8(e "b")++[assign "o"(e "payload")])
     projected=proposals packed split
 check "unique fixed-output projection admits changed width and class"(length projected==1)
 let projectedCandidate=head projected
 check "projection role recorded honestly"(any(\(_,_,k,status)->k=="unique-projected-output-role"&&status=="eligible-pending-proof-and-cost")(vaPairOutcomes(vacancyAudit 8 packed split)))
 none "projection target member occupied" packed(append split[decl VDWire 8 "operand_packed"])
 none "projection out of bounds rejected"(packed{vm_body=init(vm_body packed)++[assign "o"(VESelect(e "packed")(VEConst 9)(VEConst 2))]})split
 none "projection must cover whole current state" packed(modul(stage "payload" "NoResetReg" 7(e "b")++[assign "o"(e "payload")]))
 none "changed fixed output type rejected" packed(split{vm_ports=vm_ports split++[([VAOutput(v "o")(width 7)],[])]})
 let malformed=split{vm_body=map changeOperand(vm_body split)}
     changeOperand(VMRegGroup i cls flags(VMGroup g [ds]))=VMRegGroup i cls flags(VMGroup g [map narrower ds])
     changeOperand x=x
     narrower(VMDecl(VVDecl VDWire _ [VVar i]))|getVIdString i=="operand_payload"=decl VDWire 7 "operand_payload"
     narrower x=x
 none "nonstate operand does not follow state width" packed malformed
 let packedAmb=morePort(append packed(stage "otherPacked" "ResetReg" 9(VEConcat[lit 1 0,e "b"])++[assign "p"(VESelect(e "otherPacked")(VEConst 7)(VEConst 0))]))
     splitAmb=morePort(append split[assign "p"(e "payload")])
 none "equal projection-role tie does not loosen width alternatives" packedAmb splitAmb
 check "projected zero budget is explicit"(null(stateRoleAlternatives 0 packed split)&&vaBudgetLimited(vacancyAudit 0 packed split))
 let projectedRelation=[(b,a)|(a,b)<-srpForwardNames projectedCandidate]
     wrongReset=(srpModule projectedCandidate)
       {vm_body=vm_body(srpModule projectedCandidate)++[VMStmt False(Vinitial(VAssign(VLId(v "packed"))(lit 8 1)))]}
 forM_ [("projected-preserves-new-state-and-reset",projectedRelation,prog(srpModule projectedCandidate),True),
        ("projected-missing-relation-rejected",[],prog(srpModule projectedCandidate),False),
        ("projected-added-initial-value-rejected",projectedRelation,prog wrongReset,False)] $ \(label,relation,cand,expected)->do
  result<-runASTProof ys 20(out</>label)StrictFourState relation(prog split)cand
  writeFile(out</>label<.>"json")(renderProofResult result++"\n")
  check label(proofPassed result==expected)
 n<-readIORef count
 writeFile(out</>"summary.txt")("PASS directedchecks="++show n++"\n")
 putStrLn("PASS directedchecks="++show n)
