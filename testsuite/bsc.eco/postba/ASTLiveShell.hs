module Main where
import Control.Monad(unless,forM_)
import Data.Either(isRight)
import Data.Maybe(isNothing)
import ECOASTLiveShell
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch
import ECOASTProof
import PPrint(pp80)
import System.Environment
import System.FilePath
import Verilog
v=mkVId
e=VEVar . v
a n rhs=VMAssign(VLId(v n))rhs
d ns=VMDecl(VVDecl VDWire Nothing(map(VVar . v)ns))
op o x y=VEOp defaultVId x o y
base xs=VProgram[VModule(v "m")[] [([VAInput(v "x")Nothing,VAInput(v "z")Nothing,VAOutput(v "y")Nothing],[])]xs][][]
old=base[d["h","obsolete","mid"],a "h"(e "x"),a "obsolete"(e "z"),a "mid"(e "obsolete"),a "y"(op VLAnd(e "h")(e "mid"))]
current=base[d["k"],a "k"(e "x"),a "y"(op VLOr(e "k")(e "z"))]
expected=base[d["h"],a "h"(e "x"),a "y"(op VLOr(e "h")(e "z"))]
withBody f(VProgram[m]ds cs)=VProgram[m{vm_body=f(vm_body m)}]ds cs
append xs(VProgram[m]ds cs)=VProgram[m{vm_body=vm_body m++xs}]ds cs
change f(VProgram[m]ds cs)=VProgram[m{vm_body=map f(vm_body m)}]ds cs
constant n=[d[n],a n(VEWConst defaultVId 1 10 0)]
strict x y=isRight(checkAST StrictFourState[]x y)
check label ok=unless ok(error label)
main=do
 [out,self,ys]<-getArgs
 let candidates=liveShellCandidates old current
     proposed=map acProgram candidates
     scaffold=constant "ordinary"
     projected p=projectLiveShell current p
     keepRoot use=case projectLiveShell current(append([d["unused"],a "unused"(e "x"),use])expected)of
       Just(_,removed)->"unused" `notElem` removed;_->False
     extras n=concat[constant("limit"++show i)|i<-[1::Int ..n]]
     smallOld=base[d["h"],a "h"(e "x"),a "y"(op VLAnd(e "h")(e "z"))]
     limitOld=append(extras 510)smallOld
     limitNew=append(extras 510)current
     overOld=append(extras 511)smallOld
     overNew=append(extras 511)current
     uninspected=error "excluded OLD inspected CURRENT"
     tests=
      [("two domain-labelled proposals",length candidates==2&&map acBinaryOnly candidates==[False,True])
      ,("live role preserved, dead transitive cone removed",head proposed==expected)
      ,("strict ordinary-order proposal",strict current(head proposed))
      ,("ordinary scaffold survives unchanged",map acProgram(liveShellCandidates(append scaffold old)(append scaffold current))==replicate 2(append scaffold expected))
      ,("missing ordinary scaffold declines",null(liveShellCandidates old(append scaffold current)))
      ,("changed ordinary scaffold declines",null(liveShellCandidates(append(constant "ordinary")old)(append[d["ordinary"],a "ordinary"(e "x")]current)))
      ,("complete AST assertion roots",keepRoot(VMStmt True(VAssert(VEE(e "x"))[e "unused"])))
      ,("complete AST task roots",keepRoot(VMStmt True(Vinitial(VTask(v "$display")[e "unused"]))))
      ,("non-expression dump roots",keepRoot(VMStmt True(Vinitial(Vdumpvars 0[v "unused"]))))
      ,("procedural writer roots",keepRoot(VMStmt False(Valways(VAt(VEE(e "x"))(VAssign(VLId(v "unused"))(e "z"))))))
      ,("effectful function helper retained",case projected(append[d["effect"],a "effect"(VEFctCall(v "$random")[])]expected)of Just(_,removed)->"effect"`notElem`removed;_->False)
      ,("duplicate dead declarations decline",isNothing(projected(append[d["unused","unused"],a "unused"(e "x")]expected)))
      ,("multiple dead writers decline",isNothing(projected(append[d["unused"],a "unused"(e "x"),a "unused"(e "z")]expected)))
      ,("array declarations decline",isNothing(projected(append[VMDecl(VVDecl VDWire Nothing[VArray(VEConst 1,VEConst 0)(v "arr")])]expected)))
      ,("dead register cannot be removed",isNothing(projected(append[VMDecl(VVDecl VDReg Nothing[VVar(v "unused")]),a "unused"(e "x")]expected)))
      ,("directives decline",null(liveShellCandidates old(case current of VProgram[m]ds cs->VProgram[m{vm_comments=["synthesis keep"]}]ds cs)))
      ,("fixed ports",null(liveShellCandidates old(case current of VProgram[m]ds cs->VProgram[m{vm_ports=[]}]ds cs)))
      ,("OLD bounded before matching",null(liveShellCandidates(append(concat[constant("extra"++show n)|n<-[1::Int ..512]])old)current))
      ,("CURRENT bounded before matching",null(liveShellCandidates old(append(concat[constant("extra"++show n)|n<-[1::Int ..512]])current)))
      ,("exactly512 on both sides remains eligible",not(null(liveShellCandidates limitOld limitNew)))
      ,("513 OLD rejects before inspecting CURRENT",null(liveShellCandidates overOld uninspected))
      ,("513 CURRENT rejects",null(liveShellCandidates smallOld overNew))
      ,("translated-off group included in fixed OLD limit",null(liveShellCandidates(withBody(\xs->[VMGroup True[xs]])overOld)uninspected))
      ,("comment wrapper included in fixed OLD limit",null(liveShellCandidates(withBody(\xs->[VMComment["informational"](VMGroup False[xs])])overOld)uninspected))
      ,("register-group wrapper included in fixed OLD limit",null(liveShellCandidates(withBody(\xs->[VMRegGroup(v "wrapper") ""[](VMGroup False[xs])])overOld)uninspected))
      ,("disabled provider",null(cpGenerate liveShellProvider[]32 old current))
      ,("zero candidate budget",null(cpGenerate liveShellProvider["LiveShell"]0 old current))]
 forM_ tests(uncurry check)
 let options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["LiveShell"],soYosys=ys}
     provider=liveShellProvider{cpSources=self:cpSources liveShellProvider}
 result<-optimizeAST (defaultSearchRequest options (out</>"search")
   (SearchInput old pp80 []) (SearchInput current pp80 [])) {searchProvider=provider}
 check "fresh strict search selects live shell"(srProgram result==expected)
 replay<-replayASTPlan [] options pp80(out</>"search/search.plan")(out</>"replay")current
 check "saved plan independently reproves"(replay==expected)
 let wrong=change(\x->case x of VMAssign(VLId n)_|getVIdString n=="y"->a "y"(e "z");_->x)expected
 p<-runASTProof ys 30(out</>"wrong")Binary[]current wrong
 check "independent solver rejects wrong live rewrite"(proofStatus p=="rejected")
 putStrLn("PASS: "++show(length tests+3)++" live-shell candidate, liveness, guard and proof/replay checks")
