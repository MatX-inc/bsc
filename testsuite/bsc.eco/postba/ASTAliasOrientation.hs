module Main where
import Control.Monad(unless,forM_)
import Data.Either(isRight)
import ECOASTAliasOrientation
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch
import ECOASTProof
import PPrint(pp80)
import System.Environment
import System.FilePath
import Verilog
v=mkVId
e=VEVar . v
range w=Just(VEConst(w-1),VEConst 0)
d w ns=VMDecl(VVDecl VDWire(range w)(map(VVar . v)ns))
a n rhs=VMAssign(VLId(v n))rhs
op o x y=VEOp defaultVId x o y
value=op VAnd(e "x")(e "z")
base xs=VProgram[VModule(v "m")[] [([VAInput(v "x")(range 8),VAInput(v "z")(range 8),VAOutput(v "y")(range 8)],[])]xs][][]
old=base[d 8["original","shared"],a "original" value,a "shared"(op VOr(e "x")(e "z")),a "y"(op VXor(e "original")(e "shared"))]
current=base[d 8["original","shared"],a "original"(e "shared"),a "shared" value,a "y"(op VXor(e "original")(e "shared"))]
expected=base[d 8["original","shared"],a "original" value,a "shared"(e "original"),a "y"(op VXor(e "original")(e "shared"))]
body f(VProgram[m]ds cs)=VProgram[m{vm_body=f(vm_body m)}]ds cs
change=body . map
append xs=body(++xs)
replace n rhs=change(\x->case x of VMAssign(VLId i)_|getVIdString i==n->a n rhs;_->x)
shape=change(\x->case x of VMAssign l _->VMAssign l(VEConst 0);_->x)
strict x y=isRight(checkAST StrictFourState[]x y)
no before after=null(aliasOrientationCandidates 64 before after)
check label ok=unless ok(error label)
main=do
 [out,self,ys]<-getArgs
 let generated=aliasOrientationCandidates 64 old current
     second before=[d 8["a2","b2"],a "a2"(if before then value else e "b2"),a "b2"(if before then op VOr(e "x")(e "z") else value)]
     bothOld=append(second True)old
     bothNew=append(second False)current
     ambiguousOld=append[d 8["other"],a "other" value]old
     ambiguousNew=append[d 8["other"],a "other"(e "shared")]current
     cycleE=op VAnd(e "via")(e "x")
     cycOld=replace "original" cycleE old
     cycNew=append[d 8["via"],a "via"(e "original")](replace "shared" cycleE current)
     exposeShared(VProgram[m]ds cs)=VProgram[m{vm_ports=vm_ports m++[([VAOutput(v "shared")(range 8)],[])]}]ds cs
     extra=concat[[d 8[n],a n(VEWConst defaultVId 8 10 0)]|i<-[1::Int ..600],let n="extra"++show i]
     tests=
      [("one existing alias orientation",length generated==1&&acProgram(head generated)==expected)
      ,("strict four-state identity",strict current expected)
      ,("all declarations state control and consumers fixed",shape current==shape expected)
      ,("strict eligible no contracts or alpha",all(\c->not(acBinaryOnly c)&&null(acContracts c)&&null(acStateMap c))generated)
      ,("fixed point",no old expected)
      ,("OLD must supply exact nonalias definition",no(replace "original"(e "z")old)current)
      ,("different value declines",no old(replace "shared"(op VOr(e "x")(e "z"))current))
      ,("different destination width declines",no old(change(\x->case x of VMDecl{}->d 7["original","shared"];_->x)current))
      ,("changed OLD width declines",no(change(\x->case x of VMDecl{}->d 7["original","shared"];_->x)old)current)
      ,("unknown expression width declines",no old(replace "shared"(e "unbound")current))
      ,("duplicate wire declaration declines",no old(append[d 8["shared"]]current))
      ,("array target declines",no old(change(\x->case x of VMDecl{}->VMDecl(VVDecl VDWire(range 8)[VVar(v "original"),VArray(VEConst 1,VEConst 0)(v "shared")]);_->x)current))
      ,("procedural writer including translate-off declines",no old(append[VMStmt True(Vinitial(VAssign(VLId(v "shared"))(e "x")))]current))
      ,("alias-side procedural duplicate declines",no old(append[VMStmt True(Vinitial(VAssign(VLId(v "original"))(e "x")))]current))
      ,("existing shared port cannot be reoriented",no(exposeShared old)(exposeShared current))
      ,("initialized wire writer declines",no old(append[VMDecl(VVDWire(range 8)(VVar(v "shared"))(e "z"))]current))
      ,("function expression declines",no(replace "original"(VEFctCall(v "f")[e "x"])old)(replace "shared"(VEFctCall(v "f")[e "x"])current))
      ,("macro expression declines",no(replace "original"(VEMacro "MACRO")old)(replace "shared"(VEMacro "MACRO")current))
      ,("transitive self-dependence declines",no cycOld cycNew)
      ,("directives decline",no old(case current of VProgram[m]ds cs->VProgram[m{vm_comments=["synthesis keep"]}]ds cs))
      ,("fixed ports",no old(case current of VProgram[m]ds cs->VProgram[m{vm_ports=[]}]ds cs))
      ,("two disjoint pairs offer joint plus singles",length(aliasOrientationCandidates 64 bothOld bothNew)==3)
      ,("shared target offers alternatives without conflicting joint",length(aliasOrientationCandidates 64 ambiguousOld ambiguousNew)==2)
      ,("all disjoint proposals strict",all(strict bothNew . acProgram)(aliasOrientationCandidates 64 bothOld bothNew))
      ,("no module-size restriction",length(aliasOrientationCandidates 64(append extra old)(append extra current))==1)
      ,("proposal budget",length(aliasOrientationCandidates 1 bothOld bothNew)==1&&null(aliasOrientationCandidates 0 old current)&&null(aliasOrientationCandidates (-1) old current))
      ,("disabled provider",null(cpGenerate aliasOrientationProvider[][]64 old current))]
 forM_ tests(uncurry check)
 let options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["AliasOrientation"],soYosys=ys}
     provider=aliasOrientationProvider{cpSources=self:cpSources aliasOrientationProvider}
 result<-optimizeAST (defaultSearchRequest options (out</>"search")
   (SearchInput old pp80) (SearchInput current pp80)) {searchProvider=provider}
 check "fresh search chooses unchanged OLD helper definition"(srProgram result==expected)
 replay<-replayASTPlan options pp80(out</>"search/search.plan")(out</>"replay")current
 check "saved replay independently proves orientation"(replay==expected)
 p<-runASTProof ys 30(out</>"wrong")Binary[]current(replace "shared"(e "z")expected)
 check "solver rejects wrong transferred value"(proofStatus p=="rejected")
 putStrLn("PASS: "++show(length tests+3)++" alias-orientation candidate, guard and strict proof/replay checks")
