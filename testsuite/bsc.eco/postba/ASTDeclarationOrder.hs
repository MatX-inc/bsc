module Main where
import Control.Monad(unless,forM_)
import Data.Either(isRight)
import ECOASTDeclarationOrder
import ECOASTFinishOrder(orderCostImproves)
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch
import ECOASTProof
import PPrint(pp80)
import BinData(encode)
import GenABin()
import System.Environment
import System.FilePath
import Verilog
v=mkVId
e=VEVar . v
r w=Just(VEConst(w-1),VEConst 0)
d w ns=VMDecl(VVDecl VDWire(r w)(map(VVar . v)ns))
a n rhs=VMAssign(VLId(v n))rhs
base xs=VProgram[VModule(v"m")[][([VAInput(v"x")(r 8),VAOutput(v"y")(r 8)],[])]xs][][]
body f(VProgram[m]ds cs)=VProgram[m{vm_body=f(vm_body m)}]ds cs
old=base[d 8["a"],d 8["b"],d 8["c"],a"a"(e"x"),a"y"(e"a")]
current=body(\(a:b:c:xs)->c:a:b:xs)old
check label ok=unless ok(error label)
no x y=null(declarationOrderCandidates 1 x y)
proposal x y=acProgram(head(declarationOrderCandidates 1 x y))
main=do
 [out,self,ys]<-getArgs
 let append xs=body(++xs)
     groups before=base(if before then [VMComment["A"](VMGroup False[[d 8["a","aa"]]]),VMRegGroup(v"b")"RegA"["B"](VMGroup False[[d 8["b"]]])]
                                  else [VMRegGroup(v"b")"RegA"["B"](VMGroup False[[d 8["b"]]]),VMComment["A"](VMGroup False[[d 8["a","aa"]]])])
     novelOld=base[d 8["a"],d 8["b"]]
     novelNew=base[d 8["b"],d 8["fresh"],d 8["a"]]
     novelExpected=base[d 8["a"],d 8["b"],d 8["fresh"]]
     barrier x=no(base[d 8["a"],x,d 8["b"]])(base[d 8["b"],x,d 8["a"]])
     initialized=VMDecl(VVDWire(r 8)(VVar(v"z"))(e"x"))
     array=VMDecl(VVDecl VDWire(r 8)[VArray(VEConst 3,VEConst 0)(v"z")])
     process=VMStmt False(Vinitial(VAssign(VLId(v"y"))(e"x")))
     off=VMGroup True[[d 8["z"]]]
     wrap p=body(\xs->[VMGroup False[[a"anchor"(e"x"),VMComment["local"](VMGroup False[xs++[a"end"(e"x")]])]]])p
     dpi i=case old of VProgram ms _ cs->VProgram ms[VDPI i VDT_void []]cs
     plainDPI=dpi(v"external")
     hiddenDPI=dpi(case v"external" of VId s source _->VId s source(Just(a"debug"(VEConst 1))))
     tests=
      [("whole declaration permutation",proposal old current==old)
      ,("strict X/Z identity",isRight(checkAST StrictFourState[]current old))
      ,("exact group objects preserved",proposal(groups True)(groups False)==groups True)
      ,("stable unmatched block attachment",proposal novelOld novelNew==novelExpected)
      ,("all novel no match",no(base[d 8["other"]])novelNew)
      ,("CURRENT widths preserved",let n=body(\(c:a:b:xs)->c:d 9["a"]:b:xs)current in declarationOrderOnly n(proposal old n))
      ,("CURRENT types preserved",let n=body(\(c:a:b:xs)->c:VMDecl(VVDecl VDReg(r 8)[VVar(v"a")]):b:xs)current in declarationOrderOnly n(proposal old n))
      ,("duplicate CURRENT names decline",no old(append[d 8["a"]]current))
      ,("duplicate OLD names decline",no(append[d 8["a"]]old)current)
      ,("initialized wire barrier",barrier initialized)
      ,("array declaration barrier",barrier array)
      ,("port declaration barrier",barrier(VMDecl(VVDecl VDOutput(r 8)[VVar(v"z")])))
      ,("assignment barrier",barrier(a"z"(e"x")))
      ,("process barrier",barrier process)
      ,("translate-off barrier",barrier off)
      ,("OLD translate-off gives no ranking",no(base[VMGroup True[[d 8["a"],d 8["b"]]]])(base[d 8["b"],d 8["a"]]))
      ,("nested mixed container local ordering",declarationOrderOnly(wrap current)(proposal(wrap old)(wrap current)))
      ,("directive comment protected",no old(body(\xs->[VMComment["synthesis keep"](VMGroup False[xs])])current))
      ,("module directive protected",no old(case current of VProgram[m]ds cs->VProgram[m{vm_comments=["pragma keep"]}]ds cs))
      ,("fixed ports",no old(case current of VProgram[m]ds cs->VProgram[m{vm_ports=[]}]ds cs))
      ,("fixed point",no old old)
      ,("empty budget",null(declarationOrderCandidates 0 old current)&&null(declarationOrderCandidates(-1)old current))
      ,("disabled family",null(cpGenerate declarationOrderProvider[][]1 old current))
      ,("partition mutation rejected by structural guard",not(declarationOrderOnly(base[d 8["a","b"]])(base[d 8["a"],d 8["b"]])))
      ,("width mutation rejected by structural guard",not(declarationOrderOnly(base[d 8["a"]])(base[d 9["a"]])))
       ,("hidden top-level DPI metadata mutation rejected",plainDPI==hiddenDPI&&encode plainDPI/=encode hiddenDPI&&not(declarationOrderOnly plainDPI hiddenDPI))
      ,("nondeclaration mutation rejected",not(declarationOrderOnly old(append[a"extra"(e"x")]old)))
      ,("equal substantive raw improvement accepted",orderCostImproves(70,33)(68,33))
      ,("substantive regression rejected",not(orderCostImproves(70,33)(68,34)))
      ,("equal raw no improvement",not(orderCostImproves(70,33)(70,33)))
      ,("substantive improvement can increase raw",orderCostImproves(70,33)(71,32))]
 forM_ tests(uncurry check)
 let opts=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["DeclarationOrder"],soYosys=ys}
     provider=declarationOrderProvider{cpSources=self:cpSources declarationOrderProvider}
 result<-optimizeAST (defaultSearchRequest opts (out</>"search")
   (SearchInput old pp80) (SearchInput current pp80)) {searchProvider=provider}
 check"search selects intact declaration reorder"(srProgram result==old)
 replay<-replayASTPlan opts pp80(out</>"search/search.plan")(out</>"replay")current
 check"saved plan independently replayed"(replay==old)
 putStrLn("PASS: "++show(length tests+2)++" declaration-order guard, cost, strict proof and replay checks")
