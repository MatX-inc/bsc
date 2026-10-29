-- Optional naming-only proposals for pure local wires, including value reads
-- in effect/assertion statements. No executable expression is copied from OLD.
module ECOASTValueNames
  ( valueNamesProvider, valueNamesAudit, ValueNamesAudit(..), renameValues ) where
import Verilog
import ECOASTSearch(CandidateProvider(..),candidateProvider)
import ECOASTTransform(ASTCandidate(..),generateASTCandidates,expressionWidth)
import ECOASTPreservation(directiveFree)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(nub)
import Control.Monad(unless)

name=getVIdString
-- VId provenance is not a semantic AST child. In particular, do not interpret
-- original-instance metadata as a callable, event or variable occurrence.
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe[](:[])(G.cast x)++case G.cast x::Maybe VId of
 Just _->[]
 Nothing->concat(G.gmapQ values x)
refs x=nub[name i|VEVar i<-values x]
allIds x=[name i|i<-values x::[VId]]
flat=concatMap go where
 go(VMGroup _ gs)=flat(concat gs)
 go(VMComment _ x)=flat[x]
 go(VMRegGroup _ _ _ x)=flat[x]
 go x=[x]
visible=concatMap go where
 go(VMGroup False gs)=visible(concat gs)
 go(VMGroup True _) = []
 go(VMComment _ x)=visible[x]
 go(VMRegGroup _ _ _ x)=visible[x]
 go x=[x]
unique xs=length xs==length(nub xs)
need b e=unless b(Left e)

-- Normalize only non-emitted annotations and equivalent literal radix.
canon e=case e of
 VEVar i->VEVar(mkVId(name i))
 VEWConst _ w _ n->VEWConst defaultVId w 10 n
 VEOp _ a o b->VEOp defaultVId(canon a)o(canon b)
 VEUnOp _ o a->VEUnOp defaultVId o(canon a)
 VEConcat xs->VEConcat(map canon xs)
 VEIf c a b->VEIf(canon c)(canon a)(canon b)
 VESelect x h l->VESelect(canon x)(canon h)(canon l)
 VESelect1 x k->VESelect1(canon x)(canon k)
 VERepeat n x->VERepeat(canon n)(canon x)
 _->e
pureExpr e=case e of
 VEVar _->True;VEConst _->True;VEWConst _ w _ _->w>0
 VEOp _ a _ b->pureExpr a&&pureExpr b
 VEUnOp _ _ a->pureExpr a
 VEConcat xs->not(null xs)&&all pureExpr xs
 VEIf c a b->all pureExpr[c,a,b]
 VESelect x h l->all pureExpr[x,h,l]
 VESelect1 x k->all pureExpr[x,k]
 VERepeat n x->all pureExpr[n,x]
 _->False

-- Only declaration binders, continuous-writer lvalues and VEVar VALUE reads.
-- Never change task/function names, operator annotations, strings, metadata,
-- clocks/events, ports, instance interfaces or module identities.
renameValues pairs p=walk p where
 table=M.fromList pairs
 rid i@(VId s pos info)=maybe i(\n->VId n pos info)(M.lookup s table)
 expr(VEVar i)=VEVar(rid i);expr x=x
 target(VLId i)=VLId(rid i);target x=x
 binder(VVar i)=VVar(rid i);binder x=x
 walk :: G.Data a=>a->a
 walk x=case G.cast x::Maybe VId of
  Just _->x
  Nothing->(G.mkT expr `G.extT` target `G.extT` binder)(G.gmapT walk x)

-- The audit is proposal evidence, not an equivalence certificate. Every chosen
-- candidate still passes the unchanged complete CURRENT proof and saved replay.
data ValueNamesAudit=ValueNamesAudit
 { vnaPairs::[(String,String)],vnaRejected::[String] } deriving(Eq,Show)

evidence oldP@(VProgram[old]od _)newP@(VProgram[new]nd _)=do
 need(vm_name old==vm_name new&&map fst(vm_ports old)==map fst(vm_ports new))"module or fixed ports differ"
 need(directiveFree oldP&&directiveFree newP)"directive-bearing comment"
 let items m=flat(vm_body m)
     declarations m=[(name i,(t,r))|VMDecl(VVDecl t r vs)<-items m,VVar i<-vs]
     portDecls m=[(name(vargName a),(t,r))|(as,_)<-vm_ports m,a<-as,(t,r)<-case a of VAInput _ r->[(VDInput,r)];VAOutput _ r->[(VDOutput,r)];_->[]]
     symbols m=M.fromList(declarations m++portDecls m)
     defs m=[(name i,e)|VMAssign(VLId i)e<-visible(vm_body m)]
     os=symbols old;ns=symbols new;oa=M.fromList(defs old);na=M.fromList(defs new)
     width s n=expressionWidth s(VEVar(mkVId n))
     opaque x=case x of VMFunction{}->True;_->False
 need(null od&&null nd&&not(any opaque(items old++items new)))"function/DPI scope"
 need(all (unique.map fst.declarations)[old,new])"duplicate body declarations"
 need(all (unique.map fst.defs)[old,new])"duplicate continuous drivers"
 let bodyPortConsistent m=let bs=M.fromList(declarations m);ps=M.fromList(portDecls m) in
      all(\n->case(width bs n,width ps n)of (Just a,Just b)->a>0&&a==b;_->False)(M.keys(M.intersection bs ps))
 need(all bodyPortConsistent[old,new])"inconsistent body/port declaration width"
 let eligible m sy ds n=case M.lookup n sy of
      Just(VDWire,_)->n `notElem`map fst(portDecls m)&&M.member n ds
      _->False
     oldMissing=[(o,e)|(o,e)<-M.toAscList oa,M.notMember o ns,eligible old os oa o]
     newMissing=[(n,e)|(n,e)<-M.toAscList na,M.notMember n os,eligible new ns na n]
     typed e=all(\n->case(width os n,width ns n)of(Just a,Just b)->a>0&&a==b;_->False)(refs e)
     pairs=[(n,o)|(o,e)<-oldMissing,(n,f)<-newMissing,pureExpr e,pureExpr f,canon e==canon f,typed e,
                  Just w<-[width os o],w>0,width ns n==Just w]
     names=map fst pairs++map snd pairs
     called p=[name i|VEFctCall i _<-values p]++[name i|VTask i _<-values p]
     events p=values p::[VEventExpr]
     targets p=[name i|VLId i<-values [s|VMStmt _ s<-items p]]
     reserved m=S.fromList(allIds(vm_ports m)++concat[refs r|VMDecl(VVDecl _ r _)<-items m]++
       [name i|Vdumpvars _ is<-values(vm_body m),i<-is]++concat[allIds x|x<-items m,case x of
       VMInst{}->True;VMDecl(VVDWire{}) ->True;VMDecl(VVDecl _ _ vs)->any array vs;_->False]++
       [takeWhile(/='.') n|n<-allIds m,'.' `elem`n])
     array(VVar _)=False;array _=True
     blocked=S.unions[reserved old,reserved new,S.fromList(called oldP++called newP),
       S.fromList(targets old++targets new),S.fromList(concatMap refs(events oldP++events newP))]
     allWriters m=M.fromListWith(+)[(name i,1::Int)|VMAssign l _<-items m,VLId i<-values l]
 need(not(null pairs))"no exact typed vacant-name correspondence"
 need(unique(map fst pairs)&&unique(map snd pairs))"ambiguous exact typed correspondence"
 need(all(`S.notMember`blocked)names)"reserved non-value, event, instance, array, initialized-wire or procedural-writer occurrence"
 need(all(\(n,o)->M.lookup n(allWriters new)==Just 1&&M.lookup o(allWriters old)==Just 1)pairs)"nonunique writer"
 let candidate=renameValues pairs newP
 need(renameValues[(o,n)|(n,o)<-pairs]candidate==newP)"noninjective or incomplete inverse value rename"
 pure(pairs,candidate)
evidence _ _=Left "exactly one module required"

valueNamesAudit old current=case evidence old current of
 Left why->ValueNamesAudit[][why]
 Right(pairs,_)->ValueNamesAudit pairs[]
valueNamesProvider=candidateProvider["ValueNames","ValueNamesLayout"][] generate where
 generate _ features budget old current
  | "ValueNames" `notElem`features=[]
  | otherwise=case evidence old current of
   Left _->[]
   Right(pairs,p)->
    let base=ASTCandidate "ValueNames" "Exact typed pure local-wire value names; protected call/event/metadata identities unchanged" p
          [("simultaneous-value-only-rename",pairs)]False[(o,n)|(n,o)<-pairs][]
        layouts=[c{acFeature="ValueNamesLayout",acDescription=acDescription base++" with historical F04 layout",
          acOperations=acOperations base++acOperations c,acStateMap=acStateMap base}
          |"ValueNamesLayout" `elem`features,c<-generateASTCandidates["F04"]4 old p]
    in take(max 0 budget)(layouts++[base])
