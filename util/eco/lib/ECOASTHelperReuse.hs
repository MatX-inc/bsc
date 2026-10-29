-- Untrusted optional proposals. OLD functions are naming/placement evidence;
-- neither a pipeline bypass nor a retained state relation is an assumption.
module ECOASTHelperReuse(HelperReuseCandidate(..),helperReuseCandidates,helperReuseProvider) where
import Verilog
import ECOASTTransform(expressionWidth)
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..))
import ECOASTPreservation(directiveFree,transparentFlat)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(nub,sort)
import Data.Maybe(mapMaybe)
import Control.Monad(guard,unless,foldM)
import Control.Monad.State.Strict(StateT,evalStateT,get,put,lift)

data HelperReuseCandidate=HelperReuseCandidate
 { reuseLive::VProgram
 , reuseAlpha::[(String,String)],reuseOperations::[(String,String)] } deriving(Show)
name=getVIdString
names :: G.Data a=>a->[String]
names x=case G.cast x of Just i->[name(i::VId)];Nothing->concat(G.gmapQ names x)
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe[](:[])(G.cast x)++case G.cast x::Maybe VId of Just _->[];Nothing->concat(G.gmapQ values x)
refs x=nub[name i|VEVar i<-values x]
canon :: G.Data a=>a->a
canon=G.everywhere(G.mkT norm) . G.everywhere(G.mkT(\i->mkVId(name i))) where
 norm(VEOp _ a o b)=VEOp defaultVId a o b
 norm(VEUnOp _ o a)=VEUnOp defaultVId o a
 norm(VEWConst _ w _ n)=VEWConst defaultVId w 10 n
 norm(VEVar i)=VEVar(mkVId(name i))
 norm e=e
same a b=show(canon a)==show(canon b)
rename mp=G.everywhere(G.mkT ren) where ren i@(VId s p q)=maybe i(\t->VId t p q)(M.lookup s mp)
flat=concatMap go where
 go(VMGroup _ xs)=flat(concat xs);go(VMComment _ x)=flat[x];go(VMRegGroup _ _ _ x)=flat[x];go x=[x]
edge(VEE _)=False;edge(VEEOr a b)=edge a||edge b;edge _=True
writes s=nub([name i|VAssign(VLId i)_<-values s]++[name i|VAssignA(VLId i)_<-values s])
stateWrites xs=nub[ n|VMStmt _ (Valways(VAt ev s))<-xs,edge ev,n<-writes s]
pureExpr e=null[()|x<-values e,case x of VEFctCall{}->True;VEIndex{}->True;VEMacro{}->True;_->False]
syms m=M.fromList([(name i,(t,r))|VMDecl(VVDecl t r vs)<-flat(vm_body m),VVar i<-vs]++[(name(vargName a),(t,r))|(as,_)<-vm_ports m,a<-as,(t,r)<-case a of VAInput _ r->[(VDInput,r)];VAOutput _ r->[(VDOutput,r)];_->[]])
defs m=M.fromList[(name i,e)|VMAssign(VLId i)e<-transparentFlat(vm_body m)]
procs m=[(n,s)|VMStmt False s@(Valways(VAt ev body))<-transparentFlat(vm_body m),not(edge ev),[n]<-[writes body],null[()|VAssignA{}<-values body]]
need b why=unless b(Left why)
commentOnly=G.everywhere(G.mkT one) where
 one(VMRegGroup i _ cs x)=VMComment(["register "++name i]++cs)x
 one x=x
-- Exact OLD helper edges are proposed only with equal declared bit widths.
-- Every occurrence is subsequently proved against CURRENT, not assumed equal.
-- Each expanded expression is bounded independently. Assignment edges may only
-- be expanded when the declared owner and its RHS have the same known width;
-- truncation/extension is never erased by this proposal heuristic.
expand symbols definitions bypass procMap seen e=evalStateT(go 0 seen e)20000 where
 width x=expressionWidth symbols x
 positiveSame a b=case(width a,width b)of(Just x,Just y)->x>0&&x==y;_->False
 go :: Int->[String]->VExpr->StateT Int(Either String)VExpr
 go depth path expr=do
  left<-get
  if left<=0||depth>=64 then lift(Left"helper expansion budget exhausted")else put(left-1)
  case expr of
   VEVar i|name i `elem`path->lift(Left"cyclic helper expansion")
   VEVar i|Just n<-M.lookup(name i)bypass->do
    unless(positiveSame expr(VEVar(mkVId n)))(lift(Left"bypass width is unknown or unequal"))
    go(depth+1)(name i:path)(VEVar(mkVId n))
   VEVar i|Just n<-M.lookup(name i)procMap->return(VEVar(mkVId n))
   VEVar i|Just rhs<-M.lookup(name i)definitions->do
    unless(positiveSame expr rhs)(lift(Left"continuous boundary truncates, extends, or has unknown width"))
    go(depth+1)(name i:path)rhs
   _->G.gmapM(sub(depth+1)path)expr
 sub :: G.Data a=>Int->[String]->a->StateT Int(Either String)a
 sub depth path x=case G.cast x of
   Just expr->do y<-go depth path expr;maybe(lift(Left"expression cast"))return(G.cast y)
   Nothing->case G.cast x::Maybe VId of Just _->return x;Nothing->G.gmapM(sub depth path)x
mapExprChildren :: (VExpr->Either String VExpr)->VExpr->Either String VExpr
mapExprChildren f=G.gmapM sub where
 sub :: G.Data a=>(a->Either String a)
 sub x=case G.cast x of
  Just e->f e >>= maybe(Left"expression cast")Right . G.cast
  Nothing->case G.cast x::Maybe VId of Just _->Right x;Nothing->G.gmapM sub x
children :: G.Data a=>a->[VExpr]
children x=case G.cast x of Just e->[e];Nothing->case G.cast x::Maybe VId of Just _->[];Nothing->concat(G.gmapQ children x)
-- Only same statement kind/arm layout. NEW selectors/conditions are retained;
-- an OLD continuous alias may receive a CURRENT value of exactly the same width.
alignStmt oldWidth newWidth oldDefs old new=case(old,new)of
 (VAssign a e,VAssign b f)|a==b->do
  (rhs,bindings)<-case e of
   VEVar i|M.member(name i)oldDefs&&oldWidth e==newWidth f&&maybe False(>0)(oldWidth e)->Right(e,[(name i,f)])
   _->Right(f,[])
  Right(VAssign a rhs,bindings)
 (VSeq as,VSeq bs)|length as==length bs->do xs<-zipWithM align as bs;Right(VSeq(map fst xs),concatMap snd xs)
 (Vcase _ as p f,Vcase e bs q g)|p==q&&f==g&&length as==length bs->do
  xs<-zipWithM arm as bs;Right(Vcase e(map fst xs)p f,concatMap snd xs)
 (Vif _ a,Vif e b)->do(x,ps)<-align a b;Right(Vif e x,ps)
 (Vifelse _ a b,Vifelse e c d)->do(x,p)<-align a c;(y,q)<-align b d;Right(Vifelse e x y,p++q)
 _->Left"pure process shape differs"
 where align=alignStmt oldWidth newWidth oldDefs
       arm(VCaseArm as x)(VCaseArm bs y)|same as bs=do(z,p)<-align x y;Right(VCaseArm as z,p)
       arm(VDefault x)(VDefault y)=do(z,p)<-align x y;Right(VDefault z,p)
       arm _ _=Left"case arm/default/priority differs"
       zipWithM f xs ys=sequence(zipWith f xs ys)
projectStmt keep s=case s of
 VAssign(VLId i)_|name i `S.notMember`keep->Nothing
 VAssignA(VLId i)_|name i `S.notMember`keep->Nothing
 VSeq xs->case mapMaybe(projectStmt keep)xs of []->Nothing;ys->Just(VSeq ys)
 Vif c x->Vif c <$>projectStmt keep x
 Vifelse c a b->case(projectStmt keep a,projectStmt keep b)of (Nothing,Nothing)->Nothing;(x,y)->Just(Vifelse c(maybe(VSeq[])id x)(maybe(VSeq[])id y))
 Vcase c as p f->if S.null(S.fromList(writes s)`S.intersection`keep)then Nothing else Just(Vcase c(map arm as)p f)
  where arm(VCaseArm es x)=VCaseArm es(maybe(VSeq[])id(projectStmt keep x));arm(VDefault x)=VDefault(maybe(VSeq[])id(projectStmt keep x))
 VAt e x->VAt e <$>projectStmt keep x
 Valways x->Valways <$>projectStmt keep x
 Vinitial x->Vinitial <$>projectStmt keep x
 _->Just s
project keep x=case x of
 VMComment cs y->VMComment cs <$>project keep y
 VMRegGroup i s cs y->VMRegGroup i s cs <$>project keep y
 VMGroup t xs->case filter(not.null)(map(mapMaybe(project keep))xs)of []->Nothing;ys->Just(VMGroup t ys)
 VMDecl(VVDecl t r vs)->case[v|v@(VVar i)<-vs,name i `S.member`keep]of []->Nothing;ys->Just(VMDecl(VVDecl t r ys))
 VMAssign(VLId i)_|name i `S.notMember`keep->Nothing
 VMStmt t s->VMStmt t <$>projectStmt keep s
 _->Just x
closure deps seeds stops=go seeds where
 go seen=let next=seen`S.union`S.unions[M.findWithDefault S.empty n deps `S.difference`stops|n<-S.toList seen]in if next==seen then seen else go next
helperReuseCandidates oldP@(VProgram[old]od oldComments) current@(VProgram[new]nd _)=do
 need(directiveFree oldP&&directiveFree current)"directive comments"
 need(null od&&null nd&&vm_name old==vm_name new&&vm_ports old==vm_ports new)"module/port/DPI boundary"
 let ol=flat(vm_body old);nl=flat(vm_body new)
 need(null[x|x<-ol++nl,case x of VMInst{}->True;VMFunction{}->True;_->False])"opaque item boundary"
 need(null[x|x<-ol++nl,VMDecl(VVDecl _ _ vs)<-[x],any(\v->case v of VVar{}->False;_->True)vs])"non-scalar declaration"
 let os=syms old;ns=syms new;oDefs=defs old;nDefs=defs new
     width sy e=expressionWidth sy e
     sameWidth a b=width os(VEVar(mkVId a))==width ns(VEVar(mkVId b))&&width os(VEVar(mkVId a))/=Nothing
     oldQ=S.fromList(stateWrites ol);newQ=S.fromList(stateWrites nl);gone=oldQ`S.difference`newQ
 need(not(S.null gone)&&newQ `S.isSubsetOf`oldQ&&all(\n->M.lookup n os==M.lookup n ns)(S.toList newQ))"requires typed state removal only"
 need(length(M.keys oDefs)==length[()|VMAssign{}<-transparentFlat(vm_body old)]&&length(M.keys nDefs)==length[()|VMAssign{}<-transparentFlat(vm_body new)])"duplicate/non-scalar continuous writer"
 need(all pureExpr(M.elems oDefs++M.elems nDefs))"effectful helper expression"
 let bodyNames m=[name i|VMDecl(VVDecl _ _ vs)<-flat(vm_body m),VVar i<-vs]
     portNames m=[name(vargName a)|(as,_)<-vm_ports m,a<-as]
     unique xs=length xs==length(nub xs)
 need(all(\m->unique(bodyNames m)&&unique(portNames m))[old,new])"duplicate declaration or port"
 let portBodyConsistent m=and
       [r==pr|VMDecl(VVDecl _ r vs)<-flat(vm_body m),VVar i<-vs,(as,_)<-vm_ports m,a<-as,name(vargName a)==name i,pr<-case a of VAInput _ x->[x];VAOutput _ x->[x];_->[]]
 need(all portBodyConsistent[old,new])"inconsistent port/body declaration"
 let scalarWriters m=[n|item<-transparentFlat(vm_body m),n<-case item of
       VMAssign(VLId i)_->[name i]
       VMStmt _ s@(Valways _)->writes s
       _->[]]
 need(all(unique.scalarWriters)[old,new])"multiple scalar process/continuous writers"
 need(all(\m->length(bodyNames m)<=4096)[old,new]&&M.size oDefs<=512&&M.size nDefs<=512)"declaration/helper budget exceeded"
 let pureBody s=case s of
       VAssign(VLId _)e->pureExpr e
       VSeq xs->all pureBody xs
       Vif c x->pureExpr c&&pureBody x
       Vifelse c a b->pureExpr c&&pureBody a&&pureBody b
       Vcase c as _ _->pureExpr c&&all arm as
       _->False
       where arm(VCaseArm es x)=all pureExpr es&&pureBody x;arm(VDefault x)=pureBody x
     safeProcess symbols(_,Valways(VAt ev body))=pureExpr ev&&pureBody body&&
       null[()|n<-writes body,n `elem`refs body]&&all(`elem`refs ev)(refs body)&&
       all(\n->case M.lookup n symbols of Just(VDReg,_)->maybe False(>0)(expressionWidth symbols(VEVar(mkVId n)));_->False)(writes body)
     safeProcess _ _=False
 need(all(safeProcess os)(procs old)&&all(safeProcess ns)(procs new))"impure, self-dependent, incompletely sensitive, or undeclared procedural helper"
 need(length(procs old)<=64&&length(procs new)<=64)"procedural-helper budget exceeded"
 need(all(\m->length(procs m)==length[()|VMStmt False(Valways(VAt ev _))<-transparentFlat(vm_body m),not(edge ev)])[old,new])"non-scalar or multi-writer pure process unsupported"
 let oProcs=procs old;nProcs=procs new
     normalizeProc n=rename(M.singleton n "HELPER_OUTPUT")
     pairs=[(n,o)|(n,p)<-nProcs,let matches=[o|(o,q)<-oProcs,sameWidth o n,same(normalizeProc n p)(normalizeProc o q)],[o]<-[matches],n/=o]
 need(length(nub(map snd pairs))==length pairs)"ambiguous procedural correspondence"
 let procMap=M.fromList pairs
     dataEdges=M.fromListWith(++)[(name q,[name i])|VMStmt _ (Valways(VAt ev body))<-ol,edge ev,VAssignA(VLId q)(VEVar i)<-values body]
     bypass=M.fromList[(q,d)|(q,ds)<-M.toList dataEdges,q `S.member`gone,[d]<-[nub ds],M.member d oDefs||d `elem`map fst oProcs,Just w<-[width os(VEVar(mkVId q))],w>0,width os(VEVar(mkVId d))==Just w]
     ordinaryScaffold=[n|(n,e)<-M.toList nDefs,Just f<-[M.lookup n oDefs],same e f,M.lookup n os==M.lookup n ns]
     rootNames=nub(ordinaryScaffold++[name i|(as,_)<-vm_ports new,VAOutput i _<-as]++concat[refs s|VMStmt _ s@(Valways(VAt ev _))<-nl,edge ev]++concat[refs s|VMStmt _ s@Vinitial{}<-nl])
     continuousBindings=[case oDefs M.! n of
       VEVar i|M.member(name i)oDefs,width os(VEVar i)==width ns e,maybe False(>0)(width ns e)->(name i,e)
       _->(n,e)
       |n<-rootNames,Just e<-[M.lookup n nDefs],M.member n oDefs,sameWidth n n]
     pairedRootProcesses=[(n,op,np)|(n,np)<-nProcs,n `elem`rootNames,Just op<-[lookup n oProcs]]
 aligned<-mapM(\(n,Valways(VAt oldEv ob),Valways(VAt _ nb))->do
    (body,bs)<-alignStmt(width os)(width ns)oDefs ob nb
    let selectors s=[e|Vcase e _ _ _<-values s]
        replacements=M.fromList[(name a,name b)|(VEVar a,VEVar b)<-zip(selectors ob)(selectors nb)]
        wanted=refs body;prior=filter(`elem`wanted)(refs(rename replacements oldEv));order=nub(prior++wanted)
    need(not(null order))"constant process sensitivity unsupported"
    let event=foldl1 VEEOr[ VEE(VEVar(mkVId x))|x<-order]
    Right(n,Valways(VAt event body),bs))pairedRootProcesses
 bindings<-foldM(\m(n,e)->case M.lookup n m of Nothing->Right(M.insert n e m);Just f->if same e f then Right m else Left"conflicting historical consumer roles")M.empty(continuousBindings++concat[bs|(_,_,bs)<-aligned])
 let expandedOld=[(n,e,w)|(n,rhs)<-M.toList oDefs,M.notMember n bindings,not(null(refs rhs)),Just w<-[width os(VEVar(mkVId n))],w>0,width os rhs==Just w,Right e<-[expand os oDefs bypass M.empty[n]rhs]]
     helperRejections=[(n,"unknown/unequal declaration-to-RHS width")|(n,rhs)<-M.toList oDefs,M.notMember n bindings,let w=width os(VEVar(mkVId n)),w/=width os rhs||maybe True(<=0)w]++
       [(n,why)|(n,rhs)<-M.toList oDefs,M.notMember n bindings,Left why<-[expand os oDefs bypass M.empty[n]rhs]]
     helperBuckets=M.fromListWith(++)[(show(w,canon e),[n])|(n,e,w)<-expandedOld]
     ambiguousHelpers=[ns|ns<-M.elems helperBuckets,length ns>1]
     preferred candidates=case candidates of [x]->Just x;_->Nothing
     rewriteExpr allowRoot e=do
       expanded<-expand ns nDefs M.empty procMap [] e
       let matches=[n|(n,f,w)<-expandedOld,same expanded f,width ns e==Just w]
       case if allowRoot then preferred matches else Nothing of
        Just n->Right(VEVar(mkVId n),[n])
        Nothing->case e of
         VEVar i|Just n<-M.lookup(name i)procMap->Right(VEVar(mkVId n),[n])
         VEVar i|M.member(name i)nDefs->case M.lookup(name i)oDefs of
           Just oldE|same oldE(nDefs M.! name i)->Right(e,[name i])
           _->Left("unmatched CURRENT helper: "++name i)
         _->do
           let directChildren=concat(G.gmapQ children e)
           xs<-mapM(rewriteExpr True)directChildren
           let table=M.fromList[(show a,b)|(a,(b,_))<-zip directChildren xs]
           replaced<-mapExprChildren(\x->maybe(Left"child expression absent")Right(M.lookup(show x)table))e
           Right(replaced,concatMap snd xs)
 results<-mapM(\(n,e)->do(f,hs)<-rewriteExpr False e;Right(n,f,hs))(M.toList bindings)
 let requested=S.fromList(concat[hs|(_,_,hs)<-results])
     transformedDefs=M.map(rename bypass)oDefs
     helperDeps=M.map(S.fromList.refs)transformedDefs
     helpers=closure helperDeps requested(S.fromList(M.keys os)`S.difference`M.keysSet oDefs)
     neededHelpers=helpers`S.intersection`M.keysSet oDefs
     replacements=M.union(M.fromList[(n,e)|(n,e,_)<-results])(M.restrictKeys transformedDefs neededHelpers)
     processReplacement=M.fromList[(n,p)|(n,p,_)<-aligned]
     change item=case item of
       VMAssign(VLId i)e->VMAssign(VLId i)(M.findWithDefault e(name i)replacements)
       VMStmt False p|[n]<-writes p,Just q<-M.lookup n processReplacement->VMStmt False q
       _->item
     update item=case item of
       VMComment cs x->VMComment cs(update x);VMRegGroup i s cs x->VMRegGroup i s cs(update x);VMGroup t xs->VMGroup t(map(map update)xs);_->change item
     candidate=commentOnly(VProgram[old{vm_body=map update(vm_body old)}]od oldComments)
     VProgram[cm]cd cc=candidate
     combDeps=M.fromList([(n,S.fromList(refs e))|(n,e)<-M.toList(defs cm)]++[(n,S.fromList(refs p))|(n,p)<-procs cm])
     ports=S.fromList[name(vargName a)|(as,_)<-vm_ports new,a<-as]
     liveNames=closure combDeps(S.fromList rootNames`S.union`newQ`S.union`ports)oldQ
 need(S.null(liveNames`S.intersection`gone))"reconstructed live cone still reads removed state"
 let declared=S.fromList(M.keys os)
     live=VProgram[cm{vm_body=mapMaybe(project(declared`S.intersection`liveNames))(vm_body cm)}]cd cc
     alpha0=[(o,n)|(n,o)<-pairs]
     sources=map fst alpha0;targets=map snd alpha0
     displaced=[n|n<-targets,n `notElem`sources,n `elem`names live]
     vacated=[n|n<-sources,n `notElem`targets]
     alpha=alpha0++zip displaced vacated
 need(length displaced<=length vacated)"procedural alpha permutation cannot be completed"
 Right[HelperReuseCandidate live alpha[("boundOwners",show(M.keys bindings)),("reusedHelpers",show(S.toList neededHelpers)),("bypassEvidence",show(M.toList bypass)),("removedState",show(S.toList gone)),("removedDeadNames",show(S.toList(declared`S.difference`liveNames))), ("liveRoots",show rootNames),("unchangedCurrentScaffold",show ordinaryScaffold),("helperRejections",show helperRejections),("ambiguousHelperFunctions",show ambiguousHelpers),("limits","512 continuous helpers,64 pure processes,4096 declarations;64 expansion levels/20000 expression nodes per attempt;1 joint candidate")]]
helperReuseCandidates _ _=Left"exactly one module required"

-- Only the live projection is exposed. Retaining disconnected OLD storage is a
-- distinct policy and is deliberately absent from this provider and its SDK.
-- The normal search owns cost selection, full CURRENT proof and saved replay.
helperReuseProvider :: CandidateProvider
helperReuseProvider=CandidateProvider ["F26LiveHelpers"] [] $ \features budget old current ->
 if "F26LiveHelpers" `notElem` features then [] else case helperReuseCandidates old current of
  Left _ -> []
  Right candidates -> take budget
   [ASTCandidate "F26LiveHelpers" "Reconstruct historical live helpers after state removal"
     (reuseLive candidate) [("live-helper-reuse",reuseOperations candidate)]
     False (reuseAlpha candidate) | candidate <- candidates]
