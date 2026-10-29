-- Optional final-AST naming proposals preserve CURRENT values, types and owners.
-- Historical expressions provide evidence, never replacement values.
module ECOASTCorrespondence
  ( correspondenceProvider, correspondenceAudit, CorrespondenceAudit(..) ) where
import Verilog
import VLocalNames(mapRenameableVIds)
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..),generateASTCandidates)
import ECOASTPreservation(directiveFree)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(nub,sort)
import Control.Monad(guard,zipWithM)
import Data.Data

name=getVIdString
flat=concatMap go where
 go(VMGroup _ xs)=flat(concat xs)
 go(VMComment _ x)=go x
 go(VMRegGroup _ _ _ x)=go x
 go x=[x]
visible=concatMap go where
 go(VMGroup False xs)=visible(concat xs)
 go(VMGroup True _) = []
 go(VMComment _ x)=go x
 go(VMRegGroup _ _ _ x)=go x
 go x=[x]
ids :: Data a => a -> [String]
ids x=case cast x of
 Just i->[name(i::VId)]
 Nothing->concat(gmapQ ids x)
lhs(VLId i)=[name i]
lhs(VLConcat xs)=concatMap lhs xs
lhs(VLSub x _) = lhs x
writes s=case s of
 Valways x->writes x;Vinitial x->writes x;VAt _ x->writes x
 VSeq xs->concatMap writes xs;Vif _ x->writes x;Vifelse _ a b->writes a++writes b
 Vcase _ as _ _->concatMap arm as;Vcasex _ as _ _->concatMap arm as
 VAssign l _->lhs l;VAssignA l _->lhs l;_->[]
 where arm(VCaseArm _ x)=writes x;arm(VDefault x)=writes x
unique=M.map head.M.filter((==1).length).M.fromListWith(++)
declarations m=M.union (M.fromList [(name i,(ty,r))|(as,_)<-vm_ports m,a<-as,(i,ty,r)<-port a])
 (unique[(name i,[(t,r)])|VMDecl(VVDecl t r vs)<-flat(vm_body m),VVar i<-vs]) where
 port(VAInput i r)=[(i,VDInput,r)]
 port(VAOutput i r)=[(i,VDOutput,r)]
 port(VAInout i _ (Just r))=[(i,VDInout,r)]
 port _=[]
definitions m=unique[(name i,[e])|VMAssign(VLId i)e<-visible(vm_body m)]
ports m=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
protected dpis m=ports m `S.union` S.fromList(ids dpis++concatMap item(vm_body m)++hierarchy) where
 item x@VMInst{}=ids x
 item x@VMFunction{}=ids x
 item x@(VMGroup True _)=ids x
 item x@(VMStmt True _)=ids x
 item(VMGroup False xs)=concatMap item(concat xs)
 item(VMComment _ x)=item x
 item(VMRegGroup _ _ _ x)=item x
 item _=[]
 hierarchy=[takeWhile(/='.') n|n<-ids m,'.' `elem` n]
counts m=M.fromListWith(+)[(n,1::Int)|x<-flat(vm_body m),n<-case x of
 VMAssign l _->lhs l;VMStmt _ s->nub(writes s);VMDecl(VVDWire _ v _)->[name(vvName v)];_->[]]
value(VEConst n)=Just n
value(VEWConst _ _ _ n)=Just n
value _=Nothing
width Nothing=Just 1
width(Just(h,l))=do a<-value h;b<-value l;guard(b==0&&a>=0);pure(a+1)
-- Only same-operator, same-position evidence; changed literal values are not
-- copied. The correspondence is required to be unique on both sides.
align(VEVar a)(VEVar b)=Just[(name a,name b)]
align(VEIf c a b)(VEIf d e f)=concat <$> sequence[align c d,align a e,align b f]
align(VEOp _ a o b)(VEOp _ c p d)|o==p=(++) <$> align a c <*> align b d
align(VEUnOp _ o a)(VEUnOp _ p b)|o==p=align a b
align(VEConcat as)(VEConcat bs)|length as==length bs=concat <$> zipWithM align as bs
align(VESelect a h l)(VESelect b i j)=concat <$> sequence[align a b,align h i,align l j]
align(VESelect1 a h)(VESelect1 b i)=(++) <$> align a b <*> align h i
align a b|value a/=Nothing&&value b/=Nothing=Just[]
align a b|a==b=Just[]
align _ _=Nothing
canon e=case e of
 VEWConst _ w b n->VEWConst defaultVId w b n
 VESelect x h l->VESelect(canon x)(canon h)(canon l)
 VESelect1 x i->VESelect1(canon x)(canon i)
 VERepeat n x->VERepeat(canon n)(canon x)
 _->e
-- Direct terminal expressions only; no recursive expansion, operator identity
-- assumption, CSE, new sharing, state recoding, or dependence on a name suffix.
terminal os ns od nd e=case e of
 VEVar i->do a<-boundary i os;b<-boundary i ns;guard(a==b);pure a
 VESelect(VEVar i)h l->do a<-boundary i os;b<-boundary i ns;hi<-value h;lo<-value l;guard(0<=lo&&lo<=hi&&hi<a&&hi<b);pure(hi-lo+1)
 VESelect1(VEVar i)j->do a<-boundary i os;b<-boundary i ns;k<-value j;guard(0<=k&&k<a&&k<b);pure 1
 VERepeat n x->do k<-value n;guard(k>0);w<-terminal os ns od nd x;pure(k*w)
 _->Nothing
 where boundary i sy=do guard(M.notMember(name i)od&&M.notMember(name i)nd);(_,r)<-M.lookup(name i)sy;width r

data CorrespondenceAudit=CorrespondenceAudit
 { caWidthPairs::[(String,String)],caExactPairs::[(String,String)]
 , caRejected::[(String,String)],caBudgetLimited::Bool } deriving(Eq,Show)

analyse oldDpis newDpis old current=(grown,exact,rejected) where
 os=declarations old;ns=declarations current;od=definitions old;nd=definitions current
 oc=counts old;nc=counts current;op=protected oldDpis old;np=protected newDpis current
 allowed sy ds wc fixed n=case M.lookup n sy of
  Just(VDWire,r)->width r/=Nothing&&M.member n ds&&M.lookup n wc==Just 1&&n `S.notMember` fixed
  _->False
 oe=allowed os od oc op;ne=allowed ns nd nc np
 growth o n=oe o&&ne n&&M.notMember o ns&&M.notMember n os&&case(M.lookup o os,M.lookup n ns)of
  (Just(_,a),Just(_,b))->case(width a,width b)of(Just x,Just y)->y>x;_->False
  _->False
 votes=nub[(n,o)|(c,a)<-M.toAscList od,Just b<-[M.lookup c nd],Just ps<-[align a b],(o,n)<-ps,growth o n]
 unambiguous(n,o)=length[x|(x,y)<-votes,x==n]==1&&length[y|(x,y)<-votes,y==o]==1
 grown=filter unambiguous votes
 buckets sy ds permitted= M.fromListWith(++)
  [(show w++":"++show(canon e),[n])|(n,e)<-M.toAscList ds,permitted n,Just(_,r)<-[M.lookup n sy],Just w<-[width r],terminal os ns od nd e==Just w]
 ob=buckets os od oe;nb=buckets ns nd ne
 wanted=[(n,o)|(k,[o])<-M.toAscList ob,Just[n]<-[M.lookup k nb],n/=o]
 sources=map fst wanted;targets=map snd wanted
 displaced=sort[n|n<-targets,n `notElem` sources,M.member n ns]
 vacated=sort[n|n<-sources,n `notElem` targets]
 safe=length displaced<=length vacated&&all ne displaced
 exact=if safe then wanted++zip displaced vacated else []
 rejected=[(n++"->"++o,"ambiguous width-changing consumer role")|(n,o)<-votes,not(unambiguous(n,o))]++
  [(k,"ambiguous exact expression bucket")|(k,as)<-M.toAscList ob,Just bs<-[M.lookup k nb],length as/=1||length bs/=1]++
  [(n,"occupied target is not an eligible continuous wire")|n<-displaced,not(ne n)]

apply pairs m=m{vm_body=mapRenameableVIds rename(vm_body m)} where
 table=M.fromList pairs
 rename v@(VId s pos info)=maybe v(\t->VId t pos info)(M.lookup s table)
-- All native comment fields are checked before naming or layout proposals.
-- Opaque items also reserve every identifier they mention, including definitions
-- outside a translate-off wrapper but read inside it.
correspondenceAudit budget oldP@(VProgram olds od _) newP@(VProgram currents nd _)
 | not(directiveFree oldP&&directiveFree newP)=CorrespondenceAudit [] [] [("program","directive-bearing comment boundary")] False
 | otherwise=
 let entries=[analyse od nd old current|old<-olds,current<-currents,vm_name old==vm_name current]
     gs=concat[g|(g,_,_)<-entries];es=concat[e|(_,e,_)<-entries]
     count=sum[length(filter(not.null)[g,e])|(g,e,_)<-entries]
 in CorrespondenceAudit gs es (concat[r|(_,_,r)<-entries]) (count>max 0 budget)
correspondenceProvider=CandidateProvider ["F24WidthRole","F24ExactLeaf","F24Layout"] [] generate where
 generate _ features budget oldP@(VProgram olds od _) newP@(VProgram currents nd cs)
  | not(directiveFree oldP&&directiveFree newP)=[]
  | otherwise=take(max 0 budget)(concatMap withLayout base)
  where
   base=[ASTCandidate feature description(VProgram(take i currents++[apply pairs current]++drop(i+1)currents)nd cs)
     [("simultaneous-wire-rename",pairs)] False [(o,n)|(n,o)<-pairs][]
    |(i,current)<-zip[0..]currents,old<-olds,vm_name old==vm_name current,
     let(grown,exact,_)=analyse od nd old current,
     (feature,description,pairs)<-[("F24WidthRole","Retain historical names at uniquely corresponding widened continuous-wire roles",grown),("F24ExactLeaf","Retain historical names for unique exact typed terminal expressions",exact)],
     feature `elem` features,not(null pairs)]
   -- A bounded joint proposal avoids requiring search to accept an adverse
   -- naming intermediate. The existing F04 generators provide the layout;
   -- the complete candidate is proved directly against CURRENT. F24Layout is a separate explicit
   -- opt-in. The public runner enables it only when F04 is enabled; provider
   -- dispatch passes each provider only its own registered feature names.
   withLayout candidate=
    [layout {acFeature="F24Layout",acDescription=acDescription candidate++" with historical layout",
       acOperations=acOperations candidate++acOperations layout,
       acStateMap=acStateMap candidate}
     | "F24Layout" `elem` features,
       layout<-generateASTCandidates ["F04"] 4 oldP(acProgram candidate)]++[candidate]
