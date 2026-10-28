-- Retain every current declaration/control and re-extract only a
-- CURRENT closed typed pure value at an OLD common-name alias representative.
module ECOASTAliases (Representative(..), representativeProposals) where
import Verilog
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(sortOn,nub)
import Control.Monad(guard,foldM)

data Representative=Representative {repSource::String,repTarget::String,repConsumers::[String],repModule::VModule} deriving(Eq,Show)
name=getVIdString
flat=concatMap go where
 go(VMGroup _ xs)=flat(concat xs)
 go(VMComment _ x)=go x
 go(VMRegGroup _ _ _ x)=go x
 go x=[x]
ports m=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
syms m=M.map head.M.filter((==1).length).M.fromListWith(++)$
 [(name i,[(ty,r)])|VMDecl(VVDecl ty r vs)<-flat(vm_body m),VVar i<-vs]++
 [(name(vargName a),[(VDInput,r)])|(as,_)<-vm_ports m,a<-as,
    r<-case a of VAInput _ r->[r];VAOutput _ r->[r];_->[]]
-- Only assignments in transparent current containers may move a definition.
-- All explicit assignment writers, including opaque and translate-off scopes,
-- still count. Child directions and possible child-output conflicts remain
-- independent whole-module proof obligations. Policy comes from the proposer.
visible okay=concatMap go where
 go x@(VMComment _ child)|okay[x]=go child
 go x@(VMRegGroup _ _ _ child)|okay[x]=go child
 go(VMGroup False xs)=visible okay(concat xs)
 go(VMGroup True _) = []
 go(VMComment _ _) = []
 go(VMRegGroup _ _ _ _) = []
 go x=[x]
defs okay m=M.map head.M.filter((==1).length).M.fromListWith(++)$
 [(name i,[e])|VMAssign(VLId i)e<-visible okay(vm_body m)]
lvalues(VLId i)=[name i]
lvalues(VLConcat xs)=concatMap lvalues xs
lvalues(VLSub x _)=lvalues x
written s=case s of
 Valways x->written x;Vinitial x->written x;VAt _ x->written x;VSeq xs->concatMap written xs
 Vif _ x->written x;Vifelse _ a b->written a++written b
 Vcase _ as _ _->concatMap arm as;Vcasex _ as _ _->concatMap arm as
 VAssign l _->lvalues l;VAssignA l _->lvalues l;_->[]
 where arm(VCaseArm _ x)=written x;arm(VDefault x)=written x
writers m=M.fromListWith(+)[(n,1::Int)|x<-flat(vm_body m),n<-case x of
 VMAssign l _->lvalues l;VMStmt _ s->nub(written s);VMDecl(VVDWire _ v _)->[name(vvName v)];_->[]]
kids(VEUnOp _ _ x)=[x]
kids(VEOp _ a _ b)=[a,b]
kids(VEConcat xs)=xs
kids(VESelect x h l)=[x,h,l]
kids(VESelect1 x i)=[x,i]
kids(VERepeat n x)=[n,x]
kids(VEIf c a b)=[c,a,b]
kids _=[]
pureE(VEFctCall _ _)=False
pureE(VEMacro _)=False
pureE(VEIndex _ _)=False
pureE(VEString _)=False
pureE(VEReal _)=False
pureE e=all pureE(kids e)
rebuild(VEUnOp _ o _)[x]=VEUnOp defaultVId o x
rebuild(VEOp _ _ o _)[a,b]=VEOp defaultVId a o b
rebuild(VEConcat _)xs=VEConcat xs
rebuild(VESelect _ _ _)[x,h,l]=VESelect x h l
rebuild(VESelect1 _ _)[x,i]=VESelect1 x i
rebuild(VERepeat _ _)[n,x]=VERepeat n x
rebuild(VEIf _ _ _)[c,a,b]|a==b=a
                                |otherwise=VEIf c a b
rebuild(VEWConst _ w b n)[]=VEWConst defaultVId w b n
rebuild x[]=x
rebuild _ _=error "impossible native expression shape"
type Width = M.Map String (VDType,Maybe VRange) -> VExpr -> Maybe Integer
expanded :: Width -> M.Map String (VDType,Maybe VRange) -> M.Map String VExpr -> M.Map String Int -> VExpr -> Maybe (VExpr,Int)
expanded expressionWidth sy ds counts=walk 1024 S.empty where
 walk fuel active e=do
  guard(fuel>0&&pureE e)
  case e of
   VEIf _ a b | a==b -> walk (fuel-1) active a
   VEVar i->do
    guard(M.member(name i)sy)
    case M.lookup(name i)ds of
     Nothing->return(e,fuel-1)
     Just rhs->do
      guard(name i `S.notMember` active&&M.lookup(name i)counts==Just 1)
      let w=expressionWidth sy e
      guard(w/=Nothing&&expressionWidth sy rhs==w)
      walk(fuel-1)(S.insert(name i)active)rhs
   _->do
    (parts,left)<-foldM(\(xs,left)x->do (y,left')<-walk left active x;return(xs++[y],left'))([],fuel-1)(kids e)
    return(rebuild e parts,left)
replace table m=m{vm_body=map go(vm_body m)} where
 go(VMGroup off xs)=VMGroup off(map(map go)xs)
 go(VMComment cs x)=VMComment cs(go x)
 go(VMRegGroup i cls cs x)=VMRegGroup i cls cs(go x)
 go x@(VMAssign(VLId i)_)=maybe x(VMAssign(VLId i))(M.lookup(name i)table)
 go x=x
-- OLD contributes only alias directions. Every transferred function and every
-- equivalence comparison comes from CURRENT. The independent whole-module gate
-- remains mandatory; these finite structural matches are proposal evidence.
representativeProposals :: Width -> ([VMItem] -> Bool) -> Int -> VModule -> VModule -> [Representative]
representativeProposals expressionWidth okay bound old current=take(max 0 bound).dedup S.empty.sortOn rank$proposals where
 ns=syms current;od=defs okay old;nd=defs okay current;wc=writers current
 aliases=M.fromListWith(++)[(name h,[n])|(n,VEVar h)<-M.toAscList od,n/=name h,M.member(name h)od]
 fixed=ports current
 eligible n=case M.lookup n ns of
  Just(VDWire,_)->n `S.notMember` fixed&&M.lookup n wc==Just 1&&M.member n nd
  _->False
 sameWidth a b=let wa=expressionWidth ns(VEVar(mkVId a));wb=expressionWidth ns(VEVar(mkVId b)) in wa/=Nothing&&wa==wb
 value n=fst <$> expanded expressionWidth ns nd wc(VEVar(mkVId n))
 nonalias(VEVar _)=False
 nonalias _=True
 proposals=[Representative source target members(replace changes current)
  |(target,priorConsumers)<-M.toAscList aliases,eligible target,source<-priorConsumers,eligible source,sameWidth source target
  ,Just currentValue<-[M.lookup source nd],nonalias currentValue,pureE currentValue
  ,Just resolved<-[value source],value target==Just resolved
  ,let members=sortOn id[n|n<-priorConsumers,eligible n,sameWidth n target,value n==Just resolved]
  ,source `elem` members
  -- The value transferred is current source RHS, never any OLD definition.
  -- Reject transitive dependence on either rewritten destination. Even a
  -- semantically redundant conditional can otherwise create a structural cycle.
  ,Just dependencies<-[reach 256 S.empty(names currentValue)]
  ,S.null(dependencies `S.intersection` S.fromList(target:members))
  ,let changes=M.fromList((target,currentValue):[(n,VEVar(mkVId target))|n<-members])]
 reach fuel seen pending
  | S.null pending=Just seen
  | fuel<=0=Nothing
  | otherwise=let (n,rest)=S.deleteFindMin pending in
     if n `S.member` seen then reach fuel seen rest
     else reach(fuel-1)(S.insert n seen)(rest `S.union` maybe S.empty names(M.lookup n nd))
 names(VEVar i)=S.singleton(name i)
 names e=S.unions(map names(kids e))
 rank p=(-length(repConsumers p),repTarget p,repSource p)
 dedup _ []=[]
 dedup seen(p:ps)|repTarget p `S.member` seen=dedup seen ps
                  |otherwise=p:dedup(S.insert(repTarget p)seen)ps
