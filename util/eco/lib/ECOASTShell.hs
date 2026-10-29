-- Optional external policy; every candidate requires independent proof.
-- Preserve a historical pure combinational shell while placing CURRENT values
-- at typed consumer roles. Proposals always require the independent proof gate.
module ECOASTShell (shellProvider, shellCandidates) where
import Verilog
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..),expressionWidth)
import ECOASTPreservation
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(nub,sortOn,delete)
import Data.Maybe(mapMaybe,fromMaybe)
import Control.Monad(guard)

name=getVIdString
canon :: G.Data a => a -> a
canon=G.everywhere(G.mkT(\v->mkVId(name v)))
flat=concatMap f where
 f(VMGroup _ xs)=flat(concat xs)
 f(VMComment _ x)=f x
 f(VMRegGroup _ _ _ x)=f x
 f x=[x]
defs m=M.fromList[(name n,e)|VMAssign(VLId n)e<-transparentFlat(vm_body m)]
vars e=nub[name n|VEVar n<-G.listify(const True)e]
syms m=M.fromList([(name n,(t,r))|VMDecl(VVDecl t r vs)<-flat(vm_body m),VVar n<-vs]++
 [(name(vargName a),(t,r))|(as,_)<-vm_ports m,a<-as,(t,r)<-case a of
   VAInput _ r->[(VDInput,r)];VAOutput _ r->[(VDOutput,r)];_->[]])
rename mp=G.everywhere(G.mkT(\v->mkVId(M.findWithDefault(name v)(name v)mp)))
nonAssign=filter notDeclAssign.transparentFlat.vm_body where
 notDeclAssign(VMDecl _)=False
 notDeclAssign(VMAssign _ _)=False
 notDeclAssign _=True
comb(VEE _)=True
comb(VEEOr a b)=comb a&&comb b
comb _=False
processes m=[(n,s)|VMStmt False s@(Valways(VAt event _))<-transparentFlat(vm_body m),comb event,
  [n]<-[nub[name i|VAssign(VLId i)_<-G.listify(const True)s]]]
pureExpr e=null[()|VEFctCall _ _<-xs]&&null[()|VEIndex _ _<-xs]&&null[()|VEMacro _<-xs]
 where xs=G.listify(const True)e::[VExpr]
terms op(VEOp _ a o b)|op==o=terms op a++terms op b
terms _ e=[e]
ac :: VExpr -> String
ac=show.G.everywhere(G.mkT normal).canon where
 normal e@(VEOp _ _ o _)|o`elem`[VLAnd,VLOr]=
  foldl1(\a b->VEOp defaultVId a o b)(sortOn show(terms o e))
 normal e=e

shellProvider = CandidateProvider ["F22"] [] $ \_ features budget old current->
 if "F22"`elem`features then take budget(shellCandidates old current) else []
shellCandidates oldProgram@(VProgram[old]oldDpis _) current@(VProgram[new]dpis comments)=mapMaybe build[False,True]
 where
  od=defs old;nd=defs new;os=syms old;ns=syms new
  width sy n=expressionWidth sy(VEVar(mkVId n))
  equalType n o=width ns n==width os o&&width ns n/=Nothing
  ports=[name(vargName a)|(as,_)<-vm_ports new,a<-as]
  roots=nub([name n|(as,_)<-vm_ports new,VAOutput n _<-as]++vars(nonAssign new))
  anchors=[(n,n)|n<-roots,M.member n nd,M.member n od,equalType n n]
  procPairs=[(n,o)|(n,a)<-processes new,(o,b)<-processes old,equalType n o,
    canon(rename(M.singleton n "F22_PROCESS_OUTPUT")a)==canon(rename(M.singleton o "F22_PROCESS_OUTPUT")b)]
  add mp pairs=foldl one mp pairs where
   one m(n,o)|M.lookup n m==Just o=m
             |M.member n m||o`elem`M.elems m=m
             |equalType n o=M.insert n o m
             |otherwise=m
  initial=add(M.fromList anchors)procPairs
  align mp a b
   |canon a==canon(rename mp b)&&all(\n->M.notMember n nd||M.member n mp)(vars b)=Just[]
  align mp(VEVar o)(VEVar n)=do
   guard(M.member(name o)od&&M.member(name n)nd&&equalType(name n)(name o))
   guard(maybe True(==name o)(M.lookup(name n)mp))
   return[(name n,name o)]
  align mp a@(VEOp _ _ op _) b@(VEOp _ _ nop _)|op==nop&&op`elem`[VLAnd,VLOr]=do
   let as=terms op a;bs=terms op b
       cancel (xs,ys) x=case[y|y<-ys,canon x==canon(rename mp y)]of
         y:_->(delete x xs,delete y ys);_->(xs,ys)
       (left,right)=foldl cancel(as,bs)as
   guard(length left==length right&&length left<=1)
   fmap concat(sequence(zipWith(align mp)left right))
  align mp(VEOp _ a o b)(VEOp _ c p d)|o==p=both mp[a,b][c,d]
  align mp(VEUnOp _ o a)(VEUnOp _ p b)|o==p=align mp a b
  align mp(VEConcat as)(VEConcat bs)=both mp as bs
  align mp(VESelect a h l)(VESelect b j k)=both mp[a,h,l][b,j,k]
  align mp(VESelect1 a i)(VESelect1 b j)=both mp[a,i][b,j]
  align mp(VERepeat a n)(VERepeat b m)=both mp[a,n][b,m]
  align mp(VEIf c a b)(VEIf d x y)=both mp[c,a,b][d,x,y]
  align mp a b|canon a==canon(rename mp b)=Just[]
  align _ _ _=Nothing
  both mp as bs=guard(length as==length bs)>>fmap concat(sequence(zipWith(align mp)as bs))
  grow mp=add mp(exacts++roles) where
   exacts=[(n,o)|(n,e)<-M.toList nd,M.notMember n mp,
    let matches=[o|(o,f)<-M.toList od,o`notElem`M.elems mp,equalType n o,canon(rename mp e)==canon f],
    [o]<-[matches]]
   roles=concat[fromMaybe[](align mp a b)|(n,o)<-M.toList mp,Just a<-[M.lookup o od],Just b<-[M.lookup n nd]]
  mapping=iterate grow initial!!8
  reach seen []=seen
  reach seen(n:rest)|S.member n seen=reach seen rest
  reach seen(n:rest)=case M.lookup n nd of
   Nothing->reach seen rest
   Just e->reach(S.insert n seen)(vars e++rest)
  live=reach S.empty roots
  novel=live `S.difference` M.keysSet mapping
  used=S.fromList(M.keys os++M.keys ns)
  fresh n=head[x|i<-[0::Int ..],let x="eco_"++show i++"_"++n,S.notMember x used]
  allNames=M.union mapping(M.fromList[(n,if M.member n os then fresh n else n)|n<-S.toList novel])
  changed=[(o,rename allNames e)|(n,o)<-M.toList mapping,S.member n live,Just e<-[M.lookup n nd]]
  build historical=do
   guard(directiveFree oldProgram&&directiveFree current&&null oldDpis&&null dpis)
   guard(vm_name old==vm_name new&&vm_ports old==vm_ports new&&length(M.keys nd)<=512)
   guard(null[x|x<-flat(vm_body old++vm_body new),case x of VMInst{}->True;VMFunction{}->True;_->False])
   guard(length(M.keys od)==length[()|VMAssign(VLId _) _<-transparentFlat(vm_body old)]&&
         length(M.keys nd)==length[()|VMAssign(VLId _) _<-transparentFlat(vm_body new)])
   guard(all pureExpr(M.elems od++M.elems nd))
   guard(all(\n->M.member n od&&M.lookup n mapping==Just n)[n|n<-roots,M.member n nd])
   guard(sortOn show(map canon(nonAssign old))==sortOn show(map(canon.rename allNames)(nonAssign new)))
   guard(sortOn show[(o,r)|(o,(VDReg,r))<-M.toList os]==
         sortOn show[(M.findWithDefault n n allNames,r)|(n,(VDReg,r))<-M.toList ns])
   let bodies=M.fromList changed
       fix item@(VMAssign(VLId n)e)=case M.lookup(name n)bodies of
        Nothing->item
        Just f|canon e==canon f||historical&&ac e==ac f->item
              |otherwise->VMAssign(VLId n)f
       fix item=item
       declarations=[VMDecl(VVDecl ty r[VVar(mkVId(allNames M.! name n))|VVar n<-vs,S.member(name n)novel])|
        VMDecl(VVDecl ty r vs)<-transparentFlat(vm_body new),any(\v->S.member(name(vvName v))novel)vs]
       assignments=[VMAssign(VLId(mkVId(allNames M.! n)))(rename allNames e)|(n,e)<-M.toList nd,S.member n novel]
       result=VProgram[old{vm_body=mapTransparent fix(vm_body old)++declarations++assignments}]dpis comments
   guard(result/=current)
   return(ASTCandidate "F22" ("Historical pure shell with current typed role cones; preserveBooleanOrder="++show historical)
     result[("historical-pure-shell",[("roles",show(M.toList mapping)),("importedHelpers",show(S.toList novel)),
       ("inactiveHistoricalPureHelpers","retained"),("stateAndNoncombinationalBodies","unchanged"),
       ("proof","required independently against current")])]historical[(o,n)|(n,o)<-M.toList allNames,n/=o,Just(VDReg,_)<-[M.lookup n ns]][])
shellCandidates _ _=[]
