-- Untrusted proposals. Reorient an existing alias to restore an OLD helper's
-- definition while preserving every CURRENT declaration, consumer and value.
module ECOASTAliasOrientation(aliasOrientationProvider,aliasOrientationCandidates) where
import Verilog
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..),expressionWidth)
import ECOASTPreservation(directiveFree,transparentFlat,mapTransparent)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S

name=getVIdString
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe[](:[])(G.cast x)++case G.cast x::Maybe VId of
 Just _->[]
 Nothing->concat(G.gmapQ values x)
refs x=S.fromList[name n|VEVar n<-values x]
unique=M.map head.M.filter((==1).length).M.fromListWith(++)
definitions m=unique[(name n,[e])|VMAssign(VLId n)e<-transparentFlat(vm_body m)]
-- Count declarations and all explicit writers, including opaque/translated-off
-- scopes. Child pin directions remain independently checked proof obligations.
bodyDeclarations m=M.fromListWith(++)[(name(vvName v),[(t,r,scalar v)])|
 VMDecl(VVDecl t r vs)<-values(vm_body m),v<-vs]where
 scalar VVar{}=True
 scalar _=False
symbols m=M.union (M.mapMaybe scalar(bodyDeclarations m)) (M.fromList
 [(name(vargName a),(t,r))|(as,_)<-vm_ports m,a<-as,(t,r)<-case a of
   VAInput _ r->[(VDInput,r)];VAOutput _ r->[(VDOutput,r)];_->[]])where
 scalar[(t,r,True)]=Just(t,r)
 scalar _=Nothing
lvalue(VLId n)=[name n]
lvalue(VLConcat xs)=concatMap lvalue xs
lvalue(VLSub x _)=lvalue x
writers m=M.fromListWith(+)([(n,1::Int)|VMAssign l _<-values(vm_body m),n<-lvalue l]++
 [(n,1)|s<-values(vm_body m),l<-case s of VAssign l _->[l];VAssignA l _->[l];_->[],n<-lvalue l]++
 [(name(vvName v),1)|VMDecl(VVDWire _ v _)<-values(vm_body m)])
ports m=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
nonAlias VEVar{}=False
nonAlias _=True
canonical e=case e of
 VEVar n->VEVar(mkVId(name n))
 VEWConst _ w _ n->VEWConst defaultVId w 10 n
 VEOp _ a o b->VEOp defaultVId(canonical a)o(canonical b)
 VEUnOp _ o a->VEUnOp defaultVId o(canonical a)
 VEConcat xs->VEConcat(map canonical xs)
 VEIf c a b->VEIf(canonical c)(canonical a)(canonical b)
 VESelect x h l->VESelect(canonical x)(canonical h)(canonical l)
 VESelect1 x i->VESelect1(canonical x)(canonical i)
 VERepeat n x->VERepeat(canonical n)(canonical x)
 _->e
pureExpression e=case e of
 VEVar{}->True;VEConst{}->True;VEWConst _ w _ _->w>0
 VEOp _ a _ b->both[a,b];VEUnOp _ _ a->both[a]
 VEConcat xs->not(null xs)&&both xs
 VEIf c a b->both[c,a,b];VESelect x h l->both[x,h,l]
 VESelect1 x i->both[x,i];VERepeat n x->both[n,x]
 _->False
 where both=all pureExpression

aliasOrientationCandidates :: Int -> VProgram -> VProgram -> [ASTCandidate]
aliasOrientationCandidates budget oldP@(VProgram[old]_ _) current@(VProgram[now]ds cs)
 | budget<=0||vm_name old/=vm_name now||vm_ports old/=vm_ports now||
   not(directiveFree oldP&&directiveFree current)=[]
 | otherwise=take budget(map proposal bundles)
 where
 od=definitions old;nd=definitions now;os=symbols old;ns=symbols now
 counts=writers now;decls=bodyDeclarations now;fixed=ports now
 eligible n=S.notMember n fixed&&M.lookup n counts==Just 1&&
   case M.lookup n decls of Just[(VDWire,_,True)]->True;_->False
 sameWidth a b e=case expressionWidth ns(VEVar(mkVId a))of
   Just w->w>0&&expressionWidth ns(VEVar(mkVId b))==Just w&&expressionWidth ns e==Just w&&M.lookup a os==M.lookup a ns
   _->False
 dependencies seen pending=case S.minView pending of
   Nothing->seen
   Just(n,rest)|S.member n seen->dependencies seen rest
               |otherwise->dependencies(S.insert n seen)(rest `S.union` maybe S.empty refs(M.lookup n nd))
 pairs=[(alias,target,rhs)|(alias,VEVar targetId)<-M.toAscList nd,let target=name targetId,alias/=target,
   Just prior<-[M.lookup alias od],nonAlias prior,
   Just rhs<-[M.lookup target nd],nonAlias rhs,canonical prior==canonical rhs,
   eligible alias,eligible target,pureExpression rhs,sameWidth alias target rhs,
   S.null(S.fromList[alias,target] `S.intersection` dependencies S.empty(refs rhs))]
 uses=M.fromListWith(+)[(n,1::Int)|(a,b,_)<-pairs,n<-[a,b]]
 joint=[p|p@(a,b,_)<-pairs,M.lookup a uses==Just 1,M.lookup b uses==Just 1]
 bundles=[joint|length joint>1]++map(:[])pairs
 proposal selected=ASTCandidate "AliasOrientation" "Restore exact OLD helper definition by reorienting an existing CURRENT alias"
  (VProgram[now{vm_body=mapTransparent change(vm_body now)}]ds cs)
  [("orient-existing-alias",[(a,b)|(a,b,_)<-selected]),
   ("preservation",[("declarationsConsumersStateAndControl","unchanged"),("values","CURRENT RHS transferred at identical width"),("proof","independent complete CURRENT equivalence required")])]
  False[]
  where
   changes=M.fromList(concat[[(a,rhs),(b,VEVar(mkVId a))]|(a,b,rhs)<-selected])
   change item@(VMAssign(VLId n)_)=maybe item(VMAssign(VLId n))(M.lookup(name n)changes)
   change item=item
aliasOrientationCandidates _ _ _=[]

aliasOrientationProvider :: CandidateProvider
aliasOrientationProvider=CandidateProvider["AliasOrientation"][]$ \features budget old current->
 if "AliasOrientation" `elem` features then aliasOrientationCandidates budget old current else[]
