-- Optional preservation of OLD inactive constant helper definitions. This does
-- not infer a functional invariant or change any CURRENT fanin or state.
module ECOASTInactive(inactiveProvider,inactiveCandidates) where
import Verilog
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..))
import ECOASTPreservation
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(nub,intersect,isInfixOf)
import Data.Char(toLower)
name=getVIdString
flat=concatMap f where
 f(VMGroup _ xs)=flat(concat xs)
 f(VMComment _ x)=f x
 f(VMRegGroup _ _ _ x)=f x
 f x=[x]
assigned x=[name n|VMAssign(VLId n)_<-flat[x]]
safeComments x=not(any(\c->any(`isInfixOf`map toLower c)["pragma","synthesis","synopsys","attribute","translate_","`","(*"])
 [c|VMComment cs _<-G.listify(const True)x,c<-cs])
inactiveProvider = CandidateProvider ["F22Inactive"] [] $ \_ fs budget old current->
 if "F22Inactive"`elem`fs then take budget(inactiveCandidates old current)else[]
inactiveCandidates oldProgram@(VProgram[old]_ _) current@(VProgram[new]ds cs)
 |not(directiveFree oldProgram&&directiveFree current)=[]
 |vm_name old/=vm_name new||vm_ports old/=vm_ports new=[]
 |null wanted=[]
 |otherwise=[ASTCandidate "F22Inactive" "Optional historical inactive constant helpers at original declaration and assignment groups"
   (VProgram[new{vm_body=foldl(\body(anchor,item)->insert anchor item body)(mapDecl(vm_body new))placements}]ds cs)
   [("restore-inactive-constant-helpers",[("names",show wanted),("normalNewFunctionsAndState","unchanged"),
     ("historicalStructurePolicy","optional"),("proof","independent required")])]False[][]]
 where
  oldLeaves=transparentFlat(vm_body old);newLeaves=transparentFlat(vm_body new)
  namespace=S.fromList[name v|v<-G.listify(const True)new::[VId]]
  oldDecls=[(t,r,vs)|VMDecl(VVDecl t r vs)<-oldLeaves]
  newDecls=[(t,r,vs)|VMDecl(VVDecl t r vs)<-newLeaves]
  member ns=map(name.vvName)ns
  declared n=or[t==VDWire&&n`elem`member vs&&or[u==t&&s==r&&not(null(member vs`intersect`member ws))|(u,s,ws)<-newDecls]
   |(t,r,vs)<-oldDecls]
  constant e=null[()|VEVar _<-xs]&&null[()|VEFctCall _ _<-xs]&&null[()|VEMacro _<-xs]&&null[()|VEIndex _ _<-xs]
   where xs=G.listify(const True)e::[VExpr]
  eligible=[name n|VMAssign(VLId n)e<-oldLeaves,S.notMember(name n)namespace,declared(name n),constant e]
  groups=concatMap collect(vm_body old)
  collect x|not(null(transparentFlat[x]))&&all one(transparentFlat[x])&&safeComments x=[x]
   where one(VMAssign(VLId n) _)=name n`elem`eligible;one _=False
  collect(VMGroup False xss)=concatMap collect(concat xss)
  collect(VMComment _ x)=collect x
  collect _=[]
  ranks=M.fromList[(name n,i)|(i,VMAssign(VLId n)_)<-zip[0::Int ..]oldLeaves]
  next x=case[n|(VMAssign(VLId v)_)<-oldLeaves,let n=name v,M.findWithDefault 0 n ranks>maximum[M.findWithDefault 0 k ranks|k<-assigned x],S.member n namespace]of
   n:_->Just n;_->Nothing
  placements=[(anchor,x)|x<-groups,Just anchor<-[next x]]
  wanted=concatMap(assigned.snd)placements
  update(VMDecl(VVDecl t r vs))=case[ovs|(u,s,ovs)<-oldDecls,u==t,s==r,
      not(null(member vs`intersect`member ovs)),all(`elem`member ovs)(member vs),any(`elem`wanted)(member ovs)]of
   [ovs]->VMDecl(VVDecl t r[v|v<-ovs,name(vvName v)`elem`(member vs++wanted)])
   _->VMDecl(VVDecl t r vs)
  update x=x
  mapDecl=mapTransparent update
  insert anchor item=concatMap go where
   go x|not(null(assigned x))&&head(assigned x)==anchor=[item,x]
   go(VMGroup False xss)=[VMGroup False(map(insert anchor item)xss)]
   go(VMComment c x)=[VMComment c(wrap(insert anchor item[x]))]
   go x=[x]
   wrap[x]=x
   wrap xs=VMGroup False[xs]
inactiveCandidates _ _=[]
