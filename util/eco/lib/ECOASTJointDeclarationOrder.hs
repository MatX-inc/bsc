-- Couple vacant wire identities with internal declaration order. OLD supplies
-- naming/layout evidence only: every executable expression and declaration
-- object comes from CURRENT. Each proposal still needs independent proof.
module ECOASTJointDeclarationOrder
 (jointDeclarationOrderCandidates,jointDeclarationOrderProvider,
  jointDeclarationOrderOnly,jointDeclarationOrderReason) where
import Verilog
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch(CandidateProvider(..))
import ECOASTPreservation(directiveFree)
import ECOASTValueNames(renameValues)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List(sort,sortOn,nub)
import Control.Monad(unless)
import BinData(encode)
import GenABin()

name=getVIdString
need b why=unless b(Left why)
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe[](:[])(G.cast x)++case G.cast x::Maybe VId of
 Just _->[]
 Nothing->concat(G.gmapQ values x)
ids x=[name i|i<-values x::[VId]]
unique xs=length xs==length(nub xs)
flat=concatMap go where
 go(VMComment _ x)=go x
 go(VMRegGroup _ _ _ x)=go x
 go(VMGroup _ xss)=flat(concat xss)
 go x=[x]
-- Port, array, initialized and opaque declarations are not movable leaves.
leaf(VMDecl(VVDecl t _ vs))=t `elem`[VDWire,VDReg]&&not(null vs)&&all scalar vs
 where scalar VVar{}=True;scalar _=False
leaf(VMComment _ x)=leaf x
leaf _=False
members x=case x of
 VMDecl(VVDecl t r vs)|leaf x->Just[(name i,t,r)|VVar i<-vs]
 VMComment _ y->members y
 VMGroup False xss->concat <$> mapM members(concat xss)
 _->Nothing
leafNames x=maybe[](map(\(n,_,_)->n))(members x)
groups=concatMap go where
 go x@(VMRegGroup _ _ _ _)=[x]
 go(VMComment _ x)=go x
 go(VMGroup False xss)=groups(concat xss)
 go _=[]
key(VMRegGroup i _ _ _)=name i
counts=M.fromListWith(+).map(\n->(n,1::Int))
-- No collision with ports, instance interfaces, function/task identities,
-- events, arrays, initializers, declaration bounds, or hierarchical references.
-- VALUE reads in assertions/effect arguments are renamed consistently.
wireSafety (VProgram[_] _ _) []=Right()
wireSafety p@(VProgram[m]ds _) pairs=do
 let xs=flat(vm_body m)
     declarations=[(name(vvName v),t)|VMDecl(VVDecl t _ vs)<-xs,v<-vs]
     sources=map fst pairs;targets=map snd pairs;names=sources++targets
     declCount=counts(map fst declarations)
     allNames=ids p
     opaque x=case x of VMFunction{}->True;_->False
     reserved=S.fromList(ids(vm_ports m)++[name(vm_name m)]++concat
       [ids x|x<-xs,case x of VMInst{}->True;VMDecl(VVDWire{})->True;VMDecl(VVDecl _ _ vs)->any array vs;_->False]++
       concat[ids r|VMDecl(VVDecl _ r _)<-xs]++
       [name i|VEIndex i _<-values p]++[name i|VEFctCall i _<-values p]++[name i|VTask i _<-values p]++
       concat[ids ev|ev<-values p::[VEventExpr]]++
       [name i|VLId i<-values[s|VMStmt _ s<-xs]]++
       [name i|Vdumpvars _ is<-values p,i<-is]++
       [takeWhile(/='.') n|n<-allNames,'.' `elem`n])
     array VVar{}=False;array _=True
     drivers=counts[name i|VMAssign l _<-xs,VLId i<-values l]
     reads=S.fromList[name i|VEVar i<-values p]
 need(null ds&&not(any opaque xs))"function/DPI scope"
 need(unique sources&&unique targets&&all(uncurry(/=))pairs)"noninjective wire correspondence"
 need(S.null(S.fromList sources `S.intersection` S.fromList targets))"overlapping wire identities"
 need(all(\n->n `notElem`allNames)targets)"historical wire identity is occupied"
 need(all(vIsValidIdent)targets)"invalid historical wire identity"
 need(all(\n->M.lookup n declCount==Just 1&&lookup n declarations==Just VDWire)sources)"wire source declaration is absent, nonunique or not a wire"
 need(all(\n->M.lookup n drivers==Just 1&&S.member n reads)sources)"wire source is not live with one continuous driver"
 need(all(`S.notMember`reserved)names)"protected port, instance, event, writer, array, initializer, bound or callable identity"
 need(encode(renameValues[(o,n)|(n,o)<-pairs](renameValues pairs p))==encode p)"incomplete inverse wire rename"
wireSafety _ _=Left"exactly one module required"

-- Independent preservation guard: inverse alpha gives the same serialized
-- CURRENT objects, except scalar declaration permutation inside fixed rows of
-- existing register groups. Top-level order, partitions and wrappers stay fixed.
jointDeclarationOrderOnly :: [(String,String)] -> VProgram -> VProgram -> Bool
jointDeclarationOrderOnly pairs a b=case wireSafety a pairs of
 Left _->False
 Right _->directiveFree a&&directiveFree b&&same a(renameValues[(o,n)|(n,o)<-pairs]b)
 where
 same(VProgram am ad ac)(VProgram bm bd bc)=encode ad==encode bd&&ac==bc&&length am==length bm&&and(zipWith moduleSame am bm)
 moduleSame x y=encode x{vm_body=[]}==encode y{vm_body=[]}&&fixed(vm_body x)(vm_body y)
 fixed xs ys=length xs==length ys&&and(zipWith outer xs ys)
 outer(VMComment c x)(VMComment d y)=c==d&&outer x y
 outer(VMGroup False xs)(VMGroup False ys)=length xs==length ys&&and(zipWith fixed xs ys)
 outer(VMRegGroup n t c x)(VMRegGroup m u d y)=encode n==encode m&&t==u&&c==d&&inside x y
 outer x y=encode x==encode y
 inside(VMComment c x)(VMComment d y)=c==d&&inside x y
 inside(VMGroup False xs)(VMGroup False ys)=length xs==length ys&&and(zipWith row xs ys)
 inside x y=encode x==encode y
 row [] []=True
 row xs@(x:_) ys@(y:_)|leaf x&&leaf y=
  let(as,ar)=span leaf xs;(bs,br)=span leaf ys
  in sort(map encode as)==sort(map encode bs)&&row ar br
 row(x:xs)(y:ys)=inside x y&&row xs ys
 row _ _=False

proposal oldP@(VProgram[old]_ _)current@(VProgram[now]ds cs)=do
 need(vm_name old==vm_name now&&vm_ports old==vm_ports now)"fixed module interface mismatch"
 need(directiveFree oldP&&directiveFree current)"protected directive-bearing comment"
 let og=groups(vm_body old);ng=groups(vm_body now)
     declNames m=[name(vvName v)|x<-flat(vm_body m),vs<-case x of VMDecl(VVDecl _ _ vs)->[vs];VMDecl(VVDWire _ v _)->[[v]];_->[],v<-vs]
 need(all (unique . declNames)[old,now])"duplicate OLD/CURRENT declaration identities"
 need(unique(map key og)&&unique(map key ng))"duplicate surviving register-group identity"
 let attempt n@(VMRegGroup state cls _ body)=case [o|o<-og,key o==key n]of
      [VMRegGroup _ oldClass _ oldBody]->do
       os<-maybe(Left"protected OLD declaration group")Right(members oldBody)
       ns<-maybe(Left"protected CURRENT declaration group")Right(members body)
       need(cls==oldClass)"surviving register class differs"
       need(unique[n|(n,_,_)<-os]&&unique[n|(n,_,_)<-ns])"duplicate declaration group member"
       need([r|(n,VDReg,r)<-os,n==name state]==[r|(n,VDReg,r)<-ns,n==name state]&&
            length[n|(n,VDReg,_)<-os,n==name state]==1&&length[n|(n,VDReg,_)<-ns,n==name state]==1&&
            length[n|(n,VDReg,_)<-os]==1&&length[n|(n,VDReg,_)<-ns]==1)"surviving state shape differs"
       let common=[n|(n,_,_)<-ns,n `elem`[o|(o,_,_)<-os]]
           shape xs n=[(t,r)|(x,t,r)<-xs,x==n]
           absent xs ys=[x|x@(n,_,_)<-xs,n `notElem`[m|(m,_,_)<-ys]]
           missingOld=absent os ns;missingNow=absent ns os
       need(all(\n->shape os n==shape ns n)common)"matched member type/range differs"
       need(length missingOld==length missingNow)"declaration membership differs"
       pairs<-mapM(\(n,t,r)->case[o|(o,u,q)<-missingOld,t==VDWire,u==VDWire,r==q]of
         [o]->Right(n,o);_->Left"ambiguous or incompatible unmatched wire type/range")missingNow
       need(unique(map snd pairs))"ambiguous unmatched wire roles"
       pure(key n,pairs,oldBody)
      _->Left"no surviving OLD register-group identity"
     checked n=do
      result@(_,ps,_)<-attempt n
      wireSafety current ps
      pure result
     attempts=[(key n,checked n)|n<-ng]
     eligible=[v|(_,Right v)<-attempts]
     pairs=concat[ps|(_,ps,_)<-eligible]
     rejected=[(n,why)|(n,Left why)<-attempts]
 need(not(null eligible))("no compatible surviving declaration group; "++show rejected)
 wireSafety current pairs
 let renamed=renameValues pairs current
     ranks oldBody=M.fromList[(n,i)|(i,n)<-zip[0::Int ..](leafNames oldBody)]
     ordered oldBody=walk where
      rank x=minimum [M.findWithDefault maxBound n(ranks oldBody)|n<-leafNames x]
      row []=[]
      row xs@(x:rest)|leaf x=let(run,after)=span leaf xs in sortOn rank run++row after
                        |otherwise=walk x:row rest
      walk(VMComment c x)=VMComment c(walk x)
      walk(VMGroup False xss)=VMGroup False(map row xss)
      walk x=x
     rewrite(VMRegGroup n t c body)=case [o|(g,_,o)<-eligible,g==name n]of
      [o]->VMRegGroup n t c(ordered o body)
      _->VMRegGroup n t c body
     rewrite(VMComment c x)=VMComment c(rewrite x)
     rewrite(VMGroup False xss)=VMGroup False(map(map rewrite)xss)
     rewrite x=x
     proposed=case renamed of VProgram[m]d c->VProgram[m{vm_body=map rewrite(vm_body m)}]d c
 need(jointDeclarationOrderOnly pairs current proposed)"serialized declaration/nondeclaration preservation guard failed"
 need(encode proposed/=encode current)("already uses selected wire identities and order; declined groups: "++show rejected)
 pure(pairs,proposed,rejected)
proposal _ _=Left"exactly one module required"

jointDeclarationOrderCandidates budget old current
 |budget<=0=[]
 |otherwise=case proposal old current of
  Left _->[]
  Right(pairs,p,rejected)->[ASTCandidate "JointDeclarationOrder"
   "Unique vacant wire identities within surviving state groups, jointly with internal declaration order" p
   [("surviving-state-wire-identities",pairs),("internal-declaration-order",[("objectsPartitionsWrappersWidthsTypes","unchanged modulo wire names"),("stateAndExecutableExpressions","CURRENT only")]),("declined-register-groups",rejected)]
   False[(o,n)|(n,o)<-pairs]]
jointDeclarationOrderReason old current=either id(const"joint wire-identity/internal-order candidate available")(proposal old current)
jointDeclarationOrderProvider=CandidateProvider["JointDeclarationOrder"][]$ \features budget old current->
 if "JointDeclarationOrder" `elem`features then jointDeclarationOrderCandidates budget old current else[]
