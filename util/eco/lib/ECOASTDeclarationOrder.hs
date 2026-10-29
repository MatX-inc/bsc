{-# LANGUAGE CPP #-}
-- OLD supplies name order only. Every moved object, declaration partition,
-- wrapper, comment, width and type is the complete CURRENT object.
module ECOASTDeclarationOrder
 (declarationOrderCandidates,declarationOrderProvider,declarationOrderOnly,declarationOrderReason) where
import Verilog
import ECOASTTransform(ASTCandidate(..))
import ECOASTSearch(CandidateProvider(..),candidateProvider)
import ECOASTPreservation(directiveFree)
import qualified Data.Map.Strict as M
import Data.List(sort,sortOn)
import BinData(encode)
import GenABin()
import Data.Maybe(mapMaybe)

-- Initialized wires, arrays, port declarations and opaque items are barriers.
block (VMDecl(VVDecl t _ vs))=t `elem`[VDWire,VDReg]&&not(null vs)&&all scalar vs
 where scalar VVar{}=True;scalar _=False
block(VMComment _ x)=block x
block(VMRegGroup _ _ _ x)=block x
block(VMGroup False xss)=not(null(concat xss))&&all block(concat xss)
block _=False
leaves(VMComment _ x)=leaves x
leaves(VMRegGroup _ _ _ x)=leaves x
leaves(VMGroup _ xss)=concatMap leaves(concat xss)
leaves x=[x]
plainNames x=[getVIdString(vvName v)|VMDecl(VVDecl _ _ vs)<-leaves x,v<-vs]
allNames m=concatMap names(vm_body m)
 where
 names x=concatMap decl(leaves x)
 decl(VMDecl(VVDecl _ _ vs))=map(getVIdString.vvName)vs
 decl(VMDecl(VVDWire _ v _))=[getVIdString(vvName v)]
 decl _=[]
counts=M.fromListWith(+).map(\n->(n,1::Int))

-- Only scalar declarations reached through transparent containers rank names.
-- An OLD initialized/array/off/opaque declaration cannot supply ordering evidence.
rankedNames=concatMap go where
 go x|block x=plainNames x
 go(VMComment _ x)=go x
 go(VMGroup False xss)=rankedNames(concat xss)
 go _=[]

-- Structural acceptance guard independent of historical ranking: same complete
-- declaration objects in each sibling region, all semantic barriers fixed.
declarationOrderOnly :: VProgram -> VProgram -> Bool
declarationOrderOnly a@(VProgram am ad ac)b@(VProgram bm bd bc)=
 directiveFree a&&directiveFree b&&encode ad==encode bd&&ac==bc&&length am==length bm&&and(zipWith moduleSame am bm)
 where
 moduleSame x y=encode x{vm_body=[]}==encode y{vm_body=[]}&&lists(vm_body x)(vm_body y)
 lists [] []=True
 lists xs@(x:_) ys@(y:_)
  |block x&&block y=let(as,ar)=span block xs;(bs,br)=span block ys
                    in sort(map encode as)==sort(map encode bs)&&lists ar br
 lists(x:xs)(y:ys)=item x y&&lists xs ys
 lists _ _=False
 item(VMComment c x)(VMComment d y)=c==d&&item x y
 item(VMGroup False xs)(VMGroup False ys)=length xs==length ys&&and(zipWith lists xs ys)
 item x y=encode x==encode y

declarationOrderCandidates :: Int -> VProgram -> VProgram -> [ASTCandidate]
declarationOrderCandidates budget oldP@(VProgram[old]_ _)current@(VProgram[now]ds cs)
 |budget<=0||vm_name old/=vm_name now||vm_ports old/=vm_ports now||
  not(directiveFree oldP&&directiveFree current)||any (>1)(M.elems oc++M.elems nc)=[]
 |proposed==current||not(declarationOrderOnly current proposed)=[]
 |otherwise=[ASTCandidate "DeclarationOrder" "Historical order of intact CURRENT declaration blocks" proposed
   [("declaration-block-order",[("identity","unique OLD/CURRENT declaration members"),
     ("partitionsTypesWidthsWrappersComments","unchanged"),("unmatchedBlocks","stable attachment to preceding or first following matched block"),
     ("semanticAndContainerBarriers","fixed"),("proof","independent complete CURRENT equivalence required")])] False[][]]
 where
 oc=counts(allNames old);nc=counts(allNames now)
 unique n=M.lookup n oc==Just 1&&M.lookup n nc==Just 1
 ranks=M.fromList[(n,i)|(i,n)<-zip[0::Int ..](rankedNames(vm_body old)),unique n]
 rank x=case mapMaybe(`M.lookup`ranks)(plainNames x)of []->Nothing;xs->Just(minimum xs)
 -- Unmatched blocks retain their relative order beside a CURRENT anchor.
 -- No OLD object or invented blank-line grouping is introduced.
 arrange run=case mapMaybe rank run of
  []->run
  first:_->map snd(sortOn fst(attach first run))
 attach _ []=[]
 attach previous(x:xs)=let r=maybe previous id(rank x)in(r,x):attach r xs
 walk x|block x=x
 walk(VMComment c x)=VMComment c(walk x)
 walk(VMGroup False xss)=VMGroup False(map reorder xss)
 walk x=x
 reorder []=[]
 reorder xs@(x:rest)
  |block x=let(run,after)=span block xs in arrange run++reorder after
  |otherwise=walk x:reorder rest
 proposed=VProgram[now{vm_body=reorder(vm_body now)}]ds cs
declarationOrderCandidates _ _ _=[]

declarationOrderReason :: VProgram -> VProgram -> String
declarationOrderReason oldP@(VProgram[old]_ _)current@(VProgram[now]_ _)
 |vm_name old/=vm_name now||vm_ports old/=vm_ports now="fixed module interface mismatch"
 |not(directiveFree oldP&&directiveFree current)="protected directive-bearing native comment"
 |not(null duplicates)="duplicate declaration identities: "++show duplicates
 |null matched="no uniquely matched scalar declaration blocks outside protected containers"
 |null(declarationOrderCandidates 1 oldP current)="already ordered within eligible sibling regions; protected semantic and container barriers remain fixed"
 |otherwise="historical declaration-block ordering candidate available"
 where
 oc=counts(allNames old);nc=counts(allNames now)
 duplicates=M.keys(M.filter(>1)(M.unionWith max oc nc))
 matched=[n|n<-rankedNames(vm_body old),M.lookup n oc==Just 1,M.lookup n nc==Just 1]
declarationOrderReason _ _="unsupported module cardinality"

declarationOrderProvider :: CandidateProvider
declarationOrderProvider=candidateProvider["DeclarationOrder"][__FILE__]$ \_ features budget old current->
 if "DeclarationOrder" `elem`features then declarationOrderCandidates budget old current else[]
