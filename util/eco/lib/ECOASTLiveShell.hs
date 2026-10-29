{-# LANGUAGE CPP #-}
-- Untrusted policy: recover OLD live helper roles without retaining unused OLD
-- logic. Every result still passes the ordinary independent semantic gate.
module ECOASTLiveShell(liveShellProvider,liveShellCandidates,projectLiveShell) where
import Verilog
import ECOASTSearch(CandidateProvider(..),candidateProvider)
import ECOASTTransform(ASTCandidate(..))
import ECOASTShell(shellCandidates)
import ECOASTPreservation(directiveFree)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Maybe(mapMaybe)
import Control.Monad(guard)
import System.FilePath((</>),takeDirectory)

name=getVIdString
flat(VMGroup _ gs)=concatMap flat(concat gs)
flat(VMComment _ x)=flat x
flat(VMRegGroup _ _ _ x)=flat x
flat x=[x]
refs :: G.Data a=>a->S.Set String
refs x=S.fromList[name n|VEVar n<-(G.listify(const True)x::[VExpr])]
canon :: G.Data a=>a->a
canon=G.everywhere(G.mkT(\v->mkVId(name v)))
definitions m=[(name n,e)|VMAssign(VLId n)e<-concatMap flat(vm_body m)]
declarations m=[(name n,(t,r))|VMDecl(VVDecl t r vs)<-concatMap flat(vm_body m),VVar n<-vs]

-- Complete-AST roots, including simulation/effect/process uses. This is not
-- translated-only liveness. Non-expression identifiers in opaque executable
-- items are conservatively roots too (e.g. dumpvars and procedural writers).
deadHelpers extra m=M.keysSet ds `S.difference` closure roots where
 items=concatMap flat(vm_body m)
 pairs=definitions m
 ds=M.fromList[(n,refs e)|(n,e)<-pairs]
 nonDefinitions=[x|x<-items,case x of VMAssign(VLId _)_->False;_->True]
 opaque=[x|x<-nonDefinitions,case x of VMDecl{}->False;_->True]
 ports=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
 ids=S.fromList[name i|i<-(G.listify(const True)opaque::[VId])]
 effects=S.fromList[n|(n,e)<-pairs,not(null([()|VEFctCall{}<-(G.listify(const True)e::[VExpr])]))]
 roots=S.unions[extra,ports,refs nonDefinitions,ids,effects]
 closure seen=let next=S.unions(seen:[M.findWithDefault S.empty n ds|n<-S.toList seen])
              in if next==seen then seen else closure next

-- CURRENT's own unused scaffold is not a license to import OLD-only dead
-- logic: preserve it only when each declaration and RHS survives unchanged.
-- If shell mapping changed that scaffold, decline this proposal.
projectLiveShell :: VProgram -> VProgram -> Maybe (VProgram,[String])
projectLiveShell current@(VProgram[new]newDpis _) candidate@(VProgram[m]dpis cs)=do
 guard(directiveFree current&&directiveFree candidate&&null newDpis&&null dpis)
 guard(vm_name new==vm_name m&&vm_ports new==vm_ports m)
 guard(all scalar [d|VMDecl d<-concatMap flat(vm_body m)])
 let nd=definitions new;cd=definitions m;ns=declarations new;ss=declarations m
     preserved=deadHelpers S.empty new
     same n=case([canon e|(k,e)<-nd,k==n],[canon e|(k,e)<-cd,k==n],
                 [canon t|(k,t)<-ns,k==n],[canon t|(k,t)<-ss,k==n])of
       ([a],[b],[t],[u])->a==b&&t==u
       _->False
 guard(all same(S.toList preserved))
 let removed=deadHelpers preserved m
     uniqueWire n=length[e|(k,e)<-cd,k==n]==1&&
       case[t|(k,t)<-ss,k==n]of [(VDWire,_)]->True;_->False
 guard(all uniqueWire(S.toList removed))
 let item(VMDecl(VVDecl ty r vs))=case filter(\x->case x of VVar n->S.notMember(name n)removed;_->True)vs of
       []->Nothing;xs->Just(VMDecl(VVDecl ty r xs))
     item(VMAssign(VLId n)_)|S.member(name n)removed=Nothing
     item(VMComment c x)=VMComment c <$>item x
     item(VMRegGroup n cl c x)=VMRegGroup n cl c <$>item x
     item(VMGroup off gs)=case filter(not.null)(map(mapMaybe item)gs)of
       []->Nothing;xs->Just(VMGroup off xs)
     item x=Just x
     result=VProgram[m{vm_body=mapMaybe item(vm_body m)}]dpis cs
 return(result,S.toList removed)
projectLiveShell _ _=Nothing

scalar(VVDecl _ _ vs)=all (\v->case v of VVar{}->True;_->False) vs
scalar _=False

liveShellCandidates :: VProgram -> VProgram -> [ASTCandidate]
liveShellCandidates old current
 | not(bounded old&&bounded current)=[]
 | otherwise=mapMaybe project(shellCandidates old current)
 where
 bounded(VProgram[m]_ _)=length(take 513(definitions m))<=512
 bounded _=False
 project proposal=do
  (result,removed)<-projectLiveShell current(acProgram proposal)
  guard(result/=current)
  return proposal{acFeature="LiveShell",acProgram=result,
   acDescription="Historical live helper roles; remove disconnected OLD-only pure helpers",
   acOperations=map (\(op,fields)->(op,map (\(k,v)->(k,if k=="inactiveHistoricalPureHelpers" then "removed unless already ordinary CURRENT scaffold" else v)) fields)) (acOperations proposal)++[("live-shell-projection",
    [("removedOldOnlyHelpers",show removed),("ordinaryCurrentUnusedScaffold","unchanged"),
     ("livenessScope","complete AST including simulation and effects"),
     ("proof","required independently against CURRENT")])]}

liveShellProvider :: CandidateProvider
liveShellProvider=candidateProvider["LiveShell"]
 [__FILE__,takeDirectory __FILE__ </> "ECOASTShell.hs"] $ \_ features budget old current->
 if "LiveShell" `elem` features then take(max 0 budget)(liveShellCandidates old current)else[]
