{-# LANGUAGE ScopedTypeVariables #-}
-- Restore live historical packed-output views after pipeline insertion. This
-- module proposes concrete ASTs; ECOASTProof independently validates each one.
module ECOASTLiveAliases (liveAliasProvider,liveAliasCandidates) where

import Control.Monad (guard)
import Data.List (nub)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import ECOASTPreservation (directiveFree,transparentFlat,mapTransparent)
import ECOASTSearch(CandidateProvider(..),candidateProvider)
import ECOASTTransform (ASTCandidate(..),expressionWidth)
import Verilog

type Name=String
type View=(M.Map Name (VDType,Maybe VRange),M.Map Name [VExpr],M.Map Name Int,S.Set Name)
name=getVIdString
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe [] (:[]) (G.cast x)++case G.cast x::Maybe VId of
 Just _->[]
 Nothing->concat (G.gmapQ values x)
refs x=nub [name i|VEVar i<-values x]
outputs m=[name i|(as,_)<-vm_ports m,VAOutput i _<-as]
ports m=[name (vargName a)|(as,_)<-vm_ports m,a<-as]
symbols m=M.fromList
 ([(name i,(t,r))|VMDecl (VVDecl t r vs)<-transparentFlat (vm_body m),VVar i<-vs]++
  [(name (vargName a),(t,r))|(as,_)<-vm_ports m,a<-as,(t,r)<-case a of
    VAInput _ r->[(VDInput,r)];VAOutput _ r->[(VDOutput,r)];_->[]])
definitions m=[(name i,e)|VMAssign (VLId i) e<-transparentFlat (vm_body m)]
lvalueNames (VLId i)=[name i]
lvalueNames (VLConcat xs)=concatMap lvalueNames xs
lvalueNames (VLSub x _)=lvalueNames x
writers m=concat [lvalueNames l|VMAssign l _<-values (vm_body m)]++
 concat [lvalueNames l|s<-values (vm_body m),l<-case s of
   VAssign l _->[l];VAssignA l _->[l];_->[]]
constant (VEConst n)=Just n
constant (VEWConst _ w _ n)|w>0&&n>=0&&n<2^w=Just n
constant _=Nothing

-- Proposal-side role discovery, separate from the proof normalizer. Only
-- complete fields of private, exactly sized, uniquely driven concat wires
-- qualify. No new alias, state, or structural shell is synthesized.
view m=(symbols m,M.fromListWith (++) [(n,[rhs])|(n,rhs)<-definitions m],
        M.fromListWith (+) [(n,1)|n<-writers m],S.fromList (ports m))
fieldOf :: View -> VExpr -> Maybe (Name,Name)
fieldOf (syms,defs,counts,portNames) expression=do
 (owner,hi,lo)<-case expression of
  VESelect (VEVar i) h l->do a<-constant h;b<-constant l;pure (name i,a,b)
  VESelect1 (VEVar i) x->do n<-constant x;pure (name i,n,n)
  _->Nothing
 guard (S.notMember owner portNames&&M.lookup owner counts==Just 1)
 (VDWire,r)<-M.lookup owner syms
 [VEConcat parts]<-M.lookup owner defs
 names<-mapM (\e->case e of VEVar i->Just (name i);_->Nothing) parts
 ws<-mapM (expressionWidth syms) parts
 w<-expressionWidth syms (VEVar (mkVId owner))
 guard (not (null parts)&&all (>0) ws&&sum ws==w)
 guard (case r of Nothing->w==1;Just (h,l)->constant h==Just (w-1)&&constant l==Just 0)
 [(alias,_) ]<-pure [(n,x)|(n,(x,off))<-zip names (zip ws (tail (scanr (+) 0 ws))),lo==off,hi==off+x-1]
 pure (owner,alias)

live m=walk S.empty roots where
 ds=M.fromList (definitions m)
 opaque=[x|x<-transparentFlat (vm_body m),case x of
   VMAssign{}->False;VMDecl{}->False;_->True]
 roots=outputs m++refs opaque
 walk seen []=seen
 walk seen (n:ns)|S.member n seen=walk seen ns
                 |otherwise=walk (S.insert n seen) (maybe [] refs (M.lookup n ds)++ns)

feature="LiveAliases"
liveAliasProvider :: [FilePath] -> CandidateProvider
liveAliasProvider sources=candidateProvider [feature] sources $ \_ enabled budget old current->
 if feature `elem` enabled then liveAliasCandidates budget old current else []

liveAliasCandidates :: Int -> VProgram -> VProgram -> [ASTCandidate]
liveAliasCandidates budget oldP@(VProgram [old] _ _) current@(VProgram [now] ds cs)
 | vm_name old/=vm_name now || vm_ports old/=vm_ports now ||
   not (directiveFree oldP&&directiveFree current)=[]
 | otherwise=take budget [proposal | bundle<-bundles,Just proposal<-[make bundle]]
 where
  oldDefs=M.fromList (definitions old)
  nowDefs=M.fromList (definitions now)
  oldView@(os,_,_,_)=view old
  nowView@(ns,_,counts,_)=view now
  matches=[(output,oldExpr,currentExpr,packed,alias)
    |output<-outputs old,Just oldExpr<-[M.lookup output oldDefs]
    ,Just currentExpr@(VEVar _)<-[M.lookup output nowDefs],oldExpr/=currentExpr
    ,Just (packed,alias)<-[fieldOf oldView oldExpr]
    ,Just role<-[fieldOf nowView oldExpr],role==(packed,alias)
    ,M.lookup packed oldDefs==M.lookup packed nowDefs
    ,alias `notElem` ports now,Just (VDWire,_)<-[M.lookup alias ns]
    ,M.lookup alias os==M.lookup alias ns
    ,M.lookup alias counts==Just 1
    ,Just w<-[expressionWidth ns currentExpr],w>0
    ,expressionWidth ns (VEVar (mkVId alias))==Just w
    ,currentExpr/=VEVar (mkVId alias)
    ,Just before<-[M.lookup alias oldDefs],Just after<-[M.lookup alias nowDefs],before/=after]
  bundles=if null matches then [] else matches:[ [m] |m<-matches,length matches>1]
  make bundle=do
   let seeds=M.fromList [(alias,currentExpr)|(_,_,currentExpr,_,alias)<-bundle]
       restored=M.fromList [(output,oldExpr)|(output,oldExpr,_,_,_)<-bundle]
       reroutes=M.fromList [(owner,rhs)|(owner,e)<-M.toList nowDefs,M.notMember owner oldDefs
         ,Just (_,alias)<-[fieldOf nowView e],M.member alias seeds,Just rhs<-[M.lookup alias nowDefs]]
       reroutedAliases=[alias|(owner,e)<-M.toList nowDefs,M.member owner reroutes
         ,Just (_,alias)<-[fieldOf nowView e]]
   guard (M.size seeds==length bundle)
   guard (S.fromList reroutedAliases==M.keysSet seeds)
   let replacements=M.unions [seeds,restored,reroutes]
       change (VMAssign (VLId i) rhs)=VMAssign (VLId i) (M.findWithDefault rhs (name i) replacements)
       change x=x
       changed=now {vm_body=mapTransparent change (vm_body now)}
   -- Preserve all currently live continuous owners, including those used only
   -- by state/control/effects. This feature never retains newly unused logic.
   guard (live now `S.intersection` M.keysSet nowDefs `S.isSubsetOf` live changed)
   guard (all (`S.member` live changed) (M.keys seeds))
   pure (ASTCandidate feature "Restore live historical output aliases and reconnect new consumers"
     (VProgram [changed] ds cs)
     [("rebind-live-aliases",[(n,"current output value")|n<-M.keys seeds]),
      ("reroute-new-consumers",[(n,"previous alias value")|n<-M.keys reroutes])]
     False [] [])
liveAliasCandidates _ _ _=[]
