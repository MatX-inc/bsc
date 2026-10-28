-- Whole-chain state naming proposals. A typed transfer chain and its input
-- role suggest names only: CURRENT state, transitions and latency are intact.
-- The independent checker must prove every simultaneous rename against NEW.
module ECOASTPipelineChain
 ( PipelineChainProposal(..), PipelineChainAudit(..), pipelineChainProposals
 , pipelineChainAudit, pipelineChainProvider ) where
import Verilog
import VLocalNames(mapRenameableVIds)
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..))
import ECOASTPreservation(directiveFree)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Generics as G
import Data.List(nub,sort)
import Data.Maybe(mapMaybe)
import Control.Monad(guard)

data Group=Group String String [(String,VDType,Maybe VRange)] deriving(Eq,Show)
data Chain=Chain [String] VExpr deriving(Eq,Show)
data PipelineChainProposal=PipelineChainProposal
 { pipelineOldStates :: [String], pipelineCurrentStates :: [String]
 , pipelineForwardNames :: [(String,String)], pipelineModule :: VModule }
 deriving(Eq,Show)
data PipelineChainAudit=PipelineChainAudit
 { pipelineAuditPolicy :: String
 , pipelineGlobalIssues :: [String]
 , pipelineOldStateIssues :: [(String,String)], pipelineCurrentStateIssues :: [(String,String)]
 , pipelinePairOutcomes :: [([String],[String],String)]
 , pipelinePairLimit :: Int, pipelineUnvisitedPairs :: Int
 , pipelineEligible :: Int, pipelineEmitted :: Int, pipelineBudgetLimited :: Bool }
 deriving(Eq,Show)
name=getVIdString
values :: (G.Data a,G.Typeable b)=>a->[b]
values x=maybe[](:[])(G.cast x)++case G.cast x::Maybe VId of
 Just _->[];Nothing->concat(G.gmapQ values x)
ids x=S.fromList[name v|v<-values x]
flat=concatMap(\x->case x of VMGroup _ ys->flat(concat ys);VMComment _ y->flat[y];VMRegGroup _ _ _ y->flat[y];_->[x])
edge(VEE _)=False
edge(VEEOr a b)=edge a||edge b
edge _=True
updates=walk False where
 walk active s=case s of
  Valways x->walk active x
  VAt ev x->walk(edge ev)x
  VSeq xs->concatMap(walk active)xs
  Vif _ x->walk active x
  Vifelse _ x y->walk active x++walk active y
  Vcase _ as _ _->concatMap(\a->walk active(case a of VCaseArm _ x->x;VDefault x->x))as
  VAssignA(VLId v)e|active->[(name v,e)]
  VAssign(VLId v)e|active->[(name v,e)]
  _->[]
unique=M.map head.M.filter((==1).length).M.fromListWith(++) . map(\(k,v)->(k,[v]))
definitions m=unique[(name v,e)|x<-flat(vm_body m),(v,e)<-case x of VMAssign(VLId v)e->[(v,e)];VMDecl(VVDWire _ (VVar v)e)->[(v,e)];_->[]]
width Nothing=Just 1
width(Just(VEConst h,VEConst l))|h>=l&&l>=0=Just(h-l+1)
width _=Nothing
groups m=unique(mapMaybe group(raw(vm_body m))) where
 states=S.fromList[n|VMStmt _ s<-flat(vm_body m),(n,_)<-updates s]
 raw=concatMap(\x->case x of VMRegGroup{}->[x];VMGroup False ys->raw(concat ys);VMComment _ y->raw[y];_->[])
 group(VMRegGroup ident cls _ body)=do
  members<-fmap concat(mapM decl(flat[body]))
  let ss=[n|(n,VDReg,_)<-members,n `S.member`states]
  guard(ss==[name ident]&&length members==S.size(S.fromList[n|(n,_,_)<-members]))
  return(name ident,Group(name ident)cls members)
 group _=Nothing
 decl(VMDecl(VVDecl ty range vs))|ty `elem`[VDReg,VDWire]=do
  _<-width range;mapM(\x->case x of VVar v->Just(name v,ty,range);_->Nothing)vs
 decl _=Nothing
signature(Group state cls ms)=(cls,[(n==state,t,r)|(n,t,r)<-ms])
members(Group _ _ ms)=[n|(n,_,_)<-ms]
stateInput writes defs gs state=do
 group<-M.lookup state gs
 let uses=nub[v|VEVar v<-M.findWithDefault[]state writes,
                name v/=state,name v `elem`members group]
 wire<-case uses of[v]->Just v;_->Nothing
 M.lookup(name wire)defs
-- Expansion stops at state boundaries. Only pure continuous aliases describe
-- a chain connection; no state equality or temporal invariant is assumed.
resolve defs gs seen (VEVar v)
 | name v `M.member`gs=Just(VEVar v)
 | name v `S.member`seen=Nothing
 | Just e<-M.lookup(name v)defs=resolve defs gs(S.insert(name v)seen)e
resolve _ _ _ e=Just e
chains m=mapMaybe build terminals where
 gs=groups m
 defs=definitions m
 writes=M.fromListWith(++)[(n,[e])|VMStmt _ s<-flat(vm_body m),(n,e)<-updates s]
 ins=M.mapMaybeWithKey(\n _->stateInput writes defs gs n >>=resolve defs gs S.empty)gs
 previous=M.fromList[(n,name p)|(n,VEVar p)<-M.toList ins,Just a<-[M.lookup n gs],Just b<-[M.lookup(name p)gs],signature a==signature b]
 next=M.fromListWith(++)[(p,[n])|(n,p)<-M.toList previous]
 terminals=[n|n<-M.keys gs,M.notMember n next]
 build n=do
  ordered<-walk S.empty n
  input<-M.lookup(head ordered)ins
  guard(not(S.null(ids input))&&S.null(ids input `S.intersection`M.keysSet gs))
  return(Chain ordered input)
 walk seen n=do
  guard(n `S.notMember`seen)
  case M.lookup n previous of
   Nothing->Just[n]
   Just p->do guard(M.lookup p next==Just[n]);xs<-walk(S.insert n seen)p;return(xs++[n])
fixed m=S.fromList(name(vm_name m):[name(vargName a)|(as,_)<-vm_ports m,a<-as]) `S.union`
 S.unions[case x of
  VMInst modName inst params ps->S.fromList(map name(modName:inst:either (const []) (map fst) params++map fst ps))
  VMFunction{}->ids x
  _->S.fromList([name i|VTask i _<-values x]++[name i|VEFctCall i _<-values x])
  |x<-flat(vm_body m)]
rename pairs m=m{vm_body=mapRenameableVIds go(vm_body m)} where
 table=M.fromList pairs
 go v@(VId n a b)=maybe v(\s->VId s a b)(M.lookup n table)

pipelineChainProposals :: Int -> VModule -> VModule -> [PipelineChainProposal]
pipelineChainProposals budget old current=snd(analyze budget old current)
pipelineChainAudit :: Int -> VModule -> VModule -> PipelineChainAudit
pipelineChainAudit budget old current=fst(analyze budget old current)
-- Explain states excluded from chain discovery separately from failed chain
-- correspondences. These are structural limitations, never equivalence claims.
stateIssues m=[(n,reason n)|n<-S.toAscList states,n `S.notMember`covered] where
 gs=groups m;defs=definitions m
 writes=M.fromListWith(++)[(n,[e])|VMStmt _ s<-flat(vm_body m),(n,e)<-updates s]
 states=M.keysSet writes
 covered=S.fromList[n|Chain ns _<-chains m,n<-ns]
 raw=M.mapMaybeWithKey(\n _->stateInput writes defs gs n)gs
 ins=M.mapMaybe(resolve defs gs S.empty)raw
 previous=M.fromList[(n,name p)|(n,VEVar p)<-M.toList ins,name p `M.member`gs]
 next=M.fromListWith(++)[(p,[n])|(n,p)<-M.toList previous]
 trace seen n
  | n `S.member`seen="cycle-in-state-transfer-graph"
  | length(M.findWithDefault[]n next)>1="branch-in-state-transfer-graph"
  | Just p<-M.lookup n previous=
      if signature(gs M.!n)/=signature(gs M.!p)then"incompatible-stage-group-shape"
      else trace(S.insert n seen)p
  | M.notMember n raw="missing-or-ambiguous-data-input-owner"
  | M.notMember n ins="cycle-in-continuous-input-aliases"
  | otherwise="head-input-not-a-distinct-nonstate-role"
 reason n|M.notMember n gs="noncanonical-or-nonunique-state-group"
         |otherwise=trace S.empty n
analyze budget old current=(audit,take(max 0 budget)(singles++joint))
 where
  og=groups old;ng=groups current
  -- These predicates depend only on the two modules, not the examined pair.
  -- Share them across up to4096 attempts without changing rejection order.
  fixedPorts=vm_name old==vm_name current&&map fst(vm_ports old)==map fst(vm_ports current)
  commentsSafe=directiveFree(VProgram[old,current][][])
  oldChains=chains old;newChains=chains current
  used=ids current
  declCounts=M.fromListWith(+)[(name v,1::Int)|VMDecl(VVDecl _ _ vs)<-flat(vm_body current),VVar v<-vs]
  compatible(Chain os oi)(Chain ns ni)=oi==ni&&signature(og M.!head os)==signature(ng M.!head ns)
  allPairs=[(o,n)|n<-newChains,o<-oldChains]
  pairCount=length oldChains*length newChains
  examined=take 4096 allPairs
  attempts=[(o,n,attempt(o,n))|(o,n)<-examined]
  singles=[p|(_,_,Right p)<-attempts]
  joint=case singles of
    _:_:_->let pairs=concatMap pipelineForwardNames singles in
      if injective pairs then[PipelineChainProposal(concatMap pipelineOldStates singles)(concatMap pipelineCurrentStates singles)pairs(rename pairs current)]else[]
    _->[]
  injective pairs=let sources=S.fromList(map fst pairs);targets=S.fromList(map snd pairs)in
    length pairs==S.size sources&&length pairs==S.size targets&&S.null((targets `S.difference`sources)`S.intersection`used)
  require b why=if b then Right()else Left why
  attempt(o@(Chain os oi),n@(Chain ns ni))=do
   require fixedPorts "fixed-module-or-ports-differ"
   require commentsSafe "semantic-directive-bearing-AST"
   require(oi==ni)"unmatched-head-input-role"
   require(signature(og M.!head os)==signature(ng M.!head ns))"unmatched-head-type-or-class"
   require(length(filter(`compatible`n)oldChains)==1)"multiple-OLD-chain-matches"
   require(length(filter(compatible o)newChains)==1)"multiple-CURRENT-chain-matches"
   require(length os/=length ns)"equal-chain-length-no-tail-shift"
   -- Existing CURRENT groups outside the historical target set receive the
   -- extra head stages when growing. This is one simultaneous permutation.
   let wanted=drop(max 0(length os-length ns))os
       extras=take(max 0(length ns-length os))[n|n<-ns,n `notElem`wanted]
       target=extras++wanted
       targetGroup n=case M.lookup n og of Just g|n `elem`wanted->g;_->ng M.!n
   require(length target==length ns)"insufficient-vacant-extra-head-groups"
   require(and[signature(ng M.!n)==signature(targetGroup t)|(n,t)<-zip ns target])"incompatible-complete-group-member-shape"
   let pairs0=concat[zip(members(ng M.!n))(members(targetGroup t))|(n,t)<-zip ns target]
       pairs=filter(uncurry(/=))pairs0
   require(not(null pairs)&&injective pairs0)"occupied-target-or-noninjective-map"
   require(all(\n->M.lookup n declCounts==Just 1)(map fst pairs0))"nonunique-source-member-declaration"
   require(S.null(S.fromList(map fst pairs0++map snd pairs0)`S.intersection`fixed current))"protected-module-port-instance-or-callable-identity"
   require(all(vIsValidIdent.snd)pairs0)"invalid-target-identifier"
   require(not(any(\n->any(\s->take(length n+1)s==n++".")(S.toList used))(map fst pairs0++map snd pairs0)))"hierarchical-source-or-target-identity"
   return(PipelineChainProposal os ns pairs(rename pairs current))
  count=length singles+length joint
  audit=PipelineChainAudit "typed-input-role-and-whole-chain-tail;proposal-not-proof"
   (["fixed-module-or-ports-differ"|not fixedPorts]++
    ["semantic-directive-bearing-AST"|not commentsSafe])
   (stateIssues old)(stateIssues current)
   [(os,ns,either id(const"eligible-pending-proof-and-cost")result)|(Chain os _,Chain ns _,result)<-attempts]
   4096(max 0(pairCount-length examined))count(min(max 0 budget)count)
   (pairCount>length examined||count>max 0 budget)

pipelineChainProvider :: CandidateProvider
pipelineChainProvider=CandidateProvider["F25"][]$ \_ features budget (VProgram olds _ _) (VProgram currents ds cs)->
 if "F25" `notElem`features then[]else take budget
  [ASTCandidate "F25" "Align complete typed pipeline chains to historical tail identities"
   (VProgram(take i currents++[pipelineModule p]++drop(i+1)currents)ds cs)
   [("pipeline-chain-tail",[("old",show(pipelineOldStates p)),("current",show(pipelineCurrentStates p)),("names",show(pipelineForwardNames p)),("audit",show audit)])]
   False[(b,a)|(a,b)<-pipelineForwardNames p][]
  |(i,current)<-zip[0..]currents,old<-olds,vm_name old==vm_name current,
   -- Share discovery and its audit; metadata must not repeat all pair scans.
   let (audit,proposals)=analyze budget old current,
   p<-proposals]
