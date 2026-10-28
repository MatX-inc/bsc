-- Historical state names can describe a CURRENT aggregate view after state is
-- split. Matching input packing is naming evidence only, never a relation
-- between OLD and NEW state values, resets, enables, or latencies. The proposal
-- renames one existing wire and retains every CURRENT expression and register.
module ECOASTStateView
  ( StateViewProposal(..), StateViewAudit(..), stateViewProposals
  , stateViewAudit, stateViewProvider ) where
import Verilog
import VLocalNames(mapRenameableVIds)
import ECOASTPreservation(directiveFree,transparentFlat)
import ECOASTSearch(CandidateProvider(..))
import ECOASTTransform(ASTCandidate(..))
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Generics as G
import Data.List(nub)
import Data.Maybe(mapMaybe)
import Control.Monad(guard)

data StateViewProposal = StateViewProposal
 { svSource :: String, svHistorical :: String, svParts :: [String]
 , svModule :: VModule } deriving(Eq,Show)
data StateViewAudit = StateViewAudit
 { svaPolicy :: String, svaOutcomes :: [(String,String,String)]
 , svaOccupiedHistorical :: [String]
 , svaPairLimit :: Int, svaUnvisited :: Int, svaEligible :: Int
 , svaEmitted :: Int, svaBudgetLimited :: Bool } deriving(Eq,Show)
name=getVIdString
ids :: G.Data a => a -> [String]
ids=G.everythingBut(++)(([],False) `G.mkQ` (\v->([name v],True)))
width Nothing=Just 1
width(Just(VEConst h,VEConst 0))|h>=0=Just(h+1)
width _=Nothing
unique :: Ord k => [(k,v)] -> M.Map k v
unique=M.map head.M.filter((==1).length).M.fromListWith(++) . map(\(k,v)->(k,[v]))
declarations m=unique
 [(name i,(ty,r)) | x<-transparentFlat(vm_body m),(ty,r,vs)<-case x of
   VMDecl(VVDecl t r vs)->[(t,r,vs)]
   VMDecl(VVDWire r v _)->[(VDWire,r,[v])]
   _->[],VVar i<-vs]
definitions m=unique
 [(name i,e)|x<-transparentFlat(vm_body m),(i,e)<-case x of
   VMAssign(VLId i)e->[(i,e)]
   VMDecl(VVDWire _ (VVar i)e)->[(i,e)]
   _->[]]
inputs m=unique[(name i,r)|(as,_)<-vm_ports m,VAInput i r<-as]
portNames m=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
edge(VEE _)=False
edge(VEEOr a b)=edge a||edge b
edge _=True
-- All edge writers are recorded, including unsupported lvalues. An aggregate
-- candidate must have no edge-triggered writers; component states must be
-- simple, uniquely declared registers in a canonical retained state group.
writes=walk False where
 walk clocked s=case s of
  Valways x->walk clocked x
  VAt ev x->walk(edge ev)x
  VSeq xs->concatMap(walk clocked)xs
  Vif _ x->walk clocked x
  Vifelse _ a b->walk clocked a++walk clocked b
  Vcase _ as _ _->concatMap(arm clocked)as
  VAssignA l r|clocked->[(n,r)|n<-lhs l]
  VAssign l r|clocked->[(n,r)|n<-lhs l]
  _->[]
 arm c(VCaseArm _ s)=walk c s
 arm c(VDefault s)=walk c s
 lhs(VLId i)=[name i]
 lhs(VLSub x _) = lhs x
 lhs(VLConcat xs)=concatMap lhs xs
stateWrites m=M.fromListWith(++)[(n,[r])|VMStmt False s<-transparentFlat(vm_body m),(n,r)<-writes s]
canonicalStates m=unique(mapMaybe group(raw(vm_body m))) where
 sy=declarations m;sw=stateWrites m
 raw=concatMap go
 go x@(VMRegGroup _ _ _ _)=[x]
 go(VMGroup False xs)=raw(concat xs)
 go(VMComment _ x)=go x
 go _=[]
 group(VMRegGroup i _ _ body)=do
  let xs=transparentFlat[body]
  guard(all isDecl xs)
  let regs=[name v|VMDecl(VVDecl VDReg _ vs)<-xs,VVar v<-vs,M.member(name v)sw]
  guard(regs==[name i])
  (VDReg,r)<-M.lookup(name i)sy
  w<-width r
  return(name i,w)
 group _=Nothing
 isDecl(VMDecl(VVDecl t r vs))=t `elem`[VDReg,VDWire]&&width r/=Nothing&&all simple vs
 isDecl _=False
 simple(VVar _)=True
 simple _=False
-- A finite typed input-packing vocabulary. Expansion stops at fixed input
-- ports. No state, calls, arithmetic, macros, cycles, or unknown widths can
-- establish a transfer role. Constants are canonicalized by bits, not spelling.
data Packing=Input String Integer | Slice Packing Integer Integer | Concat [Packing] | Literal Integer Integer deriving(Eq,Ord,Show)
packingWidth(Input _ w)=w
packingWidth(Slice _ h l)=h-l+1
packingWidth(Concat xs)=sum(map packingWidth xs)
packingWidth(Literal w _)=w
hasInput(Input _ _)=True
hasInput(Slice x _ _)=hasInput x
hasInput(Concat xs)=any hasInput xs
hasInput _=False
packing m=go 64 S.empty where
 sy=declarations m;ds=definitions m;ins=inputs m
 go fuel seen _|fuel<=0=Nothing
 go fuel seen(VEVar i)=case M.lookup(name i)ins of
  Just r->Input(name i) <$> width r
  Nothing->do
   guard(name i `S.notMember` seen)
   (VDWire,r)<-M.lookup(name i)sy
   e<-M.lookup(name i)ds
   x<-go(fuel-1)(S.insert(name i)seen)e
   guard(width r==Just(packingWidth x));return x
 go fuel seen(VEConcat es)=do
  guard(not(null es));xs<-mapM(go(fuel-1)seen)es
  return(Concat(concatMap flatten xs))
 go fuel seen(VESelect e(VEConst h)(VEConst l))=do
  x<-go(fuel-1)seen e
  guard(l>=0&&h>=l&&h<packingWidth x);return(Slice x h l)
 go _ _ (VEWConst _ w _ value)|w>0=Just(Literal w(value `mod` (2^w)))
 go _ _ _=Nothing
 flatten(Concat xs)=xs
 flatten x=[x]
transfer m n=do
 w<-M.lookup n(canonicalStates m)
 rs<-M.lookup n(stateWrites m)
 let choices=nub[x|r<-rs,Just x<-[packing m r],hasInput x,packingWidth x==w]
 case choices of [x]->Just x;_->Nothing
-- No opaque identity, protected subtree, or hierarchical identifier is renamed.
protected m=portNames m `S.union` S.fromList(name(vm_name m):concatMap protect(vm_body m)) where
 protect x@(VMGroup True _)=ids x
 protect x@(VMStmt True _)=ids x
 protect x@(VMFunction _)=ids x
 protect(VMGroup False xs)=concatMap protect(concat xs)
 protect(VMComment _ x)=protect x
 protect(VMRegGroup _ _ _ x)=protect x
 protect(VMInst modName inst params ps)=map name(modName:inst:(either(const[])(map fst)params++map fst ps))
 protect x=G.everythingBut(++)(([],False) `G.mkQ` taskIds `G.extQ` callIds `G.extQ` stopIds)x
 taskIds(VTask i _)=([name i],False)
 taskIds _=([],False)
 callIds(VEFctCall i _)=([name i],False)
 callIds _=([],False)
 stopIds :: VId -> ([String],Bool)
 stopIds _=([],True)
rename a b m=m{vm_body=mapRenameableVIds f(vm_body m)} where
 f v@(VId s source info)|s==a=VId b source info
                       |otherwise=v
stateViewProposals budget old new=snd(propose budget old new)
stateViewAudit budget old new=fst(propose budget old new)
propose budget old new=(audit,take(max 0 budget)eligible) where
 ns=canonicalStates new;os=canonicalStates old;sy=declarations new
 candidates=[(n,parts)|(n,VEConcat es)<-M.toAscList(definitions new),
   let parts=[name i|VEVar i<-es],length parts==length es,length parts>=2]
 allPairs=[(n,parts,o)|(n,parts)<-candidates,o<-M.keys os,o `S.notMember` used]
 pairs=take 4096 allPairs
 used=S.fromList(ids new);fixed=protected new
 fixedPorts=vm_name old==vm_name new&&map fst(vm_ports old)==map fst(vm_ports new)
 commentsSafe=directiveFree(VProgram[old,new][][])
 attempts=[((n,o),attempt n parts o)|(n,parts,o)<-pairs]
 eligible=[p|(_,Right p)<-attempts]
 audit=StateViewAudit "typed-input-packing-role;rename-existing-wire;proof-and-cost-required"
  [(n,o,e)|((n,o),outcome)<-attempts,let e=either id(const "eligible-pending-proof-and-cost")outcome]
  [o|o<-M.keys os,o `S.member` used]
  4096 (max 0(length allPairs-length pairs)) (length eligible) (min(max 0 budget)(length eligible))
  (length allPairs>length pairs||length eligible>max 0 budget)
 attempt n parts o=do
  let require b e=if b then Right()else Left e
  require fixedPorts "fixed-module-or-ports-differ"
  require commentsSafe "directive-bearing-ast"
  require(o `S.notMember` used)"historical-name-not-vacant"
  require(n `S.notMember` fixed&&o `S.notMember` fixed)"protected-identity"
  require(not(any(\u->any(\x->take(length x+1)u==x++".")[n,o])(S.toList used)))"hierarchical-identity-reference"
  require(vIsValidIdent o)"invalid-historical-name"
  require(length parts==S.size(S.fromList parts))"repeated-state-component"
  require(all(`M.member`ns)parts)"noncanonical-state-component"
  require(M.notMember n(stateWrites new)&&M.notMember n ns)"aggregate-is-state"
  r<-case M.lookup n sy of Just(VDWire,r)->Right r;_->Left "aggregate-not-unique-wire"
  ow<-maybe(Left "historical-state-missing")Right(M.lookup o os)
  require(width r==Just ow&&sum[ns M.! p|p<-parts]==ow)"aggregate-width-mismatch"
  before<-maybe(Left "missing-historical-transfer-role")Right(transfer old o)
  after<-maybe(Left "missing-component-transfer-role")Right(mapM(transfer new)parts)
  require(before==Concat(concatMap flatPacking after))"input-packing-role-mismatch"
  return(StateViewProposal n o parts(rename n o new))
 flatPacking(Concat xs)=xs
 flatPacking x=[x]

stateViewProvider :: CandidateProvider
stateViewProvider=CandidateProvider ["F23"] [] $ \_ features budget (VProgram olds _ _) (VProgram currents ds cs)->
 if "F23" `notElem` features then [] else take budget
  [ASTCandidate "F23" "Reuse a vacant historical state name for an existing aggregate view"
   (VProgram(take i currents++[svModule p]++drop(i+1)currents)ds cs)
   [("aggregate-state-view",[("source",svSource p),("historical",svHistorical p),
     ("components",show(svParts p)),("accounting",show(stateViewAudit budget old current))])]
   False [(svHistorical p,svSource p)] []
  |(i,current)<-zip[0..]currents,old<-olds,vm_name old==vm_name current,
   p<-stateViewProposals budget old current]
