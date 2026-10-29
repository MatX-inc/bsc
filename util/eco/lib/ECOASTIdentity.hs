-- Bounded proposals for historical state identities, using retained typed ASTs.
-- An output-role match is naming evidence, never an assertion that two pipeline
-- stage values are equal. No register, transition, initial value, or enable is
-- removed or changed: each candidate is an injective simultaneous local rename.
-- The caller must prove the candidate against current NEW with the inverse
-- relation, then use independently measured cost to decide whether to accept it.
-- No source text, rendered RTL, direct reference, or suffix convention is used.
module ECOASTIdentity
  ( StateRolePermutation(..), stateRolePermutations, stateRoleAlternatives
  , VacancyAudit(..), vacancyAudit
  ) where
import Verilog
import VLocalNames(mapRenameableVIds)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.Generics as G
import Data.List(sortOn,nub)
import Data.Maybe(mapMaybe)
import Control.Monad(guard)

data Group=Group String String [(String,VDType,Maybe VRange)] deriving Show
-- Forward names are current -> candidate.  Proof APIs expecting candidate ->
-- current must invert this list. Consumer names are sorted, unique output ports;
-- each complete matched output contributes at most one vote per state pair.
data StateRolePermutation = StateRolePermutation
  { srpSourceState :: String
  , srpHistoricalState :: String
  , srpOutputConsumers :: [String]
  , srpForwardNames :: [(String,String)]
  , srpModule :: VModule
  } deriving (Eq,Show)

-- At most the requested number of separate two-group permutations, ranked by
-- descending output support, then source and historical names. This function
-- preserves CURRENT's body ordering and all control/scaffolding structure.
stateRolePermutations :: Int -> VModule -> VModule -> [StateRolePermutation]
stateRolePermutations bound old current = snd (propose bound old current)

name :: VId -> String
name=getVIdString
flat :: [VMItem] -> [VMItem]
flat=concatMap f where
 f(VMGroup _ xs)=flat(concat xs)
 f(VMComment _ x)=f x
 f(VMRegGroup _ _ _ x)=f x
 f x=[x]
lhs :: VLValue -> [String]
lhs(VLId v)=[name v]
lhs(VLConcat xs)=concatMap lhs xs
lhs(VLSub x _) = lhs x
edge :: VEventExpr -> Bool
edge(VEE _)=False
edge(VEEOr a b)=edge a||edge b
edge _=True
edgeWrites :: VStmt -> [String]
edgeWrites=walk False where
 walk e s=case s of
  Valways x->walk e x
  VAt ev x->walk(edge ev)x
  VSeq xs->concatMap(walk e)xs
  Vif _ x->walk e x
  Vifelse _ a b->walk e a++walk e b
  Vcase _ as _ _->concatMap(arm e)as
  VAssignA l _|e->lhs l
  VAssign l _|e->lhs l
  _->[]
 arm e(VCaseArm _ s)=walk e s
 arm e(VDefault s)=walk e s
width :: Maybe VRange -> Maybe Integer
width Nothing=Just 1
width(Just(VEConst h,VEConst l))|l==0&&h>=0=Just(h+1)
width _=Nothing
members :: [VMItem] -> Maybe [(String,VDType,Maybe VRange)]
members body=do
 leaves<-fmap concat(mapM onlyDeclarations body)
 fmap concat(mapM decl leaves)
 where
 onlyDeclarations(VMGroup _ xs)=fmap concat(mapM onlyDeclarations(concat xs))
 onlyDeclarations(VMComment _ x)=onlyDeclarations x
 onlyDeclarations x@(VMDecl _) = Just[x]
 onlyDeclarations _=Nothing
 decl(VMDecl(VVDecl ty r vs))|ty `elem` [VDReg,VDWire]=mapM (one ty r) vs
 decl _=Nothing
 one ty r(VVar i)=do _<-width r;return(name i,ty,r)
 one _ _ _=Nothing
getGroups :: VModule -> M.Map String Group
getGroups m=M.map head.M.filter((==1).length).M.fromListWith(++)$mapMaybe group(raw(vm_body m)) where
 states=S.fromList(concat[edgeWrites s|VMStmt _ s<-flat(vm_body m)])
 raw=concatMap go
 go x@(VMRegGroup _ _ _ _)=[x]
 go(VMGroup _ xs)=raw(concat xs)
 go(VMComment _ x)=go x
 go _=[]
 group(VMRegGroup ident cls _ body)=do
  ms<-members[body]
  let ns=[n|(n,_,_)<-ms]; ss=[n|(n,VDReg,_)<-ms,n `S.member` states]
  guard(length ss==1 && name ident==head ss && length ns==S.size(S.fromList ns))
  return(head ss,[Group(head ss)cls ms])
 group _=Nothing
sig(Group s c ms)=(c,[(n==s,t,r)| (n,t,r)<-ms])
gwidth(Group s _ ms)=head[width r|(n,_,r)<-ms,n==s]
classOf(Group _ c _)=c
ports m=S.fromList[name(vargName a)|(as,_)<-vm_ports m,a<-as]
outputs m=M.fromList[(name v,r)|(as,_)<-vm_ports m,VAOutput v r<-as]
definitions m=M.map head.M.filter((==1).length).M.fromListWith(++)$
 [(name i,[e])|x<-flat(vm_body m),(i,e)<-case x of VMAssign(VLId i)e->[(i,e)];VMDecl(VVDWire _ (VVar i)e)->[(i,e)];_->[]]
-- Local leaf types include packed and unpacked declaration ranges. Ambiguous
-- or absent types do not support a nonstate-name match.
leafTypes m=M.map head.M.filter(\xs->length(nub xs)==1).M.fromListWith(++)$
 [(name i,[(r,ar)])|(i,r,ar)<-decls++portDecls] where
  var(VVar i)=(i,Nothing)
  var(VArray ar i)=(i,Just ar)
  decls=[(i,r,ar)|x<-flat(vm_body m),(r,vs)<-case x of
          VMDecl(VVDecl _ r vs)->[(r,vs)]
          VMDecl(VVDWire r v _)->[(r,[v])]
          _->[],v<-vs,let(i,ar)=var v]
  portDecls=[(vargName a,r,Nothing)|(as,_)<-vm_ports m,a<-as,r<-case a of
          VAInput _ r->[r];VAOutput _ r->[r];VAParameter _ r _ _->[r];VAInout _ _ (Just r)->[r];_->[]]
-- Exact operator/literal/slice/nonstate-leaf structure; only corresponding
-- same-width state occurrences become vote holes. Transactional Maybe drops
-- every tentative vote if any later expression member differs.
align og ng ot nt a b=do
 ps<-go 512 a b
 let bySource=M.fromListWith S.union[(n,S.singleton o)|(n,o)<-ps]
     byTarget=M.fromListWith S.union[(o,S.singleton n)|(n,o)<-ps]
 guard(all((==1).S.size)(M.elems bySource++M.elems byTarget))
 return ps
 where
 go fuel a b|fuel<=0=Nothing
 go fuel (VEVar a)(VEVar b)=case(M.lookup(name a)og,M.lookup(name b)ng)of
  (Just x,Just y)->do guard(gwidth x==gwidth y&&classOf x==classOf y);return[(name b,name a)]
  (Nothing,Nothing)->do guard(name a==name b);t<-M.lookup(name a)ot;guard(M.lookup(name b)nt==Just t);return[]
  _->Nothing
 go fuel (VEUnOp _ x a)(VEUnOp _ y b)=guard(x==y)>>go(fuel-1)a b
 go fuel (VEOp _ a x b)(VEOp _ c y d)=guard(x==y)>>combine fuel[a,b][c,d]
 go fuel (VEConcat xs)(VEConcat ys)=combine fuel xs ys
 go fuel (VESelect a h l)(VESelect b j k)=combine fuel[a,h,l][b,j,k]
 go fuel (VESelect1 a i)(VESelect1 b j)=combine fuel[a,i][b,j]
 go fuel (VERepeat a n)(VERepeat b m)=combine fuel[a,n][b,m]
 go fuel (VEIf c a b)(VEIf d x y)=combine fuel[c,a,b][d,x,y]
 go _ (VEWConst _ w b x)(VEWConst _ z c y)=guard((w,b,x)==(z,c,y))>>return[]
 go _ a b=guard(a==b)>>return[]
 combine fuel xs ys=do guard(length xs==length ys);fmap concat(sequence[go(fuel-1)x y|(x,y)<-zip xs ys])
semanticIds::G.Data a=>a->[VId]
semanticIds=G.everythingBut(++)(([],False) `G.mkQ` (\v->([v],True)))
protected m=ports m `S.union` S.fromList
 (name(vm_name m):[name i|VMInst _ i _ _<-xs]++
  [name i|VMInst modName _ params ps<-xs,i<-modName:(map fst params ++ map fst ps)]++
  [name i|x<-xs,case x of VMFunction _->True;_->False,i<-semanticIds x]++
  G.everythingBut(++) (([],False) `G.mkQ` taskIds `G.extQ` callIds `G.extQ` stopIds) xs)
 where xs=flat(vm_body m)
       taskIds(VTask i _)=([name i],False)
       taskIds _=([],False)
       callIds(VEFctCall i _)=([name i],False)
       callIds _=([],False)
       stopIds :: VId -> ([String],Bool)
       stopIds _=([],True)
applyNames :: [(String,String)] -> VModule -> VModule
applyNames pairs m = m { vm_body=mapRenameableVIds rename(vm_body m) }
 where
 mapping=M.fromList pairs
 rename v@(VId spelling source info)=
   maybe v(\target->VId target source info)(M.lookup spelling mapping)
-- Initial bounded family: two-group permutations only. The retained incumbent
-- must have no output-role vote or a uniquely reciprocal desired role.
propose budget old new=(audit,take (max 0 budget) accepted) where
 og=getGroups old;ng=getGroups new
 od=definitions old;nd=definitions new
 ot=leafTypes old;nt=leafTypes new
 common=[n|(n,r)<-M.toAscList(outputs old),M.lookup n(outputs new)==Just r]
 outputVotes=[(out,n,o)|out<-common,Just a<-[M.lookup out od],Just b<-[M.lookup out nd],Just ps<-[align og ng ot nt a b],(n,o)<-nub ps]
 votes=M.fromListWith S.union[((n,o),S.singleton out)|(out,n,o)<-outputVotes]
 sourceVotes n=[(o,S.size os)|((n',o),os)<-M.toList votes,n==n']
 targetVotes o=[(n,S.size os)|((n,o'),os)<-M.toList votes,o==o']
 unique []=Nothing
 unique xs=case sortOn(negate.snd)xs of (n,k):rest|all((<k).snd)rest->Just n;_->Nothing
 desired=[(n,o,S.size os,S.toAscList os)|((n,o),os)<-M.toList votes,n/=o,unique(sourceVotes n)==Just o,unique(targetVotes o)==Just n]
 fixed=protected new
 usedIds=S.fromList(map name(semanticIds(vm_body new)))
 declCounts=M.fromListWith (+)
   [(name(vvName v),1::Int)|x<-flat(vm_body new),vs<-case x of
      VMDecl(VVDecl _ _ xs)->[xs];VMDecl(VVDWire _ x _)->[[x]];_->[],v<-vs]
 attempt(n,o,k,os)=do
  from<-maybe(Left "source-not-canonical-state-group")Right(M.lookup n ng)
  target<-maybe(Left "historical-name-is-not-current-state-group")Right(M.lookup o ng)
  if sig from/=sig target then Left "incompatible-populated-group-shape" else Right()
  if null(sourceVotes o)||(unique(sourceVotes o)==Just n && unique(targetVotes n)==Just o) then Right() else Left "displaced-state-has-conflicting-output-role"
  let Group _ _ fm=from;Group _ _ tm=target
      fs=[x|(x,_,_)<-fm];ts=[x|(x,_,_)<-tm]
      pairs=zip fs ts++zip ts fs
      ss=S.fromList(map fst pairs);tt=S.fromList(map snd pairs)
  if S.null(S.fromList fs `S.intersection` S.fromList ts)&&length pairs==S.size ss&&length pairs==S.size tt&&ss==tt then Right() else Left "overlap-or-name-collision"
  if all(\s->M.lookup s declCounts==Just 1)(S.toList ss) then Right() else Left "nonunique-declaration-collision"
  if S.null(ss `S.intersection` fixed) then Right() else Left "fixed-identity-collision"
  if all(vIsValidIdent.snd)pairs then Right() else Left "invalid-target-name"
  if any(\x->any(\u->take(length x+1)u==x++".")(S.toList usedIds))(S.toList ss) then Left "hierarchical-reference-to-permuted-identity" else Right()
  return(StateRolePermutation n o os pairs(applyNames pairs new))
 attempts=[((n,o),attempt e)|e@(n,o,_,_)<-sortOn(\(n,o,k,_)->(-k,n,o))desired]
 accepted=dedup S.empty[p|(_,Right p)<-attempts]
 dedup _ []=[]
 dedup seen(p:ps)|key `S.member` seen=dedup seen ps
                  |otherwise=p:dedup(S.insert key seen)ps
  where key=S.fromList[srpSourceState p,srpHistoricalState p]
 audit=([(n,o,S.size os,S.toAscList os)|((n,o),os)<-M.toList votes],[(n++" -> "++o,e)|((n,o),Left e)<-attempts])

-- At most the requested number of separate complete-group renames, ranked by
-- descending output support, then source and historical names. This function
-- preserves CURRENT's body ordering and all control/scaffolding structure.
stateRoleAlternatives :: Int -> VModule -> VModule -> [StateRolePermutation]
stateRoleAlternatives bound old current = snd (proposeVacancy bound old current)
data VacancyAudit=VacancyAudit
 { vaPolicy :: String, vaOutputVotes :: [(String,String,Int,[String])]
 , vaPairOutcomes :: [(String,String,String,String)]
 , vaPairLimit :: Int, vaUnvisitedPairs :: Int, vaProposalBudget :: Int
 , vaEligible :: Int, vaEmitted :: Int, vaBudgetLimited :: Bool
 } deriving (Eq,Show)
vacancyAudit bound old current=fst(proposeVacancy bound old current)


-- A projected fixed output can identify the payload role after a packed state
-- is split. This is only naming evidence: CURRENT's register class, range,
-- initialization, transitions and controls remain untouched. A complete output
-- must select an in-range constant slice of OLD state and read whole NEW state
-- of precisely the same width. No suffix, source name or reset policy is used.
projectionVotes og ng old new =
 [(out,name n,name o) |
  (out,r)<-M.toAscList(outputs old),M.lookup out(outputs new)==Just r,
  Just(VESelect(VEVar o)(VEConst h)(VEConst l))<-[M.lookup out(definitions old)],
  Just(VEVar n)<-[M.lookup out(definitions new)],
  Just a<-[M.lookup(name o)og],Just b<-[M.lookup(name n)ng],
  Just ow<-[gwidth a],Just nw<-[gwidth b],
  l>=0,h>=l,h<ow,nw==h-l+1,nw<ow,width r==Just nw]

-- Match the populated scaffolding by typed position, allowing the state and
-- full-state operand widths to change together. Other members (e.g. enables)
-- retain their exact type/range. This does not rewrite any declaration width.
projectionShape (Group ns _ nm) (Group os _ om) =
 length nm==length om && and(zipWith same nm om)
 where
  nw=lookupWidth ns nm;ow=lookupWidth os om
  lookupWidth n ms=case [width r|(x,_,r)<-ms,x==n] of [w]->w;_->Nothing
  same (n,t,r)(o,u,q) = (n==ns)==(o==os) && t==u &&
    (r==q || (width r==nw && width q==ow))

-- Every target member must be absent, including input/output/instance names.
proposeVacancy budget old new=(audit,take (max 0 budget) accepted) where
 og=getGroups old;ng=getGroups new
 od=definitions old;nd=definitions new
 ot=leafTypes old;nt=leafTypes new
 common=[n|(n,r)<-M.toAscList(outputs old),M.lookup n(outputs new)==Just r]
 ordinaryVotes=[(out,n,o)|out<-common,Just a<-[M.lookup out od],Just b<-[M.lookup out nd],Just ps<-[align og ng ot nt a b],(n,o)<-nub ps]
 projected=projectionVotes og ng old new
 outputVotes=nub(ordinaryVotes++projected)
 votes=M.fromListWith S.union[((n,o),S.singleton out)|(out,n,o)<-outputVotes]
 sourceVotes n=[(o,S.size os)|((n',o),os)<-M.toList votes,n==n']
 targetVotes o=[(n,S.size os)|((n,o'),os)<-M.toList votes,o==o']
 unique []=Nothing
 unique xs=case sortOn(negate.snd)xs of (n,k):rest|all((<k).snd)rest->Just n;_->Nothing
 guided=[(n,o,S.size os,S.toAscList os)|((n,o),os)<-M.toList votes,n/=o,unique(sourceVotes n)==Just o,unique(targetVotes o)==Just n]
 -- Enumerated alternatives make no uniqueness or OLD-function claim. The
 -- independent proof relates each whole CURRENT group to itself under a
 -- bijective rename; OLD-relative measured cost decides between alternatives.
 guidedKeys=S.fromList[(n,o)|(n,o,_,_)<-guided]
 projectedKeys=S.fromList[(n,o)|(_,n,o)<-projected,(n,o) `S.member` guidedKeys]
 alternatives=[(n,o,0,[])|n<-M.keys ng,o<-M.keys og,n/=o,S.notMember(n,o)guidedKeys]
 desired=sortOn(\(n,o,k,_)->(-k,n,o))guided++alternatives
 pairLimit=4096
 fixed=protected new
 usedIds=S.fromList(map name(semanticIds(vm_body new)))
 declCounts=M.fromListWith (+)
   [(name(vvName v),1::Int)|x<-flat(vm_body new),vs<-case x of
      VMDecl(VVDecl _ _ xs)->[xs];VMDecl(VVDWire _ x _)->[[x]];_->[],v<-vs]
 attempt(n,o,k,os)=do
  from<-maybe(Left "source-not-canonical-state-group")Right(M.lookup n ng)
  if M.member o ng then Left "historical-name-still-occupied" else Right()
  target<-maybe(Left "historical-name-is-not-old-state-group")Right(M.lookup o og)
  if sig from==sig target || ((n,o) `S.member` projectedKeys && projectionShape from target) then Right() else Left "incompatible-populated-group-shape-without-unique-projection-role"
  let Group _ _ fm=from;Group _ _ tm=target
      fs=[x|(x,_,_)<-fm];ts=[x|(x,_,_)<-tm]
      pairs=zip fs ts
      ss=S.fromList(map fst pairs);tt=S.fromList(map snd pairs)
  if S.null(ss `S.intersection` tt)&&length pairs==S.size ss&&length pairs==S.size tt then Right() else Left "overlap-or-name-collision"
  if S.null(tt `S.intersection` usedIds) then Right() else Left "historical-group-member-is-not-vacant"
  if all(\s->M.lookup s declCounts==Just 1)(S.toList ss) then Right() else Left "nonunique-declaration-collision"
  if S.null((ss `S.union` tt) `S.intersection` fixed) then Right() else Left "fixed-identity-collision"
  if all(vIsValidIdent.snd)pairs then Right() else Left "invalid-target-name"
  if any(\x->any(\u->take(length x+1)u==x++".")(S.toList usedIds))(S.toList(ss `S.union` tt)) then Left "hierarchical-reference-to-permuted-identity" else Right()
  return(StateRolePermutation n o os pairs(applyNames pairs new))
 attempts=[((n,o),attempt e)|e@(n,o,_,_)<-take pairLimit desired]
 accepted=dedup S.empty[p|(_,Right p)<-attempts]
 dedup _ []=[]
 dedup seen(p:ps)|key `S.member` seen=dedup seen ps
                  |otherwise=p:dedup(S.insert key seen)ps
  where key=S.fromList[srpSourceState p,srpHistoricalState p]
 audit=VacancyAudit
   "Unique output-role proposals first; then deterministic bounded alternatives. Enumeration is not unique matching; all candidates require CURRENT equivalence and measured OLD-relative improvement."
   [(n,o,S.size os,S.toAscList os)|((n,o),os)<-M.toList votes]
   [(n,o,if S.member(n,o)projectedKeys then "unique-projected-output-role" else if S.member(n,o)guidedKeys then "unique-output-role" else "enumerated-alternative",
     either id(const "eligible-pending-proof-and-cost")result)|((n,o),result)<-attempts]
   pairLimit(max 0(length desired-pairLimit))(max 0 budget)(length accepted)
   (min(max 0 budget)(length accepted))(length accepted>max 0 budget||length desired>pairLimit)
