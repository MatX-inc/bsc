-- Structural declaration proposals on retained native ASTs. Historical names
-- supply grouping and order only; every declaration, width, type and variable
-- is taken from CURRENT. Independent whole-module proof remains mandatory.
module ECOASTDeclarations (declarationProposals) where

import Verilog
import qualified Data.Map.Strict as M
import Data.List (sortOn, groupBy)
import Data.Maybe (mapMaybe, fromMaybe)

name = getVIdString

-- Only plain scalar declarations participate. Initialized wires, parameters,
-- arrays, directive comments and translate-off wrappers are barriers.
declarationProposals :: ([VMItem] -> Bool) -> VModule -> VModule -> [(String,VModule)]
declarationProposals safeComments old current =
  [(mode,current {vm_body=walkList mode (vm_body current)})
    | mode <- ["historical-declaration-partitions", "historical-register-placement",
               "historical-declarations-and-register-placement", "historical-declaration-regions"]]
 where
  allLeaves = concatMap leaf
  leaf (VMComment _ x)=leaf x
  leaf (VMRegGroup _ _ _ x)=leaf x
  leaf (VMGroup _ xss)=allLeaves (concat xss)
  leaf x=[x]
  counts m=M.fromListWith (+)
    [(name (vvName v),1::Int) | VMDecl (VVDecl _ _ vs)<-allLeaves(vm_body m),v<-vs]
  oc=counts old; nc=counts current
  unique n=M.lookup n oc==Just 1 && M.lookup n nc==Just 1
  plain (VMDecl (VVDecl t r vs))
    | t `elem` [VDWire,VDReg],not(null vs),all scalar vs=Just(t,r,vs)
  plain _=Nothing
  scalar (VVar _)=True
  scalar _=False
  -- Never consult an OLD declaration hidden by a semantic wrapper.
  safeLeaves=concatMap safeLeaf
  safeLeaf x@(VMComment _ body) | safeComments[x]=safeLeaf body
  safeLeaf x@(VMRegGroup _ _ _ body) | safeComments[x]=safeLeaf body
  safeLeaf (VMGroup False xss)=safeLeaves(concat xss)
  safeLeaf x=case plain x of Just _->[x];_->[]
  oldGroups=mapMaybe plain(safeLeaves(vm_body old))
  ranks=M.fromList
    [(name(vvName v),(groupIndex,memberIndex,t))
      | (groupIndex,(t,_,vs))<-zip[0::Int ..]oldGroups,
        (memberIndex,v)<-zip[0::Int ..]vs,unique(name(vvName v))]
  historical t v=case M.lookup(name(vvName v))ranks of
    Just(g,m,ot)|ot==t->Just(g,m)
    _->Nothing
  -- Rebuild one uninterrupted CURRENT declaration segment. Historical group
  -- boundaries survive merging/splitting by the backend, but a changed CURRENT
  -- type/width splits a group rather than restoring an obsolete type/width.
  regroup segment=
    let entries=[(i,t,r,v,historical t v)
                 |(i,x)<-zip[0::Int ..]segment,Just(t,r,vs)<-[plain x],v<-vs]
        historic=[x|x@(_,_,_,_,Just _)<-entries]
        novel=[x|x@(_,_,_,_,Nothing)<-entries]
        sorted=sortOn (\(_,_,_,_,Just(g,m))->(g,m)) historic
        sameOld (_,t,r,_,Just(g,_)) (_,u,s,_,Just(h,_))=g==h&&t==u&&r==s
        sameOld _ _=False
        -- Nonconsecutive width changes still get distinct valid declarations;
        -- names are never duplicated or dropped when a group changes widths.
        oldBuckets=groupBy sameOld sorted
        newBuckets=groupBy (\(i,_,_,_,_)(j,_,_,_,_)->i==j) novel
        render bucket=case bucket of
          (_,t,r,_,_):_->VMDecl(VVDecl t r[v|(_,_,_,v,_)<-bucket])
          _->error "empty declaration bucket"
    in map render(oldBuckets++newBuckets)
  partition mode=mode/="historical-register-placement"
  placement mode=mode/="historical-declaration-partitions"
  walk mode x@(VMComment c body)|safeComments[x]=VMComment c(one(decls mode[walk mode body]))
  walk mode x@(VMRegGroup i cls c body)|safeComments[x]=VMRegGroup i cls c(walk mode body)
  walk mode (VMGroup False xss)=VMGroup False(map(walkList mode)xss)
  walk _ x=x
  walkList mode xs=regions mode (place mode (decls mode(map(walk mode)xs)))
  decls _ []=[]
  decls mode (x:xs)
    |partition mode,plain x/=Nothing=
       let(run,rest)=span((/=Nothing).plain)(x:xs)
       in regroup run++decls mode rest
    |otherwise=x:decls mode xs
  regKey x@(VMRegGroup i _ _ body)
    |safeComments[x],not(null(safeLeaves[body])),
     length(safeLeaves[body])==length(allLeaves[body]),
     all((/=Nothing).plain)(allLeaves[body])=Just(name i)
  regKey _=Nothing
  oldRegisterRanks=M.fromListWith min
    [(k,i)|(i,x)<-zip[0::Int ..](registers(vm_body old)),Just k<-[regKey x]]
  registers=concatMap visit
  visit x@(VMRegGroup {})=[x]
  visit x@(VMComment _ body)|safeComments[x]=visit body
  visit(VMGroup False xss)=registers(concat xss)
  visit _=[]
  oldRank x=regKey x >>= (`M.lookup`oldRegisterRanks)
  -- A register wrapper is an indivisible typed declaration group. Preserve
  -- matched OLD order in a current uninterrupted register segment; keep new
  -- wrappers separate and in their current relative order. No processes,
  -- comments or other declaration classes are moved across this boundary.
  place _ []=[]
  place mode (x:xs)
    |placement mode,regKey x/=Nothing=
      let (run,rest)=span((/=Nothing).regKey)(x:xs)
          matched=sortOn(fromMaybe maxBound.oldRank)[y|y<-run,oldRank y/=Nothing]
          added=[y|y<-run,oldRank y==Nothing]
      in added++matched++place mode rest
    |otherwise=x:place mode xs

  -- Split novel declarations out of transparent CURRENT declaration wrappers.
  -- Existing comments remain exactly once on their existing group; newly split
  -- declarations have no invented historical comment. A register wrapper is
  -- indivisible, including its complete CURRENT input/enable scaffolding.
  declarationTree x=not(null(safeLeaves[x]))
    && length(safeLeaves[x])==length(allLeaves[x])
    && all((/=Nothing).plain)(allLeaves[x]) && safeComments[x]
  oldMember t v=historical t v/=Nothing
  split x@(VMDecl(VVDecl t _ vs))
    |all(oldMember t)vs=([x],[])
    |all(not.oldMember t)vs=([],[x])
  split x@VMRegGroup{}=case oldRank x of Just _->([x],[]);_->([],[x])
  split (VMComment c x)=case split x of
    ([],fresh)->([],[VMComment c (one fresh)])
    (kept,[])->([VMComment c(one kept)],[])
    (kept,fresh)->([VMComment c(one kept)],fresh)
  split (VMGroup False xss)=
    let parts=map(map split)xss
        kept=[concatMap fst xs|xs<-parts]
        fresh=[concatMap snd xs|xs<-parts]
        wrap xs=if all null xs then [] else [VMGroup False(filter(not.null)xs)]
    in (wrap kept,wrap fresh)
  split x=([x],[])
  one [x]=x
  one xs=VMGroup False[xs]
  regions "historical-declaration-regions"=moveNovel
  regions _=id
  moveNovel []=[]
  moveNovel(x:xs)
    |declarationTree x=
      let (run,rest)=span declarationTree(x:xs)
          parts=map split run
      in concatMap snd parts++concatMap fst parts++moveNovel rest
    |otherwise=x:moveNovel xs
