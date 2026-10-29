module Main where
import ECO.Types
import ECOMatchTypes
import ECOMatch
import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Exit (die)

check :: String -> Bool -> IO ()
check label ok=unless ok (die ("FAIL: "++label))
right :: Show e => Either e a -> IO a
right (Right x)=return x
right (Left e)=die (show e)
node :: EcoKey -> String -> [EcoKey] -> EcoNode
node k op args=EcoNode k (Just k) "Bit:8" op args Nothing True ["source:"++k] False
boundary :: EcoKey -> EcoNode
boundary k=(node k "boundary" []){enBoundary=Just k,enEligible=False}
graph :: [EcoNode] -> EcoGraph
graph ns=EcoGraph 1 "aggressive" "final" ns (mapMaybe enName ns)
has :: EcoKey -> EcoKey -> EcoMatchResult -> Bool
has a b r=any (\c->ecOld c==a&&ecNew c==b) (emCorrespondences r)
evidence :: EcoKey -> EcoKey -> EcoMatchResult -> Maybe String
evidence a b r=lookup (a,b) [((ecOld c,ecNew c),ecReason c)|c<-emCorrespondences r]
-- Every result is an injective, typed, maximum-cardinality assignment of the
-- eligible names whose old spelling is a legal target in the new namespace.
invariants :: EcoGraph -> EcoGraph -> EcoMatchResult -> IO ()
invariants old new result=do
  let cs=emCorrespondences result
      os=M.fromList[(enKey n,n)|n<-egNodes old]; ns=M.fromList[(enKey n,n)|n<-egNodes new]
      news=[n|n<-egNodes new,enEligible n]
      protected=S.fromList (egReserved new) S.\\ S.fromList (mapMaybe enName news)
      olds=[n|n<-egNodes old,enEligible n,maybe False (`S.notMember` protected) (enName n)]
      pairs=[(os M.! ecOld c,ns M.! ecNew c)|c<-cs,enEligible (os M.! ecOld c),enEligible (ns M.! ecNew c)]
      count ks=M.fromListWith (+) [(k,1::Int)|k<-ks]
      at m t=M.findWithDefault 0 t m
      matched=count [enType b|(_,b)<-pairs]
  check "all correspondences injective" (length cs==S.size(S.fromList(map ecOld cs)) && length cs==S.size(S.fromList(map ecNew cs)))
  check "eligible pairs share a type" (and [enType a==enType b|(a,b)<-pairs])
  check "protected old targets are never assigned" (and [maybe True (`S.notMember` protected) (enName a)|(a,_)<-pairs])
  check "complete typed maximum cardinality"
        (and [at matched t==min (at (count (map enType olds)) t) (at (count (map enType news)) t)
             |t<-S.toList (S.fromList (map enType (olds++news)))])
  _<-right(proposeRenames(egReserved new)old new result)
  return ()
run :: EcoGraph -> EcoGraph -> IO EcoMatchResult
run old new=do
  r<-right(matchGraphs old new)
  invariants old new r
  return r

main :: IO ()
main=do
  let self=graph[boundary "reg",node "a" "add" ["reg"],node "b" "add" ["reg"]]
  selfResult<-run self self
  names<-right(proposeRenames(egReserved self)self self selfResult)
  check "exact self identity" (null names)
  let old=graph[boundary "regA",boundary "regB",node "seed" "definition" ["regA"],
                node "a" "add" ["seed"],node "b" "sub" ["seed"],node "c" "mul" ["seed"],
                node "d" "consumer" ["a"],node "e" "consumer2" ["d","b"]]
      new=graph[boundary "regA",boundary "regB",node "seed" "definition" ["regB"],
                node "a" "mul" ["seed"],node "b" "add" ["seed"],node "c" "sub" ["seed"],
                node "d" "consumer" ["b"],node "e" "consumer2" ["d","c"]]
  cycleResult<-run old new
  check "joint cycle replaces changed source claims" (has "c" "a" cycleResult && has "a" "b" cycleResult && has "b" "c" cycleResult)
  check "active operand refresh preserves downstream roles" (has "d" "d" cycleResult&&has "e" "e" cycleResult)
  let chainOld=graph[boundary "regA",boundary "regB",node "seed" "definition" ["regA"],
                     node "a" "add" ["seed"],node "b" "sub" ["seed"],
                     node "c" "mul" ["seed"],node "d" "square" ["seed"]]
      chainNew=graph[boundary "regA",boundary "regB",node "seed" "definition" ["regB"],
                     node "a" "sub" ["seed"],node "b" "mul" ["seed"],node "c" "square" ["seed"]]
  chainResult<-run chainOld chainNew
  check "alternating chain reaches an unused old target jointly" (has "b" "a" chainResult&&has "c" "b" chainResult&&has "d" "c" chainResult)
  permuted<-run old{egNodes=reverse(egNodes old)} new{egNodes=reverse(egNodes new)}
  check "graph order does not choose ties" (cycleResult==permuted)
  let closed=graph[boundary "fixed",node "fresh" "different" []]
      historical=graph[node "fixed" "old-op" []]
  protection<-run historical closed
  check "old target protected in new scope is excluded" (null (emCorrespondences protection)&&"fresh" `elem` emUnmatchedNew protection)
  let wider=graph[(node "wider" "anything" []){enType="Bit:9"}]
  widthResult<-run historical wider
  check "incompatible widths stay unmatched" (null (emCorrespondences widthResult))
  let surplusOld=graph[node "old0" "x" [],node "old1" "y" []]
      surplusNew=graph[node "new0" "q" [],node "new1" "r" [],node "new2" "s" []]
  surplusResult<-run surplusOld surplusNew
  check "surplus witness exact, not unexplained ambiguity" (length (emCorrespondences surplusResult)==2&&length (emUnmatchedNew surplusResult)==1)
  let copies prefix count=graph[node(prefix++show i) "same" []|i<-[1..count::Int]]
  copiesResult<-run(copies "old" 10000)(copies "new" 10000)
  check "10000 duplicate candidates are completed without Cartesian expansion" (length(emCorrespondences copiesResult)==10000)
  let orderedOld=graph[boundary "x",boundary "y",node "old" "sub" ["x","y"]]
      orderedNew=graph[boundary "x",boundary "y",node "new" "sub" ["y","x"]]
  orderResult<-run orderedOld orderedNew
  check "ordered operand mismatch is labelled typed fallback, not exact evidence" (evidence "old" "new" orderResult==Just "aggressive:deterministic-typed-fallback")
  -- Local evidence expands anonymous DAGs while named operands terminate it.
  -- The incremental scoring guard independently recomputes a hash-consed full
  -- score after initialization and every accepted move in all these tests.
  let anonymous k op args=(node k op args){enName=Nothing,enEligible=False}
      dag side=graph [boundary "regA",boundary "regB",
        node "seed" "definition" [if side then "regB" else "regA"],
        anonymous "inner" "add" ["seed","seed"],
        anonymous "shared" "xor" ["inner","inner"],
        node "a" (if side then "sub" else "add") ["shared"],
        node "b" (if side then "add" else "sub") ["shared"],
        anonymous "child" "mul" [if side then "b" else "a","shared"],
        node "out" "result" ["child","child"]]
  dagResult<-run(dag False)(dag True)
  check "anonymous shared DAG scoring includes changed named reference consumers"
        (has "a" "b" dagResult&&has "b" "a" dagResult&&has "out" "out" dagResult)
  let opaqueGraph side=graph [boundary "regA",boundary "regB",
        (anonymous "opaque" "foreign" []){enOpaque=True},
        node "out" (if side then "sub" else "add") ["opaque"]]
  opaqueResult<-run(opaqueGraph False)(opaqueGraph True)
  check "opaque local expressions cannot assert exact evidence"
        (all((/= "aggressive:typed-local-rhs-active-operands").ecReason)(emCorrespondences opaqueResult))
  let roleOld=graph[boundary "bypassA",boundary "bypassB",boundary "qA",boundary "qB",
        node "slotA" "definition" ["bypassA"],node "slotB" "definition" ["bypassB"],
        node "qAliasA" "definition" ["qA"],node "qAliasB" "definition" ["qB"],
        node "m0" "mux" ["slotA"],node "m1" "mux" ["slotB"],
        node "outA" "consumerA" ["m0"],node "outB" "consumerB" ["m1"]]
      roleNew=graph[boundary "bypassA",boundary "bypassB",boundary "qA",boundary "qB",
        node "qAliasA" "definition" ["qA"],node "qAliasB" "definition" ["qB"],
        node "m0" "mux" ["qAliasB"],node "m1" "mux" ["qAliasA"],
        node "outA" "consumerA" ["m1"],node "outB" "consumerB" ["m0"]]
  roleResult<-run roleOld roleNew
  check "backward consumers recover reordered identical mux roles after source changes"
        (has "m0" "m1" roleResult&&has "m1" "m0" roleResult)
  let keyGraph k=graph[boundary k,node "root" "definition" [k]]
  keyResult<-run(keyGraph "new:x")(keyGraph "x")
  check "unmapped reference namespace cannot collide with literal old keys"
        (all((/="aggressive:typed-local-rhs-active-operands").ecReason)(emCorrespondences keyResult))
  let diamond count side=graph(boundary "regA":boundary "regB":
        node "seed" "definition" [if side then "regB" else "regA"]:
        node "root" "result" ["anon"++show count]:
        [anonymous ("anon"++show i) "shared" (replicate 2 (if i==1 then "seed" else "anon"++show(i-1)))
         |i<-[1..count::Int]])
  diamondResult<-run(diamond 12000 False)(diamond 12000 True)
  check "backward role traversal memoizes 12000 shared anonymous pairs"
        (has "root" "root" diamondResult&&has "seed" "seed" diamondResult)
  -- Mixed unchanged constants and changed source cones produce overlapping
  -- content/local request families. Invariant checks run after each result;
  -- reversing input order must not change the assignment.
  let mixed seed side=graph(boundary "regA":boundary "regB":
        [node ("n"++show i) ("op"++show ((seed*17+i*13+side*(i `mod` 3)) `mod` 4))
          (if i `mod` 3==0 then [] else [if i<2 then (if side==0 then "regA" else "regB") else "n"++show ((seed+i*7) `mod` i)])
          |i<-[0..14::Int]])
  mapM_ (\seed->do
      let o=mixed seed 0; n=mixed seed 1
      r<-run o n
      r'<-run o{egNodes=reverse(egNodes o)} n{egNodes=reverse(egNodes n)}
      check "overlapping families retain injection and a deterministic assignment" (r==r')) [1..60::Int]
  putStrLn "Aggressive assignment: joint cycles, active-role refresh, permutation, typed surplus/protection and 10000-way ties passed"
