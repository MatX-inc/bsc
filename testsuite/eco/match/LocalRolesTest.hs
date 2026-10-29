module Main where

import ECO.Types
import ECOMatchTypes
import ECOMatch
import ECOLocalRoles (refineLocalRoles)
import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S
import System.Exit (die)

check :: String -> Bool -> IO ()
check label ok = unless ok (die ("FAIL: " ++ label))
right :: Show a => Either a b -> IO b
right (Right x) = return x
right (Left e) = die (show e)
node :: String -> String -> [String] -> EcoNode
node k op args = EcoNode k (Just k) "Bit:8" op args Nothing True [] False
anon :: String -> String -> [String] -> EcoNode
anon k op args = (node k op args) {enName=Nothing,enEligible=False}
boundary :: String -> [String] -> EcoNode
boundary k args = (node k "boundary" args) {enBoundary=Just k,enEligible=False}
graph :: [EcoNode] -> EcoGraph
graph xs = EcoGraph ecoGraphSchema "local" "final" xs (mapMaybe enName xs)
seed :: String -> [EcoNode]
seed side = [boundary "regA" [],boundary "regB" [],
             node (side++"Seed") "definition" [if side=="old" then "regA" else "regB"],
             boundary "seedOutput" [side++"Seed"]]
pair :: [EcoNode] -> [EcoNode] -> (EcoGraph,EcoGraph)
pair a b = (graph(seed "old"++a),graph(seed "new"++b))
local :: String -> String -> EcoNode
local side op = node (side++"Local") op [side++"Seed","regA"]
has :: String -> String -> EcoMatchResult -> Bool
has a b = any (\c->ecOld c==a && ecNew c==b) . emCorrespondences
policy :: (EcoGraph,EcoGraph) -> IO EcoMatchResult
policy (o,n) = right (matchGraphsWithPolicy LocalRoles o n)

main :: IO ()
main = do
    let simple@(o,n) = pair [local "old" "add"] [local "new" "add"]
    baseline <- right (matchGraphs o n)
    explicit <- right (matchGraphsWithPolicy Baseline o n)
    check "baseline remains selectable and unchanged" (baseline==explicit)
    check "baseline lacks disconnected changed-content role" (not(has "oldLocal" "newLocal" baseline))
    matched <- policy simple
    check "matched operand roles recover changed dependency cone" (has "oldLocal" "newLocal" matched)
    check "new local match is classified changed role" (any (\c->ecOld c=="oldLocal" && ecKind c==ChangedRole) (emCorrespondences matched))
    permuted <- policy (o{egNodes=reverse(egNodes o)},n{egNodes=reverse(egNodes n)})
    check "policy deterministic under input order" (permuted==matched)
    self <- right (matchGraphsWithPolicy LocalRoles o o)
    names <- right(proposeRenames(egReserved o)o o self)
    check "local policy self identity" (null names)
    check "pairing remains injective" (let cs=emCorrespondences matched in
        S.size(S.fromList(map ecOld cs))==length cs && S.size(S.fromList(map ecNew cs))==length cs)
    changedOp <- policy (pair [local "old" "add"] [local "new" "subtract"])
    check "different local operator is not inferred" (not(has "oldLocal" "newLocal" changedOp))
    changedReg <- policy (pair [local "old" "add"] [(local "new" "add"){enOperands=["newSeed","regB"]}])
    check "different register source is not inferred" (not(has "oldLocal" "newLocal" changedReg))
    reordered <- policy (pair [local "old" "add"] [(local "new" "add"){enOperands=["regA","newSeed"]}])
    check "operand order matters" (not(has "oldLocal" "newLocal" reordered))
    width <- policy (pair [local "old" "add"] [(local "new" "add"){enType="Bit:9"}])
    check "width mismatch remains unmatched" (not(has "oldLocal" "newLocal" width))
    let copies side = [node (side++show i) "add" [side++"Seed","regA"]|i<-[1,2::Int]]
    ambiguous <- policy (pair (copies "old") (copies "new"))
    check "indistinguishable duplicate roles stay unmatched" (not(any ((=="typed-ordered-operand-role").ecReason)(emCorrespondences ambiguous)))
    blocked <- policy (pair (copies "old" ++ [node "oldDownstream" "subtract" ["old1"]])
                            (copies "new" ++ [node "newDownstream" "subtract" ["new1"]]))
    check "unresolved duplicate operand blocks downstream inference" (not(has "oldDownstream" "newDownstream" blocked))
    fanout <- policy (pair [node "oldLocal" "pair" ["oldSeed","oldSeed"]]
                          [node "newLocal" "pair" ["newSeed","regA"]])
    check "inconsistent reference fanout is rejected" (not(has "oldLocal" "newLocal" fanout))
    let shared side duplicate =
            [node(side++"Local") "definition" [side++"Pair"],
             anon(side++"Pair") "pair" [side++"And",side++(if duplicate then "Copy" else "And")],
             anon(side++"And") "and" [side++"Seed","regA"]] ++
            [anon(side++"Copy") "and" [side++"Seed","regA"] | duplicate]
    sharedResult <- policy(pair(shared "old" False)(shared "new" True))
    check "shared anonymous DAG constraints are consistent" (has "oldLocal" "newLocal" sharedResult)
    let unbound side = [node (side++"Unknown") (side++"-operator") [],
                        node (side++"Local") "add" [side++"Unknown","regA"]]
    unknown <- policy(pair(unbound "old")(unbound "new"))
    check "unmatched operand names are not assumed equivalent" (not(has "oldLocal" "newLocal" unknown))

    -- Weak named/anonymous consumer alignment cannot supply an emitted name.
    -- A unique typed definition role can replace it, while a named role is fixed.
    let definition side = [node (side++"Local") "definition" [side++"Rhs"],
                           anon (side++"Rhs") "and" [side++"Seed"]]
        weakPair = pair (definition "old" ++ [anon "embedded" "and" ["oldSeed"],boundary "use" ["embedded"]])
                        (definition "new" ++ [boundary "use" ["newLocal"]])
    weakBase <- right(uncurry matchGraphs weakPair)
    check "fixture has weak anonymous to named claim" (has "embedded" "newLocal" weakBase)
    weakResult <- policy weakPair
    check "typed named role displaces only weak cross-kind claim" (has "oldLocal" "newLocal" weakResult && not(has "embedded" "newLocal" weakResult))
    check "displaced anonymous occurrence becomes unmatched" ("embedded" `elem` emUnmatchedOld weakResult)
    let strongPair = pair [local "old" "add",node "oldOther" "multiply" ["oldSeed","regA"],boundary "use" ["oldOther"]]
                          [local "new" "add",boundary "use" ["newLocal"]]
    strong <- policy strongPair
    check "existing named consumer role has priority" (has "oldOther" "newLocal" strong && not(has "oldLocal" "newLocal" strong))
    let exactBase=weakBase {emCorrespondences=map(\c->if ecNew c=="newLocal" then c{ecReason="typed-content"}else c)(emCorrespondences weakBase)}
        exactResult=uncurry (\a b->refineLocalRoles (\_ _->ChangedRole) a b exactBase) weakPair
    check "exact anonymous claim is not displaced" (has "embedded" "newLocal" exactResult)

    -- Incremental bindings advance through a long chain without whole-graph
    -- fixed-point scans. Each operator shape is unique, so no bucket is skipped.
    let chain :: String -> Int -> [EcoNode]
        chain side count = [node (side++show i) ("layer:"++show i) [previous i] | i<-[1..count]]
          where previous 1=side++"Seed"; previous i=side++show(i-1)
    chainResult <- policy(pair(chain "old" (12000::Int))(chain "new" 12000))
    check "12000 incremental role bindings" (length(filter((=="typed-ordered-operand-role").ecReason)(emCorrespondences chainResult))==12000)
    let repeatedChain side = map(\x->x{enOp="same-layer"})(chain side 64)
    repeatedChainResult <- policy(pair(repeatedChain "old")(repeatedChain "new"))
    check "incremental bindings disambiguate a repeated operator family" (length(filter((=="typed-ordered-operand-role").ecReason)(emCorrespondences repeatedChainResult))==64)
    let repeated side = [node(side++show i) "same" [side++"Seed"]|i<-[1..10000::Int]]
    large <- policy(pair(repeated "old")(repeated "new"))
    check "10000 duplicate roles are bounded and unresolved" (not(any((=="typed-ordered-operand-role").ecReason)(emCorrespondences large)))
    putStrLn "ECO local-role policy: all tests passed (including 12000 incremental bindings and 10000 duplicates)"
