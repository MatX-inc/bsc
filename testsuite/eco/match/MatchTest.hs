module Main where

import ECO.Types
import ECOMatchTypes
import ECOMatch
import Control.Monad (unless)
import Data.List (sort)
import System.Exit (die)

check :: String -> Bool -> IO ()
check label ok = unless ok (die ("FAIL: " ++ label))

right :: Show a => Either a b -> IO b
right (Right x) = return x
right (Left e) = die (show e)

node :: String -> String -> [String] -> EcoNode
node k op args = EcoNode k (Just k) "Bit:8" op args Nothing True [] False

boundary :: String -> [String] -> EcoNode
boundary k args = (node k "boundary" args)
    { enEligible = False, enBoundary = Just k }

graph :: [EcoNode] -> EcoGraph
graph ns = EcoGraph ecoGraphSchema "test" "final" ns (map enKey ns)

oldGraph :: EcoGraph
oldGraph = graph [boundary "regA" [], boundary "regB" [],
                  node "oldConstant" "integer:1" [],
                  node "oldSum" "add" ["regA","oldConstant"],
                  boundary "result" ["oldSum"]]

changed :: String -> String -> String -> EcoGraph
changed op source value = graph [boundary "regA" [], boundary "regB" [],
                  node "newConstant" value [],
                  node "newSum" op [source,"newConstant"],
                  boundary "result" ["newSum"]]

has :: String -> String -> EcoMatchKind -> EcoMatchResult -> Bool
has a b kind r = any (\c -> ecOld c == a && ecNew c == b && ecKind c == kind)
                    (emCorrespondences r)

reason :: String -> String -> EcoMatchResult -> Maybe String
reason a b r = lookup (a,b) [((ecOld c,ecNew c),ecReason c) | c <- emCorrespondences r]

main :: IO ()
main = do
    let newConstant = changed "add" "regA" "integer:2"
        newOperator = changed "sub" "regA" "integer:1"
        newRegister = changed "add" "regB" "integer:1"
    constant <- right (matchGraphs oldGraph newConstant)
    check "changed constant retains consumer role" (has "oldConstant" "newConstant" ChangedRole constant)
    check "changed result retains role" (has "oldSum" "newSum" ChangedRole constant)
    operator <- right (matchGraphs oldGraph newOperator)
    check "changed operator retains result role" (has "oldSum" "newSum" ChangedRole operator)
    register <- right (matchGraphs oldGraph newRegister)
    check "changed register input retains consumer name" (has "oldSum" "newSum" ChangedRole register)
    check "different registers remain distinct" (not (has "regA" "regB" ExactContent register ||
                                                      has "regA" "regB" ChangedRole register))
    renamed <- right (proposeRenames (egReserved newRegister) oldGraph newRegister register)
    check "historical name proposal" (("newSum","oldSum") `elem` renamed)

    identical <- right (matchGraphs oldGraph oldGraph)
    identityNames <- right (proposeRenames (egReserved oldGraph) oldGraph oldGraph identical)
    check "self-match identity" (null identityNames && null (emUnmatchedOld identical) &&
                                all (\c -> ecOld c == ecNew c) (emCorrespondences identical))
    permuted <- right (matchGraphs (oldGraph {egNodes = reverse (egNodes oldGraph)}) newConstant)
    check "input order independence" (permuted == constant)
    permutedSelf <- right (matchGraphs oldGraph (oldGraph {egNodes = reverse (egNodes oldGraph)}))
    check "permuted self identity" (permutedSelf == identical)

    let duplicates a b = graph [node a "same" [],node b "same" [],
                               boundary "one" [a],boundary "two" [b]]
    dup <- right (matchGraphs (duplicates "a" "b") (duplicates "x" "y"))
    check "duplicate occurrences stay distinct by consumer" (has "a" "x" ExactContent dup &&
                                                            has "b" "y" ExactContent dup)
    unresolved <- right (matchGraphs (graph [node "a" "same" [],node "b" "same" []])
                                    (graph [node "x" "same" [],node "y" "same" []]))
    check "unresolved duplicate bucket is completed deterministically"
          (has "a" "x" ExactContent unresolved && has "b" "y" ExactContent unresolved &&
           emAmbiguousBuckets unresolved == 1)
    let named k s op = (node k op []) {enName = Just s}
        namedOld = graph [named "old-a" "alias" "same", named "old-b" "gone" "same"]
        namedNew = graph [named "new-a" "alias" "same", named "new-b" "added" "same"]
    namedDup <- right (matchGraphs namedOld namedNew)
    check "exact duplicate keeps surviving local name" (has "old-a" "new-a" ExactContent namedDup)
    check "remaining exact duplicate takes the historical role" (has "old-b" "new-b" ExactContent namedDup)
    namedRenames <- right (proposeRenames ["alias","added"] namedOld namedNew namedDup)
    check "unchanged alias is not needlessly freshened" (namedRenames == [("added","gone")])
    let generatedOld = graph [named "old-a" "expr__d1" "old", named "old-b" "old-copy" "old"]
        generatedNew = graph [named "new-a" "expr__d1" "changed", named "new-b" "new-copy" "changed"]
    generated <- right (matchGraphs generatedOld generatedNew)
    check "same generated spelling is a typed fallback, not content evidence"
          (reason "old-a" "new-a" generated == Just "aggressive:same-name-typed-fallback" &&
           reason "old-b" "new-b" generated == Just "aggressive:deterministic-typed-fallback")
    let fanoutOld = graph [node "a" "old" [],boundary "one" ["a"],boundary "two" ["a"]]
        fanoutNew = graph [node "b" "new" [],node "c" "new" [],
                           boundary "one" ["b"],boundary "two" ["c"]]
    conflict <- right (matchGraphs fanoutOld fanoutNew)
    check "conflicting consumer roles fall back to one deterministic choice"
          (reason "a" "b" conflict == Just "aggressive:deterministic-typed-fallback" &&
           emUnmatchedNew conflict == ["c"])

    let typedNew = newConstant {egNodes = map (\n -> if enKey n == "newSum"
                                              then n {enType = "Bit:9"} else n) (egNodes newConstant)}
    typed <- right (matchGraphs oldGraph typedNew)
    check "width mismatch stays unmatched" ("newSum" `elem` emUnmatchedNew typed)
    let oldSwap = graph [node "a" "first" [],node "b" "second" []]
        newSwap = graph [node "a" "second" [],node "b" "first" []]
    swap <- right (matchGraphs oldSwap newSwap)
    swapNames <- right (proposeRenames ["a","b"] oldSwap newSwap swap)
    check "simultaneous injective name swap" (swapNames == [("a","b"),("b","a")])
    let historical = graph [node "gone" "old" []]
        unrelated = graph [node "gone" "unrelated" []]
    gone <- right (matchGraphs historical unrelated)
    fresh <- right (proposeRenames ["gone","_eco_fresh_0"] historical unrelated gone)
    check "same spelling with changed content keeps its name as a typed fallback"
          (reason "gone" "gone" gone == Just "aggressive:same-name-typed-fallback" && null fresh)
    let widened = graph [(node "gone" "unrelated" []) {enType = "Bit:9"}]
    lost <- right (matchGraphs historical widened)
    reserved <- right (proposeRenames ["gone","_eco_fresh_0"] historical widened lost)
    check "deleted historical namespace stays reserved" (reserved == [("gone","_eco_fresh_1")])
    blocked <- right (proposeRenames ["newSum","oldSum"] oldGraph newRegister register)
    check "protected target is never taken" (all ((/= "oldSum") . snd) blocked)

    check "dangling reference rejects" (isLeft (matchGraphs (graph [node "a" "id" ["missing"]]) oldGraph))
    check "combinational cycle rejects" (isLeft (matchGraphs (graph [node "a" "id" ["a"]]) oldGraph))
    check "duplicate keys reject" (isLeft (matchGraphs (graph [node "a" "id" [],node "a" "id" []]) oldGraph))
    check "foreign schema rejects" (isLeft (matchGraphs (oldGraph {egSchema = 9}) oldGraph))
    check "serialization round trip" ((read (show oldGraph) :: EcoGraph) == oldGraph)

    -- Repeated diamonds defeat implementations that expand expression trees.
    let dag :: String -> Int -> EcoGraph
        dag prefix count = graph $ boundary "input" [] :
            [node (prefix ++ show i) "pair" [prev i,prev i] | i <- [1..count]]
          where prev 1 = "input"
                prev i = prefix ++ show (i-1)
    large <- right (matchGraphs (dag "old" (12000 :: Int)) (dag "new" 12000))
    check "large shared DAG exact matching" (length (emCorrespondences large) == 12001)
    check "stable report order" (map ecNew (emCorrespondences large) ==
                                sort (map ecNew (emCorrespondences large)))
    putStrLn "ECO matcher: all tests passed (including a 12000-node shared DAG)"
  where
    isLeft (Left _) = True
    isLeft _ = False
