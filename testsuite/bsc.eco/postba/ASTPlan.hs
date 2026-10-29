module Main where
import Control.Monad (unless)
import Data.Either (isLeft)
import ECOASTPlan
import ECO.Types

check name ok = unless ok (error name)
n key name op operands = EcoNode key name "Bit:8" op operands Nothing
    (op == "definition") [] False
input = (n "input" (Just "input") "boundary" []) { enBoundary=Just "input:input" }
rhs = n "rhs" Nothing "add" ["input","one"]
one = n "one" Nothing "integer:1" []
helper = n "helper" (Just "helper") "definition" ["rhs"]
sink k = n k (Just k) "definition" ["helper"]
old = EcoGraph 1 "m" "pre" [input,one,rhs,helper,sink "out1",sink "out2"] []
new = old { egNodes=filter ((/= "out2") . enKey) (egNodes old) }
final = new { egStage="final",egNodes=[input] }
propose a b = proposeKeeps a a b final
replace key f g = g { egNodes=[if enKey x == key then f x else x | x <- egNodes g] }
changeKey g = g { egNodes=[x { enKey=if enKey x == "helper" then "field:helper" else enKey x,
                              enOperands=map (\k -> if k == "helper" then "field:helper" else k) (enOperands x)}
                          | x <- egNodes g] }
emptyResult result = case result of Right p -> null (kpKeeps p); _ -> False

main = do
    let result=propose old new
    check "lost fanout helper" (fmap kpKeeps result == Right [("helper","Bit:8","rhs")])
    check "stable fanout" (emptyResult (propose old old))
    check "changed value" (emptyResult (propose old (replace "one" (\x -> x {enOp="integer:2"}) new)))
    check "already survives" (emptyResult (proposeKeeps old old new new))
    check "tuple field" (emptyResult (propose (changeKey old) (changeKey new)))
    check "tuple parent" (emptyResult (propose old (replace "helper" (\x -> x {enContexts=["tuple-parent:p"]}) new)))
    check "opaque expression" (emptyResult (propose old (replace "rhs" (\x -> x {enOpaque=True}) new)))
    check "undetermined" (emptyResult (propose old (replace "rhs" (\x -> x {enOp="any"}) new)))
    let constant=replace "rhs" (\x -> x {enOperands=["one","one"]})
    check "constant expression" (emptyResult (propose (constant old) (constant new)))
    check "dangling" (isLeft (propose old (replace "rhs" (\x -> x {enOperands=["missing"]}) new)))
    check "cycle" (isLeft (propose old (replace "rhs" (\x -> x {enOperands=["helper"]}) new)))
    check "duplicate key" (isLeft (propose old (new {egNodes=helper:egNodes new})))
    check "module" (isLeft (propose old (new {egModule="other"})))
    let Right p=result
    check "plan binding" (fmap ebpKeepDefinitions (makeBackendPlan p (replicate 64 '1') (replicate 64 '2')) == Right (kpKeeps p))
    check "bad digest" (isLeft (makeBackendPlan p "bad" (replicate 64 '2')))
    putStrLn "PASS: 15 typed native-graph keep-planning checks"
