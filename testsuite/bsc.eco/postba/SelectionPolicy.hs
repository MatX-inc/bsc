module Main where
import ECOASTSelection
import Control.Monad
main=do
 let a=CostPoint "a" 10 6;b=CostPoint "b" 12 4;c=CostPoint "c" 11 7;d=CostPoint "d" 20 9
     check label yes=unless yes(fail label)>>putStrLn("PASS "++label)
 check "raw winner does not dominate better normalized candidate"(paretoPoints[a,b,c,d]==[a,b])
 check "raw-first explicit policy"(choosePoint RawFirst[a,b,c,d]==a)
 check "normalized-first explicit policy"(choosePoint NormalizedFirst[a,b,c,d]==b)
 check "single candidate"(choosePoint RawFirst[b]==b)
 check "equal raw prefers better normalized"(choosePoint RawFirst[a,a{pointKey="e",pointNormalized=4}]==a{pointKey="e",pointNormalized=4})
 check "equal normalized prefers better raw"(choosePoint NormalizedFirst[b,b{pointKey="e",pointRaw=10}]==b{pointKey="e",pointRaw=10})
 check "stable key tie break"(choosePoint RawFirst[a{pointKey="z"},a]==a)
 check "all identical costs remain visible"(length(paretoPoints[a,a{pointKey="z"}])==2)
 putStrLn "PASS directedchecks=8"
