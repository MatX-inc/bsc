module Main where
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import ECO.Replay (fileIdentity, FileIdentity(..))
import ECO.Types
import ECO.Validate
import System.Environment(getArgs)

check label condition = unless condition (fail label)
readGraph path = do
  g <- read <$> readFile path
  either fail return (validateGraph g)
  return g

main = do
  args <- getArgs
  case args of
    ["plan", capture, stem, output] -> do
      pre <- readGraph (stem ++ ".eco-preopt")
      final <- readGraph (stem ++ ".eco-graph")
      ci <- fileIdentity capture
      pi <- fileIdentity (stem ++ ".eco-preopt")
      let nodes=M.fromList [(enKey n,n) | n<-egNodes pre]
          retained=S.fromList [s | n<-egNodes final, Just s<-[enName n], enEligible n]
          -- This fixture loses one shared scalar Boolean helper. Select its
          -- typed pre-optimization role, not a generated spelling or RTL text.
          lost=[(s,enType n,rhs) | n<-egNodes pre, enEligible n,
              enOp n=="definition", enType n=="Bit:1", Just s<-[enName n],
              S.notMember s retained, [rhs]<-[enOperands n],
              Just e<-[M.lookup rhs nodes], enOp e=="primitive:PrimBNot"]
      check "fixture has one lost Boolean helper" (length lost==1)
      let plan=EcoBackendPlan 1 (egModule pre) (fiSHA256 ci) (fiSHA256 pi) lost
          write suffix value=writeFile (output++suffix) (show value++"\n")
      write "" plan
      write ".empty" plan {ebpKeepDefinitions=[]}
      write ".schema" plan {ebpSchema=2}
      write ".module" plan {ebpModule="other"}
      write ".capture" plan {ebpCaptureSHA256=replicate 64 '0'}
      write ".preopt" plan {ebpPreOptSHA256=replicate 64 '0'}
      write ".duplicate" plan {ebpKeepDefinitions=lost++lost}
      write ".type" plan {ebpKeepDefinitions=[(s,"Bit:99",rhs) | (s,_,rhs)<-lost]}
      write ".rhs" plan {ebpKeepDefinitions=[(s,t,"missing") | (s,t,_)<-lost]}
      write ".unknown" plan {ebpKeepDefinitions=[("missing","Bit:1","missing")]}
      let protected=[(s,enType n,rhs) | n<-egNodes pre, not(enEligible n),
                     Just s<-[enName n], enOp n=="definition", [rhs]<-[enOperands n]]
      check "fixture has protected output definitions" (not(null protected))
      write ".protected" plan {ebpKeepDefinitions=take 1 protected}
      putStrLn "PASS: native plan fixture and nine negative requests generated"
    ["verify", planFile, finalPath] -> do
      plan <- read <$> readFile planFile
      final <- readGraph finalPath
      let names=S.fromList [s | n<-egNodes final, enEligible n, Just s<-[enName n]]
      check "requested helper survived native lowering" (all (\(s,_,_)->S.member s names) (ebpKeepDefinitions plan))
      putStrLn "PASS: requested helper survives final typed scalar checks"
    _ -> fail "plan CAPTURE STEM OUT | verify PLAN FINAL-GRAPH"
