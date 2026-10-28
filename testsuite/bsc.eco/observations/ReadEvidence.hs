module Main where
import Control.Exception (try,SomeException,evaluate)
import Control.Monad (unless)
import Data.List (isInfixOf)
import ECO.Replay
import System.Environment (getArgs)
import qualified Data.ByteString as BS
main=do
  [mode,path]<-getArgs
  Just envelope<-readReplayEvidence path
  text<-readFile (fiPath(rePayload envelope))
  let check label value=unless value(fail label)
  check "observation has both native checkpoints" (all (`isInfixOf` text) ["before-checkproofs","before-dropundet"])
  check "capture performed no matching" ("\"matchingRequiredDuringCapture\":false" `isInfixOf` text)
  check "observations do not authorize final assumptions" (all (`isInfixOf` text)
    ["\"assumptionsMayConstrainReplay\":false","\"usableAsRTLAssumptionWithoutReproof\":false"])
  check "heap aliases are bound observations" ("IExpand.eqPtrs" `isInfixOf` text)
  check "actual proof outcome retained" (if mode=="proved" then "\"classification\":\"compiler-proved\"" `isInfixOf` text
    else any (`isInfixOf` text) ["\"classification\":\"disproven\"","\"classification\":\"inconclusive\""])
  _<-evaluate (length text) -- close the lazy read handle before the tamper write
  bytes<-BS.readFile (fiPath(rePayload envelope))
  BS.appendFile (fiPath(rePayload envelope)) (BS.singleton 0)
  rejected<-try(readReplayEvidence path) :: IO(Either SomeException (Maybe ReplayEvidence))
  BS.writeFile (fiPath(rePayload envelope)) bytes
  check "changed observation payload rejected" (either (const True) (const False) rejected)
  putStrLn ("PASS: bound "++mode++" observations, explicit non-assumption status and tamper guard")
