{-# LANGUAGE CPP #-}
-- Fresh full-chain verification is shared with replay and prefix selection.
module Main where
import ECOASTReplay
import ECOASTFinishOrder
import ECOASTSearch(searchWorkersFromEnvironment)
import ECO.Replay(FileIdentity(..),fileIdentity)
import Control.Monad(unless)
import System.Environment
import System.FilePath
main=do
 raw<-getArgs
 -- --workers N (or ECO_WORKERS) evaluates finishing candidates in N forked
 -- worker processes; the result is byte-identical to the sequential search.
 args<-case raw of
  "--workers":n:rest->setEnv"ECO_WORKERS"n>>return rest
  _->return raw
 _<-searchWorkersFromEnvironment
 case args of
  [manifestPath,flowPath,output]->do
   manifest<-fileIdentity manifestPath;script<-fileIdentity __FILE__
   verified<-replaySelectedFlow flowPath output
   result<-finishDeclarationOrderFrom manifest(script:replayReceipt verified:replayInputs verified)
     (replayOptions verified)(replayOld verified)(replayContext verified)(replayProgram verified)(output</>"finish")
   validateReplay verified
   mapM_(\expected->fileIdentity(fiPath expected)>>= \actual->unless(actual==expected)(fail"finishing input changed"))[manifest,script]
   putStrLn(foStatus result++": "++foReason result++"; raw "++show(foBeforeRaw result)++" -> "++show(foAfterRaw result)++
    ", substantive "++show(foBeforeNormalized result)++" -> "++show(foAfterNormalized result))
  _->fail"usage: finish-order.hs [--workers N] FINISHING_SDK_MANIFEST QUALIFIED_FLOW FRESH_OUTPUT"
