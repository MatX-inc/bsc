-- Fresh full-chain verification is shared with replay and prefix selection.
module Main where
import ECOASTReplay
import ECOASTFinishOrder
import System.Environment
import System.FilePath
main=do
 args<-getArgs
 case args of
  [rootsPath,flowPath,output]->do
   roots<-rootsFromConfig rootsPath
   verified<-replaySelectedFlow roots flowPath output
   result<-finishDeclarationOrderFrom(replayRoots verified)(replayReceipt verified:replayInputs verified)
     (replayOptions verified)(replayOld verified)(replayContext verified)(replayProgram verified)(output</>"finish")
   validateReplay verified
   putStrLn(foStatus result++": "++foReason result++"; raw "++show(foBeforeRaw result)++" -> "++show(foAfterRaw result)++
    ", substantive "++show(foBeforeNormalized result)++" -> "++show(foAfterNormalized result))
  _->fail"usage: finish-order.hs ROOTS.config QUALIFIED_FLOW FRESH_OUTPUT"
