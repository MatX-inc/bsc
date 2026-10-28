-- Run with the compiler-matched bluehs library; no RTL parser is used.
module Main where
import ECOAST (readVerilogInfo)
import ECOASTInvariant
import System.Environment (getArgs)
import Text.Read (readMaybe)

main :: IO ()
main=do
  args<-getArgs
  case args of
    [snapshotPath,yosys,secondsText,directory] | Just seconds<-readMaybe secondsText ->do
      snapshot<-readVerilogInfo snapshotPath
      report<-consumeASTEvidence yosys seconds directory snapshot []
      writeInvariantOutputs directory report
      putStrLn (renderInvariantReport report)
    _->ioError (userError "Usage: bluehs invariants.hs SNAPSHOT YOSYS SECONDS-PER-OBLIGATION OUTPUT-DIRECTORY")
