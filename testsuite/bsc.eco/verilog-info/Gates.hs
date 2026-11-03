-- Positive control: every captured ordinary program passes both emission
-- gates, and emits its captured bytes through them.
module Main where
import Control.Monad (forM_, unless)
import ECO.Replay (FileIdentity(..))
import ECO.VerilogInfo
import System.Environment (getArgs)
import System.FilePath ((</>), takeBaseName, takeDirectory, takeFileName)

main = do
    out:paths <- getArgs
    forM_ paths $ \path -> do
        snapshot <- readVerilogInfo path
        let program = viProgram snapshot
            label gate = gate ++ " rejected " ++ path ++ ": "
        either (fail . (label "lexical gate" ++)) return (lexicallyEmittable program)
        either (fail . (label "declaration gate" ++)) return (declaredBeforeUse program)
        emitted <- verifyVerilogInfo snapshot (out </> takeFileName (takeDirectory (takeDirectory path)) </> takeBaseName path)
        unless (fiSHA256 emitted == fiSHA256 (aeOutput (viEnvelope snapshot))) $ fail ("control bytes differ: " ++ path)
        putStrLn ("PASS: " ++ path)
    putStrLn ("GATES: " ++ show (length paths) ++ " captured programs pass both emission gates")
