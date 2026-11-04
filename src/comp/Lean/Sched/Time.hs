-- | --time-phases: wall and CPU time per phase, each phase's result forced
-- before its clock is read, with extraction and emission totals.
module Lean.Sched.Time (Timer, newTimer, phase, phaseNote, reportTotals, extractionPhases) where

import Control.Monad (when)
import Data.IORef (IORef, newIORef, readIORef, modifyIORef')
import GHC.Clock (getMonotonicTime)
import System.CPUTime (getCPUTime)
import System.IO (hPutStrLn, stderr)
import Text.Printf (printf)

data Timer = Timer Bool (IORef [(String, Double, Double)])

newTimer :: Bool -> IO Timer
newTimer on = Timer on <$> newIORef []

-- | Run a phase; the action must force its own result (evaluate/rnf).
phase :: Timer -> String -> IO a -> IO a
phase (Timer on ref) name act = do
    w0 <- getMonotonicTime
    c0 <- getCPUTime
    x <- act
    w1 <- getMonotonicTime
    c1 <- getCPUTime
    let cpu = fromIntegral (c1 - c0) / 1e12
    modifyIORef' ref ((name, w1 - w0, cpu) :)
    when on $ hPutStrLn stderr (printf "schedlean: phase %s wall %.3f cpu %.3f" name (w1 - w0) cpu)
    return x

-- | An extra line under the phases (a count, say).
phaseNote :: Timer -> String -> IO ()
phaseNote (Timer on _) s = when on $ hPutStrLn stderr ("schedlean: " ++ s)

extractionPhases :: [String]
extractionPhases = ["read", "astate", "extract", "bsc-facts", "hier", "hier-read"]

-- | Extraction is read through bsc-facts and the children's reading (hier); emission is everything after.
reportTotals :: Timer -> IO ()
reportTotals (Timer on ref) = when on $ do
    ps <- readIORef ref
    let tot sel = (sum [ w | (n, w, _) <- ps, sel n ], sum [ c | (n, _, c) <- ps, sel n ])
        line :: String -> (Double, Double) -> IO ()
        line n (w, c) = hPutStrLn stderr (printf "schedlean: total %s wall %.3f cpu %.3f" n w c)
    line "extraction" (tot (`elem` extractionPhases))
    line "emission" (tot (`notElem` extractionPhases))
