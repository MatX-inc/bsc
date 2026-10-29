-- bluehs keep.hs OLD.pre OLD.final NEW.pre NEW.final CAPTURE_SHA PREOPT_SHA OUT
-- Pure proposal generation: the compiler and independent proof gate validate it.
module Main where
import ECOASTPlan
import ECO.Types
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

readGraph :: FilePath -> IO EcoGraph
readGraph path = do
    contents <- readFile path
    maybe (die ("invalid native ECO graph: " ++ path)) return (readMaybe contents)

main :: IO ()
main = do
    args <- getArgs
    case args of
      [op,ofin,np,nfin,captureHash,preoptHash,out] -> do
        oldPre <- readGraph op; oldFinal <- readGraph ofin
        newPre <- readGraph np; newFinal <- readGraph nfin
        proposal <- either die return (proposeKeeps oldPre oldFinal newPre newFinal)
        plan <- either die return (makeBackendPlan proposal captureHash preoptHash)
        writeFile out (show plan ++ "\n")
        writeFile (out ++ ".decisions") (show proposal ++ "\n")
        putStrLn ("proposed " ++ show (length (kpKeeps proposal)) ++ " typed helper keeps")
      _ -> die "usage: keep.hs OLD.pre OLD.final NEW.pre NEW.final CAPTURE_SHA PREOPT_SHA OUT"
