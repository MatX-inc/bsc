-- | Running Lean 4 on generated files: finding the executable, a capped and timed run,
-- the verdict read from the exit code and output, and a library compiled once into a cache.
module Lean.Run
    ( LeanConf(..), defaultMemory, defaultTimeout, findLean
    , Verdict(..), verdictWord, verdictCode, cannotRunCode
    , Result(..), runLean, classify, checkFile, readFileUtf8
    , libraryFiles, prepareLibrary
    ) where

import Control.Concurrent (threadDelay)
import Control.Exception (IOException, try)
import Data.Bits (xor)
import Data.Char (isSpace)
import Data.List (intercalate, isInfixOf)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTime)
import Numeric (showHex)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, findExecutable,
    makeAbsolute, removeDirectoryRecursive, renameDirectory)
import System.Environment (getEnvironment)
import System.Exit (ExitCode(..))
import System.FilePath ((</>), (<.>), searchPathSeparator, takeDirectory)
import System.IO (IOMode(..), withFile)
import System.Posix.Process (getProcessID)
import System.Posix.Signals (signalProcess, sigKILL)
import System.Process (CreateProcess(..), StdStream(..), createProcess, getPid, getProcessExitCode, proc,
    readProcessWithExitCode, terminateProcess, ProcessHandle)
import qualified Data.ByteString as BS (foldl', readFile)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Encoding as TE (decodeUtf8With)
import qualified Data.Text.Encoding.Error as TEE (lenientDecode)

-- | How Lean is run: the executable, the directories on LEAN_PATH, the wall-clock limit in
-- seconds and the address-space cap in bytes.
data LeanConf = LeanConf { lcExe :: FilePath, lcPath :: [FilePath], lcTimeout :: Int, lcMemory :: Integer }

-- | 24 GiB: the cap every Lean run gets.
defaultMemory :: Integer
defaultMemory = 25769803776

defaultTimeout :: Int
defaultTimeout = 1800

-- | The Lean executable: the given path, then $LEAN, then @lean@ on PATH.
findLean :: Maybe FilePath -> IO (Either String FilePath)
findLean given = do
    env <- getEnvironment
    case (given, lookup "LEAN" env) of
        (Just p, _) -> named "--lean" p
        (Nothing, Just p) | not (null p) -> named "$LEAN" p
        _ -> maybe (Left "Lean not found (give --lean PATH, set LEAN, or put lean on PATH)") Right
                 <$> findExecutable "lean"
  where named what p = do
            ok <- doesFileExist p
            if ok then Right <$> makeAbsolute p
            else maybe (Left (what ++ " names " ++ p ++ ", which is not an executable")) Right <$> findExecutable p

-- | What a run shows: the file checks, decide or bv_decide finds a counterexample (bv_decide's possibly
-- spurious one too), the hypotheses' consistency check fails, the time runs out, or anything else.
data Verdict = Proved | Counterexample | Vacuous | TimedOut | Failed
    deriving (Eq, Show)

verdictWord :: Verdict -> String
verdictWord v = case v of
    Proved -> "proved"; Counterexample -> "counterexample"; Vacuous -> "vacuous"
    TimedOut -> "timeout"; Failed -> "error"

-- | The exit codes: 0 proved, 3 counterexample, 4 vacuous, 5 timeout, 6 error.
verdictCode :: Verdict -> Int
verdictCode v = case v of
    Proved -> 0; Counterexample -> 3; Vacuous -> 4; TimedOut -> 5; Failed -> 6

-- | Lean could not be run at all (no executable, or the library did not build).
cannotRunCode :: Int
cannotRunCode = 7

data Result = Result { rVerdict :: Verdict, rReason :: String, rWall :: Double }

-- | Run Lean (-j1, no async elaboration) in @dir@ with @args@, its output to @logf@, under the
-- memory cap (prlimit, else the shell's ulimit) and the timeout; Nothing when it timed out.
runLean :: LeanConf -> FilePath -> [String] -> FilePath -> IO (Maybe ExitCode, Double)
runLean c dir args logf = do
    prl <- findExecutable "prlimit"
    env0 <- getEnvironment
    let lp = intercalate [searchPathSeparator] (lcPath c)
        env' = ("LEAN_PATH", lp) : filter ((/= "LEAN_PATH") . fst) env0
        largs = ["-j1", "-DElab.async=false"] ++ args
        (cmd, cargs) = case prl of
            Just p -> (p, ("--as=" ++ show (lcMemory c)) : "--" : lcExe c : largs)
            Nothing -> ("/bin/sh", ["-c", "ulimit -v " ++ show (lcMemory c `div` 1024) ++ " && exec \"$0\" \"$@\"", lcExe c] ++ largs)
    withFile logf WriteMode $ \h -> do
        (_, _, _, ph) <- createProcess (proc cmd cargs) { cwd = Just dir, env = Just env', std_in = NoStream
                                                        , std_out = UseHandle h, std_err = UseHandle h }
        t0 <- getMonotonicTime
        r <- waitFor ph (t0 + fromIntegral (lcTimeout c))
        t1 <- getMonotonicTime
        return (r, t1 - t0)

-- | Poll (the RTS is not threaded, so a blocking wait could not time out).
waitFor :: ProcessHandle -> Double -> IO (Maybe ExitCode)
waitFor ph deadline = loop
  where
    loop = do
        r <- getProcessExitCode ph
        t <- getMonotonicTime
        case r of
            Just e -> return (Just e)
            Nothing | t > deadline -> kill >> return Nothing
                    | otherwise -> threadDelay 20000 >> loop
    kill = do
        terminateProcess ph
        t <- getMonotonicTime
        let grace = do
                r <- getProcessExitCode ph
                t' <- getMonotonicTime
                case r of
                    Just _ -> return ()
                    Nothing | t' > t + 5 -> getPid ph >>= maybe (return ()) (signalProcess sigKILL) >> threadDelay 20000 >> grace
                            | otherwise -> threadDelay 20000 >> grace
        grace

-- | The verdict from the exit code (Nothing: timed out) and the output: proved needs exit 0, no error,
-- no sorry and (with @axioms@) printed axioms, each propext, Classical.choice or Quot.sound.
classify :: Bool -> Maybe ExitCode -> String -> (Verdict, String)
classify axioms rc out = case rc of
    Nothing -> (TimedOut, "no verdict within the time limit")
    Just code
        | any (isInfixOf "succeeded but was expected to fail") errs -> (Vacuous, firstErr)
        | any cex errs -> (Counterexample, firstErr)
        | "out of memory" `isInfixOf` out || "std::bad_alloc" `isInfixOf` out -> (Failed, "out of memory")
        | not (null errs) -> (Failed, firstErr)
        | code /= ExitSuccess -> (Failed, "lean exited with " ++ show code)
        | any (isInfixOf "declaration uses `sorry`") ls -> (Failed, "a declaration uses sorry")
        | axioms && null axl -> (Failed, "no #print axioms output")
        | axioms && any null' axl -> (Failed, "a #print axioms list without its closing bracket")
        | axioms, (a : _) <- [ a | Just as <- axl, a <- as, a `notElem` ["propext", "Classical.choice", "Quot.sound"] ] ->
            (Failed, "an axiom outside the standard three: " ++ a)
        | otherwise -> (Proved, "")
  where
    ls = lines out
    -- an error line and the lines of its message
    errs = [ unlines (l : takeWhile cont rest) | (l : rest) <- tails' ls, ": error" `isInfixOf` l ]
    cont l = not (": error" `isInfixOf` l || ": warning" `isInfixOf` l || "depends on axioms" `isInfixOf` l)
    firstErr = case errs of { (e : _) -> takeWhile (/= '\n') e; [] -> "" }
    -- decide's false proposition; bv_decide's counterexample (a possibly spurious one included)
    cex e = "proved that the proposition" `isInfixOf` e || ("The prover found a" `isInfixOf` e && "counterexample" `isInfixOf` e)
    axl = axiomLists ls
    null' = maybe True (const False)

-- | Each @#print axioms@ list's items, read to its closing bracket (Lean wraps a long list, an axiom
-- per line); Nothing for a list the output ends inside.
axiomLists :: [String] -> [Maybe [String]]
axiomLists [] = []
axiomLists (l : rest)
    | "' does not depend on any axioms" `isInfixOf` l = Just [] : axiomLists rest
    | Just r <- after "' depends on axioms: [" l = close r rest
    | otherwise = axiomLists rest
  where
    close acc more = case break (== ']') acc of
        (body, _ : _) -> Just (items body) : axiomLists more
        _ -> case more of
            (m : ms) -> close (acc ++ " " ++ m) ms
            [] -> [Nothing]
    items s = filter (not . null) (map (filter (not . isSpace)) (splitOn ',' s))
    after pat s = case [ drop (length pat) t | t <- tails' s, take (length pat) t == pat ] of
        (t : _) -> Just t
        [] -> Nothing

tails' :: [a] -> [[a]]
tails' [] = []
tails' xs@(_ : r) = xs : tails' r

splitOn :: Char -> String -> [String]
splitOn ch s = case break (== ch) s of
    (a, _ : r) -> a : splitOn ch r
    (a, []) -> [a]

-- | Check one file: compile it (to @olean@ when given, for files that import it), its output
-- in @logf@.
checkFile :: LeanConf -> Bool -> FilePath -> Maybe FilePath -> FilePath -> IO Result
checkFile c axioms file olean logf = do
    absF <- makeAbsolute file
    let out = maybe [] (\o -> ["-o", o <.> "olean", "-i", o <.> "ilean"]) olean
    (rc, wall) <- runLean c (takeDirectory absF) (out ++ [absF]) logf
    txt <- readFileUtf8 logf
    let (v, why) = classify axioms rc txt
    return (Result v why wall)

-- | A file as UTF-8 (lenient), read strictly.
readFileUtf8 :: FilePath -> IO String
readFileUtf8 f = do
    b <- BS.readFile f
    let s = T.unpack (TE.decodeUtf8With TEE.lenientDecode b)
    length s `seq` return s

-- | The source files of a library, modules named in build order.
libraryFiles :: FilePath -> [String] -> [FilePath]
libraryFiles dir ms = [ dir </> m <.> "lean" | m <- ms ]

-- | A directory holding the library's .olean files: @dir@ itself when it has them all, else
-- the sources in @dir@ compiled once into @cache@, keyed by the Lean version and the sources.
prepareLibrary :: LeanConf -> FilePath -> [String] -> FilePath -> IO (Either String FilePath)
prepareLibrary c dir ms cache = do
    built <- and <$> mapM (\m -> doesFileExist (dir </> m <.> "olean")) ms
    srcs <- and <$> mapM doesFileExist (libraryFiles dir ms)
    if built then Right <$> makeAbsolute dir
    else if not srcs then return (Left ("the Lean library is not in " ++ dir ++ " (neither " ++ intercalate ", " [ m ++ ".olean" | m <- ms ] ++ " nor the sources)"))
    else do
        ver <- try (readProcessWithExitCode (lcExe c) ["--version"] "") :: IO (Either IOException (ExitCode, String, String))
        case ver of
            Right (ExitSuccess, v, _) -> do
                bodies <- mapM BS.readFile (libraryFiles dir ms)
                let key = showHex (foldl fnv fnvBasis (concatMap (\b -> [BS.foldl' step fnvBasis b]) bodies ++ [strHash v, strHash (unwords ms)])) ""
                    final = cache </> key
                there <- doesDirectoryExist final
                if there then return (Right final) else build final
            Right (_, _, e) -> return (Left ("lean --version failed: " ++ takeWhile (/= '\n') e))
            Left e -> return (Left ("cannot run " ++ lcExe c ++ ": " ++ show e))
  where
    fnvBasis = 14695981039346656037 :: Word64
    fnv h x = (h `xor` x) * 1099511628211
    step h b = (h `xor` fromIntegral b) * 1099511628211
    strHash = foldl (\h ch -> (h `xor` fromIntegral (fromEnum ch)) * 1099511628211) fnvBasis
    build final = do
        pid <- getProcessID
        let tmp = final ++ ".tmp" ++ show pid
        r <- try (createDirectoryIfMissing True tmp) :: IO (Either IOException ())
        case r of
            Left e -> return (Left ("cannot create the Lean library cache " ++ tmp ++ ": " ++ show e))
            Right () -> do
                absDir <- makeAbsolute dir
                let c' = c { lcPath = [tmp] }
                    go [] = return Nothing
                    go (m : rest) = do
                        let logf = tmp </> m <.> "log"
                        res <- checkFile c' False (absDir </> m <.> "lean") (Just (tmp </> m)) logf
                        txt <- readFileUtf8 logf
                        if rVerdict res /= Proved || "sorryAx" `isInfixOf` txt
                            then return (Just ("the Lean library's " ++ m ++ ".lean did not build: " ++ verdictWord (rVerdict res)
                                               ++ (if null (rReason res) then "" else " (" ++ rReason res ++ ")") ++ "; see " ++ logf))
                            else go rest
                bad <- go ms
                case bad of
                    Just err -> return (Left err)
                    Nothing -> do
                        -- another run may have built it meanwhile: keep that one
                        moved <- try (renameDirectory tmp final) :: IO (Either IOException ())
                        there <- doesDirectoryExist final
                        case moved of
                            Left e | not there -> return (Left ("cannot move the Lean library to " ++ final ++ ": " ++ show e))
                                   | otherwise -> removeDirectoryRecursive tmp >> return (Right final)
                            Right () -> return (Right final)
