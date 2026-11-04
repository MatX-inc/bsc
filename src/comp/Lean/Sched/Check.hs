-- | --check and --mutants: the files schedlean writes, in Lean (each module's parts in build order,
-- children before parents), and each hardware mutant's file, which must fail.
module Lean.Sched.Check
    ( CheckOpts(..), libModules, writeParts, writeEmitted, setupLean, checkFiles, reportCheck, missedCode
    , Checker, newChecker, checkModule, finishChecker
    ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (intercalate)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath ((</>), (<.>), dropExtension, takeBaseName, takeDirectory)
import System.IO (IOMode(..), hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8, withFile)
import Text.Printf (printf)

import Lean.Run (LeanConf(..), Result(..), Verdict(..), cannotRunCode, checkFile, defaultMemory, findLean, prepareLibrary,
    verdictCode, verdictWord)
import Lean.Sched.Emit (Emitted(..), MutantFile(..))
import Lean.Sched.Time (Timer, phase)

data CheckOpts = CheckOpts { coLean :: Maybe FilePath, coLib :: Maybe FilePath, coTimeout :: Int, coTime :: Bool, coKeep :: Bool }

-- | The Lean library a modular file imports, in build order.
libModules :: [String]
libModules = ["Modular", "EmitLemmas", "Scan", "LastCaller", "SiteCond"]

-- | A mutant file that checks.
missedCode, unwrittenCode :: Int
missedCode = 8
unwrittenCode = 9

-- | Write a module's parts (part k at @<stem>_p<k>.lean@, the last at @file@); their paths in
-- build order.
writeParts :: FilePath -> [(Maybe Int, String)] -> IO [FilePath]
writeParts file parts = do
    let base = dropExtension file
        paths = [ (maybe file (\i -> base ++ "_p" ++ show i ++ ".lean") pn, t) | (pn, t) <- parts ]
    forM_ paths $ \(f, t) -> withFile f WriteMode $ \h -> hSetEncoding h utf8 >> hPutStr h t
    return (map fst (filter ((/= file) . fst) paths) ++ [file])

-- | A module's file as the emitter returned it: its notes, its parts written, then its summary lines.
writeEmitted :: Timer -> FilePath -> Emitted -> IO [FilePath]
writeEmitted timer file em = do
    let (notes, rest) = span (\l -> take 5 l == "note:") (emLog em)
    mapM_ putStrLn notes
    paths <- phase timer "write" $ writeParts file (emParts em)
    mapM_ putStrLn rest
    return paths

-- | Lean for a file: the executable, and for a modular file (@needLib@) the compiled library.
setupLean :: CheckOpts -> Bool -> IO (Either String LeanConf)
setupLean co needLib = do
    l <- findLean (coLean co)
    case l of
        Left e -> return (Left e)
        Right exe -> do
            let c = LeanConf exe [] (coTimeout co) defaultMemory
            if not needLib then return (Right c) else do
                d <- libDir (coLib co)
                case d of
                    Left e -> return (Left e)
                    Right dir -> do
                        cache <- cacheDir
                        fmap (\lib -> c { lcPath = [lib] }) <$> prepareLibrary c dir libModules cache

-- | The library: --lean-lib, else the installation's (@$BLUESPECDIR/schedlean@), else the checkout's.
libDir :: Maybe FilePath -> IO (Either String FilePath)
libDir (Just d) = return (Right d)
libDir Nothing = do
    env <- getEnvironment
    exe <- getExecutablePath
    let cands = [ bd </> "schedlean" | Just bd <- [lookup "BLUESPECDIR" env] ]
                ++ [ takeDirectory exe </> ".." </> ".." </> "util" </> "schedlean" </> "theory" </> "modular" ]
    found <- filterM' (\d -> doesFileExist (d </> "Modular.lean")) cands
    return $ case found of
        (d : _) -> Right d
        [] -> Left ("the Lean library (Modular.lean) is in none of " ++ intercalate ", " cands ++ "; give --lean-lib DIR")
  where filterM' p xs = fmap concat (forM xs (\x -> p x >>= \b -> return [ x | b ]))

-- | Where compiled libraries are kept: $SCHEDLEAN_CACHE, else $XDG_CACHE_HOME/schedlean, else
-- ~/.cache/schedlean.
cacheDir :: IO FilePath
cacheDir = do
    env <- getEnvironment
    return $ case (lookup "SCHEDLEAN_CACHE" env, lookup "XDG_CACHE_HOME" env, lookup "HOME" env) of
        (Just d, _, _) | not (null d) -> d
        (_, Just d, _) | not (null d) -> d </> "schedlean"
        (_, _, Just h) -> h </> ".cache" </> "schedlean"
        _ -> ".schedlean-cache"

logOf :: FilePath -> FilePath
logOf f = dropExtension f ++ ".check.log"

-- | One file in Lean, with the theorems' axioms required (@ax@), compiled to @ol@ when given.
checkOne :: CheckOpts -> LeanConf -> FilePath -> Bool -> Maybe FilePath -> IO Result
checkOne co c f ax ol = do
    hFlush stdout
    res <- checkFile c ax f ol (logOf f)
    when (coTime co) $ hPutStrLn stderr (printf "schedlean: lean %s wall %.3f" f (rWall res))
    return res

-- | Check a module's files in build order: each part compiled for the next (into @<main>.olean.d@),
-- the last one's theorems' axioms required. The result, and the log of the file it is about.
checkFiles :: CheckOpts -> LeanConf -> [FilePath] -> IO (Result, FilePath)
checkFiles co c paths = do
    let bdir = dropExtension (last paths) ++ ".olean.d"
        parts = init paths
    unless (null parts) $ createDirectoryIfMissing True bdir
    r <- checkParts co (if null parts then c else c { lcPath = lcPath c ++ [bdir] }) bdir True paths
    unless (null parts) $ removeDirectoryRecursive bdir
    return r

-- | A module's files in order, the parts compiled into @dir@ (the main file too unless @isTop@);
-- stops at the first that does not prove.
checkParts :: CheckOpts -> LeanConf -> FilePath -> Bool -> [FilePath] -> IO (Result, FilePath)
checkParts co c dir isTop paths = go paths
  where
    go [] = error "schedlean: no file to check"
    go [f] = (\r -> (r, logOf f)) <$> checkOne co c f True (if isTop then Nothing else Just (dir </> takeBaseName f))
    go (f : rest) = do
        r <- checkOne co c f False (Just (dir </> takeBaseName f))
        if rVerdict r == Proved then go rest else return (r, logOf f)

-- | The check line: the verdict, and on a failure its reason (the first error, without the
-- file's path) and the log.
reportCheck :: FilePath -> (Result, FilePath) -> IO ()
reportCheck f (res, logf) = (>> hFlush stdout) $ putStrLn $ "check " ++ f ++ ": " ++ verdictWord (rVerdict res)
    ++ (if rVerdict res == Proved then "" else ": " ++ shortReason (rReason res) ++ " (log " ++ logf ++ ")")

-- | "/path/F.lean:12:3: error: msg" -> "line 12: msg".
shortReason :: String -> String
shortReason s = go s
  where
    go r@(':' : rest) | (l@(_ : _), ':' : r2) <- span isDigit rest, (_ : _, ':' : ' ' : msg) <- span isDigit r2 = "line " ++ l ++ ": " ++ dropPre "error: " msg
                      | otherwise = continue r
    go r = continue r
    continue (_ : r) = go r
    continue [] = s
    dropPre p m = if take (length p) m == p then drop (length p) m else m

-- | Lean over a run's modules, children first (design.md §3.1), set up at the first check, children
-- compiled into one .olean directory; it stops at the first failure, whose exit code is the run's.
data Checker = Checker { ckOpts :: CheckOpts, ckHier :: Bool, ckDir :: FilePath
                       , ckConf :: IORef (Maybe (Either String LeanConf)), ckCode :: IORef Int, ckStopped :: IORef Bool }

-- | A checker whose .olean directory is the top file's @<stem>.olean.d@ (@hier@: several modules).
newChecker :: CheckOpts -> Bool -> FilePath -> IO Checker
newChecker co hier topFile = do
    c <- newIORef Nothing
    Checker co hier (dropExtension topFile ++ ".olean.d") c <$> newIORef 0 <*> newIORef False

setCode :: Checker -> Int -> IO ()
setCode ck rc = modifyIORef' (ckCode ck) (\old -> if old /= 0 then old else rc)

-- | A module's written files (its parts, then its main file), then its mutants (each checked against
-- the real children), unless checking has stopped.
checkModule :: Checker -> Bool -> [FilePath] -> Int -> [MutantFile] -> IO ()
checkModule ck isTop paths nmut muts = do
    stopped <- readIORef (ckStopped ck)
    unless stopped $ do
        conf <- readIORef (ckConf ck) >>= maybe (setupLean (ckOpts ck) True >>= \r -> writeIORef (ckConf ck) (Just r) >> return r) return
        case conf of
            Left e -> do
                hPutStrLn stderr ("schedlean: " ++ e)
                setCode ck cannotRunCode >> writeIORef (ckStopped ck) True
            Right c0 -> do
                createDirectoryIfMissing True (ckDir ck)
                let c = c0 { lcPath = lcPath c0 ++ [ckDir ck] }
                    mainF = last paths
                r@(res, _) <- checkParts (ckOpts ck) c (ckDir ck) isTop paths
                reportCheck mainF r
                if rVerdict res /= Proved
                    then do
                        when (nmut > 0) $ putStrLn ("mutants: not run (the " ++ (if ckHier ck then "files do" else "file does") ++ " not prove)")
                        setCode ck (verdictCode (rVerdict res)) >> writeIORef (ckStopped ck) True
                    else when (nmut > 0) $ do
                        when (ckHier ck) $ putStrLn ("mutants of " ++ mainF ++ ":")
                        runMutants (ckOpts ck) c mainF nmut muts >>= setCode ck

-- | The run's exit code from Lean, its .olean directory removed.
finishChecker :: Checker -> IO Int
finishChecker ck = do
    ex <- doesDirectoryExist (ckDir ck)
    when ex $ removeDirectoryRecursive (ckDir ck)
    readIORef (ckCode ck)

-- | Each mutant's file (@<stem>_mut<i>.lean@), which must fail (one not written is counted apart); a missed
-- or undecided one's files kept. Exit code 'missedCode', else 5 (no verdict), else 'unwrittenCode', else 0.
runMutants :: CheckOpts -> LeanConf -> FilePath -> Int -> [MutantFile] -> IO Int
runMutants co c file asked muts = do
    let n = length muts
    when (n < asked) $ putStrLn ("mutants: " ++ show n ++ " candidate(s), fewer than the " ++ show asked ++ " asked for")
    outs <- forM (zip [1 :: Int ..] muts) $ \(i, mf) -> do
        let path = dropExtension file ++ "_mut" ++ show i <.> "lean"
            label = "mutant " ++ show i ++ " of " ++ show n ++ ", " ++ mfKind mf ++ ": " ++ mfWhat mf
        r <- mfEmit mf path
        case r of
            Left stop -> do
                putStrLn (label ++ ": not written (the emitter stops: " ++ takeWhile (/= '\n') stop ++ ")")
                return (i, Nothing)
            Right em -> do
                paths <- writeParts path (emParts em)
                (res, _) <- checkFiles co c paths
                -- running out of memory is no verdict on the mutant, as a timeout is not
                let v0 = rVerdict res
                    v = if v0 == Failed && rReason res == "out of memory" then TimedOut else v0
                    kept = coKeep co || v `elem` [Proved, TimedOut]
                putStrLn (label ++ ": " ++ (case v of
                    Proved -> "MISSED (the file checks; kept " ++ path ++ ")"
                    TimedOut -> (if v0 == Failed then "out of memory" else "timeout") ++ " (kept " ++ path ++ ")"
                    _ -> "killed (" ++ verdictWord v ++ ")"))
                unless kept $ forM_ paths $ \f -> do
                    removeFile f
                    ex <- doesFileExist (logOf f)
                    when ex (removeFile (logOf f))
                return (i, Just v)
    let missed = [ i | (i, Just Proved) <- outs ]
        timedOut = [ i | (i, Just TimedOut) <- outs ]
        unwritten = [ i | (i, Nothing) <- outs ]
        killed = n - length missed - length timedOut - length unwritten
    putStrLn ("mutants: " ++ show killed ++ " of " ++ show n ++ " killed"
              ++ concat [ "; missed: " ++ unwords (map show missed) | not (null missed) ]
              ++ concat [ "; no verdict: " ++ unwords (map show timedOut) | not (null timedOut) ]
              ++ concat [ "; not written: " ++ unwords (map show unwritten) | not (null unwritten) ])
    return (if not (null missed) then missedCode else if not (null timedOut) then verdictCode TimedOut
            else if not (null unwritten) then unwrittenCode else 0)
