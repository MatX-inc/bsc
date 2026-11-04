-- | --check and --mutants: the files schedcheck writes, in Lean (each module's parts in build order,
-- children before parents), and each hardware mutant's file, which must fail.
module Lean.Sched.Check
    ( CheckOpts(..), writeEmitted, setupLean, checkFiles, reportCheck, pathBCode, oblOnlyCode
    , Checker, newChecker, checkModule, finishChecker
    ) where

import Control.Monad (forM, forM_, unless, when)
import Data.Char (isDigit, isSpace)
import Data.IORef (IORef, newIORef, readIORef, writeIORef, modifyIORef')
import Data.List (intercalate, isPrefixOf)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, removeDirectoryRecursive, removeFile)
import System.Environment (getEnvironment, getExecutablePath)
import System.FilePath ((</>), (<.>), dropExtension, takeBaseName, takeDirectory)
import System.IO (IOMode(..), hFlush, hPutStr, hPutStrLn, hSetEncoding, stderr, stdout, utf8, withFile)
import Text.Printf (printf)

import Lean.Run (LeanConf(..), Result(..), Verdict(..), cannotRunCode, checkFile, defaultMemory, findLean, prepareLibrary,
    readFileUtf8, verdictCode, verdictWord)
import Lean.Sched.Emit (Emitted(..), MutantFile(..), Proves(..), provesText)
import Lean.Sched.Time (Timer, phase)

data CheckOpts = CheckOpts { coLean :: Maybe FilePath, coLib :: Maybe FilePath, coTimeout :: Int, coTime :: Bool, coKeep :: Bool }

-- | The Lean library a modular file imports, in build order (src/comp/Makefile's SCHEDCHECKLIB reads this line).
libModules :: [String]
libModules = ["Modular", "EmitLemmas", "Scan", "LastCaller", "SiteCond"]

-- | A mutant file that checks; one not written, or failing in a definition rather than a theorem.
missedCode, unwrittenCode :: Int
missedCode = 8
unwrittenCode = 9

-- | A file that proves, but whose theorems rest on facts the solver did not confirm (path B); one that
-- proves its obligation only (no Acyc/top).
pathBCode, oblOnlyCode :: Int
pathBCode = 10
oblOnlyCode = 11

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

-- | The library: --lean-lib, else the installation's (@$BLUESPECDIR/schedcheck@), else the checkout's.
libDir :: Maybe FilePath -> IO (Either String FilePath)
libDir (Just d) = return (Right d)
libDir Nothing = do
    env <- getEnvironment
    exe <- getExecutablePath
    let cands = [ bd </> "schedcheck" | Just bd <- [lookup "BLUESPECDIR" env] ]
                ++ [ takeDirectory exe </> ".." </> ".." </> "util" </> "schedcheck" </> "theory" </> "modular" ]
    found <- filterM' (\d -> doesFileExist (d </> "Modular.lean")) cands
    return $ case found of
        (d : _) -> Right d
        [] -> Left ("the Lean library (Modular.lean) is in none of " ++ intercalate ", " cands ++ "; give --lean-lib DIR")
  where filterM' p xs = fmap concat (forM xs (\x -> p x >>= \b -> return [ x | b ]))

-- | Where compiled libraries are kept: $SCHEDCHECK_CACHE, else $XDG_CACHE_HOME/schedcheck, else
-- ~/.cache/schedcheck.
cacheDir :: IO FilePath
cacheDir = do
    env <- getEnvironment
    return $ case (lookup "SCHEDCHECK_CACHE" env, lookup "XDG_CACHE_HOME" env, lookup "HOME" env) of
        (Just d, _, _) | not (null d) -> d
        (_, Just d, _) | not (null d) -> d </> "schedcheck"
        (_, _, Just h) -> h </> ".cache" </> "schedcheck"
        _ -> ".schedcheck-cache"

logOf :: FilePath -> FilePath
logOf f = dropExtension f ++ ".check.log"

-- | One file in Lean, with the theorems' axioms required (@ax@), compiled to @ol@ when given.
checkOne :: CheckOpts -> LeanConf -> FilePath -> Bool -> Maybe FilePath -> IO Result
checkOne co c f ax ol = do
    hFlush stdout
    res <- checkFile c ax f ol (logOf f)
    when (coTime co) $ hPutStrLn stderr (printf "schedcheck: lean %s wall %.3f" f (rWall res))
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
    go [] = error "schedcheck: no file to check"
    go [f] = (\r -> (r, logOf f)) <$> checkOne co c f True (if isTop then Nothing else Just (dir </> takeBaseName f))
    go (f : rest) = do
        r <- checkOne co c f False (Just (dir </> takeBaseName f))
        if rVerdict r == Proved then go rest else return (r, logOf f)

-- | The check line: the verdict (for a file that proves, what it proves, when given), and on a failure its
-- reason (the first error, without the file's path) and the log.
reportCheck :: FilePath -> Maybe String -> (Result, FilePath) -> IO ()
reportCheck f proved (res, logf) = (>> hFlush stdout) $ putStrLn $ "check " ++ f ++ ": "
    ++ (if rVerdict res == Proved then maybe (verdictWord Proved) id proved
        else verdictWord (rVerdict res) ++ ": " ++ shortReason (rReason res) ++ " (log " ++ logf ++ ")")

-- | A file that proves: its check word unless top holds from Hyps alone, and the exit code.
provedAs :: Proves -> (Maybe String, Int)
provedAs pv = case provesText pv of
    Nothing -> (Nothing, 0)
    Just t | uncTotal pv > 0 -> (Just (t ++ "; not a proof of the module"), pathBCode)
           | otherwise -> (Just t, oblOnlyCode)

uncTotal :: Proves -> Int
uncTotal pv = let (a, b) = pvUnconf pv in a + b

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

-- | The run's code: its first failure's, which a path-B or obligation-only file's code gives way to.
setCode :: Checker -> Int -> IO ()
setCode ck rc = modifyIORef' (ckCode ck) (\old -> if old == 0 || (weak old && rc /= 0 && not (weak rc)) then rc else old)
  where weak c = c `elem` [pathBCode, oblOnlyCode]

-- | A module's written files (its parts, then its main file), then its mutants (each checked against
-- the real children), unless checking has stopped; a path-B or obligation-only file does not stop it.
checkModule :: Checker -> Bool -> [FilePath] -> Proves -> Int -> [MutantFile] -> IO ()
checkModule ck isTop paths pv nmut muts = do
    stopped <- readIORef (ckStopped ck)
    unless stopped $ do
        conf <- readIORef (ckConf ck) >>= maybe (setupLean (ckOpts ck) True >>= \r -> writeIORef (ckConf ck) (Just r) >> return r) return
        case conf of
            Left e -> do
                hPutStrLn stderr ("schedcheck: " ++ e)
                setCode ck cannotRunCode >> writeIORef (ckStopped ck) True
            Right c0 -> do
                createDirectoryIfMissing True (ckDir ck)
                let c = c0 { lcPath = lcPath c0 ++ [ckDir ck] }
                    mainF = last paths
                r@(res, _) <- checkParts (ckOpts ck) c (ckDir ck) isTop paths
                let (word, code) = provedAs pv
                reportCheck mainF word r
                if rVerdict res /= Proved
                    then do
                        when (nmut > 0) $ putStrLn ("mutants: not run (the " ++ (if ckHier ck then "files do" else "file does") ++ " not prove)")
                        setCode ck (verdictCode (rVerdict res)) >> writeIORef (ckStopped ck) True
                    else do
                        setCode ck code
                        when (nmut > 0) $ do
                            when (ckHier ck) $ putStrLn ("mutants of " ++ mainF ++ ":")
                            runMutants (ckOpts ck) c mainF (uncTotal pv > 0) nmut muts >>= setCode ck

-- | The run's exit code from Lean, its .olean directory removed.
finishChecker :: Checker -> IO Int
finishChecker ck = do
    ex <- doesDirectoryExist (ckDir ck)
    when ex $ removeDirectoryRecursive (ckDir ck)
    readIORef (ckCode ck)

-- | Each mutant's file (@<stem>_mut<i>.lean@), which must fail in a theorem (else counted apart), kept unless killed;
-- @weak@: a path-B file. Exit code 'missedCode', else 5 (no verdict), else 'unwrittenCode', else 0.
runMutants :: CheckOpts -> LeanConf -> FilePath -> Bool -> Int -> [MutantFile] -> IO Int
runMutants co c file weak asked muts = do
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
                (res, logf) <- checkFiles co c paths
                -- running out of memory is no verdict on the mutant, as a timeout is not
                let v0 = rVerdict res
                    v = if v0 == Failed && rReason res == "out of memory" then TimedOut else v0
                -- a kill counts when Lean's first error is in a theorem, not in a definition the edit broke
                at <- if v `elem` [Proved, TimedOut] then return Nothing else Just <$> firstErrorDecl logf
                let kept = coKeep co || v `elem` [Proved, TimedOut] || maybe False (not . isThm) at
                putStrLn (label ++ ": " ++ (case (v, at) of
                    (Proved, _) -> "MISSED (the file checks; kept " ++ path ++ ")"
                    (TimedOut, _) -> (if v0 == Failed then "out of memory" else "timeout") ++ " (kept " ++ path ++ ")"
                    (_, Just d) | not (isThm d) -> "killed (definition error, " ++ d ++ "; kept " ++ path ++ ")"
                    _ -> "killed (" ++ verdictWord v ++ ")"))
                unless kept $ forM_ paths $ \f -> do
                    removeFile f
                    ex <- doesFileExist (logOf f)
                    when ex (removeFile (logOf f))
                return (i, Just (v, maybe True isThm at))
    let missed = [ i | (i, Just (Proved, _)) <- outs ]
        timedOut = [ i | (i, Just (TimedOut, _)) <- outs ]
        defErr = [ i | (i, Just (v, False)) <- outs, v `notElem` [Proved, TimedOut] ]
        unwritten = [ i | (i, Nothing) <- outs ]
        killed = n - length missed - length timedOut - length unwritten - length defErr
    putStrLn ("mutants: " ++ show killed ++ " of " ++ show n ++ " killed"
              ++ concat [ "; missed: " ++ unwords (map show missed) | not (null missed) ]
              ++ concat [ "; no verdict: " ++ unwords (map show timedOut) | not (null timedOut) ]
              ++ concat [ "; definition error: " ++ unwords (map show defErr) | not (null defErr) ]
              ++ concat [ "; not written: " ++ unwords (map show unwritten) | not (null unwritten) ]
              ++ (if weak then " (weak evidence: the file's theorems rest on unconfirmed facts, under which a mutant may also prove)" else ""))
    return (if not (null missed) then missedCode else if not (null timedOut) then verdictCode TimedOut
            else if not (null unwritten && null defErr) then unwrittenCode else 0)
  where isThm d = "theorem " `isPrefixOf` d

-- | The declaration holding a log's first error ("theorem name", "def name", ...; "no error position" when
-- the log names none), from the file and line Lean prints.
firstErrorDecl :: FilePath -> IO String
firstErrorDecl logf = do
    txt <- readFileUtf8 logf
    case [ loc | l <- lines txt, Just loc <- [errLoc l] ] of
        [] -> return "no error position"
        ((f, n) : _) -> do
            ex <- doesFileExist f
            if not ex then return ("no file " ++ f) else do
                src <- readFileUtf8 f
                let above = reverse (take n (lines src))
                    decl = [ unwords (take 2 (words (dropMods l))) | l <- above, startsDecl l ]
                return (case decl of { (d : _) -> d; [] -> "no declaration" })
  where
    -- "/path/F.lean:12:3: error: ..." -> (path, 12)
    errLoc l = go "" l
      where go acc r@(':' : rest)
              | (ln@(_ : _), ':' : r2) <- span isDigit rest, (_ : _, ':' : ' ' : m) <- span isDigit r2, "error" `isPrefixOf` m =
                  Just (reverse acc, read ln)
              | otherwise = cont acc r
            go acc r = cont acc r
            cont acc (ch : r) = go (ch : acc) r
            cont _ [] = Nothing
    mods = ["noncomputable ", "private ", "protected ", "@[simp] ", "@[reducible] "]
    dropMods l = case [ m | m <- mods, m `isPrefixOf` l ] of
        (m : _) -> dropMods (drop (length m) l)
        [] -> l
    startsDecl l = case l of
        (ch : _) -> not (isSpace ch) && any (`isPrefixOf` dropMods l) ["theorem ", "lemma ", "def ", "abbrev ", "structure ", "instance ", "inductive ", "opaque "]
        [] -> False
