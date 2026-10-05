module Main_bsc(main, hmain) where

import System.Environment(getArgs, getProgName)
import System.IO(stdout, stderr, hSetBuffering, BufferMode(LineBuffering),
                 hSetEncoding, utf8)
import Data.List(intersperse)
import Control.Monad(when)

import CompilerInvocation
import qualified BuildPlan as BP
import DependencyReport
import Error(internalError, initErrorHandle, setErrorHandleFlags,
             bsError, bsWarning, exitOK)
import Exceptions(bsCatch)
import FileNameUtil(baseName)
import Flags(Flags(..), verbose)
import FlagsDecode(Decoded(..), decodeArgs, showFlags, showFlagsRaw,
                   exitWithUsage, exitWithHelp, exitWithHelpHidden)
import IOUtil(getEnvDef)
import TopUtils(putStrLnF, dfltBluespecDir)
import Version(bscVersionStr, copyright)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stderr LineBuffering
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    args <- getArgs
    -- bsc can raise exception,  catch them here  print the message and exit out.
    bsCatch (hmain args)

-- Use with hugs top level
hmain :: [String] -> IO ()
hmain args = do
    pprog <- getProgName
    cdir <- getEnvDef "BLUESPECDIR" dfltBluespecDir
    bscopts <- getEnvDef "BSC_OPTIONS" ""
    let args' = words bscopts ++ args
    -- reconstruct original command line (modulo whitespace)
    -- add a newline at the end so it is offset
    let cmdLine = concat ("Invoking command line:\n" : (intersperse " " (pprog:args'))) ++ "\n"
    let showPreamble flags = do
          when (verbose flags) $ putStrLnF (bscVersionStr True)
          when (verbose flags) $ putStrLnF copyright
          when ((verbose flags) || (printFlags flags)) $ putStrLnF cmdLine
          when ((printFlags flags) || (printFlagsHidden flags)) $
                putStrLnF (showFlags flags)
          when (printFlagsRaw flags) $ putStrLnF (showFlagsRaw flags)
    let (warnings, decoded) = decodeArgs (baseName pprog) args' cdir
    errh <- initErrorHandle
    let doWarnings = when ((not . null) warnings) $ bsWarning errh warnings
        setFlags = setErrorHandleFlags errh
    let (dependencyOutput, operation) = case decoded of
          DDependencies path invocation -> (Just path, invocation)
          _ -> (Nothing, decoded)
    case compilerInvocation errh operation of
        Just invocation -> do
            let flags = invocationFlags invocation
                mode = invocationMode invocation
                plan = invocationPlan invocation
            setFlags flags
            doWarnings
            case dependencyOutput of
                Nothing -> do
                    showPreamble flags
                    BP.executePlan plan
                Just path -> do
                    result <- tryDependency (BP.discoverDependencies mode plan)
                    let report = case result of
                          Right value -> value
                          Left reason -> (emptyReport mode)
                            { dependencyIncomplete = [reason] }
                    writeDependencyReport path report
            -- A dependency query succeeds when its report is written;
            -- consumers must check completeness before trusting its inputs.
            exitOK errh
        Nothing -> case operation of
            DHelp flags ->
                do { setFlags flags; doWarnings;
                     exitWithHelp errh pprog args' cdir }
            DHelpHidden flags ->
                do { setFlags flags; doWarnings;
                     exitWithHelpHidden errh pprog args' cdir }
            DUsage -> exitWithUsage errh pprog
            DError msgs -> bsError errh msgs
            DNoSrc flags ->
                -- XXX we want to allow "bsc -v" and similar calls
                -- XXX to print version, etc, but if there are other flags
                -- XXX on the command-line we probably should report an error
                do { setFlags flags; doWarnings; showPreamble flags;
                     exitOK errh }
            _ -> internalError "compiler invocation without a plan"
