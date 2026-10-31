module Main(main) where

import Control.Exception(bracket_, evaluate, try, IOException)
import Control.Monad(unless)
import qualified Data.ByteString as BS
import System.Environment(getArgs, getEnv, setEnv)
import System.Directory(createDirectoryIfMissing, createFileLink, findExecutable)
import System.FilePath((</>))

import Backend(Backend(..))
import BinData(encode,decode)
import ECO.Replay
import ECO.Options (defaultECOOptions, ecoCapture)
import Flags(Flags(..))
import FlagsDecode(defaultFlags)
import SpeedyString(fromString, snapshotInternState, InternSnapshot(..))

assert :: String -> Bool -> IO ()
assert msg ok = unless ok (ioError (userError msg))

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["capture", dir] -> do
            createDirectoryIfMissing True dir
            let native = dir </> "input.ba"
                output = dir </> "output.v"
                dependency = dir </> "foreign.ba"
                flags = (defaultFlags "/original/library")
                          { backend = Just Verilog
                          , vdir = Just "/original/output"
                          , keepFires = True, keepInlined = True
                          , inlineBool = False, optSched = False
                          }
            -- Payload checks here are byte-preservation checks.  The compiler
            -- integration smoke test separately exercises the native decoder.
            BS.writeFile native (BS.pack [0,1,2,255,0])
            writeFile output "module m;\nendmodule\n"
            writeFile dependency "dependency"
            _ <- evaluate (fromString "unused-captured-string")
            let backendInfo = ReplayBackend "m"
                    ["original header"] ["conflicts"] ["BVI"] [dependency] [output]
            let captureFlags = flags { ecoOptions = defaultECOOptions { ecoCapture = Just dir } }
            assert "ECO controls changed native flag bytes" (encode captureFlags == encode flags)
            path <- captureReplay dir native captureFlags backendInfo
            manifest <- read <$> readFile path
            expected <- BS.readFile native
            actual <- BS.readFile (fiPath (rmABin manifest))
            assert "native bytes changed" (expected == actual)
            -- Each supported utility must identify bytes independently of
            -- escaped filename spelling. Restrict PATH to exercise the macOS
            -- shasum fallback even on machines which also have sha256sum.
            nativeIdentity <- fileIdentity native
            originalPath <- getEnv "PATH"
            mapM_ (\tool -> do
                executable <- findExecutable tool
                case executable of
                  Nothing -> return ()
                  Just executablePath -> do
                    let toolDirectory = dir </> "hash-" ++ tool
                    createDirectoryIfMissing True toolDirectory
                    createFileLink executablePath (toolDirectory </> tool)
                    bracket_ (setEnv "PATH" toolDirectory) (setEnv "PATH" originalPath) $
                      mapM_ (\name -> do
                        let unusual = dir </> name
                        BS.writeFile unusual expected
                        identity <- fileIdentity unusual
                        assert (tool ++ " filename escaping changed or rejected a file identity")
                            (fiSHA256 identity == fiSHA256 nativeIdentity))
                        ["space name.ba", "back\\slash.ba", "line\nfeed.ba"])
                ["sha256sum", "shasum"]
            bytes <- BS.readFile (fiPath (rmFlags manifest))
            let decodedFlags = decode bytes :: Flags
            assert "effective paths were remapped" (vdir decodedFlags == Just "/original/output")
            assert "native flags did not round trip" (BS.pack (encode decodedFlags) == BS.pack (encode flags))
            assert "native decode restored invocation-only ECO controls"
                (ecoOptions decodedFlags == defaultECOOptions)
        ["restore", path] -> do
            manifest <- prepareReplay path
            snapshot <- snapshotInternState
            assert "unused string omitted" ("unused-captured-string" `elem` map snd (internEntries snapshot))
            assert "header not preserved" (rbHeader (rmBackend manifest) == ["original header"])
            assert "dependencies not identified" (length (rmDependencies manifest) == 1)
            assert "outputs not preserved" (length (rmOutputs manifest) == 1)
            verifyReplayOutputs manifest (map (fiPath . snd) (rmOutputs manifest))
            let changed = path ++ ".changed-output"
            writeFile changed "changed logic"
            different <- try (verifyReplayOutputs manifest [changed]) :: IO (Either IOException ())
            assert "different replay output was accepted" (either (const True) (const False) different)
            missing <- try (verifyReplayOutputs manifest []) :: IO (Either IOException ())
            assert "missing replay output was accepted" (either (const True) (const False) missing)
        ["bad-schema", path] -> do
            manifest <- read <$> readFile path
            writeFile (path ++ ".bad") (show (manifest {rmSchema = "unsupported"}))
        ["bad-producer", path] -> do
            manifest <- read <$> readFile path
            writeFile (path ++ ".bad-producer") (show (manifest
                {rmProducer = (rmProducer manifest) {fiSHA256 = replicate 64 '0'}}))
        _ -> ioError (userError "capture DIR | restore MANIFEST | bad-schema MANIFEST | bad-producer MANIFEST")
