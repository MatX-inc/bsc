module Main(main) where

import Control.Monad (unless)
import Data.List (isInfixOf)
import ECO.Options
import SpeedyString (snapshotInternState)

main :: IO ()
main = do
    before <- snapshotInternState
    let accepted arguments expected = unless (parseECOOptions arguments == Right expected) $
            fail ("unexpected invocation: " ++ show arguments)
        rejected arguments diagnostic = case parseECOOptions arguments of
            Left actual | diagnostic `isInfixOf` actual -> return ()
            actual -> fail ("accepted invalid invocation or wrong diagnostic: " ++ show (arguments, actual))
    accepted [] (CompileInvocation Nothing [])
    accepted ["-u","-eco-capture","captures","-verilog","Top.bs"]
        (CompileInvocation (Just "captures") ["-u","-verilog","Top.bs"])
    accepted ["-eco-output","out","-eco-replay","saved.replay"]
        (ReplayInvocation "saved.replay" "out")
    accepted ["-eco-adapt","saved.replay","-eco-output","out"]
        (AdaptInvocation "saved.replay" "out")
    mapM_ (uncurry rejected)
        [ (["-eco-output","out"], "requires -eco-replay")
        , (["-eco-replay","missing.replay"], "requires -eco-output")
        , (["-eco-adapt","missing.replay"], "requires -eco-output")
        , (["-eco-replay","saved.replay","-eco-output","out","-u"], "ordinary arguments are not allowed")
        , (["-eco-adapt","saved.replay","-eco-output","out","-u"], "ordinary arguments are not allowed")
        , (["-eco-replay","saved.replay","-eco-output","out","-eco-capture","captures"], "cannot be combined")
        , (["-eco-adapt","saved.replay","-eco-output","out","-eco-capture","captures"], "cannot be combined")
        , (["-eco-adapt","saved.replay","-eco-replay","other.replay","-eco-output","out"], "cannot be combined")
        , (["-eco-replay","a","-eco-replay","b","-eco-output","out"], "duplicate ECO option")
        , (["-eco-capture"], "missing argument")
        , (["-eco-graph","graphs","-eco-replay","saved.replay","-eco-output","out"], "unknown ECO option")
        , (["-eco-unknown","value"], "unknown ECO option") ]
    let replay = invocationOptions (ReplayInvocation "saved.replay" "out")
    unless (replay == defaultECOOptions {ecoReplay=Just "saved.replay", ecoOutput=Just "out"}) $
        fail "validated invocation changed runtime option projection"
    after <- snapshotInternState
    unless (before == after) $ fail "CLI mode validation touched the compiler interner"
    putStrLn "PASS: invocation modes, required paths, option conflicts and pre-restore string-only validation"
