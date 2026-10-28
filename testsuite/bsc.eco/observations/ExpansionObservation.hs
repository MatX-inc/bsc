module Main where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.List (isInfixOf)
import ECO.Options (defaultECOOptions, ECOOptions(..))
import ECO.Provenance
import FStringCompat (mkFString)
import Id (getIdString, mkId)
import Position (noPosition)
import SpeedyString (snapshotInternState)

check :: String -> Bool -> IO ()
check label ok = unless ok (fail label)

fails :: String -> IO a -> IO ()
fails label action = do
    result <- try (action >> return ()) :: IO (Either SomeException ())
    check label (either (const True) (const False) result)

main :: IO ()
main = do
    let capture = defaultECOOptions {ecoCapture = Just "unused-output"}
        sameName = mkId noPosition (mkFString "same_module_signal")
        rendered = renderExpansionAliases
    _ <- evaluate (length (getIdString sameName))
    before <- snapshotInternState
    first <- captureExpansionAliases capture [(1,Just sameName,1),(2,Nothing,1)]
    after <- snapshotInternState
    check "detaching aliases does not allocate interned names" (before == after)

    -- A later expansion with the same source name cannot overwrite the first.
    -- Values can be rendered in either order, and rendering never consumes one.
    second <- captureExpansionAliases capture [(9,Just sameName,9)]
    secondText <- evaluate (rendered second)
    firstText <- evaluate (rendered first)
    check "repeated module/name retains distinct observations"
        ("\"pointer\":1," `isInfixOf` firstText &&
         not ("\"pointer\":9," `isInfixOf` firstText) &&
         "\"pointer\":9," `isInfixOf` secondText &&
         not ("\"pointer\":1," `isInfixOf` secondText))
    check "rendering is repeatable" (rendered first == firstText && rendered second == secondText)

    -- Ordinary compilation ignores the heap observation argument entirely.
    unavailable <- captureExpansionAliases defaultECOOptions (error "uncaptured heap forced")
    check "ordinary expansion has no captured aliases"
        (rendered unavailable == "{\"stage\":\"IExpand.eqPtrs\",\"availability\":\"unavailable\"}")
    check "uncaptured expansion cannot consume earlier observations" (rendered first == firstText)

    -- Compact pointer/representative fields are detached now; names remain
    -- deferred until the caller has completed ordinary output and its snapshot.
    fails "pointer thunk must detach during expansion"
        (captureExpansionAliases capture [(error "bad heap pointer",Nothing,0)])
    fails "representative thunk must detach during expansion"
        (captureExpansionAliases capture [(0,Nothing,error "bad representative")])
    delayed <- captureExpansionAliases capture [(3,Just (error "late name rendering"),3)]
    _ <- snapshotInternState
    fails "name is rendered only after the caller's snapshot"
        (evaluate (length (rendered delayed)))

    -- A compilation that aborts after detachment leaves no slot for a later
    -- invocation to find, even when no successful render happened in between.
    fails "aborted compilation is isolated" $ do
        _ <- captureExpansionAliases capture [(7,Just sameName,7)]
        fail "simulated later compilation failure"
    empty <- captureExpansionAliases capture []
    check "empty following capture cannot inherit aborted rows"
        ("\"rows\":[]}" `isInfixOf` rendered empty)
    check "failed expansion does not change another invocation" (rendered first == firstText)
    putStrLn "PASS: per-expansion aliases, exception isolation, eager detachment and deferred names"
