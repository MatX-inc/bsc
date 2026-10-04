-- Build after bsc-core and bsc-ba, then pass any fresh successful .bmod pair:
--   cabal exec -- ghc -package bsc-core -package bsc-ba VerifyMaterializePatch.hs
--   ./VerifyMaterializePatch path/to/module.bmod
-- This does not compile or change the supplied artifacts.
module Main (main) where

import ABin (ABin(..), ABinModInfo(..))
import AModule
import AMaterializePatch
import AScheduleInfo (AScheduleInfo(..), erdbToList)
import ASchedulePatch (applyASchedulePatch)
import AScheduleRelations (deriveExclusiveRulesDB)
import ASyntax
    ( APackage(..), ADef(..), aTBool, aTrue, aFalse )
import BinData (Bin, encode, decode)
import Error (initErrorHandle)
import FlagsDecode (defaultFlags)
import FStringCompat (mkFString)
import GenModule
import Id (Id, mkId, setIdPosition)
import Position (noPosition, newPosition)
import qualified PhaseConfig as PC

import Control.Monad (forM_, unless)
import qualified Data.ByteString as B
import System.Environment (getArgs)
import System.Exit (die)
import System.FilePath (replaceExtension)

sameEncoding :: Bin a => String -> a -> a -> IO ()
sameEncoding label expected actual = unless (encode expected == encode actual) $
    die ("FAIL: different encoded " ++ label)

expectLeft :: String -> Either String a -> IO ()
expectLeft _ (Left _) = return ()
expectLeft label (Right _) = die ("FAIL: " ++ label ++ " accepted")

require :: String -> Bool -> IO ()
require label ok = unless ok (die ("FAIL: " ++ label))

ident :: String -> Id
ident = mkId noPosition . mkFString

emptyOrdered :: AOrderedPatch a
emptyOrdered = AOrderedPatch Nothing []

emptyPatch :: APackage -> AMaterializePatch
emptyPatch body = AMaterializePatch
    { amp_module = apkg_name body
    , amp_backend = apkg_backend body
    , amp_defs = emptyOrdered
    , amp_rules = emptyOrdered
    , amp_interface = emptyOrdered
    , amp_instances = emptyOrdered
    }

checkRoundTrip :: String -> APackage -> APackage -> IO AMaterializePatch
checkRoundTrip label before after = do
    patch <- either die return (diffAMaterializePatch id before after)
    sameEncoding (label ++ " patch codec") patch
        (decode (B.pack (encode patch)))
    replay <- either die return (applyAMaterializePatch before patch)
    sameEncoding (label ++ " replay") after replay
    return patch

checkStructure :: APackage -> IO ()
checkStructure fixture = do
    -- Independent definitions make ordering, additions, and deletion checks
    -- deterministic even when a supplied real module has only one definition.
    let first = ADef (ident "__materialize_first") aTBool aTrue []
        second = ADef (ident "__materialize_second") aTBool aFalse []
        added = ADef (ident "__materialize_added") aTBool aTrue []
        body = fixture { apkg_local_defs = [first, second] }
        unchanged = emptyPatch body
        unknown = ident "__materialize_unknown"
        defsPatch ordered = unchanged { amp_defs = ordered }
    replay <- either die return (applyAMaterializePatch body unchanged)
    sameEncoding "empty patch" body replay
    expectLeft "wrong module" $
        applyAMaterializePatch body (unchanged { amp_module = unknown })

    -- Every collection rejects references which neither the input nor its
    -- update objects define. This catches a forgotten validation call at any
    -- of the four uses of the generic ordered-patch implementation.
    forM_ [ ("definition", unchanged { amp_defs = AOrderedPatch (Just [unknown]) [] })
          , ("rule", unchanged { amp_rules = AOrderedPatch (Just [unknown]) [] })
          , ("interface", unchanged { amp_interface = AOrderedPatch (Just [unknown]) [] })
          , ("instance", unchanged { amp_instances = AOrderedPatch (Just [unknown]) [] })
          ] $ \(label, patch) ->
        expectLeft ("unknown " ++ label) (applyAMaterializePatch body patch)

    expectLeft "duplicate output order" $ applyAMaterializePatch body $
        defsPatch (AOrderedPatch (Just [adef_objid first, adef_objid first]) [])
    expectLeft "duplicate updates" $ applyAMaterializePatch body $
        defsPatch (AOrderedPatch Nothing [first, first])
    expectLeft "unused update" $ applyAMaterializePatch body $
        defsPatch (AOrderedPatch (Just []) [first])
    expectLeft "addition omitted from output order" $ applyAMaterializePatch body $
        defsPatch (AOrderedPatch Nothing [added])
    expectLeft "duplicate input IDs" $ applyAMaterializePatch
        (body { apkg_local_defs = [first, first] }) unchanged

    reordered <- checkRoundTrip "reordered definitions" body
        (body { apkg_local_defs = [second, first] })
    require "reordering unnecessarily copied definition bodies" $
        null (aop_updates (amp_defs reordered))
    require "reordering omitted explicit order" $
        aop_order (amp_defs reordered) == Just [adef_objid second, adef_objid first]
    _ <- checkRoundTrip "addition and deletion" body
        (body { apkg_local_defs = [added, second] })

    -- Id equality ignores source position. Diffing with Eq would silently
    -- omit this update even though the artifact's encoded metadata changed.
    let moved = first { adef_objid = setIdPosition
                            (newPosition "materialize-metadata.bsv" 17 9)
                            (adef_objid first) }
    require "metadata test must keep ordinary Id equality" $
        adef_objid moved == adef_objid first
    require "metadata test must alter the encoding" $
        encode moved /= encode first
    metadata <- checkRoundTrip "metadata-only definition change" body
        (body { apkg_local_defs = [moved, second] })
    require "metadata update missing" $
        length (aop_updates (amp_defs metadata)) == 1
    require "metadata-only update changed output order" $
        aop_order (amp_defs metadata) == Nothing

    expectLeft "unrepresented package field change" $
        diffAMaterializePatch id body
            (body { apkg_is_wrapped = not (apkg_is_wrapped body) })

checkPair :: FilePath -> IO ()
checkPair filename = do
    errh <- initErrorHandle
    moduleBytes <- B.readFile (replaceExtension filename "bmod")
    scheduleBytes <- B.readFile (replaceExtension filename "bsched")
    let (amod, hash) = readBModFile errh filename moduleBytes
        saved = readBSchedFile errh filename scheduleBytes
    require "schedule is bound to supplied module" (hash == bs_module_hash saved)
    case saved of
        BSchedError {} -> die "expected a successful module/schedule pair"
        BSched {} -> do
            scheduled <- either die return $
                applyASchedulePatch (amod_body amod) (bs_patch saved)
            final <- either die return $
                applyAMaterializePatch scheduled (bs_materialization saved)
            rebuilt <- either die return $
                reconstructModule errh (PC.materializeConfig (defaultFlags "")) amod saved
            case rebuilt of
                ABinMod info _ -> do
                    sameEncoding "saved materialization result" final (abmi_apkg info)
                    let original = bs_schedule saved
                        expectedFinal = maybe original id (bs_final_schedule saved)
                        actualFinal = abmi_aschedinfo info
                        exclusive = deriveExclusiveRulesDB (amod_body amod)
                            (bsi_rule_uses_map original)
                            (bsi_rule_relation_db original)
                            (bs_method_order saved)
                    sameEncoding "saved final schedule" expectedFinal (toBSchedInfo actualFinal)
                    require "exclusive rules derive from the original scheduling facts" $
                        erdbToList exclusive == erdbToList (asi_exclusive_rules_db actualFinal)
                _ -> die "successful pair reconstructed an error"
            _ <- checkRoundTrip "real module materialization" scheduled final
            checkStructure scheduled
    putStrLn "PASS: materialization replay, exact metadata, order, and malformed-patch checks"

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filename] -> checkPair filename
        _ -> die "usage: VerifyMaterializePatch path/to/module.bmod"
