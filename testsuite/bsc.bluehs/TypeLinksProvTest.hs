-- Verifies that TypeLinks + TypeLinksProvenance capture evaluator semantic
-- provenance (PStepField, PStepElem, PStepVariantTest, PStepVariantPayload,
-- NLitEnum, NLitTag, NLitSlot, PFactConcat) in IExpand, serialize and
-- deserialize it through .ba (GenABin), and preserve it across AState,
-- InlineWires, InlineCReg, ADropDefs, AOpt (-opt-join-defs), and AVeriQuirks.
module Main (main) where

import Control.Monad (unless)
import qualified Data.ByteString as B
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr, stdout)

import ABin (ABin(..), ABinModInfo(..))
import ADropDefs (aDropDefs)
import AOpt (aOpt)
import ASourceTypes (anchorASPackage)
import AState (aState)
import ASyntax (AExpr, APackage(..), ASPackage(..))
import AVeriQuirks (aVeriQuirks)
import Error (initErrorHandle)
import Flags (Flags(..))
import GenABin (readABinFile)
import InlineCReg (aInlineCReg)
import InlineWires (aInlineWires)
import SourceProvenance (
    NamedLitInfo(..),
    ProvFact(..),
    ProvStep(..),
    SourceProvenance
    )
import TypeLinksDecl (
    tlBitsInstance,
    tlConEncodings,
    tlFieldTypes
    )

assert :: String -> Bool -> IO ()
assert msg ok =
    unless ok $ do
        hPutStrLn stderr ("FAIL: " ++ msg)
        exitFailure

passGroup :: String -> IO ()
passGroup msg = putStrLn ("PASS: " ++ msg)

allSteps :: SourceProvenance e -> [ProvStep]
allSteps pairs =
    [ s
    | (_, fs) <- pairs
    , f <- fs
    , s <- factSteps f
    ]
  where
    factSteps (PFactAccess _ ss) = ss
    factSteps _ = []

allLits :: SourceProvenance e -> [NamedLitInfo]
allLits pairs =
    [ li
    | (_, fs) <- pairs
    , f <- fs
    , li <- factLits f
    ]
  where
    factLits (PFactLit li) = [li]
    factLits (PFactArgLits kls) = map snd kls
    factLits _ = []

hasConcatFact :: SourceProvenance e -> Bool
hasConcatFact pairs =
    any isConcat [ f | (_, fs) <- pairs, f <- fs ]
  where
    isConcat (PFactConcat _) = True
    isConcat _ = False

checkProvCategories :: String -> SourceProvenance AExpr -> IO ()
checkProvCategories stage prov = do
    let steps = allSteps prov
        lits = allLits prov
        fieldNames = [ fn | PStepField { ps_field = fn } <- steps ]
        hasElem = any (\s -> case s of PStepElem {} -> True; _ -> False) steps
        variantTests = [ cn | PStepVariantTest { ps_con = cn } <- steps ]
        variantPayloads = [ cn | PStepVariantPayload { ps_con = cn } <- steps ]
        enumMembers = [ mem | NLitEnum { nli_member = mem } <- lits ]
        tagMembers = [ mem | NLitTag { nli_con = mem } <- lits ]
        slots = [ sl | NLitSlot { nli_slot = sl } <- lits ]

    assert (stage ++ ": non-empty provenance") (not (null prov))
    assert (stage ++ ": PStepField captures FrameHdr fields, got " ++ show fieldNames)
        ("fhTag" `elem` fieldNames && "fhLen" `elem` fieldNames)
    assert (stage ++ ": PStepElem captures Vector element selection") hasElem
    assert (stage ++ ": PStepVariantTest captures Outcome variant tests, got " ++ show variantTests)
        (not (null variantTests))
    assert (stage ++ ": PStepVariantPayload captures Outcome variant payloads, got " ++ show variantPayloads)
        ("OutHit" `elem` variantPayloads || "OutMiss" `elem` variantPayloads)
    assert (stage ++ ": NLitEnum captures Phase enum literals, got " ++ show enumMembers)
        (any (`elem` ["PhIdle", "PhLoad", "PhRun", "PhDone"]) enumMembers)
    assert (stage ++ ": NLitTag captures case-matched tag literals, got " ++ show tagMembers)
        (not (null tagMembers))
    assert (stage ++ ": NLitSlot captures Vector dynamic update slot literals, got " ++ show slots)
        (not (null slots))
    assert (stage ++ ": PFactConcat captures fused fhTag ++ fhLen slice")
        (hasConcatFact prov)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    errh <- initErrorHandle

    baBytes <- B.readFile "mkTypeLinksProvMod.ba"
    (apkg, schedInfo, pps, baFlags0) <-
        case fst (readABinFile errh "mkTypeLinksProvMod.ba" baBytes) of
            ABinMod abmi _ ->
                pure (abmi_apkg abmi, abmi_aschedinfo abmi, abmi_pps abmi, abmi_flags abmi)
            _ -> do
                hPutStrLn stderr "FAIL: expected ABinMod in mkTypeLinksProvMod.ba"
                exitFailure

    -- [1] Deserialized APackage from .ba carries full evaluator provenance
    checkProvCategories "APackage (.ba round-trip)" (apkg_provenance apkg)
    passGroup "[1] APackage (.ba round-trip) preserves field, elem, variant test/payload, enum/tag/slot literal, and concat provenance"

    -- [2] Thread provenance through AState -> InlineWires -> InlineCReg -> anchorASPackage
    let flags = baFlags0 { optJoinDefs = True }
    aspkg0 <- aState errh flags pps schedInfo apkg
    let (aspkgW, _, _) = aInlineWires flags aspkg0
        aspkgC = anchorASPackage (aInlineCReg flags aspkgW)
    checkProvCategories "ASPackage (post-AState/InlineWires/InlineCReg)" (aspkg_provenance aspkgC)
    passGroup "[2] ASPackage preserves evaluator provenance across AState, InlineWires, InlineCReg, and anchorASPackage"

    -- [3] Thread provenance through ADropDefs -> AOpt (-opt-join-defs) -> AVeriQuirks
    let aspkgDrop = anchorASPackage (aDropDefs aspkgC)
    aspkgOpt0 <- aOpt errh flags aspkgDrop
    let aspkgOpt = anchorASPackage aspkgOpt0
        aspkgQuirks = anchorASPackage (aVeriQuirks flags aspkgOpt)
    checkProvCategories "ASPackage (post-ADropDefs/AOpt -opt-join-defs/AVeriQuirks)" (aspkg_provenance aspkgQuirks)
    passGroup "[3] ASPackage preserves evaluator provenance across ADropDefs, AOpt (-opt-join-defs), and AVeriQuirks"

    -- [4] Ensure TypeLinksDecl exports link cleanly
    let _ = (tlFieldTypes, tlConEncodings, tlBitsInstance)
    passGroup "[4] TypeLinksDecl symbol-table query helpers linked cleanly"
