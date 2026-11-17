-- Exercises SignalNaming / ISignalNaming / ASignalNaming cross-stage lowering
-- (IExpr, pre-AState AExpr, and post-AState/InlineWires/InlineCReg AExpr),
-- generated-suffix stripping, depth-bounded shallow hashing, h4/h8 base-36
-- collision resolution, the hard anchor invariant, and deterministic PRNG
-- matched-triple (IExpr, APackage AExpr, ASPackage AExpr) ExprName parity.
module Main (main) where

import Control.Monad (forM_, unless, when)
import Data.Bits (shiftL, shiftR, xor)
import qualified Data.ByteString as B
import Data.Char (isAsciiLower, isDigit)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Word (Word64)
import System.Environment (getEnv)
import System.Exit (exitFailure)
import System.IO (BufferMode(..), hPutStrLn, hSetBuffering, stderr, stdout)

import ABin (ABin(..), ABinModInfo(..))
import ASignalNaming (
    exprNameFromAExpr,
    mkSignalNamingEnvFromAPackage,
    mkSignalNamingEnvFromASPackage,
    signalAExpr
    )
import AState (aState)
import ASyntax (
    AExpr(..),
    APackage(..),
    ASPackage(..),
    AType(..),
    AVInst(..),
    defaultAId
    )
import BinUtil (readImports, replaceImportedSignatures)
import CSyntax (CImportedSignature(..), CPackage(..), CSignature(..))
import CtxRed (cCtxReduceIO)
import Deriving (derive)
import Error (ErrorHandle, bsError, initErrorHandle)
import FStringCompat (mkFString)
import FixupDefs (fixupDefs, mkDictBuckets, mkDictRedirects)
import Flags (Flags(..))
import FlagsDecode (adjustFinalFlags, decodeFlags, defaultFlags)
import GenABin (readABinFile)
import GenFuncWrap (genFuncWrap)
import GenWrap (WrapInfo(..), genWrap)
import IConv (iConvPackage)
import IExpand (iExpand)
import ISignalNaming (
    exprNameFromIExpr,
    mkSignalNamingEnvFromIModule,
    signalIExpr
    )
import ISimpDicts (iSimpDicts)
import ISimplify (iSimplify)
import ISyntax (
    IConInfo(..),
    IDef(..),
    IExpr(..),
    IModule(..),
    IPackage(..),
    IStateVar(..),
    IType(..)
    )
import ISyntaxUtil (itBit1, itBitN, itFun, itNat, itPair, itPrimUnit, itString)
import Id (
    Id,
    IdProp(..),
    addIdProp,
    getIdBaseString,
    getIdString,
    mkEnableId,
    mkIdCanFire,
    mkIdRule,
    mkIdWillFire,
    mkRdyId,
    mk_homeless_id,
    setBadId,
    setFromRHSId,
    setInferredId
    )
import InlineCReg (aInlineCReg)
import InlineWires (aInlineWires)
import IntLit (ilDec)
import Lex (LFlags(..), lexStart)
import LiftDicts (liftDictsPkg)
import MakeSymTab (cConvInst, mkSymTab)
import Parse (parse)
import ParseOp (parseOps)
import Parser.Classic (errSyntax, pPackage)
import PreIds (idPreludeRead, idPrimFst, idPrimPair, idPrimSnd, idPrimUnit)
import Prim (PrimOp(..))
import SignalNaming (
    AnchorCollision(..),
    AnchorKind(..),
    ExprName(..),
    ModuleNameItem(..),
    SignalExpr(..),
    SignalNamingEnv,
    SignalType(..),
    allocSignalTempName,
    assignModuleNames,
    classifyAnchorId,
    emptySignalNamingEnv,
    exprName,
    exprNameH4,
    exprNameH8,
    formatExprTempName,
    hasStructuredProvenance,
    hashSignalExprShallow,
    initSignalNameTable,
    isDefAnchorId,
    isDefTempId,
    isEnvAnchorId,
    isUnnamedTempId,
    lookupArgNamedLit,
    lookupCaseArmNamedLit,
    mkSignalNamingEnv,
    selectProvenanceSignalExpr,
    signalExprStem,
    signalHash4,
    signalHash8,
    stripGeneratedSuffixes
    )
import SourceProvenance (NamedLitInfo(..), ProvFact(..), ProvStep(..))
import Simplify (simplify)
import TypeCheck (cTypeCheck)
import VModInfo (VModInfo)

assert :: String -> Bool -> IO ()
assert label ok =
    if ok
    then return ()
    else do
        hPutStrLn stderr ("FAIL: " ++ label)
        exitFailure

passGroup :: String -> IO ()
passGroup msg = putStrLn ("PASS: " ++ msg)

mkTestId :: String -> Id
mkTestId = mk_homeless_id

mkTempId :: String -> Id
mkTempId s = setBadId (mk_homeless_id s)

mkRhsTempId :: String -> Id
mkRhsTempId s = setFromRHSId (mk_homeless_id s)

iValRef :: String -> Integer -> IExpr ()
iValRef nm w = ICon (mkTestId nm) (ICValue { iConType = itBitN w, iValDef = iZero w })

iZero :: Integer -> IExpr ()
iZero w = ICon (mkTestId "0") (ICInt { iConType = itBitN w, iVal = ilDec 0 })

iInt :: Integer -> Integer -> IExpr ()
iInt w v = ICon (mkTestId (show v)) (ICInt { iConType = itBitN w, iVal = ilDec v })

iPrim :: PrimOp -> IType -> [IType] -> [IExpr ()] -> IExpr ()
iPrim op fnTy ts es =
    IAps (ICon (mkTestId (show op)) (ICPrim { iConType = fnTy, primOp = op })) ts es

aDefRef :: String -> Integer -> AExpr
aDefRef nm w = ASDef (ATBit w) (mkTestId nm)

aIntLit :: Integer -> Integer -> AExpr
aIntLit w v = ASInt defaultAId (ATBit w) (ilDec v)

isLowerBase36Char :: Char -> Bool
isLowerBase36Char c = isDigit c || isAsciiLower c

-- Find a deterministic pair of distinct 64-bit integer constants whose
-- SignalExprs share (stem, h4) (via >=50-char stem truncation) while having
-- distinct 8-char shallow hashes h8.
findSameH4Pair :: SignalNamingEnv -> (SignalExpr, SignalExpr)
findSameH4Pair env = go M.empty 1
  where
    longVar :: SignalExpr
    longVar =
        SignalDef
            (SignalBit 64)
            "alpha_beta_gamma_delta_epsilon_zeta_eta_theta_iota"

    mkCand :: Integer -> SignalExpr
    mkCand k =
        SignalPrim (SignalBit 64) PrimAdd [longVar, SignalInt (SignalBit 64) k]

    go :: M.Map (String, String) (SignalExpr, String) -> Integer -> (SignalExpr, SignalExpr)
    go seen k =
        let se = mkCand k
            stem = signalExprStem env se
            sh = hashSignalExprShallow se
            h4 = signalHash4 sh
            h8 = signalHash8 sh
            key = (stem, h4)
        in case M.lookup key seen of
               Just (prevSe, prevH8)
                   | prevH8 /= h8 -> (prevSe, se)
               _ -> go (M.insert key (se, h8) seen) (k + 1)

lookupInstVmi :: String -> [AVInst] -> IO VModInfo
lookupInstVmi name avis =
    case [ avi_vmi avi | avi <- avis, getIdBaseString (avi_vname avi) == name ] of
        (vmi : _) -> return vmi
        [] -> do
            hPutStrLn stderr ("FAIL: missing AVInst " ++ name)
            exitFailure

elaborateEnvFromIModule :: ErrorHandle -> Flags -> IO (SignalNamingEnv, [(String, VModInfo)])
elaborateEnvFromIModule errh flags = do
    source <- readFile "SignalNamingMod.bs"
    let lflags = LFlags { lf_is_stdlib = False, lf_allow_sv_kws = True }
    cpkg <- case parse pPackage (lexStart lflags (mkFString "SignalNamingMod.bs") source) of
                Right ((p, _) : _) -> pure p
                Left (ss, ts) -> bsError errh [errSyntax (filter (not . null) ss) ts]
                Right [] -> bsError errh []
    (mimp@(CPackage _ _ _ impsigs _ _ _), binmap, _) <-
        readImports errh flags M.empty M.empty cpkg
    mop <- parseOps errh mimp
    symt00 <- mkSymTab errh mop
    (mfwrp, symt0, _) <- genFuncWrap errh flags True mop symt00
    (mwrp, gens) <- genWrap errh flags [] True mfwrp symt0
    symt1 <- mkSymTab errh mwrp
    mder <- derive errh flags symt1 mwrp
    symt2 <- mkSymTab errh mder
    (mctx, _, _) <- cCtxReduceIO errh flags symt2 False mder
    symt <- mkSymTab errh mctx
    (tmod, tcErrors, _) <- cTypeCheck errh flags symt (cConvInst errh symt mctx)
    when tcErrors $ bsError errh []
    let smod = simplify flags tmod
        (lifted, liftedDefs) =
            if liftDicts flags then liftDictsPkg errh flags symt smod else (smod, [])
    ipkg0 <- iConvPackage errh flags symt liftedDefs lifted
    let found =
            [ b
            | CImpSign _ _ (CSignature i _ _ _) <- impsigs
            , Just b <- [M.lookup (getIdString i) binmap]
            ]
        binmods = [(p, h) | (_, _, _, p, h) <- found]
    internalSymt <-
        mkSymTab errh (replaceImportedSignatures mctx [s | (_, _, s, _, _) <- found])
    let redirects = mkDictRedirects (mkDictBuckets binmods) ipkg0 binmods
        (imodf, alldefsList) = fixupDefs redirects ipkg0 binmods
        alldefs = M.fromList [(i, e) | IDef i _ e _ <- alldefsList]
        (imodsd, dictBodies) = iSimpDicts imodf
        IPackage _ _ _ defs = iSimplify alldefs dictBodies imodsd
    wi <- case [ w | w <- gens, getIdBaseString (mod_nm w) == "mkSignalNamingMod" ] of
              (w : _) -> pure w
              [] -> do
                  hPutStrLn stderr "FAIL: missing WrapInfo for mkSignalNamingMod"
                  exitFailure
    modDef <- case [ d | d@(IDef i _ _ _) <- defs, getIdBaseString i == getIdBaseString (wrapped_mod wi) ] of
                  (d : _) -> pure d
                  [] -> do
                      hPutStrLn stderr "FAIL: missing wrapped IDef for mkSignalNamingMod"
                      exitFailure
    imod <- iExpand errh flags internalSymt alldefs False (wi_prags wi) modDef
    let envI = mkSignalNamingEnvFromIModule flags imod
        instVmis = [ (getIdBaseString i, isv_vmi sv) | (i, sv) <- imod_state_insts imod ]
    return (envI, instVmis)

searchFlags :: FilePath -> IO Flags
searchFlags bluespecdir = do
    let (_, _, errs, f, rest) =
            decodeFlags ["-verilog", "-elab", "-p", ".:%/Libraries"] ([], [], [], defaultFlags bluespecdir)
        (_, errs', f') = adjustFinalFlags [] errs f
    unless (null errs' && null rest) $
        ioError (userError "cannot decode flags")
    pure f'

-- ============================================================
-- Deterministic SplitMix-style PRNG for matched-triple fuzzing
-- ============================================================

stepRng :: Word64 -> (Word64, Word64)
stepRng s0 =
    let s1 = s0 + 0x9e3779b97f4a7c15
        z0 = (s1 `xor` (s1 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z1 = (z0 `xor` (z0 `shiftR` 27)) * 0x94d049bb133111eb
        out = z1 `xor` (z1 `shiftR` 31)
    in (out, s1)

pickMod :: Int -> Word64 -> (Int, Word64)
pickMod n s =
    let (r, s') = stepRng s
    in (fromIntegral (r `mod` fromIntegral n), s')

mkDummySV :: VModInfo -> IStateVar ()
mkDummySV vmi =
    IStateVar
        { isv_is_arg = False
        , isv_is_user_import = False
        , isv_uid = 0
        , isv_vmi = vmi
        , isv_iargs = []
        , isv_type = itPrimUnit
        , isv_meth_types = []
        , isv_clocks = []
        , isv_resets = []
        , isv_isloc = []
        }

-- Generate a matched (IExpr, method-level AExpr, wire-level AExpr) triple of
-- width 8 bits or 1 bit that incorporates stage-specific representations
-- (PrimSelect vs PrimExtract, PrimTrunc vs slice, PrimZeroExt vs 0-concat,
-- PrimRange / PrimExpIf wrappers, Reg._read vs AMethCall vs ASPort regA,
-- FIFOF.first vs AMethValue vs ASPort fifo$D_OUT, RWire.wget[7:0] vs ASDef).
genTriple8 ::
    VModInfo ->
    VModInfo ->
    VModInfo ->
    Int ->
    Word64 ->
    ((IExpr (), AExpr, AExpr), Word64)
genTriple8 regVmi fifoVmi rwVmi depth s0
    | depth <= 0 = genLeaf8 regVmi fifoVmi rwVmi s0
    | otherwise =
        let (choice, s1) = pickMod 9 s0
        in case choice of
               0 -> genLeaf8 regVmi fifoVmi rwVmi s1
               1 ->
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       ((i2, m2, w2), s3) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s2
                       (opIdx, s4) = pickMod 5 s3
                       op = [PrimAdd, PrimSub, PrimAnd, PrimOr, PrimXor] !! opIdx
                       fnTy = itBitN 8 `itFun` (itBitN 8 `itFun` itBitN 8)
                   in ( ( iPrim op fnTy [ITNum 8] [i1, i2]
                        , APrim defaultAId (ATBit 8) op [m1, m2]
                        , APrim defaultAId (ATBit 8) op [w1, w2]
                        )
                      , s4
                      )
               2 ->
                   -- PrimSelect (IExpr) vs 32-bit PrimExtract (APackage) vs 3-bit PrimExtract + ZeroExt (ASPackage)
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       (loInt, s3) = pickMod 5 s2
                       lo = toInteger loInt
                       hi = lo + 3
                       iSlice4 = iPrim PrimSelect (itBitN 8 `itFun` itBitN 4) [ITNum 4, ITNum lo, ITNum 8] [i1]
                       iExt8 = iPrim PrimZeroExt (itBitN 4 `itFun` itBitN 8) [ITNum 4, ITNum 4, ITNum 8] [iSlice4]
                       mSlice4 = APrim defaultAId (ATBit 4) PrimExtract [m1, aIntLit 32 hi, aIntLit 32 lo]
                       mExt8 = APrim defaultAId (ATBit 8) PrimZeroExt [mSlice4]
                       wSlice4 = APrim defaultAId (ATBit 4) PrimExtract [w1, aIntLit 3 hi, aIntLit 3 lo]
                       wExt8 = APrim defaultAId (ATBit 8) PrimConcat [aIntLit 4 0, wSlice4]
                   in ((iExt8, mExt8, wExt8), s3)
               3 ->
                   -- PrimTrunc 4 ++ upper 4 slice
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       ((i2, m2, w2), s3) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s2
                       iLo = iPrim PrimTrunc (itBitN 8 `itFun` itBitN 4) [ITNum 4, ITNum 4, ITNum 8] [i1]
                       iHi = iPrim PrimSelect (itBitN 8 `itFun` itBitN 4) [ITNum 4, ITNum 4, ITNum 8] [i2]
                       iCat =
                           iPrim
                               PrimConcat
                               (itBitN 4 `itFun` (itBitN 4 `itFun` itBitN 8))
                               [ITNum 4, ITNum 4, ITNum 8]
                               [iHi, iLo]
                       mLo = APrim defaultAId (ATBit 4) PrimTrunc [m1]
                       mHi = APrim defaultAId (ATBit 4) PrimExtract [m2, aIntLit 32 7, aIntLit 32 4]
                       mCat = APrim defaultAId (ATBit 8) PrimConcat [mHi, mLo]
                       wLo = APrim defaultAId (ATBit 4) PrimExtract [w1, aIntLit 3 3, aIntLit 3 0]
                       wHi = APrim defaultAId (ATBit 4) PrimExtract [w2, aIntLit 3 7, aIntLit 3 4]
                       wCat = APrim defaultAId (ATBit 8) PrimConcat [aIntLit 0 0, wHi, wLo]
                   in ((iCat, mCat, wCat), s3)
               4 ->
                   -- PrimIf conditioned on a 1-bit comparison
                   let ((ic, mc, wc), s2) = genTriple1 regVmi fifoVmi rwVmi (depth - 1) s1
                       ((it, mt, wt), s3) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s2
                       ((ie, me, we), s4) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s3
                       ifTy = itBit1 `itFun` (itBitN 8 `itFun` (itBitN 8 `itFun` itBitN 8))
                   in ( ( iPrim PrimIf ifTy [itBitN 8] [ic, it, ie]
                        , APrim defaultAId (ATBit 8) PrimIf [mc, mt, me]
                        , APrim defaultAId (ATBit 8) PrimIf [wc, wt, we]
                        )
                      , s4
                      )
               5 ->
                   -- Transparent wrappers (PrimRange / PrimExpIf / PrimNosplitDeep)
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       iWrapped =
                           iPrim
                               PrimRange
                               (itNat `itFun` (itNat `itFun` (itBitN 8 `itFun` itBitN 8)))
                               []
                               [iInt 32 0, iInt 32 7, iPrim PrimExpIf (itBitN 8 `itFun` itBitN 8) [] [i1]]
                       mWrapped = APrim defaultAId (ATBit 8) PrimNosplitDeep [m1]
                   in ((iWrapped, mWrapped, w1), s2)
               6 ->
                   -- Full-width slice identity on IExpr vs raw AExpr
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       iFull = iPrim PrimSelect (itBitN 8 `itFun` itBitN 8) [ITNum 8, ITNum 0, ITNum 8] [i1]
                       mFull = APrim defaultAId (ATBit 8) PrimExtract [m1, aIntLit 32 7, aIntLit 32 0]
                   in ((iFull, mFull, w1), s2)
               7 ->
                   -- Tuple projection beta-reduction: PrimFst(PrimPair a b) == ATupleSel (ATuple [a, b]) 1
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       ((i2, m2, w2), s3) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s2
                       pairCon = ICon idPrimPair (ICTuple { iConType = itPrimUnit, fieldIds = [] })
                       iPair = IAps pairCon [itBitN 8, itBitN 8] [i1, i2]
                       iFst =
                           IAps
                               (ICon idPrimFst (ICSel { iConType = itPrimUnit `itFun` itBitN 8, selNo = 0, numSel = 2 }))
                               []
                               [iPair]
                       mTup = ATuple (ATTuple [ATBit 8, ATBit 8]) [m1, m2]
                       mFst = ATupleSel (ATBit 8) mTup 1
                   in ((iFst, mFst, w1), s3)
               _ ->
                   -- Unary bitwise invert / negate
                   let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi (depth - 1) s1
                       (uIdx, s3) = pickMod 2 s2
                       op = if uIdx == 0 then PrimInv else PrimNeg
                   in ( ( iPrim op (itBitN 8 `itFun` itBitN 8) [ITNum 8] [i1]
                        , APrim defaultAId (ATBit 8) op [m1]
                        , APrim defaultAId (ATBit 8) op [w1]
                        )
                      , s3
                      )

genTriple1 ::
    VModInfo ->
    VModInfo ->
    VModInfo ->
    Int ->
    Word64 ->
    ((IExpr (), AExpr, AExpr), Word64)
genTriple1 regVmi fifoVmi rwVmi depth s0 =
    let (choice, s1) = pickMod 3 s0
    in case choice of
           0 ->
               let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi depth s1
                   ((i2, m2, w2), s3) = genTriple8 regVmi fifoVmi rwVmi depth s2
                   (cIdx, s4) = pickMod 2 s3
                   op = if cIdx == 0 then PrimEQ else PrimULT
                   fnTy = itBitN 8 `itFun` (itBitN 8 `itFun` itBit1)
               in ( ( iPrim op fnTy [ITNum 8] [i1, i2]
                    , APrim defaultAId (ATBit 1) op [m1, m2]
                    , APrim defaultAId (ATBit 1) op [w1, w2]
                    )
                  , s4
                  )
           1 ->
               -- Dynamic 1-bit extraction with 32-bit vs 3-bit index
               let ((i1, m1, w1), s2) = genTriple8 regVmi fifoVmi rwVmi depth s1
                   iIdx = ICon (mkTestId "step_2") (ICMethArg { iConType = itBitN 3 })
                   iIdx32 = iPrim PrimZeroExt (itBitN 3 `itFun` itNat) [ITNum 3, ITNum 29, ITNum 32] [iIdx]
                   mIdx32 = APrim defaultAId (ATBit 32) PrimZeroExt [ASPort (ATBit 3) (mkTestId "step_2")]
                   wIdx3 = ASPort (ATBit 3) (mkTestId "step_2")
                   iDyn =
                       iPrim
                           PrimExtract
                           (itBitN 8 `itFun` (itNat `itFun` (itNat `itFun` itBit1)))
                           [ITNum 8, ITNum 32, ITNum 1]
                           [i1, iIdx32, iIdx32]
                   mDyn = APrim defaultAId (ATBit 1) PrimExtract [m1, mIdx32, mIdx32]
                   wDyn = APrim defaultAId (ATBit 1) PrimExtract [w1, wIdx3, wIdx3]
               in ((iDyn, mDyn, wDyn), s2)
           _ ->
               let ((i1, m1, w1), s2) = genTriple1 regVmi fifoVmi rwVmi (depth - 1) s1
                   inV op =
                       ( iPrim op (itBit1 `itFun` itBit1) [] [i1]
                       , APrim defaultAId (ATBit 1) op [m1]
                       , APrim defaultAId (ATBit 1) op [w1]
                       )
               in (inV PrimBNot, s2)

genLeaf8 ::
    VModInfo ->
    VModInfo ->
    VModInfo ->
    Word64 ->
    ((IExpr (), AExpr, AExpr), Word64)
genLeaf8 regVmi fifoVmi rwVmi s0 =
    let (choice, s1) = pickMod 6 s0
        iUnitVal = ICon idPrimUnit (ICTuple { iConType = itPrimUnit, fieldIds = [] })
        aUnitVal = ASInt idPrimUnit (ATBit 0) (ilDec 0)
    in case choice of
           0 ->
               -- regA._read vs AMethCall vs inlined ASPort regA
               let ie =
                       IAps
                           (ICon idPreludeRead (ICSel { iConType = itPrimUnit `itFun` itBitN 8, selNo = 0, numSel = 1 }))
                           []
                           [ICon (mkTestId "regA") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV regVmi })]
                   me = AMethCall (ATBit 8) (mkTestId "regA") idPreludeRead []
                   we = ASPort (ATBit 8) (mkTestId "regA")
               in ((ie, me, we), s1)
           1 ->
               -- fifo.first vs AMethValue vs ASPort fifo$D_OUT
               let ie =
                       IAps
                           (ICon (mkTestId "first") (ICSel { iConType = itPrimUnit `itFun` itBitN 8, selNo = 0, numSel = 1 }))
                           []
                           [ICon (mkTestId "fifo") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV fifoVmi }), iUnitVal]
                   me = AMethCall (ATBit 8) (mkTestId "fifo") (mkTestId "first") [aUnitVal]
                   we = ASPort (ATBit 8) (mkTestId "fifo$D_OUT")
               in ((ie, me, we), s1)
           2 ->
               -- rw.wget[7:0] across IExpr / AMethValue / post-InlineWires ASDef rw$wget
               let ie9 =
                       IAps
                           (ICon (mkTestId "wget") (ICSel { iConType = itPrimUnit `itFun` itBitN 9, selNo = 0, numSel = 1 }))
                           []
                           [ICon (mkTestId "rw") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV rwVmi })]
                   ie = iPrim PrimTrunc (itBitN 9 `itFun` itBitN 8) [ITNum 8, ITNum 1, ITNum 9] [ie9]
                   me = APrim defaultAId (ATBit 8) PrimTrunc [AMethValue (ATBit 9) (mkTestId "rw") (mkTestId "wget")]
                   we = APrim defaultAId (ATBit 8) PrimExtract [ASDef (ATBit 9) (mkTestId "rw$wget"), aIntLit 4 7, aIntLit 4 0]
               in ((ie, me, we), s1)
           3 ->
               -- Top-level method input port step_1
               let ie = ICon (mkTestId "step_1") (ICMethArg { iConType = itBitN 8 })
                   me = ASPort (ATBit 8) (mkTestId "step_1")
                   we = ASPort (ATBit 8) (mkTestId "step_1")
               in ((ie, me, we), s1)
           4 ->
               -- Local def reference with stage-different compiler suffixes (__h99 vs ___d42 vs _h1a2b)
               let ie = iValRef "shared_val__h99" 8
                   me = aDefRef "shared_val___d42" 8
                   we = aDefRef "shared_val_h1a2b" 8
               in ((ie, me, we), s1)
           _ ->
               -- Masked 8-bit constant (including negative/overflowed representations)
               let (k, s2) = pickMod 256 s1
                   v = toInteger k
               in ((iInt 8 (v + 512), aIntLit 8 v, aIntLit 8 (v + 1024)), s2)

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    bluespecdir <- getEnv "BLUESPECDIR"
    flags <- searchFlags bluespecdir
    errh <- initErrorHandle

    baBytes <- B.readFile "mkSignalNamingMod.ba"
    (apkg, schedInfo, pps, baFlags) <-
        case fst (readABinFile errh "mkSignalNamingMod.ba" baBytes) of
            ABinMod abmi _ ->
                pure (abmi_apkg abmi, abmi_aschedinfo abmi, abmi_pps abmi, abmi_flags abmi)
            _ -> do
                hPutStrLn stderr "FAIL: expected ABinMod in mkSignalNamingMod.ba"
                exitFailure

    aspkg0 <- aState errh baFlags pps schedInfo apkg
    let (aspkgW, _, _) = aInlineWires baFlags aspkg0
        aspkgC = aInlineCReg baFlags aspkgW

    regVmi <- lookupInstVmi "regA" (apkg_state_instances apkg)
    fifoVmi <- lookupInstVmi "fifo" (apkg_state_instances apkg)
    rwVmi <- lookupInstVmi "rw" (apkg_state_instances apkg)
    crVmi <- lookupInstVmi "cr" (apkg_state_instances apkg)

    let envSynth =
            mkSignalNamingEnv
                baFlags
                [ (mkTestId "regA", regVmi)
                , (mkTestId "fifo", fifoVmi)
                , (mkTestId "rw", rwVmi)
                , (mkTestId "cr", crVmi)
                ]
                [ mkTestId "rw$wget"
                , mkTestId "cr$port0__read"
                ]
                [ mkTestId "step_x"
                , mkTestId "step_idx"
                , mkTestId "peek"
                ]

    -- ============================================================
    -- [1] Cross-stage lowering equivalence
    -- ============================================================

    let ix8 = iValRef "x" 8
        ax8 = aDefRef "x" 8
        iSel = iPrim PrimSelect (itBitN 8 `itFun` itBitN 4) [ITNum 4, ITNum 2, ITNum 8] [ix8]
        iExt32 =
            iPrim
                PrimExtract
                (itBitN 8 `itFun` (itNat `itFun` (itNat `itFun` itBitN 4)))
                [ITNum 8, ITNum 32, ITNum 4]
                [ix8, iInt 32 5, iInt 32 2]
        aSel = APrim defaultAId (ATBit 4) PrimSelect [ax8]
        aExt0 = APrim defaultAId (ATBit 4) PrimExtract [ax8, aIntLit 32 3, aIntLit 32 0]
        aExt32 = APrim defaultAId (ATBit 4) PrimExtract [ax8, aIntLit 32 5, aIntLit 32 2]
        aExt3 = APrim defaultAId (ATBit 4) PrimExtract [ax8, aIntLit 3 5, aIntLit 3 2]
    assert "iSel == aExt32" (signalIExpr envSynth iSel == signalAExpr envSynth aExt32)
    assert "iExt32 == aExt3" (signalIExpr envSynth iExt32 == signalAExpr envSynth aExt3)
    assert "iSel == aExt3" (signalIExpr envSynth iSel == signalAExpr envSynth aExt3)
    assert "aSel == aExt0" (signalAExpr envSynth aSel == signalAExpr envSynth aExt0)
    assert "slice stem" (signalExprStem envSynth (signalIExpr envSynth iSel) == "x_BITS_5_TO_2")
    passGroup "[1a] PrimSelect / PrimExtract / index-truncation cross-stage equivalence"

    let iIdx32 = iValRef "idx32" 32
        aIdx32 = aDefRef "idx32" 32
        aIdxTrunc3 = APrim defaultAId (ATBit 3) PrimExtract [aIdx32, aIntLit 32 2, aIntLit 32 0]
        iDynExt =
            iPrim
                PrimExtract
                (itBitN 8 `itFun` (itNat `itFun` (itNat `itFun` itBitN 1)))
                [ITNum 8, ITNum 32, ITNum 1]
                [ix8, iIdx32, iIdx32]
        aDynExt = APrim defaultAId (ATBit 1) PrimExtract [ax8, aIdxTrunc3, aIdxTrunc3]
    assert "dynamic extract index truncation" (signalIExpr envSynth iDynExt == signalAExpr envSynth aDynExt)
    passGroup "[1b] Dynamic PrimExtract index truncation (32-bit -> log2(8) = 3-bit)"

    let iFullSel = iPrim PrimSelect (itBitN 8 `itFun` itBitN 8) [ITNum 8, ITNum 0, ITNum 8] [ix8]
        aFullExt = APrim defaultAId (ATBit 8) PrimExtract [ax8, aIntLit 32 7, aIntLit 32 0]
        iTrunc4 = iPrim PrimTrunc (itBitN 8 `itFun` itBitN 4) [ITNum 4, ITNum 4, ITNum 8] [ix8]
        aTrunc4 = APrim defaultAId (ATBit 4) PrimTrunc [ax8]
        iy4 = iValRef "y" 4
        ay4 = aDefRef "y" 4
        iZExt8 = iPrim PrimZeroExt (itBitN 4 `itFun` itBitN 8) [ITNum 4, ITNum 4, ITNum 8] [iy4]
        aZExt8 = APrim defaultAId (ATBit 8) PrimZeroExt [ay4]
        aCatZExt8 = APrim defaultAId (ATBit 8) PrimConcat [aIntLit 4 0, ay4]
    assert "full slice == x (IExpr)" (signalIExpr envSynth iFullSel == signalIExpr envSynth ix8)
    assert "full slice == x (AExpr)" (signalAExpr envSynth aFullExt == signalAExpr envSynth ax8)
    assert "iTrunc4 == aTrunc4" (signalIExpr envSynth iTrunc4 == signalAExpr envSynth aTrunc4)
    assert "aTrunc4 == aExt0" (signalAExpr envSynth aTrunc4 == signalAExpr envSynth aExt0)
    assert "iZExt8 == aZExt8" (signalIExpr envSynth iZExt8 == signalAExpr envSynth aZExt8)
    assert "aZExt8 == aCatZExt8" (signalAExpr envSynth aZExt8 == signalAExpr envSynth aCatZExt8)
    passGroup "[1c] Full-width slice, PrimTrunc, and PrimZeroExt normalization"

    let iRng =
            iPrim
                PrimRange
                (itNat `itFun` (itNat `itFun` (itBitN 8 `itFun` itBitN 8)))
                []
                [iInt 32 0, iInt 32 7, ix8]
        aRng = APrim defaultAId (ATBit 8) PrimRange [aIntLit 32 0, aIntLit 32 7, ax8]
        iStr = ICon (mkTestId "s") (ICString { iConType = itString, iStr = "ok" })
        aStr = ASStr defaultAId (ATString (Just 2)) "ok"
        iStrConv =
            iPrim
                PrimIntegerToBit
                (itNat `itFun` itBitN 16)
                [ITNum 16]
                [iPrim PrimStringToInteger (itString `itFun` itNat) [] [iStr]]
        aStrConv =
            APrim
                defaultAId
                (ATBit 16)
                PrimIntegerToBit
                [APrim defaultAId (ATBit 32) PrimStringToInteger [aStr]]
        iWrap = iPrim PrimExpIf (itBitN 8 `itFun` itBitN 8) [] [ix8]
        aWrap = APrim defaultAId (ATBit 8) PrimNosplitDeep [ax8]
    assert "PrimRange stripped (I/A)" (signalIExpr envSynth iRng == signalAExpr envSynth aRng)
    assert "PrimRange == x" (signalAExpr envSynth aRng == signalAExpr envSynth ax8)
    assert "Str->Int->Bit stripped (I/A)" (signalIExpr envSynth iStrConv == signalAExpr envSynth aStrConv)
    assert "ifWrapper stripped (I/A)" (signalIExpr envSynth iWrap == signalAExpr envSynth aWrap)
    assert "ifWrapper == x" (signalIExpr envSynth iWrap == signalAExpr envSynth ax8)
    passGroup "[1d] PrimRange, PrimIntegerToBit/PrimStringToInteger, and if-wrapper stripping"

    let iz4 = iValRef "z" 4
        az4 = aDefRef "z" 4
        iNestedCat =
            iPrim
                PrimConcat
                (itBitN 8 `itFun` (itBitN 8 `itFun` itBitN 16))
                []
                [ ix8
                , iPrim
                      PrimConcat
                      (itBitN 0 `itFun` (itBitN 8 `itFun` itBitN 8))
                      []
                      [ iZero 0
                      , iPrim PrimConcat (itBitN 4 `itFun` (itBitN 4 `itFun` itBitN 8)) [] [iy4, iz4]
                      ]
                ]
        aFlatCat = APrim defaultAId (ATBit 16) PrimConcat [ax8, ay4, az4]
    assert "nested PrimConcat + 0-bit == flat PrimConcat"
        (signalIExpr envSynth iNestedCat == signalAExpr envSynth aFlatCat)
    passGroup "[1e] Nested PrimConcat flattening and 0-bit operand removal"

    let pairCon = ICon idPrimPair (ICTuple { iConType = itPrimUnit, fieldIds = [] })
        iTriple =
            IAps
                pairCon
                [itBitN 8, itPrimUnit]
                [ ix8
                , IAps pairCon [itBitN 4, itBitN 4] [iy4, iz4]
                ]
        aTriple = ATuple (ATTuple [ATBit 8, ATBit 4, ATBit 4]) [ax8, ay4, az4]
        iSel2 =
            IAps
                (ICon idPrimFst (ICSel { iConType = itPrimUnit `itFun` itBitN 4, selNo = 0, numSel = 2 }))
                []
                [ IAps
                      (ICon idPrimSnd (ICSel { iConType = itPrimUnit `itFun` itPrimUnit, selNo = 1, numSel = 2 }))
                      []
                      [iTriple]
                ]
        aSel2 = ATupleSel (ATBit 4) (ASDef (ATTuple [ATBit 8, ATBit 4, ATBit 4]) (mkTestId "tup3")) 2
        iTupRef = ICon (mkTestId "tup3") (ICValue { iConType = itPair (itBitN 8) (itPair (itBitN 4) (itBitN 4)), iValDef = iZero 0 })
        iSel2Ref =
            IAps
                (ICon idPrimFst (ICSel { iConType = itPrimUnit `itFun` itBitN 4, selNo = 0, numSel = 2 }))
                []
                [ IAps
                      (ICon idPrimSnd (ICSel { iConType = itPrimUnit `itFun` itPrimUnit, selNo = 1, numSel = 2 }))
                      []
                      [iTupRef]
                ]
    assert "right-nested PrimPair == ATuple" (signalIExpr envSynth iTriple == signalAExpr envSynth aTriple)
    assert "beta tuple selection reduces to y" (signalIExpr envSynth iSel2 == signalAExpr envSynth ay4)
    assert "PrimFst(PrimSnd(tup3)) == ATupleSel tup3 2"
        (signalIExpr envSynth iSel2Ref == signalAExpr envSynth aSel2)
    passGroup "[1f] Right-nested PrimPair/ATuple, tuple selection, and PrimUnit arg dropping"

    let iUnitVal = ICon idPrimUnit (ICTuple { iConType = itPrimUnit, fieldIds = [] })
        aUnitVal = ASInt idPrimUnit (ATBit 0) (ilDec 0)
        iFifoFirst =
            IAps
                (ICon (mkTestId "first") (ICSel { iConType = itPrimUnit `itFun` itBitN 8, selNo = 0, numSel = 1 }))
                []
                [ICon (mkTestId "fifo") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV fifoVmi }), iUnitVal]
        aFifoMeth = AMethCall (ATBit 8) (mkTestId "fifo") (mkTestId "first") [aUnitVal]
        aFifoVal = AMethValue (ATBit 8) (mkTestId "fifo") (mkTestId "first")
        aFifoPort = ASPort (ATBit 8) (mkTestId "fifo$D_OUT")

        iRegRead =
            IAps
                (ICon idPreludeRead (ICSel { iConType = itPrimUnit `itFun` itBitN 8, selNo = 0, numSel = 1 }))
                []
                [ICon (mkTestId "regA") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV regVmi })]
        aRegMeth = AMethCall (ATBit 8) (mkTestId "regA") idPreludeRead []
        aRegPortQ = ASPort (ATBit 8) (mkTestId "regA$Q_OUT")
        aRegPortInlined = ASPort (ATBit 8) (mkTestId "regA")

        iRwGet =
            IAps
                (ICon (mkTestId "wget") (ICSel { iConType = itPrimUnit `itFun` itBitN 9, selNo = 0, numSel = 1 }))
                []
                [ICon (mkTestId "rw") (ICStateVar { iConType = itPrimUnit, iVar = mkDummySV rwVmi })]
        aRwVal = AMethValue (ATBit 9) (mkTestId "rw") (mkTestId "wget")
        aRwPort = ASPort (ATBit 9) (mkTestId "rw$wget")
        aRwInlinedDef = ASDef (ATBit 9) (mkTestId "rw$wget")

        aSub1 = AMethCall (ATBit 8) (mkTestId "rf") (mkTestId "sub") [aDefRef "addr1" 4]
        aSub2 = AMethCall (ATBit 8) (mkTestId "rf") (mkTestId "sub") [aDefRef "addr2" 4]

    assert "fifo.first IExpr == AMethCall (with dropped unit)"
        (signalIExpr envSynth iFifoFirst == signalAExpr envSynth aFifoMeth)
    assert "fifo.first AMethValue == ASPort fifo$D_OUT"
        (signalAExpr envSynth aFifoVal == signalAExpr envSynth aFifoPort)
    assert "fifo.first IExpr == ASPort fifo$D_OUT"
        (signalIExpr envSynth iFifoFirst == signalAExpr envSynth aFifoPort)
    assert "regA._read IExpr == AMethCall"
        (signalIExpr envSynth iRegRead == signalAExpr envSynth aRegMeth)
    assert "regA._read == ASPort regA$Q_OUT"
        (signalIExpr envSynth iRegRead == signalAExpr envSynth aRegPortQ)
    assert "regA$Q_OUT == inlined ASPort regA"
        (signalAExpr envSynth aRegPortQ == signalAExpr envSynth aRegPortInlined)
    assert "regA stem omits $Q_OUT"
        (signalExprStem envSynth (signalAExpr envSynth aRegPortQ) == "regA")
    assert "rw.wget IExpr == AMethValue == ASPort == post-InlineWires ASDef"
        (signalIExpr envSynth iRwGet == signalAExpr envSynth aRwVal &&
         signalAExpr envSynth aRwVal == signalAExpr envSynth aRwPort &&
         signalAExpr envSynth aRwPort == signalAExpr envSynth aRwInlinedDef)
    assert "argument-taking method calls stay distinct"
        (signalAExpr envSynth aSub1 /= signalAExpr envSynth aSub2 &&
         hashSignalExprShallow (signalAExpr envSynth aSub1) /=
         hashSignalExprShallow (signalAExpr envSynth aSub2))
    passGroup "[1g] Submodule, Reg Q_OUT, RWire, and CReg port resolution across IExpr/APackage/ASPackage"

    -- ============================================================
    -- [2] Suffix stripping, PD markers, & depth-bounded shallow hash
    -- ============================================================

    assert "strip ___d323" (stripGeneratedSuffixes "sum___d323" == "sum")
    assert "strip __q12" (stripGeneratedSuffixes "mux__q12" == "mux")
    assert "strip __f3" (stripGeneratedSuffixes "fn__f3" == "fn")
    assert "strip __h1042" (stripGeneratedSuffixes "quirk__h1042" == "quirk")
    assert "strip _d5 / _dm2 / _ds3"
        (stripGeneratedSuffixes "tmp_d5" == "tmp" &&
         stripGeneratedSuffixes "tmp_dm2" == "tmp" &&
         stripGeneratedSuffixes "tmp_ds3" == "tmp")
    assert "strip _h4 / all-alpha _h4 / _h8 / _h8_2"
        (stripGeneratedSuffixes "x_PLUS_1_h3a9f" == "x_PLUS_1" &&
         stripGeneratedSuffixes "x_habcd" == "x" &&
         stripGeneratedSuffixes "x_h3a9f7bc1" == "x" &&
         stripGeneratedSuffixes "x_h3a9f7bc1_2" == "x")
    assert "preserve SignalNaming operator literals"
        (stripGeneratedSuffixes "x_PLUS_1" == "x_PLUS_1" &&
         stripGeneratedSuffixes "x_BITS_7_TO_0" == "x_BITS_7_TO_0" &&
         stripGeneratedSuffixes "fnFFunc_0" == "fnFFunc_0" &&
         stripGeneratedSuffixes "rs_0" == "rs_0" &&
         stripGeneratedSuffixes "vs_0_0_0" == "vs_0_0_0")
    assert "preserve user double-underscore identifiers"
        (stripGeneratedSuffixes "pkt__hdr" == "pkt__hdr" &&
         stripGeneratedSuffixes "pkt__house" == "pkt__house")
    assert "preserve trailing PD markers across suffix stripping & hash placement"
        (stripGeneratedSuffixes "sig_DONT_TOUCH" == "sig_DONT_TOUCH" &&
         stripGeneratedSuffixes "sig___d42_SYNTH_KEEP" == "sig_SYNTH_KEEP" &&
         stripGeneratedSuffixes "sig_h1a2b_DONT_TOUCH" == "sig_DONT_TOUCH")
    let longPdExpr =
            SignalPrim
                (SignalBit 8)
                PrimAdd
                [ SignalDef (SignalBit 8) "very_long_datapath_stage_alpha_beta_gamma_delta_DONT_TOUCH"
                , SignalDef (SignalBit 8) "rhs_operand_omega_DONT_TOUCH"
                ]
        longPdStem = signalExprStem emptySignalNamingEnv longPdExpr
        pdEn = exprNameFromAExpr emptySignalNamingEnv (aDefRef "pipe_stage_DONT_TOUCH" 8)
        pdFormatted = formatExprTempName pdEn (exprNameH4 pdEn)
    assert "PD marker preserved at end on >=50 char stem truncation and _h4 placement"
        ("_DONT_TOUCH" `L.isSuffixOf` longPdStem &&
         length longPdStem <= 50 &&
         "_DONT_TOUCH" `L.isSuffixOf` pdFormatted &&
         stripGeneratedSuffixes pdFormatted == "pipe_stage_DONT_TOUCH")
    passGroup "[2a] stripGeneratedSuffixes and PD-marker placement/truncation"

    let fanoutBefore =
            signalAExpr
                envSynth
                (APrim defaultAId (ATBit 8) PrimAnd [aDefRef "leaf_h1234" 8, aDefRef "mask" 8])
        fanoutAfter =
            signalAExpr
                envSynth
                (APrim defaultAId (ATBit 8) PrimAnd [aDefRef "leaf_h9z8y" 8, aDefRef "mask" 8])
        fanoutLegacy =
            signalAExpr
                envSynth
                (APrim defaultAId (ATBit 8) PrimAnd [aDefRef "leaf___d323" 8, aDefRef "mask" 8])
        portCopy1 = SignalPort (SignalBit 8) "arr$D_OUT_1"
        portCopy2 = SignalPort (SignalBit 8) "arr$D_OUT_2"
    assert "depth-bounded shallow hash across child suffix edits"
        (hashSignalExprShallow fanoutBefore == hashSignalExprShallow fanoutAfter &&
         hashSignalExprShallow fanoutBefore == hashSignalExprShallow fanoutLegacy &&
         signalExprStem envSynth fanoutBefore == "leaf_AND_mask" &&
         signalExprStem envSynth fanoutAfter == "leaf_AND_mask")
    assert "multi-port submodule copies stay distinct in shallow hash"
        (hashSignalExprShallow portCopy1 /= hashSignalExprShallow portCopy2)
    passGroup "[2b] Shallow hash stays invariant across leaf suffix changes and preserves multi-port indices"

    -- ============================================================
    -- [3] Base-36 h4 default, clean user binders, & h8 collision upgrade
    -- ============================================================

    let sampleSe = signalAExpr envSynth (APrim defaultAId (ATBit 8) PrimAdd [ax8, aIntLit 8 1])
        sampleSh = hashSignalExprShallow sampleSe
        h4Sample = signalHash4 sampleSh
        h8Sample = signalHash8 sampleSh
    assert "h4/h8 format & prefix invariant"
        (length h4Sample == 4 &&
         length h8Sample == 8 &&
         all isLowerBase36Char h8Sample &&
         take 4 h8Sample == h4Sample)
    case assignModuleNames
             envSynth
             [ ItemUserBinder "u_clean" (mkTestId "clean_sum") (Just sampleSe)
             , ItemUserBinder "u_inf" (setInferredId (mkTestId "x_BITS_7_TO_0_")) (Just sampleSe)
             , ItemTemp "t0" (mkTempId "_d1") sampleSe
             ] of
        Right [("u_clean", uClean), ("u_inf", uInf), ("t0", nm0)] -> do
            assert "unique user binder stays clean" (uClean == "clean_sum")
            assert "inferred binder receives _h4 without double underscore"
                (uInf == "x_BITS_7_TO_0_h" ++ h4Sample)
            assert "default h4 temp name" (nm0 == "x_PLUS_1_h" ++ h4Sample)
        other -> assert ("unexpected assignModuleNames: " ++ show other) False
    passGroup "[3a] Clean user binders, IdPInferredName _h4 disambiguation, and base-36 h4 default"

    let (seColl1, seColl2) = findSameH4Pair envSynth
        stemColl = signalExprStem envSynth seColl1
        h4Coll = signalHash4 (hashSignalExprShallow seColl1)
        h8Coll1 = signalHash8 (hashSignalExprShallow seColl1)
        h8Coll2 = signalHash8 (hashSignalExprShallow seColl2)
    assert "collision pair shares (stem, h4) with distinct h8"
        (stemColl == signalExprStem envSynth seColl2 &&
         h4Coll == signalHash4 (hashSignalExprShallow seColl2) &&
         h8Coll1 /= h8Coll2)
    case assignModuleNames envSynth [ItemTemp "alone" (mkTempId "_d1") seColl1] of
        Right [("alone", nmAlone)] ->
            assert "alone uses h4" (nmAlone == stemColl ++ "_h" ++ h4Coll)
        other -> assert ("unexpected alone result: " ++ show other) False
    case assignModuleNames
             envSynth
             [ ItemTemp "c1" (mkTempId "_d1") seColl1
             , ItemTemp "c2" (mkRhsTempId "rhs2") seColl2
             ] of
        Right [("c1", nm1), ("c2", nm2)] ->
            assert "symmetric upgrade of both colliding (stem, h4) temps to h8"
                (nm1 == stemColl ++ "_h" ++ h8Coll1 &&
                 nm2 == stemColl ++ "_h" ++ h8Coll2 &&
                 nm1 /= nm2)
        other -> assert ("unexpected h8 upgrade result: " ++ show other) False
    case assignModuleNames
             envSynth
             [ ItemTemp "d1" (mkTempId "_d1") seColl1
             , ItemTemp "d2" (mkTempId "_d2") seColl1
             ] of
        Right [("d1", nm1), ("d2", nm2)] ->
            assert "identical (stem, h8) falls back to encounter-order _2"
                (nm1 == stemColl ++ "_h" ++ h8Coll1 &&
                 nm2 == stemColl ++ "_h" ++ h8Coll1 ++ "_2")
        other -> assert ("unexpected h8 ordinal result: " ++ show other) False
    passGroup "[3b] Symmetric h4 -> h8 upgrade on (stem, h4) collision and _2 ordinal on (stem, h8) collision"

    case initSignalNameTable envSynth [mkEnableId (mkTestId "step")] of
        Left err -> assert ("initSignalNameTable failed: " ++ show err) False
        Right tbl0 -> do
            let (inc1, tbl1) = allocSignalTempName envSynth seColl1 tbl0
                (inc2, tbl2) = allocSignalTempName envSynth seColl2 tbl1
                (inc3, _) = allocSignalTempName envSynth seColl2 tbl2
            assert "incremental table emits h4 then h8 then h8_2"
                (inc1 == stemColl ++ "_h" ++ h4Coll &&
                 inc2 == stemColl ++ "_h" ++ h8Coll2 &&
                 inc3 == stemColl ++ "_h" ++ h8Coll2 ++ "_2")
    passGroup "[3c] Incremental SignalNameTable allocation and collision handling"

    -- ============================================================
    -- [4] Hard anchor invariant & port-shadowing user binders
    -- ============================================================

    let envAnchors =
            mkSignalNamingEnv
                baFlags
                [(mkTestId "regA", regVmi), (mkTestId "fifo", fifoVmi)]
                [mkTestId "rw$wget"]
                [mkTestId "step_1"]
        envAnchorsFull =
            mkSignalNamingEnvFromASPackage
                baFlags
                (aspkgC { aspkg_parameters = [(mkTestId "PARAM_W", ATBit 8)] })
        idKeep = addIdProp (mkTestId "user_keep_sig") IdP_keepEvenUnused
        anchorCases =
            [ (mkTestId "step_1", AnchorPort)
            , (mkTestId "D_IN", AnchorPort)
            , (mkTestId "Q_OUT", AnchorPort)
            , (mkTestId "fifo$D_OUT", AnchorSubmodPort)
            , (mkEnableId (mkTestId "step"), AnchorEnableReady)
            , (mkRdyId (mkTestId "step"), AnchorEnableReady)
            , (mkTestId "PARAM_W", AnchorParam)
            , (mkTestId "CLK", AnchorClockResetInout)
            , (mkTestId "RST_N", AnchorClockResetInout)
            , (mkTestId "regA", AnchorStateInst)
            , (mkIdWillFire (mkIdRule (mkTestId "rl_pipe")), AnchorRuleFire)
            , (mkIdCanFire (mkIdRule (mkTestId "rl_pipe")), AnchorRuleFire)
            , (idKeep, AnchorExplicitKeep)
            ]
    mapM_
        (\(aid, expectedKind) -> do
            let poisoned = setFromRHSId (setBadId aid)
                nm = getIdBaseString aid
            assert ("classifyAnchorId " ++ nm)
                (classifyAnchorId envAnchorsFull aid == Just expectedKind)
            assert ("isEnvAnchorId " ++ nm) (isEnvAnchorId envAnchorsFull poisoned)
            assert ("not isUnnamedTempId " ++ nm) (not (isUnnamedTempId envAnchorsFull poisoned))
            case assignModuleNames
                     envAnchorsFull
                     [ItemAnchor nm poisoned] of
                Right [(_, assigned)] ->
                    assert ("ItemAnchor never suffixed: " ++ nm) (assigned == nm)
                other ->
                    assert ("unexpected ItemAnchor assignment for " ++ nm ++ ": " ++ show other) False
            case assignModuleNames
                     envAnchorsFull
                     [ItemTemp nm poisoned sampleSe] of
                Right [(_, assigned)]
                    | isDefAnchorId envAnchorsFull aid ->
                        assert ("def anchor never suffixed: " ++ nm) (assigned == nm)
                    | otherwise ->
                        assert ("non-def anchor disambiguated on local temp: " ++ nm) (assigned /= nm)
                other ->
                    assert ("unexpected ItemTemp assignment for " ++ nm ++ ": " ++ show other) False)
        anchorCases
    assert "envSynth isEnvAnchorId sanity" (isEnvAnchorId envAnchors (mkTestId "rw$wget"))
    passGroup "[4a] Hard anchor invariant across all 8 AnchorKinds (including bad/fromRHS flags)"

    let enStep = mkEnableId (mkTestId "step")
    case assignModuleNames
             envAnchorsFull
             [ ItemAnchor "first" enStep
             , ItemAnchor "dup" enStep
             ] of
        Left (AnchorCollision "EN_step" AnchorEnableReady AnchorEnableReady) ->
            return ()
        other ->
            assert ("expected AnchorCollision on EN_step, got: " ++ show other) False
    case assignModuleNames
             envAnchorsFull
             [ ItemAnchor "port" (mkTestId "step_1")
             , ItemUserBinder "local_expr" (mkTestId "step_1") (Just sampleSe)
             , ItemUserBinder "local_plain" (mkTestId "step_1") Nothing
             ] of
        Right [("port", "step_1"), ("local_expr", localExprNm), ("local_plain", "step_1_2")] ->
            assert "expr user binder shadowing port gets _h4"
                (localExprNm == "step_1_h" ++ h4Sample)
        other ->
            assert ("expected port preserved and local binders disambiguated, got: " ++ show other) False
    passGroup "[4b] Anchor collision detection and local user-binder port-shadowing disambiguation"

    -- ============================================================
    -- [5] End-to-end SignalNamingMod.bs IModule / APackage / ASPackage parity
    -- ============================================================

    (envI, instVmisI) <- elaborateEnvFromIModule errh flags
    let envA = mkSignalNamingEnvFromAPackage baFlags apkg
        envAS = mkSignalNamingEnvFromASPackage baFlags aspkgC

    assert "IModule discovered state instances" (not (null instVmisI))
    let mustBeAnchors =
            [ mkEnableId (mkTestId "step")
            , mkRdyId (mkTestId "step")
            , mkRdyId (mkTestId "peek")
            , mkTestId "CLK"
            , mkTestId "RST_N"
            , mkTestId "regA"
            , mkTestId "fifo$D_OUT"
            , mkTestId "fifo$EMPTY_N"
            , mkIdWillFire (mkIdRule (mkTestId "rl_pipe"))
            , mkIdCanFire (mkIdRule (mkTestId "rl_pipe"))
            ]
    mapM_
        (\aid -> do
            let s = getIdBaseString aid
            assert ("envI anchor " ++ s) (isEnvAnchorId envI aid && not (isUnnamedTempId envI aid))
            assert ("envA anchor " ++ s) (isEnvAnchorId envA aid && not (isUnnamedTempId envA aid))
            assert ("envAS anchor " ++ s) (isEnvAnchorId envAS aid && not (isUnnamedTempId envAS aid)))
        mustBeAnchors

    assert "real envI/envA/envAS regA read parity"
        (signalIExpr envI iRegRead == signalAExpr envA aRegMeth &&
         signalAExpr envA aRegMeth == signalAExpr envAS aRegPortInlined)
    assert "real envI/envA/envAS RWire wget parity"
        (signalIExpr envI iRwGet == signalAExpr envA aRwVal &&
         signalAExpr envA aRwVal == signalAExpr envAS aRwInlinedDef)
    let libBinderId = addIdProp (mkTestId "x") IdPNonUserBinder
        libShadowRegId = addIdProp (mkTestId "regA") IdPNonUserBinder
    assert "moved library binder stays a binder candidate"
        (not (isUnnamedTempId envI libBinderId) && not (isDefTempId envI libBinderId))
    assert "library binder shadowing state instance is disambiguated, never a def anchor"
        (not (isDefAnchorId envI libShadowRegId) && not (isDefAnchorId envA libShadowRegId))
    case assignModuleNames
             envI
             [ ItemUserBinder "shadow_expr" libShadowRegId (Just sampleSe)
             , ItemUserBinder "shadow_plain" libShadowRegId Nothing
             ] of
        Right [("shadow_expr", exprNm), ("shadow_plain", "regA_2")] ->
            assert "library binder shadowing state instance gets _h4"
                (exprNm == "regA_h" ++ h4Sample)
        other ->
            assert ("expected state-shadowing library binders disambiguated, got: " ++ show other) False
    passGroup "[5] End-to-end SignalNamingMod.bs IModule, APackage, and ASPackage environment & port parity"

    -- ============================================================
    -- [6] Deterministic PRNG randomized matched-triple ExprName parity
    -- ============================================================

    forM_ [1 :: Word64 .. 200] $ \seed -> do
        let seed0 = (seed * 0x517cc1b727220a95) `shiftL` 1
            ((ie, aeM, aeW), _) = genTriple8 regVmi fifoVmi rwVmi 4 seed0
            enI = exprNameFromIExpr envI ie
            enA = exprNameFromAExpr envA aeM
            enAS = exprNameFromAExpr envAS aeW
        assert ("matched triple seed " ++ show seed ++ " IExpr vs APackage: " ++ show (enI, enA))
            (enI == enA)
        assert ("matched triple seed " ++ show seed ++ " APackage vs ASPackage: " ++ show (enA, enAS))
            (enA == enAS)
    passGroup "[6] 200 deterministic PRNG matched-triple (IExpr, APackage AExpr, ASPackage AExpr) ExprName parity checks"

    -- ============================================================
    -- [7] Evaluator provenance stems & carrier-width digest stability
    -- ============================================================

    let stepOuter = PStepField "FrameHdr" "Pkg" "fhMeta" 1 3 False
        stepInner = PStepField "InnerMeta" "Pkg" "imCode" 0 2 False
        accNarrow =
            SignalAccess
                (SignalBit 8)
                (SignalDef (SignalBit 28) "hdrReg_hdbii")
                [stepOuter, stepInner]
        accWide =
            SignalAccess
                (SignalBit 8)
                (SignalDef (SignalBit 44) "hdrReg_hzzzz")
                [ PStepField "FrameHdr" "Pkg" "fhMeta" 1 5 False
                , PStepField "InnerMeta" "Pkg" "imCode" 0 4 False
                ]
        enNarrow = exprName envA accNarrow
        enWide = exprName envA accWide
    assert "nested struct projection stem"
        (en_stem enNarrow == "hdrReg_fhMeta_imCode")
    assert "nested struct projection ExprName stable across carrier widening & field-count growth"
        (enNarrow == enWide)
    assert "hasStructuredProvenance detects SignalAccess"
        (hasStructuredProvenance accNarrow)

    let elemVec4 =
            SignalAccess
                (SignalBit 8)
                (SignalMethCall (SignalBit 32) "regA" "regA$Q_OUT" Nothing [])
                [PStepElem 2 4]
        elemVec8 =
            SignalAccess
                (SignalBit 8)
                (SignalMethCall (SignalBit 64) "regA" "regA$Q_OUT" Nothing [])
                [PStepElem 2 8]
    assert "vector element projection stem & digest stable across vector resize"
        (exprName envA elemVec4 == exprName envA elemVec8 &&
         en_stem (exprName envA elemVec4) == "regA_2")

    let slotCmp2 =
            SignalPrim
                (SignalBit 1)
                PrimEQ
                [ SignalPort (SignalBit 2) "step_1"
                , SignalNamedLit (SignalBit 2) (NLitSlot 2 4 2 2)
                ]
        slotCmp4 =
            SignalPrim
                (SignalBit 1)
                PrimEQ
                [ SignalPort (SignalBit 4) "step_1"
                , SignalNamedLit (SignalBit 4) (NLitSlot 2 16 4 2)
                ]
    assert "NLitSlot comparison stem & digest stable across index-width growth"
        (exprName envA slotCmp2 == exprName envA slotCmp4 &&
         en_stem (exprName envA slotCmp2) == "step_1_EQ_2")
    passGroup "[7] Provenance-driven struct/vector/slot stems & carrier-width digest stability"

    -- ============================================================
    -- [8] Provenance-driven enum equality, case-arm recovery, & ambiguity fallback
    -- ============================================================

    let enumEq2 =
            SignalPrim
                (SignalBit 1)
                PrimEQ
                [ SignalDef (SignalBit 2) "fsReg_haaaa"
                , SignalNamedLit (SignalBit 2) (NLitEnum "FlowState" "Pkg" "FsFetch" 1 4 2 1)
                ]
        enumTag4 =
            SignalPrim
                (SignalBit 1)
                PrimEQ
                [ SignalDef (SignalBit 4) "fsReg_hbbbb"
                , SignalNamedLit (SignalBit 4) (NLitTag "FlowState" "Pkg" "FsFetch" 3 9 4 3 True)
                ]
        Just enumTest8 =
            selectProvenanceSignalExpr
                envA
                (SignalBit 1)
                (const False)
                id
                (const True)
                [ PFactAccess
                      (SignalDef (SignalBit 8) "fsReg_hcccc")
                      [PStepVariantTest "FlowState" "Pkg" "FsFetch" 3 8 True]
                ]
    assert "enum ==, enum tag literal, and custom-Bits PStepVariantTest share identical stem & digest"
        (en_stem (exprName envA enumEq2) == "fsReg_EQ_FsFetch" &&
         exprName envA enumEq2 == exprName envA enumTag4 &&
         exprName envA enumEq2 == exprName envA enumTest8)

    let ambigFacts =
            [ PFactAccess
                  (SignalDef (SignalBit 2) "rawCodeReg_haaaa")
                  [PStepVariantTest "ModeA" "Pkg" "MaRun" 1 3 True]
            , PFactAccess
                  (SignalDef (SignalBit 2) "rawCodeReg_haaaa")
                  [PStepVariantTest "ModeB" "Pkg" "MbFast" 1 3 True]
            ]
    assert "conflicting enum PStepVariantTest facts fall back cleanly"
        (selectProvenanceSignalExpr envA (SignalBit 1) (const False) id (const True) ambigFacts == Nothing &&
         lookupArgNamedLit 1 ambigFacts == Nothing &&
         lookupCaseArmNamedLit 1 2 1 ambigFacts == Nothing)

    let oneHotArmFacts =
            [ PFactAccess
                  (SignalDef (SignalBit 8) "ohReg_haaaa")
                  [PStepVariantTest "OneHotCmd" "Pkg" "OhRead" 3 8 True]
            ]
    assert "lookupCaseArmNamedLit recovers custom-Bits enum constructor from PrimEQ PStepVariantTest"
        (lookupCaseArmNamedLit 1 8 8 oneHotArmFacts ==
         Just (NLitEnum "OneHotCmd" "Pkg" "OhRead" 0 0 8 8))
    passGroup "[8] Provenance-driven enum equality, case-arm recovery, & ambiguity fallback"
