{-# LANGUAGE BangPatterns #-}
module SignalNaming (
    -- IR-independent lowered signal types, expressions, and ExprName
    SignalType(..),
    SignalExpr(..),
    ExprName(..),
    signalExprType,

    -- Per-module naming environment (free of ISyntax, ASyntax, and Ord Id)
    SignalNamingEnv(..),
    emptySignalNamingEnv,
    mkSignalNamingEnv,
    withExplicitAnchors,
    withHeapNames,
    wireInfoSpecialNames,
    ifcBoundaryNames,
    isClockOrResetPortName,

    -- Pure VModInfo port-spelling helpers
    qoutPortNameStr,
    splitPortIndexNums,
    isRegVModInfo,
    resolveMethPortString,
    lowerResolvedMethodRead,
    resolveOutputGate,
    resolveOutputOsc,
    mkSignalPort,
    mkDefOrPort,
    cleanGenBase,
    isInfixBinPrim,

    -- Algebraic & structural normalizers
    mkSignalInt,
    mkSignalNat,
    mkSignalExtract,
    mkSignalConcat,
    mkSignalTuple,
    mkSignalTupleSel,

    -- Suffix stripping, PD markers, and ExprName rendering
    stripGeneratedSuffixes,
    dropGeneratedSuffixes,
    splitTrailingPDMarker,
    signalStemLimit,
    signalExprStem,
    exprName,
    exprNameStem,
    exprNameH4,
    exprNameH8,
    formatSignalTempName,
    formatExprTempName,

    -- Shallow depth-bounded hashing and base-36 formatting
    shallowSignalExpr,
    hashSignalExprFull,
    hashSignalExprShallow,
    signalHash4,
    signalHash8,

    -- Anchor classification and hard anchor invariant
    AnchorKind(..),
    AnchorCollision(..),
    classifyAnchorId,
    isEnvAnchorId,
    isDefAnchorId,
    isUnnamedTempId,
    isDefTempId,
    isDirectNamedSignalExpr,
    provisionalBinderBase,

    -- Module-wide and incremental collision-resolved naming
    ModuleNameItem(..),
    assignModuleNames,
    SignalNameTable,
    emptySignalNameTable,
    initSignalNameTable,
    registerSignalAnchor,
    reserveExistingSignalName,
    allocSignalTempName
) where

import Data.Bits (shiftR, xor)
import Data.Char (chr, isAlphaNum, isAsciiLower, isDigit, isSpace, ord, toUpper)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Set as S
import Data.Word (Word64)

import Eval (NFData(..))
import Flags (Flags, removeCReg, removeCross, removeRWire, removeReg)
import FlagsDecode (defaultFlags)
import GenWrapUtils (genSuffix, isGenId)
import Id (
    Id,
    IdProp(..),
    getIdBaseString,
    getIdString,
    hasIdProp,
    isBadId,
    isFromRHSId,
    isIdCanFire,
    isIdEnable,
    isIdWillFire,
    isInferredId,
    isRdyId,
    unQualId
    )
import IntegerUtil (mask)
import Log2 (log2)
import PPrint (PPrint(..), text)
import PreIds (idClock, idPreludeRead, idReset)
import Prim (PrimOp(..))
import VModInfo (
    VArgInfo(..),
    VClockInfo(..),
    VFieldInfo(..),
    VModInfo,
    VName(..),
    VPort,
    VWireInfo(..),
    VeriPortProp(..),
    extractNames,
    getOutputClockPortTable,
    getOutputResetPortTable,
    getVNameString,
    lookupIfcInoutWire,
    lookupOutputClockWires,
    lookupOutputResetWire,
    vClk,
    vFields,
    vName
    )

-- ============================================================
-- IR-independent lowered signal types, expressions, and ExprName
-- ============================================================

-- Hardware type representation shared across ISyntax and ASyntax.
-- Stores Strings rather than Ids so Eq/Ord never consult intern-table order.
data SignalType
    = SignalBit !Integer
    | SignalString !(Maybe Integer)
    | SignalReal
    | SignalArray !Integer !SignalType
    | SignalTuple ![SignalType]
    | SignalAbstract !String ![Integer]
    | SignalUnknownType
    deriving (Eq, Ord, Show)

instance NFData SignalType where
    rnf (SignalBit n) = rnf n
    rnf (SignalString mn) = rnf mn
    rnf SignalReal = ()
    rnf (SignalArray sz elemTy) = rnf sz `seq` rnf elemTy
    rnf (SignalTuple tys) = rnf tys
    rnf (SignalAbstract nm szs) = rnf nm `seq` rnf szs
    rnf SignalUnknownType = ()

instance PPrint SignalType where
    pPrint _ _ st = text (show st)

-- Lowered expression representation shared across post-IExpand ISyntax,
-- method-oriented APackage, and wire-oriented ASPackage.
data SignalExpr
    = SignalPrim !SignalType !PrimOp ![SignalExpr]
    | SignalMethCall !SignalType !String !String !(Maybe Integer) ![SignalExpr]
    | SignalTupleVal !SignalType ![SignalExpr]
    | SignalTupleSel !SignalType !SignalExpr !Integer
    | SignalNoInlineCall
          !SignalType
          !String
          !String
          ![(String, Integer)]
          !([[(String, Integer)]], [(String, Integer)])
          ![SignalExpr]
    | SignalFunCall !SignalType !String !Bool ![SignalExpr]
    | SignalTaskValue !SignalType !String !Bool !Integer
    | SignalPort !SignalType !String
    | SignalParam !SignalType !String
    | SignalDef !SignalType !String
    | SignalInt !SignalType !Integer
    | SignalRealLit !SignalType !Double
    | SignalStr !SignalType !String
    | SignalAny !SignalType !(Maybe SignalExpr)
    | SignalClock !SignalExpr !SignalExpr
    | SignalReset !SignalExpr
    | SignalInout !SignalType !SignalExpr
    | SignalHeapRef !SignalType !Int
    | SignalLam !String !SignalType !SignalExpr
    | SignalOther !SignalType !String ![SignalExpr]
    deriving (Eq, Ord, Show)

instance NFData SignalExpr where
    rnf (SignalPrim ty op args) = rnf ty `seq` rnf op `seq` rnf args
    rnf (SignalMethCall ty inst port mIdx args) =
        rnf ty `seq` rnf inst `seq` rnf port `seq` rnf mIdx `seq` rnf args
    rnf (SignalTupleVal ty es) = rnf ty `seq` rnf es
    rnf (SignalTupleSel ty e idx) = rnf ty `seq` rnf e `seq` rnf idx
    rnf (SignalNoInlineCall ty obj fn params ports args) =
        rnf ty `seq` rnf obj `seq` rnf fn `seq`
        rnf params `seq` rnf ports `seq` rnf args
    rnf (SignalFunCall ty fn isC args) =
        rnf ty `seq` rnf fn `seq` rnf isC `seq` rnf args
    rnf (SignalTaskValue ty fn isC cookie) =
        rnf ty `seq` rnf fn `seq` rnf isC `seq` rnf cookie
    rnf (SignalPort ty nm) = rnf ty `seq` rnf nm
    rnf (SignalParam ty nm) = rnf ty `seq` rnf nm
    rnf (SignalDef ty nm) = rnf ty `seq` rnf nm
    rnf (SignalInt ty v) = rnf ty `seq` rnf v
    rnf (SignalRealLit ty r) = rnf ty `seq` rnf r
    rnf (SignalStr ty s) = rnf ty `seq` rnf s
    rnf (SignalAny ty me) = rnf ty `seq` rnf me
    rnf (SignalClock osc gate) = rnf osc `seq` rnf gate
    rnf (SignalReset wire) = rnf wire
    rnf (SignalInout ty wire) = rnf ty `seq` rnf wire
    rnf (SignalHeapRef ty ptr) = rnf ty `seq` rnf ptr
    rnf (SignalLam v vt body) = rnf v `seq` rnf vt `seq` rnf body
    rnf (SignalOther ty tag args) = rnf ty `seq` rnf tag `seq` rnf args

instance PPrint SignalExpr where
    pPrint _ _ se = text (show se)

-- Pre-digested naming descriptor for a lowered expression: truncated core stem,
-- preserved trailing physical-design marker (if any), and 64-bit shallow
-- depth-bounded digest.
data ExprName = ExprName
    { en_stem :: !String
    , en_pd_marker :: !String
    , en_digest :: !Word64
    }
    deriving (Eq, Ord, Show)

instance NFData ExprName where
    rnf (ExprName s pd d) = rnf s `seq` rnf pd `seq` rnf d

signalExprType :: SignalExpr -> SignalType
signalExprType (SignalPrim ty _ _) = ty
signalExprType (SignalMethCall ty _ _ _ _) = ty
signalExprType (SignalTupleVal ty _) = ty
signalExprType (SignalTupleSel ty _ _) = ty
signalExprType (SignalNoInlineCall ty _ _ _ _ _) = ty
signalExprType (SignalFunCall ty _ _ _) = ty
signalExprType (SignalTaskValue ty _ _ _) = ty
signalExprType (SignalPort ty _) = ty
signalExprType (SignalParam ty _) = ty
signalExprType (SignalDef ty _) = ty
signalExprType (SignalInt ty _) = ty
signalExprType (SignalRealLit ty _) = ty
signalExprType (SignalStr ty _) = ty
signalExprType (SignalAny ty _) = ty
signalExprType (SignalClock _ _) = SignalAbstract (getIdString idClock) []
signalExprType (SignalReset _) = SignalAbstract (getIdString idReset) []
signalExprType (SignalInout ty _) = ty
signalExprType (SignalHeapRef ty _) = ty
signalExprType (SignalLam _ _ body) = signalExprType body
signalExprType (SignalOther ty _ _) = ty

-- ============================================================
-- Pure VModInfo port-spelling helpers (no ASyntax or FString interning)
-- ============================================================

qoutPortNameStr :: String
qoutPortNameStr = "Q_OUT"

splitPortIndexNums :: [a] -> [Maybe Integer]
splitPortIndexNums [_] = [Nothing]
splitPortIndexNums xs = zipWith (\_ n -> Just n) xs [1 ..]

isRegVModInfo :: VModInfo -> Bool
isRegVModInfo vmi =
    case getVNameString (vName vmi) of
        "RegN" -> True
        "RegUN" -> True
        "RegA" -> True
        "ConfigRegN" -> True
        "ConfigRegUN" -> True
        "ConfigRegA" -> True
        "CrossingRegN" -> True
        "CrossingRegUN" -> True
        "CrossingRegA" -> True
        "RegAligned" -> True
        _ -> False

isClockCrossingRegVModInfo :: VModInfo -> Bool
isClockCrossingRegVModInfo vmi =
    isRegVModInfo vmi && length (input_clocks (vClk vmi)) > 1

inlinedRegVModInfo :: Flags -> VModInfo -> Bool
inlinedRegVModInfo flags vmi =
    removeReg flags &&
    isRegVModInfo vmi &&
    (not (isClockCrossingRegVModInfo vmi) || removeCross flags)

inlinedWireVModInfo :: Flags -> VModInfo -> Bool
inlinedWireVModInfo flags vmi =
    removeRWire flags &&
    case getVNameString (vName vmi) of
        "RWire" -> True
        "RWire0" -> True
        "BypassWire0" -> True
        "BypassWire" -> length (input_clocks (vClk vmi)) /= 2 || removeCross flags
        "CrossingBypassWire" -> length (input_clocks (vClk vmi)) /= 2 || removeCross flags
        _ -> False

inlinedCRegVModInfo :: Flags -> VModInfo -> Bool
inlinedCRegVModInfo flags vmi =
    removeCReg flags &&
    case getVNameString (vName vmi) of
        "CRegN5" -> True
        "CRegUN5" -> True
        "CRegA5" -> True
        _ -> False

keepsMethSpellingVModInfo :: Flags -> VModInfo -> Bool
keepsMethSpellingVModInfo flags vmi =
    inlinedWireVModInfo flags vmi || inlinedCRegVModInfo flags vmi

mkMethResSpelling :: String -> String -> Maybe Integer -> Maybe Integer -> String
mkMethResSpelling instStr methBase mCopy mPort =
    let withCopy =
            case mCopy of
                Nothing -> methBase
                Just c -> methBase ++ "_" ++ show c
        withPort =
            case mPort of
                Nothing -> withCopy
                Just p -> withCopy ++ "_PORT_" ++ show p
    in instStr ++ "$" ++ withPort

mkMethArgSpelling :: String -> Integer -> Maybe Integer -> String
mkMethArgSpelling methWithCopy argN mPort =
    let base = methWithCopy ++ "_" ++ show argN
    in case mPort of
           Nothing -> base
           Just p -> base ++ "_PORT_" ++ show p

cleanVerilogPortPair :: VPort -> String
cleanVerilogPortPair (vn, props) =
    let cleaned = filter (\c -> isAlphaNum c || c == '_') (map replaceChar (getVNameString vn))
        suffix = if VPinhigh `elem` props then "_AlwaysEnabled" else ""
    in cleaned ++ suffix
  where
    replaceChar :: Char -> Char
    replaceChar ' ' = '_'
    replaceChar '?' = 'X'
    replaceChar c = c

createPortMapForField :: String -> VFieldInfo -> [(String, String)]
createPortMapForField _ (Clock _) = []
createPortMapForField _ (Reset _) = []
createPortMapForField _ (Inout {}) = []
createPortMapForField instStr (Method methId _ _ mult ins outs me) =
    let methBase = getIdBaseString (unQualId methId)
        methMult =
            if mult <= 1
            then [methBase]
            else [ methBase ++ "_" ++ show n | n <- [0 .. mult - 1] ]
        methInputs =
            [ mkMethArgSpelling mn argN portM
            | mn <- methMult
            , (argN, ports) <- zip [1 :: Integer ..] ins
            , (portM, _) <- zip (splitPortIndexNums ports) ports
            ]
        veriInputs = map cleanVerilogPortPair (concat ins)
        methOutputs =
            [ case outN of
                  Nothing -> mn
                  Just p -> mn ++ "_PORT_" ++ show p
            | mn <- methMult
            , outN <- splitPortIndexNums outs
            ]
        veriOutputs = map cleanVerilogPortPair outs
        (methEnables, veriEnables) =
            case me of
                Nothing -> ([], [])
                Just ep -> (map ("EN_" ++) methMult, [cleanVerilogPortPair ep])
        multCopies names =
            if mult <= 1
            then names
            else [ nm ++ "_" ++ show n | n <- [1 .. mult], nm <- names ]
        methAll = methInputs ++ methEnables ++ methOutputs
        veriAll = multCopies veriInputs ++ multCopies veriEnables ++ multCopies veriOutputs
        prefixInst s = instStr ++ "$" ++ s
    in zip (map prefixInst methAll) (map prefixInst veriAll)

createInstPortStringMap :: Flags -> String -> VModInfo -> M.Map String String
createInstPortStringMap flags instStr vmi =
    let rawPairs = concatMap (createPortMapForField instStr) (vFields vmi)
        oldQout = instStr ++ "$" ++ qoutPortNameStr
        finalPairs =
            if inlinedRegVModInfo flags vmi
            then [ (k, if v == oldQout then instStr else v) | (k, v) <- rawPairs ]
            else rawPairs
    in L.foldl' (\acc (k, v) -> M.insert k v acc) M.empty finalPairs

isInfixBinPrim :: PrimOp -> Bool
isInfixBinPrim p =
    case p of
        PrimAdd -> True
        PrimSub -> True
        PrimAnd -> True
        PrimOr -> True
        PrimXor -> True
        PrimMul -> True
        PrimQuot -> True
        PrimRem -> True
        PrimSL -> True
        PrimSRL -> True
        PrimSRA -> True
        PrimEQ -> True
        PrimEQ3 -> True
        PrimULE -> True
        PrimULT -> True
        PrimSLE -> True
        PrimSLT -> True
        PrimBAnd -> True
        PrimBOr -> True
        PrimConcat -> True
        _ -> False

-- ============================================================
-- Per-module naming environment
-- ============================================================

data SignalNamingEnv = SignalNamingEnv
    { sne_flags :: Flags
    , sne_inst_vmi :: !(M.Map String VModInfo)
    , sne_inst_port_map :: !(M.Map String (Bool, M.Map String String))
    , sne_reg_insts :: !(S.Set String)
    , sne_state_insts :: !(S.Set String)
    , sne_state_ports :: !(S.Set String)
    , sne_inlined_ports :: !(S.Set String)
    , sne_top_ports :: !(S.Set String)
    , sne_params :: !(S.Set String)
    , sne_clk_rst_inouts :: !(S.Set String)
    , sne_explicit_anchors :: !(S.Set String)
    , sne_heap_names :: !(M.Map Int String)
    }

emptySignalNamingEnv :: SignalNamingEnv
emptySignalNamingEnv =
    SignalNamingEnv
        { sne_flags = defaultFlags ""
        , sne_inst_vmi = M.empty
        , sne_inst_port_map = M.empty
        , sne_reg_insts = S.empty
        , sne_state_insts = S.empty
        , sne_state_ports = S.empty
        , sne_inlined_ports = S.empty
        , sne_top_ports = S.empty
        , sne_params = S.empty
        , sne_clk_rst_inouts = S.empty
        , sne_explicit_anchors = S.empty
        , sne_heap_names = M.empty
        }

withExplicitAnchors :: [Id] -> SignalNamingEnv -> SignalNamingEnv
withExplicitAnchors ids env =
    let extras = S.fromList (map getIdBaseString ids)
    in env { sne_explicit_anchors = S.union (sne_explicit_anchors env) extras }

withHeapNames :: [(Int, String)] -> SignalNamingEnv -> SignalNamingEnv
withHeapNames pairs env =
    let m = L.foldl' (\acc (p, nm) -> M.insert p nm acc) (sne_heap_names env) pairs
    in env { sne_heap_names = m }

buildInstPortStringInfo :: Flags -> Id -> VModInfo -> (Bool, M.Map String String, Bool, [String])
buildInstPortStringInfo flags instId vmi =
    let instStr = getIdBaseString instId
        keepsMeth = keepsMethSpellingVModInfo flags vmi
        strMap = createInstPortStringMap flags instStr vmi
        isReg = isRegVModInfo vmi
        specialWires =
            [ instStr ++ "$" ++ getVNameString vn
            | vf <- vFields vmi
            , vn <- specialFieldVNames vmi vf
            ]
        mappedPorts =
            if isReg
            then [instStr ++ "$" ++ qoutPortNameStr]
            else M.elems strMap
    in (keepsMeth, strMap, isReg, mappedPorts ++ specialWires)
  where
    specialFieldVNames :: VModInfo -> VFieldInfo -> [VName]
    specialFieldVNames vi (Clock cid) =
        case lookupOutputClockWires cid vi of
            (oscVn, Nothing) -> [oscVn]
            (oscVn, Just gateVn) -> [oscVn, gateVn]
    specialFieldVNames vi (Reset rid) = [lookupOutputResetWire rid vi]
    specialFieldVNames vi (Inout iid _ _ _) = [lookupIfcInoutWire iid vi]
    specialFieldVNames _ (Method {}) = []

mkSignalNamingEnv :: Flags -> [(Id, VModInfo)] -> [Id] -> [Id] -> SignalNamingEnv
mkSignalNamingEnv flags instPairs inlinedPortIds topPortIds =
    let step (!vmiMap, !pMap, !regSet, !instSet, !sPortSet) (instId, vmi) =
            let instStr = getIdBaseString instId
                (keeps, !strMap, isReg, ports) = buildInstPortStringInfo flags instId vmi
                !regSet' = if isReg then S.insert instStr regSet else regSet
                !sPortSet' = L.foldl' (\ !s p -> rnf p `seq` S.insert p s) sPortSet ports
                !vmiMap' = rnf instStr `seq` M.insert instStr vmi vmiMap
                !pMap' = M.insert instStr (keeps, strMap) pMap
                !instSet' = S.insert instStr instSet
            in (vmiMap', pMap', regSet', instSet', sPortSet')
        (!vmiMap0, !pMap0, !regSet0, !instSet0, !sPortSet0) =
            L.foldl' step (M.empty, M.empty, S.empty, S.empty, S.empty) instPairs
    in emptySignalNamingEnv
           { sne_flags = flags
           , sne_inst_vmi = vmiMap0
           , sne_inst_port_map = pMap0
           , sne_reg_insts = regSet0
           , sne_state_insts = instSet0
           , sne_state_ports = sPortSet0
           , sne_inlined_ports = S.fromList (map getIdBaseString inlinedPortIds)
           , sne_top_ports = S.fromList (map getIdBaseString topPortIds)
           }

wireInfoSpecialNames :: VWireInfo -> ([String], [String])
wireInfoSpecialNames wi =
    let clkTbl = M.elems (getOutputClockPortTable (wClk wi))
        outClks =
            [ getVNameString vn
            | (oscVn, mGate) <- clkTbl
            , vn <- oscVn : maybe [] ((: []) . fst) mGate
            ]
        outRsts = map getVNameString (M.elems (getOutputResetPortTable (wRst wi)))
        argParams = [ getVNameString vn | Param vn <- wArgs wi ]
    in (argParams, outClks ++ outRsts)

ifcBoundaryNames :: VFieldInfo -> ([String], [String])
ifcBoundaryNames fi@(Method {}) =
    let (outs, rdy, ena) = extractNames fi
        inNames =
            [ getVNameString vn
            | grp <- vf_inputs fi
            , (vn, _) <- grp
            ]
        outNames = map getIdBaseString outs
        ctrlNames = map getIdBaseString [rdy, ena]
    in (inNames ++ outNames ++ ctrlNames, [])
ifcBoundaryNames (Inout _ vn _ _) = ([], [getVNameString vn])
ifcBoundaryNames (Clock _) = ([], [])
ifcBoundaryNames (Reset _) = ([], [])

isClockOrResetPortName :: String -> Bool
isClockOrResetPortName s =
    s == "CLK" ||
    s == "RST" ||
    s == "RST_N" ||
    "CLK_" `L.isPrefixOf` s ||
    "RST_N_" `L.isPrefixOf` s

-- ============================================================
-- Algebraic & structural normalizers
-- ============================================================

safeMask :: Integer -> Integer -> Integer
safeMask w v
    | w <= 0 = 0
    | otherwise = mask w v

safeLog2 :: Integer -> Integer
safeLog2 w
    | w <= 1 = 0
    | otherwise = log2 w

mkSignalInt :: SignalType -> Integer -> SignalExpr
mkSignalInt (SignalBit w) v = SignalInt (SignalBit w) (safeMask w v)
mkSignalInt ty v = SignalInt ty v

mkSignalNat :: Integer -> SignalExpr
mkSignalNat v = mkSignalInt (SignalBit 32) v

-- Normalize bit extraction across PrimSelect, PrimExtract, and PrimTrunc,
-- including constant slices, nested static slices, and slices of concatenations.
mkSignalExtract :: Integer -> Integer -> SignalExpr -> SignalExpr -> SignalExpr -> SignalExpr
mkSignalExtract outW inW se shi slo
    | outW <= 0 = mkSignalInt (SignalBit 0) 0
    | inW <= 0 = mkSignalInt (SignalBit outW) 0
    | SignalAny _ Nothing <- se = SignalAny (SignalBit outW) Nothing
    | SignalInt _ h <- shi
    , SignalInt _ l <- slo =
        let sz = safeMask 32 (h - l + 1)
        in if sz <= 0
           then mkSignalInt (SignalBit outW) 0
           else if sz < outW
                then mkSignalConcat
                         [ mkSignalInt (SignalBit (outW - sz)) 0
                         , mkSignalExtract sz inW se (mkSignalNat (l + sz - 1)) (mkSignalNat l)
                         ]
                else if sz == outW && outW == inW && l == 0
                     then se
                     else if sz == outW && l >= 0 && h < inW
                          then normalizeInBoundsStaticExtract outW inW se h l shi slo
                          else let idxW = safeLog2 inW
                               in SignalPrim
                                      (SignalBit outW)
                                      PrimExtract
                                      [se, truncExtractIdx idxW shi, truncExtractIdx idxW slo]
    | otherwise =
        let idxW = safeLog2 inW
        in SignalPrim
               (SignalBit outW)
               PrimExtract
               [se, truncExtractIdx idxW shi, truncExtractIdx idxW slo]

normalizeInBoundsStaticExtract ::
    Integer ->
    Integer ->
    SignalExpr ->
    Integer ->
    Integer ->
    SignalExpr ->
    SignalExpr ->
    SignalExpr
normalizeInBoundsStaticExtract outW _ (SignalInt (SignalBit _) v) _ l _ _ =
    mkSignalInt (SignalBit outW) (v `shiftR` fromInteger l)
normalizeInBoundsStaticExtract
    outW
    inW
    (SignalPrim _ PrimExtract [inner, SignalInt _ ih, SignalInt _ il])
    h
    l
    shi
    slo
    | SignalBit innerW <- signalExprType inner
    , il >= 0
    , ih - il + 1 == inW =
        mkSignalExtract outW innerW inner (mkSignalNat (il + h)) (mkSignalNat (il + l))
    | otherwise =
        let idxW = safeLog2 inW
        in SignalPrim
               (SignalBit outW)
               PrimExtract
               [ SignalPrim (SignalBit inW) PrimExtract [inner, mkSignalInt (SignalBit (safeLog2 innerWPt)) ih, mkSignalInt (SignalBit (safeLog2 innerWPt)) il]
               , truncExtractIdx idxW shi
               , truncExtractIdx idxW slo
               ]
  where
    innerWPt =
        case signalExprType inner of
            SignalBit w -> w
            _ -> inW
normalizeInBoundsStaticExtract outW inW se@(SignalPrim _ PrimConcat parts) h l shi slo
    | Just partWidths <- mapM bitPartWidth parts
    , sum partWidths == inW =
        mkSignalConcat (sliceConcat inW (zip parts partWidths))
    | otherwise =
        let idxW = safeLog2 inW
        in SignalPrim
               (SignalBit outW)
               PrimExtract
               [se, truncExtractIdx idxW shi, truncExtractIdx idxW slo]
  where
    bitPartWidth :: SignalExpr -> Maybe Integer
    bitPartWidth p =
        case signalExprType p of
            SignalBit pw | pw > 0 -> Just pw
            _ -> Nothing

    sliceConcat :: Integer -> [(SignalExpr, Integer)] -> [SignalExpr]
    sliceConcat _ [] = []
    sliceConcat topOff ((p, pw) : rest) =
        let base = topOff - pw
            ovLo = max l base
            ovHi = min h (topOff - 1)
            tailSlices = sliceConcat base rest
        in if ovHi >= ovLo
           then mkSignalExtract (ovHi - ovLo + 1) pw p (mkSignalNat (ovHi - base)) (mkSignalNat (ovLo - base)) : tailSlices
           else tailSlices
normalizeInBoundsStaticExtract outW inW se _ _ shi slo =
    let idxW = safeLog2 inW
    in SignalPrim
           (SignalBit outW)
           PrimExtract
           [se, truncExtractIdx idxW shi, truncExtractIdx idxW slo]

truncExtractIdx :: Integer -> SignalExpr -> SignalExpr
truncExtractIdx idxW idx
    | idxW <= 0 = mkSignalInt (SignalBit 0) 0
    | SignalInt (SignalBit _) v <- idx = mkSignalInt (SignalBit idxW) v
    | SignalBit k <- signalExprType idx
    , k > idxW =
        mkSignalExtract idxW k idx (mkSignalNat (idxW - 1)) (mkSignalNat 0)
    | otherwise = idx

-- Flatten nested PrimConcat and drop 0-bit operands.
mkSignalConcat :: [SignalExpr] -> SignalExpr
mkSignalConcat rawArgs =
    let flat = concatMap flattenOne rawArgs
        nonZero = filter (not . isZeroWidth) flat
    in case nonZero of
           [] -> mkSignalInt (SignalBit 0) 0
           [single] -> single
           many ->
               let totalW = L.foldl' (\acc e -> acc + bitWidthOf (signalExprType e)) 0 many
               in SignalPrim (SignalBit totalW) PrimConcat many
  where
    flattenOne :: SignalExpr -> [SignalExpr]
    flattenOne (SignalPrim _ PrimConcat es) = concatMap flattenOne es
    flattenOne e = [e]

    isZeroWidth :: SignalExpr -> Bool
    isZeroWidth e = signalExprType e == SignalBit 0

    bitWidthOf :: SignalType -> Integer
    bitWidthOf (SignalBit w) = w
    bitWidthOf _ = 0

mkSignalTuple :: SignalType -> [SignalExpr] -> SignalExpr
mkSignalTuple _ [] = mkSignalInt (SignalBit 0) 0
mkSignalTuple ty elems = SignalTupleVal ty elems

-- 1-based tuple element projection; immediately reduces on SignalTupleVal.
mkSignalTupleSel :: SignalType -> SignalExpr -> Integer -> SignalExpr
mkSignalTupleSel ty (SignalTupleVal _ elems) idx1
    | idx1 >= 1
    , Just chosen <- indexOneBased idx1 elems = chosen
    | otherwise =
        SignalTupleSel
            ty
            (SignalTupleVal (SignalTuple (map signalExprType elems)) elems)
            idx1
mkSignalTupleSel ty base idx1 = SignalTupleSel ty base idx1

indexOneBased :: Integer -> [a] -> Maybe a
indexOneBased _ [] = Nothing
indexOneBased 1 (x : _) = Just x
indexOneBased n (_ : xs)
    | n > 1 = indexOneBased (n - 1) xs
    | otherwise = Nothing

-- ============================================================
-- Submodule and state port resolution
-- ============================================================

mkSignalPort :: SignalNamingEnv -> SignalType -> String -> SignalExpr
mkSignalPort env ty rawName
    | rawName `S.member` sne_reg_insts env && not ('$' `elem` rawName) =
        SignalPort ty (rawName ++ "$" ++ qoutPortNameStr)
    | otherwise = SignalPort ty rawName

mkDefOrPort :: SignalNamingEnv -> SignalType -> String -> SignalExpr
mkDefOrPort env ty name
    | name `S.member` sne_inlined_ports env || name `S.member` sne_state_ports env =
        mkSignalPort env ty name
    | otherwise = SignalDef ty name

findMethMult :: VModInfo -> Id -> Maybe Integer
findMethMult vmi methId =
    let methBase = getIdBaseString (unQualId methId)
        matches =
            [ mult
            | Method { vf_name = m, vf_mult = mult } <- vFields vmi
            , getIdBaseString (unQualId m) == methBase
            ]
    in case matches of
           (mult : _) -> Just mult
           [] -> Nothing

findMethOutCount :: VModInfo -> Id -> Int
findMethOutCount vmi methId =
    let methBase = getIdBaseString (unQualId methId)
        matches =
            [ length outs
            | Method { vf_name = m, vf_outputs = outs } <- vFields vmi
            , getIdBaseString (unQualId m) == methBase
            ]
    in case matches of
           (n : _) -> n
           [] -> 1

resolveMethPortString :: SignalNamingEnv -> Maybe VModInfo -> Id -> Id -> Maybe Integer -> String
resolveMethPortString env mDirectVmi instId methId mResIdx =
    let instStr = getIdBaseString instId
        methBase = getIdBaseString (unQualId methId)
        mVmi =
            case mDirectVmi of
                Just vmi -> Just vmi
                Nothing -> M.lookup instStr (sne_inst_vmi env)
        ino =
            case mVmi >>= (`findMethMult` methId) of
                Just mult | mult > 1 -> Just 0
                _ -> Nothing
        isReg =
            methId == idPreludeRead ||
            methBase == "_read" ||
            instStr `S.member` sne_reg_insts env ||
            maybe False isRegVModInfo mVmi
        rawMeth = mkMethResSpelling instStr methBase ino mResIdx
    in if isReg && (methId == idPreludeRead || methBase == "_read" || methBase == "read")
       then instStr ++ "$" ++ qoutPortNameStr
       else case M.lookup instStr (sne_inst_port_map env) of
                Just (keepsMeth, strMap) ->
                    if keepsMeth
                    then rawMeth
                    else M.findWithDefault rawMeth rawMeth strMap
                Nothing ->
                    case mVmi of
                        Just vmi
                            | keepsMethSpellingVModInfo (sne_flags env) vmi -> rawMeth
                            | otherwise ->
                                let strMap = createInstPortStringMap (sne_flags env) instStr vmi
                                    pStr = M.findWithDefault rawMeth rawMeth strMap
                                in if pStr == instStr
                                   then instStr ++ "$" ++ qoutPortNameStr
                                   else pStr
                        Nothing -> rawMeth

lowerResolvedMethodRead ::
    SignalNamingEnv ->
    Maybe VModInfo ->
    SignalType ->
    Id ->
    Id ->
    Maybe Integer ->
    [SignalExpr] ->
    SignalExpr
lowerResolvedMethodRead env mVmi resTy instId methId mResIdx sigArgs
    | resTy == SignalBit 0 && null sigArgs = mkSignalInt (SignalBit 0) 0
    | null sigArgs
    , Nothing <- mResIdx
    , SignalTuple elemTys <- resTy
    , Just vmi <- effectiveVmi
    , length elemTys > 1
    , findMethOutCount vmi methId == length elemTys =
        let ports =
                [ mkSignalPort
                      env
                      elemTy
                      (resolveMethPortString env effectiveVmi instId methId mIdx)
                | (mIdx, elemTy) <- zip (splitPortIndexNums elemTys) elemTys
                ]
        in mkSignalTuple resTy ports
    | null sigArgs =
        let portStr = resolveMethPortString env effectiveVmi instId methId mResIdx
        in mkSignalPort env resTy portStr
    | otherwise =
        let instStr = getIdBaseString instId
            portStr = resolveMethPortString env effectiveVmi instId methId mResIdx
        in SignalMethCall resTy instStr portStr mResIdx sigArgs
  where
    effectiveVmi :: Maybe VModInfo
    effectiveVmi =
        case mVmi of
            Just v -> Just v
            Nothing -> M.lookup (getIdBaseString instId) (sne_inst_vmi env)

resolveOutputGate :: SignalNamingEnv -> Maybe VModInfo -> Id -> Id -> SignalExpr
resolveOutputGate env mDirectVmi instId clkId =
    let instStr = getIdBaseString instId
        mVmi =
            case mDirectVmi of
                Just v -> Just v
                Nothing -> M.lookup instStr (sne_inst_vmi env)
    in case mVmi of
           Just vmi ->
               case lookupOutputClockWires clkId vmi of
                   (_, Nothing) -> mkSignalInt (SignalBit 1) 1
                   (_, Just gateVn) ->
                       SignalPort (SignalBit 1) (instStr ++ "$" ++ getVNameString gateVn)
           Nothing ->
               SignalPort
                   (SignalBit 1)
                   (instStr ++ "$" ++ getIdBaseString (unQualId clkId) ++ "_GATE")

resolveOutputOsc :: SignalNamingEnv -> Maybe VModInfo -> Id -> Id -> SignalExpr
resolveOutputOsc env mDirectVmi instId clkId =
    let instStr = getIdBaseString instId
        mVmi =
            case mDirectVmi of
                Just v -> Just v
                Nothing -> M.lookup instStr (sne_inst_vmi env)
    in case mVmi of
           Just vmi ->
               let (oscVn, _) = lookupOutputClockWires clkId vmi
               in SignalPort (SignalBit 1) (instStr ++ "$" ++ getVNameString oscVn)
           Nothing ->
               SignalPort
                   (SignalBit 1)
                   (instStr ++ "$" ++ getIdBaseString (unQualId clkId))

cleanGenBase :: Id -> String
cleanGenBase i =
    let base = getIdBaseString (unQualId i)
        sufLen = length genSuffix
    in if isGenId i && length base > sufLen && genSuffix `L.isSuffixOf` base
       then take (length base - sufLen) base
       else base

-- ============================================================
-- Generated suffix stripping, PD markers, and stem rendering
-- ============================================================

pdMarkerSuffixes :: [String]
pdMarkerSuffixes =
    [ "_DONT_TOUCH"
    , "_SYNTH_KEEP"
    , "_DONT_RETIME"
    , "_NO_RETIME"
    , "_PRESERVE"
    , "_KEEP"
    ]

splitTrailingPDMarker :: String -> (String, String)
splitTrailingPDMarker s =
    case [ m | m <- pdMarkerSuffixes, length s > length m && m `L.isSuffixOf` s ] of
        (m : _) -> (take (length s - length m) s, m)
        [] -> (s, "")

isLowerBase36 :: Char -> Bool
isLowerBase36 c = isDigit c || isAsciiLower c

-- Strip compiler-generated suffixes (___d<n>, __q<n>, __f<n>, __h<n>, __i<n>,
-- __j<n>, _d<n>, _dm<n>, _ds<n>, _h<4 base36>, _h<8 base36>, and uniquifier
-- _<digits>) while preserving SignalNaming operator literals, user
-- double-underscore identifiers (pkt__hdr), and trailing PD markers.
stripGeneratedSuffixes :: String -> String
stripGeneratedSuffixes s0 =
    let (core, pd1) = peelLoop s0
        (coreNoPd, pd2) = splitTrailingPDMarker core
        (coreClean, pd3) = peelLoop coreNoPd
        finalPd = pd3 ++ pd2 ++ pd1
    in if null coreClean
       then dropWhile (== '_') finalPd
       else coreClean ++ finalPd
  where
    peelLoop :: String -> (String, String)
    peelLoop s =
        let (base, pd) = splitTrailingPDMarker s
        in case peelOneSuffix base of
               Just base' ->
                   let (base'', pdInner) = peelLoop base'
                   in (base'', pdInner ++ pd)
               Nothing -> (base, pd)

dropGeneratedSuffixes :: String -> String
dropGeneratedSuffixes = stripGeneratedSuffixes

peelOneSuffix :: String -> Maybe String
peelOneSuffix s
    | Just rest <- peelCoreTag rev = Just rest
    | Just rAfterOrd <- peelTrailingOrdinal rev
    , Just rest <- peelCoreTag rAfterOrd = Just rest
    | otherwise = Nothing
  where
    rev :: String
    rev = reverse s

    peelCoreTag :: String -> Maybe String
    peelCoreTag r
        | Just rest <- peelFixedBase36Hash 8 r = Just rest
        | Just rest <- peelFixedBase36Hash 4 r = Just rest
        | Just rest <- peelMultiUnderscoreTag r = Just rest
        | Just rest <- peelSingleUnderscoreTempTag r = Just rest
        | otherwise = Nothing

    peelFixedBase36Hash :: Int -> String -> Maybe String
    peelFixedBase36Hash k r =
        let (hChars, afterH) = splitAt k r
        in if length hChars == k && all isLowerBase36 hChars
           then case afterH of
                    ('h' : '_' : restRev)
                        | not ("_" `L.isPrefixOf` restRev) ->
                            Just (reverse restRev)
                    _ -> Nothing
           else Nothing

    peelMultiUnderscoreTag :: String -> Maybe String
    peelMultiUnderscoreTag r =
        let (digits, r1) = span isDigit r
        in case r1 of
               (tagChar : r2)
                   | not (null digits) && isAsciiLower tagChar ->
                       let (us, restRev) = span (== '_') r2
                       in if length us >= 2
                          then Just (reverse restRev)
                          else Nothing
               _ -> Nothing

    peelSingleUnderscoreTempTag :: String -> Maybe String
    peelSingleUnderscoreTempTag r =
        let (digits, r1) = span isDigit r
        in if null digits
           then Nothing
           else case r1 of
                    ('d' : '_' : restRev) -> Just (reverse restRev)
                    ('h' : '_' : restRev) -> Just (reverse restRev)
                    ('m' : 'd' : '_' : restRev) -> Just (reverse restRev)
                    ('s' : 'd' : '_' : restRev) -> Just (reverse restRev)
                    _ -> Nothing

    peelTrailingOrdinal :: String -> Maybe String
    peelTrailingOrdinal r =
        let (digits, r1) = span isDigit r
        in case r1 of
               ('_' : restRev)
                   | not (null digits)
                   , not ("_" `L.isPrefixOf` restRev) ->
                       Just restRev
               _ -> Nothing

signalStemLimit :: Int
signalStemLimit = 50

splitAndTruncateStem :: String -> (String, String)
splitAndTruncateStem raw =
    let (core, pd) = splitTrailingPDMarker raw
        sanitizedCore = sanitizeStemChars core
        trimmed = trimUnderscores sanitizedCore
        truncatedCore =
            if length trimmed + length pd < signalStemLimit
            then trimmed
            else let keepLen = max 1 (signalStemLimit - 4 - length pd)
                 in trimRightUnderscores (take keepLen trimmed) ++ "_ETC"
    in (escapeLeadingDigit truncatedCore, pd)
  where
    sanitizeStemChars :: String -> String
    sanitizeStemChars [] = []
    sanitizeStemChars ('$' : cs) = '_' : sanitizeStemChars cs
    sanitizeStemChars (c : cs)
        | c == '.' || c == '\'' = sanitizeStemChars cs
        | isAlphaNum c || c == '_' = c : sanitizeStemChars cs
        | otherwise = sanitizeStemChars cs

    escapeLeadingDigit :: String -> String
    escapeLeadingDigit s@(c : _)
        | isDigit c = '_' : s
    escapeLeadingDigit s = s

signalExprStem :: SignalNamingEnv -> SignalExpr -> String
signalExprStem env expr =
    let (core, pd) = splitAndTruncateStem (signalExprStemRaw env expr)
    in core ++ pd

exprName :: SignalNamingEnv -> SignalExpr -> ExprName
exprName env expr =
    let (core, pd) = splitAndTruncateStem (signalExprStemRaw env expr)
        !digest = hashSignalExprShallow expr
        en =
            ExprName
                { en_stem = trimRightUnderscores core
                , en_pd_marker = pd
                , en_digest = digest
                }
    in rnf en `seq` en

exprNameStem :: ExprName -> String
exprNameStem en = en_stem en ++ en_pd_marker en

exprNameH4 :: ExprName -> String
exprNameH4 en = signalHash4 (en_digest en)

exprNameH8 :: ExprName -> String
exprNameH8 en = signalHash8 (en_digest en)

formatSignalTempName :: String -> String -> String
formatSignalTempName stem hSuffix =
    let (core, pd) = splitTrailingPDMarker stem
        cleanCore = trimRightUnderscores core
    in if null cleanCore
       then "_h" ++ hSuffix ++ pd
       else cleanCore ++ "_h" ++ hSuffix ++ pd

formatExprTempName :: ExprName -> String -> String
formatExprTempName en hSuffix =
    let cleanCore = trimRightUnderscores (en_stem en)
        pd = en_pd_marker en
    in if null cleanCore
       then "_h" ++ hSuffix ++ pd
       else cleanCore ++ "_h" ++ hSuffix ++ pd

signalExprStemRaw :: SignalNamingEnv -> SignalExpr -> String
signalExprStemRaw env (SignalPrim _ PrimZeroExt [arg]) =
    signalExprStemRaw env arg
signalExprStemRaw env (SignalPrim _ PrimSignExt [arg]) =
    "SEXT_" ++ signalExprStemRaw env arg
signalExprStemRaw env (SignalPrim _ PrimExtract [arg, hi, lo])
    | hi == lo =
        connectParts "_BIT_" [signalExprStemRaw env arg, signalExprStemRaw env hi]
    | otherwise =
        connectParts
            "_BITS_"
            [ signalExprStemRaw env arg
            , connectParts "_TO_" [signalExprStemRaw env hi, signalExprStemRaw env lo]
            ]
signalExprStemRaw env (SignalPrim _ PrimIf [cond, eThen, eElse]) =
    "IF_" ++
    connectParts
        "_THEN_"
        [ signalExprStemRaw env cond
        , connectParts "_ELSE_" [signalExprStemRaw env eThen, signalExprStemRaw env eElse]
        ]
signalExprStemRaw env (SignalPrim _ PrimCase (idx : dflt : ces)) =
    "CASE_" ++ connectParts "_" (map (signalExprStemRaw env) ((idx : ces) ++ [dflt]))
signalExprStemRaw env (SignalPrim _ prim args)
    | isInfixBinPrim prim =
        connectParts ("_" ++ primOpStem prim ++ "_") (map (signalExprStemRaw env) args)
    | null args = primOpStem prim
    | otherwise =
        primOpStem prim ++ "_" ++ connectParts "_" (map (signalExprStemRaw env) args)
signalExprStemRaw env (SignalMethCall _ instStr portStr _ args) =
    let basePart = renderPortStem env portStr instStr
        flatArgs = concatMap unpackTupleArg args
    in if null flatArgs
       then basePart
       else connectParts "_" (basePart : map (signalExprStemRaw env) flatArgs)
  where
    unpackTupleArg :: SignalExpr -> [SignalExpr]
    unpackTupleArg (SignalTupleVal _ es) = es
    unpackTupleArg e = [e]
signalExprStemRaw env (SignalTupleVal _ elems) =
    "TUPLE_" ++ connectParts "_" (map (signalExprStemRaw env) elems)
signalExprStemRaw env (SignalTupleSel _ inner idx1) =
    signalExprStemRaw env inner ++ "_SEL_" ++ show idx1
signalExprStemRaw _ (SignalNoInlineCall _ objName _ _ _ _) =
    stripGeneratedSuffixes objName
signalExprStemRaw env (SignalFunCall _ fn _ args)
    | null args = fn
    | otherwise = fn ++ "_" ++ connectParts "_" (map (signalExprStemRaw env) args)
signalExprStemRaw _ (SignalTaskValue _ fn _ _) =
    "TASK_" ++ dropWhile (== '$') fn
signalExprStemRaw env (SignalPort _ portStr) =
    renderPortStem env portStr portStr
signalExprStemRaw _ (SignalParam _ paramStr) =
    stripGeneratedSuffixes paramStr
signalExprStemRaw _ (SignalDef _ defStr) =
    stripGeneratedSuffixes defStr
signalExprStemRaw _ (SignalInt _ val)
    | val < 0 = "neg" ++ show (negate val)
    | otherwise = show val
signalExprStemRaw _ (SignalRealLit _ r) =
    sanitizeReal (show r)
  where
    sanitizeReal :: String -> String
    sanitizeReal [] = []
    sanitizeReal ('.' : cs) = 'd' : sanitizeReal cs
    sanitizeReal ('-' : cs) = 'n' : 'e' : 'g' : sanitizeReal cs
    sanitizeReal (c : cs) = c : sanitizeReal cs
signalExprStemRaw _ (SignalStr _ s) =
    "STR_" ++ sanitizeStr s
  where
    sanitizeStr :: String -> String
    sanitizeStr [] = []
    sanitizeStr (c : cs)
        | isAlphaNum c = c : sanitizeStr cs
        | isSpace c = '_' : sanitizeStr cs
        | otherwise = sanitizeStr cs
signalExprStemRaw _ (SignalAny _ _) = "DONTCARE"
signalExprStemRaw env (SignalClock osc _) = signalExprStemRaw env osc
signalExprStemRaw env (SignalReset wire) = signalExprStemRaw env wire
signalExprStemRaw env (SignalInout _ wire) = signalExprStemRaw env wire
signalExprStemRaw _ (SignalHeapRef _ _) = ""
signalExprStemRaw env (SignalLam _ _ body) = signalExprStemRaw env body
signalExprStemRaw env (SignalOther _ tag args)
    | null args = stripGeneratedSuffixes tag
    | otherwise =
        connectParts "_" (stripGeneratedSuffixes tag : map (signalExprStemRaw env) args)

renderPortStem :: SignalNamingEnv -> String -> String -> String
renderPortStem env portStr fallbackInst =
    let qoutSuffix = "$" ++ qoutPortNameStr
    in if qoutSuffix `L.isSuffixOf` portStr
       then let prefix = take (length portStr - length qoutSuffix) portStr
            in if prefix `S.member` sne_reg_insts env || prefix == fallbackInst
               then prefix
               else replaceDollar portStr
       else replaceDollar portStr
  where
    replaceDollar :: String -> String
    replaceDollar = map (\c -> if c == '$' then '_' else c)

primOpStem :: PrimOp -> String
primOpStem PrimAdd = "PLUS"
primOpStem PrimSub = "MINUS"
primOpStem PrimBAnd = "AND"
primOpStem PrimBOr = "OR"
primOpStem PrimBNot = "NOT"
primOpStem PrimBuildArray = "ARR"
primOpStem PrimArrayDynSelect = "SEL"
primOpStem op =
    map toUpper $
        case show op of
            ('P' : 'r' : 'i' : 'm' : rest) -> rest
            other -> other

trimUnderscores :: String -> String
trimUnderscores = trimRightUnderscores . dropWhile (== '_')

trimRightUnderscores :: String -> String
trimRightUnderscores = reverse . dropWhile (== '_') . reverse

connectParts :: String -> [String] -> String
connectParts sep parts =
    let cleaned = filter (not . null) (map trimUnderscores parts)
    in L.intercalate sep cleaned

-- ============================================================
-- Shallow depth-bounded hashing and base-36 h4 / h8 suffixes
-- ============================================================

shallowSignalExpr :: SignalExpr -> SignalExpr
shallowSignalExpr (SignalPrim ty op args) =
    SignalPrim ty op (map shallowSignalExpr args)
shallowSignalExpr (SignalMethCall ty inst port mIdx args) =
    SignalMethCall ty inst port mIdx (map shallowSignalExpr args)
shallowSignalExpr (SignalTupleVal ty elems) =
    SignalTupleVal ty (map shallowSignalExpr elems)
shallowSignalExpr (SignalTupleSel ty inner idx1) =
    SignalTupleSel ty (shallowSignalExpr inner) idx1
shallowSignalExpr (SignalNoInlineCall ty obj fn params ports args) =
    SignalNoInlineCall ty (stripGeneratedSuffixes obj) fn params ports (map shallowSignalExpr args)
shallowSignalExpr (SignalFunCall ty fn isCFlag args) =
    SignalFunCall ty fn isCFlag (map shallowSignalExpr args)
shallowSignalExpr e@(SignalTaskValue {}) = e
shallowSignalExpr e@(SignalPort {}) = e
shallowSignalExpr e@(SignalParam {}) = e
shallowSignalExpr (SignalDef ty nm) =
    SignalDef ty (stripGeneratedSuffixes nm)
shallowSignalExpr e@(SignalInt {}) = e
shallowSignalExpr e@(SignalRealLit {}) = e
shallowSignalExpr e@(SignalStr {}) = e
shallowSignalExpr (SignalAny ty mVal) =
    SignalAny ty (fmap shallowSignalExpr mVal)
shallowSignalExpr (SignalClock osc gate) =
    SignalClock (shallowSignalExpr osc) (shallowSignalExpr gate)
shallowSignalExpr (SignalReset wire) =
    SignalReset (shallowSignalExpr wire)
shallowSignalExpr (SignalInout ty wire) =
    SignalInout ty (shallowSignalExpr wire)
shallowSignalExpr (SignalHeapRef ty _) =
    SignalDef ty ""
shallowSignalExpr (SignalLam v vt body) =
    SignalLam (stripGeneratedSuffixes v) vt (shallowSignalExpr body)
shallowSignalExpr (SignalOther ty tag args) =
    SignalOther ty (stripGeneratedSuffixes tag) (map shallowSignalExpr args)

fnvOffset64 :: Word64
fnvOffset64 = 0xcbf29ce484222325

fnvPrime64 :: Word64
fnvPrime64 = 0x100000001b3

feedByte :: Word64 -> Word64 -> Word64
feedByte h b = (h `xor` (b `rem` 256)) * fnvPrime64

feedWord64 :: Word64 -> Word64 -> Word64
feedWord64 h0 w =
    let h1 = feedByte h0 w
        h2 = feedByte h1 (w `shiftR` 8)
        h3 = feedByte h2 (w `shiftR` 16)
        h4 = feedByte h3 (w `shiftR` 24)
        h5 = feedByte h4 (w `shiftR` 32)
        h6 = feedByte h5 (w `shiftR` 40)
        h7 = feedByte h6 (w `shiftR` 48)
    in feedByte h7 (w `shiftR` 56)

feedInt :: Word64 -> Int -> Word64
feedInt h n = feedWord64 h (fromIntegral n)

feedBool :: Word64 -> Bool -> Word64
feedBool h False = feedByte h 0
feedBool h True = feedByte h 1

feedInteger :: Word64 -> Integer -> Word64
feedInteger h0 n
    | n == 0 = feedByte h0 0
    | n < 0 = feedLimbs (feedByte h0 1) (negate n)
    | otherwise = feedLimbs (feedByte h0 2) n
  where
    two64 :: Integer
    two64 = (2 :: Integer) ^ (64 :: Int)

    feedLimbs :: Word64 -> Integer -> Word64
    feedLimbs h 0 = feedByte h 0xff
    feedLimbs h mag =
        let (q, r) = mag `quotRem` two64
        in feedLimbs (feedWord64 h (fromIntegral r)) q

feedString :: Word64 -> String -> Word64
feedString h0 s =
    let hLen = feedInt h0 (length s)
    in L.foldl' (\h c -> feedWord64 h (fromIntegral (ord c))) hLen s

feedDouble :: Word64 -> Double -> Word64
feedDouble h d
    | isNaN d = feedByte h 0
    | d == 0.0 = feedByte h 1
    | isInfinite d = feedByte h (if d > 0 then 2 else 3)
    | otherwise =
        let (mant, expo) = decodeFloat d
        in feedInt (feedInteger (feedByte h 4) mant) expo

feedList :: (Word64 -> a -> Word64) -> Word64 -> [a] -> Word64
feedList elemFn h0 xs =
    let hLen = feedInt h0 (length xs)
    in L.foldl' elemFn hLen xs

feedSignalType :: Word64 -> SignalType -> Word64
feedSignalType h (SignalBit n) = feedInteger (feedByte h 1) n
feedSignalType h (SignalString Nothing) = feedByte h 2
feedSignalType h (SignalString (Just n)) = feedInteger (feedByte h 3) n
feedSignalType h SignalReal = feedByte h 4
feedSignalType h (SignalArray sz elemTy) =
    feedSignalType (feedInteger (feedByte h 5) sz) elemTy
feedSignalType h (SignalTuple tys) =
    feedList feedSignalType (feedByte h 6) tys
feedSignalType h (SignalAbstract nm szs) =
    feedList feedInteger (feedString (feedByte h 7) nm) szs
feedSignalType h SignalUnknownType = feedByte h 8

feedSignalExpr :: Word64 -> SignalExpr -> Word64
feedSignalExpr h0 (SignalPrim ty op args) =
    let h1 = feedSignalType (feedByte h0 1) ty
        h2 = feedInt h1 (fromEnum op)
    in feedList feedSignalExpr h2 args
feedSignalExpr h0 (SignalMethCall ty inst port mIdx args) =
    let h1 = feedSignalType (feedByte h0 2) ty
        h2 = feedString (feedString h1 inst) port
        h3 =
            case mIdx of
                Nothing -> feedByte h2 0
                Just idx -> feedInteger (feedByte h2 1) idx
    in feedList feedSignalExpr h3 args
feedSignalExpr h0 (SignalTupleVal ty elems) =
    feedList feedSignalExpr (feedSignalType (feedByte h0 3) ty) elems
feedSignalExpr h0 (SignalTupleSel ty inner idx1) =
    feedInteger (feedSignalExpr (feedSignalType (feedByte h0 4) ty) inner) idx1
feedSignalExpr h0 (SignalNoInlineCall ty obj fn params (inGroups, outs) args) =
    let h1 = feedString (feedString (feedSignalType (feedByte h0 5) ty) obj) fn
        feedPair hp (s, n) = feedInteger (feedString hp s) n
        h2 = feedList feedPair h1 params
        h3 = feedList (feedList feedPair) h2 inGroups
        h4 = feedList feedPair h3 outs
    in feedList feedSignalExpr h4 args
feedSignalExpr h0 (SignalFunCall ty fn isCFlag args) =
    let h1 = feedBool (feedString (feedSignalType (feedByte h0 6) ty) fn) isCFlag
    in feedList feedSignalExpr h1 args
feedSignalExpr h0 (SignalTaskValue ty fn isCFlag cookie) =
    feedInteger (feedBool (feedString (feedSignalType (feedByte h0 7) ty) fn) isCFlag) cookie
feedSignalExpr h0 (SignalPort ty nm) =
    feedString (feedSignalType (feedByte h0 8) ty) nm
feedSignalExpr h0 (SignalParam ty nm) =
    feedString (feedSignalType (feedByte h0 9) ty) nm
feedSignalExpr h0 (SignalDef ty nm) =
    feedString (feedSignalType (feedByte h0 10) ty) nm
feedSignalExpr h0 (SignalInt ty v) =
    feedInteger (feedSignalType (feedByte h0 11) ty) v
feedSignalExpr h0 (SignalRealLit ty r) =
    feedDouble (feedSignalType (feedByte h0 12) ty) r
feedSignalExpr h0 (SignalStr ty s) =
    feedString (feedSignalType (feedByte h0 13) ty) s
feedSignalExpr h0 (SignalAny ty Nothing) =
    feedByte (feedSignalType (feedByte h0 14) ty) 0
feedSignalExpr h0 (SignalAny ty (Just e)) =
    feedSignalExpr (feedByte (feedSignalType (feedByte h0 14) ty) 1) e
feedSignalExpr h0 (SignalClock osc gate) =
    feedSignalExpr (feedSignalExpr (feedByte h0 15) osc) gate
feedSignalExpr h0 (SignalReset wire) =
    feedSignalExpr (feedByte h0 16) wire
feedSignalExpr h0 (SignalInout ty wire) =
    feedSignalExpr (feedSignalType (feedByte h0 17) ty) wire
feedSignalExpr h0 (SignalHeapRef ty ptr) =
    feedInt (feedSignalType (feedByte h0 18) ty) ptr
feedSignalExpr h0 (SignalLam v vt body) =
    feedSignalExpr (feedSignalType (feedString (feedByte h0 19) v) vt) body
feedSignalExpr h0 (SignalOther ty tag args) =
    feedList feedSignalExpr (feedString (feedSignalType (feedByte h0 20) ty) tag) args

avalanche64 :: Word64 -> Word64
avalanche64 z0 =
    let z1 = (z0 `xor` (z0 `shiftR` 30)) * 0xbf58476d1ce4e5b9
        z2 = (z1 `xor` (z1 `shiftR` 27)) * 0x94d049bb133111eb
    in z2 `xor` (z2 `shiftR` 31)

hashSignalExprFull :: SignalExpr -> Word64
hashSignalExprFull expr = avalanche64 (feedSignalExpr fnvOffset64 expr)

hashSignalExprShallow :: SignalExpr -> Word64
hashSignalExprShallow expr = hashSignalExprFull (shallowSignalExpr expr)

base36Digit :: Word64 -> Char
base36Digit d
    | d < 10 = chr (ord '0' + fromIntegral d)
    | otherwise = chr (ord 'a' + fromIntegral (d - 10))

extractBase36 :: Int -> Word64 -> String
extractBase36 count w0 = go count w0
  where
    go :: Int -> Word64 -> String
    go k w
        | k <= 0 = []
        | otherwise =
            let (q, r) = w `quotRem` 36
            in base36Digit r : go (k - 1) q

signalHash4 :: Word64 -> String
signalHash4 = extractBase36 4

signalHash8 :: Word64 -> String
signalHash8 = extractBase36 8

-- ============================================================
-- Anchor classification & hard anchor invariant
-- ============================================================

data AnchorKind
    = AnchorPort
    | AnchorSubmodPort
    | AnchorEnableReady
    | AnchorParam
    | AnchorClockResetInout
    | AnchorStateInst
    | AnchorRuleFire
    | AnchorExplicitKeep
    deriving (Eq, Ord, Show)

data AnchorCollision = AnchorCollision
    { ac_name :: !String
    , ac_existing_kind :: !AnchorKind
    , ac_new_kind :: !AnchorKind
    }
    deriving (Eq, Ord, Show)

classifyAnchorId :: SignalNamingEnv -> Id -> Maybe AnchorKind
classifyAnchorId env i
    | isIdWillFire i || isIdCanFire i ||
      "WILL_FIRE_" `L.isPrefixOf` s || "CAN_FIRE_" `L.isPrefixOf` s =
        Just AnchorRuleFire
    | isIdEnable i || isRdyId i ||
      "EN_" `L.isPrefixOf` s || "RDY_" `L.isPrefixOf` s =
        Just AnchorEnableReady
    | s `S.member` sne_params env =
        Just AnchorParam
    | s `S.member` sne_clk_rst_inouts env || isClockOrResetPortName s =
        Just AnchorClockResetInout
    | s `S.member` sne_state_insts env =
        Just AnchorStateInst
    | '$' `elem` s || s `S.member` sne_state_ports env || s `S.member` sne_inlined_ports env =
        Just AnchorSubmodPort
    | s `S.member` sne_top_ports env || s == "D_IN" || s == "Q_OUT" =
        Just AnchorPort
    | hasIdProp i IdPProbe ||
      hasIdProp i IdP_keepEvenUnused ||
      hasIdProp i IdPMethodPredicate ||
      "COND_" `L.isPrefixOf` s ||
      s `S.member` sne_explicit_anchors env =
        Just AnchorExplicitKeep
    | otherwise = Nothing
  where
    s :: String
    s = getIdBaseString i

isEnvAnchorId :: SignalNamingEnv -> Id -> Bool
isEnvAnchorId env i = isJust (classifyAnchorId env i)

isDefAnchorKind :: AnchorKind -> Bool
isDefAnchorKind AnchorStateInst = False
isDefAnchorKind AnchorParam = False
isDefAnchorKind AnchorClockResetInout = False
isDefAnchorKind _ = True

-- Whether an IDef or ADef in local_defs is itself a preserved anchor
-- (ports, submodule ports, EN_/RDY_, WILL_FIRE_/CAN_FIRE_, explicit keep/probe).
-- State instances, parameters, and clocks/resets reserve their names in
-- envAnchorNames, so a local definition sharing their spelling must disambiguate
-- rather than impersonate the instance/param/clock.
classifyDefAnchorId :: SignalNamingEnv -> Id -> Maybe AnchorKind
classifyDefAnchorId env i =
    case classifyAnchorId env i of
        Just k | isDefAnchorKind k -> Just k
        _ -> Nothing

isDefAnchorId :: SignalNamingEnv -> Id -> Bool
isDefAnchorId env i = isJust (classifyDefAnchorId env i)

isUnnamedTempId :: SignalNamingEnv -> Id -> Bool
isUnnamedTempId env i
    | isEnvAnchorId env i = False
    | otherwise = isBadId i || isFromRHSId i

isDefTempId :: SignalNamingEnv -> Id -> Bool
isDefTempId env i
    | isDefAnchorId env i = False
    | otherwise = isBadId i || isFromRHSId i

-- True when a lowered expression is already a direct reference to a port,
-- parameter, named definition, or 0-argument state/submodule read, so a
-- library formal parameter (isLibraryId) attached to an argument alias yields
-- to the underlying signal rather than renaming it.
isDirectNamedSignalExpr :: SignalExpr -> Bool
isDirectNamedSignalExpr (SignalPort {}) = True
isDirectNamedSignalExpr (SignalParam {}) = True
isDirectNamedSignalExpr (SignalDef {}) = True
isDirectNamedSignalExpr (SignalMethCall _ _ _ _ []) = True
isDirectNamedSignalExpr _ = False

provisionalBinderBase :: Id -> String
provisionalBinderBase i =
    let (core, pd) = splitTrailingPDMarker (getIdBaseString i)
        trimmed = if isInferredId i then trimRightUnderscores core else core
    in if null trimmed
       then if null pd then "_v" else dropWhile (== '_') pd
       else trimmed ++ pd

-- ============================================================
-- Module-wide 2-pass collision resolver & incremental table
-- ============================================================

data ModuleNameItem tag
    = ItemAnchor !tag !Id
    | ItemUserBinder !tag !Id !(Maybe SignalExpr)
    | ItemTemp !tag !Id !SignalExpr
    deriving (Show)

isReservedCompilerPrefix :: String -> Bool
isReservedCompilerPrefix s =
    "MUX_" `L.isPrefixOf` s ||
    "mux_" `L.isPrefixOf` s ||
    "WILL_FIRE_" `L.isPrefixOf` s ||
    "CAN_FIRE_" `L.isPrefixOf` s ||
    "EN_" `L.isPrefixOf` s ||
    "RDY_" `L.isPrefixOf` s ||
    "COND_" `L.isPrefixOf` s ||
    "RL_" `L.isPrefixOf` s ||
    "instance_" `L.isPrefixOf` s ||
    "__me_check" `L.isPrefixOf` s

-- Assign final names across a module's anchors, user binders, and compiler
-- temporaries:
--   * Anchors keep their exact spelling; colliding anchors return Left.
--   * Unique non-colliding user binders keep their clean spelling; user binders
--     with an expression that collide with an anchor or sibling binder (or
--     carry IdPInferredName) disambiguate symmetrically via <base>_h<h4>
--     (upgrading to _h<h8> on (base, h4) collision); expressionless user
--     binders keep their spelling or disambiguate with _2, _3.
--   * Compiler-minted unnamed temporaries receive <stem>_h<h4>, upgrading
--     symmetrically to <stem>_h<h8> across any colliding (stem, h4) bucket.
envAnchorNames :: SignalNamingEnv -> S.Set String
envAnchorNames env =
    S.unions
        [ sne_params env
        , sne_clk_rst_inouts env
        , sne_state_insts env
        , sne_state_ports env
        , sne_inlined_ports env
        , sne_top_ports env
        , sne_explicit_anchors env
        , S.fromList ["CLK", "RST", "RST_N", "D_IN", "Q_OUT"]
        ]

assignModuleNames ::
    SignalNamingEnv ->
    [ModuleNameItem tag] ->
    Either AnchorCollision [(tag, String)]
assignModuleNames env items = do
    anchorMap <- L.foldl' checkAnchor (Right M.empty) items
    let anchorNames = S.union (envAnchorNames env) (M.keysSet anchorMap)
        indexedItems = zip [0 :: Int ..] items

        userBaseCounts :: M.Map String Int
        userBaseCounts =
            L.foldl'
                (\acc (_, item) ->
                    case item of
                        ItemUserBinder _ i _
                            | not (isItemAnchor item) ->
                                M.insertWith (+) (cleanUserBase i) 1 acc
                        _ -> acc)
                M.empty
                indexedItems

        needsHashUserBinder :: Id -> Maybe SignalExpr -> Bool
        needsHashUserBinder i (Just _) =
            let base = cleanUserBase i
            in isInferredId i ||
               isReservedCompilerPrefix base ||
               base `S.member` anchorNames ||
               M.findWithDefault 0 base userBaseCounts > 1
        needsHashUserBinder _ Nothing = False

        -- Place clean / expressionless user binders first.
        placeDirectUser (!usedSet, !ordMap, !acc) (!idx, item) =
            case item of
                ItemUserBinder _ i mSe
                    | not (isItemAnchor item)
                    , not (needsHashUserBinder i mSe) ->
                        let rawBase =
                                case mSe of
                                    Just _ -> cleanUserBase i
                                    Nothing -> getIdBaseString i
                            nextOrd = M.findWithDefault 1 rawBase ordMap
                            (chosen, !nextOrd') = pickFreeOrdinal usedSet rawBase nextOrd
                            !usedSet' = rnf chosen `seq` S.insert chosen usedSet
                            !ordMap' = rnf rawBase `seq` M.insert rawBase nextOrd' ordMap
                            !acc' = (idx, chosen) : acc
                        in (usedSet', ordMap', acc')
                _ -> (usedSet, ordMap, acc)

        (!takenAfterDirectUsers, !ordAfterDirectUsers, directUserRev) =
            L.foldl' placeDirectUser (anchorNames, M.empty, []) indexedItems

        -- Collect all hash-named items (disambiguated user binders + unnamed temps)
        -- in grounded encounter order so (stem, pd, h4) buckets upgrade
        -- symmetrically across the whole module.
        hashInfos =
            [ (idx, en, h4, h8)
            | (idx, item) <- indexedItems
            , Just en <- [extractHashExprName needsHashUserBinder item]
            , let h4 = exprNameH4 en
                  h8 = exprNameH8 en
            ]
        !h4Counts =
            L.foldl'
                (\ !acc (_, en, h4, _) ->
                    let !k = rnf h4 `seq` (en_stem en, en_pd_marker en, h4)
                    in M.insertWith (+) k (1 :: Int) acc)
                M.empty
                hashInfos
        needsH8 en h4 =
            M.findWithDefault 0 (en_stem en, en_pd_marker en, h4) h4Counts > 1 ||
            formatExprTempName en h4 `S.member` takenAfterDirectUsers

        allocHashed (!usedSet, !ordMap, !acc) (!idx, en, h4, h8) =
            let baseCand =
                    if needsH8 en h4
                    then formatExprTempName en h8
                    else formatExprTempName en h4
                nextOrd = M.findWithDefault 1 baseCand ordMap
                (chosen, !nextOrd') = pickFreeOrdinal usedSet baseCand nextOrd
                !usedSet' = rnf chosen `seq` S.insert chosen usedSet
                !ordMap' = rnf baseCand `seq` M.insert baseCand nextOrd' ordMap
                !acc' = (idx, chosen) : acc
            in (usedSet', ordMap', acc')
        (_, _, hashedRev) =
            L.foldl' allocHashed (takenAfterDirectUsers, ordAfterDirectUsers, []) hashInfos

        assignedMap = M.fromList (reverse directUserRev ++ reverse hashedRev)

    Right
        [ (itemTag item, resolveIdx idx item assignedMap)
        | (idx, item) <- indexedItems
        ]
  where
    cleanUserBase :: Id -> String
    cleanUserBase = provisionalBinderBase

    userBinderExprName :: Id -> SignalExpr -> ExprName
    userBinderExprName i se =
        let (core, pd) = splitAndTruncateStem (getIdBaseString i)
            cleanCore = trimRightUnderscores core
            stemCore = if null cleanCore then "_v" else cleanCore
            !digest = hashSignalExprShallow se
            en =
                ExprName
                    { en_stem = stemCore
                    , en_pd_marker = pd
                    , en_digest = digest
                    }
        in rnf en `seq` en

    explicitAnchorSet :: S.Set String
    explicitAnchorSet =
        L.foldl'
            (\acc item ->
                case item of
                    ItemAnchor _ i -> S.insert (getIdBaseString i) acc
                    _ -> acc)
            S.empty
            items

    classifyIntrinsicAnchor :: Id -> Maybe AnchorKind
    classifyIntrinsicAnchor i
        | isIdWillFire i || isIdCanFire i ||
          "WILL_FIRE_" `L.isPrefixOf` s || "CAN_FIRE_" `L.isPrefixOf` s =
            Just AnchorRuleFire
        | isIdEnable i || isRdyId i ||
          "EN_" `L.isPrefixOf` s || "RDY_" `L.isPrefixOf` s =
            Just AnchorEnableReady
        | '$' `elem` s =
            Just AnchorSubmodPort
        | hasIdProp i IdPProbe ||
          hasIdProp i IdP_keepEvenUnused ||
          hasIdProp i IdPMethodPredicate ||
          "COND_" `L.isPrefixOf` s =
            Just AnchorExplicitKeep
        | otherwise = Nothing
      where
        s = getIdBaseString i

    itemAnchorKind :: ModuleNameItem tag1 -> Maybe (String, AnchorKind)
    itemAnchorKind (ItemAnchor _ i) =
        let nm = getIdBaseString i
            kind = fromMaybe AnchorExplicitKeep (classifyAnchorId env i)
        in Just (nm, kind)
    itemAnchorKind (ItemUserBinder _ i (Just _)) =
        let nm = getIdBaseString i
        in case classifyIntrinsicAnchor i of
               Just AnchorExplicitKeep -> Nothing
               Just k -> Just (nm, k)
               Nothing -> Nothing
    itemAnchorKind (ItemUserBinder _ i Nothing) =
        let nm = getIdBaseString i
        in case classifyIntrinsicAnchor i of
               Just k -> Just (nm, k)
               Nothing
                   | nm `S.member` explicitAnchorSet -> Nothing
                   | otherwise -> fmap (\k -> (nm, k)) (classifyDefAnchorId env i)
    itemAnchorKind (ItemTemp _ i _) =
        let nm = getIdBaseString i
        in case classifyIntrinsicAnchor i of
               Just k -> Just (nm, k)
               Nothing
                   | (isBadId i || isFromRHSId i) && nm `S.member` explicitAnchorSet ->
                       Nothing
                   | otherwise -> fmap (\k -> (nm, k)) (classifyDefAnchorId env i)

    isItemAnchor :: ModuleNameItem tag1 -> Bool
    isItemAnchor item = isJust (itemAnchorKind item)

    checkAnchor ::
        Either AnchorCollision (M.Map String AnchorKind) ->
        ModuleNameItem tag ->
        Either AnchorCollision (M.Map String AnchorKind)
    checkAnchor (Left err) _ = Left err
    checkAnchor (Right m) item =
        case itemAnchorKind item of
            Nothing -> Right m
            Just (nm, kind) ->
                case M.lookup nm m of
                    Just prevKind -> Left (AnchorCollision nm prevKind kind)
                    Nothing -> Right (M.insert nm kind m)

    extractHashExprName ::
        (Id -> Maybe SignalExpr -> Bool) ->
        ModuleNameItem tag ->
        Maybe ExprName
    extractHashExprName needsHash item@(ItemUserBinder _ i mSe@(Just se))
        | not (isItemAnchor item) && needsHash i mSe =
            Just (userBinderExprName i se)
    extractHashExprName _ item@(ItemTemp _ _ se)
        | not (isItemAnchor item) =
            Just (exprName env se)
    extractHashExprName _ _ = Nothing

    itemTag :: ModuleNameItem tag -> tag
    itemTag (ItemAnchor t _) = t
    itemTag (ItemUserBinder t _ _) = t
    itemTag (ItemTemp t _ _) = t

    resolveIdx :: Int -> ModuleNameItem tag -> M.Map Int String -> String
    resolveIdx _ (ItemAnchor _ i) _ = getIdBaseString i
    resolveIdx idx (ItemUserBinder _ i _) aMap =
        M.findWithDefault (getIdBaseString i) idx aMap
    resolveIdx idx (ItemTemp _ i _) aMap =
        M.findWithDefault (getIdBaseString i) idx aMap

pickFreeOrdinal :: S.Set String -> String -> Int -> (String, Int)
pickFreeOrdinal usedSet base startOrd = go (max 1 startOrd)
  where
    go :: Int -> (String, Int)
    go k =
        let cand = if k == 1 then base else base ++ "_" ++ show k
        in if cand `S.member` usedSet
           then go (k + 1)
           else (cand, k + 1)

-- Incremental temporary-name table for passes (AVeriQuirks, ANoInline) that
-- mint helper definitions one at a time against an existing module namespace.
data SignalNameTable = SignalNameTable
    { snt_anchors :: !(M.Map String AnchorKind)
    , snt_used :: !(S.Set String)
    , snt_next_ord :: !(M.Map String Int)
    , snt_h4_seen :: !(M.Map (String, String, String) Word64)
    }
    deriving (Eq, Show)

emptySignalNameTable :: SignalNameTable
emptySignalNameTable =
    SignalNameTable
        { snt_anchors = M.empty
        , snt_used = S.empty
        , snt_next_ord = M.empty
        , snt_h4_seen = M.empty
        }

registerSignalAnchor ::
    SignalNamingEnv ->
    Id ->
    SignalNameTable ->
    Either AnchorCollision SignalNameTable
registerSignalAnchor env i tbl =
    let nm = getIdBaseString i
        kind = fromMaybe AnchorExplicitKeep (classifyAnchorId env i)
    in case M.lookup nm (snt_anchors tbl) of
           Just prevKind -> Left (AnchorCollision nm prevKind kind)
           Nothing ->
               Right
                   tbl
                       { snt_anchors = M.insert nm kind (snt_anchors tbl)
                       , snt_used = S.insert nm (snt_used tbl)
                       }

reserveExistingSignalName :: String -> SignalNameTable -> SignalNameTable
reserveExistingSignalName nm tbl =
    tbl { snt_used = S.insert nm (snt_used tbl) }

initSignalNameTable ::
    SignalNamingEnv ->
    [Id] ->
    Either AnchorCollision SignalNameTable
initSignalNameTable env ids = do
    let (anchorIds, nonAnchorIds) = L.partition (isEnvAnchorId env) ids
        uniqueAnchorIds = dedupByBase anchorIds
    tblWithAnchors <-
        L.foldl'
            (\acc i -> acc >>= registerSignalAnchor env i)
            (Right emptySignalNameTable)
            uniqueAnchorIds
    let tblWithEnv =
            tblWithAnchors
                { snt_used = S.union (envAnchorNames env) (snt_used tblWithAnchors)
                }
    Right
        (L.foldl'
            (\tbl i -> reserveExistingSignalName (getIdBaseString i) tbl)
            tblWithEnv
            nonAnchorIds)
  where
    dedupByBase :: [Id] -> [Id]
    dedupByBase = reverse . snd . L.foldl' step (S.empty, [])
      where
        step (seen, acc) i =
            let s = getIdBaseString i
            in if s `S.member` seen
               then (seen, acc)
               else (S.insert s seen, i : acc)

allocSignalTempName ::
    SignalNamingEnv ->
    SignalExpr ->
    SignalNameTable ->
    (String, SignalNameTable)
allocSignalTempName env se tbl =
    let en = exprName env se
        key4 = (en_stem en, en_pd_marker en, exprNameH4 en)
        cand4 = formatExprTempName en (exprNameH4 en)
        cand8 = formatExprTempName en (exprNameH8 en)
    in if not (M.member key4 (snt_h4_seen tbl)) &&
          not (cand4 `S.member` snt_used tbl)
       then
           let tbl' =
                   tbl
                       { snt_used = S.insert cand4 (snt_used tbl)
                       , snt_next_ord = M.insert cand4 2 (snt_next_ord tbl)
                       , snt_h4_seen = M.insert key4 (en_digest en) (snt_h4_seen tbl)
                       }
           in (cand4, tbl')
       else
           let nextOrd = M.findWithDefault 1 cand8 (snt_next_ord tbl)
               (chosen, nextOrd') = pickFreeOrdinal (snt_used tbl) cand8 nextOrd
               tbl' =
                   tbl
                       { snt_used = S.insert chosen (snt_used tbl)
                       , snt_next_ord = M.insert cand8 nextOrd' (snt_next_ord tbl)
                       , snt_h4_seen = M.insert key4 (en_digest en) (snt_h4_seen tbl)
                       }
           in (chosen, tbl')
