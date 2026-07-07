{-# LANGUAGE OverloadedStrings #-}

-- | BIR export: serialize the post-scheduling simulation system for the
-- TRS backend (src/trs).
--
-- The format is specified in src/trs/BIR.md and defined operationally
-- by the Rust types in src/trs/crates/trs-ir; @trs ir dump@
-- round-trips the output.  The input is the same 'SimSystem' that
-- 'simMakeCBlocks' consumes today (post 'simExpand' / 'simPackageOpt'),
-- so schedule merging and package optimization stay in this compiler.
--
-- Wire conventions match ciborium's serde defaults on the Rust side:
-- structs are CBOR maps keyed by field name, tuples and Vecs are arrays,
-- @Option@ is null-or-value, unit enum variants are strings, and payload
-- variants are single-entry maps ({variant: payload}).
--
-- STATUS (P0, in progress): exports the design skeleton plus module
-- bodies — clock domains, resets, inputs, instances (with method-order
-- pairs), defs, rules, and interface methods, with full expression and
-- action trees.  Not yet exported: segmented schedules, compositions,
-- ME inhibitors, foreign-function signatures, content hashes.  Unhandled
-- IR constructs fail loudly ('internalError') rather than exporting
-- wrong data.
module SimExportIR
    ( birVersion
    , simSystemToBir
    , writeBirFile
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import Control.Monad.State.Strict (State, runState, gets, modify)
import Data.Bits (shiftR, (.&.))
import Data.Word (Word32)

import qualified Codec.CBOR.Encoding as C
import qualified Codec.CBOR.Write as CW

import ErrorUtil (internalError)
import Id (Id, getIdBaseString, mkIdCanFire, mkIdWillFire)
import IntLit (IntLit(..))
import PPrint (ppReadable)
import Prim (PrimOp(..))
import Pragma (RulePragma(..))
import Wires (ClockDomain(..), ResetId, writeResetId, WireProps(..))
import VModInfo (vName, getVNameString)
import ASyntax
import SimPackage

-- | Bumped on any change to the encoded shape; must equal BIR_VERSION in
-- trs-ir/src/lib.rs.
birVersion :: Word32
birVersion = 1

-- ===============
-- String interning
--
-- All identifiers in BIR are indices into one design-wide string table.
-- Encoders run in a state monad accumulating the table; the table is
-- emitted after all bodies are encoded (CBOR encodings are values, so
-- assembly order is independent of write order).

data StrTable = StrTable !(M.Map String Word32) ![String] !Word32

type EncM = State StrTable

emptyStrTable :: StrTable
emptyStrTable = StrTable M.empty [] 0

str :: String -> EncM Word32
str s = do
    StrTable m rev n <- gets id
    case M.lookup s m of
      Just i  -> return i
      Nothing -> do
        modify (\_ -> StrTable (M.insert s n m) (s : rev) (n + 1))
        return n

strE :: String -> EncM C.Encoding
strE s = encW32 <$> str s

idE :: Id -> EncM C.Encoding
idE = strE . getIdBaseString

tableStrings :: StrTable -> [String]
tableStrings (StrTable _ rev _) = reverse rev

-- ===============
-- Encoding helpers (ciborium/serde conventions)

-- A struct is a map keyed by field name.
encStruct :: [(String, C.Encoding)] -> C.Encoding
encStruct fields =
    C.encodeMapLen (fromIntegral (length fields))
    <> mconcat [ encStr k <> v | (k, v) <- fields ]

-- A unit enum variant is its name.
encUnitVariant :: String -> C.Encoding
encUnitVariant = encStr

-- A payload-carrying enum variant is {name: payload}.
encVariant :: String -> C.Encoding -> C.Encoding
encVariant name payload = C.encodeMapLen 1 <> encStr name <> payload

-- Vec<T> and tuples are arrays.
encList :: [C.Encoding] -> C.Encoding
encList xs = C.encodeListLen (fromIntegral (length xs)) <> mconcat xs

encPair :: C.Encoding -> C.Encoding -> C.Encoding
encPair a b = C.encodeListLen 2 <> a <> b

-- Option<T> is null or the value.
encMaybe :: (a -> C.Encoding) -> Maybe a -> C.Encoding
encMaybe _ Nothing  = C.encodeNull
encMaybe f (Just x) = f x

encW32 :: Word32 -> C.Encoding
encW32 = C.encodeWord32

encBool :: Bool -> C.Encoding
encBool = C.encodeBool

encStr :: String -> C.Encoding
encStr = C.encodeString . T.pack

-- ===============
-- SimSystem -> BIR

-- | Encode a 'SimSystem' as a BIR design document.
simSystemToBir :: SimSystem -> L.ByteString
simSystemToBir ssys = CW.toLazyByteString (encDesign ssys)

-- | Write the design's .bir file.
writeBirFile :: FilePath -> SimSystem -> IO ()
writeBirFile path ssys = L.writeFile path (simSystemToBir ssys)

encDesign :: SimSystem -> C.Encoding
encDesign ssys =
    let pkgs = M.elems (ssys_packages ssys)
        pkgNames = S.fromList (map (getIdBaseString . sp_name) pkgs)
        instmap = M.toList (ssys_instmap ssys)

        action :: EncM [(String, C.Encoding)]
        action = do
          topId <- str (getIdBaseString (ssys_top ssys))
          modsEnc <- mapM (encModule pkgNames) pkgs
          instEnc <- mapM (\(p, m) -> encPair <$> strE p <*> strE m) instmap
          clkId <- traverse str (ssys_default_clk ssys)
          rstId <- traverse str (ssys_default_rst ssys)
          return
            [ ("version", encW32 birVersion)
            , ("strings", mempty)   -- placeholder, replaced below
            , ("top", encW32 topId)
            , ("modules", encList modsEnc)
            , ("instance_map", encList instEnc)
            , ("compositions", encList [])   -- P0 TODO: schedule composition
            , ("foreign_funcs", encList [])  -- P0 TODO: from ssys_ffuncmap
            , ("default_clock", encMaybe encW32 clkId)
            , ("default_reset", encMaybe encW32 rstId)
            ]

        (fields, finalTbl) = runState action emptyStrTable
        strsEnc = encList (map encStr (tableStrings finalTbl))
        fields' = [ (k, if k == "strings" then strsEnc else v)
                  | (k, v) <- fields ]
    in  encStruct fields'

-- ===============
-- Modules

encModule :: S.Set String -> SimPackage -> EncM C.Encoding
encModule pkgNames pkg = do
    nameId <- idE (sp_name pkg)
    domsEnc <- mapM encClockDomain (sp_clock_domains pkg)
    rstsEnc <- mapM encReset (sp_reset_list pkg)
    insEnc <- mapM encInput (sp_inputs pkg)
    instsEnc <- mapM (encInstance pkgNames (sp_method_order_map pkg))
                     (M.elems (sp_state_instances pkg))
    defsEnc <- mapM encDef (M.elems (sp_local_defs pkg))
    rulesEnc <- mapM encRule (sp_rules pkg)
    methodsEnc <- concat <$> mapM encMethod (sp_interface pkg)
    return $ encStruct
      [ ("name", nameId)
      , ("content_hash", encList (replicate 32 (C.encodeWord8 0))) -- P0 TODO
      , ("clock_domains", encList domsEnc)
      , ("resets", encList rstsEnc)
      , ("inputs", encList insEnc)
      , ("instances", encList instsEnc)
      , ("defs", encList defsEnc)
      , ("rules", encList rulesEnc)
      , ("methods", encList methodsEnc)
      , ("schedule", encSchedule)
      ]

-- P0 TODO: segmented per-(domain,edge) schedules (BIR.md section 4).
encSchedule :: C.Encoding
encSchedule =
    encStruct
      [ ("domains", encList [])
      , ("conflicts", encList [])
      , ("disjoint", encList [])
      ]

encClockDomain :: AClockDomain -> EncM C.Encoding
encClockDomain (ClockDomain n, clocks) = do
    clksEnc <- mapM (\c -> encPair <$> encExpr (aclock_osc c)
                                   <*> encExpr (aclock_gate c))
                    clocks
    return $ encStruct
      [ ("id", encW32 (fromIntegral n))
      , ("clocks", encList clksEnc)
      ]

encReset :: (ResetId, AReset) -> EncM C.Encoding
encReset (rid, rst) = do
    wireEnc <- encExpr (areset_wire rst)
    return $ encStruct
      [ ("id", encW32 (fromIntegral (writeResetId rid)))
      , ("wire", wireEnc)
      ]

encInput :: AAbstractInput -> EncM C.Encoding
encInput (AAI_Port (i, t)) = encPort (i, t) "MethodArg"
encInput (AAI_Clock osc _mgate) = do
    n <- idE osc
    return $ encPortRaw n 1 "Clock"
encInput (AAI_Reset r) = do
    n <- idE r
    return $ encPortRaw n 1 "Reset"
encInput (AAI_Inout {}) =
    internalError "SimExportIR.encInput: Inout not supported by Bluesim"

encPort :: (Id, AType) -> String -> EncM C.Encoding
encPort (i, t) kind = do
    n <- idE i
    return $ encPortRaw n (aTypeWidth t) kind

encPortRaw :: C.Encoding -> Word32 -> String -> C.Encoding
encPortRaw nameEnc w kind =
    encStruct
      [ ("name", nameEnc)
      , ("width", encW32 w)
      , ("kind", encUnitVariant kind)
      ]

encInstance :: S.Set String -> MethodOrderMap -> AVInst -> EncM C.Encoding
encInstance pkgNames mom avi = do
    nameId <- idE (avi_vname avi)
    let modName = getVNameString (vName (avi_vmi avi))
    kindEnc <-
      if modName `S.member` pkgNames
        then encVariant "Module" <$> strE modName
        -- P0 TODO: map primitives to their structured kinds (Reg, Fifo,
        -- ...) instead of Other; the structured mapping lands with codegen.
        else do mEnc <- strE modName
                return $ encVariant "Prim"
                           (encVariant "Other" (encStruct [("name", mEnc)]))
    argsEnc <- mapM encExpr (avi_iargs avi)
    let morder = S.toList (M.findWithDefault S.empty (avi_vname avi) mom)
    morderEnc <- mapM (\(a, b) -> encPair <$> idE a <*> idE b) morder
    portsEnc <- mapM (\(m, n) -> encPair <$> idE m
                                         <*> pure (encW32 (fromIntegral n)))
                     (avi_iarray avi)
    return $ encStruct
      [ ("name", nameId)
      , ("kind", kindEnc)
      , ("args", encList argsEnc)
      , ("method_order", encList morderEnc)
      , ("port_counts", encList portsEnc)
      ]

encDef :: ADef -> EncM C.Encoding
encDef (ADef i t e _props) = do
    nameId <- idE i
    exprEnc <- encExpr e
    let base = getIdBaseString i
        isCF = take 9 base == "CAN_FIRE_"
        isWF = take 10 base == "WILL_FIRE_"
    return $ encStruct
      [ ("name", nameId)
      , ("width", encW32 (aTypeWidth t))
      , ("expr", exprEnc)
      , ("props", encStruct
          [ ("can_fire", encBool isCF)
          , ("will_fire", encBool isWF)
          , ("signed", encBool False)   -- P0 TODO: from id props
          ])
      ]

encRule :: ARule -> EncM C.Encoding
encRule r = do
    nameId <- idE (arule_id r)
    -- The predicate is a reference to the CAN_FIRE def after
    -- aAddScheduleDefs; recover the def names.
    let cfId = case arule_pred r of
                 ASDef _ i -> i
                 _         -> mkIdCanFire (arule_id r)
    cf <- idE cfId
    wf <- idE (mkIdWillFire (arule_id r))
    bodyEnc <- mapM encAction (arule_actions r)
    let dom = case wpClockDomain (arule_wprops r) of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
        crossing = RPclockCrossingRule `elem` arule_pragmas r
    return $ encStruct
      [ ("name", nameId)
      , ("can_fire", cf)
      , ("will_fire", wf)
      , ("body", encList bodyEnc)
      , ("clock_domain", encW32 dom)
      , ("crossing", encBool crossing)
      , ("me_inhibits", encList [])   -- P0 TODO: with segmented schedules
      ]

-- Interface methods.  Clock/reset/inout interface entries carry no
-- executable content (they are in the clock/reset lists); skip them.
encMethod :: AIFace -> EncM [C.Encoding]
encMethod (AIDef name inputs props pred_ (ADef _ t e _) _ _) = do
    m <- encMethodStruct name "Value" (concat inputs) (Just pred_) [] (Just (t, e)) props
    return [m]
encMethod (AIAction inputs props pred_ name body _) = do
    m <- encMethodStruct name "Action" (concat inputs) (Just pred_)
                         (concatMap arule_actions body) Nothing props
    return [m]
encMethod (AIActionValue inputs props pred_ name body (ADef _ t e _) _) = do
    m <- encMethodStruct name "ActionValue" (concat inputs) (Just pred_)
                         (concatMap arule_actions body) (Just (t, e)) props
    return [m]
encMethod (AIClock {}) = return []
encMethod (AIReset {}) = return []
encMethod (AIInout {}) = return []

encMethodStruct :: Id -> String -> [AInput] -> Maybe APred -> [AAction]
                -> Maybe (AType, AExpr) -> WireProps -> EncM C.Encoding
encMethodStruct name kind inputs mpred body mresult props = do
    nameId <- idE name
    argsEnc <- mapM (\it -> encPort it "MethodArg") inputs
    readyEnc <- traverse encExpr mpred
    bodyEnc <- mapM encAction body
    resultEnc <- traverse (encExpr . snd) mresult
    let dom = case wpClockDomain props of
                Just (ClockDomain n) -> fromIntegral n
                Nothing -> 0
    return $ encStruct
      [ ("name", nameId)
      , ("kind", encUnitVariant kind)
      , ("args", encList argsEnc)
      , ("ready", encMaybe id readyEnc)
      , ("body", encList bodyEnc)
      , ("result", encMaybe id resultEnc)
      , ("clock_domain", encW32 dom)
      ]

-- ===============
-- Expressions

aTypeWidth :: AType -> Word32
aTypeWidth (ATBit n) = fromIntegral n
aTypeWidth (ATString _) = 0
aTypeWidth t = internalError ("SimExportIR.aTypeWidth: " ++ ppReadable t)

-- An Integer as little-endian 32-bit limbs (matching WideData layout).
toLimbs :: Word32 -> Integer -> [Word32]
toLimbs w v =
    let nlimbs = max 1 ((fromIntegral w + 31) `div` 32)
        limb k = fromIntegral ((v `shiftR` (32 * k)) .&. 0xFFFFFFFF)
    in  map limb [0 .. nlimbs - 1]

encExpr :: AExpr -> EncM C.Encoding
encExpr (ASInt _ t lit) =
    let w = aTypeWidth t
    in  return $ encVariant "Const" $ encStruct
          [ ("width", encW32 w)
          , ("limbs", encList (map encW32 (toLimbs w (ilValue lit))))
          ]
encExpr (ASDef _ i) = encVariant "Def" <$> idE i
encExpr (ASPort _ i) = encVariant "Port" <$> idE i
encExpr (ASParam _ i) = encVariant "Param" <$> idE i
encExpr (ASStr _ _ s) = encVariant "Str" <$> strE s
encExpr (AMethCall t obj meth args) = do
    o <- idE obj
    m <- idE meth
    argsEnc <- mapM encExpr args
    return $ encVariant "MethCall" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("instance", o)
      , ("method", m)
      , ("port", encW32 0)   -- P0 TODO: multi-port assignment
      , ("args", encList argsEnc)
      ]
encExpr (AMethValue t obj meth) = do
    o <- idE obj
    m <- idE meth
    return $ encVariant "MethValue" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("instance", o)
      , ("method", m)
      ]
encExpr (ATaskValue t _ _ _ cookie) =
    return $ encVariant "TaskValue" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("cookie", encW32 (fromIntegral cookie))
      ]
encExpr (AFunCall t _ fun _ args) = do
    f <- strE fun
    argsEnc <- mapM encExpr args
    return $ encVariant "ForeignCall" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("func", f)
      , ("args", encList argsEnc)
      ]
encExpr (AMGate _ obj clk) = do
    o <- idE obj
    c <- idE clk
    return $ encVariant "Gate" $ encStruct
      [ ("instance", o)
      , ("clock", c)
      ]
encExpr (ASClock _ clk) = do
    oscEnc <- encExpr (aclock_osc clk)
    gateEnc <- encExpr (aclock_gate clk)
    return $ encVariant "Clock" $ encStruct
      [ ("osc", oscEnc)
      , ("gate", gateEnc)
      ]
encExpr (ASReset _ rst) = do
    wireEnc <- encExpr (areset_wire rst)
    return $ encVariant "Reset" $ encStruct
      [ ("wire", wireEnc)
      ]
encExpr (APrim _ t PrimIf [c, x, y]) = do
    cEnc <- encExpr c
    xEnc <- encExpr x
    yEnc <- encExpr y
    return $ encVariant "If" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("cond", cEnc)
      , ("then_", xEnc)
      , ("else_", yEnc)
      ]
encExpr (APrim _ t PrimCase (scrut : dflt : arms)) = do
    sEnc <- encExpr scrut
    dEnc <- encExpr dflt
    armsEnc <- encCaseArms arms
    return $ encVariant "Case" $ encStruct
      [ ("width", encW32 (aTypeWidth t))
      , ("scrutinee", sEnc)
      , ("arms", encList armsEnc)
      , ("default", dEnc)
      ]
encExpr (APrim _ t op args) = do
    argsEnc <- mapM encExpr args
    return $ encVariant "Prim" $ encStruct
      [ ("op", encUnitVariant (primOpName op))
      , ("width", encW32 (aTypeWidth t))
      , ("args", encList argsEnc)
      ]
encExpr e = internalError ("SimExportIR.encExpr: " ++ ppReadable e)

encCaseArms :: [AExpr] -> EncM [C.Encoding]
encCaseArms [] = return []
encCaseArms (ASInt _ _ lit : v : rest) = do
    vEnc <- encExpr v
    restEnc <- encCaseArms rest
    return (encPair (C.encodeWord64 (fromIntegral (ilValue lit))) vEnc
            : restEnc)
encCaseArms es =
    internalError ("SimExportIR.encCaseArms: " ++ ppReadable es)

primOpName :: PrimOp -> String
primOpName PrimAdd = "Add"
primOpName PrimSub = "Sub"
primOpName PrimAnd = "And"
primOpName PrimOr = "Or"
primOpName PrimXor = "Xor"
primOpName PrimMul = "Mul"
primOpName PrimQuot = "Quot"
primOpName PrimRem = "Rem"
primOpName PrimSL = "Shl"
primOpName PrimSRL = "Lshr"
primOpName PrimSRA = "Ashr"
primOpName PrimInv = "Not"
primOpName PrimNeg = "Neg"
primOpName PrimEQ = "Eq"
primOpName PrimEQ3 = "Eq"   -- Bluesim is 2-state; === is ==
primOpName PrimULE = "Ule"
primOpName PrimULT = "Ult"
primOpName PrimSLE = "Sle"
primOpName PrimSLT = "Slt"
primOpName PrimSignExt = "SignExt"
primOpName PrimZeroExt = "ZeroExt"
primOpName PrimExtract = "Extract"
primOpName PrimConcat = "Concat"
primOpName PrimBNot = "Not"
primOpName PrimBAnd = "And"
primOpName PrimBOr = "Or"
primOpName PrimArrayDynSelect = "Select"
primOpName op = internalError ("SimExportIR.primOpName: " ++ show op)

-- ===============
-- Actions

encAction :: AAction -> EncM C.Encoding
encAction (ACall obj meth (cond : args)) = do
    o <- idE obj
    m <- idE meth
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "MethCall" $ encStruct
      [ ("instance", o)
      , ("method", m)
      , ("port", encW32 0)   -- P0 TODO: multi-port assignment
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      ]
encAction (AFCall _ fun _ (cond : args) _) = do
    f <- strE fun
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "Foreign" $ encStruct
      [ ("func", f)
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      ]
encAction (ATaskAction _ fun _ cookie (cond : args) mtemp mty _) = do
    f <- strE fun
    tempEnc <- traverse idE mtemp
    condEnc <- encExpr cond
    argsEnc <- mapM encExpr args
    return $ encVariant "Task" $ encStruct
      [ ("func", f)
      , ("cookie", encW32 (fromIntegral cookie))
      , ("temp", encMaybe id tempEnc)
      , ("width", encW32 (aTypeWidth mty))
      , ("cond", condEnc)
      , ("args", encList argsEnc)
      ]
encAction a = internalError ("SimExportIR.encAction: " ++ ppReadable a)
