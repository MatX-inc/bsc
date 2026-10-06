{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module Prim(
            -- the compilation-phase kind that PrimOp, IExpr and the
            -- rest of ISyntax are indexed by (re-exported by ISyntax)
            Binders(..), Evald(..), Phase(..),
            PreElab, Elab, PostElab, BinderPhase, EvaldPhase,
            PrimOp(..),
            primOpCode, primOpFromCode, allPrimOps, primOpAnyPhase,
            toPrim,
            toWString,
            stringSize,
            writePrimOp, readPrimOp,
            PrimResult(..), PrimArg(..)
            ) where

import Numeric(floatToDigits)
import Eval(NFData(..))
import PPrint
import Id
import Position
import Log2
import IntegerUtil
import RealUtil hiding (log2, log10)
import qualified RealUtil as R (log2,log10)
import ErrorUtil(internalError)
import Error(ErrMsg(..))
import PrimTH(primOpTables)

-- ==============================
-- The compilation-phase kind
--
-- An IExpr (and everything built from IExprs: IDef, IModule, IPackage,
-- IStateVar, Pred, ...) is indexed by the phase of compilation it
-- belongs to, and each constructor is restricted to the phases where it
-- can occur.  The index is a pair of axes rather than a flat enumeration
-- so that every restriction is a (partially) refined return type: GHC
-- stores nothing for those, whereas a constraint in a constructor's
-- context is an evidence field in every node.
--   Binders: may ILam/IVar/ILAM (and the pre-elaboration IConInfo
--            variants) occur?  Yes until the evaluator's rebuild (pDef).
--   Evald:   may the evaluator-made variants (instances, ports, module
--            definitions) occur?  Yes from the evaluator onwards.
-- The kind is declared here, in the lowest module whose type is indexed
-- by it (a PrimOp is the payload of ISyntax's ICPrim); ISyntax
-- re-exports it with the KnownPhase class, so the rest of the compiler
-- imports the phases from ISyntax as before.
data Binders = WithBinders | NoBinders
data Evald = Unevaluated | Evaluated
data Phase = Ph Binders Evald

-- The three phases that are inhabited:
--   PreElab:  IConv's output, the .bo contents, LiftDicts, FixupDefs,
--             ISimpDicts, ISimplify and the readers (bluetcl, dumpbo)
--   Elab:     inside the evaluator (IExpand/IExpandUtils, HExpr); the
--             only phase with heap references (IRefT, ArrayCell)
--   PostElab: from pDef's rebuild of the heap to AConv
-- ('Ph 'NoBinders 'Unevaluated is uninhabited.)
type PreElab = 'Ph 'WithBinders 'Unevaluated
type Elab = 'Ph 'WithBinders 'Evaluated
type PostElab = 'Ph 'NoBinders 'Evaluated

-- The two half-axes, for code that is polymorphic over the other axis:
--   BinderPhase e: PreElab or Elab (IConv's output, anything that builds
--                  ILam/IVar/ILAM or the pre-elaboration constants)
--   EvaldPhase b:  Elab or PostElab (the evaluator-made constants)
type BinderPhase e = 'Ph 'WithBinders e
type EvaldPhase b = 'Ph b 'Evaluated

-- ==============================
-- PrimOp
--
-- A primitive operation, indexed by the phases in which an ICPrim
-- holding it may occur: a constructor whose result type is `PrimOp p`
-- exists at every phase, one whose result type is refined exists only
-- at the phases it names.  (ICPrim :: PrimOp p -> IConInfo p, so the
-- index of the primitive is the index of the node holding it.)
--
-- The constructors are declared in three sections, and the order
-- within a section is deliberate: GHC gives the first six constructors
-- of a type with more than seven a direct pointer tag, so the hottest
-- six come first.  The declaration order is NOT the encoding order:
-- the .bo and .ba files write a primitive as its code, assigned by the
-- table at the end of this declaration, which keeps the encoding of
-- every existing primitive fixed while the declaration is free to be
-- arranged for the compiler.
data PrimOp (p :: Phase) where
        -- (1) The six most frequently dispatched primitives that also
        -- survive to AConv, hottest first.  Dispatch counts over the
        -- Sudoku, h264 and Flute elaborations (evalAp'/conAp'/walkNF/
        -- hToDef/iTrAp/doPrimOp' scrutinies, 2026-10-06): PrimIf 17.6%,
        -- PrimBNot 9.3%, PrimEQ 9.1%, PrimBAnd 8.2%, PrimBOr 4.1%,
        -- PrimConcat 3.7% of all PrimOp scrutinies.
        PrimIf :: PrimOp p
        PrimBNot :: PrimOp p
        PrimEQ :: PrimOp p
        PrimBAnd :: PrimOp p
        PrimBOr :: PrimOp p
        PrimConcat :: PrimOp p

        -- (2) The other primitives that exist at every phase: the
        -- hardware operations AConv converts, the action combinators
        -- flatAction consumes, and the ones a pass after elaboration
        -- still builds or consumes before AConv -- the four split
        -- markers (ISplitIf builds PrimExpIf and asserts at its end that
        -- no marker survives; ILift 262-266 re-wraps them, presumably
        -- dead after ISplitIf) and PrimFmtConcat (the Prelude's Fmt
        -- concatenation, which the evaluator leaves in normal form and
        -- IInlineFmt consumes).  Those five are gone by AConv by a
        -- runtime check, not by type.  PrimMux and PrimPriMux exist
        -- only in ASyntax, built after AConv; they are declared here
        -- until the muxes get AExpr constructors of their own.
        PrimAdd :: PrimOp p
        PrimSub :: PrimOp p
        PrimAnd :: PrimOp p
        PrimOr :: PrimOp p
        PrimXor :: PrimOp p

        PrimMul :: PrimOp p

        PrimQuot :: PrimOp p
        PrimRem :: PrimOp p

        PrimSL :: PrimOp p
        PrimSRL :: PrimOp p
        PrimSRA :: PrimOp p

        PrimInv :: PrimOp p
        PrimNeg :: PrimOp p

        PrimULE :: PrimOp p
        PrimULT :: PrimOp p

        PrimSLE :: PrimOp p
        PrimSLT :: PrimOp p

        PrimSignExt :: PrimOp p
        PrimZeroExt :: PrimOp p

        PrimTrunc :: PrimOp p

        PrimExtract :: PrimOp p
        PrimMux :: PrimOp p  -- built by AOpt/AState after AConv, never an ICPrim
        PrimPriMux :: PrimOp p  -- built by AOpt/AState after AConv, never an ICPrim

        PrimFmtConcat :: PrimOp p  -- Prelude.bs primFmtConcat; consumed by IInlineFmt

        -- Only in ATS
        -- use: PrimCase e d c1 e1 c2 e2 ... cn en
        -- e is the scrutinized expression, d is the default value, (ck, ek) forms a case arm
        PrimCase :: PrimOp p

        -- Only used in intermediate code
        -- primSelect @k @m @n e  selects k bits at position m from n bits
        -- primSelect :: \/ k, m, n :: * -> Bit n -> Bit k
        PrimSelect :: PrimOp p

        -- primRange lo hi x, promises lo <= x <= hi
        PrimRange :: PrimOp p

        PrimStringConcat :: PrimOp p

        PrimJoinActions :: PrimOp p
        PrimNoActions :: PrimOp p
        PrimExpIf :: PrimOp p  -- "split shallow"
        PrimNoExpIf :: PrimOp p  -- "nosplit shallow"
        PrimSplitDeep :: PrimOp p
        PrimNosplitDeep :: PrimOp p
        PrimResetUnassertedVal :: PrimOp p
        PrimArrayDynSelect :: PrimOp p
        PrimBuildArray :: PrimOp p  -- only exists after IExpand and in ASyntax

        PrimEQ3 :: PrimOp p  -- === / Verilog case equality


        -- (3) The primitives of elaboration only: folded by the
        -- evaluator (IPrims.doPrimOp', IExpand.conAp') or consumed by
        -- it (lists, strings, characters, Integer and Real arithmetic,
        -- names, positions, types, modules, rules, clock and reset
        -- queries, handles, when, poison, errors, arrays, pack/unpack).
        -- PrimSplit never reaches the evaluator at all (IConv rewrites
        -- a saturated primSplit to two selects) and PrimDynamicError
        -- has no use; both stay for the Prelude's declarations.
        PrimSplit :: PrimOp p

        PrimInoutCast :: PrimOp p
        PrimInoutUncast :: PrimOp p

        PrimMethod :: PrimOp p
        PrimNoInline :: PrimOp p

        -- primitives without hardware representation
        PrimIntegerToBit :: PrimOp p
        PrimIntegerToUIntBits :: PrimOp p
        PrimIntegerToIntBits :: PrimOp p
        PrimBitToInteger :: PrimOp p  -- XXX dangerous
        PrimIntegerToString :: PrimOp p

        -- must be called on compile-time values
        PrimIntBitsToInteger :: PrimOp p
        PrimUIntBitsToInteger :: PrimOp p

        PrimIsStaticInteger :: PrimOp p
        PrimAreStaticBits :: PrimOp p

        PrimValueOf :: PrimOp p
        PrimStringOf :: PrimOp p

        PrimWhen :: PrimOp p
        PrimWhenPred :: PrimOp p  -- takes abstract predicate

        PrimOrd :: PrimOp p
        PrimChr :: PrimOp p

        PrimError :: PrimOp p
        PrimGenerateError :: PrimOp p
        PrimMessage :: PrimOp p
        PrimWarning :: PrimOp p
        PrimPoisonedDef :: PrimOp p

        PrimDynamicError :: PrimOp p
        PrimStringToInteger :: PrimOp p
        PrimStringEQ :: PrimOp p
        PrimStringLT :: PrimOp p
        PrimStringLE :: PrimOp p
        PrimStringLength :: PrimOp p

        PrimStringSplit :: PrimOp p
        PrimStringCons :: PrimOp p

        PrimCharToString :: PrimOp p
        PrimStringToChar :: PrimOp p
        PrimCharOrd :: PrimOp p
        PrimCharChr :: PrimOp p
        PrimAddRules :: PrimOp p
        PrimModuleBind :: PrimOp p
        PrimModuleReturn :: PrimOp p
        PrimModuleFix :: PrimOp p
        PrimModuleClock :: PrimOp p
        PrimModuleReset :: PrimOp p
        PrimBuildModule :: PrimOp p
        PrimCurrentClock :: PrimOp p
        PrimCurrentReset :: PrimOp p
        PrimSameFamilyClock :: PrimOp p
        PrimIsAncestorClock :: PrimOp p
        PrimChkClockDomain :: PrimOp p
        PrimClockEQ :: PrimOp p
        PrimClockOf :: PrimOp p
        PrimClocksOf :: PrimOp p
        PrimNoClock :: PrimOp p
        PrimResetEQ :: PrimOp p
        PrimResetOf :: PrimOp p
        PrimResetsOf :: PrimOp p
        PrimNoReset :: PrimOp p
        PrimJoinRules :: PrimOp p
        PrimJoinRulesPreempt :: PrimOp p
        PrimJoinRulesUrgency :: PrimOp p
        PrimJoinRulesExecutionOrder :: PrimOp p
        PrimJoinRulesMutuallyExclusive :: PrimOp p
        PrimJoinRulesConflictFree :: PrimOp p
        PrimNoRules :: PrimOp p
        PrimRule :: PrimOp p

        -- PrimAddSchedPragmas :: [SchedulePragma] -> Rules -> Rules
        PrimAddSchedPragmas :: PrimOp p

        PrimGetName :: PrimOp p

        -- primStateName :: Name -> Module b -> Module b
        -- This primitive is used to name state components.
        -- The first argument is an abstract name that is added to
        -- the names of state elements instantiated by the second argument.
        PrimStateName :: PrimOp p
        PrimGetModuleName :: PrimOp p

        PrimJoinNames :: PrimOp p
        PrimExtendNameInteger :: PrimOp p
        PrimGetNamePosition :: PrimOp p
        PrimGetNameString :: PrimOp p
        PrimMakeName :: PrimOp p

        -- primStateAttrib :: Attributes -> Module b -> Module b
        -- This primitive is used to add attributes to submod instantiations.
        -- The first argument is an abstract list of attributes.
        PrimStateAttrib :: PrimOp p

        PrimNoPosition :: PrimOp p
        PrimPrintPosition :: PrimOp p
        PrimGetStringPosition :: PrimOp p
        PrimSetStringPosition :: PrimOp p
        PrimGetEvalPosition :: PrimOp p

        -- environment
        PrimGenC :: PrimOp p
        PrimGenVerilog :: PrimOp p
        PrimGenModuleName :: PrimOp p

        -- elaboration-time file IO
        PrimOpenFile :: PrimOp p
        PrimCloseHandle :: PrimOp p
        PrimHandleIsEOF :: PrimOp p
        PrimHandleIsOpen :: PrimOp p
        PrimHandleIsClosed :: PrimOp p
        PrimHandleIsReadable :: PrimOp p
        PrimHandleIsWritable :: PrimOp p
        PrimSetHandleBuffering :: PrimOp p
        PrimGetHandleBuffering :: PrimOp p
        PrimFlushHandle :: PrimOp p
        PrimWriteHandle :: PrimOp p
        PrimReadHandleLine :: PrimOp p
        PrimReadHandleChar :: PrimOp p

        -- reflective type primitives
        PrimTypeOf :: PrimOp p
        PrimPrintType :: PrimOp p
        PrimTypeEQ :: PrimOp p
        PrimIsIfcType :: PrimOp p

        -- type-tracking primitive
        PrimSavePortType :: PrimOp p

        -- compile time numbers
        PrimIntegerAdd :: PrimOp p
        PrimIntegerSub :: PrimOp p
        PrimIntegerNeg :: PrimOp p
        PrimIntegerMul :: PrimOp p
        PrimIntegerDiv :: PrimOp p
        PrimIntegerMod :: PrimOp p
        PrimIntegerExp :: PrimOp p
        PrimIntegerLog2 :: PrimOp p
        PrimIntegerLog10 :: PrimOp p
        PrimIntegerQuot :: PrimOp p
        PrimIntegerRem :: PrimOp p

        PrimIntegerEQ :: PrimOp p
        PrimIntegerLE :: PrimOp p
        PrimIntegerLT :: PrimOp p

        -- Real numbers: Show
        PrimRealToString :: PrimOp p

        -- Real numbers: Literal
        PrimIntegerToReal :: PrimOp p

        -- Real numbers: Eq and Ord
        PrimRealEQ :: PrimOp p
        PrimRealLE :: PrimOp p
        PrimRealLT :: PrimOp p

        -- Real numbers: Arith
        PrimRealAdd :: PrimOp p
        PrimRealSub :: PrimOp p
        PrimRealNeg :: PrimOp p
        PrimRealMul :: PrimOp p
        PrimRealDiv :: PrimOp p
        PrimRealAbs :: PrimOp p
        PrimRealSignum :: PrimOp p
        PrimRealExpE :: PrimOp p
        PrimRealPow :: PrimOp p
        PrimRealLogE :: PrimOp p
        PrimRealLogBase :: PrimOp p
        PrimRealLog2 :: PrimOp p
        PrimRealLog10 :: PrimOp p

        -- Real numbers: Bits
        PrimRealToBits :: PrimOp p
        PrimBitsToReal :: PrimOp p

        -- Real numbers: Trig
        PrimRealSin :: PrimOp p
        PrimRealCos :: PrimOp p
        PrimRealTan :: PrimOp p
        PrimRealSinH :: PrimOp p
        PrimRealCosH :: PrimOp p
        PrimRealTanH :: PrimOp p
        PrimRealASin :: PrimOp p
        PrimRealACos :: PrimOp p
        PrimRealATan :: PrimOp p
        PrimRealASinH :: PrimOp p
        PrimRealACosH :: PrimOp p
        PrimRealATanH :: PrimOp p
        PrimRealATan2 :: PrimOp p

        -- Real numbers: Sqrt
        PrimRealSqrt :: PrimOp p

        -- Real numbers: Rounding
        PrimRealTrunc :: PrimOp p
        PrimRealCeil :: PrimOp p
        PrimRealFloor :: PrimOp p
        PrimRealRound :: PrimOp p

        -- Real numbers: Introspection
        PrimSplitReal :: PrimOp p
        PrimDecodeReal :: PrimOp p
        PrimRealToDigits :: PrimOp p
        PrimRealIsInfinite :: PrimOp p
        PrimRealIsNegativeZero :: PrimOp p

        PrimSeq :: PrimOp p  -- args are eval in sequence
                             -- for side effects or strictness
        PrimSeqCond :: PrimOp p  -- implicit-condition strictness
        PrimUninitialized :: PrimOp p
        PrimRawUninitialized :: PrimOp p  -- error out with a use of an uninitialized value
        PrimMarkArrayUninitialized :: PrimOp p  -- mark array as uninitialized
        PrimMarkArrayInitialized :: PrimOp p  -- mark array as initialized
        PrimUninitBitArray :: PrimOp p  -- make an array of uninitialized bits
        PrimIsBitArray :: PrimOp p  -- is this Bit n represented as an array
        PrimUpdateBitArray :: PrimOp p
        PrimBuildUndefined :: PrimOp p  -- build a type-appropriate undefined value
        PrimRawUndefined :: PrimOp p  -- create a "raw" undefined value
        PrimIsRawUndefined :: PrimOp p  -- test if a value is a "raw" undefined value
        PrimImpCondOf :: PrimOp p  -- XXX experimental
        PrimArrayNew :: PrimOp p  -- Primitive array operators
        PrimArrayLength :: PrimOp p
        PrimArraySelect :: PrimOp p
        PrimArrayUpdate :: PrimOp p
        PrimArrayDynUpdate :: PrimOp p

        PrimSetSelPosition :: PrimOp p

        PrimGetParamName :: PrimOp p  -- get the parameter name associated with the function value

        -- implicit Bits pack/unpack coercions; the Prelude wrappers
        -- (Prelude.pack/Prelude.unpack) apply these to the Bits dictionary,
        -- and the evaluator unfolds them to the corresponding class method.
        -- They never survive past IExpand.
        PrimPack :: PrimOp p
        PrimUnpack :: PrimOp p


-- The code tables, generated by Template Haskell (the first use of it
-- in this repository) from the declaration above and the list below:
--
--   primOpCode     :: PrimOp p -> Int       the code a file writes
--   primOpFromCode :: Int -> PrimOp PreElab the primitive a .bo reads
--   allPrimOps     :: [PrimOp PreElab]      every primitive, code order
--   primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
--                     the same primitive at another phase, when it
--                     exists at every phase (Nothing for one declared
--                     at the binder phases only; the evaluator's rebuild
--                     and the .ba reader refuse those)
--
-- The list is the encoding: a primitive's code is its position, and
-- the positions are those of the `deriving Enum` the type had before
-- it was indexed, so the .bo and .ba bytes are unchanged.  A new
-- primitive goes at the END of the list, with the .bo and .ba format
-- tags bumped (GenBin.header, GenABin.header); a removed one keeps its
-- entry, named in the retired list, so later codes do not shift.  The
-- splice reifies the type and refuses to compile when a constructor
-- has no entry, an entry has no constructor, or a name repeats, so the
-- table cannot drift from the declaration.
--
-- Why generated: with the declaration arranged for the compiler, the
-- two directions of a 224-entry table would otherwise be hand-written
-- twice and checked by nothing.  If a build cannot run a splice (a
-- cross-compiler, or a profiling build without the non-profiled
-- objects or -fexternal-interpreter), the fallback is to paste the
-- generated declarations (ghc -ddump-splices) into a PrimCodes.hs and
-- import it here in place of the splice.
$(primOpTables ''PrimOp ''PreElab
  [
    "PrimAdd", "PrimSub", "PrimAnd", "PrimOr",
    "PrimXor", "PrimMul", "PrimQuot", "PrimRem",
    "PrimSL", "PrimSRL", "PrimSRA", "PrimInv",
    "PrimNeg", "PrimEQ", "PrimULE", "PrimULT",
    "PrimSLE", "PrimSLT", "PrimSignExt", "PrimZeroExt",
    "PrimTrunc", "PrimExtract", "PrimConcat", "PrimSplit",
    "PrimBNot", "PrimBAnd", "PrimBOr", "PrimInoutCast",
    "PrimInoutUncast", "PrimMethod", "PrimNoInline", "PrimIf",
    "PrimMux", "PrimPriMux", "PrimFmtConcat", "PrimCase",
    "PrimSelect", "PrimIntegerToBit", "PrimIntegerToUIntBits", "PrimIntegerToIntBits",
    "PrimBitToInteger", "PrimIntegerToString", "PrimIntBitsToInteger", "PrimUIntBitsToInteger",
    "PrimIsStaticInteger", "PrimAreStaticBits", "PrimValueOf", "PrimStringOf",
    "PrimWhen", "PrimWhenPred", "PrimOrd", "PrimChr",
    "PrimRange", "PrimError", "PrimGenerateError", "PrimMessage",
    "PrimWarning", "PrimPoisonedDef", "PrimDynamicError", "PrimStringConcat",
    "PrimStringToInteger", "PrimStringEQ", "PrimStringLT", "PrimStringLE",
    "PrimStringLength", "PrimStringSplit", "PrimStringCons", "PrimCharToString",
    "PrimStringToChar", "PrimCharOrd", "PrimCharChr", "PrimJoinActions",
    "PrimNoActions", "PrimExpIf", "PrimNoExpIf", "PrimSplitDeep",
    "PrimNosplitDeep", "PrimAddRules", "PrimModuleBind", "PrimModuleReturn",
    "PrimModuleFix", "PrimModuleClock", "PrimModuleReset", "PrimBuildModule",
    "PrimCurrentClock", "PrimCurrentReset", "PrimSameFamilyClock", "PrimIsAncestorClock",
    "PrimChkClockDomain", "PrimClockEQ", "PrimClockOf", "PrimClocksOf",
    "PrimNoClock", "PrimResetEQ", "PrimResetOf", "PrimResetsOf",
    "PrimNoReset", "PrimResetUnassertedVal", "PrimJoinRules", "PrimJoinRulesPreempt",
    "PrimJoinRulesUrgency", "PrimJoinRulesExecutionOrder", "PrimJoinRulesMutuallyExclusive", "PrimJoinRulesConflictFree",
    "PrimNoRules", "PrimRule", "PrimAddSchedPragmas", "PrimGetName",
    "PrimStateName", "PrimGetModuleName", "PrimJoinNames", "PrimExtendNameInteger",
    "PrimGetNamePosition", "PrimGetNameString", "PrimMakeName", "PrimStateAttrib",
    "PrimNoPosition", "PrimPrintPosition", "PrimGetStringPosition", "PrimSetStringPosition",
    "PrimGetEvalPosition", "PrimGenC", "PrimGenVerilog", "PrimGenModuleName",
    "PrimOpenFile", "PrimCloseHandle", "PrimHandleIsEOF", "PrimHandleIsOpen",
    "PrimHandleIsClosed", "PrimHandleIsReadable", "PrimHandleIsWritable", "PrimSetHandleBuffering",
    "PrimGetHandleBuffering", "PrimFlushHandle", "PrimWriteHandle", "PrimReadHandleLine",
    "PrimReadHandleChar", "PrimTypeOf", "PrimPrintType", "PrimTypeEQ",
    "PrimIsIfcType", "PrimSavePortType", "PrimIntegerAdd", "PrimIntegerSub",
    "PrimIntegerNeg", "PrimIntegerMul", "PrimIntegerDiv", "PrimIntegerMod",
    "PrimIntegerExp", "PrimIntegerLog2", "PrimIntegerLog10", "PrimIntegerQuot",
    "PrimIntegerRem", "PrimIntegerEQ", "PrimIntegerLE", "PrimIntegerLT",
    "PrimRealToString", "PrimIntegerToReal", "PrimRealEQ", "PrimRealLE",
    "PrimRealLT", "PrimRealAdd", "PrimRealSub", "PrimRealNeg",
    "PrimRealMul", "PrimRealDiv", "PrimRealAbs", "PrimRealSignum",
    "PrimRealExpE", "PrimRealPow", "PrimRealLogE", "PrimRealLogBase",
    "PrimRealLog2", "PrimRealLog10", "PrimRealToBits", "PrimBitsToReal",
    "PrimRealSin", "PrimRealCos", "PrimRealTan", "PrimRealSinH",
    "PrimRealCosH", "PrimRealTanH", "PrimRealASin", "PrimRealACos",
    "PrimRealATan", "PrimRealASinH", "PrimRealACosH", "PrimRealATanH",
    "PrimRealATan2", "PrimRealSqrt", "PrimRealTrunc", "PrimRealCeil",
    "PrimRealFloor", "PrimRealRound", "PrimSplitReal", "PrimDecodeReal",
    "PrimRealToDigits", "PrimRealIsInfinite", "PrimRealIsNegativeZero", "PrimSeq",
    "PrimSeqCond", "PrimUninitialized", "PrimRawUninitialized", "PrimMarkArrayUninitialized",
    "PrimMarkArrayInitialized", "PrimUninitBitArray", "PrimIsBitArray", "PrimUpdateBitArray",
    "PrimBuildUndefined", "PrimRawUndefined", "PrimIsRawUndefined", "PrimImpCondOf",
    "PrimArrayNew", "PrimArrayLength", "PrimArraySelect", "PrimArrayUpdate",
    "PrimArrayDynSelect", "PrimArrayDynUpdate", "PrimBuildArray", "PrimSetSelPosition",
    "PrimGetParamName", "PrimEQ3", "PrimPack", "PrimUnpack"
  ]
  [])

-- Equality and order are by code, which is the order of the enum the
-- type derived them from before it was indexed: the Map and Set
-- iteration orders that key on an expression holding a primitive are
-- unchanged, whatever the declaration order above.
instance Eq (PrimOp p) where
    a == b = primOpCode a == primOpCode b

instance Ord (PrimOp p) where
    compare a b = compare (primOpCode a) (primOpCode b)

deriving instance Show (PrimOp p)


-- Just some size, have to be coordinated with Prelude.bs
stringSize :: String -> Integer
stringSize s = toInteger (8 * length s)        -- in bytes as bits

toPrim :: Id -> PrimOp (BinderPhase e)
toPrim i = tp (getIdBaseString i)                -- XXXXX
  where tp "primAdd" = PrimAdd
        tp "primSub" = PrimSub
        tp "primAnd" = PrimAnd
        tp "primOr"  = PrimOr
        tp "primXor" = PrimXor
        tp "primMul" = PrimMul
        tp "primQuot" = PrimQuot
        tp "primRem" = PrimRem
        tp "primSL"  = PrimSL
        tp "primSRL" = PrimSRL
        tp "primSRA" = PrimSRA
        tp "primInv" = PrimInv
        tp "primNeg" = PrimNeg
        tp "primEQ"  = PrimEQ
        tp "primEQ3" = PrimEQ3
        tp "primULE" = PrimULE
        tp "primULT" = PrimULT
        tp "primSLE" = PrimSLE
        tp "primSLT" = PrimSLT
        tp "primSignExt" = PrimSignExt
        tp "primZeroExt" = PrimZeroExt
        tp "primTrunc" = PrimTrunc
        tp "primExtractInternal" = PrimExtract
        tp "primConcat" = PrimConcat
        tp "primSplit" = PrimSplit
        tp "primBNot" = PrimBNot
        tp "primBAnd" = PrimBAnd
        tp "primBOr" = PrimBOr
        tp "primInoutCast" = PrimInoutCast
        tp "primInoutUncast" = PrimInoutUncast
        tp "primMethod" = PrimMethod
        tp "primNoInline" = PrimNoInline
        tp "primIntegerToBit" = PrimIntegerToBit
        tp "primIntegerToUIntBits" = PrimIntegerToUIntBits
        tp "primIntegerToIntBits"  = PrimIntegerToIntBits
        tp "primBitToInteger" = PrimBitToInteger
        tp "primIntegerToString" = PrimIntegerToString
        tp "primUIntBitsToInteger" = PrimUIntBitsToInteger
        tp "primIntBitsToInteger"  = PrimIntBitsToInteger
        tp "primIsStaticInteger" = PrimIsStaticInteger
        tp "primAreStaticBits" = PrimAreStaticBits
        tp "primWhen" = PrimWhen
        tp "primValueOf" = PrimValueOf
        tp "primStringOf" = PrimStringOf
        tp "primOrd" = PrimOrd
        tp "primChr" = PrimChr
        tp "primIf" = PrimIf
        tp "primRange" = PrimRange
        tp "primError" = PrimError
        tp "primPoisonedDef" = PrimPoisonedDef
        tp "primGenerateError" = PrimGenerateError
        tp "primMessage" = PrimMessage
        tp "primWarning" = PrimWarning
        tp "primDynamicError" = PrimDynamicError

        tp "primJoinActions" = PrimJoinActions
        tp "primNoActions" = PrimNoActions
        tp "primExpIf" = PrimExpIf
        tp "primNoExpIf" = PrimNoExpIf
        tp "primSplitDeep" = PrimSplitDeep
        tp "primNosplitDeep" = PrimNosplitDeep
        tp "primAddRules" = PrimAddRules
        tp "primModuleBind" = PrimModuleBind
        tp "primModuleReturn" = PrimModuleReturn
        tp "primModuleFix" = PrimModuleFix
        tp "primModuleClock" = PrimModuleClock
        tp "primModuleReset" = PrimModuleReset
        tp "primBuildModule" = PrimBuildModule
        tp "primJoinRules" = PrimJoinRules
        tp "primJoinRulesPreempt" = PrimJoinRulesPreempt
        tp "primJoinRulesUrgency" = PrimJoinRulesUrgency
        tp "primJoinRulesExecutionOrder" = PrimJoinRulesExecutionOrder
        tp "primJoinRulesMutuallyExclusive" = PrimJoinRulesMutuallyExclusive
        tp "primJoinRulesConflictFree" = PrimJoinRulesConflictFree
        tp "primNoRules" = PrimNoRules
        tp "primRule" = PrimRule

        tp "primStringConcat" = PrimStringConcat
        tp "primStringToInteger" = PrimStringToInteger
        tp "primStringEQ" = PrimStringEQ
        tp "primStringLT" = PrimStringLT
        tp "primStringLE" = PrimStringLE
        tp "primStringLength" = PrimStringLength

        tp "primStringSplit" = PrimStringSplit
        tp "primStringCons" = PrimStringCons

        tp "primCharToString" = PrimCharToString
        tp "primStringToChar" = PrimStringToChar
        tp "primCharOrd" = PrimCharOrd
        tp "primCharChr" = PrimCharChr

        tp "primFmtConcat" = PrimFmtConcat
        tp "primCurrentClock" = PrimCurrentClock
        tp "primCurrentReset" = PrimCurrentReset
        tp "primSameFamilyClock" = PrimSameFamilyClock
        tp "primIsAncestorClock" = PrimIsAncestorClock
        tp "primChkClockDomain" = PrimChkClockDomain
        tp "primClockEQ" = PrimClockEQ
        tp "primClockOf" = PrimClockOf
        tp "primClocksOf" = PrimClocksOf
        tp "primNoClock" = PrimNoClock
        tp "primResetEQ" = PrimResetEQ
        tp "primResetOf" = PrimResetOf
        tp "primResetsOf" = PrimResetsOf
        tp "primNoReset" = PrimNoReset
        tp "primResetUnassertedVal" = PrimResetUnassertedVal

        tp "primGetName" = PrimGetName
        tp "primGetParamName" = PrimGetParamName
        tp "primStateName" = PrimStateName
        tp "primGetModuleName" = PrimGetModuleName
        tp "primJoinNames" = PrimJoinNames
        tp "primExtendNameInteger" = PrimExtendNameInteger
        tp "primGetNamePosition" = PrimGetNamePosition
        tp "primGetNameString" = PrimGetNameString
        tp "primMakeName" = PrimMakeName

        tp "primStateAttrib" = PrimStateAttrib

        tp "primNoPosition" = PrimNoPosition
        tp "primPrintPosition" = PrimPrintPosition
        tp "primGetStringPosition" = PrimGetStringPosition
        tp "primSetStringPosition" = PrimSetStringPosition
        tp "primGetEvalPosition" = PrimGetEvalPosition

        tp "primGenC" = PrimGenC
        tp "primGenVerilog" = PrimGenVerilog
        tp "primGenModuleName" = PrimGenModuleName

        tp "primOpenFile" = PrimOpenFile
        tp "primCloseHandle" = PrimCloseHandle
        tp "primHandleIsEOF" = PrimHandleIsEOF
        tp "primHandleIsOpen" = PrimHandleIsOpen
        tp "primHandleIsClosed" = PrimHandleIsClosed
        tp "primHandleIsReadable" = PrimHandleIsReadable
        tp "primHandleIsWritable" = PrimHandleIsWritable
        tp "primSetHandleBuffering" = PrimSetHandleBuffering
        tp "primGetHandleBuffering" = PrimGetHandleBuffering
        tp "primFlushHandle" = PrimFlushHandle
        tp "primWriteHandle" = PrimWriteHandle
        tp "primReadHandleLine" = PrimReadHandleLine
        tp "primReadHandleChar" = PrimReadHandleChar

        tp "primTypeOf" = PrimTypeOf
        tp "primPrintType" = PrimPrintType
        tp "primTypeEQ" = PrimTypeEQ
        tp "primIsIfcType" = PrimIsIfcType

        tp "primSavePortType" = PrimSavePortType

        tp "primSeq" = PrimSeq
        tp "primSeqCond" = PrimSeqCond

        tp "primIntegerAdd" = PrimIntegerAdd
        tp "primIntegerSub" = PrimIntegerSub
        tp "primIntegerNeg" = PrimIntegerNeg
        tp "primIntegerMul" = PrimIntegerMul
        tp "primIntegerDiv" = PrimIntegerDiv
        tp "primIntegerMod" = PrimIntegerMod
        tp "primIntegerQuot" = PrimIntegerQuot
        tp "primIntegerRem" = PrimIntegerRem
        tp "primIntegerExp" = PrimIntegerExp
        tp "primIntegerLog2" = PrimIntegerLog2
        tp "primIntegerLog10" = PrimIntegerLog10

        tp "primIntegerEQ"  = PrimIntegerEQ
        tp "primIntegerLE"  = PrimIntegerLE
        tp "primIntegerLT"  = PrimIntegerLT

        tp "primRealToString" = PrimRealToString
        tp "primIntegerToReal" = PrimIntegerToReal
        tp "primRealEQ"  = PrimRealEQ
        tp "primRealLE"  = PrimRealLE
        tp "primRealLT"  = PrimRealLT
        tp "primRealAdd"  = PrimRealAdd
        tp "primRealSub"  = PrimRealSub
        tp "primRealNeg"  = PrimRealNeg
        tp "primRealMul"  = PrimRealMul
        tp "primRealDiv"  = PrimRealDiv
        tp "primRealAbs"  = PrimRealAbs
        tp "primRealSignum" = PrimRealSignum
        tp "primRealExpE" = PrimRealExpE
        tp "primRealPow"  = PrimRealPow
        tp "primRealLogE" = PrimRealLogE
        tp "primRealLogBase" = PrimRealLogBase
        tp "primRealLog2" = PrimRealLog2
        tp "primRealLog10" = PrimRealLog10
        tp "primRealToBits" = PrimRealToBits
        tp "primBitsToReal" = PrimBitsToReal
        tp "primRealSin"    = PrimRealSin
        tp "primRealCos"    = PrimRealCos
        tp "primRealTan"    = PrimRealTan
        tp "primRealSinH"   = PrimRealSinH
        tp "primRealCosH"   = PrimRealCosH
        tp "primRealTanH"   = PrimRealTanH
        tp "primRealASin"   = PrimRealASin
        tp "primRealACos"   = PrimRealACos
        tp "primRealATan"   = PrimRealATan
        tp "primRealASinH"  = PrimRealASinH
        tp "primRealACosH"  = PrimRealACosH
        tp "primRealATanH"  = PrimRealATanH
        tp "primRealATan2"  = PrimRealATan2
        tp "primRealSqrt"   = PrimRealSqrt
        tp "primRealTrunc"  = PrimRealTrunc
        tp "primRealCeil"   = PrimRealCeil
        tp "primRealFloor"  = PrimRealFloor
        tp "primRealRound"  = PrimRealRound
        tp "primSplitReal"  = PrimSplitReal
        tp "primDecodeReal" = PrimDecodeReal
        tp "primRealToDigits" = PrimRealToDigits
        tp "primRealIsInfinite" = PrimRealIsInfinite
        tp "primRealIsNegativeZero" = PrimRealIsNegativeZero

        tp "primUninitialized" = PrimUninitialized
        tp "primMakeRawUninitialized" = PrimRawUninitialized
        tp "primMarkArrayUninitialized" = PrimMarkArrayUninitialized
        -- the same primitive is used to implement both of these
        tp "primMarkArrayInitialized" = PrimMarkArrayInitialized
        tp "primMarkBitArrayInitialized" = PrimMarkArrayInitialized
        tp "primUninitBitArray" = PrimUninitBitArray
        tp "primIsBitArray" = PrimIsBitArray
        tp "primUpdateBitArray" = PrimUpdateBitArray
        tp "primBuildUndefined" = PrimBuildUndefined
        tp "primMakeRawUndefined" = PrimRawUndefined
        tp "primIsRawUndefined" = PrimIsRawUndefined
        tp "primImpCondOf" = PrimImpCondOf
        tp "primArrayNew" = PrimArrayNew
        tp "primArrayLength" = PrimArrayLength
        tp "primArraySelect" = PrimArraySelect
        tp "primArrayUpdate" = PrimArrayUpdate
        tp "primArrayDynSelect" = PrimArrayDynSelect
        tp "primArrayDynUpdate" = PrimArrayDynUpdate
        tp "primBuildArray" = PrimBuildArray

        tp "primSetSelPosition" = PrimSetSelPosition

        tp "primPack" = PrimPack
        tp "primUnpack" = PrimUnpack
        tp s = internalError ("unknown primitive: " ++ s ++ " " ++ prPosition (getIdPosition i))

instance PPrint (PrimOp p) where
    pPrint d p op = text (toString op)

toString :: PrimOp p -> String
toString PrimAdd = "+"
toString PrimSub = "-"
toString PrimAnd = "&"
toString PrimOr = "|"
toString PrimXor = "^"
toString PrimMul = "*"
toString PrimQuot = "/"
toString PrimRem = "%"
toString PrimSL = "<<"
toString PrimSRL = ">>"
toString PrimSRA = ">>>"
toString PrimInv = "~"
toString PrimNeg = "-"
toString PrimEQ = "=="
toString PrimEQ3 = "==="
toString PrimULE = "<="
toString PrimULT = "<"
toString PrimSLE = ".<="
toString PrimSLT = ".<"
toString PrimSignExt = "sext"
toString PrimZeroExt = "zext"
toString PrimTrunc = "trunc"
toString PrimExtract = "extract"
toString PrimConcat = "++"
toString PrimSplit = "split"
toString PrimBNot = "!"
toString PrimBAnd = "&&"
toString PrimBOr = "||"
toString PrimInoutCast = "primInoutCast"
toString PrimInoutUncast = "primInoutUncast"
toString PrimIf = "_if_"
toString PrimSelect = "select"
toString PrimValueOf = "valueOf"
toString PrimStringOf = "stringOf"
toString PrimOrd = "ord"
toString PrimChr = "chr"
toString PrimError = "_error"
toString PrimCurrentClock = "primCurrentClock"
toString p = show p

-- to name of wide-data operations
toWString :: PrimOp p -> String
toWString PrimAdd = "add"
toWString PrimSub = "sub"
toWString PrimAnd = "and"
toWString PrimOr = "or"
toWString PrimXor = "xor"
toWString PrimMul = "mul"
toWString PrimQuot = "quot"
toWString PrimRem = "rem"
toWString PrimSL = "sl"
toWString PrimSRL = "srl"
toWString PrimSRA = "sra"
toWString PrimInv = "inv"
toWString PrimNeg = "neg"
toWString PrimEQ = "eq"
toWString PrimULE = "ule"
toWString PrimULT = "ult"
toWString PrimSLE = "sle"
toWString PrimSLT = "slt"
toWString PrimSignExt = "sext"
toWString PrimZeroExt = "zext"
toWString PrimTrunc = "trunc"
toWString PrimExtract = "extract"
toWString PrimConcat = "concat"
toWString PrimSplit = "split"
toWString PrimBNot = "bnot"
toWString PrimBAnd = "band"
toWString PrimBOr = "bor"
toWString PrimIf = "_if_"
toWString PrimSelect = "select"
toWString PrimValueOf = "valueOf"
toWString PrimStringOf = "stringOf"
toWString PrimOrd = "ord"
toWString PrimChr = "chr"
toWString PrimError = "_error"
toWString PrimCurrentClock = "primCurrentClock"
toWString p = show p

instance NFData (PrimOp p) where
    rnf PrimAdd = ()
    rnf PrimSub = ()
    rnf PrimAnd = ()
    rnf PrimOr = ()
    rnf PrimXor = ()
    rnf PrimMul = ()
    rnf PrimQuot = ()
    rnf PrimRem = ()
    rnf PrimSL = ()
    rnf PrimSRL = ()
    rnf PrimSRA = ()
    rnf PrimInv = ()
    rnf PrimNeg = ()
    rnf PrimEQ = ()
    rnf PrimULE = ()
    rnf PrimULT = ()
    rnf PrimSLE = ()
    rnf PrimSLT = ()
    rnf PrimSignExt = ()
    rnf PrimZeroExt = ()
    rnf PrimTrunc = ()
    rnf PrimExtract = ()
    rnf PrimConcat = ()
    rnf PrimSplit = ()
    rnf PrimBNot = ()
    rnf PrimBAnd = ()
    rnf PrimBOr = ()
    rnf PrimInoutCast = ()
    rnf PrimInoutUncast = ()
    rnf PrimMethod = ()
    rnf PrimNoInline = ()
    rnf PrimIf = ()
    rnf PrimMux = ()
    rnf PrimPriMux = ()
    rnf PrimFmtConcat = ()
    rnf PrimCase = ()
    rnf PrimSelect = ()
    rnf PrimIntegerToBit = ()
    rnf PrimIntegerToUIntBits = ()
    rnf PrimIntegerToIntBits = ()
    rnf PrimBitToInteger = ()
    rnf PrimIntegerToString = ()
    rnf PrimIntBitsToInteger = ()
    rnf PrimUIntBitsToInteger = ()
    rnf PrimIsStaticInteger = ()
    rnf PrimAreStaticBits = ()
    rnf PrimValueOf = ()
    rnf PrimStringOf = ()
    rnf PrimWhen = ()
    rnf PrimWhenPred = ()
    rnf PrimOrd = ()
    rnf PrimChr = ()
    rnf PrimRange = ()
    rnf PrimError = ()
    rnf PrimGenerateError = ()
    rnf PrimMessage = ()
    rnf PrimWarning = ()
    rnf PrimPoisonedDef = ()
    rnf PrimDynamicError = ()
    rnf PrimStringConcat = ()
    rnf PrimStringToInteger = ()
    rnf PrimStringEQ = ()
    rnf PrimStringLT = ()
    rnf PrimStringLE = ()
    rnf PrimStringLength = ()
    rnf PrimStringSplit = ()
    rnf PrimStringCons = ()
    rnf PrimCharToString = ()
    rnf PrimStringToChar = ()
    rnf PrimCharOrd = ()
    rnf PrimCharChr = ()
    rnf PrimJoinActions = ()
    rnf PrimNoActions = ()
    rnf PrimExpIf = ()
    rnf PrimNoExpIf = ()
    rnf PrimSplitDeep = ()
    rnf PrimNosplitDeep = ()
    rnf PrimAddRules = ()
    rnf PrimModuleBind = ()
    rnf PrimModuleReturn = ()
    rnf PrimModuleFix = ()
    rnf PrimModuleClock = ()
    rnf PrimModuleReset = ()
    rnf PrimBuildModule = ()
    rnf PrimCurrentClock = ()
    rnf PrimCurrentReset = ()
    rnf PrimSameFamilyClock = ()
    rnf PrimIsAncestorClock = ()
    rnf PrimChkClockDomain = ()
    rnf PrimClockEQ = ()
    rnf PrimClockOf = ()
    rnf PrimClocksOf = ()
    rnf PrimNoClock = ()
    rnf PrimResetEQ = ()
    rnf PrimResetOf = ()
    rnf PrimResetsOf = ()
    rnf PrimNoReset = ()
    rnf PrimResetUnassertedVal = ()
    rnf PrimJoinRules = ()
    rnf PrimJoinRulesPreempt = ()
    rnf PrimJoinRulesUrgency = ()
    rnf PrimJoinRulesExecutionOrder = ()
    rnf PrimJoinRulesMutuallyExclusive = ()
    rnf PrimJoinRulesConflictFree = ()
    rnf PrimNoRules = ()
    rnf PrimRule = ()
    rnf PrimAddSchedPragmas = ()
    rnf PrimGetName = ()
    rnf PrimStateName = ()
    rnf PrimGetModuleName = ()
    rnf PrimJoinNames = ()
    rnf PrimExtendNameInteger = ()
    rnf PrimGetNamePosition = ()
    rnf PrimGetNameString = ()
    rnf PrimMakeName = ()
    rnf PrimStateAttrib = ()
    rnf PrimNoPosition = ()
    rnf PrimPrintPosition = ()
    rnf PrimGetStringPosition = ()
    rnf PrimSetStringPosition = ()
    rnf PrimGetEvalPosition = ()
    rnf PrimGenC = ()
    rnf PrimGenVerilog = ()
    rnf PrimGenModuleName = ()
    rnf PrimOpenFile = ()
    rnf PrimCloseHandle = ()
    rnf PrimHandleIsEOF = ()
    rnf PrimHandleIsOpen = ()
    rnf PrimHandleIsClosed = ()
    rnf PrimHandleIsReadable = ()
    rnf PrimHandleIsWritable = ()
    rnf PrimSetHandleBuffering = ()
    rnf PrimGetHandleBuffering = ()
    rnf PrimFlushHandle = ()
    rnf PrimWriteHandle = ()
    rnf PrimReadHandleLine = ()
    rnf PrimReadHandleChar = ()
    rnf PrimTypeOf = ()
    rnf PrimPrintType = ()
    rnf PrimTypeEQ = ()
    rnf PrimIsIfcType = ()
    rnf PrimSavePortType = ()
    rnf PrimIntegerAdd = ()
    rnf PrimIntegerSub = ()
    rnf PrimIntegerNeg = ()
    rnf PrimIntegerMul = ()
    rnf PrimIntegerDiv = ()
    rnf PrimIntegerMod = ()
    rnf PrimIntegerExp = ()
    rnf PrimIntegerLog2 = ()
    rnf PrimIntegerLog10 = ()
    rnf PrimIntegerQuot = ()
    rnf PrimIntegerRem = ()
    rnf PrimIntegerEQ = ()
    rnf PrimIntegerLE = ()
    rnf PrimIntegerLT = ()
    rnf PrimRealToString = ()
    rnf PrimIntegerToReal = ()
    rnf PrimRealEQ = ()
    rnf PrimRealLE = ()
    rnf PrimRealLT = ()
    rnf PrimRealAdd = ()
    rnf PrimRealSub = ()
    rnf PrimRealNeg = ()
    rnf PrimRealMul = ()
    rnf PrimRealDiv = ()
    rnf PrimRealAbs = ()
    rnf PrimRealSignum = ()
    rnf PrimRealExpE = ()
    rnf PrimRealPow = ()
    rnf PrimRealLogE = ()
    rnf PrimRealLogBase = ()
    rnf PrimRealLog2 = ()
    rnf PrimRealLog10 = ()
    rnf PrimRealToBits = ()
    rnf PrimBitsToReal = ()
    rnf PrimRealSin = ()
    rnf PrimRealCos = ()
    rnf PrimRealTan = ()
    rnf PrimRealSinH = ()
    rnf PrimRealCosH = ()
    rnf PrimRealTanH = ()
    rnf PrimRealASin = ()
    rnf PrimRealACos = ()
    rnf PrimRealATan = ()
    rnf PrimRealASinH = ()
    rnf PrimRealACosH = ()
    rnf PrimRealATanH = ()
    rnf PrimRealATan2 = ()
    rnf PrimRealSqrt = ()
    rnf PrimRealTrunc = ()
    rnf PrimRealCeil = ()
    rnf PrimRealFloor = ()
    rnf PrimRealRound = ()
    rnf PrimSplitReal = ()
    rnf PrimDecodeReal = ()
    rnf PrimRealToDigits = ()
    rnf PrimRealIsInfinite = ()
    rnf PrimRealIsNegativeZero = ()
    rnf PrimSeq = ()
    rnf PrimSeqCond = ()
    rnf PrimUninitialized = ()
    rnf PrimRawUninitialized = ()
    rnf PrimMarkArrayUninitialized = ()
    rnf PrimMarkArrayInitialized = ()
    rnf PrimUninitBitArray = ()
    rnf PrimIsBitArray = ()
    rnf PrimUpdateBitArray = ()
    rnf PrimBuildUndefined = ()
    rnf PrimRawUndefined = ()
    rnf PrimIsRawUndefined = ()
    rnf PrimImpCondOf = ()
    rnf PrimArrayNew = ()
    rnf PrimArrayLength = ()
    rnf PrimArraySelect = ()
    rnf PrimArrayUpdate = ()
    rnf PrimArrayDynSelect = ()
    rnf PrimArrayDynUpdate = ()
    rnf PrimBuildArray = ()
    rnf PrimSetSelPosition = ()
    rnf PrimGetParamName = ()
    rnf PrimEQ3 = ()
    rnf PrimPack = ()
    rnf PrimUnpack = ()

-----

-- The .ba encoding of a primitive (BinData's Bin instance): the code,
-- read back at PostElab.  The .bo (GenBin) uses primOpCode and
-- primOpFromCode directly, at PreElab, where every code is a primitive.

writePrimOp :: PrimOp p -> Int
writePrimOp = primOpCode

readPrimOp :: Int -> PrimOp PostElab
readPrimOp n =
    case primOpAnyPhase (primOpFromCode n) of
      Just p -> p
      Nothing -> internalError ("readPrimOp: an elaboration-only primitive " ++
                                "in a .ba: " ++ show (primOpFromCode n))

-- -------------------------------------------------------------------
-- Routines for evaluating prim ops with constant arguments

-- shorthand notation
ans :: a -> Maybe (Either ErrMsg a)
ans x = Just $ Right x

err :: ErrMsg -> Maybe (Either ErrMsg a)
err msg = Just $ Left msg

no_answer :: Maybe a
no_answer = Nothing  -- XXX shouldn't these really be errors?
                     -- XXX See discussion of doPrimOp in IPrims.hs


-- ----
-- Data type for mixing argument types

data PrimArg = I Integer
             | D Double
             deriving (Eq)

instance PPrint PrimArg where
    pPrint d p (I i) = pparen (p > 0) $ text "I" <+> pPrint d 0 i
    pPrint d p (D r) = pparen (p > 0) $ text "D" <+> pPrint d 0 r

-- ----
-- Primitives returning Integer
evalPrimToInt :: PrimOp p -> [Integer] -> [PrimArg] ->
                 Maybe (Either ErrMsg Integer)

-- basic math on sized values
evalPrimToInt PrimAdd  [s]     [I i1, I i2] = ans $ mask s (i1+i2)
evalPrimToInt PrimSub  [s]     [I i1, I i2] = ans $ mask s (i1-i2)
evalPrimToInt PrimMul  [_,_,s] [I i1, I i2] = ans $ mask s (i1*i2)
evalPrimToInt PrimQuot [s,_]   [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ mask s (i1 `quot` i2)
evalPrimToInt PrimRem  [_,s]   [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ mask s (i1 `rem` i2)
evalPrimToInt PrimNeg  [s]     [I i1]    = ans $ mask s (-i1)

-- bit-wise operators on sized values
evalPrimToInt PrimAnd [s] [I i1, I i2] = ans $ i1 `integerAnd` i2
evalPrimToInt PrimOr  [s] [I i1, I i2] = ans $ i1 `integerOr`  i2
evalPrimToInt PrimXor [s] [I i1, I i2] = ans $ i1 `integerXor` i2
evalPrimToInt PrimInv [s] [I i1]       = ans $ mask s (integerInvert i1)

-- shifting on sized values
evalPrimToInt PrimSL  [s,_] [I x, I sh]  =
    if (sh >= s)
    then ans 0
    else if (sh >= 0)
         then ans $ mask s (x * (2^sh))
         else no_answer
evalPrimToInt PrimSRL [s,_] [I x, I sh]  =
    if (sh >= s)
    then ans 0
    else if (sh >= 0)
         then ans $ x `div` (2^sh)
         else no_answer
evalPrimToInt PrimSRA [s,_] [I x, I sh]  =
    if (sh >= s)
    then -- this behavior, i.e., 0 if you shift too much,
         -- is intended to emulate what verilog simulators
         -- do.  iverilog and ncverilog have this behavior
         ans 0
    else if (sh >= 0)
         then evalPrimToInt PrimSignExt [sh, s-sh, s] [I (x `div` (2^sh))]
         else no_answer

-- extension and truncation on sized values
evalPrimToInt PrimZeroExt [_,_,_]   [I i] = ans i
evalPrimToInt PrimSignExt [_,s1,s2] [I i] =
    if (s1 >= 1 && s2 >= 0)  -- _ + s1 = s2
    then ans $ if i >= 2^(s1-1) then 2^s2 - 2^s1 + i else i
    else no_answer
evalPrimToInt PrimTrunc   [_,s,_]   [I i] = ans $ mask s i

-- extraction, concatenation and range-checking on sized values
evalPrimToInt PrimRange [s] [I lo, I hi, I x] =
    if x < lo || x > hi then
        internalError ("evalPrimToInt: PrimRange " ++ ppReadable (lo,hi,x))
    else
        ans $ x
evalPrimToInt PrimExtract [n,_,m] [I e, I h, I l] =
    if (h-l+1 < 0)
    then internalError
             ("evalPrimToInt: PrimExtract extract negative number of bits "
              ++ ppReadable ((n,m),(h,l,e)))
    else if (l >= 0)
         then ans $ mask m (integerSelect (h-l+1) l e)
         else no_answer
evalPrimToInt PrimConcat [s1,s2,s3] [I i1, I i2] =
    if (s2 >= 0)
    then ans $ i1 * 2^s2 + i2
    else no_answer
evalPrimToInt PrimSelect [k,m,_]    [I i] =
    if (m >= 0)
    then ans $ integerSelect k m i
    else no_answer
-- evalPrimToInt PrimSplit _ _ =

-- math on unbounded Integers
evalPrimToInt PrimIntegerAdd  _ [I i1, I i2] = ans $ i1 + i2
evalPrimToInt PrimIntegerSub  _ [I i1, I i2] = ans $ i1 - i2
evalPrimToInt PrimIntegerNeg  _ [I i1]       = ans $ (-i1)
evalPrimToInt PrimIntegerMul  _ [I i1, I i2] = ans $ i1 * i2
evalPrimToInt PrimIntegerDiv  _ [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ i1 `div` i2
evalPrimToInt PrimIntegerMod  _ [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ i1 `mod` i2
evalPrimToInt PrimIntegerQuot _ [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ i1 `quot` i2
evalPrimToInt PrimIntegerRem  _ [I i1, I i2] =
    if (i2 == 0)
    then err EDivideByZero
    else ans $ i1 `rem` i2
evalPrimToInt PrimIntegerExp  _ [I i1, I i2] =
    if (i2 < 0)
    then err EIntegerNegativeExponent
    else ans $ i1 ^ i2
evalPrimToInt PrimIntegerLog2 _ [I i1]       =
    if (i1 <= 0)
    then err EInvalidLog
    else ans $ log2 i1
evalPrimToInt PrimIntegerLog10 _ [I i1]      =
    if (i1 <= 0)
    then err EInvalidLog
    else ans $ log10 i1

-- conversions from unbounded to sized and vice-versa
evalPrimToInt PrimIntegerToBit [s] [I i] =
    if (i < 2^s && i > -(2^s))
    then ans $ mask s i
    else no_answer
evalPrimToInt PrimBitToInteger _   [I i] = ans i

-- conditional
evalPrimToInt PrimIf [s] [I i1, I i2, I i3] =
    ans $ if (i1 == 1) then i2 else i3

-- conversion to bits
evalPrimToInt PrimRealToBits _ [D d1] =
    ans $ toInteger (doubleToWord64 d1)

-- rounding, truncation, etc.
evalPrimToInt PrimRealTrunc _ [D d1] = ans $ truncate d1
evalPrimToInt PrimRealCeil  _ [D d1] = ans $ ceiling d1
evalPrimToInt PrimRealFloor _ [D d1] = ans $ floor d1
evalPrimToInt PrimRealRound _ [D d1] = ans $ round d1

-- all other prim ops are unhandled
evalPrimToInt _ _ _ = no_answer


-- ----
-- Primitives returning Bool
evalPrimToBool :: PrimOp p -> [Integer] -> [PrimArg] ->
                  Maybe (Either ErrMsg Bool)

-- relational operators on sized values
evalPrimToBool PrimEQ  [s] [I i1, I i2] = ans $ i1 == i2
evalPrimToBool PrimEQ3 [s] [I i1, I i2] = ans $ i1 == i2
evalPrimToBool PrimULE [s] [I i1, I i2] = ans $ i1 <= i2
evalPrimToBool PrimULT [s] [I i1, I i2] = ans $ i1 <  i2
evalPrimToBool PrimSLE [s] [I i1, I i2] = ans $ (ext s i1) <= (ext s i2)
evalPrimToBool PrimSLT [s] [I i1, I i2] = ans $ (ext s i1) <  (ext s i2)

-- boolean logic
evalPrimToBool PrimBNot      _ [I i]        = ans $ i == 0
evalPrimToBool PrimBAnd      _ [I i1, I i2] = ans $ (i1 == 1) && (i2 == 1)
evalPrimToBool PrimBOr       _ [I i1, I i2] = ans $ (i1 == 1) || (i2 == 1)

-- relational operators on unbounded Integers
evalPrimToBool PrimIntegerEQ _ [I i1, I i2] = ans $ i1 == i2
evalPrimToBool PrimIntegerLE _ [I i1, I i2] = ans $ i1 <= i2
evalPrimToBool PrimIntegerLT _ [I i1, I i2] = ans $ i1 <  i2

-- relational operators
evalPrimToBool PrimRealEQ _ [D d1, D d2] = ans $ d1 == d2
evalPrimToBool PrimRealLE _ [D d1, D d2] = ans $ d1 <= d2
evalPrimToBool PrimRealLT _ [D d1, D d2] = ans $ d1 <  d2

-- property tests
evalPrimToBool PrimRealIsInfinite     _ [D d1] = ans $ isInfinite d1
evalPrimToBool PrimRealIsNegativeZero _ [D d1] = ans $ isNegativeZero d1

-- all other prim ops are unhandled
evalPrimToBool _ _ _ = no_answer


-- ----
-- Primitives returning Double
evalPrimToDouble :: PrimOp p -> [Integer] -> [PrimArg] ->
                    Maybe (Either ErrMsg Double)

evalPrimToDouble PrimIntegerToReal _ [I i1] = ans $ fromInteger i1
evalPrimToDouble PrimBitsToReal    _ [I i1] =
    let d = word64ToDouble (fromInteger i1)
    in  if (isNaN d) then err EFloatNaN else ans d

-- basic math operators
evalPrimToDouble PrimRealAdd _ [D d1, D d2] =
    if ( (isPosInfinite d1 && isNegInfinite d2) ||
         (isNegInfinite d1 && isPosInfinite d2) )
    then err EAddPosAndNegInfinity
    else ans $ d1 + d2
evalPrimToDouble PrimRealSub _ [D d1, D d2] =
    if ( (isPosInfinite d1 && isPosInfinite d2) ||
         (isNegInfinite d1 && isNegInfinite d2) )
    then err EAddPosAndNegInfinity
    else ans $ d1 - d2
evalPrimToDouble PrimRealNeg _ [D d1] = ans (-d1)
evalPrimToDouble PrimRealMul _ [D d1, D d2] =
    if ( (isInfinite d1 && (d2 == 0.0)) ||
         (isInfinite d2 && (d1 == 0.0)) )
    then err EMultiplyZeroAndInfinity
    else ans $ d1 * d2
evalPrimToDouble PrimRealDiv _ [D d1, D d2] =
    if (d2 == 0.0)
    then err EDivideByZero
    else ans $ d1 / d2
evalPrimToDouble PrimRealAbs    _ [D d1] = ans (abs d1)
evalPrimToDouble PrimRealSignum _ [D d1] = ans (signum d1)

-- exponents and logarithms
evalPrimToDouble PrimRealExpE _ [D d1] = ans $ exp d1
evalPrimToDouble PrimRealPow  _ [D d1, D d2] = ans $ d1 ** d2
evalPrimToDouble PrimRealLogE _ [D d1] =
    if (d1 <= 0)
    then err EInvalidLog
    else  ans $ log d1
evalPrimToDouble PrimRealLogBase _ [D d1, D d2] =
    if (d2 <= 0)
    then err EInvalidLog
    else if (d1 <= 0)
    then err EInvalidLogBase
    else if ((d1 == 1) && (d2 == 1))
    then err EInvalidLogOneOne
    else ans $ logBase d1 d2
evalPrimToDouble PrimRealLog2 _ [D d1] =
    if (d1 <= 0)
    then err EInvalidLog
    else ans $ R.log2 d1
evalPrimToDouble PrimRealLog10 _ [D d1] =
    if (d1 <= 0)
    then err EInvalidLog
    else ans $ R.log10 d1
evalPrimToDouble PrimRealSqrt _ [D d1] =
    if (d1 < 0)
    then err ENegativeSqrt
    else ans $ sqrt d1

-- trig functions
evalPrimToDouble PrimRealSin   _ [D d1] = ans $ sin d1
evalPrimToDouble PrimRealCos   _ [D d1] = ans $ cos d1
evalPrimToDouble PrimRealTan   _ [D d1] = ans $ tan d1
evalPrimToDouble PrimRealSinH  _ [D d1] = ans $ sinh d1
evalPrimToDouble PrimRealCosH  _ [D d1] = ans $ cosh d1
evalPrimToDouble PrimRealTanH  _ [D d1] = ans $ tanh d1
evalPrimToDouble PrimRealASin  _ [D d1] = ans $ asin d1
evalPrimToDouble PrimRealACos  _ [D d1] = ans $ acos d1
evalPrimToDouble PrimRealATan  _ [D d1] = ans $ atan d1
evalPrimToDouble PrimRealASinH _ [D d1] = ans $ asinh d1
evalPrimToDouble PrimRealACosH _ [D d1] = ans $ acosh d1
evalPrimToDouble PrimRealATanH _ [D d1] = ans $ atanh d1
evalPrimToDouble PrimRealATan2 _ [D d1, D d2] = ans $ atan2 d1 d2

-- all other prim ops are unhandled
evalPrimToDouble _ _ _ = no_answer


-- ----
-- Double to String primitives
evalPrimToString :: PrimOp p -> [Integer] -> [PrimArg] ->
                    Maybe (Either ErrMsg String)

-- XXX PrimIntegerToString could go here, but it needs access to the base
-- XXX (which we could add to PrimArg)
evalPrimToString PrimRealToString _ [D d1] = ans $ show d1

-- all other prim ops on Doubles are unhandled
evalPrimToString _ _ _ = no_answer


-- ----
-- Primitives returning (Integer, Double)
evalPrimToIntDouble :: PrimOp p -> [Integer] -> [PrimArg] ->
                       Maybe (Either ErrMsg (Integer,Double))

evalPrimToIntDouble PrimSplitReal _ [D d1] = ans $ properFraction d1

-- all other prim ops are unhandled
evalPrimToIntDouble _ _ _ = no_answer


-- ----
-- Primitives returning (Bool, Integer, Integer)
evalPrimToBoolIntInt :: PrimOp p -> [Integer] -> [PrimArg] ->
                        Maybe (Either ErrMsg (Bool,Integer,Integer))

evalPrimToBoolIntInt PrimDecodeReal _ [D d1] =
    let (mantissa, exponent) = decodeFloat d1
        -- include positive zero
        is_pos = (d1 >= 0) && not (isNegativeZero d1)
    in  ans (is_pos, mantissa, (toInteger exponent))

-- all other prim ops are unhandled
evalPrimToBoolIntInt _ _ _ = no_answer


-- ----
-- Primitives returning ([Integer], Integer)
evalPrimToListIntInt :: PrimOp p -> [Integer] -> [PrimArg] ->
                        Maybe (Either ErrMsg ([Integer], Integer))

evalPrimToListIntInt PrimRealToDigits _ [I i, D d] =
    let -- "floatToDigits" only works for non-negative values
        (digits, exponent) = floatToDigits i (abs d)
    in  ans (map toInteger digits, toInteger exponent)

-- all other prim ops are unhandled
evalPrimToListIntInt _ _ _ = no_answer


-- -------------------------------------------------------------------
-- Typeclass to simplify evaluation of prim ops

class PrimResult a where
  primResult :: PrimOp p -> [Integer] -> [PrimArg] -> Maybe (Either ErrMsg a)
  -- default implementation is to return no_answer for everything
  primResult _ _ _ = no_answer

instance PrimResult Bool where
  primResult op ts vs = evalPrimToBool op ts vs

instance PrimResult Integer where
  primResult op ts vs = evalPrimToInt op ts vs

instance PrimResult Double where
  primResult op ts vs = evalPrimToDouble op ts vs

instance PrimResult String where
  primResult op ts vs = evalPrimToString op ts vs

instance PrimResult (Integer, Double) where
  primResult op ts vs = evalPrimToIntDouble op ts vs

instance PrimResult (Bool, Integer, Integer) where
  primResult op ts vs = evalPrimToBoolIntInt op ts vs

instance PrimResult ([Integer], Integer) where
  primResult op ts vs = evalPrimToListIntInt op ts vs
