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
            primOpTableHash,
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
-- Which is which: the primitives of elaboration only -- folded by the
-- evaluator (IPrims.doPrimOp', IExpand.conAp') or consumed by it:
-- lists, strings, characters, Integer and Real arithmetic, names,
-- positions, types, modules, rules, clock and reset queries, handles,
-- when, poison, errors, arrays, pack/unpack -- are declared at the
-- binder phases, 'Ph 'WithBinders e.  (PrimSplit never reaches the
-- evaluator at all, IConv rewrites a saturated primSplit to two
-- selects, and PrimDynamicError has no use; both stay for the
-- Prelude's declarations.)  The rest, `PrimOp p`, are the hardware
-- operations AConv converts, the action combinators flatAction
-- consumes, and five that a pass after elaboration still builds or
-- consumes before AConv: the four split markers (ISplitIf builds
-- PrimExpIf and asserts at its end that no marker survives; ILift
-- re-wraps them) and PrimFmtConcat (the Prelude's Fmt concatenation,
-- which the evaluator leaves in normal form and IInlineFmt consumes);
-- those five are gone by AConv by a runtime check, not by type.
-- PrimOrd and PrimChr, the Bool <-> Bit 1 coercions, are universal
-- although elaboration folds them, because LambdaCalcUtil builds them
-- again after AConv, as the casts its type repair inserts for the SAL
-- and lambda-calculus dumps.  (The two muxes AState and AOpt build
-- after AConv are not primitives: ASyntax's AMux.)
--
-- The declaration order is the order of the enumeration this type was
-- before it was indexed, with PrimMux and PrimPriMux (now AMux) taken
-- out, and it is load-bearing twice.  It is the encoding: the .bo and
-- .ba files write a primitive as its code, which is its position here
-- (its constructor tag), and the format tags carry a hash of the table
-- (see the splice after the declaration), so a primitive added,
-- removed, renamed or moved changes the format identity by itself.
-- And it is the derived Ord, which the AExpr-keyed containers of the
-- passes after elaboration follow (see the instances after the
-- splice).  Re-declaring the constructors in another order -- grouped
-- by the phases their types name, say, or hottest first -- is
-- therefore a format change like any other, which the hash announces
-- and the testsuite's pin (bsc.binary/primcodes) shows; the one such
-- re-declaration tried cost 0.5-1.6% of user time on the
-- elaboration-only harness against this order, with the same
-- instances and the same allocation (the commit that made the codes
-- follow the declaration has the numbers), so the old order stays.
data PrimOp (p :: Phase) where
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

        PrimEQ :: PrimOp p

        PrimULE :: PrimOp p
        PrimULT :: PrimOp p

        PrimSLE :: PrimOp p
        PrimSLT :: PrimOp p

        PrimSignExt :: PrimOp p
        PrimZeroExt :: PrimOp p

        PrimTrunc :: PrimOp p

        PrimExtract :: PrimOp p
        PrimConcat :: PrimOp p
        PrimSplit :: PrimOp ('Ph 'WithBinders e)

        PrimBNot :: PrimOp p
        PrimBAnd :: PrimOp p
        PrimBOr :: PrimOp p

        PrimInoutCast :: PrimOp ('Ph 'WithBinders e)
        PrimInoutUncast :: PrimOp ('Ph 'WithBinders e)

        PrimMethod :: PrimOp ('Ph 'WithBinders e)
        PrimNoInline :: PrimOp ('Ph 'WithBinders e)

        PrimIf :: PrimOp p

        PrimFmtConcat :: PrimOp p  -- Prelude.bs primFmtConcat; consumed by IInlineFmt

        -- Only in ATS
        -- use: PrimCase e d c1 e1 c2 e2 ... cn en
        -- e is the scrutinized expression, d is the default value, (ck, ek) forms a case arm
        PrimCase :: PrimOp p

        -- Only used in intermediate code
        -- primSelect @k @m @n e  selects k bits at position m from n bits
        -- primSelect :: \/ k, m, n :: * -> Bit n -> Bit k
        PrimSelect :: PrimOp p

        -- primitives without hardware representation
        PrimIntegerToBit :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerToUIntBits :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerToIntBits :: PrimOp ('Ph 'WithBinders e)
        PrimBitToInteger :: PrimOp ('Ph 'WithBinders e)  -- XXX dangerous
        PrimIntegerToString :: PrimOp ('Ph 'WithBinders e)

        -- must be called on compile-time values
        PrimIntBitsToInteger :: PrimOp ('Ph 'WithBinders e)
        PrimUIntBitsToInteger :: PrimOp ('Ph 'WithBinders e)

        PrimIsStaticInteger :: PrimOp ('Ph 'WithBinders e)
        PrimAreStaticBits :: PrimOp ('Ph 'WithBinders e)

        PrimValueOf :: PrimOp ('Ph 'WithBinders e)
        PrimStringOf :: PrimOp ('Ph 'WithBinders e)

        PrimWhen :: PrimOp ('Ph 'WithBinders e)
        PrimWhenPred :: PrimOp ('Ph 'WithBinders e)  -- takes abstract predicate

        PrimOrd :: PrimOp p  -- universal: LambdaCalcUtil rebuilds them after
        PrimChr :: PrimOp p  -- AConv (see the note on the declaration)

        -- primRange lo hi x, promises lo <= x <= hi
        PrimRange :: PrimOp p

        PrimError :: PrimOp ('Ph 'WithBinders e)
        PrimGenerateError :: PrimOp ('Ph 'WithBinders e)
        PrimMessage :: PrimOp ('Ph 'WithBinders e)
        PrimWarning :: PrimOp ('Ph 'WithBinders e)
        PrimPoisonedDef :: PrimOp ('Ph 'WithBinders e)

        PrimDynamicError :: PrimOp ('Ph 'WithBinders e)

        PrimStringConcat :: PrimOp p
        PrimStringToInteger :: PrimOp ('Ph 'WithBinders e)
        PrimStringEQ :: PrimOp ('Ph 'WithBinders e)
        PrimStringLT :: PrimOp ('Ph 'WithBinders e)
        PrimStringLE :: PrimOp ('Ph 'WithBinders e)
        PrimStringLength :: PrimOp ('Ph 'WithBinders e)

        PrimStringSplit :: PrimOp ('Ph 'WithBinders e)
        PrimStringCons :: PrimOp ('Ph 'WithBinders e)

        PrimCharToString :: PrimOp ('Ph 'WithBinders e)
        PrimStringToChar :: PrimOp ('Ph 'WithBinders e)
        PrimCharOrd :: PrimOp ('Ph 'WithBinders e)
        PrimCharChr :: PrimOp ('Ph 'WithBinders e)

        PrimJoinActions :: PrimOp p
        PrimNoActions :: PrimOp p
        PrimExpIf :: PrimOp p  -- "split shallow"
        PrimNoExpIf :: PrimOp p  -- "nosplit shallow"
        PrimSplitDeep :: PrimOp p
        PrimNosplitDeep :: PrimOp p
        PrimAddRules :: PrimOp ('Ph 'WithBinders e)
        PrimModuleBind :: PrimOp ('Ph 'WithBinders e)
        PrimModuleReturn :: PrimOp ('Ph 'WithBinders e)
        PrimModuleFix :: PrimOp ('Ph 'WithBinders e)
        PrimModuleClock :: PrimOp ('Ph 'WithBinders e)
        PrimModuleReset :: PrimOp ('Ph 'WithBinders e)
        PrimBuildModule :: PrimOp ('Ph 'WithBinders e)
        PrimCurrentClock :: PrimOp ('Ph 'WithBinders e)
        PrimCurrentReset :: PrimOp ('Ph 'WithBinders e)
        PrimSameFamilyClock :: PrimOp ('Ph 'WithBinders e)
        PrimIsAncestorClock :: PrimOp ('Ph 'WithBinders e)
        PrimChkClockDomain :: PrimOp ('Ph 'WithBinders e)
        PrimClockEQ :: PrimOp ('Ph 'WithBinders e)
        PrimClockOf :: PrimOp ('Ph 'WithBinders e)
        PrimClocksOf :: PrimOp ('Ph 'WithBinders e)
        PrimNoClock :: PrimOp ('Ph 'WithBinders e)
        PrimResetEQ :: PrimOp ('Ph 'WithBinders e)
        PrimResetOf :: PrimOp ('Ph 'WithBinders e)
        PrimResetsOf :: PrimOp ('Ph 'WithBinders e)
        PrimNoReset :: PrimOp ('Ph 'WithBinders e)
        PrimResetUnassertedVal :: PrimOp p
        PrimJoinRules :: PrimOp ('Ph 'WithBinders e)
        PrimJoinRulesPreempt :: PrimOp ('Ph 'WithBinders e)
        PrimJoinRulesUrgency :: PrimOp ('Ph 'WithBinders e)
        PrimJoinRulesExecutionOrder :: PrimOp ('Ph 'WithBinders e)
        PrimJoinRulesMutuallyExclusive :: PrimOp ('Ph 'WithBinders e)
        PrimJoinRulesConflictFree :: PrimOp ('Ph 'WithBinders e)
        PrimNoRules :: PrimOp ('Ph 'WithBinders e)
        PrimRule :: PrimOp ('Ph 'WithBinders e)
        -- PrimAddSchedPragmas :: [SchedulePragma] -> Rules -> Rules
        PrimAddSchedPragmas :: PrimOp ('Ph 'WithBinders e)

        PrimGetName :: PrimOp ('Ph 'WithBinders e)
        -- primStateName :: Name -> Module b -> Module b
        -- This primitive is used to name state components.
        -- The first argument is an abstract name that is added to
        -- the names of state elements instantiated by the second argument.
        PrimStateName :: PrimOp ('Ph 'WithBinders e)
        PrimGetModuleName :: PrimOp ('Ph 'WithBinders e)

        PrimJoinNames :: PrimOp ('Ph 'WithBinders e)
        PrimExtendNameInteger :: PrimOp ('Ph 'WithBinders e)
        PrimGetNamePosition :: PrimOp ('Ph 'WithBinders e)
        PrimGetNameString :: PrimOp ('Ph 'WithBinders e)
        PrimMakeName :: PrimOp ('Ph 'WithBinders e)

        -- primStateAttrib :: Attributes -> Module b -> Module b
        -- This primitive is used to add attributes to submod instantiations.
        -- The first argument is an abstract list of attributes.
        PrimStateAttrib :: PrimOp ('Ph 'WithBinders e)

        PrimNoPosition :: PrimOp ('Ph 'WithBinders e)
        PrimPrintPosition :: PrimOp ('Ph 'WithBinders e)
        PrimGetStringPosition :: PrimOp ('Ph 'WithBinders e)
        PrimSetStringPosition :: PrimOp ('Ph 'WithBinders e)
        PrimGetEvalPosition :: PrimOp ('Ph 'WithBinders e)

        -- environment
        PrimGenC :: PrimOp ('Ph 'WithBinders e)
        PrimGenVerilog :: PrimOp ('Ph 'WithBinders e)
        PrimGenModuleName :: PrimOp ('Ph 'WithBinders e)

        -- elaboration-time file IO
        PrimOpenFile :: PrimOp ('Ph 'WithBinders e)
        PrimCloseHandle :: PrimOp ('Ph 'WithBinders e)
        PrimHandleIsEOF :: PrimOp ('Ph 'WithBinders e)
        PrimHandleIsOpen :: PrimOp ('Ph 'WithBinders e)
        PrimHandleIsClosed :: PrimOp ('Ph 'WithBinders e)
        PrimHandleIsReadable :: PrimOp ('Ph 'WithBinders e)
        PrimHandleIsWritable :: PrimOp ('Ph 'WithBinders e)
        PrimSetHandleBuffering :: PrimOp ('Ph 'WithBinders e)
        PrimGetHandleBuffering :: PrimOp ('Ph 'WithBinders e)
        PrimFlushHandle :: PrimOp ('Ph 'WithBinders e)
        PrimWriteHandle :: PrimOp ('Ph 'WithBinders e)
        PrimReadHandleLine :: PrimOp ('Ph 'WithBinders e)
        PrimReadHandleChar :: PrimOp ('Ph 'WithBinders e)

        -- reflective type primitives
        PrimTypeOf :: PrimOp ('Ph 'WithBinders e)
        PrimPrintType :: PrimOp ('Ph 'WithBinders e)
        PrimTypeEQ :: PrimOp ('Ph 'WithBinders e)
        PrimIsIfcType :: PrimOp ('Ph 'WithBinders e)

        -- type-tracking primitive
        PrimSavePortType :: PrimOp ('Ph 'WithBinders e)

        -- compile time numbers
        PrimIntegerAdd :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerSub :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerNeg :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerMul :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerDiv :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerMod :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerExp :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerLog2 :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerLog10 :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerQuot :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerRem :: PrimOp ('Ph 'WithBinders e)

        PrimIntegerEQ :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerLE :: PrimOp ('Ph 'WithBinders e)
        PrimIntegerLT :: PrimOp ('Ph 'WithBinders e)

        -- Real numbers: Show
        PrimRealToString :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Literal
        PrimIntegerToReal :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Eq and Ord
        PrimRealEQ :: PrimOp ('Ph 'WithBinders e)
        PrimRealLE :: PrimOp ('Ph 'WithBinders e)
        PrimRealLT :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Arith
        PrimRealAdd :: PrimOp ('Ph 'WithBinders e)
        PrimRealSub :: PrimOp ('Ph 'WithBinders e)
        PrimRealNeg :: PrimOp ('Ph 'WithBinders e)
        PrimRealMul :: PrimOp ('Ph 'WithBinders e)
        PrimRealDiv :: PrimOp ('Ph 'WithBinders e)
        PrimRealAbs :: PrimOp ('Ph 'WithBinders e)
        PrimRealSignum :: PrimOp ('Ph 'WithBinders e)
        PrimRealExpE :: PrimOp ('Ph 'WithBinders e)
        PrimRealPow :: PrimOp ('Ph 'WithBinders e)
        PrimRealLogE :: PrimOp ('Ph 'WithBinders e)
        PrimRealLogBase :: PrimOp ('Ph 'WithBinders e)
        PrimRealLog2 :: PrimOp ('Ph 'WithBinders e)
        PrimRealLog10 :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Bits
        PrimRealToBits :: PrimOp ('Ph 'WithBinders e)
        PrimBitsToReal :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Trig
        PrimRealSin :: PrimOp ('Ph 'WithBinders e)
        PrimRealCos :: PrimOp ('Ph 'WithBinders e)
        PrimRealTan :: PrimOp ('Ph 'WithBinders e)
        PrimRealSinH :: PrimOp ('Ph 'WithBinders e)
        PrimRealCosH :: PrimOp ('Ph 'WithBinders e)
        PrimRealTanH :: PrimOp ('Ph 'WithBinders e)
        PrimRealASin :: PrimOp ('Ph 'WithBinders e)
        PrimRealACos :: PrimOp ('Ph 'WithBinders e)
        PrimRealATan :: PrimOp ('Ph 'WithBinders e)
        PrimRealASinH :: PrimOp ('Ph 'WithBinders e)
        PrimRealACosH :: PrimOp ('Ph 'WithBinders e)
        PrimRealATanH :: PrimOp ('Ph 'WithBinders e)
        PrimRealATan2 :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Sqrt
        PrimRealSqrt :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Rounding
        PrimRealTrunc :: PrimOp ('Ph 'WithBinders e)
        PrimRealCeil :: PrimOp ('Ph 'WithBinders e)
        PrimRealFloor :: PrimOp ('Ph 'WithBinders e)
        PrimRealRound :: PrimOp ('Ph 'WithBinders e)
        -- Real numbers: Introspection
        PrimSplitReal :: PrimOp ('Ph 'WithBinders e)
        PrimDecodeReal :: PrimOp ('Ph 'WithBinders e)
        PrimRealToDigits :: PrimOp ('Ph 'WithBinders e)
        PrimRealIsInfinite :: PrimOp ('Ph 'WithBinders e)
        PrimRealIsNegativeZero :: PrimOp ('Ph 'WithBinders e)

        PrimSeq :: PrimOp ('Ph 'WithBinders e)  -- args are eval in sequence
                        -- for side effects or strictness
        PrimSeqCond :: PrimOp ('Ph 'WithBinders e)  -- implicit-condition strictness
        PrimUninitialized :: PrimOp ('Ph 'WithBinders e)
        PrimRawUninitialized :: PrimOp ('Ph 'WithBinders e)  -- error out with a use of an uninitialized value
        PrimMarkArrayUninitialized :: PrimOp ('Ph 'WithBinders e)  -- mark array as uninitialized
        PrimMarkArrayInitialized :: PrimOp ('Ph 'WithBinders e)  -- mark array as initialized
        PrimUninitBitArray :: PrimOp ('Ph 'WithBinders e)  -- make an array of uninitialized bits
        PrimIsBitArray :: PrimOp ('Ph 'WithBinders e)  -- is this Bit n represented as an array
        PrimUpdateBitArray :: PrimOp ('Ph 'WithBinders e)
        PrimBuildUndefined :: PrimOp ('Ph 'WithBinders e)  -- build a type-appropriate undefined value
        PrimRawUndefined :: PrimOp ('Ph 'WithBinders e)  -- create a "raw" undefined value
        PrimIsRawUndefined :: PrimOp ('Ph 'WithBinders e)  -- test if a value is a "raw" undefined value
        PrimImpCondOf :: PrimOp ('Ph 'WithBinders e)  -- XXX experimental
        PrimArrayNew :: PrimOp ('Ph 'WithBinders e)  -- Primitive array operators
        PrimArrayLength :: PrimOp ('Ph 'WithBinders e)
        PrimArraySelect :: PrimOp ('Ph 'WithBinders e)
        PrimArrayUpdate :: PrimOp ('Ph 'WithBinders e)
        PrimArrayDynSelect :: PrimOp p
        PrimArrayDynUpdate :: PrimOp ('Ph 'WithBinders e)
        PrimBuildArray :: PrimOp p  -- only exists after IExpand and in ASyntax

        PrimSetSelPosition :: PrimOp ('Ph 'WithBinders e)

        PrimGetParamName :: PrimOp ('Ph 'WithBinders e)  -- get the parameter name associated with the function value

        PrimEQ3 :: PrimOp p  -- === / Verilog case equality

        -- implicit Bits pack/unpack coercions; the Prelude wrappers
        -- (Prelude.pack/Prelude.unpack) apply these to the Bits dictionary,
        -- and the evaluator unfolds them to the corresponding class method.
        -- They never survive past IExpand.
        PrimPack :: PrimOp ('Ph 'WithBinders e)
        PrimUnpack :: PrimOp ('Ph 'WithBinders e)


-- The code tables, generated by Template Haskell (the first use of it
-- in this repository) from the declaration above:
--
--   primOpCode     :: PrimOp p -> Int       the code a file writes: the
--                     constructor's position in the declaration, which
--                     is its tag (dataToTag#)
--   primOpFromCode :: Int -> PrimOp PreElab the primitive a .bo reads
--   allPrimOps     :: [PrimOp PreElab]      every primitive, code order
--   primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
--                     the same primitive at another phase, when it
--                     exists at every phase (Nothing for one declared
--                     at the binder phases only; the evaluator's rebuild
--                     and the .ba reader refuse those)
--   primOpTableHash :: String
--                     a hash of the whole table (FNV-1a 64, 16 hex
--                     digits), computed by the splice; the .bo and .ba
--                     format tags end in it (GenBin.header,
--                     GenABin.header), so any change to the table makes
--                     every file written before it unreadable, with the
--                     usual "Binary version mismatch", and no one has to
--                     remember to bump the tags
--
-- The declaration is the encoding: a primitive's code is its position
-- above, so the table cannot drift from the declaration.  The price is
-- that adding, removing or moving a primitive renumbers the ones after
-- it; the hash in the format tags changes with the table either way,
-- so old files are refused, not misread, and the testsuite pins the
-- listing (dumpbo -prim-codes, bsc.binary/primcodes) so the change is
-- a visible one.  The hash covers this table only; a change elsewhere
-- in the formats (an IConInfo or AExpr tag in BinData, a field) still
-- needs the manual bump.
--
-- Why generated: the inverse of a 222-entry table would otherwise be
-- hand-written and checked by nothing.  If a build cannot run a splice
-- (a cross-compiler, or a profiling build without the non-profiled
-- objects or -fexternal-interpreter), the fallback is to paste the
-- generated declarations (ghc -ddump-splices) into a PrimCodes.hs and
-- import it here in place of the splice.
$(primOpTables ''PrimOp ''PreElab)

-- Eq, Ord and Show are derived.  Every constructor is nullary, so GHC
-- compiles == and compare to a comparison of the constructor tags
-- (dataToTag#: the position in the declaration, the same at every
-- phase, so equal tags are the same primitive), as it did when the type
-- was a plain enumeration; the first form of this type wrote the two
-- instances through primOpCode, then a case over every constructor,
-- and a primitive comparison sits in every `op == PrimX` test and
-- `op elem [...]` of the evaluator and the passes and in the Ord of
-- AExpr behind AConv's CSE map and the scheduler's use analysis (the
-- Ord of IExpr compares an ICPrim by the constant's Id, ISyntax.cmpC,
-- never the primitive), so that cost 1-3% of elaboration time.  The
-- order is the declaration order, which is the code order and the old
-- enumeration's order: the Map and Set iteration orders that key on
-- an expression holding a primitive follow it, so moving a constructor
-- above can move a definition in the generated output, and the order
-- has a cost of its own (the note on the declaration).
deriving instance Eq (PrimOp p)
deriving instance Ord (PrimOp p)
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
