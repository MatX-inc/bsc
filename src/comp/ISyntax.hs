{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, TypeFamilies #-}
{-# LANGUAGE PatternSynonyms, StandaloneDeriving #-}
{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module ISyntax(
        Binders(..),
        Evald(..),
        Phase(..),
        PreElab,
        Elab,
        PostElab,
        BinderPhase,
        EvaldPhase,
        KnownPhase(..),
        Ref,
        IPackage(..),
        IDef(..),
        IKind(..),
        IType(ITVar, ITCon, ITNum, ITStr, ITAp, ITForAll),
        IExpr(ILam, IAps, IVar, ILAM, ICon, IRefT),
        cmpE,
        cmpC,
        ExprKey(..),
        PTermKey(..),
        cmpPred,
        ConTagInfo(..),
        IConInfo(..),
        IRules(..),
        IRule(..),
        Body,
        IAction(..),
        SplitMode(..),
        AVSel(..),
        itAction,
        IEFace(..),
        IMethodInput,
        IModule(..),
        IAbstractInput(..),
        IStateVar(..),
        PortTypeMap,
        IClock(..),
        IReset(..),
        IInout(..),
        ILazyArray,
        ArrayCell(..),
        Pred(..),
        PTerm(..),
        getClockMap,
        getResetMap,
        getVModInfo,
        iRUnion,
        iRUnionPreempt,
        iRUnionUrgency,
        iRUnionExecutionOrder,
        iRUnionMutuallyExclusive,
        iRUnionConflictFree,
        iREmpty,
        uniquifyRules,
        fdVars,
        splitITAp,
        aTVars,
        fTVars,
        VarSet, fTVarSet,
        vsEmpty, vsSingleton, vsUnion, vsInsert, vsDelete, vsMember, vsNull,
        ftvCacheEnabled,
        itArrow,
        iToCT,
        iToCK,
        iAp,
        iAP,
        fVars,
        ftVars,
        aVars,
        mkNumConT,
        showTypeless,
        showTypelessRules,
        getIExprPosition,
        getIExprPositionCross,
        getIRuleId,
        getIRuleStateLoc,
        sameClockDomain,
        inClockDomain,
        getClockDomain,
        isNoClock,
        isMissingDefaultClock,
        makeClock,
        getClockWires,
        setClockWires,
        makeReset,
        getResetWire,
        getResetClock,
        getResetId,
        isNoReset,
        isMissingDefaultReset,
        makeInout,
        getInoutWire,
        getInoutClock,
        getInoutReset,
        getWireInfo,
        isIConInt, isIConReal, isIConParam,
        IATFCache, mergeIATFCaches
        ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import System.IO(Handle)
import qualified Data.Map as M
import Data.List(intercalate)
import Data.Kind(Type)
import Data.Void(Void)

import qualified Data.Array as Array
import IntLit
import Undefined
import Eval
import Id
import Wires(ResetId, ClockDomain, ClockId, noClockId, noResetId, noDefaultClockId, noDefaultResetId, WireProps)
import IdPrint
import PreIds(idBind, idReturn, idPack, idUnpack, idMonad, idLiftModule, idBit, idFromInteger,
              idPrimAction, idPrimIf, idPrimNoActions,
              idPrimExpIf, idPrimNoExpIf, idPrimSplitDeep, idPrimNosplitDeep)
import Backend
import Prim(PrimOp(..))
import ConTagInfo
import CType(TISort(..))
import VModInfo(VModInfo, vArgs, vName, VName(..), {- VeriPortProp(..), -}
                VArgInfo(..), VFieldInfo(..), isParam, VWireInfo)
import DefProp(DefProp)
import Pragma(Pragma, PProp, RulePragma, ISchedulePragma,
              CSchedulePragma, SchedulePragma(..),
              extractSchedPragmaIds, removeSchedPragmaIds, mapSPIds)
import Position
import Data.Maybe

import qualified Data.Set as S
import Flags
import Error(internalError, EMsg, ErrMsg(..))
import PFPrint
import IStateLoc(IStateLoc)
import IType

-- ============================================================
-- IPackage, IModule

-- A package of top-level definitions and pragmas
-- * This corresponds to a .bo file.
-- * During iExpand, top-level defs for modules are synthesized
--   and those defs are replaceds with new defs that are merely
--   import-BVI of the generated module.
--
data IPackage a
        = IPackage {
              -- package name
              ipkg_name :: Id,
              -- linked packages (name, signature)
              ipkg_depends :: [(Id, String)],
              -- pragmas
              ipkg_pragmas :: [Pragma],
              -- definition list
              ipkg_defs :: [IDef a],
              -- cache of resolved associated type function applications
              ipkg_atf_cache :: IATFCache
          }
     deriving (Eq, Show)

type IATFCache = M.Map (Id, [IType]) IType

mergeIATFCaches :: IATFCache -> IATFCache -> IATFCache
mergeIATFCaches = M.union

-- An elaborated module
-- * These are created during iExpand for each module to be synthesized
--   from the IPackage.
data IModule a
        = IModule {
                imod_name :: Id,                      -- module name
                imod_is_wrapped :: Bool,              -- function wrapper?
                imod_backend_specifc :: Maybe Backend,
                imod_external_wires :: VWireInfo,     -- boundary wire information (clock, reset, arguments, etc.)
                imod_pragmas :: [Pragma],             -- all top level pragmas
                -- XXX The list of type args is always empty (unused).
                -- If we supported generation of modules with numeric type
                -- variables, they would be in this list.
                imod_type_args :: [(Id, IKind)],      -- package type arguments
                imod_wire_args :: [IAbstractInput],   -- package (wire) arguments
                imod_clock_domains :: [(ClockDomain, [IClock a])], -- clocks (internal and external)
                imod_resets :: [IReset a],              -- resets (internal and external)
                imod_state_insts :: [(Id, IStateVar a)], -- state elements
                imod_port_types :: PortTypeMap, -- map from state variable -> port -> source type
                imod_local_defs :: [IDef a],            -- local definitions
                imod_rules :: IRules a,                 -- rules
                imod_interface :: [IEFace a],           -- package interface
                imod_ffcallNo :: Int,                    -- next available unique ffcalNo
                -- comments on submodule instantiations
                imod_instance_comments :: [(Id, [String])]
          }

-- The instances that look inside a rule body (through IRule, IRules
-- and IEFace) are per phase: the body's type is the Body family, and
-- a `Show (Body a)` context on a polymorphic instance would be a
-- dictionary passed at every use.  An IModule exists only after
-- elaboration.
deriving instance Show (IModule PostElab)

getWireInfo :: IModule a -> VWireInfo
getWireInfo = imod_external_wires

-- Map from submod instance name to a map from port to its source type.
-- Toplevel ports of the current module are represented in the same map
-- using the Nothing value in place of an instance name.
type PortTypeMap = M.Map (Maybe Id) (M.Map VName IType)

data IDef a = IDef Id IType (IExpr a) [DefProp]
        deriving (Eq, Show)

data IAbstractInput =
        -- simple input using one port
        IAI_Port (Id, IType) |
        -- clock osc and maybe gate
        IAI_Clock Id (Maybe Id) |
        IAI_Reset Id |
        IAI_Inout Id Integer
        -- room to add other types here, like:
        --   IAI_Struct [(Id, IType)]
    deriving (Eq, Show)

-- One method argument, decomposed into the ports it occupies (one port for an
-- unsplit argument, several for a split struct/tuple).  A method's arguments
-- are a list of these groups.
type IMethodInput = [(Id, IType)]

data IEFace a = IEFace {
        -- This is either an actual method or a ready signal for another
        -- method.  Use 'isRdyId' to determine which.  Use 'mkRdyId' on
        -- the name of an actual method to construct the name of its
        -- associated ready method.
        ief_name :: Id,
        -- arguments, split into ports.
        ief_args :: [IMethodInput],
        -- Prior to 'iSplitIface', 'ief_value' contains the expression for
        -- the whole method and 'ief_body' is empty.  After 'iSplitIface',
        -- 'ief_value' contains the return value (if any) and 'ief_body'
        -- contains the rules (the Actions) (if any).
        -- XXX Should we use a different type for these two forms?
        ief_value :: (Maybe (IExpr a, IType)),
        ief_body :: (Maybe (IRules a)),
        ief_wireprops :: WireProps,
        ief_fieldinfo :: VFieldInfo
     }

deriving instance Show (IEFace Elab)
deriving instance Show (IEFace PostElab)


-- ---------------
-- IStateVar

-- a state variable (foreign module instantiation)
data IStateVar a = IStateVar {
    isv_is_arg :: Bool,           -- real state variable (or argument)
    isv_is_user_import :: Bool,   -- whether it is a foreign module
    isv_uid :: Int,               -- unique number
    isv_vmi :: VModInfo,          -- foreign module info
    isv_iargs :: [IExpr a],       -- params + arguments
    isv_type :: IType,            -- type of the svar (like "Prelude.VReg")
    -- The next list corresponds to vFields in the VModInfo, but cannot be
    -- stored there, because VModInfo is created before types are known:
    isv_meth_types :: [[IType]],  -- method types
    isv_clocks :: [(Id, IClock a)], -- named clocks
    isv_resets :: [(Id, IReset a)], -- named resets
    isv_isloc :: IStateLoc        -- instantiation path
}
    deriving (Show)

getResetMap :: IStateVar a -> [(Id, IReset a)]
getResetMap = isv_resets

getClockMap :: IStateVar a -> [(Id, IClock a)]
getClockMap = isv_clocks

getVModInfo :: IStateVar a -> VModInfo
getVModInfo = isv_vmi

instance Eq (IStateVar a) where
    a == b =  isv_uid a == isv_uid b

instance Ord (IStateVar a) where
    a `compare` b =  isv_uid a `compare` isv_uid b

-- ==============================
-- IAction

-- The body of a rule or of a method after elaboration.  The evaluator
-- reduces a body to a tree of joins and conditionals over method calls
-- and foreign calls (pDef never makes a definition of an action-typed
-- cell, so a body is materialised down to those leaves, whose
-- arguments are ordinary expressions).  pDef's rebuild turns that tree
-- into an IAction, and from then on no IExpr contains an action.
--
-- The leaf constants (a method selector, a state variable, a foreign
-- function, the avAction_ selector, an undetermined action) are kept
-- as the ICon nodes the evaluator's application carried, so that
-- equality (cmpE: the Id and its inlined positions, the selector's
-- type, the state variable's uid, the foreign call's cookie), printing
-- and the inlined-position rewrite are the expression's by
-- construction.  Which variant a field holds is established by the
-- converter (IExpand.toIAction) and stated at each constructor.

-- The split annotation on a conditional action: none (the -split-if
-- flag decides), primExpIf, primNoExpIf.
data SplitMode = SplitDefault | SplitIf | NoSplitIf
    deriving (Eq, Show)

-- The avAction_ selector that takes the action half of an ActionValue
-- call: the selector constant (ICon idAVAction_ t (ICSel 1 2)) and the
-- type arguments it is applied to.
data AVSel = AVSel (IExpr PostElab) [IType]
    deriving (Eq, Show)

data IAction
    = -- PrimNoActions
      ANoActions
      -- PrimJoinActions a1 a2 (binary, as the evaluator builds it:
      -- the nesting takes part in equality, as it did in the expression)
    | AJoin IAction IAction
      -- [primExpIf | primNoExpIf] (PrimIf ·Action c t e)
    | AIf SplitMode (IExpr PostElab) IAction IAction
      -- primSplitDeep (True) / primNosplitDeep (False) around an
      -- action: the region whose bare conditionals are split / are not
      -- split (ISplitIf consumes it)
    | ADeep Bool IAction
      -- [primExpIf | primNoExpIf]
      --   (PrimArrayDynSelect ·Action ·sz (PrimBuildArray ·Action es) idx):
      -- the mode, the Ids of the PrimArrayDynSelect and PrimBuildArray
      -- constants (the positions of the index literals the passes
      -- build), the index width, the elements, the index
    | AArrSel SplitMode Id Id Integer [IAction] (IExpr PostElab)
      -- [avAction_ ·t] (.m ·ts inst args): the selector constant
      -- (ICon m mt (ICSel n k)), its type arguments, the instance
      -- constant (ICon i it (ICStateVar sv)), the arguments
    | ACallMethod (Maybe AVSel) (IExpr PostElab) [IType] (IExpr PostElab) [IExpr PostElab]
      -- [avAction_ ·t] (f ·ts es), or the bare constant f (Nothing):
      -- the foreign constant (ICon f ft (ICForeign ..)), and the type
      -- arguments and arguments when it is applied
    | ACallForeign (Maybe AVSel) (IExpr PostElab) (Maybe ([IType], [IExpr PostElab]))
      -- the constant ICon i Action (ICUndet k): an action selected
      -- from an array by a static index that is out of range
    | AUndet (IExpr PostElab)
    deriving (Show)

-- Equality as cmpE gave on the expression each node stands for: the
-- mode (the marker constants' Ids), the stored constants through
-- cmpE, the argument lists pairwise and by length.  The derived
-- instance would differ on AArrSel's two Ids, whose inlined positions
-- cmpE compares and Id's Eq does not.
instance Eq IAction where
    ANoActions == ANoActions = True
    AJoin a1 a2 == AJoin b1 b2 = a1 == b1 && a2 == b2
    AIf m1 c1 t1 e1 == AIf m2 c2 t2 e2 =
        m1 == m2 && c1 == c2 && t1 == t2 && e1 == e2
    ADeep b1 a1 == ADeep b2 a2 = b1 == b2 && a1 == a2
    AArrSel m1 s1 r1 n1 es1 i1 == AArrSel m2 s2 r2 n2 es2 i2 =
        m1 == m2 && eqConId s1 s2 && eqConId r1 r2 && n1 == n2 &&
        es1 == es2 && i1 == i2
    ACallMethod v1 s1 ts1 i1 es1 == ACallMethod v2 s2 ts2 i2 es2 =
        v1 == v2 && s1 == s2 && ts1 == ts2 && i1 == i2 && es1 == es2
    ACallForeign v1 f1 as1 == ACallForeign v2 f2 as2 =
        v1 == v2 && f1 == f2 && as1 == as2
    AUndet c1 == AUndet c2 = c1 == c2
    _ == _ = False

-- two constants' Ids as cmpE compares them (the variant is ICPrim, so
-- the type does not take part): the name, then the inlined positions
eqConId :: Id -> Id -> Bool
eqConId i j = i == j && getIdInlinedPositions i == getIdInlinedPositions j

instance NFData SplitMode where
    rnf m = m `seq` ()

instance NFData AVSel where
    rnf (AVSel s ts) = rnf2 s ts

instance NFData IAction where
    rnf ANoActions = ()
    rnf (AJoin a1 a2) = rnf2 a1 a2
    rnf (AIf m c t e) = rnf4 m c t e
    rnf (ADeep b a) = rnf2 b a
    rnf (AArrSel m s r n es i) = rnf6 m s r n es i
    rnf (ACallMethod v s ts i es) = rnf5 v s ts i es
    rnf (ACallForeign v f as) = rnf3 v f as
    rnf (AUndet c) = rnf c

-- The type of an action (tiAction is TIabstract).  ISyntaxUtil
-- re-exports it.
itAction :: IType
itAction = ITCon idPrimAction IKStar TIabstract

-- An IAction prints as the expression it stands for: the three ppAps
-- arms over its parts.  The stored constants print through their own
-- instance; a primitive constant the node stands for (PrimIf, the
-- join, the markers, the array primitives) prints as the PrimOp's
-- name at PDReadable, as the expression did, and as its Id at the
-- other details (where the expression also printed its type).
instance PPrint IAction where
    pPrint = ppAction

ppAction :: PDetail -> Int -> IAction -> Doc
ppAction d p a =
    case a of
      ANoActions -> ppPrimHead d idPrimNoActions PrimNoActions
      AJoin _ _ ->
          text "{" <+> sepList (map (ppAction d 0) (joinArms a)) (text ";") <+> text "}"
      AIf mode c t e ->
          ppMarked d p mode $ \ p' ->
              ppApsA d p' (ppPrimHead d idPrimIf PrimIf) [itAction]
                  [pPrint d maxPrec c, ppAction d maxPrec t, ppAction d maxPrec e]
      ADeep b a' ->
          let (i, op) = if b then (idPrimSplitDeep, PrimSplitDeep)
                        else (idPrimNosplitDeep, PrimNosplitDeep)
          in  ppApsA d p (ppPrimHead d i op) [] [ppAction d maxPrec a']
      AArrSel mode i_sel i_arr sz es idx ->
          ppMarked d p mode $ \ p' ->
              ppApsA d p' (ppPrimHead d i_sel PrimArrayDynSelect)
                  [itAction, ITNum sz]
                  [ppApsA d maxPrec (ppPrimHead d i_arr PrimBuildArray) [itAction]
                       (map (ppAction d maxPrec) es),
                   pPrint d maxPrec idx]
      ACallMethod mav sel ts inst args ->
          ppAV d p mav $ \ p' ->
              ppApsA d p' (pPrint d (maxPrec-1) sel) ts
                  (map (pPrint d maxPrec) (inst : args))
      ACallForeign mav f (Just (ts, es)) ->
          ppAV d p mav $ \ p' ->
              ppApsA d p' (pPrint d (maxPrec-1) f) ts (map (pPrint d maxPrec) es)
      ACallForeign mav f Nothing ->
          ppAV d p mav $ \ p' -> pPrint d p' f
      AUndet c -> pPrint d p c
  where
    -- the actions of a join, nested joins flattened on both sides
    joinArms (AJoin a1 a2) = joinArms a1 ++ joinArms a2
    joinArms a' = [a']
    -- the split marker around a conditional, when there is one
    ppMarked d' p' SplitDefault k = k p'
    ppMarked d' p' SplitIf k =
        ppApsA d' p' (ppPrimHead d' idPrimExpIf PrimExpIf) [] [k maxPrec]
    ppMarked d' p' NoSplitIf k =
        ppApsA d' p' (ppPrimHead d' idPrimNoExpIf PrimNoExpIf) [] [k maxPrec]
    -- the avAction_ selector around a call, when there is one
    ppAV d' p' Nothing k = k p'
    ppAV d' p' (Just (AVSel s ts)) k =
        ppApsA d' p' (pPrint d' (maxPrec-1) s) ts [k maxPrec]

-- the generic ppAps arm, over the head's and the arguments' documents
-- (the arguments rendered at maxPrec by the caller)
ppApsA :: PDetail -> Int -> Doc -> [IType] -> [Doc] -> Doc
ppApsA d p f ts es = pparen (p>(maxPrec-1)) $
    sep (f : map (nest 2 . (text"\183" <>) . pPrint d maxPrec) ts ++ map (nest 2) es)

-- a primitive constant's text (the ICPrim arm of PPrint (IExpr a) at
-- PDReadable, the Id arm otherwise)
ppPrimHead :: PDetail -> Id -> PrimOp -> Doc
ppPrimHead d@PDReadable _ op = text (show op)
ppPrimHead d i _ = ppId d i

-- The body of a rule, per phase: an expression until pDef's rebuild,
-- an IAction after it.  Two instances on apart patterns (as Ref): a
-- partially known phase reduces, a phase variable is stuck, which is
-- what lets the name-only rule code stay phase-polymorphic.
type family Body (p :: Phase) :: Type
type instance Body ('Ph 'WithBinders e) = IExpr ('Ph 'WithBinders e)
type instance Body PostElab = IAction

-- ==============================
-- IRule

-- last Id is original rule if rule has been split, Nothing otherwise
-- (argument descriptions are guesses based on ARule)
data IRule a =
    IRule {
      -- rule name
      irule_name :: Id,
      -- rule pragmas, e.g., no-implicit-conditions
      irule_pragmas :: [RulePragma],
      -- String that describes the rule
      irule_description :: String,
      -- Rule wire properties
      irule_wire_properties :: WireProps,
      -- Rule predicate
      irule_pred :: (IExpr a),
      -- Rule body: an expression until pDef's rebuild, an IAction after
      irule_body :: Body a,
      {- orig rule - for splitting -}
      irule_original :: (Maybe Id),
      -- Instantiation hierarchy
      irule_state_loc :: IStateLoc
      }

deriving instance Show (IRule Elab)
deriving instance Show (IRule PostElab)

instance NFData (IRule Elab) where
    rnf (IRule i ps s wp r1 r2 orig isl) = rnf8 i ps s wp r1 r2 orig isl

instance NFData (IRule PostElab) where
    rnf (IRule i ps s wp r1 r2 orig isl) = rnf8 i ps s wp r1 r2 orig isl

getIRuleId :: IRule a -> Id
getIRuleId = irule_name

getIRuleStateLoc :: IRule a -> IStateLoc
getIRuleStateLoc = irule_state_loc

data IRules a = IRules [ISchedulePragma] [IRule a]

deriving instance Show (IRules Elab)
deriving instance Show (IRules PostElab)

instance NFData (IRules Elab) where
    rnf (IRules sps rs) = rnf2 sps rs

instance NFData (IRules PostElab) where
    rnf (IRules sps rs) = rnf2 sps rs


-- renames the rules according to the Id,Id list
renameIRules :: [(Id,Id)] -> IRules a -> IRules a
renameIRules [] rls = rls
renameIRules idmap rls@(IRules schedPara rules) = IRules newSchedPara newRules
    where
    newRules = map (renameIRule idmap) rules
    newSchedPara = mapSPIds (renameFromMap idmap) schedPara --  map (renameSchedPara idmap) schedPara

renameIRule ::  [(Id,Id)] -> IRule a -> IRule a
renameIRule idmap orig = newRule
    where
    newId = lookup (irule_name orig) idmap
    newRule = if ( isNothing newId )
              then orig
              else orig {irule_name = (fromJust newId)}

renameFromMap ::  (Eq a) =>  [(a,a)] -> a -> a
renameFromMap idmap id = fromMaybe id newId
    where
    newId = lookup id idmap

-- Return a new second set of rules, with names changed to not clash
-- with a first set of rules
uniquifyRules :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a)
uniquifyRules flags suf r1@(IRules _ rs1) r2@(IRules sps2 rs2) =
    if (ruleNameCheck flags)
    then let rids1 = map getIRuleId rs1
             rids2 = map getIRuleId rs2
             -- rename the rules in r2 if needed
             (_, idmap) = genUniqueIdsAndMap rids1 rids2
         in  (suf, renameIRules idmap r2)
    else let fn r (n, m, rs) = let oldname = irule_name r
                                   (basename, _) = stripId_Suffix oldname
                                   newname = addId_Suffix basename n
                                   r' = r { irule_name = newname }
                                   m' = ((oldname,newname):m)
                               in  (n+1, m', r':rs)
             (suf', idmap, rs2') = foldr fn (suf, [], []) rs2
             sps2' = mapSPIds (renameFromMap idmap) sps2
         in  (suf', IRules sps2' rs2')

iRUnion :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnion flags suf r1@(IRules _ rs1) r2 =
    let (suf', r2_unique@(IRules _ rs2)) = uniquifyRules flags suf r1 r2
        (errs, sps) = checkRUnionAttributes r1 r2_unique
    in  (suf', IRules sps (rs1 ++ rs2), errs)

iRUnionPreempt :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionPreempt flags suf r1@(IRules _ rs1) r2 =
    let (suf', r2_unique@(IRules _ rs2)) = uniquifyRules flags suf r1 r2
        (errs, sps) = checkRUnionAttributes r1 r2_unique
        sps3 = [SPPreempt (map getIRuleId rs1) (map getIRuleId rs2)]
    in  (suf', IRules (sps3 ++ sps) (rs1 ++ rs2), errs)

iRUnionUrgency :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionUrgency flags suf r1@(IRules _ rs1) r2 =
    let (suf', r2_unique@(IRules _ rs2)) = uniquifyRules flags suf r1 r2
        (errs, sps) = checkRUnionAttributes r1 r2_unique
        sps3 = [ SPUrgency [rid1, rid2]
                   | rid1 <- map getIRuleId rs1,
                     rid2 <- map getIRuleId rs2 ]
    in  (suf', IRules (sps3 ++ sps) (rs1 ++ rs2), errs)

iRUnionExecutionOrder :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionExecutionOrder flags suf r1@(IRules _ rs1) r2 =
    let (suf', r2_unique@(IRules _ rs2)) = uniquifyRules flags suf r1 r2
        (errs, sps) = checkRUnionAttributes r1 r2_unique
        sps3 = [ SPExecutionOrder [rid1, rid2]
                   | rid1 <- map getIRuleId rs1,
                     rid2 <- map getIRuleId rs2 ]
    in  (suf', IRules (sps3 ++ sps) (rs1 ++ rs2), errs)

iRUnionPairwiseSchedPragma :: Flags -> Integer
                           -> ([[Id]] -> ISchedulePragma)
                           -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionPairwiseSchedPragma flags suf sched_pragma r1@(IRules _ rs1) r2 =
    let (suf', r2_unique@(IRules _ rs2)) = uniquifyRules flags suf r1 r2
        (errs, sps) = checkRUnionAttributes r1 r2_unique
        sps3 = [sched_pragma [ map getIRuleId rs1, map getIRuleId rs2 ]]
    in  (suf', IRules (sps3 ++ sps) (rs1 ++ rs2), errs)

iRUnionMutuallyExclusive :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionMutuallyExclusive flags suf r1 r2 =
    iRUnionPairwiseSchedPragma flags suf SPMutuallyExclusive r1 r2

iRUnionConflictFree :: Flags -> Integer -> IRules a -> IRules a -> (Integer, IRules a, [EMsg])
iRUnionConflictFree flags suf r1 r2 =
    -- trace "iRUnionConflictFree" $
    iRUnionPairwiseSchedPragma flags suf SPConflictFree r1 r2

iREmpty :: IRules a
iREmpty = IRules [] []

-- Check that all rule attribute are defined in the given (joined) rules
-- XXX Is that the behavior we want?
-- Return the pragmas with the bad names filtered out
checkRUnionAttributes :: IRules a -> IRules a -> ([EMsg], [ISchedulePragma])
checkRUnionAttributes (IRules sps1 rs1) (IRules sps2 rs2) =
    let
        definedIds = map getIRuleId rs1 ++ map getIRuleId rs2
        attrIds = extractSchedPragmaIds (sps1 ++ sps2)

        testMap  = M.fromList $ zip definedIds (repeat (0 :: Int ))
        checkMap = M.fromList $ zip attrIds (repeat (0 :: Int ))

        badIds :: [Id]
        badIds = map fst $ M.toList $ M.difference checkMap testMap

        mkErr i = (getIdPosition i, EUnknownRuleIdAttribute (pfpString i))
        msgs = map mkErr badIds
        sps' = if (null badIds)
               then sps1 ++ sps2
               else removeSchedPragmaIds badIds (sps1 ++ sps2)
    in
        (msgs, sps')


-- The type-function reduction that used to live here (normITAp) is
-- now performed by the ITAp smart constructor itself; see
-- IType.mkITAp.

aTVars :: IType -> S.Set Id
aTVars (ITForAll i _ t) = S.insert i (aTVars t)
aTVars (ITAp f a) = (aTVars f) `S.union` (aTVars a)
aTVars (ITVar i) = S.singleton i
aTVars (ITCon _ _ _) = S.empty
aTVars (ITNum _) = S.empty
aTVars (ITStr _) = S.empty

-- fTVars now lives in IType (answered from the free-variable sets
-- cached on interned nodes) and is re-exported here.


splitITAp :: IType -> (IType, [IType])
splitITAp t0 = go t0 []
  where go (ITAp f a) acc = go f (a : acc)
        go t          acc = (t, acc)


-- ==============================
-- Phases

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

-- The payload of a heap reference, per phase.  Only the evaluator has a
-- heap: `type instance Ref Elab = HeapData` lives in IExpandUtils beside
-- HeapData.  Before and after elaboration the payload type is Void, so
-- no IRefT or ArrayCell can be built there.
type family Ref (p :: Phase) :: Type
type instance Ref PreElab = Void
type instance Ref PostElab = Void

-- Every application and constant node carries an annotation, computed
-- from the node's parts when it is built through the IAps/ICon pattern
-- synonyms.  Ann p is () in every phase for now; the class is the hook
-- for a phase to cache something per node (free-variable sets, a
-- content key) without touching the sites that build nodes.
class KnownPhase (p :: Phase) where
    type Ann p :: Type
    annAps :: IExpr p -> [IType] -> [IExpr p] -> Ann p
    annCon :: Id -> IType -> IConInfo p -> Ann p

instance KnownPhase PreElab where
    type Ann PreElab = ()
    annAps _ _ _ = ()
    annCon _ _ _ = ()
    {-# INLINE annAps #-}
    {-# INLINE annCon #-}

instance KnownPhase Elab where
    type Ann Elab = ()
    annAps _ _ _ = ()
    annCon _ _ _ = ()
    {-# INLINE annAps #-}
    {-# INLINE annCon #-}

instance KnownPhase PostElab where
    type Ann PostElab = ()
    annAps _ _ _ = ()
    annCon _ _ _ = ()
    {-# INLINE annAps #-}
    {-# INLINE annCon #-}

-- ==============================
-- IExpr

data IExpr (p :: Phase) where
        -- vanishes after IExpand
        ILam  :: Id -> IType -> IExpr ('Ph 'WithBinders e)
              -> IExpr ('Ph 'WithBinders e)
        IAps_ :: Ann p -> IExpr p -> [IType] -> [IExpr p] -> IExpr p
        -- vanishes after IExpand
        IVar  :: Id -> IExpr ('Ph 'WithBinders e)
        -- vanishes after IExpand
        ILAM  :: Id -> IKind -> IExpr ('Ph 'WithBinders e)
              -> IExpr ('Ph 'WithBinders e)
        ICon_ :: Ann p -> Id -> IType -> IConInfo p -> IExpr p
        -- IRef is only used during reduction, it refers to a "heap" cell
        IRefT :: IType -> !Int -> S.Set Position -> Ref Elab -> IExpr Elab

-- IAps_ and ICon_ are matched directly only inside this module (where
-- the instances, the comparison and the traversals must not depend on
-- the phase); every other module builds and matches through these
-- synonyms, whose builders fill in the annotation.  The KnownPhase
-- context is required (a builder's class cannot be provided by a
-- match), so a polymorphic function that builds or matches through
-- IAps/ICon carries `KnownPhase p =>`; code at a fixed phase needs
-- nothing.
pattern IAps :: KnownPhase p => IExpr p -> [IType] -> [IExpr p] -> IExpr p
pattern IAps f ts es <- IAps_ _ f ts es
  where IAps f ts es = IAps_ (annAps f ts es) f ts es

pattern ICon :: KnownPhase p => Id -> IType -> IConInfo p -> IExpr p
pattern ICon i t ic <- ICon_ _ i t ic
  where ICon i t ic = ICon_ (annCon i t ic) i t ic

{-# COMPLETE ILam, IAps, IVar, ILAM, ICon, IRefT #-}

instance Show (IExpr a) where
  show (ILam i t e)   = "(ILam " ++ show i ++ " " ++ show t ++ " " ++ show e ++ ")"
  show (IAps_ _ f ts es) = "(IAps " ++ show f ++ " " ++ show ts ++ " " ++ show es ++ ")"
  show (IVar i)       = "(IVar " ++ show i ++ ")"
  show (ILAM i k e)   = "(ILAM " ++ show i ++ " " ++ show k ++ " " ++ show e ++ ")"
  show (ICon_ _ i _ (ICDef {})) = "(ICDef " ++ show i ++ ")"
  show (ICon_ _ i _ (ICValue {})) = "(ICValue " ++ show i ++ ")"
  show (ICon_ _ i t ic)  = "(ICon " ++ show i ++ " " ++ showsPrec 11 t "" ++ " " ++ show ic ++ ")"
  show (IRefT t p _ _)  = "(IRefT " ++ show t ++ " " ++ "_" ++ show p ++ ")"

-- The structural comparison of expressions, in every phase: Ids by
-- their intern order, heap references by pointer.  It is the Ord of
-- IExpr PostElab (the only phase with an Ord instance) and of the
-- ExprKey/PTermKey wrappers that the elaboration-time maps use.
cmpE :: IExpr a -> IExpr a -> Ordering
cmpE (ILam i1 _ e1)  (ILam i2 _ e2)  =
        case compare i1 i2 of
        EQ -> cmpE e1 e2
        o  -> o
cmpE (ILam _ _ _)    _               = LT

cmpE (IAps_ _ _ _ _)   (ILam _ _ _)    = GT
cmpE (IAps_ _ e1 ts1 es1) (IAps_ _ e2 ts2 es2) =
        case cmpE e1 e2 of
        EQ ->
                case cmpEs es1 es2 of
                EQ -> compare ts1 ts2
                o -> o
{-
                case compare ts1 ts2 of
                EQ -> compare es1 es2
                o  -> o
-}
        o  -> o
cmpE (IAps_ _ _ _ _)   _               = LT

cmpE (IVar _)        (ILam _ _ _)    = GT
cmpE (IVar _)        (IAps_ _ _ _ _) = GT
cmpE (IVar i1)       (IVar i2)       = compare i1 i2
cmpE (IVar _)        _               = LT

cmpE (ILAM _ _ _)    (ILam _ _ _)    = GT
cmpE (ILAM _ _ _)    (IAps_ _ _ _ _) = GT
cmpE (ILAM _ _ _)    (IVar _)        = GT
cmpE (ILAM i1 _ e1)  (ILAM i2 _ e2)  =
        case compare i1 i2 of
        EQ -> cmpE e1 e2
        o  -> o
cmpE (ILAM _ _ _)    (IRefT _ _ _ _)   = GT -- ???????

cmpE (ILAM _  _ _)   _               = LT

cmpE (ICon_ _ _ _ _) (ILam _ _ _)    = GT
cmpE (ICon_ _ _ _ _) (IAps_ _ _ _ _) = GT
cmpE (ICon_ _ _ _ _) (IVar _)        = GT
cmpE (ICon_ _ i1 t1 ic1) (ICon_ _ i2 t2 ic2) =
        case compare i1 i2 of
        EQ -> case (cmpC t1 ic1 t2 ic2) of
                -- inlined positions need to be considered in equality tests
                EQ -> let mposs1 = getIdInlinedPositions i1
                          mposs2 = getIdInlinedPositions i2
                      in  compare mposs1 mposs2
                o  -> o
        o  -> o
cmpE (ICon_ _ _ _ _) _               = LT

cmpE (IRefT _ _ _ _)   (ILam _ _ _)    = GT
cmpE (IRefT _ _ _ _)   (IAps_ _ _ _ _) = GT
cmpE (IRefT _ _ _ _)   (IVar _)        = GT
cmpE (IRefT _ _ _ _)   (ICon_ _ _ _ _) = GT
cmpE (IRefT _ p1 _ _)  (IRefT _ p2 _ _)  = compare p1 p2                -- XXX

cmpE (IRefT _ _ _ _)     (ILAM _ _ _)  = LT -- ??????????

{- all cases are covered above, so the compiler complains about this line:
cmpE e1              e2              = internalError ("not match in cmpE " ++ ppReadable (e1,e2))
-}

-- the lexicographic list order over cmpE (what `compare` on [IExpr a]
-- was when IExpr had an Ord instance in every phase)
cmpEs :: [IExpr a] -> [IExpr a] -> Ordering
cmpEs [] [] = EQ
cmpEs [] (_:_) = LT
cmpEs (_:_) [] = GT
cmpEs (x:xs) (y:ys) =
        case cmpE x y of
        EQ -> cmpEs xs ys
        o  -> o

instance Eq (IExpr a) where
    x == y  =  cmpE x y == EQ
    x /= y  =  cmpE x y /= EQ

-- The only Ord instance: the post-elaboration passes (ITransform's CSE,
-- AConv, ...) key maps and sets by expressions.  During elaboration the
-- maps are keyed by ExprKey/PTermKey instead, so that the comparison
-- used is explicit at each site.
instance Ord (IExpr PostElab) where
    compare x y = cmpE x y

-- An expression as a map/set key, ordered by cmpE, in any phase.
newtype ExprKey p = ExprKey { unExprKey :: IExpr p }

instance Eq (ExprKey p) where
    ExprKey x == ExprKey y  =  cmpE x y == EQ
    ExprKey x /= ExprKey y  =  cmpE x y /= EQ

instance Ord (ExprKey p) where
    compare (ExprKey x) (ExprKey y) = cmpE x y

instance Show (ExprKey p) where
    showsPrec d (ExprKey e) = showsPrec d e

-- ==============================
-- IClock

-- ISyntax clocks
data IClock a = IClock { ic_id      :: ClockId,      -- unique id
                         ic_domain  :: ClockDomain,  -- unique id for clock "family"
                         ic_wires   :: IExpr a       -- expression for clock wires
                                              -- will be ICSel of (ICStateVar) or ICTuple of ICModPorts / ICInt (1) for ungated clocks
                                              -- theoretically ICTuple (ICInt (0), ICInt (0)) for noClock, but should  not appear
                     }

-- break recursion of wires so that showing a clock does not loop
instance Show (IClock a) where
  show (IClock clockid domain wires) = "IClock id: " ++ (show clockid) ++ " domain: " ++ (show domain) ++ " " ++ (ppString wires)

-- simple instance for now
instance PPrint (IClock a) where
  pPrint p d c = text (show c)

instance Eq (IClock a) where
  IClock {ic_id = x} == IClock {ic_id = y} = x == y

instance Ord (IClock a) where
  IClock {ic_id = x} `compare` IClock {ic_id = y} = x `compare` y

instance NFData (IClock a) where
  -- XXX clock wires can be recursive (so just check equality)
  rnf c = (c==c) `seq` ()

makeClock :: ClockId -> ClockDomain -> IExpr a -> IClock a
makeClock clockid domain wires = IClock { ic_id     = clockid,
                                          ic_domain = domain,
                                          ic_wires  = wires }

getClockWires :: IClock a -> IExpr a
getClockWires = ic_wires

-- used to implement primReplaceClockGate
setClockWires :: IClock a -> IExpr a -> IClock a
setClockWires ic e = ic { ic_wires = e }

getClockDomain :: IClock a -> ClockDomain
getClockDomain = ic_domain

-- noClock value defined in ISyntaxUtil
isNoClock :: IClock a -> Bool
isNoClock IClock {ic_id = clockid} = clockid == noClockId

--
isMissingDefaultClock :: IClock a -> Bool
isMissingDefaultClock (IClock {ic_id = clockid}) = clockid == noDefaultClockId

sameClockDomain :: IClock a -> IClock a -> Bool
sameClockDomain (IClock {ic_domain = d1}) (IClock {ic_domain = d2}) = d1 == d2

inClockDomain :: ClockDomain -> IClock a -> Bool
inClockDomain d (IClock {ic_domain = d'}) = d == d'

-- ==============================
-- IReset

-- ISyntax resets
-- XXX this will change as reset is more fully implemented
data IReset a = IReset { ir_id   :: ResetId, -- unique id
                         ir_clock :: IClock a, -- associated clock (may be noClock)
                         -- reset_sync :: Bool, -- synchronous or asynchronous
                         ir_wire :: IExpr a  -- expression for reset wire
                                             -- currently must be an ICModPort or 0,
                                             -- since we do not support reset output
                       }

-- must break recursion of wire so showing a reset output does not loop
instance Show (IReset a) where
  show (IReset resetid clock wire) = "IReset id: " ++ (show resetid) ++ " clock: " ++ (show clock) ++ " " ++ (ppString wire)

-- simple instance for now
instance PPrint (IReset a) where
  pPrint p d r = text (show r)

instance Eq (IReset a) where
  IReset {ir_id = x} == IReset {ir_id = y} = x == y

instance Ord (IReset a) where
  IReset {ir_id = x} `compare` IReset {ir_id = y} = x `compare` y

instance NFData (IReset a) where
  -- XXX reset wires can be recursive (so just check equality)
  rnf r = (r==r) `seq` ()

makeReset :: ResetId -> IClock a -> IExpr a -> IReset a
makeReset i c w = IReset { ir_id = i, ir_clock = c, ir_wire = w }

getResetWire :: IReset a -> IExpr a
getResetWire = ir_wire

getResetClock :: IReset a -> IClock a
getResetClock = ir_clock

getResetId :: IReset a -> ResetId
getResetId = ir_id

-- noReset defined in ISyntaxUtil (like noClock)
isNoReset :: IReset a -> Bool
isNoReset IReset { ir_id = i } = i == noResetId

isMissingDefaultReset :: IReset a -> Bool
isMissingDefaultReset (IReset { ir_id = i }) = i == noDefaultResetId

-- ==============================
-- IInout

data IInout a =
    IInout { io_clock :: IClock a, -- associated clock (may be noClock)
             io_reset :: IReset a, -- associated reset (may be noReset)
             io_wire :: IExpr a  -- expression for inout wire
           }

instance Show (IInout a) where
  show (IInout clock reset wire) =
      "IInout clock: " ++ show clock ++ " reset: " ++ show reset ++
      " |" ++ ppReadable wire ++ "|"

instance PPrint (IInout a) where
  pPrint p d r@IInout { io_wire = wire } = pPrint p d wire

instance NFData (IInout a) where
  -- XXX wires can be recursive, so just check the other parts
  rnf (IInout c r w) = (c==c) `seq` (r==r) `seq` ()

makeInout :: IClock a -> IReset a -> IExpr a -> IInout a
makeInout c r w = IInout { io_clock = c, io_reset = r, io_wire = w }

getInoutClock :: IInout a -> IClock a
getInoutClock = io_clock

getInoutReset :: IInout a -> IReset a
getInoutReset = io_reset

getInoutWire :: IInout a -> IExpr a
getInoutWire = io_wire

-- ==============================
-- Primitive Arrays

-- We guarantee that ICLazyArray elements are references during IExpand,
-- using this type for the elements.
-- This ensures that the equality check in "improveIf" is inexpensive.
-- After IExpand, we can't have heap refs anymore, so we convert ICLazyArray
-- into application of PrimBuildArray to the element expressions.
--
data ArrayCell a = ArrayCell { ac_ptr :: Int, ac_ref :: Ref a }

instance Show (ArrayCell a) where
  show (ArrayCell i _) = "_" ++ show i

instance NFData (ArrayCell a) where
  rnf (ArrayCell i _) = rnf i

type ILazyArray a = Array.Array Integer (ArrayCell a)

-- NFData instance for Array is provided by Control.DeepSeq

-- ==============================
-- Pred

-- Predicates used for implicit conditions.
-- most utility functions in IExpandUtils
newtype Pred a = PConj (PSet a)
        deriving (Eq, Show)

instance PPrint (Pred a) where
    pPrint d p (PConj ps) = pPrint d p (map unPTermKey (S.toList ps))

instance PPrint (PTerm a) where
    pPrint d p (PAtom e) = pPrint d p e
    pPrint d p (PIf c t e) = text "PIf(" <> sepList [pPrint d 0 c, pPrint d 0 t, pPrint d 0 e] (text ",") <> text ")"
    pPrint d p (PSel idx _ es) = text "PSel(" <> sepList (pPrint d 0 idx : map (pPrint d 0) es) (text ",") <> text ")"

instance NFData (Pred a) where
-- XXX - see if we can get away with not forcing
--       the internal Pred structure
--       worried about Array/Reset/Clock-like issues
   rnf p = ()

-- The conjuncts of a Pred are kept in a set ordered by PTermKey (the
-- structural order over cmpE); S.toList gives them in that order.
type PSet a = S.Set (PTermKey a)
data PTerm a = PAtom (IExpr a)
             | PIf (IExpr a) (Pred a) (Pred a)
             | PSel (IExpr a) Integer [Pred a]
        deriving (Eq, Show)

-- A predicate term as a set/map key, ordered structurally (the order
-- that `deriving Ord` gave PTerm when IExpr had an Ord instance in
-- every phase: constructor order, then the fields left to right).
newtype PTermKey p = PTermKey { unPTermKey :: PTerm p }

instance Eq (PTermKey p) where
    PTermKey x == PTermKey y  =  x == y

instance Ord (PTermKey p) where
    compare (PTermKey x) (PTermKey y) = cmpPTerm x y

instance Show (PTermKey p) where
    showsPrec d (PTermKey t) = showsPrec d t

cmpPTerm :: PTerm p -> PTerm p -> Ordering
cmpPTerm (PAtom e1) (PAtom e2) = cmpE e1 e2
cmpPTerm (PAtom _) _ = LT
cmpPTerm (PIf _ _ _) (PAtom _) = GT
cmpPTerm (PIf c1 t1 e1) (PIf c2 t2 e2) =
        case cmpE c1 c2 of
        EQ -> case cmpPred t1 t2 of
              EQ -> cmpPred e1 e2
              o  -> o
        o  -> o
cmpPTerm (PIf _ _ _) (PSel _ _ _) = LT
cmpPTerm (PSel e1 n1 ps1) (PSel e2 n2 ps2) =
        case cmpE e1 e2 of
        EQ -> case compare n1 n2 of
              EQ -> cmpPreds ps1 ps2
              o  -> o
        o  -> o
cmpPTerm (PSel _ _ _) _ = GT

-- the order of the conjunct sets (what `deriving Ord` gave Pred)
cmpPred :: Pred p -> Pred p -> Ordering
cmpPred (PConj s1) (PConj s2) = compare s1 s2

cmpPreds :: [Pred p] -> [Pred p] -> Ordering
cmpPreds [] [] = EQ
cmpPreds [] (_:_) = LT
cmpPreds (_:_) [] = GT
cmpPreds (x:xs) (y:ys) =
        case cmpPred x y of
        EQ -> cmpPreds xs ys
        o  -> o

-- ==============================
-- IConInfo

-- The variants are restricted by phase through their return types (see
-- the Phase comment above).  The constant's type lives on the ICon node
-- (ICon i t ic), not here: GHC requires constructors that share a
-- record field to share a result type, so a type field on every
-- variant would need a name per constructor.  A variant carries only
-- its own fields.
data IConInfo (p :: Phase) where
          -- top level definition
          --  iconDef has the definition body
          -- may be _ if the ICDef was read from a .bo file and has not been fixed-up yet
          -- these disappear in IExpand and do not exists in IModule
        ICDef :: { iConDef :: IExpr ('Ph 'WithBinders e) }
              -> IConInfo ('Ph 'WithBinders e)
          -- primitive
        ICPrim :: { primOp :: PrimOp } -> IConInfo p
          -- foreign function; foports specifies input and output port names in verilog
          -- (for functions implemented via module instantiation - primarily "noinlined")
          -- The inputs are grouped per argument (the inner list is the ports of
          -- one argument, of which there may be several when the argument splits);
          -- the outputs are a flat list (the single result, possibly split).
          -- Each port is its name and bit size.
          -- Nothing in foports indicates this is a "true" foreign function
          -- (positional module instantiation is no longer supported)
          -- fcallNo is a cookie used to mark foreign function calls during elaboration
          -- so an association can be made between the Action and Value parts of an
          -- ActionValue call (e.g. $fopen or $stime) for use deep in the output codegens
        ICForeign :: { fName :: String,
                       isC :: Bool,
                       foports :: Maybe ([[(String, Integer)]], [(String, Integer)]),
                       -- the declaration's type variable names, in
                       -- quantification order: the numeric type arguments
                       -- at an application pair with these, and they name
                       -- the Verilog instance parameters (only used when
                       -- foports is set)
                       fTyVarNames :: [String],
                       fcallNo :: Maybe Integer }
                  -> IConInfo p
          -- constructor
        ICCon :: { conTagInfo :: ConTagInfo } -> IConInfo p
          -- function that tests whether its argument is the right kind of a constructor
          --  eventually cancels out and turns into ICInt 0 (false) or 1 (true)
        ICIs :: ConTagInfo -> IConInfo ('Ph 'WithBinders e)
          -- function that projects the data associated with a particular constructor
          -- only used after doing appropriate ICIs, otherwise turns into _,
          --   which is "convenient for some transformations" (_s can be "optimized later")
          -- (used to bind variables in pattern matching)
        ICOut :: ConTagInfo -> IConInfo ('Ph 'WithBinders e)
          -- tuple constructor
          -- fieldIds names fields of struct that turned into this tuple
        ICTuple :: { fieldIds :: [Id] } -> IConInfo p
          -- select field selNo out of tuple that has numSel fields
        ICSel :: { selNo :: Integer, numSel :: Integer } -> IConInfo p
          -- reference to a Verilog module; vMethTs has types of method arguments
        ICVerilog :: { isUserImport :: Bool,
                       vInfo :: VModInfo,
                       vMethTs :: [[IType]] }
                  -> IConInfo ('Ph 'WithBinders e)
          -- underscores of different varieties:
          --   - user-inserted (IUDontCare)
          --   - unreachable _ (IUNotUsed) (needed for some expression data structs)
          --   - pattern matching failure (IUNoMatch)
        ICUndet :: { iuKind :: UndefKind } -> IConInfo p
          -- numeric integer literal
        ICInt :: { iVal :: IntLit } -> IConInfo p
          -- numeric real literal
        ICReal :: { iReal :: Double } -> IConInfo p
          -- string literal
        ICString :: { iStr :: String } -> IConInfo p
          -- character literal
        ICChar :: { iChar :: Char } -> IConInfo p
          -- IO handle
        ICHandle :: { iHandle :: Handle } -> IConInfo Elab
          -- instantiated Verilog module
        ICStateVar :: { iVar :: IStateVar ('Ph b 'Evaluated) }
                   -> IConInfo ('Ph b 'Evaluated)
          -- interface method argument variable
          -- only exists after expansion
          -- note that the port's identifier and type come from the surrounding ICon
        ICMethArg :: IConInfo ('Ph b 'Evaluated)
          -- external module input (either as port or parameter)
          -- Only exists after expansion.
          -- Note that the port/param's identifier and type come from
          -- the surrounding ICon.
          -- ICModPort is used for dynamic inputs (including clock and reset wires)
        ICModPort :: IConInfo ('Ph b 'Evaluated)
        ICModParam :: IConInfo ('Ph b 'Evaluated)
          -- reference to a local def in a module
          -- (similar to ICDef, which is a reference to a package def)
          -- this is created in iExpand, so it only exists in IModule
          -- and does not appear in IPackage
          -- XXX consider renaming it to ICModDef?
        ICValue :: { iValDef :: IExpr PostElab } -> IConInfo PostElab
          -- a constructor containing rule pragmas, which is used in the
          -- arguments to PrimRule.
          -- only exists before expansion
        ICRuleAssert :: { iAsserts :: [RulePragma] }
                     -> IConInfo ('Ph 'WithBinders e)
          -- a constructor containing scheduling pragmas, which is used
          -- as an argument to PrimAddSchedPragmas (applied to rules).
          -- only exists before expansion
        ICSchedPragmas :: { iPragmas :: [CSchedulePragma] }
                       -> IConInfo ('Ph 'WithBinders e)

          -- iInputNames: per-source-argument input port name groups
        ICMethod :: { iInputNames :: [[String]],
                      iOutputNames :: [String],
                      iMethod :: IExpr Elab }
                 -> IConInfo Elab
        ICClock :: { iClock :: IClock ('Ph b 'Evaluated) }
                -> IConInfo ('Ph b 'Evaluated)
        -- iReset has effective type itBit1
        ICReset :: { iReset :: IReset ('Ph b 'Evaluated) }
                -> IConInfo ('Ph b 'Evaluated)
        ICInout :: { iInout :: IInout ('Ph b 'Evaluated) }
                -> IConInfo ('Ph b 'Evaluated)
        -- uninit is used to give simpler error messages for completely uninitialized bit vectors / vectors
        ICLazyArray :: { iArray :: ILazyArray Elab,
                         uninit :: Maybe (IExpr Elab, IExpr Elab) }
                    -> IConInfo Elab
          -- a held pack/unpack coercion (see PrimPack/PrimUnpack in IExpand):
          -- created only during elaboration; never appears in a .bo or in
          -- the final IModule (walkNF/evalStaticOp' eliminate it on demand).
          -- lzOrig is the (heaped) value the coercion was applied to, and
          -- lzApplied is a (heaped, unevaluated) application of the
          -- underlying Bits class method to lzOrig -- so forcing evaluates
          -- the method body at most once no matter how many consumers
          -- demand the result, while a matching opposite coercion can
          -- still cancel against lzOrig at any time (Bits is coherent, so
          -- equal type arguments imply interchangeable dictionaries).
          -- lzTa/lzTn are the (a, n) type arguments, used for the
          -- cancellation match.
        ICLazyPack :: { lzTa :: IType, lzTn :: IType,
                        lzOrig :: IExpr Elab, lzApplied :: IExpr Elab }
                   -> IConInfo Elab
        ICLazyUnpack :: { lzTa :: IType, lzTn :: IType,
                          lzOrig :: IExpr Elab, lzApplied :: IExpr Elab }
                     -> IConInfo Elab
        ICName :: { iName :: Id } -> IConInfo ('Ph 'WithBinders e)
        ICAttrib :: { iAttributes :: [(Position,PProp)] }
                 -> IConInfo ('Ph 'WithBinders e)
          -- This was updated to support a list of positions,
          -- though most uses are a single position
        ICPosition :: { iPosition :: [Position] }
                   -> IConInfo ('Ph 'WithBinders e)
        ICType :: { iType :: IType } -> IConInfo ('Ph 'WithBinders e)
        ICPred :: { iPred :: Pred Elab } -> IConInfo Elab

deriving instance Show (IConInfo p)

ordC :: IConInfo a -> Int
ordC (ICDef { }) = 0
ordC (ICPrim { }) = 1
ordC (ICForeign { }) = 2
ordC (ICCon { }) = 3
ordC (ICIs { }) = 4
ordC (ICOut { }) = 5
ordC (ICTuple { }) = 6
ordC (ICSel { }) = 7
ordC (ICVerilog { }) = 8
ordC (ICUndet { }) = 9
ordC (ICInt { }) = 10
ordC (ICReal { }) = 11
ordC (ICString { }) = 12
ordC (ICChar { }) = 13
ordC (ICHandle { }) = 14
ordC (ICStateVar { }) = 15
ordC (ICMethArg { }) = 16
ordC (ICModPort { }) = 17
ordC (ICModParam { }) = 18
ordC (ICValue { }) = 19
ordC (ICRuleAssert { }) = 20
ordC (ICSchedPragmas { }) = 21
ordC (ICClock { }) = 22
ordC (ICReset { }) = 23
ordC (ICInout { }) = 24
ordC (ICLazyArray { }) = 25
ordC (ICName { }) = 26
ordC (ICAttrib { }) = 27
ordC (ICPosition { }) = 28
ordC (ICType { }) = 29
ordC (ICPred { }) = 30
ordC (ICMethod { }) = 31
ordC (ICLazyPack { }) = 32
ordC (ICLazyUnpack { }) = 33

-- Two constants with equal Ids (cmpE compares the Ids first): the
-- variant rank, then the payload fields.  The node's type takes part
-- for exactly the variants whose arms read t1 and t2 below: the ones
-- that compared it when it was a payload field.  The others ignore it
-- (two ICPrim or two ICForeign nodes with the same Id compare EQ
-- whatever their instantiated types), and every map keyed by
-- expressions depends on this order staying as it is.
cmpC :: IType -> IConInfo a -> IType -> IConInfo a -> Ordering
cmpC t1 c1 t2 c2 =
    case compare (ordC c1) (ordC c2) of
    LT -> LT
    GT -> GT
    EQ ->
        case c1 of
        ICDef { } -> EQ
        ICPrim { } -> EQ
        ICForeign { } -> compare (fcallNo c1) (fcallNo c2)
        -- XXX ICCon should check conNo and numCon instead of relying
        -- on the identifier equality from ICon
        ICCon { } -> compare t1 t2
        ICIs _ -> compare t1 t2
        ICOut _ -> compare t1 t2
        ICTuple { } -> compare t1 t2
        ICSel { } -> compare t1 t2
        ICVerilog { vInfo = s1 } ->
            -- ignores method types and whether they are user imports or not
            compare (t1, s1) (t2, vInfo c2)
        ICUndet { } -> compare t1 t2
        ICInt { iVal = i1 } -> compare (t1, i1) (t2, iVal c2)
        ICReal { iReal = r1 } -> compare (t1, r1) (t2, iReal c2)
        ICString { iStr = s1 } -> compare (t1, s1) (t2, iStr c2)
        ICChar { iChar = chr1 } ->
            -- the type should always be Char (should we compare anyway?)
            compare chr1 (iChar c2)
        ICHandle { } -> EQ
        ICStateVar { iVar = n1 } -> compare n1 (iVar c2)
        ICValue { } -> EQ
        ICMethArg { } -> EQ
        ICModPort { } -> EQ
        ICModParam { } -> EQ
        ICRuleAssert { iAsserts = asserts } -> compare asserts (iAsserts c2)
        ICSchedPragmas { iPragmas = pragmas } -> compare pragmas (iPragmas c2)
        ICMethod { iInputNames = inames1, iOutputNames = outnames1, iMethod = meth1 } ->
            case compare (inames1, outnames1) (iInputNames c2, iOutputNames c2) of
              EQ -> cmpE meth1 (iMethod c2)
              o  -> o
        -- the ICon Id is not sufficient for equality comparison for Clk/Rst
        ICClock { iClock = clock1 } -> compare clock1 (iClock c2)
        ICReset { iReset = reset1 } -> compare reset1 (iReset c2)
        -- for Inout, the ICon Id is the correct Id
        ICInout { } -> EQ
        ICLazyArray { iArray = arr } -> compare (map ac_ptr (Array.elems arr))
                                                (map ac_ptr (Array.elems (iArray c2)))
        -- lzApplied is deliberately not compared: it is determined by
        -- lzOrig and the (coherent) dictionary, and two independently
        -- created coercions of the same value should compare equal even
        -- though they hold separate applied cells
        ICLazyPack { lzTa = ta1, lzTn = tn1, lzOrig = o1 } ->
            case compare (ta1, tn1) (lzTa c2, lzTn c2) of
              EQ -> cmpE o1 (lzOrig c2)
              o  -> o
        ICLazyUnpack { lzTa = ta1, lzTn = tn1, lzOrig = o1 } ->
            case compare (ta1, tn1) (lzTa c2, lzTn c2) of
              EQ -> cmpE o1 (lzOrig c2)
              o  -> o
        ICName { iName = n } -> compare n (iName c2)
        ICAttrib { iAttributes = pps } ->
            let pps_no_pos = map snd pps
                pps2_no_pos = map snd (iAttributes c2)
            in  compare pps_no_pos pps2_no_pos
        ICPosition { iPosition = p1 } -> compare p1 (iPosition c2)
        ICType {iType = t1 } -> compare t1 (iType c2)
        ICPred {iPred = p1 } -> cmpPred p1 (iPred c2)

isIConInt, isIConReal, isIConParam :: IExpr a -> Bool
isIConInt (ICon_ _ _ _ (ICInt { })) = True
isIConInt _ = False

isIConReal (ICon_ _ _ _ (ICReal { })) = True
isIConReal _ = False

isIConParam (ICon_ _ _ _ ICModParam) = True
isIConParam _ = False

-- ============================================================
-- value/type substitution, free value/type variables
-- (tSubst, eSubst, etSubst have been moved to ISyntaxSubst)

-- --------------------

-- All variables
aVars :: IExpr a -> S.Set Id
aVars (ILam i t e) = S.insert i (aVars e `S.union` fTVars t)
aVars (IVar i) = S.singleton i
aVars (ILAM i _ e) = S.insert i (aVars e)
aVars (IAps_ _ f ts es) = (aVars f) `S.union`
                        (S.unions (map fTVars ts)) `S.union`
                        (S.unions (map aVars es))
aVars (ICon_ _ _ _ _) = S.empty  -- XXX
aVars (IRefT _ _ _ _) = S.empty

-- --------------------

-- Free variables
fVars :: IExpr a -> S.Set Id
fVars (ILam i _ e) = S.delete i (fVars e)
fVars (IVar i) = S.singleton i
fVars (ILAM _ _ e) = fVars e
fVars (IAps_ _ f ts es) = fVars f `S.union` (S.unions (map fVars es))
fVars (ICon_ _ _ _ _) = S.empty
fVars (IRefT _ _ _ _) = S.empty

-- --------------------

-- All definitions and variables
fdVars :: IExpr a -> [Id]
fdVars e = S.toList (fdVars' e)

fdVars' :: IExpr a -> S.Set Id
fdVars' (ILam i _ e) = fdVars' e
fdVars' (IVar i) = S.singleton i
fdVars' (ILAM _ _ e) = fdVars' e
fdVars' (IAps_ _ f ts es) = fdVars' f `S.union` (S.unions (map fdVars' es))
fdVars' (ICon_ _ i _ (ICDef { })) = S.singleton i
fdVars' (ICon_ _ i _ (ICValue { })) = S.singleton i
fdVars' (ICon_ _ _ _ _) = S.empty
fdVars' (IRefT _ _ _ _) = S.empty

-- --------------------

-- Free type variables
ftVars :: IExpr a -> S.Set Id
ftVars (ILam i _ e) = ftVars e
ftVars (IVar i) = S.empty
ftVars (ILAM i _ e) = S.delete i (ftVars e)
ftVars (IAps_ _ f ts es) = (ftVars f) `S.union` (S.unions (map fTVars ts))
                                     `S.union` (S.unions (map ftVars es))
ftVars (ICon_ _ _ _ _) = S.empty                -- XXX
ftVars (IRefT _ _ _ _) = S.empty

-- ============================================================
-- PPrint (for those instances not defined alongside the type, above)

pPrintLink :: PDetail -> Int -> (Id, String) -> Doc
pPrintLink d i (mi, hash) = (ppId d mi) <+> (text hash)

instance PPrint (IPackage a) where
 pPrint d p (IPackage mi lps ps ds _) =
        (text "IPackage" <+> ppId d mi) $+$
        (text "  --linked packages") $+$
        foldr (($+$) . pPrintLink d 0) (text "") lps $+$
        (text "  --pragmas ")  $+$
        foldr (($+$) . pPrint d 0) (text "") ps $+$
        (text "  --idefs ")  $+$
        foldr (sep (text "next def..........................................................") . ppDef d) (text "") ds
  where sep a b c = b $+$ a $+$ c

instance PPrint (IModule PostElab) where
 pPrint d p (IModule mi fmod be wi ps ks as clks rsts vs pts ds rs ifc ffcalNo cmap) =
        (text "IModule" <+> ppId d mi <> if fmod then text " -- function" else text "") $+$
        (case be of
             Nothing -> empty
             Just be -> text " -- backend specific:" <+> pPrint d 0 be) $+$
        text "-- wire info" $+$
        pPrint d p wi $+$
        text "-- pragmas" $+$
        foldr (($+$) . pPrint d 0) (text "") ps $+$
        text "-- imod parameters" $+$
        foldr (($+$) . ppMV d) (text "") ks $+$
        text "-- imod args" $+$
        foldr (($+$) . pPrint d 0) (text "") as $+$
        text "-- imod clock domains" $+$
        foldr (($+$) . pPrint d 0) (text "") clks $+$
        foldr (($+$) . pPrint d 0) (text "") rsts $+$
        text "-- imod state instances" $+$
        foldr (($+$) . ppSV) (text "") vs $+$
        text "-- port types" $+$
        foldr (($+$) . ppPT) (text "") (M.toList pts) $+$
        text "-- imod local defs" $+$
        foldr (($+$) . ppDef d) (text "") ds $+$
        text "-- imod rules" $+$
        pPrint d 0 rs $+$
        text "" $+$
        text "-- imod interface" $+$
        foldr (($+$) . pPrint d 0) (text "") ifc
  where ppSV (i, sv) = ppId d i <> pPrint d 0 sv
        ppPT (i, m) =
            foldr ($+$) (text "")
                [ ppPort i port <> text " :: " <>
                  pPrint d 0 ty | (VName port, ty) <- M.toList m ]
          where ppPort Nothing p = text p
                ppPort (Just i) p = ppId d i <> text ("$" ++ p)

ppMV :: (PPrint a) => PDetail -> (Id, a) -> Doc
ppMV d (i, ty) = ppId d i <+> text "::" <+> pPrint d 0 ty

-- the per-phase printers of the rule-holding records, through one
-- printer each that takes the printer of the part whose instance is
-- per phase (the body, the rule, the rules)
instance PPrint (IEFace Elab) where
    pPrint = ppIEFace pPrint

instance PPrint (IEFace PostElab) where
    pPrint = ppIEFace pPrint

ppIEFace :: (PDetail -> Int -> Maybe (IRules a) -> Doc) -> PDetail -> Int -> IEFace a -> Doc
ppIEFace ppRules d p (IEFace i vs et rules wp fi)
        =       text "-- args" $+$
                foldr (($+$) . ppMV d) b (concat vs)
              where b =        text "-- body" $+$
                        (case et of
                          Just (e,t) -> ppDef d $ IDef i t e []
                          _ -> empty ) $+$
                        text "-- rules" $+$
                        ppRules d 0 rules $+$
                        text "-- wire properties" $+$
                        pPrint d 0 wp $+$
                        text "-- field info" $+$
                        pPrint d 0 fi $+$
--                      text "-- guard" $+$
--                        ppDef d wi wt we $+$
                        text ""

instance PPrint IAbstractInput where
    pPrint d p (IAI_Port (i,ty)) = ppId d i <+> text "::" <+> pPrint d 0 ty
    pPrint d p (IAI_Clock osc Nothing) =
        text "clock {" <+>
        (text "osc =" <+> pPrint d 0 osc) <+>
        text "}"
    pPrint d p (IAI_Clock osc (Just gate)) =
        text "clock {" <+>
        (text "osc =" <+> pPrint d 0 osc <> text "," <+>
         text "gate =" <+> pPrint d 0 gate) <+>
        text "}"
    pPrint d p (IAI_Reset r) =
        text "reset {" <+> pPrint d 0 r <+> text "}"
    pPrint d p (IAI_Inout r n) =
        text "inout {" <+> pPrint d 0 r <> text"[" <> pPrint d 0 n <> text"]" <+> text "}"

instance PPrint (IStateVar a) where
    pPrint d p sv@(IStateVar _ _ _ vi xs t _ _ _ _) =
        let ps = [e | (i,e) <- zip (vArgs vi) xs, isParam i]
            as = [(v,e) | (Port (v,_) _ _, e) <- zip (vArgs vi) xs]
            ppPortConnection (VName s,e) =
                text ("." ++ s ++ "(") <> pPrint d 0 e <> text ")"
        in  text " ::" <+> pPrint d 0 t <+> text "=" <+> pPrint d 0 (vName vi) <>
            (case ps of
              [] -> text ""
              _ -> text " #(" <>
                   sepList (map (pPrint d 0) ps) (text ",")
                   <> text ")") <>
            (case as of
              [] -> text ""
              _ -> text " (" <>
                   sepList (map ppPortConnection as) (text ",") <>
                   text ")")


instance PPrint (IRule Elab) where
    pPrint = ppIRule pPrint

instance PPrint (IRule PostElab) where
    pPrint = ppIRule pPrint

ppIRule :: (PDetail -> Int -> Body a -> Doc) -> PDetail -> Int -> IRule a -> Doc
ppIRule ppBody d p (IRule {
                   irule_name = longname,
                   irule_pragmas = rps,
                   irule_description = s,
                   irule_pred = c,
                   irule_body = a }) =
        (text "" <+> vcat (map (pPrint d 0) rps)) $+$
        (text "" <+> text (show longname) <> text ":") $+$
        (text "" <+> text (show s) <> text ":") $+$
        (text "  when" <+> pPrint d 0 c) $+$
        (text "   ==>" <+> ppBody d 0 a)

instance PPrint (IRules Elab) where
    pPrint = ppIRules pPrint

instance PPrint (IRules PostElab) where
    pPrint = ppIRules pPrint

ppIRules :: (PDetail -> Int -> IRule a -> Doc) -> PDetail -> Int -> IRules a -> Doc
ppIRules ppRule d p (IRules sps rs) =
        foldr (($+$) . pPrint d 0) (text "") sps $+$
        foldr (($+$) . ppRule d 0) (text "") rs

ppQuant :: PPrint a => String -> PDetail -> Int -> Id -> a -> IExpr b -> Doc
ppQuant s d p i t e =
    pparen (p>0) (sep [text s <> pparen True (pPrint d 0 i <>text" ::" <+> pPrint d 0 t) <+> text ".", pPrint d 0 e])
    --pparen (p>0) (text s <> pparen True (pPrint d 0 i <>text" ::" <+> pPrint d 0 t) <+> text "." <+> pPrint d 0 e)

instance PPrint (IDef a) where
    pPrint d _p def = ppDef d def

ppDef :: PDetail -> IDef a -> Doc
ppDef d (IDef i t e p) =
    sep [pPrint d 0 i <+> text "::", nest 2 (pPrint d 0 t)] $+$
    sep [pPrint d 0 i <+> text "=", nest 2 (pPrint d 0 e)] $+$
    (if (null $ getIdProps i) then empty else
       text "-- IdProp:" <+> text (show i) ) $+$
    (if (null p) then empty else
       text "-- Properties:" <+> text (show p
             -- avoid line wrap in what is supposed to be a comment
                                      ))

instance PPrint (IExpr a) where
    pPrint d p (ILam i t e) = ppQuant "\\ "  d p i t e
    pPrint d p (IAps_ _ f ts es) = ppAps d p f ts es
    pPrint d p (ICon_ _ i t (ICUndet _)) = text "_ :: " <+> pPrint d maxPrec t
    pPrint d@PDReadable p (ICon_ _ i _ (ICDef _)) = ppId d i <> text "="
    pPrint d@PDReadable p (ICon_ _ i _ (ICVerilog { vInfo = vi })) = pparen True $ text "verilog" <+> pPrint d 0 vi
    pPrint d@PDReadable p (ICon_ _ i _ (ICIs _)) = ppId d i <> text "?"
    pPrint d@PDReadable p (ICon_ _ i _ (ICOut _)) = text "out" <> ppId d i
    pPrint d@PDReadable p (ICon_ _ i _ (ICSel _ _)) = text "." <> ppId d i
    pPrint d@PDReadable _ (ICon_ _ i _ (ICPrim p)) = text (show p)
    pPrint d@PDReadable _ (ICon_ _ i _ (ICLazyArray {})) = ppId d i <> text "[Array]"
    -- distinguish held coercions from an application of the bare
    -- primitive (they only ever print from diagnostics, so show the
    -- payload ref too)
    pPrint d@PDReadable _ (ICon_ _ i _ (ICLazyPack { lzOrig = o })) =
        ppId d i <> text "[Held " <> pPrint d 0 o <> text "]"
    pPrint d@PDReadable _ (ICon_ _ i _ (ICLazyUnpack { lzOrig = o })) =
        ppId d i <> text "[Held " <> pPrint d 0 o <> text "]"
--    pPrint d@PDReadable _ (ICon id con) = ppId d id <> text (": " ++ show con)
    pPrint d p (IVar i) = ppId d i -- <> text ":V"
    pPrint d p (ILAM i k e) = ppQuant "/\\ "  d p i k e
    pPrint d p (ICon_ _ _ _ (ICString s)) = text (show s)
    pPrint d p (ICon_ _ _ _ (ICChar c)) = text (show c)
    pPrint d@PDDebug p (ICon_ _ _ t (ICInt { iVal = i })) = pPrint d p i <> text "::" <> pPrint d maxPrec t
    pPrint d p (ICon_ _ _ _ (ICInt { iVal = i })) = pPrint d p i
    pPrint d p (ICon_ _ _ _ (ICReal { iReal = r })) = pPrint d p r
    pPrint d@PDDebug p (ICon_ _ i t _) = ppId d i <> text "::" <> pPrint d maxPrec t
    pPrint d p ict@(ICon_ _ i _ (ICForeign {fcallNo = (Just n)})) = ppId d i <> text ("#" ++ show n)
    pPrint d p (ICon_ _ i _ _) = ppId d i
    pPrint d p (IRefT _ ptr _ _) = text ("_") <> pPrint d 0 ptr

-- An application, given its parts (so that the let-printing arm can
-- print the body applied to the remaining arguments without building a
-- node, which would need the phase's annotation).  The three arms are
-- the ones an application node can match in the instance above, in
-- that order.
ppAps :: PDetail -> Int -> IExpr a -> [IType] -> [IExpr a] -> Doc
ppAps d p (ICon_ _ _ _ (ICPrim { primOp = PrimJoinActions })) _ [e1, e2] =
    let getActions (IAps_ _ (ICon_ _ _ _ (ICPrim { primOp = PrimJoinActions })) _ [e1', e2']) = getActions e1' ++ getActions e2'
        getActions e = [e]
        as = getActions e1 ++ getActions e2
    in  text "{" <+> sepList (map (pPrint d 0) as) (text ";") <+> text "}"
ppAps d p (ILam i t e') [] (e:es) = pparen (p > 0) $
    (text "let" <+> ppDef d (IDef i t e [])) $+$
    (text "in  " <> ppApsRest d 0 e' es)
ppAps d p f ts es = pparen (p>(maxPrec-1)) $
    sep (pPrint d (maxPrec-1) f : map (nest 2 . (text"\183" <>) . pPrint d maxPrec) ts ++ map (nest 2 . pPrint d maxPrec) es)

-- f applied to es (what `iAps f es` would print)
ppApsRest :: PDetail -> Int -> IExpr a -> [IExpr a] -> Doc
ppApsRest d p f [] = pPrint d p f
ppApsRest d p f es = ppAps d p f [] es

-- ============================================================
-- Hyper (for those instances not defined alongside the type, above)

instance NFData (IPackage a) where
    rnf (IPackage i lps ps ds atfCache) = rnf5 i ps lps ds atfCache

instance NFData (IModule PostElab) where
    rnf (IModule x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16) =
        rnf16 x1 x2 x3 x4 x5 x6 x7 x8 x9 x10 x11 x12 x13 x14 x15 x16

instance NFData (IEFace Elab) where
    rnf (IEFace x1 x2 x3 x4 x5 x6) = rnf6 x1 x2 x3 x4 x5 x6

instance NFData (IEFace PostElab) where
    rnf (IEFace x1 x2 x3 x4 x5 x6) = rnf6 x1 x2 x3 x4 x5 x6

instance NFData IAbstractInput where
    rnf (IAI_Port p) = rnf p
    rnf (IAI_Clock o mg) = rnf2 o mg
    rnf (IAI_Reset r) = rnf r
    rnf (IAI_Inout r n) = rnf2 r n

instance NFData (IDef a) where
    rnf (IDef i t e p) = rnf4 i t e p

instance NFData (IExpr a) where
    rnf (ILam i t e) = rnf3 i t e
    rnf (IAps_ _ e ts es) = rnf3 e ts es
    rnf (IVar i) = rnf i
    rnf (ILAM i k e) = rnf3 i k e
    -- the type is forced with the payload, except under ICDef and ICValue,
    -- whose payload arms force nothing (as they did when the type was a
    -- payload field: a forced ICDef can loop through its definition)
    rnf (ICon_ _ i t ic) = case ic of
        ICDef { }   -> rnf i
        ICValue { } -> rnf i
        _           -> rnf3 i t ic
    rnf (IRefT t p poss _) = rnf2 t poss

instance NFData (IConInfo a) where
--    rnf (ICDef x1) = rnf x1
    rnf (ICDef _) = ()                               -- XXX a hack to avoid circular defs
    rnf (ICPrim x1) = rnf x1
    rnf (ICForeign x1 x2 x3 x4 x5) = rnf5 x1 x2 x3 x4 x5
    rnf (ICCon x1) = rnf x1
    rnf (ICIs x1) = rnf x1
    rnf (ICOut x1) = rnf x1
    rnf (ICTuple x1) = rnf x1
    rnf (ICSel x1 x2) = rnf2 x1 x2
    rnf (ICVerilog x1 x2 x3) = rnf3 x1 x2 x3
    rnf (ICUndet x1) = rnf x1
    rnf (ICInt x1) = rnf x1
    rnf (ICReal x1) = rnf x1
    rnf (ICString x1) = rnf x1
    rnf (ICChar x1) = rnf x1
    rnf (ICHandle x1) = rnf x1
    rnf (ICStateVar x1) = rnf x1
    rnf ICMethArg = ()
    rnf ICModPort = ()
    rnf ICModParam = ()
    -- rnf (ICValue x1) = rnf x1
    -- XXX the above line causes cycles somehow so, like ICDef, we don't enter ICValue
    rnf (ICValue _) = ()
    rnf (ICRuleAssert x1) = rnf x1
    rnf (ICSchedPragmas x1) = rnf x1
    rnf (ICMethod x1 x2 x3) = rnf3 x1 x2 x3
    rnf (ICClock x1) = rnf x1
    rnf (ICReset x1) = rnf x1
    rnf (ICInout x1) = rnf x1
    rnf (ICName x1) = rnf x1
    rnf (ICAttrib x1) = rnf x1
    rnf (ICLazyArray x1 x2) = rnf2 x1 x2
    rnf (ICLazyPack x1 x2 x3 x4) = rnf4 x1 x2 x3 x4
    rnf (ICLazyUnpack x1 x2 x3 x4) = rnf4 x1 x2 x3 x4
    rnf (ICPosition x1) = rnf x1
    rnf (ICType x1) = rnf x1
    rnf (ICPred x1) = rnf x1

instance NFData (IStateVar a) where
    rnf x = (x==x) `seq` ()                -- XXX (does not evaluate IStateVar components)

-- ============================================================
-- XRef (and other utilities?) beyond this point


-- #############################################################################
-- #
-- #############################################################################

getIExprPositionCross :: IExpr a -> Position
getIExprPositionCross iexpr =
    if (True)
       then (getIExprPositionCrossInternal 0 iexpr)
       else noPosition

-- #############################################################################
-- #
-- #############################################################################

getIExprPositionCrossInternal :: Int -> IExpr a -> Position
getIExprPositionCrossInternal 10 _ = noPosition

getIExprPositionCrossInternal n (ILam i _ e) =
    let pos = (getIExprPositionCrossInternal (n + 1) e)
    in  firstPos [pos, getIdPosition i]

getIExprPositionCrossInternal n (IAps_ _ e ts es) =
    let pos = (getIExprPositionCrossInternal (n + 1) e)
    in  firstPos (pos : (map (getIExprPositionCrossInternal (n + 1)) es))

getIExprPositionCrossInternal _ (IVar i) = getIdPosition i

getIExprPositionCrossInternal n (ILAM i _ e) =
    let pos = (getIExprPositionCrossInternal (n + 1) e)
    in  firstPos [pos, getIdPosition i]

getIExprPositionCrossInternal _ (ICon_ _ i _ (ICSel _ _)) =
    if (isPassThroughOp i)
        then -- trace("DDD " ++ (pfpString i)) $
             noPosition
        else -- trace("CCC " ++ (pfpString i)) $
             (getIdPosition i)


getIExprPositionCrossInternal _ (ICon_ _ i _ _) = getIdPosition i
-- The positions stamped on the heap ref (collected out of band when
-- expressions are rewritten).  There is no type fallback: Ids embedded
-- in ITypes carry no positions (IType normalizes them on entry).
getIExprPositionCrossInternal _ (IRefT _ _ poss _) =
    firstPos (S.toAscList poss)


-- #############################################################################
-- # A bunch of operators we don't want messing with "real" position info.
-- #############################################################################

idMonadNoPos, idBindNoPos, idFromIntegerNoPos, idReturnNoPos :: Id
idMonadNoPos = idMonad noPosition
idBindNoPos = idBind noPosition
idFromIntegerNoPos = idFromInteger noPosition
idReturnNoPos = idReturn noPosition

isPassThroughOp :: Id -> Bool
isPassThroughOp i = (i == idBit) ||
                    (i == idMonadNoPos) ||
                    (i == idBindNoPos) ||
                    (i == idFromIntegerNoPos) ||
                    (i == idLiftModule) ||
                    (i == idPack) ||
                    (i == idReturnNoPos) ||
                    (i == idUnpack)

-- #############################################################################
-- #
-- #############################################################################

-- Types contribute no positions here: Ids embedded in ITypes carry
-- noPosition by construction (IType normalizes them on entry), so
-- only term-side Ids and heap-ref stamps are consulted.
getIExprPosition :: IExpr a -> Position

getIExprPosition (ILam i _ e) =
    firstPos [getIdPosition i, getIExprPosition e]

getIExprPosition (IAps_ _ e _ es) =
    firstPos (getIExprPosition e : map getIExprPosition es)

getIExprPosition (IVar i) = getIdPosition i

getIExprPosition (ILAM i _ e) = firstPos [getIdPosition i, getIExprPosition e]
getIExprPosition (ICon_ _ i _ _) = getIdPosition i
-- The positions stamped on the heap ref (collected out of band when
-- expressions are rewritten).
-- When poss has several entries the pick is by Ord Position, whose
-- FString file component compares by intern order.  Today every live
-- poss is a singleton allocation seed, so the pick rule is moot; when
-- issue #863 re-enables stamping at the evaluator sites, revisit it
-- (recency is not representable in a set).
getIExprPosition (IRefT _ _ poss _) =
    firstPos (S.toAscList poss)

--------

iAP :: KnownPhase a => IExpr a -> IType -> IExpr a
{-# SPECIALISE iAP :: IExpr PreElab -> IType -> IExpr PreElab #-}
{-# SPECIALISE iAP :: IExpr Elab -> IType -> IExpr Elab #-}
{-# SPECIALISE iAP :: IExpr PostElab -> IType -> IExpr PostElab #-}
iAP (IAps f ts []) t = IAps f (ts ++ [t]) []
iAP f t = IAps f [t] []

iAp :: KnownPhase a => IExpr a -> IExpr a -> IExpr a
{-# SPECIALISE iAp :: IExpr PreElab -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE iAp :: IExpr Elab -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE iAp :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab #-}
iAp (IAps f ts es) e = IAps f ts (es ++ [e])
iAp f e = IAps f [] [e]

--------

-- shallow printing that avoids looping
showTypeless :: IExpr a -> String
showTypeless (ILam i _ e) = "(ILam " ++ (show i) ++ " _ " ++ (showTypeless e) ++ ")"
showTypeless (IAps_ _ e _ es) = "(IAps " ++ (showTypeless e) ++ " _ " ++ showTypelessList es ++ ")"
showTypeless (IVar i) = "(IVar " ++ (show i) ++ ")"
showTypeless (ILAM i k e) = "(ILAM " ++ (show i) ++ " " ++ (show k) ++ " " ++ (showTypeless e) ++ ")"
showTypeless (ICon_ _ i _ ci) = "(ICon " ++ (show i) ++ " " ++ (showTypelessCI ci) ++ " )"
showTypeless (IRefT _ i _ _) = "(IRefT " ++ "_" ++ (show i) ++ ")"

-- (the bodies are expressions only before pDef's rebuild; after it,
-- `show` of the IAction is the shallow form)
showTypelessRule :: IRule Elab -> String
showTypelessRule (IRule {
                     irule_name = n,
                     irule_pragmas = rps,
                     irule_description = s,
                     irule_pred = c,
                     irule_body = a }) =
    "(IRule " ++ (show n) ++ "\n\t" ++ (show rps) ++ "\n\t" ++
    (show s) ++ "\n\t" ++ (showTypeless c) ++ "\n\t" ++
    (showTypeless a) ++ "\n)"

showTypelessRules :: IRules Elab -> String
showTypelessRules (IRules sps rs) =
    "(IRules " ++ show sps ++ " [" ++
    foldr1 (\x y -> x ++ ", " ++ y) (map showTypelessRule rs) ++ "])"

showTypelessCI :: IConInfo a -> String
showTypelessCI (ICDef {iConDef = e}) = "(ICDef)"
showTypelessCI (ICPrim {primOp = p}) = "(ICPrim _ " ++ (show p) ++ ")"
showTypelessCI (ICForeign {fName = n, isC = b, foports = f}) = "(ICForeign _ " ++ n ++ " " ++ show b ++ " " ++ (show f) ++ ")"
showTypelessCI (ICCon {conTagInfo = cti}) = "(ICCon _ " ++ (ppReadable cti) ++ ")"
showTypelessCI (ICIs cti) = "(ICIs _ " ++ (ppReadable cti) ++ ")"
showTypelessCI (ICOut cti) = "(ICOut _ " ++ (ppReadable cti) ++ ")"
showTypelessCI (ICTuple {fieldIds = fs}) = "(ICTuple _ " ++ (show fs) ++ ")"
showTypelessCI (ICSel {selNo = i, numSel = j}) = "(ICSel _ " ++ (show i) ++ " " ++ (show j) ++ ")"
showTypelessCI (ICLazyPack {lzOrig = o}) = "(ICLazyPack _ [" ++ showTypeless o ++ "])"
showTypelessCI (ICLazyUnpack {lzOrig = o}) = "(ICLazyUnpack _ [" ++ showTypeless o ++ "])"
showTypelessCI (ICVerilog {isUserImport = ui, vInfo = v, vMethTs = vts}) = "(ICVerilog _ " ++ {--(show v)--} "<vmodinfo>" ++ " [_])"
showTypelessCI (ICUndet {iuKind = k}) = "(ICUndet _ _ )"
showTypelessCI (ICInt {iVal = v}) = "(ICInt _ " ++ (show v) ++ ")"
showTypelessCI (ICReal {iReal = v}) = "(ICReal _ " ++ (show v) ++ ")"
showTypelessCI (ICString {iStr = s}) = "(ICString _ " ++ (show s) ++ ")"
showTypelessCI (ICChar {iChar = c}) = "(ICChar _ " ++ (show c) ++ ")"
showTypelessCI (ICHandle {iHandle = h}) = "(ICHandle _ " ++ (show h) ++ ")"
showTypelessCI (ICStateVar {iVar = v}) = "(ICStateVar _ " ++ (showTypelessStateVar v) ++ ")"
showTypelessCI (ICMethArg {}) = "(ICMethArg _ )"
showTypelessCI (ICModPort {}) = "(ICModPort _ )"
showTypelessCI (ICModParam {}) = "(ICModParam _ )"
showTypelessCI (ICValue {iValDef = e}) = "(ICValue)"
showTypelessCI (ICRuleAssert {iAsserts = rps}) = "(ICRuleAssert _ " ++ (show rps) ++ ")"
showTypelessCI (ICSchedPragmas {iPragmas = sps}) = "(ICSchedPragmas _ " ++ (show sps) ++ ")"
showTypelessCI (ICMethod {iInputNames = ins, iOutputNames = outs, iMethod = m }) = "(ICMethod " ++ (show ins) ++ " " ++ (show outs) ++ " " ++ (ppReadable m) ++ ")"
showTypelessCI (ICClock {iClock = clock}) = "(ICClock)"
showTypelessCI (ICReset {iReset = reset}) = "(ICReset)"
showTypelessCI (ICInout {iInout = inout}) = "(ICInout)"
showTypelessCI (ICName {iName = name}) = "(ICName _ " ++ (show name) ++ ")"
showTypelessCI (ICAttrib {iAttributes = pps}) = "(ICAttrib _ " ++ (show (map snd pps)) ++ ")"
showTypelessCI (ICLazyArray {iArray = arr}) = "(ICLazyArray _ " ++ (ppReadable (map ac_ptr (Array.elems arr))) ++ ")"
showTypelessCI (ICPosition {iPosition = pos}) = "(ICPosition _ " ++ (show pos) ++ ")"
showTypelessCI (ICType {iType = t}) = "(ICType _ " ++ (show t) ++ ")"
showTypelessCI (ICPred {iPred = p}) = "(ICPred _ " ++ (show p) ++ ")"

showTypelessStateVar :: IStateVar a -> String
showTypelessStateVar (IStateVar b ui i v es ts mts ncs nrs l) =
    "(IStateVar " ++ (show b) ++ " "
    ++ (show i)
    ++ " <vmodinfo> "
    ++ " <params + args>" {-- showTypelessList es --}
    ++ "_"
    ++ " [[_]] "
    ++ "<IStateLoc>" ++ ")"

showTypelessList :: [IExpr a] -> String
showTypelessList es = "[" ++ intercalate ", " (map showTypeless es) ++ "]"

-- #############################################################################
-- #
-- #############################################################################
