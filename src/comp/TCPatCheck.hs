{-# LANGUAGE CPP #-}

-- | Pattern-match exhaustiveness and redundancy checking.
--
-- This implements the classic pattern-matrix "usefulness" analysis
-- (Luc Maranget, \"Warnings for pattern matching\", JFP 17(3), 2007),
-- the same foundation used by GHC's -Wincomplete-patterns and
-- -Woverlapping-patterns.
--
-- Literal patterns are rewritten into boolean guards before pattern typing.
-- To retain both source meaning and typed constructor evidence, each match
-- reserves a source skeleton before its clauses are checked and attaches a
-- typed sidecar afterward.  The checker zips the two only after the enclosing
-- top-level definition has finished typechecking (see 'flushPatObligations'
-- in TIMonad).
--
-- Typed Bool guards built from constants, variables, @not@, @&&@ and @||@
-- are lowered to propositional cubes and analyzed with the pattern columns.
-- Unsupported filters and pattern generators remain conservative: they do
-- not contribute to coverage or make later arms redundant.
-- Constructor patterns are resolved against the (resolved) column type using
-- the symbol table, so tagged unions, enums, structs and tuples are all
-- handled.  Literal matches over @Bit n@, @UInt n@ and @Int n@ additionally
-- know the finite domain of the type; mixed @Bit@ literals are represented as
-- symbolic cubes, so both enumerated literals and unions of masks can be
-- recognized as exhaustive.
module TCPatCheck(
    PatMatchContext(..),
    PatMatchTypedRow(..),
    PatMatchRow(..),
    PatMatchObligation(..),
    mkClausesObligation,
    completePatMatchObligation,
    isGeneratedScrutinee,
    checkPatMatches
) where

import Data.Bits((.&.), (.|.), xor, complement, bit, testBit, setBit, clearBit,
                 shiftR)
import Data.List(find, findIndex, intercalate, nub, zipWith4)
import Data.Maybe(isNothing)
import qualified Data.Map as M
import qualified Data.Set as S

import Classic(isClassic)
import Id
import PreIds(idBit, idUInt, idInt, idInteger, idReal, idChar, idBool,
              idFalse, idTrue, idNot, idAnd, idOrAt,
              idPrimUnit, idPrimPair, idComma, tupleIds)
import Position
import Error(WMsg, ErrMsg(..))
import Flags(Flags, patternCheckFuel, warnIncompletePatterns,
             warnOverlappingPatterns)
import CSyntax
import CFreeVars(getPV, getFVE, fvSetToFreeVars)
import CType
import Type(tString, tBool, tBitN)
import Pred(expandSyn, Qual(..))
import qualified Pred(inst)
import Scheme
import Assump
import SymTab
import IntLit
import PFPrint
import TCPatCheckTypes

-- ---------------------------------------------------------------------------
-- Obligations: pattern matches recorded during typechecking

-- | Construct an obligation from the original clauses of a match, or
-- Nothing if the match should not be checked (compiler-generated code,
-- trivially exhaustive single wildcard clauses, malformed arities).
mkClausesObligation :: PatMatchContext -> Position -> Type -> [CClause]
                    -> Maybe PatMatchObligation
mkClausesObligation _ _ _ [] = Nothing
mkClausesObligation ctx pos ty cls@(CClause ps0 _ _ : _)
    -- definitions with compiler-generated names hold compiler-copied
    -- clauses (e.g. a typeclass default replicated into an instance,
    -- which was already checked at the class declaration)
    | PMDef i <- ctx, isBadId i || isRenamingId i = Nothing
    -- a definition with no patterns has nothing to report examples of
    -- (a guard-only definition can still fail to match, but there is
    -- no pattern to point at)
    | ncols == 0 = Nothing
    | any badArity cls = Nothing
    | any (clauseBindsGeneratedVar) cls = Nothing
    | trivial = Nothing
    | otherwise = Just (PatMatchObligation pos' ctx ty ncols (map row cls))
  where
    -- point definition warnings at the first clause, not the signature
    pos' = case (ctx, cls) of
             (PMDef _, c : _) -> rowPos c
             _ -> pos
    ncols = length ps0
    badArity (CClause ps _ _) = length ps /= ncols
    -- matches in compiler-generated code (deriving, parser desugaring)
    -- bind compiler-generated pattern variables
    clauseBindsGeneratedVar (CClause ps qs _) =
        any isGenVar (S.toList (S.unions (map getPV ps))) ||
        any qualBindsGeneratedVar qs
    qualBindsGeneratedVar (CQGen _ p _) = any isGenVar (S.toList (getPV p))
    qualBindsGeneratedVar (CQFilter _) = False
    isGenVar i = isBadId i || isRenamingId i
    -- a single unguarded clause of variable/wildcard patterns is
    -- trivially exhaustive and trivially irredundant (the common case
    -- for every ordinary definition, so worth filtering early)
    trivial = case cls of
                [CClause ps [] _] -> all isWildPat ps
                _ -> False
    isWildPat (CPVar _) = True
    isWildPat (CPAny _) = True
    isWildPat (CPAs _ p) = isWildPat p
    isWildPat _ = False
    row c@(CClause ps qs _) =
        PatMatchRow { pmr_pos = rowPos c,
                      pmr_sourcePats = ps,
                      pmr_sourceQuals = qs,
                      pmr_typed = Nothing,
                      pmr_gen_dflt = isGenDflt ps }
    rowPos (CClause (p:_) _ _) | getPosition p /= noPosition = getPosition p
    rowPos c = pos
    -- the BSV parser inserts an implicit "default: noop" arm (a CPAny
    -- carrying the case statement's own position) in statement contexts;
    -- deriving uses CPAny at noPosition
    isGenDflt [CPAny p] = p == pos || p == noPosition
    isGenDflt _ = False

-- | Attach the post-typechecking sidecars to a reserved source obligation.
-- A mismatch is an internal shape inconsistency; staying quiet is safer than
-- manufacturing diagnostics from partially paired rows.
completePatMatchObligation :: PatMatchObligation -> [PatMatchTypedRow]
                           -> Maybe PatMatchObligation
completePatMatchObligation o typedRows
    | length (pmo_rows o) /= length typedRows = Nothing
    | otherwise = Just o { pmo_rows = zipWith attach (pmo_rows o) typedRows }
  where
    attach row typed = row { pmr_typed = Just typed }

-- | Is the scrutinee of a case expression compiler-generated?  The BSV
-- parser encodes plain @case@ (and @if@ with @matches@ conditions) as a
-- degenerate case on the unit struct with guard-only arms, and deriving
-- scrutinizes compiler-generated temporaries.
isGeneratedScrutinee :: CExpr -> Bool
isGeneratedScrutinee (CStruct _ i []) | qualEq i idPrimUnit = True
isGeneratedScrutinee e =
    any (\ i -> isBadId i || isRenamingId i) (fvSetToFreeVars (getFVE e))

-- ---------------------------------------------------------------------------
-- Normalized patterns

-- | A constructor (or the single implicit constructor of a struct),
-- resolved against the column type, with instantiated argument types.
data NConDesc = NConDesc {
    nc_typeId :: Id,        -- ^ qualified type id
    nc_name :: Id,          -- ^ constructor name (for witness rendering)
    nc_conNo :: Integer,    -- ^ identity within the type
    nc_isStruct :: Bool,    -- ^ the implicit constructor of a struct
    nc_isEnum :: Bool,      -- ^ member of an enum (no constructor has data)
    nc_argTypes :: [CType], -- ^ instantiated sub-column types
    nc_fieldIds :: [Id]     -- ^ field names (structs); [] for constructors
}

-- | Literal domains: Bit n and UInt n have the values [0, 2^n), while Int n
-- has [-2^(n-1), 2^(n-1)-1]; everything else is treated as unbounded.
data LitDomain = LDFinite Integer Integer  -- lo, hi (inclusive)
               | LDInfinite
    deriving (Eq)

data LitKey = LKInt Integer | LKStr String | LKChar Char | LKReal Double
    deriving (Eq, Ord)

-- A symbolic subset of a Bit column.  A set bit in bc_mask fixes the
-- corresponding bit to bc_value; all other bits are unconstrained.
data BitCube = BitCube {
    bc_width :: Integer,
    bc_mask :: Integer,
    bc_value :: Integer
} deriving (Eq, Ord)

data NPat = NWild
          | NCon NConDesc [NPat]
          | NLit LitKey
          | NMask BitCube
          -- The pattern is valid, but its match set is not statically known
          -- (for example, an overloaded StringLiteral conversion).
          | NUnknown

-- | What a pattern column looks like, derived from its type
data ColKind = CKData Id [NConDesc]
             | CKStruct NConDesc
             | CKLit LitDomain
             | CKOpaque

-- ---------------------------------------------------------------------------
-- Column type analysis

-- Full normalization is supplied by the typechecker.  In particular, it
-- reduces closed associated type functions; 'expandSyn' deliberately does
-- not.  A residual TGen belongs to a higher-rank/existential field scheme and
-- cannot be put through the ordinary typechecker normalizer in isolation.
type NormType = CType -> CType

normalizeType :: NormType -> CType -> CType
normalizeType normTy t =
    let t' = expandSyn t
    in  if hasTGen t' || not (hasATF t')
        then t'
        else expandSyn (normTy t')
  where
    hasTGen (TGen _ _) = True
    hasTGen (TAp f a) = hasTGen f || hasTGen a
    hasTGen _ = False
    hasATF (TCon (TyCon _ _ (TIatf {}))) = True
    hasATF (TAp f a) = hasATF f || hasATF a
    hasATF _ = False

colKind :: NormType -> SymTab -> CType -> ColKind
colKind normTy r t =
    let t' = normalizeType normTy t
    in  case leftCon t' of
          Nothing -> CKOpaque
          Just tcid
            | tcid `qualEq` idBit || tcid `qualEq` idUInt ->
                case tyConArgs t' of
                  -- bound the width, so that the 2^n domain value stays
                  -- cheap to compute and compare
                  [n] | isTNum n, getTNum n <= 65536 ->
                    let w = getTNum n
                    in  CKLit (LDFinite 0 (2^w - 1))
                  [n] | isTNum n -> CKLit LDInfinite
                  _ -> CKOpaque
            | tcid `qualEq` idInt ->
                case tyConArgs t' of
                  [n] | isTNum n, getTNum n == 0 ->
                    -- Prelude's inLiteralRange for Int#(0) admits exactly 0.
                    CKLit (LDFinite 0 0)
                  [n] | isTNum n, getTNum n >= 1, getTNum n <= 65536 ->
                    let w = getTNum n
                    in  CKLit (LDFinite (negate (2^(w-1))) (2^(w-1) - 1))
                  [n] | isTNum n -> CKLit LDInfinite
                  _ -> CKOpaque
            | any (qualEq tcid)
                  [idInteger, idReal, idChar, idStringTC] ->
                CKLit LDInfinite
            | otherwise ->
                case findType r tcid of
                  Just (TypeInfo (Just qtid) _ _ (TIdata cons is_enum) _) ->
                      case mapM (conDesc normTy r qtid t' is_enum) cons of
                        Just ds | not (null ds) -> CKData qtid ds
                        _ -> CKOpaque
                  Just (TypeInfo (Just qtid) _ _ (TIstruct ss fs) _)
                    | isCheckableStruct ss ->
                      case structDesc normTy r qtid t' fs of
                        Just d -> CKStruct d
                        Nothing -> CKOpaque
                  _ -> CKOpaque
  where
    isCheckableStruct SStruct = True
    isCheckableStruct (SDataCon _ _) = True
    -- tuples are the PrimPair interface in the Prelude
    isCheckableStruct (SInterface _) = True
    isCheckableStruct _ = False
    -- the type id of String (tString's TyCon)
    idStringTC = case tString of
                   TCon (TyCon i _ _) -> i
                   _ -> idInteger  -- unreachable

-- | Resolve one constructor of a data type at the given (expanded)
-- column type, computing its instantiated payload type.
conDesc :: NormType -> SymTab -> Id -> CType -> Bool -> Id -> Maybe NConDesc
conDesc normTy r qtid colty is_enum conName = do
    cis <- findCon r conName
    ci <- find (\ ci -> qualEq (ci_id ci) qtid) cis
    let (_ :>: Forall ks (_ :=> cty)) = ci_assump ci
        cti = ci_taginfo ci
    (argty, resty) <- splitArrow1 cty
    tsub <- tgenMatch resty colty
    let vec = instVector ks tsub
    return (NConDesc { nc_typeId = qtid,
                       nc_name = conName,
                       nc_conNo = conNo cti,
                       nc_isStruct = False,
                       nc_isEnum = is_enum,
                       nc_argTypes = [normalizeType normTy (Pred.inst vec argty)],
                       nc_fieldIds = [] })

-- | The single implicit constructor of a struct type, with instantiated
-- field types.
structDesc :: NormType -> SymTab -> Id -> CType -> [Id] -> Maybe NConDesc
structDesc normTy r qtid colty fieldIds = do
    fts <- mapM fieldType fieldIds
    return (NConDesc { nc_typeId = qtid,
                       nc_name = qtid,
                       nc_conNo = 0,
                       nc_isStruct = True,
                       nc_isEnum = False,
                       nc_argTypes = fts,
                       nc_fieldIds = fieldIds })
  where
    fieldType fid = do
        fis <- findField r fid
        fi <- find (\ fi -> qualEq (fi_id fi) qtid) fis
        let (_ :>: Forall ks (_ :=> fty)) = fi_assump fi
        (sty, resty) <- splitArrow1 fty
        tsub <- tgenMatch sty colty
        let vec = instVector ks tsub
        return (normalizeType normTy (Pred.inst vec resty))

-- Instantiate the variables fixed by the result/containing struct, while
-- leaving field-local quantified variables recognizable as opaque TGen nodes.
-- Requiring every quantified variable to occur in the result used to make one
-- higher-rank field discard the entire enclosing constructor.
instVector :: [Kind] -> M.Map Int Type -> [Type]
instVector ks tsub =
    [ M.findWithDefault (TGen noPosition n) n tsub
    | n <- [0 .. length ks - 1] ]

-- | Split exactly one (the outermost) arrow of a type.
splitArrow1 :: Type -> Maybe (Type, Type)
splitArrow1 (TAp (TAp (TCon arr@(TyCon _ _ _)) a) rt)
    | isTConArrow arr = Just (a, rt)
splitArrow1 _ = Nothing

-- | Match a scheme body type (containing TGen) against a concrete type,
-- returning the TGen instantiation.
tgenMatch :: Type -> Type -> Maybe (M.Map Int Type)
tgenMatch (TGen _ n) t = Just (M.singleton n t)
tgenMatch (TAp f a) (TAp f' a') = do
    m1 <- tgenMatch f f'
    m2 <- tgenMatch a a'
    mergeMatch m1 m2
tgenMatch (TCon c) (TCon c') | c == c' = Just M.empty
tgenMatch (TVar v) (TVar v') | v == v' = Just M.empty
tgenMatch _ _ = Nothing

mergeMatch :: M.Map Int Type -> M.Map Int Type -> Maybe (M.Map Int Type)
mergeMatch m1 m2 =
    let both = M.intersectionWith (==) m1 m2
    in  if and (M.elems both) then Just (M.union m1 m2) else Nothing

-- ---------------------------------------------------------------------------
-- Pattern normalization

-- Normalizing yields Nothing only for an impossible/inconsistent shape, such
-- as a literal outside the finite target domain.  Valid patterns whose match
-- set is not statically knowable become NUnknown, so they suppress conclusions
-- only locally instead of abandoning unrelated constructor diagnostics.
normPat :: NormType -> SymTab -> CType -> CPat -> Maybe NPat
normPat normTy r t p =
    case p of
      CPVar _ -> Just NWild
      CPAny _ -> Just NWild
      CPAs _ p' -> normPat normTy r t p'
      CPMixedLit _ base chunks ->
          case bitColumnWidth normTy t of
            Just width -> NMask <$> mixedLitCube width base chunks
            Nothing -> Just NUnknown
      CPLit (CLiteral _ l) ->
          case colKind normTy r t of
            CKLit dom -> do
                sourceKey <- litKey l
                case canonicalLitKey normTy t sourceKey of
                  Nothing -> Just NUnknown
                  Just k ->
                    -- a literal outside the column's finite domain (a
                    -- typecheck-time literal is only range-checked at
                    -- elaboration) would break completeness analysis
                    case (k, dom) of
                      (LKInt v, LDFinite lo hi) | v < lo || v > hi -> Nothing
                      _ -> Just (NLit k)
            _ -> Just NUnknown
      CPCon1 _ c p' -> normPat normTy r t (CPCon c [p'])
      CPCon c [p1, p2] | c `qualEq` idComma ->
          -- tuple pattern; the column type is (a possibly nested) PrimPair
          case colKind normTy r t of
            CKStruct sd | [t1, t2] <- nc_argTypes sd -> do
                n1 <- normPat normTy r t1 p1
                n2 <- normPat normTy r t2 p2
                Just (NCon sd [n1, n2])
            CKOpaque -> Just NUnknown
            _ -> Nothing
      CPCon c ps ->
          case colKind normTy r t of
            CKData qtid cons -> do
                cd <- resolveCon r qtid cons c
                payloadTy <- case nc_argTypes cd of
                               [pt] -> Just pt
                               _ -> Nothing
                np <- case ps of
                        [] -> Just NWild
                        [q] -> normPat normTy r payloadTy q
                        _ -> -- positional multi-argument pattern: the
                             -- payload is an anonymous-field SDataCon
                             -- struct
                             normPat normTy r payloadTy
                               (CPstruct (Just True) c (zip tupleIds ps))
                Just (NCon cd [np])
            CKOpaque -> Just NUnknown
            _ -> Nothing
      CPstruct _ c ips ->
          case colKind normTy r t of
            CKStruct sd -> do
                nps <- normFields normTy r sd ips
                Just (NCon sd nps)
            CKData qtid cons -> do
                -- a constructor with named fields, written in struct form
                cd <- resolveCon r qtid cons c
                payloadTy <- case nc_argTypes cd of
                               [pt] -> Just pt
                               _ -> Nothing
                np <- case colKind normTy r payloadTy of
                        CKStruct psd -> do
                            nps <- normFields normTy r psd ips
                            Just (NCon psd nps)
                        _ -> Nothing
                Just (NCon cd [np])
            CKOpaque -> Just NUnknown
            _ -> Nothing
      _ -> Just NUnknown

-- ---------------------------------------------------------------------------
-- Typed source/sidecar zipper

-- Build a constructor descriptor from the exact constructor instance emitted
-- by pattern typing.  Unlike 'conDesc', this does not recover instantiations by
-- matching a scheme result against the column type: CPConTs already records
-- the instantiation vector chosen by the typechecker.
typedCoverageCon :: NormType -> SymTab -> Id -> Id -> [CType]
                 -> Maybe CoverageCon
typedCoverageCon normTy r typeId conId instTypes = do
    cis <- findCon r conId
    ci <- find (\ x -> qualEq (ci_id x) typeId) cis
    let assump@(_ :>: sc@(Forall ks (qs :=> cty))) = ci_assump ci
    if length ks /= length instTypes
      then Nothing
      else do
        (argty, resty) <- splitArrow1 (Pred.inst instTypes cty)
        let qtid = ci_id ci
            cname = case assump of (i :>: _) -> i
            isEnum = case findType r qtid of
                       Just (TypeInfo _ _ _ (TIdata _ b) _) -> b
                       _ -> False
            refinement = emptyCoverageRefinement {
                cvr_required = Pred.inst instTypes qs }
        return CoverageCon {
            cc_typeId = qtid,
            cc_name = cname,
            cc_conNo = conNo (ci_taginfo ci),
            cc_isStruct = False,
            cc_isEnum = isEnum,
            cc_argTypes = [normalizeType normTy argty],
            cc_fieldIds = [],
            cc_scheme = Just sc,
            cc_instTypes = map (normalizeType normTy) instTypes,
            cc_resultType = normalizeType normTy resty,
            cc_refinement = refinement }

coverageStructCon :: CType -> NConDesc -> CoverageCon
coverageStructCon resultTy sd = CoverageCon {
    cc_typeId = nc_typeId sd,
    cc_name = nc_name sd,
    cc_conNo = nc_conNo sd,
    cc_isStruct = True,
    cc_isEnum = False,
    cc_argTypes = nc_argTypes sd,
    cc_fieldIds = nc_fieldIds sd,
    cc_scheme = Nothing,
    cc_instTypes = [],
    cc_resultType = resultTy,
    cc_refinement = emptyCoverageRefinement }

-- Pair one untouched source pattern with the pattern emitted by TCPat.  The
-- result is total: a mismatch becomes a local opaque node, preserving useful
-- facts in the enclosing constructor rather than abandoning the whole match.
buildCoveragePat :: NormType -> SymTab -> CoveragePlace -> CType
                 -> CPat -> CPat -> (CoveragePat, [CoverageBinder])
buildCoveragePat normTy r place ty source typed =
    case source of
      CPVar i ->
          (CoverageWild pos place nty,
           [CoverageBinder i place nty (getPosition i)])
      CPAny _ -> (CoverageWild pos place nty, [])
      CPAs i p ->
          case typed of
            CPAs _ p' ->
                let (cp, bs) = buildCoveragePat normTy r place nty p p'
                in  (cp, CoverageBinder i place nty (getPosition i) : bs)
            _ -> (CoverageOpaque pos place nty,
                  [CoverageBinder i place nty (getPosition i)])
      CPLit l -> (CoverageLitPat pos place nty (CoveragePositive l), [])
      CPMixedLit p base chunks ->
          (CoverageMaskPat p place nty base chunks, [])
      CPCon c [p1, p2] | c `qualEq` idComma ->
          case typed of
            CPstruct _ _ typedFields ->
                buildCoverageStruct normTy r place nty pos
                    [(tupleIds !! 0, p1), (tupleIds !! 1, p2)] typedFields
            _ -> opaque
      CPCon _ ps -> buildCoverageConstructor ps
      CPCon1 _ _ p -> buildCoverageConstructor [p]
      CPstruct _ _ fields ->
          case typed of
            CPstruct _ _ typedFields ->
                buildCoverageStruct normTy r place nty pos fields typedFields
            CPConTs ti ci instTypes [typedPayload] ->
                -- Constructor-with-named-fields syntax: the source struct is
                -- the constructor payload, not the outer tagged-union node.
                buildOuterConstructor ti ci instTypes source typedPayload
            _ -> opaque
      _ -> opaque
  where
    pos = getPosition source
    nty = normalizeType normTy ty
    opaque = (CoverageOpaque pos place nty, [])

    buildCoverageConstructor sourceArgs =
        case typed of
          CPConTs ti ci instTypes [typedPayload] ->
              let sourcePayload =
                    case sourceArgs of
                      [] -> CPstruct (Just True) idPrimUnit []
                      [p] -> p
                      ps -> CPstruct (Just True) ti (zip tupleIds ps)
              in  buildOuterConstructor ti ci instTypes
                                        sourcePayload typedPayload
          _ -> opaque

    buildOuterConstructor ti ci instTypes sourcePayload typedPayload =
        case typedCoverageCon normTy r ti ci instTypes of
          Just cc | [payloadTy] <- cc_argTypes cc ->
            let payloadPlace = projectCoveragePlace place
                                   (CoverageConArg (cc_name cc) 0)
                (payload, bs) = buildCoveragePat normTy r payloadPlace
                                                 payloadTy sourcePayload
                                                 typedPayload
            in  (CoverageConPat pos place nty cc [payload], bs)
          _ -> opaque

-- Pair the explicitly mentioned fields by their resolved typed Id, and
-- synthesize wildcards for omitted fields.
buildCoverageStruct :: NormType -> SymTab -> CoveragePlace -> CType -> Position
                    -> [(Id, CPat)] -> [(Id, CPat)]
                    -> (CoveragePat, [CoverageBinder])
buildCoverageStruct normTy r place ty pos sourceFields typedFields =
    case colKind normTy r ty of
      CKStruct sd | length sourceFields == length typedFields ->
          let cc = coverageStructCon ty sd
              supplied = zipWith pairField sourceFields typedFields
              children = map (buildField supplied)
                             (zip (nc_fieldIds sd) (nc_argTypes sd))
              (ps, bss) = unzip children
          in  (CoverageConPat pos place ty cc ps, concat bss)
      _ -> (CoverageOpaque pos place ty, [])
  where
    pairField (_, sourcePat) (typedId, typedPat) =
        (typedId, sourcePat, typedPat)
    sameField f1 f2 = f1 `qualEq` f2 || unQualId f1 == unQualId f2
    buildField supplied (fieldId, fieldTy) =
        let fieldPlace = projectCoveragePlace place (CoverageField fieldId)
        in  case find (\ (typedId, _, _) -> sameField typedId fieldId)
                      supplied of
              Just (_, sourcePat, typedPat) ->
                  buildCoveragePat normTy r fieldPlace fieldTy
                                   sourcePat typedPat
              Nothing ->
                  (CoverageWild noPosition fieldPlace fieldTy, [])

coverageNConDesc :: CoverageCon -> NConDesc
coverageNConDesc cc = NConDesc {
    nc_typeId = cc_typeId cc,
    nc_name = cc_name cc,
    nc_conNo = cc_conNo cc,
    nc_isStruct = cc_isStruct cc,
    nc_isEnum = cc_isEnum cc,
    nc_argTypes = cc_argTypes cc,
    nc_fieldIds = cc_fieldIds cc }

normCoveragePat :: NormType -> SymTab -> CoveragePat -> Maybe NPat
normCoveragePat normTy r cp =
    case cp of
      CoverageWild {} -> Just NWild
      CoverageOpaque {} -> Just NUnknown
      CoverageConPat _ _ _ cc ps ->
          NCon (coverageNConDesc cc) <$> mapM (normCoveragePat normTy r) ps
      CoverageLitPat _ _ t (CoveragePositive l) ->
          normPat normTy r t (CPLit l)
      CoverageMaskPat p _ t base chunks ->
          normPat normTy r t (CPMixedLit p base chunks)

-- ---------------------------------------------------------------------------
-- Typed guard formulas

guardAnd :: CoverageGuard -> CoverageGuard -> CoverageGuard
guardAnd CoverageGuardFalse _ = CoverageGuardFalse
guardAnd _ CoverageGuardFalse = CoverageGuardFalse
guardAnd CoverageGuardTrue b = b
guardAnd a CoverageGuardTrue = a
guardAnd CoverageGuardUnknown _ = CoverageGuardUnknown
guardAnd _ CoverageGuardUnknown = CoverageGuardUnknown
guardAnd a b | a == b = a
guardAnd a b = CoverageGuardAnd a b

guardOr :: CoverageGuard -> CoverageGuard -> CoverageGuard
guardOr CoverageGuardTrue _ = CoverageGuardTrue
guardOr _ CoverageGuardTrue = CoverageGuardTrue
guardOr CoverageGuardFalse b = b
guardOr a CoverageGuardFalse = a
guardOr CoverageGuardUnknown _ = CoverageGuardUnknown
guardOr _ CoverageGuardUnknown = CoverageGuardUnknown
guardOr a b | a == b = a
guardOr a b = CoverageGuardOr a b

guardNot :: CoverageGuard -> CoverageGuard
guardNot CoverageGuardTrue = CoverageGuardFalse
guardNot CoverageGuardFalse = CoverageGuardTrue
guardNot CoverageGuardUnknown = CoverageGuardUnknown
guardNot (CoverageGuardNot g) = g
guardNot g = CoverageGuardNot g

buildCoverageGuard :: NormType -> [CoverageBinder] -> [CQual] -> [[CQual]]
                   -> CoverageGuard
buildCoverageGuard normTy binders sourceQuals typedGroups
    | length sourceQuals /= length typedGroups = CoverageGuardUnknown
    | otherwise = foldl guardAnd CoverageGuardTrue
                        (zipWith guardQual sourceQuals typedGroups)
  where
    binderMap = M.fromList [(cb_id b, b) | b <- binders]

    guardQual (CQFilter _) [CQFilter e] = guardExpr e
    -- A generator can fail and brings new binders into scope.  Until the
    -- coverage IR has a term/pattern-guard oracle, keep the whole row opaque.
    guardQual (CQGen {}) _ = CoverageGuardUnknown
    guardQual _ _ = CoverageGuardUnknown

    guardExpr (CHasType e _) = guardExpr e
    guardExpr (CConT ti ci _)
      | ti == idBool && ci == idTrue = CoverageGuardTrue
      | ti == idBool && ci == idFalse = CoverageGuardFalse
    guardExpr (CCon0 (Just ti) ci)
      | ti == idBool && ci == idTrue = CoverageGuardTrue
      | ti == idBool && ci == idFalse = CoverageGuardFalse
    guardExpr (CVar i) =
      case M.lookup i binderMap of
        Just b | isBoolType (cb_type b) ->
          CoverageGuardAtom (CoveragePatternPlace (cb_place b))
        Just _ -> CoverageGuardUnknown
        -- The enclosing typed filter (or one of the exact Prelude Boolean
        -- operators recognized below) establishes that this leaf is Bool.
        -- Its resolved Id is stable across rows in this obligation.
        Nothing -> CoverageGuardAtom (CoverageFreeVariable i)
    guardExpr (CApply (CVar i) [e])
      | i == idNot = guardNot (guardExpr e)
    guardExpr (CApply (CVar i) [e1, e2])
      | i == idAnd = guardAnd (guardExpr e1) (guardExpr e2)
      | i == idOrAt noPosition = guardOr (guardExpr e1) (guardExpr e2)
    -- Accept the curried form defensively; current typed dumps use the
    -- multi-argument form above.
    guardExpr (CApply (CApply (CVar i) [e1]) [e2])
      | i == idAnd = guardAnd (guardExpr e1) (guardExpr e2)
      | i == idOrAt noPosition = guardOr (guardExpr e1) (guardExpr e2)
    guardExpr (CBinOp e1 i e2)
      | i == idAnd = guardAnd (guardExpr e1) (guardExpr e2)
      | i == idOrAt noPosition = guardOr (guardExpr e1) (guardExpr e2)
    guardExpr _ = CoverageGuardUnknown

    isBoolType t =
      case leftCon (normalizeType normTy t) of
        Just i -> i == idBool
        Nothing -> False

type GuardCube = M.Map CoverageGuardAtom Bool

guardFormulaAtoms :: CoverageGuard -> S.Set CoverageGuardAtom
guardFormulaAtoms CoverageGuardTrue = S.empty
guardFormulaAtoms CoverageGuardFalse = S.empty
guardFormulaAtoms (CoverageGuardAtom a) = S.singleton a
guardFormulaAtoms (CoverageGuardNot g) = guardFormulaAtoms g
guardFormulaAtoms (CoverageGuardAnd a b) =
    S.union (guardFormulaAtoms a) (guardFormulaAtoms b)
guardFormulaAtoms (CoverageGuardOr a b) =
    S.union (guardFormulaAtoms a) (guardFormulaAtoms b)
guardFormulaAtoms CoverageGuardUnknown = S.empty

-- Convert a supported formula to a reduced union of Boolean cubes.  The Bool
-- parameter is the desired polarity, which pushes negation inward without an
-- intermediate formula.  Every syntax node and cartesian-product pair is
-- charged to the same user-configurable fuel as the matrix analysis.
guardCubes :: Fuel -> CoverageGuard -> (Fuel, Maybe [GuardCube])
guardCubes fuel CoverageGuardUnknown = (fuel, Nothing)
guardCubes fuel g = dnf fuel True g
  where
    dnf f _ _ | f <= 0 = (f, Nothing)
    dnf f polarity formula =
      let f' = f - 1
      in case formula of
           CoverageGuardTrue ->
             (f', Just (if polarity then [M.empty] else []))
           CoverageGuardFalse ->
             (f', Just (if polarity then [] else [M.empty]))
           CoverageGuardAtom a ->
             (f', Just [M.singleton a polarity])
           CoverageGuardNot x -> dnf f' (not polarity) x
           CoverageGuardAnd a b
             | polarity -> productDNF f' a True b True
             | otherwise -> unionDNF f' a False b False
           CoverageGuardOr a b
             | polarity -> unionDNF f' a True b True
             | otherwise -> productDNF f' a False b False
           CoverageGuardUnknown -> (f', Nothing)

    unionDNF f a pa b pb =
      case dnf f pa a of
        (f1, Nothing) -> (f1, Nothing)
        (f1, Just as) ->
          case dnf f1 pb b of
            (f2, Nothing) -> (f2, Nothing)
            (f2, Just bs) -> reduceCubes f2 (as ++ bs)

    productDNF f a pa b pb =
      case dnf f pa a of
        (f1, Nothing) -> (f1, Nothing)
        (f1, Just as) ->
          case dnf f1 pb b of
            (f2, Nothing) -> (f2, Nothing)
            (f2, Just bs) ->
              case cross f2 as bs [] of
                (f3, Nothing) -> (f3, Nothing)
                (f3, Just cs) -> reduceCubes f3 cs

    cross f [] _ acc = (f, Just (reverse acc))
    cross f _ _ _ | f <= 0 = (f, Nothing)
    cross f (a:as) bs acc =
      case crossOne f a bs acc of
        (f1, Nothing) -> (f1, Nothing)
        (f1, Just acc') -> cross f1 as bs acc'

    crossOne f _ [] acc = (f, Just acc)
    crossOne f _ _ _ | f <= 0 = (f, Nothing)
    crossOne f a (b:bs) acc =
      case mergeCube f a b of
        (f', Nothing) -> (f', Nothing)
        (f', Just Nothing) -> crossOne f' a bs acc
        (f', Just (Just c)) -> crossOne f' a bs (c : acc)

-- The outer Maybe reports fuel exhaustion; the inner Maybe reports two
-- incompatible cubes.  Iterate over the smaller map, charging the pair and
-- every lookup/insertion, so growing conjunctions cannot hide quadratic work
-- behind a single charge per cartesian-product pair.
mergeCube :: Fuel -> GuardCube -> GuardCube
          -> (Fuel, Maybe (Maybe GuardCube))
mergeCube fuel _ _ | fuel <= 0 = (fuel, Nothing)
mergeCube fuel a b = mergeAtoms (fuel - 1) (M.toList smaller) larger
  where
    (smaller, larger)
      | M.size a <= M.size b = (a, b)
      | otherwise = (b, a)

    mergeAtoms f [] merged = (f, Just (Just merged))
    mergeAtoms f _ _ | f <= 0 = (f, Nothing)
    mergeAtoms f ((atom, value):atoms) merged =
      let f' = f - 1
      in case M.lookup atom merged of
           Just value' | value /= value' -> (f', Just Nothing)
           Just _ -> mergeAtoms f' atoms merged
           Nothing -> mergeAtoms f' atoms (M.insert atom value merged)

-- Reduce a union of cubes incrementally.  Besides charging each input cube,
-- every subsumption comparison and every atom lookup consumes fuel.  This is
-- important because repeated reduction of a growing disjunction is otherwise
-- quadratic (or worse across a nested formula) despite a small syntax tree.
reduceCubes :: Fuel -> [GuardCube] -> (Fuel, Maybe [GuardCube])
reduceCubes fuel cubes = go fuel [] cubes
  where
    go f acc [] = (f, Just acc)
    go f _ _ | f <= 0 = (f, Nothing)
    go f acc (cube:cubes') =
      case insertCube (f - 1) cube acc [] of
        (f', Nothing) -> (f', Nothing)
        (f', Just acc') -> go f' acc' cubes'

    -- The cubes in @kept@ precede those still in @rest@.  If an existing
    -- cube subsumes the candidate, cubes already removed by the candidate
    -- are also subsumed by that existing cube (by transitivity), so the
    -- partial filtering remains valid.
    insertCube f cube [] kept = (f, Just (reverse kept ++ [cube]))
    insertCube f _ _ _ | f <= 0 = (f, Nothing)
    insertCube f cube rest@(existing:existing') kept =
      case cubeSubsumes f existing cube of
        (f1, Nothing) -> (f1, Nothing)
        (f1, Just True) -> (f1, Just (reverse kept ++ rest))
        (f1, Just False) ->
          case cubeSubsumes f1 cube existing of
            (f2, Nothing) -> (f2, Nothing)
            (f2, Just True) -> insertCube f2 cube existing' kept
            (f2, Just False) ->
              insertCube f2 cube existing' (existing:kept)

    cubeSubsumes f _ _ | f <= 0 = (f, Nothing)
    cubeSubsumes f general specific =
      checkAtoms (f - 1) (M.toList general)
      where
        checkAtoms f' [] = (f', Just True)
        checkAtoms f' _ | f' <= 0 = (f', Nothing)
        checkAtoms f' ((atom, value):atoms) =
          let f'' = f' - 1
          in if M.lookup atom specific == Just value
             then checkAtoms f'' atoms
             else (f'', Just False)

guardCubePats :: [CoverageGuardAtom] -> GuardCube -> [NPat]
guardCubePats atoms cube = map atAtom atoms
  where
    atAtom a = case M.lookup a cube of
                 Nothing -> NWild
                 Just False -> NLit (LKInt 0)
                 Just True -> NLit (LKInt 1)

-- A guard atom that denotes a pattern place is not independent of the
-- patterns: it IS the value at that place.  Modeling it as an extra matrix
-- column would let the analysis assign the atom and the place's pattern
-- inconsistently and report impossible uncovered witnesses.  Instead, each
-- cube's pattern-place assignments are folded into the row's own pattern
-- vector, so the guard constrains the same column the patterns constrain.
-- Only free-variable atoms become extra (shared) Bit#(1) columns.

-- | The Bool constructor matched when the atom has the given value.
boolConDesc :: NormType -> SymTab -> Bool -> Maybe NConDesc
boolConDesc normTy r v =
    case colKind normTy r tBool of
      CKData _ cds -> find (\ cd -> nc_name cd `qualEq` name) cds
      _ -> Nothing
  where name = if v then idTrue else idFalse

-- | Constrain the pattern vector at a place to a Boolean value.
-- @Nothing@: the place cannot be navigated (e.g. it is inside an opaque
-- node), so the enclosing guard must be treated as unknown; this is
-- independent of the value, letting callers pre-check with either one.
-- @Just Nothing@: the assignment contradicts the pattern (an empty match
-- set).  @Just (Just ps)@: the constrained vector.
foldPlaceAtom :: NormType -> SymTab -> [NPat] -> CoveragePlace -> Bool
              -> Maybe (Maybe [NPat])
foldPlaceAtom normTy r ps0 (CoveragePlace col projs) v = atCol col ps0
  where
    atCol _ [] = Nothing
    atCol 0 (p : rest) = fmap (fmap (: rest)) (atPat projs p)
    atCol n (p : rest) = fmap (fmap (p :)) (atCol (n - 1) rest)

    atPat [] p = leaf p
    atPat (proj : rest) (NCon cd args) = do
        n <- projIndex cd proj
        arg <- indexMaybe args n
        let rewrap arg' = NCon cd (take n args ++ arg' : drop (n + 1) args)
        fmap (fmap rewrap) (atPat rest arg)
    atPat _ _ = Nothing

    projIndex cd (CoverageConArg cn n) | nc_name cd `qualEq` cn = Just n
    projIndex cd (CoverageField f) =
        findIndex (\ fid -> fid `qualEq` f || unQualId fid == unQualId f)
                  (nc_fieldIds cd)
    projIndex _ _ = Nothing

    indexMaybe xs n | n >= 0, (x : _) <- drop n xs = Just x
                    | otherwise = Nothing

    -- The binder was typed Bool (buildCoverageGuard checks), so the leaf
    -- is a wildcard or a Bool constructor pattern (via an as-pattern).
    leaf NWild = do
        cd <- boolConDesc normTy r v
        Just (Just (NCon cd [NWild]))
    leaf p@(NCon cd _)
      | nc_typeId cd `qualEq` idBool =
          if nc_name cd `qualEq` (if v then idTrue else idFalse)
          then Just (Just p)
          else Just Nothing
    leaf _ = Nothing

-- Mixed literals are implemented by rmPatLit as equality tests on their known
-- bit slices.  The rightmost source chunk is at bit zero; wildcard chunks and
-- bits above the textual literal impose no constraint.
mixedLitCube :: Integer -> Integer -> [(Integer, Maybe Integer)]
             -> Maybe BitCube
mixedLitCube colWidth base chunks = do
    bitsPerDigit <- case base of
                      2 -> Just 1
                      8 -> Just 3
                      16 -> Just 4
                      _ -> Nothing
    (textWidth, mask, value) <- foldr (addChunk bitsPerDigit)
                                     (Just (0, 0, 0)) chunks
    if textWidth <= colWidth
       then Just (BitCube colWidth mask value)
       else Nothing
  where
    addChunk bitsPerDigit (len, mbValue) acc = do
        (offset, mask, value) <- acc
        if len < 0 then Nothing else do
          let width = len * bitsPerDigit
              chunkMask = lowMask width
          case mbValue of
            Nothing -> Just (offset + width, mask, value)
            Just v | v >= 0 && v <= chunkMask ->
              Just (offset + width,
                    mask .|. (chunkMask * 2^offset),
                    value .|. (v * 2^offset))
            _ -> Nothing

bitColumnWidth :: NormType -> CType -> Maybe Integer
bitColumnWidth normTy t =
    case normalizeType normTy t of
      t' | Just tcid <- leftCon t', tcid `qualEq` idBit,
           [n] <- tyConArgs t', isTNum n,
           let width = getTNum n,
           width >= 0, width <= 65536 -> Just width
      _ -> Nothing

canonicalLitKey :: NormType -> CType -> LitKey -> Maybe LitKey
canonicalLitKey normTy t k =
    case leftCon (normalizeType normTy t) of
      Nothing -> Nothing
      Just tcid ->
          case k of
            LKInt v
              | any (qualEq tcid) [idBit, idUInt, idInt, idInteger] -> Just k
              | tcid `qualEq` idReal -> Just (LKReal (fromInteger v))
            LKReal _ | tcid `qualEq` idReal -> Just k
            LKChar _ | tcid `qualEq` idChar -> Just k
            -- BSV spells Char patterns with a string literal.  The closed
            -- Prelude instance reduces primStringToChar only for singletons;
            -- all other strings are rejected during elaboration.
            LKStr [c] | tcid `qualEq` idChar -> Just (LKChar c)
            LKStr _ | tcid `qualEq` stringId -> Just k
            _ -> Nothing
  where
    stringId = case tString of
                 TCon (TyCon i _ _) -> i
                 _ -> idInteger

lowMask :: Integer -> Integer
lowMask width
    | width <= 0 = 0
    | otherwise = bit (fromInteger width) - 1

normFields :: NormType -> SymTab -> NConDesc -> [(Id, CPat)] -> Maybe [NPat]
normFields normTy r sd ips
    -- reject patterns naming fields the struct does not have
    -- (the typechecker rejects them too; this is belt and braces)
    | any (isNothing . structFieldOf . fst) ips = Nothing
    | otherwise = mapM normField (zip (nc_fieldIds sd) (nc_argTypes sd))
  where
    sameField f1 f2 = f1 `qualEq` f2 || unQualId f1 == unQualId f2
    structFieldOf i = find (sameField i) (nc_fieldIds sd)
    normField (fid, fty) =
        case find (sameField fid . fst) ips of
          Just (_, p) -> normPat normTy r fty p
          Nothing -> Just NWild

resolveCon :: SymTab -> Id -> [NConDesc] -> Id -> Maybe NConDesc
resolveCon r qtid cons c = do
    cis <- findCon r c
    ci <- find (\ ci -> qualEq (ci_id ci) qtid) cis
    find (\ cd -> nc_conNo cd == conNo (ci_taginfo ci)) cons

hasUnknown :: NPat -> Bool
hasUnknown NUnknown = True
hasUnknown (NCon _ ps) = any hasUnknown ps
hasUnknown _ = False

-- Exhaustiveness uses an over-approximation of unknown patterns.  Redundancy
-- uses the same view for the row being queried, but never uses an unknown row
-- as a premise (see 'prior'' below).
unknownAsWild :: NPat -> NPat
unknownAsWild NUnknown = NWild
unknownAsWild (NCon cd ps) = NCon cd (map unknownAsWild ps)
unknownAsWild p = p

litKey :: Literal -> Maybe LitKey
litKey (LInt il) = Just (LKInt (ilValue il))
litKey (LString s) = Just (LKStr s)
litKey (LChar c) = Just (LKChar c)
litKey (LReal d) = Just (LKReal d)
litKey LPosition = Nothing

-- ---------------------------------------------------------------------------
-- The usefulness analysis

-- Bound on the total number of matrix operations, so that pathological
-- matches degrade to "no warning" instead of blowing up compile time
-- (the analysis is exponential in the worst case, as in GHC).
type Fuel = Int

-- The analysis state: the remaining fuel, plus a cache of column kinds.
-- 'colKind' resolves every constructor of the column type against the
-- symbol table; the matrix analysis asks about the same column types over
-- and over across specialization steps, so the resolution is memoized per
-- (un-normalized) column type for the duration of one warning class.
data St = St { stFuel :: !Fuel, stKinds :: M.Map CType ColKind }

mkSt :: Fuel -> St
mkSt fuel = St { stFuel = fuel, stKinds = M.empty }

spend :: St -> St
spend st = st { stFuel = stFuel st - 1 }

stColKind :: NormType -> SymTab -> St -> CType -> (St, ColKind)
stColKind normTy r st t =
    case M.lookup t (stKinds st) of
      Just ck -> (st, ck)
      Nothing -> let ck = colKind normTy r t
                 in  (st { stKinds = M.insert t ck (stKinds st) }, ck)

-- maximum number of example patterns reported for a non-exhaustive match
maxWitnesses :: Int
maxWitnesses = 4

-- maximum number of literals listed in a "not one of" witness
maxLitsListed :: Int
maxLitsListed = 8

-- | Witness patterns for uncovered values
data WPat = WWild
          | WCon NConDesc [WPat]
          | WLit LitKey
          | WLitOther [LitKey]  -- ^ any value other than these

headCons :: [[NPat]] -> [NConDesc]
headCons rows = nubByConNo [cd | (NCon cd _ : _) <- rows]
  where nubByConNo = foldr (\ cd acc ->
                             if any (\ cd' -> nc_conNo cd' == nc_conNo cd) acc
                             then acc else cd : acc) []

headLits :: [[NPat]] -> [LitKey]
headLits rows = nub [k | (NLit k : _) <- rows]

specCon :: NConDesc -> [[NPat]] -> [[NPat]]
specCon cd rows =
    [ args ++ ps | (NCon cd' args : ps) <- rows, nc_conNo cd' == nc_conNo cd ]
    ++ [ replicate arity NWild ++ ps | (NWild : ps) <- rows ]
  where arity = length (nc_argTypes cd)

specLit :: LitKey -> [[NPat]] -> [[NPat]]
specLit k rows =
    [ ps | (NLit k' : ps) <- rows, k' == k ]
    ++ [ ps | (NWild : ps) <- rows ]

defaultMat :: [[NPat]] -> [[NPat]]
defaultMat rows = [ ps | (NWild : ps) <- rows ]

litDomainSize :: LitDomain -> Maybe Integer
litDomainSize (LDFinite lo hi) = Just (hi - lo + 1)
litDomainSize LDInfinite = Nothing

domainCube :: LitDomain -> Maybe BitCube
domainCube dom@(LDFinite _ _) = do
    size <- litDomainSize dom
    width <- exactLog2 size
    return (BitCube width 0 0)
domainCube LDInfinite = Nothing

-- Finite numeric domains here always have power-of-two size.  Refuse any
-- future domain that does not, rather than silently applying bit-cube logic.
exactLog2 :: Integer -> Maybe Integer
exactLog2 n
    | n <= 0 = Nothing
    | otherwise = go 0 n
  where
    go width 1 = Just width
    go width value
      | even value = go (width + 1) (value `div` 2)
      | otherwise = Nothing

patCube :: LitDomain -> NPat -> Maybe BitCube
patCube dom pat = do
    universe <- domainCube dom
    case (dom, pat) of
      (_, NWild) -> Just universe
      (_, NUnknown) -> Just universe
      (LDFinite lo hi, NLit (LKInt v))
        | v >= lo, v <= hi ->
            Just (BitCube (bc_width universe) (lowMask (bc_width universe))
                          (v - lo))
      (LDFinite lo hi, NMask cube)
        | lo == 0, hi + 1 == 2^(bc_width cube),
          bc_width cube == bc_width universe -> Just cube
      _ -> Nothing

cubeIntersects :: BitCube -> BitCube -> Bool
cubeIntersects a b =
    bc_width a == bc_width b &&
    (((bc_value a `xor` bc_value b) .&. (bc_mask a .&. bc_mask b)) == 0)

-- Every point of the second cube is in the first cube.
cubeContains :: BitCube -> BitCube -> Bool
cubeContains outer inner =
    bc_width outer == bc_width inner &&
    (bc_mask outer .&. bc_mask inner) == bc_mask outer &&
    (((bc_value outer `xor` bc_value inner) .&. bc_mask outer) == 0)

-- If cut partially overlaps cell, this returns a bit constrained by cut but
-- not cell.  Splitting cell there makes progress toward a uniform membership
-- decision for cut.
cubeSplitBit :: BitCube -> BitCube -> Maybe Int
cubeSplitBit cut cell
    | not (cubeIntersects cut cell) = Nothing
    | cubeContains cut cell = Nothing
    | otherwise = leastSetBit (bc_mask cut .&. complement (bc_mask cell))
  where
    leastSetBit value
      | value <= 0 = Nothing
      | otherwise = Just (go 0 value)
    go n value
      | testBit value 0 = n
      | otherwise = go (n + 1) (value `shiftR` 1)

splitCube :: Int -> BitCube -> (BitCube, BitCube)
splitCube n cube =
    (cube { bc_mask = setBit (bc_mask cube) n,
            bc_value = clearBit (bc_value cube) n },
     cube { bc_mask = setBit (bc_mask cube) n,
            bc_value = setBit (bc_value cube) n })

-- Partition root until every cut either contains or is disjoint from every
-- cell.  This is symbolic in the column width: only mask bits mentioned by
-- rows are split.  Each work-list node consumes fuel, bounding the otherwise
-- exponential number of overlap regions.
partitionCubes :: Fuel -> BitCube -> [BitCube]
               -> (Fuel, Maybe [BitCube])
partitionCubes fuel root cuts = go fuel [root] []
  where
    uniqueCuts = S.toList (S.fromList cuts)
    go f _ _ | f <= 0 = (f, Nothing)
    go f [] acc = (f, Just (reverse acc))
    go f (cell : work) acc =
        case firstSplit (f - 1) cell uniqueCuts of
          (f', Nothing) -> (f', Nothing)
          (f', Just Nothing) -> go f' work (cell : acc)
          (f', Just (Just n)) ->
              let (zero, one) = splitCube n cell
              in  go f' (zero : one : work) acc
    -- The outer Maybe is fuel exhaustion; the inner Maybe is whether a split
    -- was found.  Charge every cut comparison so fuel bounds cells * cuts.
    firstSplit f _ _ | f <= 0 = (f, Nothing)
    firstSplit f _ [] = (f, Just Nothing)
    firstSplit f cell (cut : rest) =
        case cubeSplitBit cut cell of
          Just n -> (f - 1, Just (Just n))
          Nothing -> firstSplit (f - 1) cell rest

cubeWitness :: LitDomain -> BitCube -> WPat
cubeWitness (LDFinite lo _) cube = WLit (LKInt (lo + bc_value cube))
cubeWitness LDInfinite _ = WWild

-- | Compute (up to maxWitnesses+1) examples of value vectors not covered by
-- the normalized pattern/guard rows.  Nothing means the fuel ran out.
uncovered :: NormType -> SymTab -> St -> [CType] -> [[NPat]]
          -> (St, Maybe [[WPat]])
uncovered normTy r st _ _ | stFuel st <= 0 = (st, Nothing)
uncovered normTy r st ts [] =
    -- nothing covers anything: everything is a witness
    (spend st, Just [replicate (length ts) WWild])
uncovered normTy r st [] rows = (spend st, Just [])
uncovered normTy r st0 (t : ts) rows =
    case stColKind normTy r st0 t of
      (st, CKData _ cons)
        | sigComplete cons -> perCon (spend st) cons []
        | otherwise ->
            -- witnesses with the missing constructors first, then keep
            -- looking inside the constructors that are present
            let missing = [ cd | cd <- cons, notPresent cd ]
                wits = if null present
                       then [WWild]
                       else [ WCon cd (replicate (length (nc_argTypes cd)) WWild)
                            | cd <- missing ]
            in  thenPerCon (prefixDefault (spend st) wits) present
      (st, CKStruct sd) -> perCon (spend st) [sd] []
      (st, CKLit dom@(LDFinite _ _)) -> uncoveredFinite (spend st) dom
      (st, CKLit LDInfinite) ->
          let lits = headLits rows
              wit = if null lits then WWild else WLitOther lits
          in  thenPerLit (prefixDefault (spend st) [wit]) lits
      (st, CKOpaque)
        | all wildHead rows -> prefixDefault (spend st) [WWild]
        | otherwise -> (spend st, Just [])  -- can't analyze: assume covered
  where
    present = headCons rows
    notPresent cd = all (\ cd' -> nc_conNo cd' /= nc_conNo cd) present
    sigComplete cons = all (\ cd -> not (notPresent cd)) cons
    wildHead (NWild : _) = True
    wildHead _ = False
    cap ws = take (maxWitnesses + 1) ws
    -- specialize per constructor, wrapping witnesses back up
    perCon st [] acc = (st, Just (cap acc))
    perCon st (cd : cds) acc
      | length acc > maxWitnesses = (st, Just (cap acc))
      | otherwise =
        let arity = length (nc_argTypes cd)
            (st', mws) = uncovered normTy r st (nc_argTypes cd ++ ts)
                                          (specCon cd rows)
        in  case mws of
              Nothing -> (st', Nothing)
              Just ws ->
                  perCon st' cds
                      (acc ++ [ WCon cd (take arity w) : drop arity w
                              | w <- ws ])
    perLit st [] acc = (st, Just (cap acc))
    perLit st (k : ks) acc
      | length acc > maxWitnesses = (st, Just (cap acc))
      | otherwise =
        let (st', mws) = uncovered normTy r st ts (specLit k rows)
        in  case mws of
              Nothing -> (st', Nothing)
              Just ws -> perLit st' ks (acc ++ [ WLit k : w | w <- ws ])
    uncoveredFinite st dom =
        case (domainCube dom, mapM rowCube rows) of
          (Just universe, Just cubeRows) ->
              case partitionCubes (stFuel st) universe (map fst cubeRows) of
                (f', Nothing) -> (st { stFuel = f' }, Nothing)
                (f', Just cells) ->
                    perCell (st { stFuel = f' }) dom cubeRows cells []
          _ -> (st, Nothing)
      where
        rowCube (p : ps) = do cube <- patCube dom p; return (cube, ps)
        rowCube [] = Nothing
    perCell st _ _ [] acc = (st, Just (cap acc))
    perCell st dom cubeRows (cell : cells) acc
      | stFuel st <= 0 = (st, Nothing)
      | length acc > maxWitnesses = (st, Just (cap acc))
      | otherwise =
          let tails = [ ps | (cube, ps) <- cubeRows,
                             cubeContains cube cell ]
              (st', mws) = uncovered normTy r st ts tails
          in  case mws of
                Nothing -> (st', Nothing)
                Just ws ->
                    perCell st' dom cubeRows cells
                            (acc ++ [ cubeWitness dom cell : w | w <- ws ])
    prefixDefault st wits =
        let (st', mws) = uncovered normTy r st ts (defaultMat rows)
        in  case mws of
              Nothing -> (st', Nothing)
              Just ws -> (st', Just (cap [ wit : w | w <- ws, wit <- wits ]))
    thenPerCon (st, Nothing) _ = (st, Nothing)
    thenPerCon (st, Just acc) cds
      | length acc > maxWitnesses = (st, Just (cap acc))
      | otherwise = perCon st cds acc
    thenPerLit (st, Nothing) _ = (st, Nothing)
    thenPerLit (st, Just acc) ks
      | length acc > maxWitnesses = (st, Just (cap acc))
      | otherwise = perLit st ks acc

-- | Is the vector qs useful w.r.t. the rows (can it match something the
-- rows do not)?  Nothing means the fuel ran out.
useful :: NormType -> SymTab -> St -> [CType] -> [[NPat]] -> [NPat]
       -> (St, Maybe Bool)
useful normTy r st _ _ _ | stFuel st <= 0 = (st, Nothing)
useful normTy r st [] rows [] = (spend st, Just (null rows))
useful normTy r st [] _ _ = (spend st, Just True)  -- shape mismatch: be quiet
useful normTy r st _ _ [] = (spend st, Just True)
useful normTy r st0 (t : ts) rows (q : qs) =
    case q of
      NCon cd args ->
          useful normTy r (spend st0) (nc_argTypes cd ++ ts)
                 (specCon cd rows) (args ++ qs)
      NLit k ->
          case stColKind normTy r st0 t of
            (st, CKLit dom@(LDFinite _ _)) -> usefulFinite (spend st) dom
            (st, _) -> useful normTy r (spend st) ts (specLit k rows) qs
      NMask _ ->
          case stColKind normTy r st0 t of
            (st, CKLit dom@(LDFinite _ _)) -> usefulFinite (spend st) dom
            (st, _) -> (spend st, Nothing)
      NUnknown ->
          useful normTy r (spend st0) (t : ts) rows (NWild : qs)
      NWild ->
          case stColKind normTy r st0 t of
            (st, CKData _ cons)
              | sigComplete cons -> anyCon (spend st) cons
              | otherwise ->
                  useful normTy r (spend st) ts (defaultMat rows) qs
            (st, CKStruct sd) -> anyCon (spend st) [sd]
            (st, CKLit dom@(LDFinite _ _)) -> usefulFinite (spend st) dom
            (st, CKLit LDInfinite) ->
                useful normTy r (spend st) ts (defaultMat rows) qs
            (st, CKOpaque) -> useful normTy r (spend st) ts (defaultMat rows) qs
  where
    present = headCons rows
    sigComplete cons =
        all (\ cd -> any (\ cd' -> nc_conNo cd' == nc_conNo cd) present) cons
    anyCon st [] = (st, Just False)
    anyCon st (cd : cds) =
        let arity = length (nc_argTypes cd)
            (st', mu) = useful normTy r st (nc_argTypes cd ++ ts)
                               (specCon cd rows) (replicate arity NWild ++ qs)
        in  case mu of
              Nothing -> (st', Nothing)
              Just True -> (st', Just True)
              Just False -> anyCon st' cds
    usefulFinite st dom =
        case (patCube dom q, mapM rowCube rows) of
          (Just queryCube, Just cubeRows) ->
              case partitionCubes (stFuel st) queryCube (map fst cubeRows) of
                (f', Nothing) -> (st { stFuel = f' }, Nothing)
                (f', Just cells) ->
                    usefulCells (st { stFuel = f' }) cubeRows cells
          _ -> (st, Nothing)
      where
        rowCube (p : ps) = do cube <- patCube dom p; return (cube, ps)
        rowCube [] = Nothing
    usefulCells st _ [] = (st, Just False)
    usefulCells st _ _ | stFuel st <= 0 = (st, Nothing)
    usefulCells st cubeRows (cell : cells) =
        let tails = [ ps | (cube, ps) <- cubeRows,
                           cubeContains cube cell ]
            (st', mu) = useful normTy r st ts tails qs
        in  case mu of
              Nothing -> (st', Nothing)
              Just True -> (st', Just True)
              Just False -> usefulCells st' cubeRows cells

-- ---------------------------------------------------------------------------
-- Witness rendering
--
-- Hand-rolled rather than reusing the CPat pretty-printers, so that the
-- examples read like the patterns a user would actually write (no
-- "tagged" on enum tags in BSV, wildcards for omitted struct fields, etc.)

isAnonFields :: NConDesc -> Bool
isAnonFields cd =
    and (zipWith qualEq (nc_fieldIds cd) tupleIds)

isUnitType :: CType -> Bool
isUnitType t = case leftCon t of
                 Just i -> qualEq i idPrimUnit
                 Nothing -> False

isWWild :: WPat -> Bool
isWWild WWild = True
isWWild _ = False

showLitKey :: LitKey -> String
showLitKey (LKInt v) = show v
showLitKey (LKStr s) = show s
showLitKey (LKChar c) = show c
showLitKey (LKReal d) = show d

-- collapse right-nested pairs, so tuple witnesses print as (a, b, c)
tupleElems :: NConDesc -> [WPat] -> [WPat]
tupleElems cd [w1, w2]
    | nc_typeId cd `qualEq` idPrimPair =
        case w2 of
          WCon cd' ws' | nc_isStruct cd', nc_typeId cd' `qualEq` idPrimPair ->
              w1 : tupleElems cd' ws'
          _ -> [w1, w2]
tupleElems _ ws = ws

showWitness :: Bool -> [WPat] -> String
showWitness classic ws = unwords (map (showW False) ws)
  where
    wild = if classic then "_" else ".*"
    showW _ WWild = wild
    showW _ (WLit k) = showLitKey k
    showW _ (WLitOther ks) =
        let shown = map showLitKey (take maxLitsListed ks)
            more = if length ks > maxLitsListed then ", ..." else ""
        in  "v when v is not one of {" ++ intercalate ", " shown ++ more ++ "}"
    showW nested (WCon cd ws')
        | nc_isStruct cd = showStruct cd ws'
        | otherwise = showCon nested cd ws'
    conName cd = getIdBaseString (nc_name cd)
    -- a constructor and its single payload
    showCon nested cd ws' =
        let payload = case ws' of
                        [w] -> w
                        _ -> WWild
            noArg = case nc_argTypes cd of
                      [pt] -> isUnitType pt
                      _ -> True
            -- BSV writes union members as "tagged Name", but enum tags bare
            tag = if classic || nc_isEnum cd then "" else "tagged "
            body
              | noArg = tag ++ conName cd
              | otherwise =
                  case payload of
                    WCon sd fws | nc_isStruct sd, isAnonFields sd,
                                  not (null (nc_fieldIds sd)) ->
                        -- positional constructor arguments
                        tag ++ conName cd ++ " " ++
                        unwords (map (showW True) fws)
                    WCon sd fws | nc_isStruct sd,
                                  not (nc_typeId sd `qualEq` idPrimPair),
                                  not (null (nc_fieldIds sd)) ->
                        -- named fields
                        tag ++ conName cd ++ " " ++ showFields (fieldPats sd fws)
                    _ -> tag ++ conName cd ++ " " ++ showW True payload
            -- parenthesize nested constructor patterns: in Classic only
            -- when there is a payload, in BSV any nested "tagged"
            needParen = nested && (if classic then not noArg
                                   else not (null tag))
        in  if needParen then "(" ++ body ++ ")" else body
    showStruct cd ws'
      | nc_typeId cd `qualEq` idPrimPair =
          let es = tupleElems cd ws'
              open = if classic then "(" else "{"
              close = if classic then ")" else "}"
          in  open ++ intercalate ", " (map (showW False) es) ++ close
      | null (nc_fieldIds cd) = if classic then "()" else "{}"
      | otherwise =
          getIdBaseString (nc_typeId cd) ++ " " ++
          showFields (fieldPats cd ws')
    -- only show the interesting (non-wildcard) fields, unless that
    -- would show nothing at all
    fieldPats cd ws' =
        let named = zip (nc_fieldIds cd) ws'
            interesting = [ nw | nw@(_, w) <- named, not (isWWild w) ]
        in  if null interesting then take 1 named else interesting
    showFields fps =
        let sep = if classic then " = " else ": "
        in  "{ " ++
            intercalate ", " [ getIdBaseString f ++ sep ++ showW False w
                      | (f, w) <- fps ] ++ " }"

-- ---------------------------------------------------------------------------
-- The checker

describeCtx :: PatMatchContext -> String
describeCtx PMCase = "this case expression"
describeCtx (PMDef i)
    | isBadId i || isRenamingId i = "this pattern match"
    | base == "_lam" = "this lambda expression"
    | otherwise = "the clauses for `" ++ displayName ++ "'"
  where
    base = getIdBaseString i
    -- Instance methods are internally prefixed with an underscore; their
    -- display name records the source spelling (see Id.mkUId).  A user
    -- definition that genuinely begins with an underscore has no display
    -- name and is reported exactly as written.
    displayName = case getIdDisplayName i of
                    Just _ -> getIdBaseString (addIdDisplayName i)
                    Nothing -> base

-- | Check one obligation, producing warnings for non-exhaustive matching
-- and redundant arms.  Runs after typechecking of the enclosing top-level
-- definition has succeeded, with the final substitution applied to
-- 'pmo_type'.
checkPatMatches :: Flags -> SymTab -> NormType -> PatMatchObligation -> [WMsg]
checkPatMatches flags r normTy o
    | not doIncomplete && not doOverlap = []
    | length colTypes < pmo_ncols o = []
    | otherwise =
        case mapM normRow (pmo_rows o) of
          Nothing -> []  -- something we cannot analyze; stay quiet
          Just nrows -> incompleteWarnings nrows ++ redundantWarnings nrows
  where
    doIncomplete = warnIncompletePatterns flags
    doOverlap = warnOverlappingPatterns flags
    ctxStr = describeCtx (pmo_ctx o)
    (argTypes, _) = getArrows (pmo_type o)
    colTypes = take (pmo_ncols o) argTypes
    nSourceCols = length colTypes

    normRow row = do
        typedRow <- pmr_typed row
        let sourcePats = pmr_sourcePats row
            typedPats = pmtr_pats typedRow
        if length sourcePats /= nSourceCols ||
           length typedPats /= nSourceCols
          then Nothing
          else do
            let built = zipWith4 (buildCoveragePat normTy r)
                                 (map rootCoveragePlace [0..])
                                 colTypes
                                 sourcePats
                                 typedPats
                (coveragePats, binderLists) = unzip built
                binders = concat binderLists
                guard = buildCoverageGuard normTy binders
                           (pmr_sourceQuals row) (pmtr_qualGroups typedRow)
            nps <- mapM (normCoveragePat normTy r) coveragePats
            return (row, nps, vetGuard nps guard)

    -- Demote a guard to unknown when any of its pattern-place atoms cannot
    -- be folded into this row's pattern vector (for example, the place is
    -- inside an opaque node).  After this check, folding a cube in
    -- 'guardedVector' can only fail by contradicting the patterns.
    vetGuard nps guard
        | all foldable [pl | CoveragePatternPlace pl
                               <- S.toList (guardFormulaAtoms guard)] = guard
        | otherwise = CoverageGuardUnknown
      where
        foldable pl = case foldPlaceAtom normTy r nps pl True of
                        Nothing -> False
                        Just _ -> True

    -- Only free-variable atoms become matrix columns; pattern-place atoms
    -- are folded into the pattern vector itself (see 'foldPlaceAtom').
    guardAtoms nrows = S.toList (S.unions
        [S.filter isFreeAtom (guardFormulaAtoms guard)
        | (_, _, guard) <- nrows])
      where
        isFreeAtom (CoverageFreeVariable _) = True
        isFreeAtom (CoveragePatternPlace _) = False

    matrixTypes atoms =
        colTypes ++ replicate (length atoms) (tBitN 1 noPosition)

    -- The vector matched by one cube of a row's guard: the row's patterns
    -- constrained by the cube's pattern-place assignments, then one column
    -- per free-variable atom.  Nothing if the cube contradicts the patterns
    -- (the cube matches nothing).
    guardedVector atoms nps cube = do
        nps' <- foldPlaces nps [ (pl, v)
                               | (CoveragePatternPlace pl, v) <- M.toList cube ]
        return (map unknownAsWild nps' ++ guardCubePats atoms cube)
      where
        foldPlaces ps [] = Just ps
        foldPlaces ps ((pl, v) : rest) =
            case foldPlaceAtom normTy r ps pl v of
              Just (Just ps') -> foldPlaces ps' rest
              Just Nothing -> Nothing
              -- unreachable: 'vetGuard' pre-checked every place atom
              Nothing -> Nothing

    guardedVectors atoms nps cubes =
        [ vec | Just vec <- map (guardedVector atoms nps) cubes ]

    -- Unknown guards retain the historical conservative policy and do not
    -- contribute to completeness.  Fuel exhaustion is different: the flag's
    -- contract is to issue no conclusion, so abort this warning class.
    coverMatrix fuel atoms = go fuel []
      where
        go f acc [] = (f, Just (reverse acc))
        go f acc ((_, nps, CoverageGuardUnknown) : rows) = go f acc rows
        go f acc ((_, nps, guard) : rows) =
          case guardCubes f guard of
            (f', Nothing) -> (f', Nothing)
            (f', Just cubes) ->
              let vectors = guardedVectors atoms nps cubes
              in  go f' (reverse vectors ++ acc) rows

    incompleteWarnings nrows
      | not doIncomplete = []
      | otherwise =
        let atoms = guardAtoms nrows
        in case coverMatrix (patternCheckFuel flags) atoms nrows of
             (_, Nothing) -> []
             (fuel, Just matrix) ->
               case snd (uncovered normTy r (mkSt fuel)
                                   (matrixTypes atoms) matrix) of
                 Just ws@(_:_) ->
                   let shown = map (showWitness (isClassic ()) .
                                    take nSourceCols)
                                   (take maxWitnesses ws)
                       truncated = length ws > maxWitnesses
                   in  [(pmo_pos o,
                         WNonExhaustivePattern ctxStr shown truncated)]
                 _ -> []

    redundantWarnings nrows
      | not doOverlap = []
      | otherwise = go (mkSt (patternCheckFuel flags)) [] nrows
      where
        atoms = guardAtoms nrows
        tys = matrixTypes atoms

        go _ _ [] = []
        go st _ _ | stFuel st <= 0 = []
        go st prior ((row, nps, guard) : rest) =
              case rowVectors (stFuel st) nps guard of
                (_, Nothing) -> []
                (fuel1, Just (queries, premises)) ->
                  case usefulAny (st { stFuel = fuel1 }) prior queries of
                    (_, Nothing) -> []
                    (st2, Just isUseful) ->
                      let prior' = if any hasUnknown nps
                                   then prior
                                   else prior ++ premises
                          warnings =
                            if not (pmr_gen_dflt row) && not isUseful
                            then [(pmr_pos row,
                                   WRedundantPattern ctxStr (showRow row))]
                            else []
                      in  warnings ++ go st2 prior' rest
          where
            rowVectors f ps CoverageGuardUnknown =
              let query = map unknownAsWild ps ++ replicate (length atoms) NWild
              in  (f, Just ([query], []))
            rowVectors f ps g =
              case guardCubes f g of
                (f', Nothing) -> (f', Nothing)
                (f', Just cubes) ->
                  let vectors = guardedVectors atoms ps cubes
                  in  (f', Just (vectors, vectors))

        -- A source row whose guard has several cubes is redundant only when
        -- every satisfying cube is already covered.  A guard with no
        -- satisfiable cube (False, or contradicting its own patterns) is
        -- unreachable.
        usefulAny st _ [] = (st, Just False)
        usefulAny st prior (query:queries) =
          case useful normTy r st tys prior query of
            (st', Nothing) -> (st', Nothing)
            (st', Just True) -> (st', Just True)
            (st', Just False) -> usefulAny st' prior queries

        showRow row = unwords (map pfpString (pmr_sourcePats row))
