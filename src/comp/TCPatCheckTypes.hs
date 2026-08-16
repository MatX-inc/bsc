-- | Typed, source-preserving input to pattern coverage checking.
--
-- The ordinary typechecked 'CPat' syntax is deliberately not used as the
-- coverage checker's semantic boundary: literal patterns are erased into
-- equality filters before pattern typing, and constructor patterns do not
-- retain the predicates instantiated while they are checked.  These types
-- preserve both the source form needed for diagnostics and the typed facts
-- needed by the coverage algorithm.
module TCPatCheckTypes(
    PatMatchContext(..),
    CoverageProjection(..), CoveragePlace(..), rootCoveragePlace,
    projectCoveragePlace,
    CoverageRefinement(..), emptyCoverageRefinement,
    CoverageCon(..),
    CoverageLiteral(..), CoveragePat(..), coveragePatType,
    CoverageBinder(..),
    CoverageGuardAtom(..), CoverageGuard(..),
    PatMatchTypedRow(..), PatMatchRow(..), PatMatchObligation(..)
) where

import Id
import Position
import CSyntax(CPat, CQual, CLiteral)
import CType
import Scheme
import Pred(PredWithPositions)
import Subst(Types(..))

-- | What kind of source construct the match came from (for warning text).
data PatMatchContext
    = PMCase
    | PMDef Id
    deriving (Eq, Show)

-- | A path from a top-level pattern column to a value inspected or bound by
-- a nested pattern.  Constructor and field identities, rather than source
-- binder names, make alpha-renamed clauses refer to the same place.
data CoverageProjection
    = CoverageConArg Id Int
    | CoverageField Id
    deriving (Eq, Ord, Show)

data CoveragePlace = CoveragePlace Int [CoverageProjection]
    deriving (Eq, Ord, Show)

rootCoveragePlace :: Int -> CoveragePlace
rootCoveragePlace n = CoveragePlace n []

projectCoveragePlace :: CoveragePlace -> CoverageProjection -> CoveragePlace
projectCoveragePlace (CoveragePlace n ps) p = CoveragePlace n (ps ++ [p])

-- | Constructor facts available on the branch selected by a pattern.
-- Current BSC constructors only populate required predicates.  The remaining
-- fields are explicit hooks for existential/GADT constructors: a future
-- typechecker can fill them without changing the pattern-matrix IR.
data CoverageRefinement = CoverageRefinement {
    cvr_existentials :: [CType],
    cvr_required :: [PredWithPositions],
    cvr_provided :: [PredWithPositions],
    cvr_equalities :: [(CType, CType)]
} deriving (Eq, Show)

emptyCoverageRefinement :: CoverageRefinement
emptyCoverageRefinement = CoverageRefinement [] [] [] []

instance Types CoverageRefinement where
    apSub s r = r { cvr_existentials = apSub s (cvr_existentials r),
                    cvr_required = apSub s (cvr_required r),
                    cvr_provided = apSub s (cvr_provided r),
                    cvr_equalities = apSub s (cvr_equalities r) }
    tv r = tv (cvr_existentials r) ++ tv (cvr_required r) ++
           tv (cvr_provided r) ++ tv (cvr_equalities r)

-- | A constructor or the single implicit constructor of a struct, captured
-- where pattern typing has already resolved and instantiated it.
data CoverageCon = CoverageCon {
    cc_typeId :: Id,
    cc_name :: Id,
    cc_conNo :: Integer,
    cc_isStruct :: Bool,
    cc_isEnum :: Bool,
    cc_argTypes :: [CType],
    cc_fieldIds :: [Id],
    cc_scheme :: Maybe Scheme,
    cc_instTypes :: [CType],
    cc_resultType :: CType,
    cc_refinement :: CoverageRefinement
} deriving (Eq, Show)

instance Types CoverageCon where
    apSub s c = c { cc_argTypes = apSub s (cc_argTypes c),
                    cc_scheme = apSub s (cc_scheme c),
                    cc_instTypes = apSub s (cc_instTypes c),
                    cc_resultType = apSub s (cc_resultType c),
                    cc_refinement = apSub s (cc_refinement c) }
    tv c = tv (cc_argTypes c) ++ tv (cc_scheme c) ++
           tv (cc_instTypes c) ++ tv (cc_resultType c) ++
           tv (cc_refinement c)

data CoverageLiteral
    = CoveragePositive CLiteral
    deriving (Eq, Ord, Show)

-- | A typed pattern.  Every node retains its scrutinee place and resolved
-- type; opaque nodes conservatively stop analysis only at that node.
data CoveragePat
    = CoverageWild Position CoveragePlace CType
    | CoverageConPat Position CoveragePlace CType CoverageCon [CoveragePat]
    | CoverageLitPat Position CoveragePlace CType CoverageLiteral
    | CoverageMaskPat Position CoveragePlace CType Integer
                      [(Integer, Maybe Integer)]
    | CoverageOpaque Position CoveragePlace CType
    deriving (Eq, Show)

coveragePatType :: CoveragePat -> CType
coveragePatType (CoverageWild _ _ t) = t
coveragePatType (CoverageConPat _ _ t _ _) = t
coveragePatType (CoverageLitPat _ _ t _) = t
coveragePatType (CoverageMaskPat _ _ t _ _) = t
coveragePatType (CoverageOpaque _ _ t) = t

instance Types CoveragePat where
    apSub s (CoverageWild p place t) = CoverageWild p place (apSub s t)
    apSub s (CoverageConPat p place t c ps) =
        CoverageConPat p place (apSub s t) (apSub s c) (apSub s ps)
    apSub s (CoverageLitPat p place t l) =
        CoverageLitPat p place (apSub s t) l
    apSub s (CoverageMaskPat p place t b chunks) =
        CoverageMaskPat p place (apSub s t) b chunks
    apSub s (CoverageOpaque p place t) = CoverageOpaque p place (apSub s t)
    tv p = tv (coveragePatType p) ++
           case p of
             CoverageConPat _ _ _ c ps -> tv c ++ tv ps
             _ -> []

data CoverageBinder = CoverageBinder {
    cb_id :: Id,
    cb_place :: CoveragePlace,
    cb_type :: CType,
    cb_pos :: Position
} deriving (Eq, Show)

instance Types CoverageBinder where
    apSub s b = b { cb_type = apSub s (cb_type b) }
    tv = tv . cb_type

-- | Stable identities for Boolean facts mentioned by typed guards.  A
-- pattern-bound variable denotes its scrutinee place, so alpha-renamed rows
-- compare equal.  A free variable keeps its resolved post-typechecking 'Id';
-- using a distinct constructor prevents a shadowing binder with the same
-- source spelling from being conflated with it.
data CoverageGuardAtom
    = CoveragePatternPlace CoveragePlace
    | CoverageFreeVariable Id
    deriving (Eq, Ord, Show)

-- | Propositional guard facts supported by the first typed guard analysis.
-- Unknown guards are deliberately not comparable between rows.
data CoverageGuard
    = CoverageGuardTrue
    | CoverageGuardFalse
    | CoverageGuardAtom CoverageGuardAtom
    | CoverageGuardNot CoverageGuard
    | CoverageGuardAnd CoverageGuard CoverageGuard
    | CoverageGuardOr CoverageGuard CoverageGuard
    | CoverageGuardUnknown
    deriving (Eq, Ord, Show)

-- | The post-typechecking sidecar for one source row.  Qualifiers are kept in
-- one group per source qualifier; filters introduced solely to implement a
-- main-column literal pattern are excluded before this value is built.
data PatMatchTypedRow = PatMatchTypedRow {
    pmtr_pats :: [CPat],
    pmtr_qualGroups :: [[CQual]]
} deriving (Eq, Show)

instance Types PatMatchTypedRow where
    apSub s r = r { pmtr_pats = apSub s (pmtr_pats r),
                    pmtr_qualGroups = apSub s (pmtr_qualGroups r) }
    tv r = tv (pmtr_pats r) ++ tv (pmtr_qualGroups r)

-- | Persistent source skeleton plus its typed sidecar.  The source syntax is
-- never substituted, preserving exact literal spelling and include-file
-- positions; the typed sidecar follows TI substitutions until flush.
data PatMatchRow = PatMatchRow {
    pmr_pos :: Position,
    pmr_sourcePats :: [CPat],
    pmr_sourceQuals :: [CQual],
    pmr_typed :: Maybe PatMatchTypedRow,
    pmr_gen_dflt :: Bool
} deriving (Eq, Show)

instance Types PatMatchRow where
    apSub s r = r { pmr_typed = apSub s (pmr_typed r) }
    tv = tv . pmr_typed

data PatMatchObligation = PatMatchObligation {
    pmo_pos :: Position,
    pmo_ctx :: PatMatchContext,
    pmo_type :: CType,
    pmo_ncols :: Int,
    pmo_rows :: [PatMatchRow]
} deriving (Eq, Show)

instance Types PatMatchObligation where
    apSub s o = o { pmo_type = apSub s (pmo_type o),
                    pmo_rows = apSub s (pmo_rows o) }
    tv o = tv (pmo_type o) ++ tv (pmo_rows o)
