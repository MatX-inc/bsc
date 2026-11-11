-- | Typed, source-preserving input to pattern coverage checking.
--
-- The ordinary typechecked 'CPat' syntax is deliberately not used as the
-- coverage checker's semantic boundary: literal patterns are erased into
-- equality filters before pattern typing, and constructor patterns do not
-- retain the predicates instantiated while they are checked.  These types
-- preserve both the source form needed for diagnostics and the typed facts
-- needed by the coverage algorithm.
--
-- Values holding types have no Eq or Show instance: the checker runs after
-- the typecheck barrier and compares types only through its own helpers,
-- which never build a generated type variable's name.
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

-- | What kind of source construct the match came from (for warning text).
-- A definition carries the spelling to report: an instance method's source
-- name rather than its internal underscore-prefixed one.
data PatMatchContext
    = PMCase
    | PMDef Id String
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
}

emptyCoverageRefinement :: CoverageRefinement
emptyCoverageRefinement = CoverageRefinement [] [] [] []

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
}

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

coveragePatType :: CoveragePat -> CType
coveragePatType (CoverageWild _ _ t) = t
coveragePatType (CoverageConPat _ _ t _ _) = t
coveragePatType (CoverageLitPat _ _ t _) = t
coveragePatType (CoverageMaskPat _ _ t _ _) = t
coveragePatType (CoverageOpaque _ _ t) = t

data CoverageBinder = CoverageBinder {
    cb_id :: Id,
    cb_place :: CoveragePlace,
    cb_type :: CType,
    cb_pos :: Position
}

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

-- | The typechecked form of one source row.  Qualifiers are kept in one
-- group per source qualifier; filters introduced solely to implement a
-- main-column literal pattern are excluded before this value is built.
data PatMatchTypedRow = PatMatchTypedRow {
    pmtr_pats :: [CPat],
    pmtr_qualGroups :: [[CQual]]
}

-- | Source skeleton plus its typechecked form.  The source syntax preserves
-- exact literal spelling and include-file positions.
data PatMatchRow = PatMatchRow {
    pmr_pos :: Position,
    pmr_sourcePats :: [CPat],
    pmr_sourceQuals :: [CQual],
    pmr_typed :: Maybe PatMatchTypedRow,
    pmr_gen_dflt :: Bool
}

data PatMatchObligation = PatMatchObligation {
    pmo_pos :: Position,
    pmo_ctx :: PatMatchContext,
    pmo_type :: CType,
    pmo_ncols :: Int,
    pmo_rows :: [PatMatchRow]
}

