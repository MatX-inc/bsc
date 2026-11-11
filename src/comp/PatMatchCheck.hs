-- | Pattern-match warnings (T0165, T0166) for a typechecked package.
--
-- The pass runs after the DFtypecheck dump has deepseq'd the typechecked
-- package, as -warn-redundant-provisos does, so type inference records
-- nothing for it and every later pass sees what it would with the warnings
-- off.  It reads only data that dump and the DFconvinst one already forced,
-- plus symbol-table entries whose evaluation builds no names; it never
-- tests what is evaluated.  Its only output is a list of warnings.
--
-- Each match in the typechecked package (the generated "_case" definition
-- of a case expression, or the clauses of a named definition) is paired
-- with its skeleton in the package the typechecker read, keyed by position
-- within the same top-level definition: literal patterns, their spelling
-- and the grouping of guards survive only there, because rmPatLit replaces
-- literal patterns with variables and equality guards before pattern
-- typing.  A match without exactly one source skeleton is not checked.
-- That covers the class defaults an instance inherits (copied in by
-- TCheck.findFieldDefault, with no skeleton in the instance) and the
-- defaults themselves, because the typechecker returns class declarations
-- untyped.
module PatMatchCheck(patternMatchWarnings) where

import Data.Maybe(fromMaybe, maybeToList)
import qualified Data.Map as M

import Id
import Position
import Error(WMsg)
import Flags(Flags, warnIncompletePatterns, warnOverlappingPatterns)
import CSyntax
import SymTab(SymTab)
import TCPatCheck

-- | The warnings for the typechecked package, given the package the
-- typechecker read (the output of convInst).
patternMatchWarnings :: Flags -> SymTab -> CPackage -> CPackage -> [WMsg]
patternMatchWarnings flags symt srcPkg tcPkg
    | not (warnIncompletePatterns flags || warnOverlappingPatterns flags) = []
    | otherwise =
        concat [ concatMap (checkPatMatches flags symt)
                           (topObligations src i t cls)
               | CValueSign (CDefT i _ (CQType _ t) cls) <- tcDefns,
                 Just src <- [M.lookup i srcDefs] ]
  where
    CPackage _ _ _ _ _ srcDefns _ = srcPkg
    CPackage _ _ _ _ _ tcDefns _ = tcPkg
    srcDefs = M.fromList [ (i, d) | CValueSign d@(CDef i _ _) <- srcDefns ]

topObligations :: CDef -> Id -> CType -> [CClause] -> [PatMatchObligation]
topObligations src i t cls = tcDef (mkSrcIndex src) i t cls []

-- ---------------------------------------------------------------------------
-- Sub-terms

data SubTerm = SubExpr CExpr | SubDefl CDefl | SubQual CQual

-- | The immediate sub-terms of an expression that can contain matches.
subTerms :: CExpr -> [SubTerm]
subTerms expr =
    case expr of
      CLam _ e -> [SubExpr e]
      CLamT _ _ e -> [SubExpr e]
      Cletseq ds e -> map SubDefl ds ++ [SubExpr e]
      Cletrec ds e -> map SubDefl ds ++ [SubExpr e]
      CSelect e _ -> [SubExpr e]
      CCon _ es -> map SubExpr es
      Ccase _ e arms ->
          SubExpr e : concat [ map SubQual qs ++ [SubExpr c]
                             | CCaseArm _ qs c <- arms ]
      CStruct _ _ fs -> [ SubExpr e | (_, e) <- fs ]
      CStructUpd e fs -> SubExpr e : [ SubExpr e' | (_, e') <- fs ]
      Cwrite _ e1 e2 -> [SubExpr e1, SubExpr e2]
      CAny {} -> []
      CVar _ -> []
      CApply e es -> map SubExpr (e : es)
      CTaskApply e es -> map SubExpr (e : es)
      CTaskApplyT e _ es -> map SubExpr (e : es)
      CLit _ -> []
      CBinOp e1 _ e2 -> [SubExpr e1, SubExpr e2]
      CHasType e _ -> [SubExpr e]
      Cif _ c e1 e2 -> map SubExpr [c, e1, e2]
      CSub _ e1 e2 -> [SubExpr e1, SubExpr e2]
      CSub2 e1 e2 e3 -> map SubExpr [e1, e2, e3]
      CSubUpdate _ e (h, l) rhs -> map SubExpr [e, h, l, rhs]
      Cmodule _ ms -> concatMap mstmtTerms ms
      Cinterface _ _ ds -> map SubDefl ds
      CmoduleVerilog e _ _ _ args _ _ _ -> SubExpr e : [ SubExpr a | (_, a) <- args ]
      CForeignFuncC {} -> []
      Cdo _ ss -> concatMap stmtTerms ss
      Caction _ ss -> concatMap stmtTerms ss
      Crules _ rs -> concatMap ruleTerms rs
      COper ops -> [ SubExpr e | CRand e <- ops ]
      CCon1 _ _ e -> [SubExpr e]
      CSelectTT _ e _ -> [SubExpr e]
      CCon0 {} -> []
      CConT _ _ es -> map SubExpr es
      CStructT _ fs -> [ SubExpr e | (_, e) <- fs ]
      CSelectT {} -> []
      CLitT {} -> []
      CAnyT {} -> []
      CmoduleVerilogT _ e _ _ _ args _ _ _ -> SubExpr e : [ SubExpr a | (_, a) <- args ]
      CForeignFuncCT {} -> []
      CTApply e _ -> [SubExpr e]
      Cattributes _ -> []
  where
    stmtTerms (CSBindT _ name _ _ e) = map SubExpr (maybeToList name ++ [e])
    stmtTerms (CSBind _ name _ e) = map SubExpr (maybeToList name ++ [e])
    stmtTerms (CSletseq ds) = map SubDefl ds
    stmtTerms (CSletrec ds) = map SubDefl ds
    stmtTerms (CSExpr name e) = map SubExpr (maybeToList name ++ [e])
    mstmtTerms (CMStmt s) = stmtTerms s
    mstmtTerms (CMrules e) = [SubExpr e]
    mstmtTerms (CMinterface e) = [SubExpr e]
    mstmtTerms (CMTupleInterface _ es) = map SubExpr es
    ruleTerms (CRule _ name qs e) =
        map SubExpr (maybeToList name) ++ map SubQual qs ++ [SubExpr e]
    ruleTerms (CRuleNest _ name qs rs) =
        map SubExpr (maybeToList name) ++ map SubQual qs ++
        concatMap ruleTerms rs

-- ---------------------------------------------------------------------------
-- Source skeletons

-- | The matches of one top-level definition in the package the typechecker
-- read: case expressions by position, and clause-carrying definitions by
-- name and position (Id equality ignores the position).
data SrcIndex = SrcIndex {
    si_cases :: M.Map Position [(CExpr, [CCaseArm])],
    si_defs :: M.Map (Id, Position) [[CClause]],
    -- instance method (convInst's underscore-prefixed Id) to its source name
    si_methods :: M.Map Id String
}

data SrcMatch = SrcCase Position CExpr [CCaseArm] | SrcDef Id [CClause]

mkSrcIndex :: CDef -> SrcIndex
mkSrcIndex d =
    SrcIndex {
      si_cases = M.fromListWith (flip (++))
                   [ (pos, [(e, arms)]) | SrcCase pos e arms <- ms ],
      si_defs = M.fromListWith (flip (++))
                  [ ((j, getIdPosition j), [cls]) | SrcDef j cls <- ms ],
      si_methods = instanceMethods d }
  where
    ms = case d of
           CDef i _ cls -> srcDef i cls [] []
           CDefT i _ _ cls -> srcDef i cls [] []

    srcDef j cls me = (SrcDef j cls :) . srcQuals me . srcClauses cls
    srcClauses cls k = foldr srcClause k cls
    srcClause (CClause _ qs e) = srcQuals qs . srcExpr e
    srcQuals qs k = foldr srcQual k qs
    srcQual (CQGen _ _ e) = srcExpr e
    srcQual (CQFilter e) = srcExpr e
    srcExpr e@(Ccase pos s arms) = (SrcCase pos s arms :) . srcSubs e
    srcExpr e = srcSubs e
    srcSubs e k = foldr srcSub k (subTerms e)
    srcSub (SubExpr e) = srcExpr e
    srcSub (SubDefl dl) = srcDefl dl
    srcSub (SubQual q) = srcQual q
    srcDefl (CLValueSign (CDef j _ cls) me) = srcDef j cls me
    srcDefl (CLValueSign (CDefT j _ _ cls) me) = srcDef j cls me
    srcDefl (CLValue j cls me) = srcDef j cls me
    srcDefl (CLMatch _ e) = srcExpr e

-- convInst turns an instance into a dictionary definition whose methods are
-- local definitions _m, collected in a struct with fields m = _m.
instanceMethods :: CDef -> M.Map Id String
instanceMethods (CDef i _ [CClause [] [] (Cletrec _ (CStruct _ _ fs))])
    | isDictId i =
        M.fromList [ (u, getIdBaseString f)
                   | (f, CVar u) <- fs,
                     getIdBaseString u == '_' : getIdBaseString f ]
instanceMethods _ = M.empty

-- | The source skeleton for a key, if there is exactly one: candidates at
-- the same key (one include expanded twice, say) count as one only when
-- they agree in everything the checker reads from the source.  It reads
-- the patterns, and of a guard only whether it is a filter or a generator
-- (with the generator's pattern): the guard's meaning comes from its
-- typechecked form.  Nothing here compares types.
uniqueBy :: (a -> a -> Bool) -> Maybe [a] -> Maybe a
uniqueBy same (Just (c : cs)) | all (same c) cs = Just c
uniqueBy _ _ = Nothing

sameClauses :: [CClause] -> [CClause] -> Bool
sameClauses cs cs' =
    length cs == length cs' &&
    and [ samePats ps ps' && length qs == length qs' &&
          and (zipWith sameQual qs qs')
        | (CClause ps qs _, CClause ps' qs' _) <- zip cs cs' ]
  where
    sameQual (CQFilter _) (CQFilter _) = True
    sameQual (CQGen _ p _) (CQGen _ p' _) = samePat p p'
    sameQual _ _ = False

samePats :: [CPat] -> [CPat] -> Bool
samePats ps ps' = length ps == length ps' && and (zipWith samePat ps ps')

-- source patterns, with positions (they place the warnings); TCPat's
-- CPConTs never occurs in source
samePat :: CPat -> CPat -> Bool
samePat p p' = getPosition p == getPosition p' && same p p'
  where
    same (CPCon c ps) (CPCon c' ps') = c == c' && samePats ps ps'
    same (CPstruct mb c fs) (CPstruct mb' c' fs') =
        mb == mb' && c == c' && map fst fs == map fst fs' &&
        samePats (map snd fs) (map snd fs')
    same (CPVar v) (CPVar v') = v == v'
    same (CPAs v q) (CPAs v' q') = v == v' && samePat q q'
    same (CPAny _) (CPAny _) = True
    same (CPLit l) (CPLit l') = l == l'
    same (CPMixedLit _ b cs) (CPMixedLit _ b' cs') = b == b' && cs == cs'
    same (CPCon1 t c q) (CPCon1 t' c' q') = t == t' && c == c' && samePat q q'
    same _ _ = False

sameCase :: (CExpr, [CCaseArm]) -> (CExpr, [CCaseArm]) -> Bool
sameCase (e, arms) (e', arms') =
    isGeneratedScrutinee e == isGeneratedScrutinee e' &&
    sameClauses (armClauses arms) (armClauses arms')

armClauses :: [CCaseArm] -> [CClause]
armClauses arms = [ CClause [p] qs c | CCaseArm p qs c <- arms ]

-- | Is the scrutinee of a case expression compiler-generated?  The BSV
-- parser encodes plain @case@ (and @if@ with @matches@ conditions) as a
-- degenerate case on the unit struct with guard-only arms, and deriving
-- scrutinizes compiler-generated temporaries.  Any generated variable in
-- the scrutinee counts, free or bound.
isGeneratedScrutinee :: CExpr -> Bool
isGeneratedScrutinee (CStruct _ i []) | isPrelude "PrimUnit" i = True
isGeneratedScrutinee e = any (\ i -> isBadId i || isRenamingId i) (exprVars e)
  where
    exprVars (CVar i) = [i]
    exprVars (CBinOp e1 i e2) = i : exprVars e1 ++ exprVars e2
    exprVars (COper ops) = concat [ case op of
                                      CRand e' -> exprVars e'
                                      CRator _ i -> [i]
                                  | op <- ops ]
    exprVars e' = concatMap subVars (subTerms e')
    subVars (SubExpr e') = exprVars e'
    subVars (SubQual q) = qualVars q
    subVars (SubDefl (CLValueSign (CDef _ _ cls) me)) = defVars cls me
    subVars (SubDefl (CLValueSign (CDefT _ _ _ cls) me)) = defVars cls me
    subVars (SubDefl (CLValue _ cls me)) = defVars cls me
    subVars (SubDefl (CLMatch _ e')) = exprVars e'
    defVars cls me =
        concatMap qualVars me ++
        concat [ concatMap qualVars qs ++ exprVars b | CClause _ qs b <- cls ]
    qualVars (CQGen _ _ e') = exprVars e'
    qualVars (CQFilter e') = exprVars e'

-- ---------------------------------------------------------------------------
-- Typechecked matches

tcDef :: SrcIndex -> Id -> CType -> [CClause] -> [CQual]
      -> [PatMatchObligation]
tcDef idx i t cls me =
    concatMap (tcQual idx) me ++
    maybeToList (defObligation idx i t cls) ++
    concatMap (tcClause idx) cls

tcClause :: SrcIndex -> CClause -> [PatMatchObligation]
tcClause idx (CClause _ qs e) = concatMap (tcQual idx) qs ++ tcExpr idx e

tcQual :: SrcIndex -> CQual -> [PatMatchObligation]
tcQual idx (CQGen _ _ e) = tcExpr idx e
tcQual idx (CQFilter e) = tcExpr idx e

tcDefl :: SrcIndex -> CDefl -> [PatMatchObligation]
tcDefl idx (CLValueSign (CDefT i _ (CQType _ t) cls) me) = tcDef idx i t cls me
tcDefl idx (CLValueSign (CDef _ _ cls) me) =
    concatMap (tcQual idx) me ++ concatMap (tcClause idx) cls
tcDefl idx (CLValue _ cls me) =
    concatMap (tcQual idx) me ++ concatMap (tcClause idx) cls
tcDefl idx (CLMatch _ e) = tcExpr idx e

tcExpr :: SrcIndex -> CExpr -> [PatMatchObligation]
-- TCheck types "case e of arms" as "let _case :: T -> R; _case arms in
-- _case e", with _case at the case's position
tcExpr idx (Cletseq [CLValueSign (CDefT ic _ (CQType _ t) cls) []]
                    (CApply (CVar ic') (e : es)))
    | getIdBaseString ic == "_case", null (getIdQualString ic), ic == ic',
      Just src <- uniqueBy sameCase (M.lookup pos (si_cases idx)) =
        tcExpr idx e ++
        maybeToList (caseObligation pos t cls src) ++
        concatMap (tcClause idx) cls ++
        concatMap (tcExpr idx) es
  where pos = getIdPosition ic
tcExpr idx e = concatMap sub (subTerms e)
  where sub (SubExpr e') = tcExpr idx e'
        sub (SubDefl d) = tcDefl idx d
        sub (SubQual q) = tcQual idx q

caseObligation :: Position -> CType -> [CClause] -> (CExpr, [CCaseArm])
               -> Maybe PatMatchObligation
caseObligation pos t tcls (scrut, arms)
    | isGeneratedScrutinee scrut = Nothing
    | otherwise = do
        let srcCls = armClauses arms
        o <- mkClausesObligation PMCase pos t srcCls
        rows <- typedRows srcCls tcls
        completePatMatchObligation o rows

defObligation :: SrcIndex -> Id -> CType -> [CClause]
              -> Maybe PatMatchObligation
defObligation idx i t cls = do
    srcCls <- uniqueBy sameClauses (M.lookup (i, getIdPosition i) (si_defs idx))
    let name = fromMaybe (getIdBaseString i) (M.lookup i (si_methods idx))
    o <- mkClausesObligation (PMDef i name) (getIdPosition i) t' srcCls
    rows <- typedRows srcCls tcls
    completePatMatchObligation o rows
  where
    (t', tcls) = fromMaybe (t, cls) (dictWrapped cls)

-- | The user's clauses of a definition with dictionary arguments or
-- bindings.  TCheck (tiExpl''', tiImpls) types it as one clause binding
-- the dictionaries, whose body is
--   letseq nonrec in letrec rec in letseq ii :: t; ii clauses in ii
-- (either outer let absent when empty, and cLetSeq merging the two letseqs)
-- with ii a generated name.
dictWrapped :: [CClause] -> Maybe (CType, [CClause])
dictWrapped [CClause ps [] body] | all isDictPat ps =
    case body of
      Cletrec _ e -> inner e
      Cletseq _ (Cletrec _ e) -> inner e
      e -> inner e
  where
    isDictPat (CPVar v) = isDictId v
    isDictPat _ = False
    inner (Cletseq ds@(_ : _) (CVar v))
      | CLValueSign (CDefT ii _ (CQType _ t) cls) [] <- last ds,
        ii == v, isBadId ii = Just (t, cls)
    inner _ = Nothing
dictWrapped _ = Nothing

-- | Pair source rows with typechecked ones.  The typechecked guards begin
-- with the equality filters rmPatLit made for literals in the patterns;
-- then each source guard has its own group, a generator with the filters
-- for literals in its pattern.
typedRows :: [CClause] -> [CClause] -> Maybe [PatMatchTypedRow]
typedRows srcCls tcls
    | length srcCls /= length tcls = Nothing
    | otherwise = sequence (zipWith typedRow srcCls tcls)
  where
    typedRow (CClause sps sqs _) (CClause tps tqs _)
      | length sps /= length tps = Nothing
      | length tqs /= nPatFilters + sum sizes = Nothing
      | otherwise =
          Just (PatMatchTypedRow tps (groups sizes (drop nPatFilters tqs)))
      where
        nPatFilters = sum (map litFilters sps)
        sizes = map qualSize sqs
    qualSize (CQFilter _) = 1
    qualSize (CQGen _ p _) = 1 + litFilters p
    groups [] _ = []
    groups (n : ns) qs = let (g, rest) = splitAt n qs in g : groups ns rest

-- | The number of equality filters TCMisc.rmPatLit makes for a pattern.
litFilters :: CPat -> Int
litFilters p =
    case p of
      CPLit _ -> 1
      CPMixedLit _ _ chunks -> length [ () | (_, Just _) <- chunks ]
      CPCon _ ps -> sum (map litFilters ps)
      CPstruct _ _ fs -> sum (map (litFilters . snd) fs)
      CPAs _ p' -> litFilters p'
      CPCon1 _ _ p' -> litFilters p'
      CPConTs _ _ _ ps -> sum (map litFilters ps)
      _ -> 0
