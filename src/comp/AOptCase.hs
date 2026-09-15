{-# OPTIONS_GHC -fwarn-name-shadowing -fwarn-missing-signatures #-}

-- | Building case expressions: dynamic array selects, and nested
-- if-expressions that are really a case on one variable.
--
-- Both rewrite one expression tree into another, taking the def lookup
-- they need as an argument, and neither asks a solver anything.  That
-- is why they sit apart from AOpt: AOpt's and/or redundancy pass does
-- reach a SAT solver, so importing AOpt means linking STP and Yices.
-- A consumer of an already-scheduled design -- the Bluesim path, and
-- the BIR exporter behind it -- wants these two entry points and
-- nothing else from AOpt, and gets them without the solvers.
--
-- AOpt calls both, and owns the passes that do need a solver.
module AOptCase(
                -- entry points
                aExpandDynSel, aInsertCaseDef,

                -- shared with AOpt, which builds case arms of its own
                rmDupsInCasePairs, isInt
                ) where

import qualified Data.Set as S

import Error(internalError)
import IOUtil(progArgs)
import IntLit(IntLit(..), ilDec)
import PPrint(ppReadable)
import Prim
import Util(flattenPairs, makePairs, initOrErr, lastOrErr, allSame, tracep)

import ASyntax
import ASyntaxUtil(AExprs, mapAExprs)

-- Tracing follows AOpt's switch: AOpt calls both entry points, so
-- their traces belong with its own.
debug :: Bool
debug = "-trace-aopt" `elem` progArgs

debug2 :: Bool
debug2 = debug  && False -- debug

-- ========================================================================
-- Dynamic array select
--

-- entry point for replacing dynamic select with case expressions
-- (or if-expressions, for Strings, which Verilog can't support as case)
--
aExpandDynSel :: (AExprs a) => Bool -> (AId -> AExpr) -> a -> a
aExpandDynSel stringOK findFn = mapAExprs expDynSel
  where
        expDynSel (APrim sel_i sel_ty PrimArrayDynSelect [arr_e, idx_e]) =
           case arr_e of
             ASDef _ i ->
                 let arr_e' = findFn i
                 in  expDynSel
                         (APrim sel_i sel_ty PrimArrayDynSelect [arr_e', idx_e])
             APrim arr_i arr_ty PrimBuildArray elem_es ->
               let idx_ty = ae_type idx_e
                   max_idx = case idx_ty of
                               ATBit sz -> (2^sz) - 1
                               _ -> internalError ("aExpandDynSel: idx_ty")
                   -- number of arms is the min of the elems and the max index
                   arms = zip [0..max_idx] elem_es
                   -- make the last string be the default, if necessary
                   (arms_init, arms_last) =
                       (initOrErr "aExpandDynSel: init" arms,
                        lastOrErr "aExpandDynSel: last" arms)
                   mkLit n = ASInt defaultAId idx_ty (ilDec n)
                   mkCaseArm (n, e) = (mkLit n, e)
               in
                 if (isStringType sel_ty) && (not stringOK)
                 then
                   -- Verilog has to use if expressions for non-bit types
                   let foldFn (n, thn) els =
                         let cond = APrim sel_i aTBool PrimEQ [idx_e, mkLit n]
                         in  APrim sel_i sel_ty PrimIf [cond, thn, els]
                   in  foldr foldFn (snd arms_last) arms_init
                 else if (isStringType sel_ty)
                 then
                   -- We still want to avoid creating an ASAny of type String
                   -- so for now use the last string as the default
                   let case_arms = map mkCaseArm arms_init
                       default_e = snd arms_last
                   in  APrim sel_i sel_ty PrimCase
                          (idx_e : default_e : flattenPairs case_arms)
                 else
                   let case_arms = map mkCaseArm arms
                       default_e = ASAny (getArrayElemType arr_ty) Nothing
                   in  APrim sel_i sel_ty PrimCase
                          (idx_e : default_e : flattenPairs case_arms)
             _ -> internalError ("aExpandDynSel: unexpected array: " ++
                                 ppReadable arr_e)
        expDynSel e = e

-- ========================================================================
-- Nested if-expressions to case
--

-- entry point for optimizing nested if-expressions in an expr,
-- converting them to case-expressions in some situations.
-- * nested ifs of 4 or more arms (including default) are converted
-- * XXX could check for completeness here?
-- * Note: this can inline references to other defs, to build the case
--
aInsertCaseDef :: Bool -> (AId -> AExpr) -> ADef -> ADef
aInsertCaseDef stringOK findFn (ADef i t e p) =
    ADef i t (aInsertCase stringOK findFn e) p

aInsertCase :: Bool -> (AId -> AExpr) -> AExpr -> AExpr
aInsertCase stringOK findFn (APrim i t p es) =
    let es' = map (aInsertCase stringOK findFn) es
    in  aPrimInsertCase stringOK findFn i t p es'
aInsertCase stringOK findFn (AMethCall t i m es) =
    let es' = map (aInsertCase stringOK findFn) es
    in  AMethCall t i m es'
aInsertCase stringOK findFn (AFunCall t i f isC es) =
    let es' = map (aInsertCase stringOK findFn) es
    in  AFunCall t i f isC es'
aInsertCase _ _ e = e

-- Convert any nested if-expressions that are checking the value of
-- one variable (e.g. "if (v == 1) else if (v == 2) ...") into a case
-- expression.  Allows arms such as "if (v == 3) || (v == 5)", but
-- they become separate arms in the case statement.
aPrimInsertCase :: Bool -> (AId -> AExpr) ->
                   AId -> AType -> PrimOp -> [AExpr] -> AExpr
aPrimInsertCase stringOK findFn aid t PrimIf es@[cond, _, _]
  | stringOK || not (isStringType t) =
    -- if the condition is of the form "(v == c) || (v2 == c2) || ...",
    -- then this will be Just [(v,c), (v2,c2), ...]
    let mcs = getConsts findFn (const True) cond
        res = case mcs of
                -- if all the v's are the same
                Just cs@((v,_):_) | allSame (map fst cs) ->
                    -- collect the ifs,
                    -- if there are any nested, convert to case
                    let (ces, d) = collIf findFn v [] (APrim aid t PrimIf es)
                        -- aPrim will check this again
                        -- but we do it here for the Bluesim backend
                        ces' = rmDupsInCasePairs ces
                    in  if length ces > 1
                        then APrim aid t PrimCase (v:d:flattenPairs ces')
                        else APrim aid t PrimIf es
                _ -> APrim aid t PrimIf es
    in  tracep debug2 ("aPrimInsertCase: " ++ ppReadable es) $
        tracep debug2 ("aPrimInsertCase: result: " ++ ppReadable res) $
        res
aPrimInsertCase stringOK findFn aid t p es = APrim aid t p es

-- ========================================================================
-- Helpers
--

-- keep expanding an expression until it is not a variable reference
-- This is used for inferring case statements, so the find function
-- returns the pre-optimized def
expandVarRef :: (AId -> AExpr) -> AExpr -> AExpr
expandVarRef findf (ASDef { ae_objid = aid }) = expandVarRef findf (findf aid)
expandVarRef _ e = e

-- Collect nested if's and case's
-- Given a find function (for following defs),
-- and a variable (v) that the first if expression is conditional on,
-- and the constant expressions found so far (ces):
-- if the else clause is an if expression on the same variable
-- (according to getConsts), then add those constants to ces and
-- recurse on the else clause of the next level.
-- If a case statement on the same variable is found, add the ces to
-- those found in the case.
-- If anything else is found, return the ces so far.
-- The second item (d) returned is the final else expression (the default).
-- Note that "if (x == 1) || (x == 2)" generates two entries in "ces"
-- (two arms in the case).
--
-- Note: The "find" function probably needs to be a lookup of
-- pre-optimization values.  Because aPrimInsertCase is called on each opt,
-- interleaved with aOptDef, and so each if-else will be converted to
-- PrimCase and then aOptDef will fill out this case.  This does not seem
-- to be too much extra work; but if one wants to save work, maybe we can
-- map aPrimInsertCase over all the defs first, then call aOptDef ...
-- perhaps even removing any defs that were inlined away by aPrimInsertCase
-- before mapping aOptDef.
--
collIf :: (AId -> AExpr) -> AExpr -> [(AExpr, AExpr)] -> AExpr ->
          ([(AExpr, AExpr)], AExpr)
collIf findf v ces d =
    let -- this is either the original expr or an expanded variable ref
        d' = expandVarRef findf d
    in
        case (collIfPrim findf v ces d') of
            Just res -> res
            Nothing  -> (reverse ces, d)

-- This does the real work.
-- It returns (Just result) if optimization was possible
collIfPrim :: (AId -> AExpr) -> AExpr -> [(AExpr, AExpr)] -> AExpr -> Maybe ([(AExpr, AExpr)], AExpr)
collIfPrim findf v ces (APrim _ _ PrimIf [_, t, e]) | t == e =
        Just $ collIf findf v ces t
collIfPrim findf v ces (APrim _ _ PrimIf [cond, t, e])
  | (Just cs) <- getConsts findf (== v) cond =
        Just $ collIf findf v (zip (map snd cs) (repeat t) ++ ces) e
collIfPrim findf v ces (APrim _ _ PrimCase (v':d:ces')) | v == v' =
        Just (reverse ces ++ makePairs ces', d)
collIfPrim _ _ ces d = Nothing


-- Given a find function (from an Id to its Expr),
-- and a function which tells whether to consider the current expression
-- (either "const True" or "== v"),
-- return "Just vcs"
--    if the expression has one or more expressions like "(v == c)" OR'd
--    together, where vcs is a mapping from v to c
--    (if the expression is a def, follow its definition)
-- return Nothing otherwise
getConsts :: (AId -> AExpr) -> (AExpr -> Bool) -> AExpr -> Maybe [(AExpr, AExpr)]
getConsts find p (APrim _ _ PrimEQ [v, c]) | p v && isInt c = Just [(v, c)]
getConsts find p (APrim _ _ PrimBOr es) = mapM (getConsts find p) es >>= return . concat
getConsts find p (ASDef _ i) = getConsts find p (find i)
getConsts find _ _ = Nothing

isInt :: AExpr -> Bool
isInt (ASInt _ _ _) = True
isInt _ = False

rmDupsInCasePairs :: [(AExpr,AExpr)] ->  [(AExpr,AExpr)]
rmDupsInCasePairs prs = rmDups S.empty prs
    where
      rmDups :: S.Set Integer -> [(AExpr,AExpr)] -> [(AExpr,AExpr)]
      rmDups set [] = []
      rmDups set ( (p@(ASInt _ _ (IntLit { ilValue = n })), a) : rest )
          | n `S.member` set = rmDups set rest
          | otherwise = (p,a) : rmDups (S.insert n set) rest
      rmDups set ((p,a):rest) =
          internalError ("rmDupsInCasePairs: " ++ ppReadable p)
