{-# LANGUAGE MonoLocalBinds, ScopedTypeVariables, TypeApplications #-}
{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module ISyntaxXRef(
                   updateIExprPosition,
                   mapIExprPosition,
                   mapIExprPositionRef,
                   mapIExprPosition2,
                   mapIExprPositionConservative
                  ) where

import qualified Data.Set as S

import ISyntax
import Id
import Position(Position, noPosition, isUsefulPosition)

-- #############################################################################
-- #
-- #############################################################################

-- (the recursive calls under the binder matches are typed at the outer
-- phase, so that the per-phase specialisations apply to them)
updateIExprPosition :: forall a . KnownPhase a => Position -> IExpr a -> IExpr a
{-# SPECIALISE updateIExprPosition :: Position -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE updateIExprPosition :: Position -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE updateIExprPosition :: Position -> IExpr PostElab -> IExpr PostElab #-}
updateIExprPosition pos (ILam i t e) = (ILam (setIdPosition pos i) t (updateIExprPosition @a pos e))
updateIExprPosition pos (IAps e ts [e0]) = (IAps (updateIExprPosition pos e) ts [(updateIExprPosition pos e0)])
updateIExprPosition pos (IAps e ts es) = (IAps (updateIExprPosition pos e) ts es)
updateIExprPosition pos (IVar i) = (IVar (setIdPosition pos i))
updateIExprPosition pos (ILAM i kind e) = (ILAM (setIdPosition pos i) kind (updateIExprPosition @a pos e))
updateIExprPosition pos iexpr@(ICon i t (ICStateVar isv)) = iexpr
updateIExprPosition pos (ICon i t info) = (ICon (setIdPosition pos i) t info)
-- The heap ref's position set carries the stamped position out of
-- band; the type itself is deliberately not restamped.  Rewriting the
-- positions of the Ids inside a type rebuilds the whole type, and
-- once ITypes are hash-consed the rebuild re-canonicalizes to the
-- original node, dropping the stamps anyway.
updateIExprPosition pos (IRefT t p poss r) = (IRefT t p poss' r)
  where poss' = S.insert pos poss


updateIExprPosition2 :: forall a . KnownPhase a => Position -> IExpr a -> IExpr a
{-# SPECIALISE updateIExprPosition2 :: Position -> IExpr PreElab -> IExpr PreElab #-}
{-# SPECIALISE updateIExprPosition2 :: Position -> IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE updateIExprPosition2 :: Position -> IExpr PostElab -> IExpr PostElab #-}
updateIExprPosition2 pos (ILam i t e) = (ILam (setIdPosition pos i) t (updateIExprPosition @a pos e))
updateIExprPosition2 pos iexpr@(IAps e@(ICon i _ (ICCon _)) ts [e0]) =
    if (not (isUsefulPosition (getIdPosition i)))
    then updateIExprPosition pos iexpr
    else iexpr
updateIExprPosition2 pos iexpr@(IAps e@(ICon i _ (ICPrim _)) ts es) =
    if (not (isUsefulPosition (getIdPosition i)))
    then updateIExprPosition pos iexpr
    else iexpr
updateIExprPosition2 pos (IAps e ts [e0]) = (IAps (updateIExprPosition pos e) ts [(updateIExprPosition pos e0)])
updateIExprPosition2 pos (IAps e ts es) = (IAps (updateIExprPosition pos e) ts es)
updateIExprPosition2 pos (IVar i) = (IVar (setIdPosition pos i))
updateIExprPosition2 pos (ILAM i kind e) = (ILAM (setIdPosition pos i) kind (updateIExprPosition @a pos e))
updateIExprPosition2 pos iexpr@(ICon i t (ICStateVar isv)) = iexpr
updateIExprPosition2 pos (ICon i t info) = (ICon (setIdPosition pos i) t info)
updateIExprPosition2 pos (IRefT t p poss r) = (IRefT t p poss' r)
  where poss' = S.insert pos poss

mapIExprPosition :: KnownPhase a => Bool -> (IExpr a, IExpr a) -> IExpr a
{-# SPECIALISE mapIExprPosition :: Bool -> (IExpr PreElab, IExpr PreElab) -> IExpr PreElab #-}
{-# SPECIALISE mapIExprPosition :: Bool -> (IExpr Elab, IExpr Elab) -> IExpr Elab #-}
{-# SPECIALISE mapIExprPosition :: Bool -> (IExpr PostElab, IExpr PostElab) -> IExpr PostElab #-}
mapIExprPosition False (expr_0, expr_1) = expr_1
mapIExprPosition True (expr_0, expr_1) =
    let positionModel = (getIExprPositionCross expr_0)
    in if (positionModel == noPosition) || (not (isUsefulPosition positionModel))
       then expr_1
       else let positionCurrent = (getIExprPositionCross expr_1)
            in if (positionModel == positionCurrent)
               then expr_1
               else if (isEquivIExprIncluded expr_1 expr_0)
                    then let pos = (getIExprPositionCross (head (extractEquivIExpr expr_1 expr_0)))
                             expr = (updateIExprPosition pos expr_1)
                         in expr
                    else (updateIExprPosition positionModel expr_1)

-- mapIExprPosition for a model that is a heap reference (of any phase):
-- a reference is never equivalent to any node of the rebuilt expression
-- (equivIExprs holds only between two references with the same pointer,
-- and the rebuilt side has none), so the equivalence search above is
-- vacuous and only the reference's own position is consulted.
mapIExprPositionRef :: KnownPhase b => Bool -> (IExpr a, IExpr b) -> IExpr b
{-# SPECIALISE mapIExprPositionRef :: Bool -> (IExpr a, IExpr PreElab) -> IExpr PreElab #-}
{-# SPECIALISE mapIExprPositionRef :: Bool -> (IExpr a, IExpr Elab) -> IExpr Elab #-}
{-# SPECIALISE mapIExprPositionRef :: Bool -> (IExpr a, IExpr PostElab) -> IExpr PostElab #-}
mapIExprPositionRef False (expr_0, expr_1) = expr_1
mapIExprPositionRef True (expr_0, expr_1) =
    let positionModel = (getIExprPositionCross expr_0)
    in if (positionModel == noPosition) || (not (isUsefulPosition positionModel))
       then expr_1
       else let positionCurrent = (getIExprPositionCross expr_1)
            in if (positionModel == positionCurrent)
               then expr_1
               else (updateIExprPosition positionModel expr_1)

mapIExprPosition2 :: KnownPhase a => Bool -> (IExpr a, IExpr a) -> IExpr a
{-# SPECIALISE mapIExprPosition2 :: Bool -> (IExpr PreElab, IExpr PreElab) -> IExpr PreElab #-}
{-# SPECIALISE mapIExprPosition2 :: Bool -> (IExpr Elab, IExpr Elab) -> IExpr Elab #-}
{-# SPECIALISE mapIExprPosition2 :: Bool -> (IExpr PostElab, IExpr PostElab) -> IExpr PostElab #-}
mapIExprPosition2 False (expr_0, expr_1) = expr_1
mapIExprPosition2 True (expr_0, expr_1) =
    let positionModel = (getIExprPositionCross expr_0)
    in if (positionModel == noPosition) || (not (isUsefulPosition positionModel))
       then expr_1
       else let positionCurrent = (getIExprPositionCross expr_1)
            in if (positionModel == positionCurrent)
               then expr_1
               else if (isEquivIExprIncluded expr_1 expr_0)
                    then let pos = (getIExprPositionCross (head (extractEquivIExpr expr_1 expr_0)))
                             expr = (updateIExprPosition2 pos expr_1)
                         in expr
                    else (updateIExprPosition2 positionModel expr_1)

mapIExprPositionConservative :: KnownPhase a => Bool -> (IExpr a,IExpr a) -> IExpr a
{-# SPECIALISE mapIExprPositionConservative :: Bool -> (IExpr PreElab,IExpr PreElab) -> IExpr PreElab #-}
{-# SPECIALISE mapIExprPositionConservative :: Bool -> (IExpr Elab,IExpr Elab) -> IExpr Elab #-}
{-# SPECIALISE mapIExprPositionConservative :: Bool -> (IExpr PostElab,IExpr PostElab) -> IExpr PostElab #-}
mapIExprPositionConservative False (expr_0, expr_1) = expr_1
mapIExprPositionConservative True (expr_0, expr_1) =
    let positionModel = (getIExprPositionCross expr_0)
    in if (positionModel == noPosition) || (not (isUsefulPosition positionModel))
       then expr_1
       else let positionCurrent = (getIExprPositionCross expr_1)
            in if (positionModel == positionCurrent) || (isUsefulPosition positionCurrent)
               then expr_1
               else if (isEquivIExprIncluded expr_1 expr_0)
                    then let pos = (getIExprPositionCross (head (extractEquivIExpr expr_1 expr_0)))
                             expr = (updateIExprPosition pos expr_1)
                         in expr
                    else (updateIExprPosition positionModel expr_1)

-- #############################################################################
-- #
-- #############################################################################

isEquivIExprIncluded :: KnownPhase a => IExpr a -> IExpr a -> Bool
{-# SPECIALISE isEquivIExprIncluded :: IExpr PreElab -> IExpr PreElab -> Bool #-}
{-# SPECIALISE isEquivIExprIncluded :: IExpr Elab -> IExpr Elab -> Bool #-}
{-# SPECIALISE isEquivIExprIncluded :: IExpr PostElab -> IExpr PostElab -> Bool #-}
isEquivIExprIncluded sub_expr expr@(ILam i t e) =
    ((equivIExprs expr sub_expr) || (isEquivIExprIncluded sub_expr e))
isEquivIExprIncluded sub_expr expr@(IAps e ts es) =
    ((equivIExprs expr sub_expr) || (or (map (equivIExprs sub_expr) es)))
isEquivIExprIncluded sub_expr expr@(IVar _) =
    (equivIExprs expr sub_expr)
isEquivIExprIncluded sub_expr expr@(ILAM i kind e) =
    ((equivIExprs expr sub_expr) || (isEquivIExprIncluded sub_expr e))
isEquivIExprIncluded sub_expr expr@(ICon _ _ _) =
    (equivIExprs expr sub_expr)
isEquivIExprIncluded sub_expr expr@(IRefT t p poss r) =
    (equivIExprs expr sub_expr)

-- #############################################################################
-- #
-- #############################################################################

extractEquivIExpr :: KnownPhase a => IExpr a -> IExpr a -> [IExpr a]
{-# SPECIALISE extractEquivIExpr :: IExpr PreElab -> IExpr PreElab -> [IExpr PreElab] #-}
{-# SPECIALISE extractEquivIExpr :: IExpr Elab -> IExpr Elab -> [IExpr Elab] #-}
{-# SPECIALISE extractEquivIExpr :: IExpr PostElab -> IExpr PostElab -> [IExpr PostElab] #-}
extractEquivIExpr sub_expr expr@(ILam i t e) = if (equivIExprs sub_expr expr)
                                               then [expr]
                                               else (extractEquivIExpr sub_expr e)

extractEquivIExpr sub_expr expr@(IAps e ts es) = if (equivIExprs sub_expr expr)
                                                 then [expr]
                                                 else (concatMap (extractEquivIExpr sub_expr) es)

extractEquivIExpr sub_expr expr@(IVar _) =  if (equivIExprs sub_expr expr)
                                            then [expr]
                                            else []

extractEquivIExpr sub_expr expr@(ILAM i kind e) =  if (equivIExprs sub_expr expr)
                                                   then [expr]
                                                   else (extractEquivIExpr sub_expr e)

extractEquivIExpr sub_expr expr@(ICon _ _ _) =  if (equivIExprs sub_expr expr)
                                              then [expr]
                                              else []

extractEquivIExpr sub_expr expr@(IRefT t p poss r) =  if (equivIExprs sub_expr expr)
                                                      then [expr]
                                                      else []

-- #############################################################################
-- #
-- #############################################################################

equivIExprs :: KnownPhase a => IExpr a -> IExpr a -> Bool
{-# SPECIALISE equivIExprs :: IExpr PreElab -> IExpr PreElab -> Bool #-}
{-# SPECIALISE equivIExprs :: IExpr Elab -> IExpr Elab -> Bool #-}
{-# SPECIALISE equivIExprs :: IExpr PostElab -> IExpr PostElab -> Bool #-}
equivIExprs e0@(ILam i0 t0 ee0) e1@(ILam i1 t1 ee1) = ((equivId i0 i1) && (t0 == t1) && (ee0 == ee1))
equivIExprs e0@(IAps ee0 ts0 es0) e1@(IAps ee1 ts1 es1) = ((equivIExprs ee0 ee1) && (ts0 == ts1) && (es0 == es1))
equivIExprs e0@(IVar i0) e1@(IVar i1) = (equivId i0 i1)
equivIExprs e0@(ILAM i0 k0 ee0) e1@(ILAM i1 k1 ee1) = ((equivId i0 i1) && (k0 == k1) && (ee0 == ee1))
equivIExprs e0@(ICon i0 t0 info0) e1@(ICon i1 t1 info1) = ((equivId i0 i1) && (cmpC t0 info0 t1 info1 == EQ))
equivIExprs e0@(IRefT t0 p0 poss0 r0) e1@(IRefT t1 p1 poss1 r1) = (p0 == p1)
equivIExprs e0 e1 = False

equivId :: Id -> Id -> Bool
equivId id0 id1 = (id0 == id1) && (getIdProps id0 == getIdProps id1)

-- #############################################################################
-- #
-- #############################################################################
