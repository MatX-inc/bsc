module ISyntaxXRef(
                   updateIExprPosition,
                   usablePosition,
                   fillIExprPosition,
                   spineHeadPosition,
                   restampReaches,
                   mapIExprPosition,
                   mapIExprPosition2,
                   mapIExprPositionConservative
                  ) where

import qualified Data.Set as S

import ISyntax
import Id
import Position(Position, noPosition, isUsefulPosition, firstPos)

-- #############################################################################
-- #
-- #############################################################################

updateIExprPosition :: Position -> IExpr a -> IExpr a
updateIExprPosition pos (ILam i t e) = (ILam (setIdPosition pos i) t (updateIExprPosition pos e))
updateIExprPosition pos (IAps e ts [e0]) = (IAps (updateIExprPosition pos e) ts [(updateIExprPosition pos e0)])
updateIExprPosition pos (IAps e ts es) = (IAps (updateIExprPosition pos e) ts es)
updateIExprPosition pos (IVar i) = (IVar (setIdPosition pos i))
updateIExprPosition pos (ILAM i kind e) = (ILAM (setIdPosition pos i) kind (updateIExprPosition pos e))
updateIExprPosition pos iexpr@(ICon i (ICStateVar t isv)) = iexpr
updateIExprPosition pos (ICon i info) = (ICon (setIdPosition pos i) info)
-- The heap ref's position set carries the stamped position out of
-- band; the type itself is deliberately not restamped.  Rewriting the
-- positions of the Ids inside a type rebuilds the whole type, and
-- once ITypes are hash-consed the rebuild re-canonicalizes to the
-- original node, dropping the stamps anyway.
updateIExprPosition pos (IRefT t p poss r) = (IRefT t p poss' r)
  where poss' = S.insert pos poss

-- A position of the user's own code: not empty and not in the
-- standard library.  Diagnostics report these, and a stamp never
-- replaces one.
usablePosition :: Position -> Bool
usablePosition p = (p /= noPosition) && (isUsefulPosition p)

-- updateIExprPosition's walk, filling positions in rather than
-- replacing them: an Id whose position is usable keeps it, and a heap
-- reference whose stamped positions hold a usable one is left alone.
-- A sole argument lies on the spine the application reduces to, so the
-- stamp reaches it, but one the user wrote (the one field of a
-- constructor application, say) keeps its own position.  A binder is
-- never stamped: no diagnostic is raised at a binder, and the cell
-- the evaluator binds to it is named after it only when its position
-- is not the standard library's (improveCellName), so a library
-- parameter stamped with a use site would name the user's signals.
fillIExprPosition :: Position -> IExpr a -> IExpr a
fillIExprPosition pos (ILam i t e) = ILam i t (fillIExprPosition pos e)
fillIExprPosition pos (IAps e ts [e0]) =
    IAps (fillIExprPosition pos e) ts [fillIExprPosition pos e0]
fillIExprPosition pos (IAps e ts es) = IAps (fillIExprPosition pos e) ts es
fillIExprPosition pos (IVar i) = IVar (fillIdPosition pos i)
fillIExprPosition pos (ILAM i kind e) = ILAM i kind (fillIExprPosition pos e)
fillIExprPosition _ iexpr@(ICon _ (ICStateVar _ _)) = iexpr
fillIExprPosition pos (ICon i info) = ICon (fillIdPosition pos i) info
fillIExprPosition pos iexpr@(IRefT t p poss r)
    | any usablePosition (S.toList poss) = iexpr
    | otherwise = IRefT t p (S.insert pos poss) r

fillIdPosition :: Position -> Id -> Id
fillIdPosition pos i | usablePosition (getIdPosition i) = i
                     | otherwise = setIdPosition pos i

-- The position at the head of the spine that fillIExprPosition
-- stamps: through the binders of a lambda or type abstraction, the
-- function of an application, the Id of a variable or constant, the
-- stamped positions of a heap reference.  A state variable has none,
-- as it is never stamped.
spineHeadPosition :: IExpr a -> Position
spineHeadPosition (ILam _ _ e) = spineHeadPosition e
spineHeadPosition (IAps e _ _) = spineHeadPosition e
spineHeadPosition (IVar i) = getIdPosition i
spineHeadPosition (ILAM _ _ e) = spineHeadPosition e
spineHeadPosition (ICon _ (ICStateVar _ _)) = noPosition
spineHeadPosition (ICon i _) = getIdPosition i
spineHeadPosition (IRefT _ _ poss _) = firstPos (S.toAscList poss)

-- Whether a reference to the variable lies on the part of the
-- expression that updateIExprPosition restamps.  A lambda that
-- rebinds the variable hides the references in its body.
restampReaches :: Id -> IExpr a -> Bool
restampReaches i (ILam i' _ e) = i /= i' && restampReaches i e
restampReaches i (IAps e _ [e0]) = restampReaches i e || restampReaches i e0
restampReaches i (IAps e _ _) = restampReaches i e
restampReaches i (IVar i') = i == i'
restampReaches i (ILAM _ _ e) = restampReaches i e
restampReaches _ (ICon _ _) = False
restampReaches _ (IRefT _ _ _ _) = False


updateIExprPosition2 :: Position -> IExpr a -> IExpr a
updateIExprPosition2 pos (ILam i t e) = (ILam (setIdPosition pos i) t (updateIExprPosition pos e))
updateIExprPosition2 pos iexpr@(IAps e@(ICon i (ICCon _ _)) ts [e0]) =
    if (not (isUsefulPosition (getIdPosition i)))
    then updateIExprPosition pos iexpr
    else iexpr
updateIExprPosition2 pos iexpr@(IAps e@(ICon i (ICPrim _ _)) ts es) =
    if (not (isUsefulPosition (getIdPosition i)))
    then updateIExprPosition pos iexpr
    else iexpr
updateIExprPosition2 pos (IAps e ts [e0]) = (IAps (updateIExprPosition pos e) ts [(updateIExprPosition pos e0)])
updateIExprPosition2 pos (IAps e ts es) = (IAps (updateIExprPosition pos e) ts es)
updateIExprPosition2 pos (IVar i) = (IVar (setIdPosition pos i))
updateIExprPosition2 pos (ILAM i kind e) = (ILAM (setIdPosition pos i) kind (updateIExprPosition pos e))
updateIExprPosition2 pos iexpr@(ICon i (ICStateVar t isv)) = iexpr
updateIExprPosition2 pos (ICon i info) = (ICon (setIdPosition pos i) info)
updateIExprPosition2 pos (IRefT t p poss r) = (IRefT t p poss' r)
  where poss' = S.insert pos poss

mapIExprPosition :: Bool -> (IExpr a, IExpr a) -> IExpr a
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

mapIExprPosition2 :: Bool -> (IExpr a, IExpr a) -> IExpr a
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

mapIExprPositionConservative :: Bool -> (IExpr a,IExpr a) -> IExpr a
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

isEquivIExprIncluded :: IExpr a -> IExpr a -> Bool
isEquivIExprIncluded sub_expr expr@(ILam i t e) =
    ((equivIExprs expr sub_expr) || (isEquivIExprIncluded sub_expr e))
isEquivIExprIncluded sub_expr expr@(IAps e ts es) =
    ((equivIExprs expr sub_expr) || (or (map (equivIExprs sub_expr) es)))
isEquivIExprIncluded sub_expr expr@(IVar _) =
    (equivIExprs expr sub_expr)
isEquivIExprIncluded sub_expr expr@(ILAM i kind e) =
    ((equivIExprs expr sub_expr) || (isEquivIExprIncluded sub_expr e))
isEquivIExprIncluded sub_expr expr@(ICon _ _) =
    (equivIExprs expr sub_expr)
isEquivIExprIncluded sub_expr expr@(IRefT t p poss r) =
    (equivIExprs expr sub_expr)

-- #############################################################################
-- #
-- #############################################################################

extractEquivIExpr :: IExpr a -> IExpr a -> [IExpr a]
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

extractEquivIExpr sub_expr expr@(ICon _ _) =  if (equivIExprs sub_expr expr)
                                              then [expr]
                                              else []

extractEquivIExpr sub_expr expr@(IRefT t p poss r) =  if (equivIExprs sub_expr expr)
                                                      then [expr]
                                                      else []

-- #############################################################################
-- #
-- #############################################################################

equivIExprs :: IExpr a -> IExpr a -> Bool
equivIExprs e0@(ILam i0 t0 ee0) e1@(ILam i1 t1 ee1) = ((equivId i0 i1) && (t0 == t1) && (ee0 == ee1))
equivIExprs e0@(IAps ee0 ts0 es0) e1@(IAps ee1 ts1 es1) = ((equivIExprs ee0 ee1) && (ts0 == ts1) && (es0 == es1))
equivIExprs e0@(IVar i0) e1@(IVar i1) = (equivId i0 i1)
equivIExprs e0@(ILAM i0 k0 ee0) e1@(ILAM i1 k1 ee1) = ((equivId i0 i1) && (k0 == k1) && (ee0 == ee1))
equivIExprs e0@(ICon i0 info0) e1@(ICon i1 info1) = ((equivId i0 i1) && (info0 == info1))
equivIExprs e0@(IRefT t0 p0 poss0 r0) e1@(IRefT t1 p1 poss1 r1) = (p0 == p1)
equivIExprs e0 e1 = False

equivId :: Id -> Id -> Bool
equivId id0 id1 = (id0 == id1) && (getIdProps id0 == getIdProps id1)

-- #############################################################################
-- #
-- #############################################################################
