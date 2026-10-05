{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module BExpr(BExpr, bNothing, bAdd, bImplies, bImpliesB) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import Util(isOrdSubset, mergeOrdNoDup)
import PPrint
import ISyntax
import ISyntaxUtil
--import BDD
import Prim

--import Debug.Trace


-- A BExpr records information when is know to be true.
-- bNothing is no information
-- bAdd adds an additional fact
-- bImplies checks if the know facts implies an expression.
--  bImplies is allowed to answer False even if the implication
--  is true, but not the other way around.

bNothing :: KnownPhase a => BExpr a
{-# SPECIALISE bNothing :: BExpr Elab #-}
{-# SPECIALISE bNothing :: BExpr PostElab #-}
bAdd :: KnownPhase a => IExpr a -> BExpr a -> BExpr a
{-# SPECIALISE bAdd :: IExpr Elab -> BExpr Elab -> BExpr Elab #-}
{-# SPECIALISE bAdd :: IExpr PostElab -> BExpr PostElab -> BExpr PostElab #-}
bImplies :: KnownPhase a => BExpr a -> IExpr a -> Bool
{-# SPECIALISE bImplies :: BExpr Elab -> IExpr Elab -> Bool #-}
{-# SPECIALISE bImplies :: BExpr PostElab -> IExpr PostElab -> Bool #-}
bImpliesB :: KnownPhase a => BExpr a -> BExpr a -> Bool
{-# SPECIALISE bImpliesB :: BExpr Elab -> BExpr Elab -> Bool #-}
{-# SPECIALISE bImpliesB :: BExpr PostElab -> BExpr PostElab -> Bool #-}

{-
-- This implementation is exact, but slow.
newtype BExpr = B (BDD IExpr)

instance PPrint BExpr where
    pPrint d p _ = text "BExpr"

toBExpr :: IExpr -> BDD IExpr
toBExpr (IAps (ICon _ (ICPrim _ PrimBAnd)) _ [e1, e2]) = bddAnd (toBExpr e1) (toBExpr e2)
toBExpr (IAps (ICon _ (ICPrim _ PrimBOr))  _ [e1, e2]) = bddOr  (toBExpr e1) (toBExpr e2)
toBExpr (IAps (ICon _ (ICPrim _ PrimBNot)) _ [e]) = bddNot (toBExpr e)
toBExpr e = if e == iTrue then bddTrue else if e == iFalse then bddFalse else bddVar e

bNothing = B bddTrue

bAdd e (B bdd) = B (bddAnd (toBExpr e) bdd)

bImplies (B bdd) e = bddIsTrue (bddImplies bdd (toBExpr e))
-}

---------

{-
-- Trivial implementation.

newtype BExpr a = B ()

instance PPrint (BExpr a) where
    pPrint d p _ = text "BExpr"

bNothing = B ()

bAdd _ _ = B ()

bImplies _ e = isTrue e

bImpliesB _ _ = False

---------
-}

-- the conjuncts, as an ordered (by the structural order cmpE) list
-- without duplicates
newtype BExpr a = A [ExprKey a]

instance PPrint (BExpr a) where
    pPrint d p (A es) = text "(B" <+> pPrint d 0 (map unExprKey es) <> text ")"

bNothing = A [ExprKey iTrue]

bAdd e (A es) = A $ mergeOrdNoDup (get e) es

bImplies (A es) e =
--        if length es > 1 then trace (ppReadable (e, es, isOrdSubset (get e) es)) $ isOrdSubset (get e) es
        isOrdSubset (get e) es

bImpliesB b (A es) = all (bImplies b . unExprKey) es

get :: KnownPhase a => IExpr a -> [ExprKey a]
{-# SPECIALISE get :: IExpr Elab -> [ExprKey Elab] #-}
{-# SPECIALISE get :: IExpr PostElab -> [ExprKey PostElab] #-}
get = getAnds . norm

getAnds :: KnownPhase a => IExpr a -> [ExprKey a]
{-# SPECIALISE getAnds :: IExpr Elab -> [ExprKey Elab] #-}
{-# SPECIALISE getAnds :: IExpr PostElab -> [ExprKey PostElab] #-}
getAnds (IAps (ICon _ (ICPrim _ PrimBAnd)) _ [e1, e2]) = mergeOrdNoDup (getAnds e1) (getAnds e2)
getAnds e = [ExprKey e]

norm :: KnownPhase a => IExpr a -> IExpr a
{-# SPECIALISE norm :: IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE norm :: IExpr PostElab -> IExpr PostElab #-}
norm (IAps (ICon _ (ICPrim _ PrimBNot)) _ [e]) = invert e
norm e = e

invert :: KnownPhase a => IExpr a -> IExpr a
{-# SPECIALISE invert :: IExpr Elab -> IExpr Elab #-}
{-# SPECIALISE invert :: IExpr PostElab -> IExpr PostElab #-}
invert (IAps (ICon _ (ICPrim _ PrimBAnd)) _ [e1, e2]) = ieOr  (invert e1) (invert e2)
invert (IAps (ICon _ (ICPrim _ PrimBOr )) _ [e1, e2]) = ieAnd (invert e1) (invert e2)
invert (IAps (ICon _ (ICPrim _ PrimBNot)) _ [e]     ) = e
invert e = ieNot e
