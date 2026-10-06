{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
{-# LANGUAGE MonoLocalBinds #-}
module IInlineUtil(iSubst, iSubstWhen, iSubstIfc) where
import qualified Data.Map as M

import ISyntax
import ISyntaxUtil(irulesMap)
import Id
import PPrint(ppReadable)
import Util(fromJustOrErr)

iSubst :: M.Map Id (IExpr PostElab) -> M.Map Id (IExpr PostElab) -> IExpr PostElab -> IExpr PostElab
iSubst = iSubstWhen (const True)

-- TODO: The tst could be removed from this by cleaning up the smap in the callers
-- See https://github.com/B-Lang-org/bsc/pull/107#discussion_r390037780
iSubstWhen :: (IExpr PostElab -> Bool) -> M.Map Id (IExpr PostElab) -> M.Map Id (IExpr PostElab) -> IExpr PostElab -> IExpr PostElab
iSubstWhen tst subMap defMap e = sub e
  where sub (IAps f ts es) = IAps (sub f) ts (map sub es)
        sub d@(ICon i t val@(ICValue {})) =
            case M.lookup i subMap of
            Nothing ->
              let ev = fromJustOrErr ("iSubstWhen ICValue def not found: " ++ ppReadable i)
                                     (M.lookup i defMap)
              in ICon i t (val { iValDef = ev })
            Just e -> if tst e then e else d
        sub c@(ICon {}) = c


iSubstIfc :: M.Map Id (IExpr PostElab) -> M.Map Id (IExpr PostElab) -> IEFace PostElab -> IEFace PostElab
iSubstIfc smap dmap (IEFace i xs me mrs wp fi) = IEFace i xs me' mrs' wp fi
  where me'  = fmap (\(e, t) -> (iSubst smap dmap e, t)) me
        mrs' = fmap (irulesMap (iSubst smap dmap)) mrs
