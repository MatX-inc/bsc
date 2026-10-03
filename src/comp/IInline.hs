module IInline(iInline, iSortDs) where
import Data.List(group, sort, nub)
import Data.Maybe(mapMaybe)
import Util
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.IntMap as IM
import SCC(tsort)
import PPrint
import ErrorUtil
import Id
import ISyntax
import ISyntaxUtil(irulesMap, iDefsMap)
import IInlineUtil(iSubst, iSubstIfc)
import Prim
import Data.Maybe(catMaybes)
-- import Debug.Trace(trace)


iInline :: Bool -> IModule a -> IModule a
iInline inlSimp = iInline1 . iInlineS inlSimp -- . iSortDs
--iInline inlSimp = iInlineAll

-- Sort the defs in dependency order, breaking the ties between
-- independent defs by their position in the list, so that the order
-- coming in (heap order from the evaluator, which is elaboration order)
-- is the order going out as far as the dependencies allow.  The sort runs
-- on the positions: tsort breaks ties with the node's Ord, and for Id that
-- is string-intern order, which depends on which names the process has
-- already met.  The passes after this one keep the list's order, so this
-- order is the one ITransform first sights the defs in.
iSortDs :: IModule a -> IModule a
iSortDs imod@(IModule { imod_local_defs = ds }) =
    let nds = zip [0 :: Int ..] ds
        pos = M.fromList [ (i, n) | (n, IDef i _ _ _) <- nds ]
        -- a reference to anything but a local def orders nothing here
        deps e = mapMaybe (\ i -> M.lookup i pos) (fdVars e)
        g = [ (n, deps e) | (n, IDef _ _ e _) <- nds ]
        m = IM.fromList nds
        get n = case IM.lookup n m of
                Nothing -> internalError ("iSortDs: lookup: " ++ ppReadable n)
                Just d -> d
        name n = let IDef i _ _ _ = get n in i
        ds' = case tsort g of
                Left nss -> internalError ("iSortDs: cyclic definition: " ++
                                           ppReadable (map (map name) nss))
                Right ns -> map get ns
    in  imod { imod_local_defs = ds' }


-- Inline definitions that are simple
iInlineS :: Bool -> IModule a -> IModule a
iInlineS False m = m
iInlineS True imod@(IModule { imod_local_defs = ds,
                              imod_rules      = rs,
                              imod_interface  = ifc}) =
    let smap = M.fromList [ (i, iSubst smap dmap e) | IDef i _ e _ <- ds, not (isKeepId i), simple e ]
        ds' = iDefsMap (iSubst smap dmap) ds
        dmap = M.fromList [ (i, e) | IDef i t e _ <- ds' ]
        ifc' = map (iSubstIfc smap dmap) ifc
        rs' = irulesMap (iSubst smap dmap) rs
        state_vars' = [ (name, sv { isv_iargs = es' })
                      | (name, sv@(IStateVar { isv_iargs = es }))
                            <- imod_state_insts imod,
                        let es' = map (iSubst smap dmap) es ]
        -- concatenation and bit selection are just wires, inline them
        -- this is to improve the performance of the ITransform pass
        -- ITransform should re-CSE anything that it does not simplify
        simple (IAps (ICon _ (ICPrim _ PrimConcat)) _ [e1, e2]) = simple e1 && simple e2
        simple (IAps (ICon _ (ICPrim _ PrimSelect)) _ [e]) = simple e
        simple (IAps (ICon _ (ICPrim _ PrimExtract)) _ [e1,e2,e3]) = simple e1 && simple e2 && simple e3
        -- boolean expressions are just gates, inline them
        simple (IAps (ICon _ (ICPrim _ PrimBAnd)) _ [e1, e2]) = simple e1 && simple e2
        simple (IAps (ICon _ (ICPrim _ PrimBOr)) _ [e1, e2]) = simple e1 && simple e2
        simple (IAps (ICon _ (ICPrim _ PrimBNot)) _ [e]) = simple e
        -- these are noops, inline them
        simple (ICon _ (ICMethArg { })) = True
        simple (ICon _ (ICModPort { })) = True
        simple (ICon _ (ICModParam { })) = True
        simple (ICon _ (ICStateVar { })) = True
        simple (ICon _ (ICValue { })) = True
        simple (ICon _ (ICInt { })) = True
        simple (ICon _ (ICReal { })) = True
        --
        simple _ = False

    in  imod { imod_local_defs  = ds',
               imod_rules       = rs',
               imod_interface   = ifc',
               imod_state_insts = state_vars' }

ruleVars :: IRules a -> [Id]
ruleVars (IRules sps rs) = concatMap leafVars rs
    where leafVars r = iValVars (irule_pred r) ++ iValVars (irule_body r)

varVars :: (a, IStateVar b) -> [Id]
varVars (_, IStateVar { isv_iargs = es }) =
        let vs = concatMap iValVars es
        in  vs ++ vs                -- XXX

-- Inline definitions used once
iInline1 :: IModule a -> IModule a
iInline1 = iInlineUseLimit 1

-- iInlineAll :: IModule a -> IModule a
-- iInlineAll = iInlineUseLimit 100000

iInlineUseLimit :: Int -> IModule a -> IModule a
iInlineUseLimit use_limit
                imod@(IModule { imod_state_insts = itvs,
                                imod_local_defs  = ds,
                                imod_rules       = rs,
                                imod_interface   = ifc }) =
    let ifcRuleVars = concatMap ruleVars (catMaybes [ mrs | IEFace _ _ _ mrs _ _ <- ifc ])
        is = [ i | (IEFace i _ (Just (e, _)) _ _ _) <- ifc ]
             ++ ruleVars rs
             ++ concatMap varVars itvs
             ++ ifcRuleVars
             ++ [ i | (IDef i _ _ _) <- ds, keepEvenUnused i]
        keepEvenUnused :: Id -> Bool
        keepEvenUnused i = hasIdProp i IdP_keepEvenUnused
        defids = S.fromList [ i | IDef i _ _ _ <- ds ]
        dm = M.fromList ([(i, e) | IDef i _ e _ <- ds ] ++
                         [ (i, e) | (IEFace i _ (Just (e,_)) _ _ _) <- ifc ])
        get i = case M.lookup i dm of (Just e) -> e; _-> internalError ("iInlineUseLimit " ++ ppString use_limit ++ ": " ++ ppReadable i)
        step allIds done [] = allIds
        step allIds done (i:pend) =
                if i `S.member` done then
                    step allIds done pend
                else
                    let is = iValVars (get i)
                    in  -- trace ("add " ++ ppReadable (i, is)) $
                        step (is ++ allIds) (S.insert i done) (nub is ++ pend)
        dis = step is S.empty (remOrdDup (sort is))
        ics = [ (i, length is) | is@(i:_) <- group (sort dis), i `S.member` defids ]
        onemap = M.fromList [ (i, iSubst onemap dmap (get i))
                             | (i, n_uses) <- ics, not (isKeepId i),
                               n_uses <= use_limit ]
        ds' = iDefsMap (iSubst onemap dmap) ds
        ifc' = map (iSubstIfc onemap dmap) ifc
        rs' = irulesMap (iSubst onemap dmap) rs
        uses = M.fromList ics
        getc' i = M.findWithDefault 0 i uses
        getc i = {- trace ("getc: " ++ ppReadable (i, getc' i)) $ -} getc' i
        ds'' = filter (\ (IDef i _ _ _) ->
                        let uses = getc i in
                        (keepEvenUnused i || uses > 0 && isKeepId i)
                        || uses > use_limit) ds'
        dmap = M.fromList [(i, e) | IDef i t e _ <- ds']
        state_vars' = [ (name, sv { isv_iargs = es' })
                      | (name, sv@(IStateVar { isv_iargs = es }))
                            <- imod_state_insts imod,
                        let es' = map (iSubst onemap dmap) es ]
        new_imod = imod { imod_local_defs = ds'',
                          imod_rules = rs',
                          imod_interface = ifc',
                          imod_state_insts = state_vars' }
    in new_imod

iValVars :: IExpr a -> [Id]
iValVars (IAps e _ es) = iValVars e ++ concatMap iValVars es
iValVars (ICon i (ICValue { })) = [i]
iValVars (ICon i (ICUndet {imVal = Just e})) = iValVars e
iValVars (ICon _ _) = []
iValVars e = internalError ("iValVars: " ++ ppReadable e)

-- #############################################################################
-- #
-- #############################################################################

-- zshow   (IAps (ICon _ (ICPrim _ PrimFmtConcat)) _ [e0, e1]) = text "(FmtConcat "  <+> (zshow e0) <+> (zshow e1) <+> text ")"
-- zshow x@(IAps (ICon _ (ICPrim _ PrimIf)) _ [cond, e0, e1]) = text "(? " <> (zshow e0) <+> text " : " <+> (zshow e1) <+> text ")"
-- zshow   (IAps (ICon fid f@(ICForeign {fcallNo = n, iConType = ict})) ts es) = text (pfpString (unQualId fid)) <> text (show n) <> text "# (" <> sep (map zshow es) <> text ")"
-- zshow   (IAps _ _ [e]) | isAVFFWithFmts e = text("<ZZ ") <> (zshow e) <> text(">")
-- zshow   (IAps icJoinActions _ es@(a:b:rest))  = text("action { ") <> sep (map zshow es) <> text("} endaction")
-- zshow   (ICon _ ICString {iStr = xx}) = text ("\"" ++ xx ++ "\"")
-- zshow   x@(ICon i ICValue {}) = text ("<ref " ++ show i ++ " > ")
-- -- zshow x | (myGetType "K1") x == itFmt = text "<Fmt>"
-- -- zshow x | (myGetType "K1") x == itAction = text ("<Action> " ++ (show x))
-- -- zshow x | isitActionValue_ ((myGetType "K1") x) = text ("<ActionValue_> " ++ (show x))
-- zshow (IAps x ts es) = sep (map zshow es)
-- zshow x = text "<?>"
-- -- zshow x = text ("<?> " ++ ppReadable x)
-- -- zshow x = text (show x)
