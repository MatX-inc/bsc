module IInline(iInline, iSortDs) where
import Data.List(group, sort, nub, partition)
import Util
import qualified Data.Set as S
import qualified Data.Map as M
import SCC(tsort)
import PPrint
import ErrorUtil
import Id
import ISyntax
import ISyntaxUtil(irulesMap, iDefsMap)
import IInlineUtil(iSubst, iSubstIfc)
import IExpandMeta(PassTag, MetaEvent(..), appendEvents)
import Prim
import Data.Maybe(catMaybes)
-- import Debug.Trace(trace)


-- The tag names the stage that runs the pass on the ledger events it
-- writes: the driver's inline stage, or the transform stage, which runs
-- the single-use inlining at its end.
iInline :: PassTag -> Bool -> IModule a -> IModule a
iInline tag inlSimp imod =
    let (imod_s, simple_inlined) = iInlineS tag inlSimp imod
    in  iInline1 tag simple_inlined imod_s -- . iSortDs
--iInline inlSimp = iInlineAll

-- Sort definitions in dependency order
iSortDs :: IModule a -> IModule a
iSortDs imod@(IModule { imod_local_defs = ds }) =
    let g = [(i, fdVars e) | IDef i _ e _ <- ds ]
        m = M.fromList [(i, d) | d@(IDef i _ _ _) <- ds]
        get i = case M.lookup i m of
                Nothing -> internalError ("iSortDs: lookup: " ++ ppReadable i)
                Just d -> d
        ds' = case tsort g of
                Left iss -> internalError ("iSortDs: cyclic definition: " ++ ppReadable iss)
                Right is -> map get is
    in  imod { imod_local_defs = ds' }


-- Inline definitions that are simple.  The substitution is a map
-- applied to every expression and holds no referrer, so the ledger
-- records each simple def as inlined into users it does not name; the
-- def itself stays in the package, unreferenced, until iInline1 drops
-- it as unused, which is no event of its own (the ids are returned so
-- that iInline1 writes none).  A def kept even unused is substituted
-- too but stays a def, so it has no event.
iInlineS :: PassTag -> Bool -> IModule a -> (IModule a, S.Set Id)
iInlineS _ False m = (m, S.empty)
iInlineS tag True imod@(IModule { imod_local_defs = ds,
                                  imod_rules      = rs,
                                  imod_interface  = ifc}) =
    let smap = M.fromList [ (i, iSubst smap dmap e) | IDef i _ e _ <- ds, not (isKeepId i), simple e ]
        inlined_ids = [ i | i <- M.keys smap, not (hasIdProp i IdP_keepEvenUnused) ]
        events = [ Inlined tag i [] | i <- inlined_ids ]
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

    in  (imod { imod_local_defs  = ds',
                imod_rules       = rs',
                imod_interface   = ifc',
                imod_state_insts = state_vars',
                imod_meta        = appendEvents events (imod_meta imod) },
         S.fromList inlined_ids)

-- the references of each rule, with the rule
ruleVars :: IRules a -> [(Id, [Id])]
ruleVars (IRules sps rs) = map leafVars rs
    where leafVars r = (getIRuleId r,
                        iValVars (irule_pred r) ++ iValVars (irule_body r))

-- the references of an instance's arguments, with the instance
varVars :: (Id, IStateVar b) -> (Id, [Id])
varVars (name, IStateVar { isv_iargs = es }) =
        let vs = concatMap iValVars es
        in  (name, vs ++ vs)        -- XXX

-- Inline definitions used once (simple_inlined: the defs iInlineS has
-- recorded as inlined, unused here and given no second event)
iInline1 :: PassTag -> S.Set Id -> IModule a -> IModule a
iInline1 tag simple_inlined = iInlineUseLimit tag simple_inlined 1

-- iInlineAll :: IModule a -> IModule a
-- iInlineAll = iInlineUseLimit 100000

iInlineUseLimit :: PassTag -> S.Set Id -> Int -> IModule a -> IModule a
iInlineUseLimit tag simple_inlined use_limit
                imod@(IModule { imod_state_insts = itvs,
                                imod_local_defs  = ds,
                                imod_rules       = rs,
                                imod_interface   = ifc }) =
    let -- the roots of the use count: the method values and the defs
        -- kept even unused by their own ids, and the references of the
        -- rules, the instance arguments and the method bodies, each
        -- with the rule or instance that holds them (the user the
        -- ledger names when one is inlined)
        ruleRefs = ruleVars rs
        instRefs = map varVars itvs
        ifcRuleRefs = concatMap ruleVars (catMaybes [ mrs | IEFace _ _ _ mrs _ _ <- ifc ])
        is = [ i | (IEFace i _ (Just (e, _)) _ _ _) <- ifc ]
             ++ concatMap snd ruleRefs
             ++ concatMap snd instRefs
             ++ concatMap snd ifcRuleRefs
             ++ [ i | (IDef i _ _ _) <- ds, keepEvenUnused i]
        keepEvenUnused :: Id -> Bool
        keepEvenUnused i = hasIdProp i IdP_keepEvenUnused
        defids = S.fromList [ i | IDef i _ _ _ <- ds ]
        dm = M.fromList ([(i, e) | IDef i _ e _ <- ds ] ++
                         [ (i, e) | (IEFace i _ (Just (e,_)) _ _ _) <- ifc ])
        get i = case M.lookup i dm of (Just e) -> e; _-> internalError ("iInlineUseLimit " ++ ppString use_limit ++ ": " ++ ppReadable i)
        -- the walk keeps the references of each def or method value it
        -- visits, with the visitor, most recently visited first; a def
        -- is visited after the def, rule or instance it is reached
        -- through, so a def used once comes after its one user
        step allIds refs done [] = (allIds, refs)
        step allIds refs done (i:pend) =
                if i `S.member` done then
                    step allIds refs done pend
                else
                    let is = iValVars (get i)
                    in  -- trace ("add " ++ ppReadable (i, is)) $
                        step (is ++ allIds) ((i, is) : refs) (S.insert i done) (nub is ++ pend)
        (dis, defRefs) = step is [] S.empty (remOrdDup (sort is))
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
        (ds'', gone) = partition (\ (IDef i _ _ _) ->
                        let uses = getc i in
                        (keepEvenUnused i || uses > 0 && isKeepId i)
                        || uses > use_limit) ds'
        dmap = M.fromList [(i, e) | IDef i t e _ <- ds']
        state_vars' = [ (name, sv { isv_iargs = es' })
                      | (name, sv@(IStateVar { isv_iargs = es }))
                            <- imod_state_insts imod,
                        let es' = map (iSubst onemap dmap) es ]
        -- the ledger: a def inlined (in onemap) went into the def,
        -- method value, rule or instance holding its one reference,
        -- which the walk above kept; deepest first, so that a def
        -- inlined into a def that was itself inlined is recorded
        -- before it (defRefs is most recently visited first, and the
        -- roots come after every def).  A def that went without being
        -- inlined had no reachable reference and is dropped, unless
        -- iInlineS substituted it and recorded that already.
        inlined = [ Inlined tag v [u]
                  | (u, vs) <- defRefs ++ ruleRefs ++ instRefs ++ ifcRuleRefs,
                    v <- vs, M.member v onemap ]
        dropped = [ Dropped tag i | IDef i _ _ _ <- gone, not (M.member i onemap),
                                    not (S.member i simple_inlined) ]
        new_imod = imod { imod_local_defs = ds'',
                          imod_rules = rs',
                          imod_interface = ifc',
                          imod_state_insts = state_vars',
                          imod_meta = appendEvents (inlined ++ dropped) (imod_meta imod) }
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
