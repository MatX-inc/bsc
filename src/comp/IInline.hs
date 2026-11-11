module IInline(iInline, iSortDs, iValVars) where
import Data.List(group, sort, nub, foldl')
import Util
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array(listArray, (!))
import SCC(tsort)
import PPrint
import ErrorUtil
import Id
import ISyntax
import ISyntaxUtil(irulesMap, iDefsMap)
import IInlineUtil(iSubst, iSubstIfc)
import Prim
import Data.Maybe(catMaybes)
import CaptureLive(Cap(..), capOf, CapTable, HostL(..),
                   capMerge, capGone, capDead, viaInlineAlias, viaAtom, viaInline,
                   kindInner, kindConst)
-- import Debug.Trace(trace)


-- The stage is the capture's (CaptureLive), for the defs iInline1 drops.
iInline :: Int -> Bool -> IModule a -> IModule a
iInline stage inlSimp = iInline1 stage . iInlineS inlSimp -- . iSortDs
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
iInline1 :: Int -> IModule a -> IModule a
iInline1 stage = iInlineUseLimit stage 1

-- iInlineAll :: IModule a -> IModule a
-- iInlineAll = iInlineUseLimit 100000

iInlineUseLimit :: Int -> Int -> IModule a -> IModule a
iInlineUseLimit stage use_limit
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
        -- Each def Id's last place in ds, as M.fromList keeps; an interface
        -- entry in dm replaces a def's.  `sort is` forces every key before
        -- these maps are used, so their shapes force nothing.
        dpos = M.fromList [ (i, k) | (k, IDef i _ _ _) <- zip [0 :: Int ..] ds ]
        nds = length ds
        dsArr = listArray (0, nds - 1) ds
        ds'Arr = listArray (0, nds - 1) ds'
        defExpr (IDef _ _ e _) = e
        defids = M.keysSet dpos
        dm0 = M.map (\ k -> defExpr (dsArr ! k)) dpos
        dm = dm0 `seq`
             foldl' (\ m (i, e) -> M.insert i e m) dm0
                    [ (i, e) | (IEFace i _ (Just (e,_)) _ _ _) <- ifc ]
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
        dmap = M.map (\ k -> defExpr (ds'Arr ! k)) dpos
        state_vars' = [ (name, sv { isv_iargs = es' })
                      | (name, sv@(IStateVar { isv_iargs = es }))
                            <- imod_state_insts imod,
                        let es' = map (iSubst onemap dmap) es ]
        cap = inlineCapture stage get uses ds ds'' imod
                            (capOf (imod_capture imod))
        new_imod = imod { imod_local_defs = ds'',
                          imod_rules = rs',
                          imod_interface = ifc',
                          imod_state_insts = state_vars',
                          imod_capture = Cap cap }
    in new_imod

-- The capture's view of iInlineUseLimit: each def it drops was inlined
-- into its one use (an alias of the def or port it names, a constant, or
-- an inner record of the user) or was unused.  Reads the dropped defs'
-- bodies only as deep as the use count's iValVars did, and the users'
-- references as that count found them.
inlineCapture :: Int -> (Id -> IExpr a) -> M.Map Id Int ->
                 [IDef a] -> [IDef a] -> IModule a -> CapTable -> CapTable
inlineCapture stage get uses ds ds'' imod t0 =
    let kept = S.fromList [ i | IDef i _ _ _ <- ds'' ]
        gone = dedup S.empty [ i | IDef i _ _ _ <- ds, not (S.member i kept) ]
        dedup _ [] = []
        dedup seen (i:is) | S.member i seen = dedup seen is
                          | otherwise = i : dedup (S.insert i seen) is
        -- a dropped def with a use was inlined (production keeps one with
        -- more uses than the limit, or a used keep Id); read from the
        -- counts, so that isKeepId is never read here
        inlined i = M.member i uses
        inners = S.fromList [ i | i <- gone, inlined i, not (isAtom (get i)) ]
        isAtom (ICon _ (ICValue {})) = True
        isAtom (ICon _ _) = True
        isAtom _ = False
        -- the user of each inner def
        users = [ (HLId u, [get u]) | u <- M.keys uses ] ++
                [ (HLId (irule_name r), [irule_pred r, irule_body r])
                | let IRules _ rs = imod_rules imod, r <- rs ] ++
                [ (HLId i, maybe [] ((:[]) . fst) me ++
                           concat [ [irule_pred r, irule_body r]
                                  | Just (IRules _ rs) <- [mrs], r <- rs ])
                | IEFace i _ me mrs _ _ <- imod_interface imod ] ++
                [ (HLInst inst k, [e])
                | (inst, IStateVar { isv_iargs = es }) <- imod_state_insts imod,
                  (k, e) <- zip [0..] es ]
        hosts | S.null inners = M.empty
              | otherwise = M.fromList [ (r, h) | (h, es) <- users, e <- es,
                                                 r <- iValVars e, S.member r inners ]
        -- an alias of an alias that also went: the one that stayed
        aliasOf = M.fromList [ (i, j) | i <- gone, inlined i,
                                        ICon j (ICValue {}) <- [get i] ]
        resolve :: Int -> Id -> Id
        resolve n j | n > 64 = j
                    | otherwise = case M.lookup j aliasOf of
                                    Just k | k /= j -> resolve (n + 1) k
                                    _ -> j
        -- the merges first, so that a name merged into a def that went
        -- too leaves with that def's record
        merge t i
          | not (inlined i) = t
          | otherwise =
              case get i of
                ICon j (ICValue {}) -> capMerge stage viaInlineAlias (resolve 0 j) [i] t
                ICon p ic | isPort ic -> capMerge stage viaAtom p [i] t
                _ -> t
        leave t i
          | not (inlined i) = capDead stage i t
          | otherwise =
              case get i of
                ICon _ (ICValue {}) -> t
                ICon _ ic | isPort ic -> t
                ICon _ _ -> capGone kindConst stage viaAtom i [] t
                _ -> capGone kindInner stage viaInline i
                             (maybe [] (:[]) (M.lookup i hosts)) t
        isPort (ICModPort {}) = True
        isPort (ICMethArg {}) = True
        isPort (ICModParam {}) = True
        isPort (ICStateVar {}) = True
        isPort _ = False
    in  foldl' leave (foldl' merge t0 gone) gone

-- The references in e, left to right.  The list is built onto an
-- accumulator: appending each subterm's list copied it once per enclosing
-- node, which is quadratic in the depth of long action and if chains.
iValVars :: IExpr a -> [Id]
iValVars e0 = go e0 []
  where go (IAps e _ es) acc = go e (foldr go acc es)
        go (ICon i (ICValue { })) acc = i : acc
        go (ICon i (ICUndet {imVal = Just e})) acc = go e acc
        go (ICon _ _) acc = acc
        go e _ = internalError ("iValVars: " ++ ppReadable e)

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
