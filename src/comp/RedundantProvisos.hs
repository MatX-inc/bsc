-- Warn (with -warn-redundant-provisos) about the provisos of a top-level
-- definition that survive context reduction, and so become dictionary
-- parameters, but that the typechecked body never uses.  Provisos that
-- context reduction removes cost nothing and are not reported.  The check
-- reduces nothing itself: it reads the signature provisos that context
-- reduction kept (with their ancestry back to the written provisos) and
-- the typechecked package, and it stays silent whenever it cannot be sure.
-- It reports only written provisos that can be deleted whole, and never
-- one whose own class passes no dictionary (a numeric class, or a class
-- with no methods or superclasses): such a proviso asserts sizes or serves
-- typing, and an unused dictionary under it comes from a type function in
-- its arguments.  Nor is one whose fundep determines a type variable that
-- the type or a proviso that stays also names, or that another reported
-- proviso also determines, as the size in (Bits t n, Add n _pad (SizeOf r))
-- or (Bits a n, Bits b n): it relates that variable to the rest of the
-- signature.
--
-- What is reported depends on what the body needs, not on which dictionary
-- the typechecker took it from.  A superclass selected from one dictionary
-- parameter counts as a use of the parameter for that superclass, when the
-- definition has one, and otherwise of every parameter that has it as a
-- superclass.  A proviso is then redundant when its dictionary is unused,
-- or when another proviso that stays has it as a superclass: with
-- (Arith a, Literal a), a body that uses only a literal makes Arith a
-- redundant, and one that also adds makes Literal a redundant, whichever
-- dictionary the literal was taken from.
--
-- The flag is a trace flag, not part of Flags, so adding it does not make
-- -u recompile a package that is otherwise up to date.
module RedundantProvisos(
        warnRedundantProvisos,
        checkRedundantProvisos
    ) where

import Control.Monad(when)
import Data.List(isInfixOf, nub)
import qualified Data.Map as M
import qualified Data.Set as S

import Assump(Assump(..))
import CFreeVars(getFVE, fvSetToFreeVars, getFTCC)
import CSyntax
import CType(allTyCons, noTyVarNo, CPred(..))
import CtxRed(SignProvisos(..))
import Error(EMsg, ErrMsg(..), ErrorHandle, bsWarning)
import GenWrapUtils(genSuffix)
import Id
import IOUtil(progArgs)
import PFPrint
import Pragma(Pragma(..))
import Pred
import Scheme(Scheme(..))
import StdPrel(isPreClass)
import Subst(Types(..), mkSubst)
import SymTab(SymTab, findType, findField, findCon, findVar, findSClass,
              TypeInfo(..), FieldInfo(..), ConInfo(..), VarInfo(..))

warnRedundantProvisos :: Bool
warnRedundantProvisos = "-warn-redundant-provisos" `elem` progArgs

-- The packages are as written (before context reduction) and as
-- typechecked; context reduction of the former gave the signature provisos.
checkRedundantProvisos :: ErrorHandle -> SymTab -> CPackage -> [SignProvisos] ->
                          CPackage -> IO ()
checkRedundantProvisos errh symt user_pkg sps tc_pkg = do
    let CPackage _ _ _ _ _ uds _ = user_pkg
        CPackage _ _ _ _ _ tds _ = tc_pkg
        names = S.fromList [ getIdBaseString i | Right i <- map getName uds ]
        pragma_ids = S.fromList (concat [ pragmaIds p | CPragma p <- uds ])
        -- Exclusion: synthesize and noinline boundaries fix their own types
        -- (GenWrap and GenFuncWrap add a companion with a suffixed name).
        boundary i = i `S.member` pragma_ids ||
                     genSuffix `isInfixOf` getIdBaseString i ||
                     (getIdBaseString i ++ genSuffix) `S.member` names
        user_defs = M.fromList [ (i, d) | CValueSign d@(CDef i _ _) <- uds ]
        tc_defs = M.fromList [ (i, d) | d@(CValueSign (CDefT i _ _ _)) <- tds ]
        msgs = concat [ checkDef symt d sp tc
                      | sp <- sps, not (null (sp_reduced sp)),
                        let i = sp_id sp, not (boundary i),
                        Just d <- [M.lookup i user_defs],
                        Just tc <- [M.lookup i tc_defs] ]
    when (not (null msgs)) $ bsWarning errh msgs
  where
    pragmaIds (Pproperties i _) = [i]
    pragmaIds (Pnoinline is) = is

-- the given type constructors and those their synonyms expand to
closeSyns :: SymTab -> S.Set Id -> [Id] -> S.Set Id
closeSyns _ seen [] = seen
closeSyns symt seen (c:cs)
    | c `S.member` seen = closeSyns symt seen cs
    | otherwise =
        let more = case findType symt c of
                     Just ti | TItype _ t <- ti_sort ti ->
                         [ i | TyCon i _ _ <- allTyCons t ]
                     _ -> []
        in  closeSyns symt (S.insert c seen) (more ++ cs)

checkDef :: SymTab -> CDef -> SignProvisos -> CDefn -> [EMsg]
checkDef symt (CDef i (CQType _ ut) _) (SignProvisos _ written reduced rt) tc
    | Just used <- usedDicts preds tc = analyse used
  where
    ks = [0 .. length reduced - 1]
    preds = map removePredPositions reduced
    rpred k = preds !! k
    clauses = case tc of
                CValueSign (CDefT _ _ _ cs) -> cs
                _ -> []
    -- type variables the typechecked body mentions (an improvement the
    -- body relied on shows up here even without a dictionary)
    body_tvs = case clauses of
                 [CClause _ [] e] -> tv e
                 _ -> []
    visible = S.fromList (tv ut ++ tv rt ++ body_tvs)
    -- type constructors the type and the body mention, through synonyms,
    -- including those in the declared types of the variables, methods,
    -- fields and constructors the body uses (typing a use can need the
    -- associated types its declared type names)
    tycons = closeSyns symt S.empty
               ([ c | TyCon c _ _ <- concatMap allTyCons
                                       (ut : rt : concatMap (refTypes symt) refs) ] ++
                S.toList (S.unions (map getFTCC clauses)))
    refs = concatMap refsC clauses

    -- Exclusion: numeric classes and classes with no methods or
    -- superclasses have no dictionary to pass, so they are not reported on
    -- their own.
    memberless (IsIn c _) = memberlessClass c
    emptyDict (IsIn c _) = noDictClass c
    -- Exclusion: a proviso that typing may need although its dictionary is
    -- unused.  A memberless class with a fundep or an associated type exists
    -- only for typing; if typing resolves any associated type (SizeOf
    -- included), the instance that resolves it may need such a proviso
    -- without a trace in the typechecked body, so it stays unless another
    -- supplies it.  Otherwise, a class whose associated type the type, the
    -- body or a field the body uses names, or a fundep that determines a
    -- type variable seen elsewhere (the type, the body, or a proviso that
    -- stays).
    typingOnly p@(IsIn c _) =
        not (isPreClass c) && memberless p &&
        (any or (funDeps c) || not (null (assocTypes c))) && resolvesAssoc
    resolvesAssoc = any isAssoc (S.toList tycons)
    isAssoc c = case findType symt c of
                  Just ti | TIatf {} <- ti_sort ti -> True
                  _ -> False
    namesAssoc (IsIn c _) =
        or [ a `S.member` tycons | (TyCon a _ _, _, _) <- assocTypes c ]
    determined (IsIn c ts) =
        [ v | row <- funDeps c, (True, t) <- zip row ts, v <- tv t ]

    -- the written provisos each reduced proviso comes from: itself, or the
    -- roots of its reduction ancestry (several, if reduction joined them)
    direct_roots = [ [ w | (w, (_, wps)) <- zip [0 :: Int ..] written,
                           or [ samePred q wp
                              | q <- p : [ q' | PredAncestor q' _ <- anc ],
                                wp <- wps ] ]
                   | PredWithPositions p _ anc <- reduced ]
    -- Expanding a type function or an associated type during reduction
    -- records no ancestry, but the provisos it makes share fresh type
    -- variables (ones the signature does not name) with those they came
    -- with; such a proviso takes the roots of the attributed ones that share
    -- a fresh variable with it, directly or through other such provisos.
    sig_tvs = S.fromList (tv ut ++ concat [ tv ts | (CPred _ ts, _) <- written ])
    fresh_list = [ S.fromList (filter (not . (`S.member` sig_tvs)) (tv p))
                 | p <- preds ]
    unattributed_ks = [ k | k <- ks, null (direct_roots !! k) ]
    connected vs j = not (S.null (S.intersection vs (fresh_list !! j)))
    inherited k = grow (S.singleton k) (fresh_list !! k)
      where grow g vs =
              let g' = S.union g (S.fromList [ j | j <- unattributed_ks,
                                                   connected vs j ])
                  vs' = S.unions (map (fresh_list !!) (S.toList g'))
              in  if S.size g' == S.size g
                  then nub (concat [ r | (j, r) <- zip ks direct_roots,
                                         not (null r), connected vs j ])
                  else grow g' vs'
    roots_list = [ if null r then inherited k else r
                 | (k, r) <- zip ks direct_roots ]
    roots k = roots_list !! k
    supers_list = map superClosure preds
    wis = [0 .. length written - 1]
    from_w w = [ k | k <- ks, w `elem` roots k ]

    def_str = pfpString (unQualId i)
    wcpred w = fst (written !! w)
    -- the written provisos whose own class passes a dictionary
    dictW w = maybe False (not . noDictClass)
                    (findSClass symt (cpred_tc (wcpred w)))
    -- the type variables of a written proviso, and those its fundeps
    -- determine
    varsW w = let CPred _ ts = wcpred w in tv ts
    detW w =
        case findSClass symt (cpred_tc cp) of
          Just c -> [ v | row <- funDeps c, (True, t) <- zip row ts, v <- tv t ]
          Nothing -> []
      where cp@(CPred _ ts) = wcpred w
    -- Exclusion: a written proviso whose fundep determines a type variable
    -- (a size, for Bits) that the type or a written proviso that stays also
    -- names, or that another one deleted with it also determines.  It
    -- relates the variable to the rest of the signature: deleting it would
    -- leave the variable ambiguous (T0079) or drop the relation.
    linked dels w = any (`S.member` others) (detW w)
      where others = S.fromList (tv ut ++
                                 concat [ if w' `elem` dels then detW w' else varsW w'
                                        | w' <- wis, w' /= w ])

    -- Only written provisos that can go whole are reported: one that
    -- reduces to something typing needs, or a reduced proviso traced to no
    -- written one, would have to be rewritten, not deleted.
    analyse used = map msg dels_final
      where
        unused k = not (used !! k)
        -- another proviso that the body uses has it as a superclass
        impliedByUsed k =
            or [ sameUpToFundeps (rpred k) q | j <- ks, j /= k, used !! j,
                                               q <- tail (supers_list !! j) ]
        reportable k = (unused k || impliedByUsed k) && not (emptyDict (rpred k))
        -- whether reduced proviso k can go while those in "staying" stay:
        -- it is unused and typing does not need it, or one that stays
        -- supplies it (itself or as a superclass)
        canGo staying k =
            (unused k && not (typingOnly p) && not (namesAssoc p) &&
             not (any (`S.member` seen) (determined p))) ||
            or [ sameUpToFundeps p q | j <- S.toList staying, j /= k,
                                       q <- supers_list !! j ]
          where p = rpred k
                seen = visible `S.union`
                       S.fromList (tv [ rpred j | j <- S.toList staying, j /= k ])
        -- the reduced provisos that stay when the written ones in "dels"
        -- are deleted (those whose source is unknown stay)
        stayingFor dels =
            S.fromList [ k | k <- ks, null (roots k) ||
                                      any (`notElem` dels) (roots k) ]
        -- Report as redundant the written provisos that can be deleted
        -- together: every reduced proviso that goes with them can go.  Start
        -- from those that supply a reported one, and give up the last that
        -- fails until the rest hold.
        settle dels =
            let staying = stayingFor dels
                fails w = linked dels w ||
                          not (all (canGo staying)
                                   [ k | k <- from_w w,
                                         not (k `S.member` staying) ])
            in  case reverse (filter fails dels) of
                  [] -> dels
                  (w:_) -> settle (filter (/= w) dels)
        cands = [ w | w <- wis, dictW w, any reportable (from_w w) ]
        dels_final = settle cands
        staying_final = stayingFor dels_final
        -- a proviso that stays and supplies the used reduced provisos that
        -- go with w, if any (the first written one, else a reduced one)
        supplier w =
            let goes = [ k | k <- from_w w, not (k `S.member` staying_final),
                             not (unused k) ]
                sups = [ j | k <- goes, j <- S.toList staying_final, j /= k,
                             any (sameUpToFundeps (rpred k)) (supers_list !! j) ]
                written_sups = concatMap roots sups
            in  case (written_sups, sups) of
                  ((_:_), _) -> Just (pfpString (wcpred (minimum written_sups)))
                  ([], (j:_)) -> Just (pfpString (rpred j))
                  ([], []) -> Nothing
        msg w = (getPosition (wcpred w),
                 WRedundantProviso def_str (pfpString (wcpred w)) (supplier w))
checkDef _ _ _ _ = []

-- A class that passes no dictionary: a numeric class, or one with no
-- methods or superclasses
noDictClass :: Class -> Bool
noDictClass c = isPreClass c || memberlessClass c

memberlessClass :: Class -> Bool
memberlessClass c =
    null (super c) && case tyConOf c of
                        TyCon _ _ (TIstruct SClass ms) -> null ms
                        _ -> False

-- Predicate equality that identifies a generated type variable by its
-- number alone: comparing names would spell out (and so intern) names that
-- the compile itself never forces.
samePred :: Pred -> Pred -> Bool
samePred (IsIn c ts) (IsIn c' ts') =
    name c == name c' && length ts == length ts' && and (zipWith sameType ts ts')

-- As samePred, but the arguments that a functional dependency determines
-- from the others may differ: the dependency forces them equal.
sameUpToFundeps :: Pred -> Pred -> Bool
sameUpToFundeps (IsIn c ts) (IsIn c' ts') =
    name c == name c' && length ts == length ts' &&
    or [ and [ sameType t t' | (False, t, t') <- zip3 row ts ts' ]
       | row <- funDeps c ]

sameType :: Type -> Type -> Bool
sameType (TVar (TyVar i n _)) (TVar (TyVar i' n' _)) =
    n == n' && (n /= noTyVarNo || i == i')
sameType (TAp l r) (TAp l' r') = sameType l l' && sameType r r'
sameType (TCon tc) (TCon tc') = tc == tc'
sameType (TGen _ g) (TGen _ g') = g == g'
sameType _ _ = False

-- A predicate and all its superclasses.
superClosure :: Pred -> [Pred]
superClosure p@(IsIn c ts) =
    p : concatMap (superClosure . apSub s . snd) (super c)
  where s = mkSubst (zip (csig c) ts)

-- Whether the typechecked definition uses each of its dictionary
-- parameters (one per context-reduced proviso, in order).  Passing the
-- dictionary on counts as a use.  A superclass selected from a dictionary
-- counts as a use of the parameter for that superclass, if there is one,
-- and otherwise of every parameter that has it as a superclass, so the
-- answer does not depend on which dictionary the typechecker selected it
-- from (TCMisc.bySuperE).  A selection bound to a variable that nothing
-- uses is not a use.  A parameter that occurs in the body only where the
-- walk below does not look is counted as used.
usedDicts :: [Pred] -> CDefn -> Maybe [Bool]
usedDicts preds (CValueSign (CDefT _ _ _ [CClause ps [] body]))
    | length ps == length preds, Just ds <- mapM pvar ps =
        let fvs = S.fromList (fvSetToFreeVars (getFVE body))
            refs = refsE body
            idx = M.fromList (zip ds [0 :: Int ..])
            selected = [ (mv, d, path) | RSuper mv d path <- refs ]
            -- a selection from a variable that is not a parameter uses it
            direct = S.fromList ([ v | RVar v <- refs ] ++
                                 [ d | (_, d, _) <- selected, M.notMember d idx ])
            live (Just v) = v `S.member` direct
            live Nothing = True
            inSuper = S.fromList [ d | (_, d, _) <- selected ]
            superUses = S.fromList
                          [ k | (mv, d, path) <- selected, live mv,
                                Just j <- [M.lookup d idx], k <- superUsers j path ]
            usedK k d = (d `S.member` fvs &&
                         (d `S.member` direct || not (d `S.member` inSuper))) ||
                        k `S.member` superUses
        in  Just (zipWith usedK [0 ..] ds)
  where pvar (CPVar d) = Just d
        pvar _ = Nothing
        ips = zip [0 :: Int ..] preds
        superUsers j path =
            case foldl selectSuper (Just (preds !! j)) path of
              Nothing -> [j]
              Just p ->
                case [ k | (k, q) <- ips, sameUpToFundeps q p ] of
                  [] -> [ k | (k, q) <- ips,
                              any (sameUpToFundeps p) (tail (superClosure q)) ]
                  ks -> ks
usedDicts _ _ = Nothing

-- The superclass with the given name of a predicate's class
selectSuper :: Maybe Pred -> (Id, Id) -> Maybe Pred
selectSuper mp (_, s) = do
    IsIn c ts <- mp
    let sub = mkSubst (zip (csig c) ts)
    case [ apSub sub sp | (CTypeclass i, sp) <- super c, i == s ] of
      (sp:_) -> Just sp
      [] -> Nothing

-- A variable and the superclasses selected from it in turn (class and
-- superclass ids), as the typechecker writes the selection
superSelection :: CExpr -> Maybe (Id, [(Id, Id)])
superSelection (CVar d) = Just (d, [])
superSelection (CApply (CTApply (CSelectT c s) _) [e]) = do
    (d, path) <- superSelection e
    return (d, path ++ [(c, s)])
superSelection _ = Nothing

-- A variable, a field (of the given type, if known) or a constructor that
-- the body uses, selects, constructs, updates or matches, or a superclass
-- selected from a variable (bound to a variable by a let, if it is).
data Ref = RVar Id | RField (Maybe Id) Id | RCon Id
         | RSuper (Maybe Id) Id [(Id, Id)]

-- the declared types of a variable, field or constructor
refTypes :: SymTab -> Ref -> [Type]
refTypes symt (RVar v) =
    case findVar symt v of
      Just vi | (_ :>: Forall _ (qs :=> t)) <- vi_assump vi ->
          t : concat [ ts | IsIn _ ts <- map removePredPositions qs ]
      _ -> []
refTypes _ (RSuper _ _ _) = []
refTypes symt (RField mti f) =
    let fis = maybe [] id (findField symt f)
        fis' = [ fi | Just ti <- [mti], fi <- fis, fi_id fi == ti ]
    in  concat [ schemeType (fi_assump fi) : maybe [] (: []) (fi_orig_type fi)
               | fi <- if null fis' then fis else fis' ]
refTypes symt (RCon c) =
    [ schemeType (ci_assump ci) | ci <- maybe [] id (findCon symt c) ]

schemeType :: Assump -> Type
schemeType (_ :>: Forall _ (_ :=> t)) = t

refsC :: CClause -> [Ref]
refsC (CClause ps qs e) = concatMap refsP ps ++ concatMap refsQ qs ++ refsE e

refsE :: CExpr -> [Ref]
refsE expr | Just (d, path@(_:_)) <- superSelection expr = refsSuper Nothing d path
refsE expr =
  case expr of
    CVar v -> [RVar v]
    CLam _ e -> refsE e
    CLamT _ _ e -> refsE e
    Cletseq ds e -> concatMap refsDl ds ++ refsE e
    Cletrec ds e -> concatMap refsDl ds ++ refsE e
    CSelect e f -> RField Nothing f : refsE e
    CCon c es -> RCon c : concatMap refsE es
    Ccase _ e as -> refsE e ++ concat [ refsP p ++ concatMap refsQ qs ++ refsE c
                                      | CCaseArm p qs c <- as ]
    CStruct _ ti fs -> [ RField (Just ti) f | (f, _) <- fs ] ++
                       concatMap (refsE . snd) fs
    CStructUpd e fs -> [ RField Nothing f | (f, _) <- fs ] ++
                       concatMap refsE (e : map snd fs)
    Cwrite _ l r -> refsE l ++ refsE r
    CApply f es -> concatMap refsE (f:es)
    CTaskApply f es -> concatMap refsE (f:es)
    CTaskApplyT f _ es -> concatMap refsE (f:es)
    CBinOp l o r -> RVar o : refsE l ++ refsE r
    CHasType e _ -> refsE e
    Cif _ c t e -> concatMap refsE [c, t, e]
    CSub _ v i -> refsE v ++ refsE i
    CSub2 v h l -> concatMap refsE [v, h, l]
    CSubUpdate _ v (h, l) r -> concatMap refsE [v, h, l, r]
    Cmodule _ ss -> concatMap refsMS ss
    Cinterface _ mi ds -> [ RField mi (getLName d) | d <- ds, isNamed d ] ++
                          concatMap refsDl ds
    CmoduleVerilog e _ _ _ ses _ _ _ -> concatMap refsE (e : map snd ses)
    CmoduleVerilogT _ e _ _ _ ses _ _ _ -> concatMap refsE (e : map snd ses)
    Cdo _ ss -> concatMap refsS ss
    Caction _ ss -> concatMap refsS ss
    Crules _ rs -> concatMap refsR rs
    COper os -> concat [ refsE e | CRand e <- os ]
    CCon1 _ c e -> RCon c : refsE e
    CSelectTT ti e f -> RField (Just ti) f : refsE e
    CCon0 _ c -> [RCon c]
    CConT _ c es -> RCon c : concatMap refsE es
    CStructT t fs -> [ RField (leftCon t) f | (f, _) <- fs ] ++
                     concatMap (refsE . snd) fs
    CSelectT ti f -> [RField (Just ti) f]
    CTApply e _ -> refsE e
    _ -> []
  where isNamed (CLMatch _ _) = False
        isNamed _ = True

refsP :: CPat -> [Ref]
refsP pat =
  case pat of
    CPCon c ps -> RCon c : concatMap refsP ps
    CPstruct _ ti fs -> [ RField (Just ti) f | (f, _) <- fs ] ++
                        concatMap (refsP . snd) fs
    CPAs _ p -> refsP p
    CPOper os -> concat [ refsP p | CPRand p <- os ]
    CPCon1 _ c p -> RCon c : refsP p
    CPConTs _ c _ ps -> RCon c : concatMap refsP ps
    _ -> []

-- a selection of the form superSelection matches (which a field selection
-- can also have)
refsSuper :: Maybe Id -> Id -> [(Id, Id)] -> [Ref]
refsSuper mv d path = RSuper mv d path : [ RField (Just c) s | (c, s) <- path ]

refsQ :: CQual -> [Ref]
refsQ (CQGen _ p e) = refsP p ++ refsE e
refsQ (CQFilter e) = refsE e

refsDl :: CDefl -> [Ref]
refsDl (CLValueSign (CDefT v _ _ [CClause [] [] e]) [])
    | Just (d, path@(_:_)) <- superSelection e = refsSuper (Just v) d path
refsDl (CLValueSign d qs) = refsD d ++ concatMap refsQ qs
refsDl (CLValue _ cs qs) = concatMap refsC cs ++ concatMap refsQ qs
refsDl (CLMatch p e) = refsP p ++ refsE e

refsD :: CDef -> [Ref]
refsD (CDef _ _ cs) = concatMap refsC cs
refsD (CDefT _ _ _ cs) = concatMap refsC cs

refsS :: CStmt -> [Ref]
refsS (CSBindT p me _ _ e) = refsP p ++ maybe [] refsE me ++ refsE e
refsS (CSBind p me _ e) = refsP p ++ maybe [] refsE me ++ refsE e
refsS (CSletseq ds) = concatMap refsDl ds
refsS (CSletrec ds) = concatMap refsDl ds
refsS (CSExpr me e) = maybe [] refsE me ++ refsE e

refsMS :: CMStmt -> [Ref]
refsMS (CMStmt s) = refsS s
refsMS (CMrules e) = refsE e
refsMS (CMinterface e) = refsE e
refsMS (CMTupleInterface _ es) = concatMap refsE es

refsR :: CRule -> [Ref]
refsR (CRule _ me qs e) = maybe [] refsE me ++ concatMap refsQ qs ++ refsE e
refsR (CRuleNest _ me qs rs) =
    maybe [] refsE me ++ concatMap refsQ qs ++ concatMap refsR rs
