{-# LANGUAGE NondecreasingIndentation #-}
{-# LANGUAGE PatternGuards #-}
module TypeCheck(cTypeCheck,
                 cCtxReduceDef, cCtxReduceIO,
                 topExpr,
                 qualifyClassDefaults
                ) where

import Data.List
import Data.Maybe(catMaybes, isJust, fromMaybe)
import Control.Monad(when, mapAndUnzipM)
import qualified Data.Map as M
import qualified Data.Set as S

import PFPrint
import Id
import Error(internalError, EMsg, EMsgs(..), WMsg, ErrMsg(..),
             ErrorHandle, bsError, bsErrorNoExit, bsErrorUnsafe, bsWarning)
import ContextErrors
import Flags(Flags, enablePoisonPills, allowIncoherentMatches)
import CSyntax
import CSyntaxUtil(isCPVar)
import CType(leftTyCon, getArrows, isDictType, isDictFun)
import PoisonUtils
import Type
import Pred(Class(..))
import Subst
import SolvedBinds
import TIMonad
import TCMisc
import TCheck
import CtxRed
import SymTab
import Assump
import CSubst(cSubstN)
import CFreeVars(getFVC, getFTCC, getPV)
import Util(separate, apFst, quote, fst3, fromJustOrErr, mapSndM)
import Debug.Trace(trace, traceM)

cTypeCheck :: ErrorHandle -> Flags -> SymTab -> CPackage -> IO (CPackage, Bool, S.Set Id)
cTypeCheck errh flags symtab (CPackage name exports imports impsigs fixs defns includes) = do
    (typecheckedDefns, typeWarns, usedPkgs, haveErrors) <- tiDefns errh symtab flags defns

    -- Issue type warnings
    when (not (null typeWarns)) $ bsWarning errh typeWarns

    return (CPackage name exports imports impsigs fixs typecheckedDefns includes,
            haveErrors,
            usedPkgs)


-- type check top-level definitions in parallel (since they are independent)
tiDefns :: ErrorHandle -> SymTab -> Flags -> [CDefn] -> IO ([CDefn], [WMsg], S.Set Id, Bool)
tiDefns errh s flags ds = do
  let ai = allowIncoherentMatches flags
  let checkDef d = (defErr, warns, usedPkgs)
        where (result, warns, usedPkgs) = runTI flags ai s $ tiOneDef d
              defErr = case result of
                          (Left emsgs)  -> Left emsgs
                          (Right cdefn) -> rmFreeTypeVars cdefn
  let checked = map checkDef ds
  let (checks, wss, pkgss) = unzip3 checked
  let (errors, ds') = apFst concat $ separate checks
  let have_errors = not (null errors)
  let mkErrorDef (Left _)  (CValueSign (CDef i t _)) = Just (mkPoisonedCDefn i t)
      mkErrorDef (Left _)  (Cclass {}) = Nothing
      mkErrorDef (Left _)  (CValue {}) = Nothing -- Can't recover from missing signature error.
      mkErrorDef (Left _)  d = internalError ("tiDefns - not CValueSign or Cclass: " ++ ppReadable d)
      mkErrorDef (Right _) _ = Nothing
  let error_defs = catMaybes (zipWith mkErrorDef checks ds)
  let checked_error_defs = map checkDef error_defs
  let (double_error_msgs, error_defs') =
          apFst concat $ separate $ map fst3 checked_error_defs
  -- Accumulate all used packages (only from the first round, poison pills don't use new symbols)
  let allUsedPkgs = S.unions pkgss
  -- XXX: we give up - some type signatures are bogus
  when ((not (null double_error_msgs)) || (have_errors && not (enablePoisonPills flags))) $
      bsError errh (nub errors) -- the underyling error should be in errors
  when (have_errors && enablePoisonPills flags) $ bsErrorNoExit errh errors
  (ds'', new_warns) <- if have_errors
                       -- Don't try to check coherence on code known to be incorrect
                       then return (ds', [])
                       -- coherence checks are not independent across definitions, need to check all at once
                       else case runTI flags ai s (markAndCheckDefns ds') of
                              -- We skip trying to inject poison-pill dictionaries
                              (Left new_errors, _, _) -> bsError errh (nub new_errors)
                              (Right ds'', new_warns, _) -> return (ds'', new_warns)
  return (ds'' ++ error_defs', concat (new_warns:wss), allUsedPkgs, have_errors)

nullAssump :: [Assump]
nullAssump = []

tiOneDef :: CDefn -> TI CDefn
tiOneDef d@(CValueSign (CDef i t s)) = do
        --trace ("TC " ++ ppReadable i) $ return ()
        (rs, ~(CLValueSign d' _)) <- tiExpl nullAssump (i, t, s, [])
        checkTopPreds d rs
        s <- getSubst'
        clearSubst
        return (CValueSign (apSub s d'))
tiOneDef d@(Cclass incoh cps ik is fd fs) = do
    let -- we can assume that the current class is available, so make it
        -- as a pred
        ts :: [CType]
        ts = map cTVar is -- XXX no kind?
        this_p :: CPred
        this_p = CPred (CTypeclass (iKName ik)) ts
        tiF :: CField -> TI CField
        tiF f@(CField { cf_default = [] }) = return f
        tiF f@(CField fid _ (CQType fps fty) fcs _) = do
          let -- we don't need to include the superclasses ("cps")
              fps' = [this_p] ++ fps
              fqt' = CQType fps' fty
          (rs, ~(CLValueSign (CDefT _ _ _ fcs') _))
               <- tiExpl nullAssump (fid, fqt', fcs, [])
          checkTopPreds fid rs
          clearSubst
          return (f { cf_default = fcs' })
    fs' <- mapM tiF fs
    -- XXX We could return the mangled typechecked clauses here if
    -- XXX * we typecheck Cclass first and re-insert into the symt
    -- XXX * typecheck the rest of the pkg (which may use those defaults)
    -- XXX   and have "tiExpl" of structs not re-typecheck defaults
    -- XXX * have "genSign" use the symt containing the typechecked defaults
    -- XXX Until then, "genSign" inserts qualifiers, to preserve the scope
    -- traceM ("Cclass: " ++ ppReadable fs')
    return d
tiOneDef d@(CValue i s) = errorAtId ENoTopTypeSign i
tiOneDef d = return d

-- Force the substitution to make sure we don't drag around cruft.
getSubst' :: TI Subst
getSubst' = do
        s <- getSubst
        if s == s then return s else internalError "TypeCheck.getSubst': s /= s (WTF!?)"

-- Any predicates at the top level should be reported as an error
checkTopPreds :: (HasPosition a, PPrint a) => a -> [VPred] -> TI ()
checkTopPreds _ [] = return ()
checkTopPreds a ps = do
    -- reduce the predicates as much as possible
    (ps', ls) <- satisfy [] ps
    if null ps' then
        -- they shouldn't reduce away completely
        internalError ("checkTopPreds " ++ ppReadable (a, ps))
     else do
        addExplPreds []  -- add en empty context
        handleContextReduction (getPosition a) ps'

-- typecheck an expression as a top-level object
-- returning any unsatisfied preds
topExpr :: CType -> CExpr -> TI ([VPred], CExpr)
topExpr td e = do
  (ps, e') <- tiExpr [] td e
  (ps', sbs) <- satisfy [] ps
  s <- getSubst
  let rec_defls    = getRecursiveDefls sbs
      nonrec_defls = getNonRecursiveDefls sbs
  -- Generate code: nonrec outside (letseq), rec inside (letrec)
  return (apSub s (ps', cLetSeq nonrec_defls $ cLetRec rec_defls e'))

------

qualifyClassDefaults :: ErrorHandle -> SymTab -> [CDefn] -> [CDefn]
qualifyClassDefaults errh symt ds =
    let
        mkCQual c =
            case (findCon symt c) of
              Just [ConInfo { ci_assump = (qc :>: _) }] -> (c, qc)
              Just ds ->
                  let msg = "The signature file generation for typeclass " ++
                            "defaults cannot disambiguate the constructor " ++
                            quote (getIdString c) ++ ".  Perhaps adding " ++
                            "a package qualifier will help."
                  in  bsErrorUnsafe errh [(getPosition c, EGeneric msg)]
              Nothing ->
                  -- it could be a struct/interface (or an alias of one?)
                  case (findType symt c) of
                    Just (TypeInfo (Just qc) _ _ _ _) -> (c, qc)
                    _ -> internalError ("qualifyClassDefaults: " ++
                                        "constructor not found: " ++
                                        ppReadable c)
        mkTQual t =
            case (findType symt t) of
              Just (TypeInfo (Just qt) _ _ _ _) -> (t, qt)
              Just (TypeInfo Nothing _ _ _ _) ->
                  internalError ("qualifyClassDefaults: " ++
                                 "unexpected numeric or string type: " ++ ppReadable t)
              Nothing -> internalError ("qualifyClassDefaults: " ++
                                        "type not found: " ++ ppReadable t)
        mkVQual v =
            case (findVar symt v) of
              Just (VarInfo _ (qv :>: _) _ _) -> (v, CVar qv)
              Nothing -> internalError ("qualifyClassDefaults: " ++
                                        "var not found: " ++ ppReadable v)
        qualDef (Cclass incoh cps ik is deps fs) =
            let qualField (CField fi fps fqt fdefaults foqt) =
                    let (csets, vsets) = unzip $ map getFVC fdefaults
                        cset = S.unions csets
                        vset = S.unions vsets
                        tset = S.unions (map getFTCC fdefaults)
                        -- make the mappings
                        cmap = M.fromList (map mkCQual (S.toList cset))
                        vmap = M.fromList (map mkVQual (S.toList vset))
                        tmap = M.fromList (map mkTQual (S.toList tset))
                        -- substitute into the clauses
                        fdefaults' = cSubstN (tmap,cmap,vmap,M.empty) fdefaults
                    in  (CField fi fps fqt fdefaults' foqt)
            in  (Cclass incoh cps ik is deps (map qualField fs))
        qualDef d = d
    in
        map qualDef ds

------

-- remove free type variables from toplevel definitions:
--  - substitute vars of kind * with ()
--  - report unknown size on remaining free vars of kind #
-- crash on free type variables of other kinds (presumed typecheck bug)
rmFreeTypeVars :: CDefn -> Either [EMsg] CDefn
rmFreeTypeVars defn
    | null typeVars = Right defn
    | not (null unresolvedTypeVarsNum) = Left msgs
    | null typeVarsBad =
        Right (apSub substitution defn)
    | otherwise = internalError ("Typecheck.rmFreeD: toplevel type vars with" ++
                                 " kind neither * nor #:\n" ++
                                 ppReadable [(var, kind var) | var <- typeVarsBad] ++
                                 ppReadable defn)
    where typeVars = getFree defn
          (unresolvedTypeVarsNum, typeVars') = partition ((== KNum) . kind) typeVars
          (typeVarsStar, typeVarsBad) = partition ((== KStar) . kind) typeVars'
          substitution = mkSubst (zip typeVarsStar (repeat tPrimUnit))
          msgs = [(getPosition var, EUnknownSize) | var <- unresolvedTypeVarsNum]

-- get free type variables and hints about unknown size bit vectors
getFree :: CDefn -> [TyVar]
getFree (CValueSign (CDefT i vs qt cs)) = concatMap (getFreeC vs) cs
getFree d = []

getFreeC :: [TyVar] -> CClause -> [TyVar]
getFreeC vs (CClause ps qs e) = concatMap (getFreeQ vs) qs ++ getFreeE vs e

getFreeQ :: [TyVar] -> CQual -> [TyVar]
getFreeQ vs (CQGen t p e) = getFreeT vs t ++ getFreeE vs e
getFreeQ vs (CQFilter e) = getFreeE vs e

getFreeE :: [TyVar] -> CExpr -> [TyVar]
--getFreeE vs (CLam i e) = getFreeE vs e
--getFreeE vs (CLamT i _ e) = getFreeE vs e
getFreeE vs (Cletseq ds e) = concatMap (getFreeD vs) ds ++ getFreeE vs e
getFreeE vs (Cletrec ds e) = concatMap (getFreeD vs) ds ++ getFreeE vs e
getFreeE vs (CSelectT ti i) = []
getFreeE vs (CConT t i es) = concatMap (getFreeE vs) es
--getFreeE vs (Ccase _ _ _) =
getFreeE vs (CStructT t fs) = getFreeT vs t ++ concatMap (getFreeE vs . snd) fs
getFreeE vs e@(CAny {}) = []
getFreeE vs e@(CVar _) = []
getFreeE vs e@(CApply f es) = getFreeE vs f ++ concatMap (getFreeE vs) es
-- task application is the same as a normal application here
-- except checking the type for free variables (as CmoduleVerilogT does)
getFreeE vs e@(CTaskApplyT f t es) = getFreeT vs t ++ getFreeE vs f ++ concatMap (getFreeE vs) es
getFreeE vs e@(CLit _) = []
--getFreeE vs (CBinOp _ _ _) =
--getFreeE vs (CHasType _ _) =
--getFreeE vs (Cif _ _ _ _) =
--getFreeE vs (Csub _ _) =
getFreeE vs (Crules _ rs) = concatMap (getFreeR vs) rs
getFreeE vs (CTApply e ts) = getFreeE vs e ++ concatMap (getFreeT vs) ts
--getFreeE vs (CmoduleVerilog e _ _ _ ses _ _ _) =
getFreeE vs (CmoduleVerilogT t e es _ _ ses _ _ _) =
   getFreeT vs t ++ getFreeE vs e ++ concatMap (getFreeE vs . snd) ses
--getFreeE vs (CForeignFuncC _ cqt) =
getFreeE vs (CForeignFuncCT _ t) = getFreeT vs t
getFreeE vs e@(CLitT t _) = getFreeT vs t
getFreeE vs e@(CAnyT _ _ t) = getFreeT vs t
getFreeE vs e@(Cattributes _) = []
getFreeE vs e = internalError ("TypeCheck.getFreeE: " ++ ppReadable e ++ "\nshowing " ++ show e )

getFreeD :: [TyVar] -> CDefl -> [TyVar]
getFreeD vs (CLValueSign (CDefT i vs' qt cs) qs) =
        let vs'' = vs' ++ vs
        in  getFreeQT vs'' qt ++ concatMap (getFreeC vs'') cs ++ concatMap (getFreeQ vs) qs
getFreeD vs _ = internalError "TypeCheck.getFreeD: not CLValueSign"

getFreeT :: [TyVar] -> CType -> [TyVar]
getFreeT vs (TVar v) | v `notElem` vs = [v]
getFreeT vs (TAp t1 t2) = getFreeT vs t1 ++ getFreeT vs t2
getFreeT vs t = []

getFreeQT :: [TyVar] -> CQType -> [TyVar]
getFreeQT vs (CQType ps t) = getFreeT vs t                -- XXX ps

getFreeR :: [TyVar] -> CRule -> [TyVar]
getFreeR vs (CRule _ mi qs e) = getFreeME vs mi ++ concatMap (getFreeQ vs) qs ++ getFreeE vs e
getFreeR vs (CRuleNest _ mi qs rs) = getFreeME vs mi ++ concatMap (getFreeQ vs) qs ++ concatMap (getFreeR vs) rs

getFreeME :: [TyVar] -> Maybe CExpr -> [TyVar]
getFreeME vs Nothing = []
getFreeME vs (Just e) = getFreeE vs e

-- If Nothing then the dict bindings is coherent. We store the type in the incoherent case
-- to give better error messages.
type IncoherenceMap = M.Map Id (Maybe CType)

getCoherenceInfo :: IncoherenceMap -> Id -> Maybe CType
getCoherenceInfo m i = fromJustOrErr msg $ M.lookup i m
  where msg = "Dictionary dependency not in coherence map: " ++ ppReadable (i,m)

isIncohDep :: IncoherenceMap -> Id -> Bool
isIncohDep m d = isJust $ getCoherenceInfo m d

-- returns set of used external identifiers and a set of the types of incoherent instances used by this expression
analyzeDictExpr :: CExpr -> (S.Set Id, S.Set CType)
analyzeDictExpr (CVar i)
  | isDictId i = (S.singleton i, S.empty)
  | otherwise = (S.empty, S.empty)
analyzeDictExpr (CTApply e ts) = analyzeDictExpr e
analyzeDictExpr (CApply f es) = analyzeDictExprs (f:es)
analyzeDictExpr (CStructT _ fields) = analyzeDictExprs $ map snd fields
analyzeDictExpr (Cletseq ds e) = (idSet, typeSet)
  where (defsIdSet, defsTypeSet, localIds) = analyzeDeflsSeq ds
        (bodyIdSet, bodyTypeSet) = analyzeDictExpr e
        idSet = defsIdSet `S.union` (bodyIdSet `S.difference` localIds)
        typeSet = defsTypeSet `S.union` bodyTypeSet
analyzeDictExpr (Cletrec ds e) = (externalRefs, S.unions (bodyTypeSet : typeSets))
  where (idSets, typeSets) = unzip $ map analyzeDefl ds
        (bodyIdSet, bodyTypeSet) = analyzeDictExpr e
        localIds = S.fromList [ getLName d | d <- ds ]
        allRefs = S.unions $ bodyIdSet : idSets
        externalRefs = allRefs `S.difference` localIds
analyzeDictExpr (CTaskApplyT _ _ es) = analyzeDictExprs es
analyzeDictExpr (CConT _ _ es) = analyzeDictExprs es
analyzeDictExpr (CmoduleVerilogT _ m _ _ _ ses _ _ _) = analyzeDictExprs (m : map snd ses)
analyzeDictExpr (Crules _ rs) = (S.unions idSets, S.unions typeSets)
  where (idSets, typeSets) = unzip $ map analyzeRule rs
analyzeDictExpr e = (S.empty, S.empty)

analyzeDictExprs :: [CExpr] -> (S.Set Id, S.Set CType)
analyzeDictExprs es = (S.unions idSets, S.unions typeSets)
  where (idSets, typeSets) = unzip $ map analyzeDictExpr es

analyzeRule :: CRule -> (S.Set Id, S.Set CType)
analyzeRule (CRule _ me qs e) = (S.unions idSets, S.unions typeSets)
  where (meIdSet, meTypeSet) = maybe (S.empty, S.empty) analyzeDictExpr me
        (qualsIdSets, qualsTypeSets) = unzip $ map analyzeQual qs
        (bodyIdSet, bodyTypeSet) = analyzeDictExpr e
        idSets = meIdSet : bodyIdSet : qualsIdSets
        typeSets = meTypeSet : bodyTypeSet : qualsTypeSets
analyzeRule (CRuleNest _ me qs rs) = (S.unions idSets, S.unions typeSets)
  where (meIdSet, meTypeSet) = maybe (S.empty, S.empty) analyzeDictExpr me
        (qualsIdSets, qualsTypeSets) = unzip $ map analyzeQual qs
        (rulesIdSets, rulesTypeSets) = unzip $ map analyzeRule rs
        idSets = meIdSet : qualsIdSets ++ rulesIdSets
        typeSets = meTypeSet : qualsTypeSets ++ rulesTypeSets

-- quals do not bind dictionaries, so we just need to analyze the contained expressions
analyzeQual :: CQual -> (S.Set Id, S.Set CType)
analyzeQual (CQFilter e) = analyzeDictExpr e
analyzeQual (CQGen _ _ e) = analyzeDictExpr e

analyzeDef :: CDef -> (S.Set Id, S.Set CType)
analyzeDef (CDefT i _ _ _) | trace ("analyzeDef: " ++ ppReadable (i, isDictId i, isIncoherentDict i)) False = internalError "unpossible"
analyzeDef (CDefT i _ cqt cs) = (S.unions idSets, S.unions (incoherentBinding : typeSets))
  where (idSets, typeSets) = unzip $ map analyzeClause cs
        (CQType _ t) = cqt
        incoherentBinding = if isIncoherentDict i then S.singleton t else S.empty
analyzeDef d@(CDef _ _ _ ) = internalError $ "analyzeDef unexpected CDef: " ++ ppReadable d

analyzeDefl :: CDefl -> (S.Set Id, S.Set CType)
analyzeDefl (CLValueSign d qs) = (S.unions (defIdSet : idSets), S.unions (defTypeSet : typeSets))
  where (idSets, typeSets) = unzip $ map analyzeQual qs
        (defIdSet, defTypeSet) = analyzeDef d

analyzeDeflsSeq :: [CDefl] -> (S.Set Id, S.Set CType, S.Set Id)
analyzeDeflsSeq = analyzeDeflsSeq' S.empty S.empty S.empty
  where analyzeDeflsSeq' idSet typeSet localIds []     = (idSet, typeSet, localIds)
        analyzeDeflsSeq' idSet typeSet localIds (d:ds) = analyzeDeflsSeq' idSet' typeSet' localIds' ds
          where (deflIdSet, deflTypeSet) = analyzeDefl d
                idSet' = idSet `S.union` (deflIdSet `S.difference` localIds)
                typeSet' = typeSet `S.union` deflTypeSet
                localIds' = S.insert (getLName d) localIds

analyzeClause :: CClause -> (S.Set Id, S.Set CType)
-- Clauses may bind dictionary parameters, which are not free dependencies
-- We do not worry about what qs bind, because they are never used to bind dictionaries.
analyzeClause (CClause ps qs e) = (idSet `S.difference` S.unions (map getPV ps), typeSet)
  where (idSet, typeSet) = analyzeDictExpr e

isIncohClause :: IncoherenceMap -> CClause -> Bool
isIncohClause m c = not (S.null $ S.filter (isIncohDep m) idSet) || not (S.null typeSet)
  where (idSet, typeSet) = analyzeClause c

reportNewIncoherence :: IncoherenceMap -> CType -> CClause -> TI ()
reportNewIncoherence m t c = do
  let (clauseIdSet, clauseTypeSet) = analyzeClause c
      tsIncoherent = S.toList $ clauseTypeSet `S.union` (S.fromList $ catMaybes $ map (getCoherenceInfo m) (S.toList clauseIdSet))
  when (null tsIncoherent) $
    internalError $ "Newly incoherent with no incoherent dependencies: " ++ ppReadable (t, c, m)
  case leftTyCon t of
    Just (TyCon i _ (TIstruct SClass _)) -> do
      r <- getSymTab
      let cls_ai = allowIncoherent $ mustFindClass r (CTypeclass i)
      ai <- getAllowIncoherent -- from flags
      if fromMaybe ai cls_ai
      then do -- incoherent match allowed
        when (cls_ai /= Just True) $ do -- warn unless the class explicitly allows incoherent matches
          when (tsIncoherent /= [t]) $ do -- Don't warn on dictionary-dictionary bindings
            twarn (getPosition i, WIncoherentDepends (pfpString t) (map pfpString tsIncoherent))
      else do -- incoherent match forbidden
        when (tsIncoherent /= [t]) $ do -- Don't add a redundant error for a dictionary-dictionary binding
        let msg = (getPosition i, EIncoherentDepends (pfpString t) (map pfpString tsIncoherent))
        -- This error is effectively recoverable, so keep looking for more errors
        accumulateError $ EMsgs [msg]
    _ -> internalError $ "Dict has non-dict type: " ++ ppReadable (t, c)

getDictBinding :: CDef -> Maybe (Id, [TyVar], CType, CClause)
getDictBinding (CDefT i vs (CQType [] t) [c@(CClause _ [] _)])
  -- | trace (ppReadable (i, isDictId i, t, isDictFun t, vs, e)) False = internalError "unpossible"
  | isDictId i && isDictFun t = Just (i, vs, t, c)
getDictBinding d = {- trace ("not matching template: " ++ ppReadable d ++ "\n" ++ show d) -} Nothing

markAndCheckDefns :: [CDefn] -> TI [CDefn]
markAndCheckDefns defns = do
  let defs  = [ def | CValueSign def <- defns ]
{-
  traceM $ "markAndCheckDefns: " ++ ppReadable defns
  let defs  = [ def | CValueSign def <- defns ]
      (dfuns, notdfuns) = partition (isJust . getDictBinding) defs

  traceM $ "defs: " ++ ppReadable defs
  traceM $ "dfuns: " ++ ppReadable dfuns

  let dfunSet = S.fromList [ getDName dfun | dfun <- dfuns ]
      allDictDeps = S.unions $ map (fst . analyzeDef) defs
      importedDictDeps = allDictDeps `S.difference` dfunSet

  traceM $ "allDictDeps: " ++ ppReadable (S.toList allDictDeps)
  traceM $ "importedDictDeps: " ++ ppReadable (S.toList importedDictDeps)

  symt <- getSymTab

  let makeImportedCoherenceInfo i = do
        let msg = "imported dictionary not found in symbol table: " ++ ppReadable i
            VarInfo _ (i' :>: sc) _ = fromJustOrErr msg (findVar symt i)
        (_ :=> t, _) <- freshInst "markAndCheck" i' sc
        return (i', toMaybe (isIncoherentDict i') t)

  importedIncoherenceMap <- fmap M.fromList $ mapM makeImportedCoherenceInfo $ S.toList importedDictDeps

  traceM $ "importedIncoherenceMap: " ++ ppReadable importedIncoherenceMap

  (dfuns', topIncoherenceMap) <- markAndCheckRecursiveDefs importedIncoherenceMap dfuns
  traceM $ "topIncoherenceMap: " ++ ppReadable topIncoherenceMap

  notdfuns' <- mapM (fmap fst . markAndCheckDef topIncoherenceMap) notdfuns

  let defs' = dfuns' ++ notdfuns'
-}

  let allDictFunDeps = S.unions $ map (fst . analyzeDef) defs
  let topIncoherenceMap :: M.Map Id (Maybe CType)
      topIncoherenceMap = M.fromList [(i, Nothing) | i <- S.toList allDictFunDeps ]

  traceM $ "topIncoherenceMap: " ++ ppReadable topIncoherenceMap

  defs' <- mapM (fmap fst . markAndCheckDef topIncoherenceMap) defs

  traceM $ "defs': " ++ ppReadable defs'

  let defMap = M.fromList [ (i, def') | def' <- defs', let i = getDName def' ]
      updateDefn defn
        | Right name <- getName defn,
          Just def' <- M.lookup name defMap = CValueSign def'
        | otherwise = defn

  return $ map updateDefn defns

markAndCheckDef :: IncoherenceMap -> CDef -> TI (CDef, IncoherenceMap)
markAndCheckDef _ d | trace("markAndCheckDef: " ++ ppReadable (getDName d)) False = internalError "unpossible"
markAndCheckDef m d
  -- match dictionary binding
  | Just (i, vs, t, c) <- getDictBinding d = do
      if isIncoherentDict i then do
        let m' = M.insert i (Just t) m
        c' <- fmap (head . fst) $ markAndCheckClauses m' (CQType [] t) [c]
        return (CDefT i vs (CQType [] t) [c'], m')
      else do
        if isIncohClause m c && M.notMember i m
        then do
          -- incoherent because of dictionary dependencies, not instance match
          reportNewIncoherence m t c
          let m' = M.insert i (Just t) m
          c' <- fmap (head . fst) $ markAndCheckClauses m' (CQType [] t) [c]
          let i' = addIdProp i IdPIncoherent
          return (CDefT i' vs (CQType [] t) [c'], m')
        else do
          let m' = M.insert i Nothing m
          c' <- fmap (head . fst) $ markAndCheckClauses m' (CQType [] t) [c]
          return (CDefT i vs (CQType [] t) [c'], m')
markAndCheckDef m (CDefT i vs cqt cs) = do
  -- Any dictionaries bound by cs are out-of-scope after this definition
  (cs', _) <- markAndCheckClauses m cqt cs
  return (CDefT i vs cqt cs', m)
markAndCheckDef _ d@(CDef _ _ _) = internalError $ "markAndCheckDef unexpected CDef: " ++ ppReadable d

-- This match is very precise to ensure we are matching a typechecker-generated
-- dictionary binding clause
markAndCheckClauses :: IncoherenceMap -> CQType -> [CClause] -> TI ([CClause], IncoherenceMap)
markAndCheckClauses m (CQType [] t) [CClause ps [] body]
  | (a:as, res) <- getArrows t, isDictType a, all isCPVar ps,
    let dictArgIds = [ v | CPVar v <- ps ], all isDictId dictArgIds = do
  let dictArgTypes = a : takeWhile isDictType as
  when (length dictArgTypes < length dictArgIds) $
    internalError $ "markAndCheckClauses - not enough dict arguments for dict parameter clause: " ++
                    ppReadable (t, ps, body)
  let -- dictionary parameters are always effectively coherent
      newEntries = M.fromList $ map (\i -> (i, Nothing)) dictArgIds
      m' = M.union newEntries m
  body' <- markAndCheckExpr m' body
  return ([CClause ps [] body'], m')
markAndCheckClauses m cqt cs = do
  cs' <- mapM (markAndCheckClause m cqt) cs
  -- the clauses don't bind dictionaries, so the incoherence map does not change
  return (cs', m)

markAndCheckClause :: IncoherenceMap -> CQType -> CClause -> TI CClause
markAndCheckClause m (CQType _ t) (CClause ps qs body) = do
  body' <- markAndCheckExpr m body
  qs' <- mapM (markAndCheckQual m) qs
  -- ps doesn't bind or contain dictionaries so we don't have to process it
  return $ CClause ps qs' body'

markAndCheckDeflsSeq :: IncoherenceMap -> [CDefl] -> TI ([CDefl], IncoherenceMap)
markAndCheckDeflsSeq m [] = return ([], m)
markAndCheckDeflsSeq m (d:ds) = do
  (d', m')   <- markAndCheckDefl m d
  (ds', m'') <- markAndCheckDeflsSeq m' ds
  return (d':ds', m'')

markAndCheckDefl :: IncoherenceMap -> CDefl -> TI (CDefl, IncoherenceMap)
markAndCheckDefl m (CLValueSign d qs) = do
  qs' <- mapM (markAndCheckQual m) qs
  (d', m') <- markAndCheckDef m d
  return (CLValueSign d' qs', m')
markAndCheckDefl _ d = internalError $ "markAndCheckDefl unexpected CDefl (not CLValueSign): " ++ ppReadable d

markAndCheckDictDeflsRec :: IncoherenceMap -> [CDefl] -> TI ([CDefl], IncoherenceMap)
markAndCheckDictDeflsRec m ds = do
  let defs = [ d | CLValueSign d [] <- ds ] -- Dict bindings have no qualifiers
  when (length defs /= length ds) $
    internalError $ "markAndCheckDeflsRec unexpected CDefls (not CLValuesign _ []): " ++ ppReadable ds
  (defs', m') <- markAndCheckRecursiveDefs m defs
  return (map (flip CLValueSign $ []) defs', m')

markAndCheckRecursiveDefs :: IncoherenceMap -> [CDef] -> TI ([CDef], IncoherenceMap)
markAndCheckRecursiveDefs m defs = do
  let bindings = catMaybes $ map getDictBinding defs
      knownIncoherent = M.fromList [ (i, Just t) | (i, _, t, _) <- bindings, isIncoherentDict i ]
      workingMap = m `M.union` knownIncoherent
  traceM("def names: " ++ ppReadable [ i | (i, _, _, _) <- bindings ])
  traceM("knownIncoherent: " ++ ppReadable knownIncoherent)
  let propagateIncoherence currentMap
        | M.size nextMap == M.size currentMap = testMap -- the assumed coherence has been confirmed
        | otherwise = propagateIncoherence nextMap
        where assumeCoherentMap = M.fromList [(i, Nothing) | (i, _, _, _) <- bindings, M.notMember i currentMap ]
              testMap = currentMap `M.union` assumeCoherentMap
              nextMap = currentMap `M.union` M.fromList newEntries
              newEntries = [ (i, Just t) | (i, _, t, c) <- bindings,
                                           M.notMember i currentMap,
                                           isIncohClause testMap c ]
      finalMap = propagateIncoherence workingMap
  traceM("finalMap: " ++ ppReadable finalMap)
  let newIncoherent = [ (t, c) | (i, _, t, c) <- bindings,
                                 isJust (getCoherenceInfo finalMap i),
                                 M.notMember i knownIncoherent ]
  traceM("newIncoherent: " ++ ppReadable newIncoherent)
  -- mapM_ (uncurry $ reportNewIncoherence finalMap) newIncoherent
  results <- mapM (markAndCheckDef finalMap) defs
  return (map fst results, finalMap)

markAndCheckExpr :: IncoherenceMap -> CExpr -> TI CExpr
markAndCheckExpr m (Cletseq ds e) = do
  (ds', m') <- markAndCheckDeflsSeq m ds
  e' <- markAndCheckExpr m' e
  return $ Cletseq ds' e'
markAndCheckExpr m (Cletrec [] e) = markAndCheckExpr m e
-- Set of recursive dictionary bindings
markAndCheckExpr m (Cletrec ds@(d:_) e)
  | CLValueSign defl [] <- d, Just (i, _, t, _) <- getDictBinding defl,
    isDictId i, isDictType t = do
  (ds', m') <- markAndCheckDictDeflsRec m ds
  e' <- markAndCheckExpr m' e
  return $ Cletrec ds' e'
-- no dictionary bindings
markAndCheckExpr m (Cletrec ds e) = do
  -- traceM $ "Cletrec no dicts: " ++ ppReadable (Cletrec ds e)
  -- no dictionary bindings means we do not update the incoherence map
  (ds', _) <- mapAndUnzipM (markAndCheckDefl m) ds
  e' <- markAndCheckExpr m e
  return $ Cletrec ds' e'
markAndCheckExpr m (CApply f es) = do
  f'  <- markAndCheckExpr m f
  es' <- mapM (markAndCheckExpr m) es
  return $ CApply f' es'
-- Not all type applications are dictionary bindings
markAndCheckExpr m (CTApply e ts) = do
  e' <- markAndCheckExpr m e
  return $ CTApply e' ts
markAndCheckExpr m e@(CVar i)
  | isDictId i = do
    let incoherent = isIncohDep m i
    traceM $ "markAndCheckExpr: " ++ ppReadable (i, incoherent)
    traceM $ "incoherenceMap: " ++ ppReadable m
    if incoherent
    then return $ CVar $ addIdProp i IdPIncoherent
    else return e
  | otherwise = return e
markAndCheckExpr m (CTaskApplyT task t es) = do
  es' <- mapM (markAndCheckExpr m) es
  return $ CTaskApplyT task t es'
markAndCheckExpr m (Crules sps rs) = do
  rs' <- mapM (markAndCheckRule m) rs
  return $ Crules sps rs'
markAndCheckExpr m (CConT ti con es) = do
  es' <- mapM (markAndCheckExpr m) es
  return $ CConT ti con es'
markAndCheckExpr m (CStructT ct fields) = do
  fields' <- mapSndM (markAndCheckExpr m) fields
  return $ CStructT ct fields'
markAndCheckExpr m (CmoduleVerilogT ty name ui clks rst args fields sch paths) = do
  name' <- markAndCheckExpr m name
  args' <- mapSndM (markAndCheckExpr m) args
  return $ CmoduleVerilogT ty name' ui clks rst args' fields sch paths
markAndCheckExpr _ e = return e

markAndCheckQual :: IncoherenceMap -> CQual -> TI CQual
markAndCheckQual m (CQGen t p e) = do
  e' <- markAndCheckExpr m e
  -- p does not bind dictionaries, so it does not need to be processed
  return $ CQGen t p e'
markAndCheckQual m (CQFilter e) = do
  e' <- markAndCheckExpr m e
  return $ CQFilter e'

markAndCheckRule :: IncoherenceMap -> CRule -> TI CRule
markAndCheckRule m (CRule rps me qs e) = do
  me' <- mapM (markAndCheckExpr m) me
  qs' <- mapM (markAndCheckQual m) qs
  e'  <- markAndCheckExpr m e
  return $ CRule rps me' qs' e'
markAndCheckRule m (CRuleNest rps me qs rs) = do
  me' <- mapM (markAndCheckExpr m) me
  qs' <- mapM (markAndCheckQual m) qs
  rs' <- mapM (markAndCheckRule m) rs
  return $ CRuleNest rps me' qs' rs'
