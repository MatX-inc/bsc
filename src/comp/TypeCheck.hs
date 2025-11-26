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
import CType(leftTyCon, getArrows, isDictType)
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
import CFreeVars(getFVC, getFTCC)
import Util(separate, apFst, quote, fst3, fromJustOrErr, mapSndM)
-- import Debug.Trace(traceM)

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
  return (ds' ++ error_defs', concat wss, allUsedPkgs, have_errors)

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

getDictDeps :: CExpr -> [Id]
getDictDeps (CVar i) = [i]
getDictDeps (CTApply e ts) = getDictDeps e
-- _f is a dictionary function, not a dictionary dependency itself
-- any incoherence from _f's match should be recorded in the binding already.
getDictDeps (CApply _f es) = concatMap getDictDeps es
getDictDeps (CAnyT _ _ _) = []
getDictDeps e = internalError $ "getDictDeps invalid dictionary expression: " ++ ppReadable e

isIncohDict :: IncoherenceMap -> CExpr -> Bool
isIncohDict m e = any (isIncohDep m) (getDictDeps e)

reportNewIncoherence :: IncoherenceMap -> CType -> CExpr -> TI ()
reportNewIncoherence m t e = do
  let tsIncoherent = catMaybes $ map (getCoherenceInfo m) (getDictDeps e)
  when (null tsIncoherent) $
    internalError $ "Newly incoherent with no incoherent dependencies: " ++ ppReadable (t, e, m)
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
    _ -> internalError $ "Dict has non-dict type: " ++ ppReadable (t, e)

getDictBinding :: CDef -> Maybe (Id, CType, CExpr)
getDictBinding (CDefT i [] (CQType [] t) [CClause [] [] e]) = Just (i, t, e)
getDictBinding _ = Nothing

markAndCheckDef :: IncoherenceMap -> CDef -> TI (CDef, IncoherenceMap)
markAndCheckDef m d
  -- match dictionary binding
  | Just (i, t, e) <- getDictBinding d, isDictId i = do
      if isIncoherentDict i then do
        let m' = M.insert i (Just t) m
        e' <- markAndCheckExpr m e
        return (CDefT i [] (CQType [] t) [CClause [] [] e'], m')
      else do
        if isIncohDict m e
        then do
          -- incoherent because of dictionary dependencies, not instance match
          reportNewIncoherence m t e
          let m' = M.insert i (Just t) m
          e' <- markAndCheckExpr m' e
          let i' = addIdProp i IdPIncoherent
          return (CDefT i' [] (CQType [] t) [CClause [] [] e'], m')
        else do
          let m' = M.insert i Nothing m
          e' <- markAndCheckExpr m' e
          return (CDefT i [] (CQType [] t) [CClause [] [] e'], m')
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
markAndCheckDefl _ d = internalError $ "TypeCheck.markAndCheckDefl unexpected CDefl " ++ ppReadable d

markAndCheckExpr :: IncoherenceMap -> CExpr -> TI CExpr
markAndCheckExpr m (Cletseq ds e) = do
  (ds', m') <- markAndCheckDeflsSeq m ds
  e' <- markAndCheckExpr m' e
  return $ Cletseq ds' e'
markAndCheckExpr m (Cletrec [] e) = markAndCheckExpr m e
-- Set of recursive dictionary bindings
markAndCheckExpr m (Cletrec ds@(d:_) e)
  | CLValueSign defl [] <- d, Just (i, t, _) <- getDictBinding defl, isDictId i = do
  let bindings = catMaybes $ map getDictBinding [ d | CLValueSign d [] <- ds ]
  -- traceM $ "Cletrec dicts: " ++ ppReadable (Cletrec ds e)
  when (length bindings /= length ds) $
    internalError $ "Non-dictionary bindings in dictionary binding group: " ++ ppReadable ds
  let knownIncoherent = M.fromList [ (i, Just t) | (i, t, _) <- bindings, isIncoherentDict i ]
      workingMap = m `M.union` knownIncoherent
      propagateIncoherence currentMap
        | M.size nextMap == M.size currentMap = testMap -- the assumed coherence has been confirmed
        | otherwise = propagateIncoherence nextMap
        where assumeCoherentMap = M.fromList [(i, Nothing) | (i, _, _) <- bindings, M.notMember i currentMap ]
              testMap = currentMap `M.union` assumeCoherentMap
              nextMap = currentMap `M.union` M.fromList newEntries
              newEntries = [ (i, Just t) | (i, t, e) <- bindings,
                                           M.notMember i currentMap,
                                           isIncohDict testMap e ]
      finalMap = propagateIncoherence workingMap
  sequence_  [ reportNewIncoherence finalMap t e | (i, t, e) <- bindings,
                                                   isJust (getCoherenceInfo finalMap i),
                                                   M.notMember i knownIncoherent ]
  (ds', _) <- mapAndUnzipM (markAndCheckDefl finalMap) ds
  e' <- markAndCheckExpr finalMap e
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
  | isDictId i && isIncohDep m i = return $ CVar $ addIdProp i IdPIncoherent
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
