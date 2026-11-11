{-# LANGUAGE CPP #-}
{-# LANGUAGE PatternSynonyms #-}
module TIMonad(
        TI,
        apSubTI,
        CATFCache, mergeCATFCaches,
        TIResult(..), runTI, err, errs, twarn, handle,
        RecEv(..), runTIWithPool, getDictPool, setDictPool, countDP, countDPAt,
        getTyVarCounter, advanceTyVarCounter, dictIdFor, newDictN,
        openRecWindow, closeRecWindow, recLog, recActive,
        getSatStackSizes, bumpRecMaxima, getSatKeys, setSatKeys, setTopSatKeys,
        getAllowIncoherent, maskAllowIncoherent,
        getFlags, setFlags, getSymTab,
        getSubst, clearSubst, extSubst, updSubst,
        newTVar, newTVarId, skipTVar, isNewTVar, newDict, newVar,
        freshInst,
        VPred, pattern VPred, vpNum, vpWith, mkVPredN,
        getVPredPositions, expandSynVPred,
        EPred(..), Infer2, CheckT, TaskCheckT,
        getBoundTVs, getTopBoundTVs, addBoundTVs, popBoundTVs,
        getExplPreds, getTopExplPreds, addExplPreds, popExplPreds, mkEPred,
        GSeg(..), GTree(..), Givens, mkGivens, givensSegs, givensList, givensOnList,
        givensAppend,
        ClassIdx, classIdxLookup,
        classKey, getExplGivens, getTopExplSeg,
        getNumProven, getNumRefuted, addNumDecided,
        errorAtId, findCons, findTyCon, findFields, findCls,
        bitCls,
        literalCls, realLiteralCls, sizedLiteralCls, stringLiteralCls,
        numEqCls,
        updAssumpPos,
        recordPackageUse, recordATFResult,
        incrementSatStack, decrementSatStack, getSatStack, getSatStackIdx,
              mkTSSatElement, TSSatElement,
              pushSatStackContext, popSatStackContext
        , tiRecoveringFromError
        , tiRecoveringFromErrorxx
        , disambiguateStruct
        ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import PFPrint
import Id
import IdPrint
import Position
import CSyntax(CExpr(..))
import CType
import Error(internalError, EMsg, WMsg, EMsgs(..), ErrMsg(..))
import Flags(Flags, maxTIStackDepth)
import Subst
import Pred
import Scheme
import Assump
import SymTab
import PreIds(idBits, idLiteral, idRealLiteral, idSizedLiteral,
              idStringLiteral, idNumEq)
import Control.Monad(when)
import Control.Monad.Except(ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State(State, StateT, runState, runStateT,
                           lift, gets, get, put, modify)
import qualified Data.IntSet as IS
import Data.Array(Array, listArray, assocs)
import FStringCompat(fstringId)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map as M
import qualified Data.Set as S
import Util(headOrErr)
import DictPool(DictPool(..), Cnt, Summary, traceDictPool, GKeys(..))

-------

import Debug.Trace(traceM)
import IOUtil(progArgs)

doVarTrace, doSubstTrace, doBoundCheck, dontTrim :: Bool
doVarTrace = elem "-trace-tcvar" progArgs
doSubstTrace = elem "-trace-type-extsubst" progArgs
-- Invariant check (development): a bound (rigid) type variable has
-- exactly one binder -- its quantifier -- so it must never appear in
-- the domain of a substitution extension.  The unifier's guards and
-- the modal/actual split in instance matching are supposed to
-- guarantee this; this check enforces it at the single choke point
-- where unification results enter the monad state.
doBoundCheck = elem "-check-subst-bound" progArgs
dontTrim = elem "-trace-skip-trim" progArgs

-------

-- typechecking state that persists across errors
data TStatePersistent = TStatePersistent {
   tsFlags :: Flags,
   tsSymTab :: SymTab, -- symbol table
   tsNextTyVar :: !Int, -- index of the next generated tyvar
   tsRecoveredErrors :: [EMsg],
     -- accumulated error messages from which we have recovered
   -- whether TI monad allows general incoherent instance matches
   -- or only for marked typeclasses
   tsAllowIncoherent :: Bool,
   tsWarns :: [WMsg], -- accumulated warning messages
   tsUsedPackages :: S.Set Id, -- packages from which symbols were used
   tsATFCache :: CATFCache,
   -- the package's dictionary pool (TCMisc.sat), threaded through the
   -- package's definitions by TypeCheck.tiDefns; Nothing elsewhere
   tsDictPool :: Maybe DictPool,
   -- open recording windows of the pool and their log (most recent
   -- first; see RecEv), and the stack sizes reached inside them
   tsRecDepth :: !Int,
   tsRecLog :: [RecEv],
   tsRecLen :: !Int,
   tsRecMaxSat :: !Int,
   tsRecMaxSuper :: !Int,
   -- work counts for -trace-tc-dict-pool
   tsCounts :: !(IM.IntMap Int)
}

-- | An effect made while a recording window of the dictionary pool is
-- open (TCMisc.recordSolve), in the order made.
data RecEv
    = EvVar                  -- ^ newVar drew a number (not replayable)
    | EvTaint                -- ^ anything else a recording cannot replay
    | EvATF Id [Type] Type   -- ^ recordATFResult
    | EvPkg Id               -- ^ recordPackageUse
    | EvGoal Class [Type]    -- ^ a nested sat goal
    | EvSub Summary          -- ^ a nested solve's record (its window's
                             --   events, summarised when it closed), or
                             --   a nested pool hit's
    | EvLookfor              -- ^ a goal met by a given or a stack entry
    | EvRoot (Maybe Id)      -- ^ a full solve's own records follow
    | EvJoin !Int !Int [Int] -- ^ a join of the goals whose dictionaries
                             --   have the first two numbers, among the
                             --   goals numbered by the list (in order;
                             --   TCMisc.joinNeededCtxs')


type CATFCache = M.Map (Id, [Type]) Type

mergeCATFCaches :: CATFCache -> CATFCache -> CATFCache
mergeCATFCaches = M.union

-- typechecking state that is restored in case of error
data TStateRecover = TStateRecover {
  tsCurSubst :: !Subst, -- current substitution
  -- stack of bound tyvars (list of lists for stuff bound at each level)
  tsBoundTyVarStack :: [[TyVar]],
  -- the explicit preds bound at each level, newest first, each level's
  -- with its class index (GSeg)
  tsExplPreds :: [GSeg],
  -- the goals in progress, per context, each context's with its class
  -- index (TSSatStack)
  tsSatStack :: TSSuperSatStack,
  -- the dictionary pool's keys of the sat stack's entries (TCMisc.hitGuards),
  -- per context and entry as tsSatStack
  tsSatKeys :: [[GKeys]],
  -- numeric predicates already decided by the proviso SAT solver in
  -- this definition, so repeated queries are answered from here
  -- rather than re-posed
  tsNumProven :: S.Set Pred,
  tsNumRefuted :: S.Set Pred
}

type TSSatElement = EPred

mkTSSatElement :: (Maybe [TyVar]) -> Givens -> VPred -> TSSatElement
-- this solves a VPred in a circular way by simply assigning the
-- variable to the pred.  We still will need to solve the pred "p".
-- It is useful when there is recursion, where the solved "p" will
-- refer right back to this predicate.
mkTSSatElement _ _ (VPred i (PredWithPositions p _ _)) = EPred (CVar i) p

-- | One context of the sat stack: its goals, newest first, their index by
-- class (each goal under its own class), and the context as it was before
-- the newest goal was pushed, which popping the goal restores.
data TSSatStack = TSSatStack !(SizedStack TSSatElement) !(ClassIdx TSSatElement)
                             !(Maybe TSSatStack)
type TSSuperSatStack = SizedStack TSSatStack

emptySatStack :: TSSatStack
emptySatStack = TSSatStack (mkSizedStack []) (ClassIdx IM.empty) Nothing

-- | A goal's entry is filed under its class's key (sat's scan of the stack
-- does not expand superclasses).
satElementKey :: TSSatElement -> Int
satElementKey (EPred _ (IsIn c _)) = classKey c

-- a stack that keeps track of its depth
data SizedStack a = SizedStack Int [a]

mkSizedStack :: [a] -> SizedStack a
mkSizedStack v = SizedStack (length v) v

sizedStackPush
  :: ( Monad m
#if !defined(__GLASGOW_HASKELL__) || (__GLASGOW_HASKELL__ >= 808)
     , MonadFail m
#endif
     ) => Int -> SizedStack a -> a -> m (SizedStack a)
sizedStackPush maxStackDepth (SizedStack size stack) x =
    if ( size >= maxStackDepth )
        then fail "stack overflow"
        else return (SizedStack (succ size) (x:stack))

sizedStackPop :: SizedStack a -> SizedStack a
sizedStackPop (SizedStack size (_:rest)) =
    SizedStack (pred size) rest
sizedStackPop _ = internalError "sizedStackPop: stack underflow"

sizedStackPeek :: SizedStack a -> a
sizedStackPeek (SizedStack size (x:_)) = x
sizedStackPeek _ = internalError "sizedStackPeek: stack underflow"

sizedStackModify :: SizedStack a -> (a -> a) -> SizedStack a
sizedStackModify (SizedStack size (x:rest)) f =
    SizedStack size ((f x):rest)
sizedStackModify _ _ = internalError "sizedStackModify: stack underflow"

-- state/error monad with bsc error messages and hidden TState
type TI = StateT TStateRecover (ExceptT EMsgs (State TStatePersistent))

-- apply the current substitution to something
apSubTI :: (Types a) => a -> TI a
apSubTI x = do
              subst <- getSubst
              return (apSub subst x)

initPersistentState :: Flags -> Bool -> SymTab -> TStatePersistent
initPersistentState flags ai s = TStatePersistent {
    tsFlags = flags,
    tsSymTab = s,
    tsNextTyVar = 1000,
    tsWarns = [],
    tsAllowIncoherent = ai,
    tsRecoveredErrors = [],
    tsUsedPackages = S.empty,
    tsATFCache = M.empty,
    tsDictPool = Nothing,
    tsRecDepth = 0,
    tsRecLog = [],
    tsRecLen = 0,
    tsRecMaxSat = 0,
    tsRecMaxSuper = 0,
    tsCounts = IM.empty
  }

initRecoverState :: TStateRecover
initRecoverState = TStateRecover {
    tsCurSubst = nullSubst,
    tsBoundTyVarStack = [],
    tsExplPreds = [],
    tsSatStack = mkSizedStack [emptySatStack],
    tsSatKeys = [[]],
    tsNumProven = S.empty,
    tsNumRefuted = S.empty
  }

data TIResult a = TIResult {
    tiResult       :: Either [EMsg] a,
    tiWarnings     :: [WMsg],
    tiUsedPackages :: S.Set Id,
    tiATFCache     :: CATFCache,
    tiCounts       :: IM.IntMap Int
  }

runTI :: Flags -> Bool -> SymTab -> TI a -> TIResult a
runTI flags ai s m = TIResult {
    tiResult       = mkFinalResult result rec_errors,
    tiWarnings     = tsWarns pState,
    tiUsedPackages = tsUsedPackages pState,
    tiATFCache     = tsATFCache pState,
    tiCounts       = tsCounts pState
  }
  where (result, pState) = runTIState flags ai s m
        rec_errors = tsRecoveredErrors pState

-- "runTI" with the dictionary pool, seeded from and returning the pool
-- that TypeCheck.tiDefns threads through the package.  The result's
-- fields are the final state's, taken out when the pool is, so that no
-- thunk in the result (whose warnings, packages and ATF cache live until
-- the .bo is written) holds the state, and the pool with it.
runTIWithPool :: Flags -> Bool -> SymTab -> DictPool -> TI a
              -> (TIResult a, DictPool)
runTIWithPool flags ai s dp m =
    case pState of
      TStatePersistent { tsRecoveredErrors = errs, tsWarns = ws,
                         tsUsedPackages = pkgs, tsATFCache = atfs,
                         tsCounts = cnts, tsDictPool = mdp } ->
        let res = TIResult {
                tiResult       = mkFinalResult result errs,
                tiWarnings     = ws,
                tiUsedPackages = pkgs,
                tiATFCache     = atfs,
                tiCounts       = cnts
              }
        in  case mdp of
              Just dp' -> (res, dp')
              Nothing -> internalError "runTIWithPool: pool lost"
  where (result, pState) =
            runState (runExceptT (runStateT m initRecoverState))
                     ((initPersistentState flags ai s) { tsDictPool = Just dp })

getDictPool :: TI (Maybe DictPool)
getDictPool = lift (gets tsDictPool)

setDictPool :: DictPool -> TI ()
setDictPool dp = lift $ do
  s <- get
  put $! s { tsDictPool = Just dp }

countDP :: Cnt -> Int -> TI ()
countDP c n = countDPAt (fromEnum c) n

countDPAt :: Int -> Int -> TI ()
countDPAt k n = when traceDictPool $ lift $ do
  s <- get
  let s' = s { tsCounts = IM.insertWith (+) k n (tsCounts s) }
  s' `seq` put s'

-- The counter that numbers generated names (getTyVarNum), read and
-- advanced as that many draws would (TCMisc.sat's pool hits).
getTyVarCounter :: TI Int
getTyVarCounter = lift (gets tsNextTyVar)

advanceTyVarCounter :: Int -> TI ()
advanceTyVarCounter k = lift $ do
  s <- get
  put $! s { tsNextTyVar = tsNextTyVar s + k }

-- Recording windows (TCMisc.recordSolve).  A window remembers the log
-- length and the stack-size maxima at its start; the log is cleared when
-- the outermost window closes.
data RecWindow = RecWindow !Int !Int !Int

-- (The pool's state updates are made strictly, so that no thunk holds an
-- earlier state, and the pool in it, alive.)
openRecWindow :: TI RecWindow
openRecWindow = do
  (sat, super) <- getSatStackSizes
  lift $ do
    s <- get
    let w = RecWindow (tsRecLen s) (tsRecMaxSat s) (tsRecMaxSuper s)
    w `seq` (put $! s { tsRecDepth = tsRecDepth s + 1,
                        tsRecMaxSat = sat, tsRecMaxSuper = super })
    return w

-- The window's events (in order), taken off the log (the caller logs
-- their summary in their place), and the stack-size maxima reached in it.
closeRecWindow :: RecWindow -> TI ([RecEv], Int, Int)
closeRecWindow (RecWindow len0 msat0 msuper0) = lift $ do
  s <- get
  let n = tsRecLen s - len0
      (now, rest) = splitAt n (tsRecLog s)
      evs = reverse now
      msat = tsRecMaxSat s
      msuper = tsRecMaxSuper s
      d = tsRecDepth s - 1
      rest' = if d == 0 then [] else rest
  length evs `seq` rest' `seq`
    (put $! s { tsRecDepth = d,
                tsRecLog = rest',
                tsRecLen = if d == 0 then 0 else len0,
                tsRecMaxSat = max msat0 msat,
                tsRecMaxSuper = max msuper0 msuper })
  return (evs, msat, msuper)

recActive :: TI Bool
recActive = lift (gets (\ s -> tsRecDepth s > 0))

recLog :: RecEv -> TI ()
recLog ev = lift $ do
  s <- get
  when (tsRecDepth s > 0) $ do
    let lg = tsRecLog s
    lg `seq` (put $! s { tsRecLog = ev : lg, tsRecLen = tsRecLen s + 1 })

-- A nested hit inside a window reaches the stack sizes its solve would.
bumpRecMaxima :: Int -> Int -> TI ()
bumpRecMaxima sat super = lift $ do
  s <- get
  when (tsRecDepth s > 0) $
    put $! s { tsRecMaxSat = max sat (tsRecMaxSat s),
               tsRecMaxSuper = max super (tsRecMaxSuper s) }

-- The current sat-stack and super-stack sizes.
getSatStackSizes :: TI (Int, Int)
getSatStackSizes = do
  SizedStack super ctxs <- gets tsSatStack
  case ctxs of
    (TSSatStack (SizedStack sat _) _ _ : _) -> return (sat, super)
    [] -> return (0, super)

runTIState :: Flags -> Bool -> SymTab -> TI a ->
              (Either EMsgs (a, TStateRecover), TStatePersistent)
runTIState flags ai s m = runState error_run (initPersistentState flags ai s)
  where error_run = runExceptT (runStateT m initRecoverState)

mkFinalResult :: Either EMsgs (a, TStateRecover) -> [EMsg] -> Either [EMsg] a
mkFinalResult result rec_errors =
    case result of
      Left es -> Left (errmsgs es ++ rec_errors)
      Right (answer, _) ->
          case rec_errors of
            [] -> Right answer
            es -> Left es

-- Add an error from which we have recovered to the accumulating list of
-- such errors.  If you forget to call this function in your error handler
-- (i.e., second argument to catchError), the error will be ignored and
-- the TI Monad will succeed.
accumulateError :: EMsgs -> TI ()
accumulateError es = do
  -- traceM "hello accumulateError"
  lift (modify addError)
  -- list might have poor time complexity when building
  -- this kind of concatenation. XXX
  -- Possibly explore "finger trees" in the future.
  where addError s = s { tsRecoveredErrors = (tsRecoveredErrors s) ++ (errmsgs es) }

-- XXX maybe someday get rid of the next two functions and replace them with
-- throwError.  But throwError requires importing Control.Monad.Error
err :: EMsg -> TI a
err e = throwError (EMsgs [e])

errs :: String -> [EMsg] -> TI a
errs tag [] = internalError ("TIMonad errs: no errors - " ++ tag)
errs tag es = throwError (EMsgs es)

tiRecoveringFromError :: Types a => TI a -> TI a -> TI a
tiRecoveringFromError = tiRecoveringFromError'
                        -- tiRecoveringFromErrorxx

tiRecoveringFromError' :: Types a => TI a -> TI a -> TI a
tiRecoveringFromError' do_something create_fake_output = do
  oldState {- :: TStateRecover -} <- get
  catchError
     (if dontTrim then do_something else
          (do
       -- the SUCCESS path

       -- getPosition noPosition == noPosition
       -- via "instance HasPosition Position"
            (dummy, _) <- newTVarId "tiRecoveringFromError" KStar noPosition
            answer <- do_something

           {-
             The reason we trimSubst on success is twofold:
             1. As a sanity check to make sure that no type information
             is leaking out of the "do_something" code, and that this
             error recovery framework is sound.
             2. Performance (memory) improvement.
             The reason we getSubst and apSub is if a certain type
             variable is generated in do_something, we do not want
             to trim it away, but force (apSub) it into the returned value.
             (after consultation with Ravi.)
            -}
            s <- getSubst
            updSubst (trimSubst dummy)
            return (apSub s answer)
          ))
     (\es ->
          -- the FAIL path

          do accumulateError es
             put oldState
             -- create_fake_output usually returns some bogus
             -- return value simulating the output of "do_something"
             create_fake_output
     )

-- exists only for debugging
tiRecoveringFromErrorxx :: TI a -> TI a -> TI a
tiRecoveringFromErrorxx do_something _ = do_something

twarn :: WMsg -> TI ()
twarn w = lift (modify (addWarning w))
  where addWarning w s = s { tsWarns = w:(tsWarns s) }

-- Record that a symbol from a package was used
recordPackageUse :: Maybe Id -> TI ()
recordPackageUse Nothing = return ()  -- no package to record
recordPackageUse (Just pkg) = do
  recLog (EvPkg pkg)
  lift (modify (addPackage pkg))
  -- (the thunk holds the set, not the state and the pool in it)
  where addPackage pkg s@(TStatePersistent { tsUsedPackages = ps }) =
            s { tsUsedPackages = S.insert pkg ps }

recordATFResult :: Id -> [Type] -> Type -> TI ()
recordATFResult atfId args result = do
    recLog (EvATF atfId args result)
    lift (modify addATF)
  -- (the thunk holds the map, not the state and the pool in it)
  where addATF s@(TStatePersistent { tsATFCache = c }) =
            s { tsATFCache = M.insert (atfId, args) result c }

-- XXX maybe someday get rid of this function and replace with catchError
handle :: TI a -> (EMsgs -> TI a) -> TI a
handle = catchError

getAllowIncoherent :: TI Bool
getAllowIncoherent = lift (gets tsAllowIncoherent)

-- temporarily turn off allowIncoherent
-- used for reducePredsAggressive (and possibly other things)
maskAllowIncoherent :: TI a -> TI a
maskAllowIncoherent m = do
  s_p <- lift get
  let old_ai = tsAllowIncoherent s_p
  lift $ put $ s_p  { tsAllowIncoherent = False }
  result <- m
  s_p' <- lift get
  lift $ put $ s_p' { tsAllowIncoherent = old_ai }
  return result

getFlags :: TI Flags
getFlags = lift (gets tsFlags)

setFlags :: Flags -> TI ()
setFlags new_flags = do
  let updFn s = s { tsFlags = new_flags }
  lift (modify updFn)

getSymTab :: TI SymTab
getSymTab = lift (gets tsSymTab)

getSubst :: TI Subst
getSubst = gets tsCurSubst

transSubst :: (Subst -> Subst) -> TStateRecover -> TStateRecover
transSubst f s = s { tsCurSubst = f (tsCurSubst s) }

updSubst :: (Subst -> Subst) -> TI ()
updSubst f = modify (transSubst f)

getBoundTVs :: TI [TyVar]
getBoundTVs = gets tsBoundTyVarStack >>= (return . concat)

-- get just the most recently bound tvars
getTopBoundTVs :: TI [TyVar]
getTopBoundTVs = gets tsBoundTyVarStack >>= (return . headOrErr "getTopBoundTVs")

addBoundTVs :: [TyVar] -> TI ()
addBoundTVs is = modify addVars
  where addVars s = s { tsBoundTyVarStack = is:(tsBoundTyVarStack s) }

popBoundTVs :: TI ()
popBoundTVs = modify dropVars
  where dropVars s = s { tsBoundTyVarStack = tail (tsBoundTyVarStack s) }

getExplPreds :: TI [EPred]
getExplPreds = gets tsExplPreds >>= (return . concatMap gsList)

getTopExplPreds :: TI [EPred]
getTopExplPreds = gets tsExplPreds >>= (return . gsList . headOrErr "getTopExplPreds")

-- getExplPreds with the levels' class indexes
getExplGivens :: TI Givens
getExplGivens = gets tsExplPreds >>= (return . mkGivensSegs)

-- the newest level (getTopExplPreds is its gsList)
getTopExplSeg :: TI GSeg
getTopExplSeg = gets tsExplPreds >>= (return . headOrErr "getTopExplSeg")

addExplPreds :: [EPred] -> TI ()
addExplPreds ps = modify addPreds
  where addPreds s = s { tsExplPreds = mkGSeg ps : (tsExplPreds s) }

popExplPreds :: TI ()
popExplPreds = modify dropPreds
  where dropPreds s = s { tsExplPreds = tail (tsExplPreds s) }

getNumProven :: TI (S.Set Pred)
getNumProven = gets tsNumProven

getNumRefuted :: TI (S.Set Pred)
getNumRefuted = gets tsNumRefuted

addNumDecided :: [Pred] -> [Pred] -> TI ()
addNumDecided proven refuted = modify (\ s ->
    s { tsNumProven = foldr S.insert (tsNumProven s) proven,
        tsNumRefuted = foldr S.insert (tsNumRefuted s) refuted })

mkEPred :: Pred -> TI EPred
mkEPred p = do i <- newDict
               return $ EPred (CVar i) p

clearSubst :: TI ()
clearSubst = modify (transSubst (const nullSubst))

extSubst :: String -> Subst -> TI ()
extSubst loc s' = do
  s <- getSubst
  when (doSubstTrace) $ do
    when (not (chkSubstOrder s' s)) $
      internalError(loc ++ " extSubst: " ++ ppReadable (s', s))
    traceM (loc ++ " extSubst: " ++ ppReadable s')
  when (doBoundCheck) $ do
    bvs <- getBoundTVs
    case filter (`elem` bvs) (getSubstDomain s') of
      []  -> return ()
      bad -> internalError (loc ++ " extSubst: bound type variable(s) in " ++
                            "substitution domain: " ++ ppReadable (bad, s'))
  modify (transSubst (\s -> s' @@ s))

getTyVarNum :: TI (Int)
getTyVarNum = lift $ do
  s <- get
  let n = tsNextTyVar s
  put (s { tsNextTyVar = n + 1 })
  return n

-- XXX consider adding tracing support for string in the name as with newVar
newTVar :: HasPosition a => String -> Kind -> a -> TI Type
newTVar msg k x = do
  let pos = getPosition x
  bvss <- gets tsBoundTyVarStack
  let loopVar = do
        n <- getTyVarNum
        let v = mkGenTyVar pos n k
        if any (v `elem`) bvss then loopVar
         else do
           when doVarTrace $ traceM ("newTVar: " ++ show n ++ " " ++ msg);
           return (TVar v)
  loopVar

-- convenience function for getting the TyVar
-- (when create a TVar for marking a place in the subst to trim back to)
newTVarId :: HasPosition a => String -> Kind -> a -> TI (TyVar, Type)
newTVarId msg k x = do
  tv <- newTVar msg k x
  case tv of
    TVar i -> return (i, tv)
    _ -> internalError (msg ++ ": call to newTVarId failed")

-- The draw of newTVar without its variable: advance the counter as
-- newTVar does for a variable of kind k at position pos, passing over,
-- as its loop does, a number that a bound variable has, so that a
-- caller that passes an instance over without instantiating it leaves
-- the counter where instantiating it would have.  The bound variables
-- and the set of their numbers are the caller's, fetched once for all
-- its draws: newTVar concatenates the bound stack and scans it at
-- every call, and TyVar equality compares the number before the name,
-- so the set decides a draw and the scan, which is what forces the
-- variable's name, runs only for a number the set holds, as newTVar's
-- scan does.
skipTVar :: [TyVar] -> IS.IntSet -> String -> Kind -> Position -> TI ()
skipTVar bvs bvnums msg k pos = loopVar
  where
    loopVar = do
        n <- getTyVarNum
        if (IS.member n bvnums && mkGenTyVar pos n k `elem` bvs)
          then loopVar
          else when doVarTrace $ traceM ("newTVar: " ++ show n ++ " " ++ msg)

-- distinguish TyVars with generated names from those with user-given names
isNewTVar :: TyVar -> Bool
isNewTVar = isBadTyVar

newDict :: TI Id
newDict = fmap snd newDictN

-- newDict, with the number the name is drawn with
newDictN :: TI (Int, Id)
newDictN = do
 n <- getTyVarNum
 when doVarTrace $ traceM ("newDict: " ++ ppReadable n)
 return (n, dictIdFor n)

-- The dictionary name newDict makes for number n.
dictIdFor :: Int -> Id
dictIdFor n = addIdProp (enumId "tcdict" noPosition n) IdPDict

newVar :: Position -> String -> TI Id
newVar p str = do
    -- only make the name worse if tracing is on
    let varname = if doVarTrace then "tctemp_"++str++"_" else "tctemp"
    recLog EvVar
    n <- getTyVarNum
    --traceM (ppReadable (p, n)) $
    return (enumId varname p n)

freshInst :: HasPosition a => String -> a -> Scheme -> TI (Qual Type, [Type])
freshInst msg x (Forall ks qt@(_ :=> t)) = do
    ts <- mapM (flip (newTVar ("freshInst " ++ msg)) x) ks
{-
    let ps :=> t = inst ts qt
        ps' = [ IsIn clsSize [t] | t <- ts, kind t == KNum ]
    return ((ps ++ ps') :=> t, ts)
-}
    return (inst ts qt, ts)

------

-- VPred is an unsolved predicate (at least in satisfy, satMany, sat...).
-- The field that the pattern VPred leaves out is the number its
-- dictionary name was drawn with (newDictN), or -1 if unknown, so that
-- TCMisc's dictionary pool can tell the names apart without forcing them;
-- building with VPred leaves it unknown.
data VPred = VPred_ !Int Id PredWithPositions

pattern VPred :: Id -> PredWithPositions -> VPred
pattern VPred i p <- VPred_ _ i p
  where VPred i p = VPred_ (-1) i p

{-# COMPLETE VPred #-}

-- (as deriving Show over the pattern would)
instance Show VPred where
    showsPrec d (VPred i p) =
        showParen (d >= 11) (showString "VPred " . showsPrec 11 i .
                             showChar ' ' . showsPrec 11 p)

-- The number the name was drawn with, -1 if unknown.
vpNum :: VPred -> Int
vpNum (VPred_ n _ _) = n

-- The same name (and number) with another predicate.
vpWith :: VPred -> PredWithPositions -> VPred
vpWith (VPred_ n i _) p = VPred_ n i p

mkVPredN :: Int -> Id -> PredWithPositions -> VPred
mkVPredN = VPred_

instance Types VPred where
    apSub s v@(VPred _ p) = vpWith v (apSub s p)
    tv (VPred _ p) = tv p

instance PPrint VPred where
-- note that the colon (:) that gets printed here is NOT a list cons!
    pPrint d p (VPred i q) = pparen (p>0) (ppId d i <> text":" <> pPrint d 10 q)

getVPredPositions :: VPred -> [Position]
getVPredPositions (VPred i p) = getPredPositions p

-- the best position for a VPred should be its dictionary binding
instance HasPosition VPred where
  getPosition (VPred i p) = getPosition i


expandSynVPred :: VPred -> VPred
expandSynVPred v@(VPred _ (PredWithPositions (IsIn c ts) poss anc)) = vpWith v pwp'
  where pwp' = PredWithPositions p' poss anc
        p'   = IsIn c ts'
        ts'  = map expandSyn ts

-- the CExpr is a dictionary
data EPred = EPred CExpr Pred

instance Types EPred where
    apSub s (EPred e p) = EPred e (apSub s p)
    tv (EPred _ p) = tv p

instance PPrint EPred where
    pPrint d p (EPred e q) = pparen (p>0) (pparen True (pPrint d 0 e) <> text":" <> pPrint d 10 q)

-- Indexes by class
--
-- sat looks for its goal among the goals in progress (the sat stack, newest
-- first) and in TCMisc.bySuperE's expansion of the givens (each given, its
-- superclasses' expansions in super-list order, then its commuted form);
-- its partial-reduction check looks in the givens' expansion too, and
-- isReduciblePred among the givens themselves.  Each scan compares an
-- entry's class with the goal's before anything else and does nothing
-- more with an entry of another class, so visiting only the entries whose
-- class has the goal's base name (classKey), in the same order, makes the
-- same comparisons on the same entries and stops at the same one.  Both
-- keep that index (ClassIdx): a level of givens files each given under
-- every key of its superclass closure (GSeg), and a context of the sat
-- stack each goal under its own class as it is pushed (TSSatStack).  The
-- givens' index is built from classes alone, and apSub keeps every given's
-- class and position, so a list's index serves every substitution of it.
--
-- What an index forces that a scan might not have by then: building a
-- level's index, each given to weak head normal form and the class, name,
-- super list and isComm of every class in its superclass closure; a push,
-- the goal's entry and Pred to weak head normal form and its class and
-- name, which the goal's own scan of the stack forced (on an empty stack,
-- the givens' lookup, the pool or reducePred forces them soon after).  No
-- type, position, type variable or dictionary name.  A Class is a
-- symbol-table record keyed by its name, and a superclass is looked up by
-- name (MakeSymTab), so this interns nothing (class names are interned by
-- the parser or by the .bo reader's eager string-table read).

-- | Items filed by class key, each key's in the order a scan meets them.
newtype ClassIdx a = ClassIdx (IM.IntMap [a])

-- | The items filed under the key, in scan order.  An empty index does not
-- look at the key, as a scan of nothing does not look at the goal.
classIdxLookup :: ClassIdx a -> Int -> [a]
classIdxLookup (ClassIdx m) k
  | IM.null m = []
  | otherwise = IM.findWithDefault [] k m

-- | Items in scan order, each with a key it is filed under.
classIdxFromList :: [(Int, a)] -> ClassIdx a
classIdxFromList kxs =
    ClassIdx (IM.map reverse (IM.fromListWith (++) [ (k, [x]) | (k, x) <- kxs ]))

-- | Files the item under the key ahead of the others there.
classIdxPush :: Int -> a -> ClassIdx a -> ClassIdx a
classIdxPush k x (ClassIdx m) = ClassIdx (IM.alter (Just . maybe [x] (x :)) k m)

-- | A class and its superclass closure, as bySuperE expands a given of the
-- class: the class's key, the keys anywhere below, and the superclasses'
-- trees in the order of the class's super list.
data GTree = GTree { gtKey :: !Int, gtReach :: !IS.IntSet, gtSubs :: [GTree] }

-- | Equal classes have equal keys: Eq Class is qualEq on the names, which
-- needs equal base names.  Unequal classes may share a key (the
-- qualifier is not in it); a scan of a key's entries still compares the
-- classes.
classKey :: Class -> Int
classKey c = fstringId (getIdBase (typeclassId (name c)))

classTree :: Class -> GTree
classTree c = GTree k (IS.insert k (IS.unions (map gtReach subs))) subs
  where k = classKey c
        subs = [ classTree c' | (_, IsIn c' _) <- super c ]

-- | One level of givens: the list and, by position, each given's class
-- tree, with the positions whose tree holds each key (ascending).
data GSeg = GSeg {
    gsList  :: [EPred],
    gsTrees :: Array Int GTree,
    gsIdx   :: ClassIdx Int
  }

mkGSeg :: [EPred] -> GSeg
mkGSeg ps = GSeg ps trees idx
  where trees = listArray (0, length ps - 1) [ classTree c | EPred _ (IsIn c _) <- ps ]
        idx = classIdxFromList [ (k, i) | (i, t) <- assocs trees,
                                          k <- IS.toList (gtReach t) ]

-- | The givens of a solve: its levels, newest first, with no empty level.
-- Their list (givensList) is the levels' lists concatenated.  No givens
-- is no level: nothing of an index is made for it.
newtype Givens = Givens [GSeg]

mkGivens :: [EPred] -> Givens
mkGivens [] = Givens []
mkGivens ps = Givens [mkGSeg ps]

mkGivensSegs :: [GSeg] -> Givens
mkGivensSegs segs = Givens [ seg | seg <- segs, not (null (gsList seg)) ]

givensSegs :: Givens -> [GSeg]
givensSegs (Givens segs) = segs

givensList :: Givens -> [EPred]
givensList (Givens [seg]) = gsList seg
givensList (Givens segs) = concatMap gsList segs

-- | The givens of both, in order (the lists' (++)).
givensAppend :: Givens -> Givens -> Givens
givensAppend (Givens segs1) (Givens segs2) = Givens (segs1 ++ segs2)

-- | The level's index over another list of its givens: the level's list
-- with a substitution applied (same classes, same positions).
givensOnList :: GSeg -> [EPred] -> Givens
givensOnList seg ps = mkGivensSegs [seg { gsList = ps }]

instance Types Givens where
    apSub s (Givens segs) = Givens [ seg { gsList = apSub s (gsList seg) } | seg <- segs ]
    tv gv = tv (givensList gv)

type Infer2 e t r = [Assump] -> e -> TI ([VPred], t, r)

type CheckT e = [Assump] -> Type -> e -> TI ([VPred], e)
type TaskCheckT = [Assump] -> Type -> CExpr -> [CExpr] -> TI ([VPred], CExpr)

------

errorAtId :: (String -> ErrMsg) -> Id -> TI a
errorAtId econ i =
    let emsg = (getIdPosition i, econ (pfpString i))
    in  err emsg

findCons :: Type -> Id -> TI (Assump, Id)
findCons ct i = do
    -- traceM ("findCons: " ++ show (ct,i))
    r <- getSymTab
    case findConVis r i of
     Just [ConInfo { ci_id = ti, ci_assump = a, ci_pkg = pkg }] -> do
        recordPackageUse pkg
        return (updAssumpPos i a, ti)
     Just cs -> do
        s <- getSubst
        let ct' = apSub s ct
        case leftCon (expandSyn ct') of
         Nothing -> errorAtId (EConstrAmb (pfpString ct')) i
         Just di -> case [ (a, pkg) | ConInfo {ci_id = i', ci_assump = a, ci_pkg = pkg} <- cs, qualEq di i'] of
                   [(a, pkg)] -> do
                       recordPackageUse pkg
                       return (updAssumpPos i a, di)
                   []  -> errSuggest r i
                   _   -> internalError "findCons ambig"
     Nothing -> errSuggest r i
  where
    errSuggest :: SymTab -> Id -> TI (Assump, Id)
    errSuggest r i =
      let mSuggest = case findType r i of
            Just (TypeInfo _ KNum _ _ _) -> Just "valueOf"
            Just (TypeInfo _ KStr _ _ _) -> Just "stringOf"
            _ -> Nothing
      in err (getIdPosition i, EUnboundCon (pfpString i) mSuggest)

findTyCon :: Id -> TI TyCon
findTyCon i = do
    r <- getSymTab
    case findType r i of
     Just (TypeInfo (Just i') k _ ts@(TItype _ t) pkg) -> do
        recordPackageUse pkg
        -- It's a type alias.  If the left element of the alias is a
        -- constructor, find the type of that constructor; otherwise,
        -- give up and return the info that's available.
        case (leftCon t) of
            Just aliased_i -> findTyCon aliased_i
            Nothing -> return (TyCon i' (Just k) ts)
     Just (TypeInfo (Just i') k _ ts pkg) -> do
        recordPackageUse pkg
        return (TyCon i' (Just k) ts)
     Just (TypeInfo Nothing _ _ _ _) ->
        internalError ("findTyCon: unexpected numeric or string type: " ++ ppReadable i)
     Nothing -> errorAtId EUnboundTyCon i

findCls :: CTypeclass -> TI Class
findCls i = do
    r <- getSymTab
    case findSClass r i of
     Just cl -> do
        recordPackageUse (pkg_src cl)
        return cl
     Nothing -> errorAtId EUnboundTyCon (typeclassId i)

bitCls :: TI Class
bitCls = findCls (CTypeclass idBits)

literalCls :: TI Class
literalCls = findCls (CTypeclass idLiteral)

realLiteralCls :: TI Class
realLiteralCls = findCls (CTypeclass idRealLiteral)

sizedLiteralCls :: TI Class
sizedLiteralCls = findCls (CTypeclass idSizedLiteral)

stringLiteralCls :: TI Class
stringLiteralCls = findCls (CTypeclass idStringLiteral)

numEqCls :: TI Class
numEqCls = findCls (CTypeclass idNumEq)

-- Given a field "field_id" encountered in the program (as a field select,
-- a field update, a field definition in a struct literal, or a pattern)
-- and the type "struct_ty" for the struct from the context (possibly just
-- a variable if we don't know anything yet),
-- Returns the type information for that field, if it is able to identify
-- a single field by that name which matches that type.
-- If we know the struct type, then we start with that information.  If
-- the type is only visible internally and is not visible to the user
-- (because it was not imported) or if the fields of the type were not
-- exported with the type, then we want to report those errors first
-- (to maintain the abstraction); if the type does not have a field by
-- that name, then we report that.  Otherwise, if there is not enough
-- type information, we report an error if no field by that name exists,
-- or if multiple fields by that name exist.  If one field by that name
-- is found, then we assume the struct type is that type, and the user
-- will get a mismatch error later if not.
--
findFields :: Type -> Id -> TI (Assump, Id, Int)
findFields struct_ty0 field_id = do
    --traceM("findFields: " ++ ppReadable (struct_ty, field_id))
    symt <- getSymTab

    -- Figure out what we know about the struct type
    -- Return values:
    --    Nothing = Type is not concrete
    --    Just (qtc, isImp, isVis, Nothing)
    --            = Type is not a struct
    --    Just (qtc, isImp, isVis, Just fs)
    --            = Type is a struct
    -- where
    --    qtc   = qualified type name
    --    isImp = whether the data type was imported by the user
    --    isVis = whether the internals of the type were exported
    --    fs    = struct fields
    --
    let getTInfo t =
            case (leftTyCon (expandSyn t)) of
              Nothing -> Nothing
              Just (TyNum n _) -> Just (mkNumId n, True, True, Nothing)
              Just (TyStr s _) -> Just (mkStrId s, True, True, Nothing)
              Just (TyCon tc _ _) ->
                  case (findType symt tc) of
                    Just (TypeInfo (Just qtc) _ _ tcsort _)
                      -> let -- XXX would it be better to compute "isImp"
                             -- XXX by extracting the qualifier and looking
                             -- XXX in the import list?
                             isImp = case (findType symt (unQualId tc)) of
                                       Nothing -> False
                                       Just _  -> True
                             (isVis, tisort_info) = getSortInfo qtc tcsort
                         in  Just (qtc, isImp, isVis, tisort_info)
                    -- We expect it to be a value type
                    -- and we expect that the type has already been checked
                    _ -> internalError ("getTInfo: " ++ ppReadable tc)
        getSortInfo qtc tcsort@(TItype {}) =
            -- this should have been expanded
            internalError ("getSortInfo: " ++ ppReadable tcsort)
        getSortInfo qtc TIabstract =
            -- this should only occur for primitives
            (True, Nothing)
        getSortInfo qtc (TIatf {}) =
            -- ATF type constructors are not structs; expansion should have
            -- removed them, but if the type is still polymorphic treat as
            -- non-struct so the caller can report a proper type error
            (True, Nothing)
        getSortInfo qtc (TIdata [] _) =
            -- XXX we can't tell if its fields are visible
            (True, Nothing)
        getSortInfo qtc (TIdata (c:_) _) =
            case (findCon symt c) of
              Nothing -> internalError ("getSortInfo findCon: " ++
                                        ppReadable (qtc, c))
              Just cis ->
                  case [ (i, v) | (ConInfo { ci_id = i,
                                             ci_visible = v }) <- cis,
                                   i == qtc ] of
                    [(_, v)] -> (v, Nothing)
                    _ -> internalError ("getSortInfo findCon2: " ++
                                        ppReadable (qtc, c, cis))
        getSortInfo qtc (TIstruct _ []) =
            -- XXX we can't tell if its fields are visible
            (True, Just [])
        getSortInfo qtc (TIstruct _ fs@(f:_)) =
            case (findField symt f) of
              Nothing -> internalError ("getSortInfo findField: " ++
                                        ppReadable (qtc, f))
              Just fis ->
                  case [ (i, v) | (FieldInfo { fi_id = i,
                                               fi_visible = v }) <- fis,
                                   i == qtc ] of
                    [(_, v)] -> (v, Just fs)
                    _ -> internalError ("getSortInfo findField2: " ++
                                        ppReadable (qtc, f, fis))

    s <- getSubst
    let struct_ty = apSub s struct_ty0
    -- internally generated code is allowed to refer to invisible fields
    let ext = not (isInternal field_id)
              -- XXX some internal code is qualified but not marked
              -- XXX so for now allow this (Bug 1858)
              && isUnqualId field_id

    -- now report the error or the info
    case (getTInfo struct_ty) of
      Just (qtc, False, _, _) | ext ->
          err (getIdPosition field_id,
               EFieldsNotImp (pfpString qtc) (getIdQualString qtc))
      Just (qtc, _, False, _) | ext ->
          err (getIdPosition field_id, EFieldsNotVis (pfpString qtc))
      Just (qtc, _, _, Nothing) ->
          -- report the full type, not just the left constructor
          errorAtId (ENotStruct (pfpString struct_ty)) field_id
      Just (qtc, _, _, Just fs) ->
          case (findField symt field_id) of
            Nothing -> errorAtId (ENotField (pfpString qtc)) field_id
            Just fs ->
                case [ (i, a, n, pkg) | (FieldInfo { fi_id = i,
                                                      fi_arity = n,
                                                      fi_assump = a,
                                                      fi_pkg = pkg }) <- fs,
                                        i == qtc ] of
                  [(_, a, n, pkg)] -> do
                      recordPackageUse pkg
                      return (updAssumpPos field_id a, qtc, n)
                  [] -> errorAtId (ENotField (pfpString qtc)) field_id
                  xs -> internalError ("findFields ambig: " ++
                                       ppReadable (struct_ty, field_id, xs))
      Nothing ->
          -- not a concrete type, so we have no type to go on
          case (if ext then findFieldVis else findField) symt field_id of
            Nothing ->
                -- there are no structs with this field
                errorAtId EUnboundField field_id
            Just [FieldInfo {fi_id = qtc, fi_arity = n, fi_assump = a, fi_pkg = pkg }] -> do
                -- there is only one struct with this field
                -- if the expression is not that type, the user will get a
                -- mismatch error later
                recordPackageUse pkg
                return (updAssumpPos field_id a, qtc, n)
            Just fs ->
                let tis = map (pfpString . fi_id) fs
                in  errorAtId (EFieldAmb tis) field_id

updAssumpPos :: Id -> Assump -> Assump
updAssumpPos i (i' :>: s) = setIdPosition (getIdPosition i) i' :>: s


-- (A push makes the context strictly: it forces the goal's class key at
-- once.)
incrementSatStack :: TSSatElement -> TI ()
incrementSatStack x = do
  superstack <- gets tsSatStack
  let old@(TSSatStack oldstack idx _) = sizedStackPeek superstack
  flags <- getFlags
  case (sizedStackPush (maxTIStackDepth flags) oldstack x) of
    Nothing -> (err(noPosition,ETypeStackOverflow))
    Just newstack@(SizedStack sz _) -> do
        let ctx = TSSatStack newstack (classIdxPush (satElementKey x) x idx) (Just old)
        ctx `seq` modify (\ts -> ts { tsSatStack =
                                        sizedStackModify superstack (const ctx)})
        modifySatKeys (onTopKeys (GKNone :))
        bumpRecMaxima sz 0

decrementSatStack :: TI ()
decrementSatStack = do
  superstack <- gets tsSatStack
  let ctx = case sizedStackPeek superstack of
              TSSatStack _ _ (Just below) -> below
              _ -> internalError "decrementSatStack: stack underflow"
  ctx `seq` modify (\ts -> ts { tsSatStack =
                                  sizedStackModify superstack (const ctx)})
  modifySatKeys (onTopKeys dropTopKey)

modifySatKeys :: ([[GKeys]] -> [[GKeys]]) -> TI ()
modifySatKeys f = do
  s <- get
  let kss' = f (tsSatKeys s)
  kss' `seq` put (s { tsSatKeys = kss' })

-- (the stack's current context, top entry first, and the rest)
onTopKeys :: ([GKeys] -> [GKeys]) -> [[GKeys]] -> [[GKeys]]
onTopKeys f (ks : kss) = let ks' = f ks in ks' `seq` (ks' : kss)
onTopKeys _ [] = internalError "onTopKeys: no context"

dropTopKey :: [GKeys] -> [GKeys]
dropTopKey (_ : ks) = ks
dropTopKey [] = internalError "dropTopKey: no entry"

-- The pool's keys of the current context's sat-stack entries, top first.
getSatKeys :: TI [GKeys]
getSatKeys = gets (headOrErr "getSatKeys" . tsSatKeys)

setSatKeys :: [GKeys] -> TI ()
setSatKeys ks = modifySatKeys (onTopKeys (const ks))

-- The keys of the entry on top of the stack.
setTopSatKeys :: GKeys -> TI ()
setTopSatKeys k = modifySatKeys (onTopKeys ((k :) . dropTopKey))

getSatStack :: TI [EPred]
getSatStack = do
  superstack <- gets tsSatStack
  case (sizedStackPeek superstack) of
    TSSatStack (SizedStack _ l) _ _ -> return l

-- | getSatStack's entries by class (classIdxLookup).
getSatStackIdx :: TI (ClassIdx EPred)
getSatStackIdx = do
  superstack <- gets tsSatStack
  case (sizedStackPeek superstack) of
    TSSatStack _ idx _ -> return idx

pushSatStackContext :: TI ()
pushSatStackContext = do
  superstack <- gets tsSatStack
  flags <- getFlags
  case (sizedStackPush (maxTIStackDepth flags) superstack emptySatStack) of
    Nothing -> err (noPosition, ETypeSuperStackOverflow)
    Just newstack@(SizedStack sz _) -> do
        modify (\ts -> ts { tsSatStack = newstack })
        modifySatKeys ([] :)
        bumpRecMaxima 0 sz

popSatStackContext :: TI ()
popSatStackContext = do
  superstack <- gets tsSatStack
  modify (\ts -> ts { tsSatStack = (sizedStackPop superstack) })
  modifySatKeys (drop 1)


-- CStruct and CPstruct can be used for either structs fields or
-- constructor fields, and it is up to the typechecker to decide
-- which is intended.
-- Given:
--   * Possible clue from the syntax (True for struct)
--   * Return-type expected by context
--   * Id of the type/constructor
--   * Ids of the fields
-- Returns either Left (for typecheck as struct) or Right (for constr).
--   * Left TyCon  = struct and its type constructor
--   * Right Id    = constructor and the name of its struct arg type
--
disambiguateStruct :: Maybe Bool -> Type -> Id -> [Id] ->
                      TI (Either TyCon Id)
disambiguateStruct mb td c is =
    case mb of
      Just True -> maybeFindTyCon c >>= handleStruct
      Just False -> findCons td c >>= \ (_, ti) -> handleCons (Right ti)
      Nothing -> do
        -- Determine if there is a constructor by this name
        mcons <- maybeFindCons c
        -- Determine if there is a type by this name
        mtype <- maybeFindTyCon c
        -- Attempt to disambiguate
        case (mcons, mtype) of
          (Nothing, _)        -> handleStruct mtype
          (Just mti, Nothing) -> handleCons mti
          (Just (Left _), Just _) ->
            -- XXX we could do more checking, or possibly warn?
            handleStruct mtype
          (Just (Right ti), Just _) -> do
            -- Confirm that the constructor has an SDataCon argument with named fields
            arg_is_cons <- isSDataConNamedM (mkTCId ti c)
            if arg_is_cons
              then -- XXX further check that some of the names match?
                   handleCons (Right ti)
              else handleStruct mtype
 where
   -- Whether a constructor with this name and expected return type exists,
   -- and then either its type or ambiguity errors (if multiple exist)
   maybeFindCons :: Id -> TI (Maybe (Either [EMsg] Id))
   maybeFindCons i =
      let isEConstrAmb (_, EConstrAmb _ _) = True
          isEConstrAmb _ = False
          err_handler es = if all isEConstrAmb (errmsgs es)
                           then return $ Just (Left (errmsgs es))
                           else return $ Nothing
      in  (findCons td i >>= \ (_, ti) -> return (Just (Right ti))) `handle` err_handler

   maybeFindTyCon i =
      (findTyCon i >>= \ tyc -> return (Just tyc)) `handle` \ _ -> return Nothing

   isSDataConNamedM i = do
      mcons <- maybeFindTyCon i
      case mcons of
          Just (TyCon _ _ (TIstruct (SDataCon _ True) fs))
            -> return True
          _ -> return False

   handleCons (Left es) = errs "tiExpr CStruct" es
   handleCons (Right ti) = do
      -- Confirm that the constructor has an SDataCon argument with named fields
      arg_is_cons <- isSDataConNamedM (mkTCId ti c)
      if arg_is_cons
        then return (Right (mkTCId ti c))
        else err (getPosition c, EConstrFieldsNotNamed (pfpString c) (pfpString ti))

   handleStruct (Just tyc@(TyCon c' (Just k) (TIstruct ss qfs))) =
      return (Left tyc)
   handleStruct (Just (TyCon _ _ _)) =
      err (getPosition c, ENotStructId (pfpString c))
   handleStruct _ =
      -- This is the same message that findTyCon would report
      err (getPosition c, EUnboundTyCon (pfpString c))
