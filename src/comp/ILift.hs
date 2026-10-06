{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE PatternGuards #-}
module ILift(iLift) where
import PPrint(ppReadable)
import Flags(Flags, ifLift)
import Error(ErrorHandle)
import ISyntax
import ISyntaxUtil(ieNot,
                   iTrue,
                   iGetType, ieIf, ieIfxA, flatActionA,
                   joinActionsA,
                   isTrue, isFalse, ieAndOpt, ieOrOpt,
                   iDefMap, isPairType
                   )
import ITransform(iTransExpr, iTransBoolExpr, iTransAction)


-- for trace arguments
import IOUtil(progArgs)
import Util(tracep)

trace_lift :: Bool
trace_lift = "-trace-lift" `elem` progArgs

iLift :: ErrorHandle -> Flags -> IModule PostElab -> IModule PostElab
iLift errh flags imod@(IModule { imod_local_defs = ds,
                                 imod_rules      = rs,
                                 imod_interface  = ifc }) =
    imod { imod_local_defs = ds', imod_rules = rs', imod_interface = ifc' }
  where ds'  = map (iLiftDef errh flags) ds
        rs'  = (iLiftRules errh flags) rs
        ifc' = map (iLiftIfc errh flags) ifc
--      itvs'? do we need to go into state vars?

-- just lift the def inside
iLiftDef :: ErrorHandle -> Flags -> IDef PostElab -> IDef PostElab
iLiftDef errh flags def = iDefMap (iLiftExpr errh flags) def

-- just lift the def inside
iLiftIfc :: ErrorHandle -> Flags -> IEFace PostElab -> IEFace PostElab
iLiftIfc errh flags (IEFace i x maybe_e maybe_rs wp fi) =
  IEFace i x (do { (e,t) <- maybe_e ; return ((iLiftExpr errh flags e),t) })
             (do { rs <- maybe_rs ; return (iLiftRules errh flags rs) })
             wp fi

-- an expression holds no action (a method's action is its rule), so
-- there is nothing to lift in a def or a method value
iLiftExpr :: ErrorHandle -> Flags -> IExpr PostElab -> IExpr PostElab
iLiftExpr errh flags e = e

iLiftRules :: ErrorHandle -> Flags -> IRules PostElab -> IRules PostElab
iLiftRules errh flags (IRules sps rs) = IRules sps (map (iLiftRule errh flags) rs)

iLiftRule :: ErrorHandle -> Flags -> IRule PostElab -> IRule PostElab
iLiftRule errh flags r =
    r { irule_body = iLiftAction errh flags $ irule_body r }

-- Conditional actions (an action, extracted from an if, combined
-- with an expression describing its condition)
-- this is an intermediate representation for power-lifting
-- open question: how to handle explicit ExpIfs and NoExpIfs
-- i.e. splits and nosplits
data IActionCond = IActionCond {
                    action :: IAction,
                    condition :: IExpr PostElab
                  }

-- adds an additional condition to an ActionCond
addCond :: Flags -> IExpr PostElab -> IActionCond -> IActionCond
addCond flags newcond (IActionCond { action = ac, condition = c}) =
  IActionCond { action = ac, condition = (iTransBoolExpr flags) (newcond `ieAndOpt` c)}

-- transform an action expression into a list of conditional actions (i.e. pushing all the ifs into conditions)
condActions :: Flags -> IAction -> [IActionCond]

condActions flags e = concatMap (condActions1 flags) (flatActionA e)

condActions1 :: Flags -> IAction -> [IActionCond]

-- if case (recurses using condActions to handle action lists)
-- (an annotated conditional is a different node, as the marker
-- application around the if was: it is a base case)
condActions1 flags (AIf SplitDefault c t f) =
   let ts = condActions flags t
       fs = condActions flags f in
       (map (addCond flags c) ts) ++ (map (addCond flags ((iTransBoolExpr flags) (ieNot c))) fs)

-- base case - the action always happens
condActions1 flags e = [IActionCond { action = e, condition = iTrue }]

-- convert an actionCond back into an action expression
actionCondToIExpr :: ErrorHandle -> Flags -> IActionCond -> IAction
actionCondToIExpr errh flags (IActionCond { action = ac, condition = c}) =
  (fst (iTransAction errh (ieIfxA ((iTransBoolExpr flags) c) ac ANoActions)))

-- implicitly assuming action list
-- and flattens out
iLiftAction :: ErrorHandle -> Flags -> IAction -> IAction
iLiftAction errh flags e =
    if (ifLift flags)
    then (joinActionsA (concatMap (lift1 errh flags) (flatActionA e)))
    else (joinActionsA (flatActionA e))

-- takes in an expression and produces a list of "lifted" expressions - where conditional actions are replaced with conditional
-- expressions where possible e.g. if p r:=5 else r:=7 --> r:= if p then 7 else 5
-- now implements "power lifting" - combining the same method calls together if they are in
-- mutually exclusive if branches even if the methods are not always called
lift1 :: ErrorHandle -> Flags -> IAction -> [IAction]

-- bulk of interesting lifting happens when we find an if expression
lift1 errh flags ifexp@(AIf SplitDefault cunsimp t f) =
  --
  -- loopT loops through the true-arm actions
  -- parameters are lifted expressions, unliftable (true) expressions, true expressions to scan, false actions to lift against
  let -- convert an action cond list back into an expression list - reversing for action order preservation
      -- simplify the predicate to be safe
      c = ((iTransBoolExpr flags) cunsimp)
      raclcvt acl = (reverse (map (actionCondToIExpr errh flags) acl))

      -- end up with only lifted expressions we are done and return that
      loopT lifted [] [] [] = (raclcvt lifted)
      --
      -- done scanning but have some lifted expressions, some unliftable expressions and some false expressions
      -- tack an if of the unliftable and false expressions onto the lifted expressions
      loopT lifted unlifted [] false =
          --
          -- lifted should be first because we want to put actions with broader conditions first
          -- this will mean that we will try further lifting with "better" actions first
          -- and lift this whole thing out all the way (since we go 95% anyway and I think this
          -- combines predicates usefully
          (raclcvt lifted) ++
          (raclcvt (map (addCond flags c) unlifted)) ++ -- true side
          (map (actionCondToIExpr errh flags . addCond flags ((iTransBoolExpr flags) (ieNot c))) false) -- false side

      -- going through the true expressions and find a module method call
      loopT lifted unlifted (firstT@(IActionCond {action = ACallMethod Nothing expT tsT icsvT@(ICon _ _ (ICStateVar svT)) argsT,
                                                     condition = firstTcond}):restT) f =
      -- loop through the list of false actions
      -- first parameter is scanned false actions, second is false actions to scan


        -- done scanning the list of false actions (restF = [])
        let loopF scanned [] = loopT lifted (firstT:unlifted) restT (reverse scanned)
            --
            -- when we find a matching method call for a module lift the method call (into a joint ActionCond)
            -- and put conditional expressions on the arguments
            -- the length check is for things like $display so we do not lift when there are different numbers of arguments
            loopF scanned ((firstF@(IActionCond {action = ACallMethod Nothing expF _ (ICon _ _ (ICStateVar svF)) argsF,
                                                 condition = firstFcond})):restF) | (expF == expT) && (svF == svT) &&
                                                                                    ((length argsT) == (length argsF)),
                                                                                    Just newargs <- mapM (uncurry (mergeLiftArg errh c)) (zip argsT argsF) =
              -- just make an ActionCond out of this when it matches
              -- eventual conversion back into IExpr will force simplification
              -- c is used as the predicate to determine the argument value because when c is true the T branch should be executed
              -- note that c has already been optimized so further optimization on it directly is redundant
              loopT ((IActionCond { action = (fst
                                               (iTransAction errh
                                                (ACallMethod Nothing expT tsT icsvT newargs))),
                                    condition = genLiftCond flags c firstTcond firstFcond
                                  }
                     ):lifted)
                    unlifted
                    restT
                    ((reverse scanned) ++ restF)
            --
            -- otherwise keep scanning the list of false actions
            loopF scanned (firstF:restF) = loopF (firstF:scanned) restF in
        --
        -- start with the list of scanned false actions empty
              loopF [] f

      -- ----
      -- do the same for actionvalue calls, with an additional avAction_ selector
      -- (the true side's selector is kept, as before)
      loopT lifted unlifted
            (firstT@(IActionCond {action = ACallMethod (Just av) expT tsT icsvT@(ICon _ _ (ICStateVar svT)) argsT,
                                  condition = firstTcond}):restT) f =
            -- loop through the list of false actions
            -- first parameter is scanned false actions, second is false actions to scan
            --
            -- done scanning the list of false actions (restF = [])
        let loopF scanned [] = loopT lifted (firstT:unlifted) restT (reverse scanned)
            --
            -- when we find a matching method call for a module lift the method call (into a joint ActionCond)
            -- and put conditional expressions on the arguments
            -- the length check is for things like $display so we do not lift when there are different numbers of arguments
            loopF scanned ((firstF@(IActionCond {action = ACallMethod (Just _) expF _ (ICon _ _ (ICStateVar svF)) argsF,
                                                 condition = firstFcond})):restF) | (expF == expT) && (svF == svT) &&
                                                                                    ((length argsT) == (length argsF)),
                                                                                    Just newargs <- mapM (uncurry (mergeLiftArg errh c)) (zip argsT argsF) =
              -- just make an ActionCond out of this when it matches
              -- eventual conversion back into IExpr will force simplification
              -- c is used as the predicate to determine the argument value because when c is true the T branch should be executed
              -- note that c has already been optimized so further optimization on it directly is redundant
              loopT ((IActionCond { action = (fst
                                               (iTransAction errh
                                                (ACallMethod (Just av) expT tsT icsvT newargs))),
                                    condition = genLiftCond flags c firstTcond firstFcond
                                  }
                     ):lifted)
                    unlifted
                    restT
                    ((reverse scanned) ++ restF)
            --
            -- otherwise keep scanning the list of false actions
            loopF scanned (firstF:restF) = loopF (firstF:scanned) restF in
              --
              -- start with the list of scanned false actions empty
              loopF [] f
              -- ----

      -- catch-all case: didn't match so can't be lifted
      loopT lifted unlifted (firstT:restT) f = loopT lifted (firstT:unlifted) restT f

  in
      tracep trace_lift ("found if: " ++ (ppReadable ifexp)) $
      let result = (loopT [] [] (concatMap (condActions flags) (concatMap (lift1 errh flags) (flatActionA t)))
                                (concatMap (condActions flags) (concatMap (lift1 errh flags) (flatActionA f)))) in
        tracep trace_lift ("if lifting result: " ++ (ppReadable result)) $ result

-- lift "through" an explicit split or nosplit
lift1 errh flags exp@(AIf p c t f) =
      tracep trace_lift ("found noexp or exp:" ++ (ppReadable exp)) $
      let es = (lift1 errh flags (AIf SplitDefault c t f)) in
          -- can be multiple ifs in the result, so map over and be safe
          [case e' of { AIf SplitDefault c' t' f' -> AIf p c' t' f'; _ -> e' } | e' <- es ]

-- if nothing else matched there is no lifting work
lift1 errh flags e = tracep trace_lift ("default: " ++ (show e)) $ [e]


-- build the expression cond && otrue || ~cond && ofalse
-- in an optimized manner
genLiftCond :: Flags -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab
genLiftCond flags cond otrue ofalse | isTrue otrue, isTrue ofalse = iTrue
                                    | isTrue otrue                = cond  `ieOrOpt` ofalse
                                    | isTrue ofalse               = otrue `ieOrOpt` (ieNot cond)
                                    | isTrue cond                 = otrue
                                    | isFalse cond                = ofalse
genLiftCond flags cond otrue ofalse =
    (iTransBoolExpr flags) $ (cond `ieAndOpt` otrue) `ieOrOpt` ( (ieNot cond) `ieAndOpt` ofalse)


-- Merge one argument pair of two mutually exclusive calls to the same
-- method into (if c then argT else argF), distributing the mux over
-- PrimPair structure.  Since the port-splitting rework, tuple-typed
-- method arguments must remain tuple constructions all the way to the
-- backend (AVerilogUtil.vDefMpd only renders literal tuple defs), so a
-- whole-tuple mux def would be an internal error there.  Returns
-- Nothing when a tuple-typed argument does not expose a tuple literal
-- on both sides (looking through ICValue definition references); the
-- caller then skips lifting that call pair and leaves the two calls
-- for ACleanup, which merges per element at the ASyntax level where an
-- opaque tuple reference can be selected with ATupleSel.
mergeLiftArg :: ErrorHandle -> IExpr PostElab -> IExpr PostElab -> IExpr PostElab -> Maybe (IExpr PostElab)
mergeLiftArg errh c argT argF
    | isPairType (iGetType argT) =
        case (unwrap argT, unwrap argF) of
          (IAps conT@(ICon i _ (ICTuple {})) tsT esT,
           IAps      (ICon i' _ (ICTuple {})) _  esF)
              | i == i' && length esT == length esF -> do
                  es <- sequence (zipWith (mergeLiftArg errh c) esT esF)
                  return (IAps conT tsT es)
          _ -> Nothing
    | otherwise =
        Just $ fst $ iTransExpr errh $
            ieIf (iGetType argT) c (fst (iTransExpr errh argT))
                                   (fst (iTransExpr errh argF))
  where
    unwrap (ICon _ _ (ICValue { iValDef = e })) = unwrap e
    unwrap e = e
