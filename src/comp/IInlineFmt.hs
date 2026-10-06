{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
module IInlineFmt(iInlineFmt) where
import PPrint
import ErrorUtil
import ISyntax
import IInlineUtil(iSubstIfc, iSubstWhen)
import ISyntaxUtil(itString, itBit, irulesMap, irulesMapM, itFmt, itGetArrows, itFun, itInst, iGetType, joinActions, iMkString, isitAction, isitActionValue_, iDefMapM, iDefsMap, emptyFmt,
                   icIf, joinActionsA, onActionArgs, onActionArgsM)
import Id
import Prim
import PreIds(idActionValue_, idArrow, tmpVarIds, idAVValue_, idAVAction_, idPrimFmtConcat)
import ForeignFunctions
import Control.Monad.Except(ExceptT, runExceptT)
import Control.Monad.State
import Error(EMsg, ErrorHandle, bsError)
import Position(noPosition)
import CType(TISort(..), StructSubType(..))
import qualified Data.Map as M
-- import Debug.Trace(trace)

type F a = StateT (Int, [IDef a]) (ExceptT EMsg (IO))

newFFCallNo :: (F PostElab) Integer
newFFCallNo = do (n, ds) <- get
                 put ((n + 1), ds)
                 return (toInteger n)

addDefs :: [IDef PostElab] -> (F PostElab) ()
addDefs ds = do (n, ds') <- get
                put (n, ds' ++ ds)
                return ()

-- #############################################################################
-- #
-- #############################################################################

-- Every rewrite below comes in two forms: over an expression (a def, a
-- method value, a predicate, a state variable argument, the arguments
-- of a call) and over an action (a rule body), suffixed A.  The action
-- form holds the arms that rewrote an action-typed node (a task call
-- and its arguments, the action half of an ActionValue task); the
-- expression form keeps the arms a value can meet.  Each pair visits
-- a tree in the order the single traversal of the expression did, so
-- the fcallNo cookies (the ATaskAction/ATaskValue numbers of the .ba)
-- are allocated in the same order.

iInlineFmt :: ErrorHandle -> IModule PostElab -> IO (IModule PostElab)
iInlineFmt errh imod =
    do let imod_fmt = iInlineFmts imod
       let ffcallNo = (imod_ffcallNo imod_fmt)
       let ds       = (imod_local_defs imod_fmt)
       result <- runExceptT (runStateT (splitFmtsF imod_fmt) (ffcallNo, []))
       case result of
            Right x@(imod', (ffcallNo', ds')) ->
                return (imod' {imod_local_defs = ds ++ ds',
                               imod_ffcallNo = ffcallNo'})
            Left msg -> bsError errh [msg]

splitFmtsF :: IModule PostElab -> F PostElab (IModule PostElab)
splitFmtsF imod@(IModule { imod_local_defs  = ds,
                           imod_rules       = rs,
                           imod_interface   = ifc,
                           imod_state_insts = state_vars}) =
    do  let ds' = [ IDef id t e p | IDef id t e p <- ds, (t /= itFmt) ] -- remove (now unused defs)
            updateDef = iDefMapM ssplitFmt
        ds'' <- mapM updateDef ds'

        ifc' <- ssplitFmt_ifc ifc
        rs'  <- irulesMapM ssplitFmt ssplitFmtA rs
        let updateStateVar (name, sv@(IStateVar { isv_iargs = es })) = do es' <- mapM ssplitFmt es
                                                                          return (name, sv { isv_iargs = es' })
        state_vars' <- mapM updateStateVar (imod_state_insts imod)
        return imod { imod_local_defs  = ds'',
                      imod_rules       = rs',
                      imod_interface   = ifc',
                      imod_state_insts = state_vars' }

ssplitFmt :: IExpr PostElab -> F PostElab (IExpr PostElab)
ssplitFmt e =
    do expr' <- fsplitFmt e
       splitFmt expr'

ssplitFmtA :: IAction -> F PostElab IAction
ssplitFmtA a =
    do a' <- fsplitFmtA a
       splitFmtA a'

--------------------------------------------------------------------------------
-- Special handling for $fdisplay, $fwrite etc.
-- 1) Remove first arg (the file descriptor)
-- 2) Do split processing as usual
-- 3) Add first arg back to all the "descendant" foreign functions.
--------------------------------------------------------------------------------
-- (the file-descriptor tasks are actions; an expression only holds
-- their arguments)
fsplitFmt :: IExpr PostElab -> F PostElab (IExpr PostElab)
fsplitFmt (IAps x ts es) =
    do  es' <- mapM fsplitFmt es
        return (IAps x ts es')
fsplitFmt x = return x

fsplitFmtA :: IAction -> F PostElab IAction
fsplitFmtA (ACallForeign Nothing (ICon fid t f@(ICForeign { })) (Just ([], e:rest)))
              | isFileId fid  =
    do  expr' <- splitFmtA (ACallForeign Nothing (ICon fid t' f) (Just ([], rest)))
        return (addFileArgA e expr')
    where (_ , rt) = itGetArrows (getInnerType t)
          at'      = map iGetType rest
          t'       = foldr1 itFun (at' ++ [rt])
fsplitFmtA a = onActionArgsM fsplitFmt fsplitFmtA a

addFileArg :: IExpr PostElab -> IExpr PostElab -> IExpr PostElab
addFileArg e (IAps x ts es) = (IAps x ts (map (addFileArg e) es))
addFileArg e x              = x

addFileArgA :: IExpr PostElab -> IAction -> IAction
addFileArgA e (ACallForeign mav (ICon fid t f@(ICForeign { })) (Just ([], es)))
             | isFileId fid = (ACallForeign mav (ICon fid t' f) (Just ([], es')))
    where (_ , rt) = itGetArrows (getInnerType t)
          es'      = e : es
          at'      = map iGetType es'
          t'       = foldr1 itFun (at' ++ [rt])
addFileArgA e a = onActionArgs (addFileArg e) (addFileArgA e) a

splitFmt :: IExpr PostElab -> F PostElab (IExpr PostElab)
splitFmt e =
  do let e0 = replaceDisplays e
     e1 <- unNestFmts [] [] e0
     let e2 = combineFmts e1
         e3 = promoteConcat False e2
     e4 <- splitFF True [] [] e3
     e5 <- removeConcat e4
     return e5;

splitFmtA :: IAction -> F PostElab IAction
splitFmtA a =
  do let a0 = replaceDisplaysA a
     a1 <- unNestFmtsA a0
     let a2 = combineFmtsA a1
         a3 = promoteConcatA a2
     a4 <- splitFFA True [] [] a3
     a5 <- removeConcatA a4
     return a5;

-- first turn any $display (and friends) calls into $write (and friends) calls
-- but only if there are arguments of type Fmt ... leave the rest alone
-- (they are actions; an expression only holds their arguments)
replaceDisplays :: IExpr PostElab -> IExpr PostElab
replaceDisplays (IAps x ts es) = (IAps x ts (map replaceDisplays es))
replaceDisplays x = x

replaceDisplaysA :: IAction -> IAction
replaceDisplaysA (ACallForeign mav (ICon fid t f@(ICForeign { })) (Just (_, es)))
                 | isDisplayId(fid) && (any (== itFmt) at) = expr
       where (_ , rt) = itGetArrows (getInnerType t)
             at       = map iGetType es
             fid'     = fromDisplayId fid
             name'    = getIdString(unQualId(fid'))
             es'      = es ++ [iMkString "\n"]
             at'      = map iGetType es'
             t'       = foldr1 itFun (at' ++ [rt])
             expr = (ACallForeign mav (ICon fid' t' (f {fName = name'})) (Just ([], es')))
replaceDisplaysA a = onActionArgs replaceDisplays replaceDisplaysA a

-- eliminated nested Fmts (replace with primFmtConcat ops).
-- after this step all $formats are leaves
-- top down processing
unNestFmts :: [IExpr PostElab] -> [IExpr PostElab] -> IExpr PostElab -> F PostElab (IExpr PostElab)
unNestFmts _   _         (IAps (ICon _ t (ICForeign { })) [] [e])
                 | iGetType e == itFmt && rt == itFmt =
       do e' <- unNestFmts [] [] e
          return e'
       where (_ , rt) = itGetArrows (getInnerType t)
unNestFmts []  []      x@(IAps (ICon _ t (ICForeign { })) [] es@(e:rest))
                 | rt == itFmt =
       do e' <- unNestFmts [e] rest x
          return e'
       where (_ , rt) = itGetArrows (getInnerType t)
unNestFmts es0 []      x@(IAps (ICon _ t (ICForeign { })) [] es@(e:rest))
                 | rt == itFmt = return x
       where (_ , rt) = itGetArrows (getInnerType t)
unNestFmts es0 (e:rest)  (IAps (ICon fid t f@(ICForeign { })) [] es)
                 | iGetType e == itFmt && rt == itFmt =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- unNestFmts [] [] (IAps (ICon fid t' (f {fcallNo = (Just n0)}))  [] es0)
          e1 <- unNestFmts [] [] (IAps (ICon fid t'' (f {fcallNo = (Just n1)})) [] (e:rest))
          return (IAps (ICon idPrimFmtConcat tc (ICPrim PrimFmtConcat)) [] [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType (e:rest)
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
             tc       = foldr1 itFun [itFmt, itFmt, itFmt]
unNestFmts es0@(e:rest) es1 (IAps (ICon fid t f@(ICForeign { })) [] es)
                 | iGetType (last es0) == itFmt && rt == itFmt =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- unNestFmts [] [] (IAps (ICon fid t' (f {fcallNo = (Just n0)}))  [] es0)
          e1 <- unNestFmts [] [] (IAps (ICon fid t'' (f {fcallNo = (Just n1)})) [] es1)
          return (IAps (ICon idPrimFmtConcat tc (ICPrim PrimFmtConcat)) [] [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType es1
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
             tc       = foldr1 itFun [itFmt, itFmt, itFmt]
unNestFmts es0 (e:rest) x@(IAps (ICon fid t f@(ICForeign { })) [] es) =
       do e' <- unNestFmts (es0 ++ [e]) rest x
          return e'
unNestFmts es0 es1      (IAps x ts es) =
       do es' <- mapM (unNestFmts es0 es1) es
          return (IAps x ts es')
unNestFmts _   _      x  = return x


-- (the scan state is the Fmt-returning call's own; an action's
-- arguments are entered with none)
unNestFmtsA :: IAction -> F PostElab IAction
unNestFmtsA a = onActionArgsM (unNestFmts [] []) unNestFmtsA a

combineFmts :: IExpr PostElab -> IExpr PostElab
combineFmts (IAps (ICon _ _ (ICPrim PrimFmtConcat)) _
             [(IAps (ICon fid t f@(ICForeign { })) ts0 es0),
              (IAps (ICon _     _ (ICForeign {})) ts1 es1)]) =
   let es = (es0 ++ es1)
       ts = (map iGetType es)
       (_ , rt) = itGetArrows (getInnerType t)
       tc = foldr1 itFun (ts ++ [rt])
       e = (IAps (ICon fid tc f) [] es)
   in e
combineFmts (IAps c ts es) =
   let es' = map combineFmts es
   in (IAps c ts es')
combineFmts e = e


combineFmtsA :: IAction -> IAction
combineFmtsA a = onActionArgs combineFmts combineFmtsA a

-- next move primFmtConcats up so that after this step, all the primFmtConcats in any
-- expression come first
-- bottom up processing
promoteConcat :: Bool -> IExpr PostElab -> IExpr PostElab
promoteConcat r (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti es@[cond, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e0, e1]), e2]) =
  promoteConcat False (IAps cc tc [(IAps ci ti [cond, e0, e2]), (IAps ci ti [cond, e1, emptyFmt])])
promoteConcat r (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e2, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e0, e1])]) =
  promoteConcat False (IAps cc tc [(IAps ci ti [cond, e2, e0]), (IAps ci ti [cond, emptyFmt, e1])])
promoteConcat False (IAps x@(ICon _ _ (ICForeign {})) ts es) =
  IAps x ts (map (promoteConcat False) es)
promoteConcat False (IAps x ts es) =
  promoteConcat True (IAps x ts (map (promoteConcat False) es))
promoteConcat _ x = x


promoteConcatA :: IAction -> IAction
promoteConcatA a = onActionArgs (promoteConcat False) promoteConcatA a

-- next the first phase of action-ff splitting
-- all action-ff calls (which include Fmt arguments) are split
-- into multiple ff calls (along the Fmt argument boundaries)
-- (an expression holds no action-ff call, so only the $format arm and
-- the recursion remain here; the splitting is splitFFA)
splitFF :: Bool -> [IExpr PostElab] -> [IExpr PostElab] -> IExpr PostElab -> F PostElab (IExpr PostElab)
splitFF d _   _       x@(IAps (ICon _ t (ICForeign { })) [] [e])
                 | iGetType e == itFmt && rt == itFmt =
       splitFF d [] [] e
       where (_ , rt) = itGetArrows (getInnerType t)
splitFF d es0 es1      (IAps x ts es) =
       do es' <- mapM (splitFF d es0 es1) es
          return (IAps x ts es')
splitFF _ _   _      x  =  return x

-- the scan: es0 are the arguments already scanned, es1 those still to
-- scan, of the call the arms rebuild
splitFFA :: Bool -> [IExpr PostElab] -> [IExpr PostElab] -> IAction -> F PostElab IAction
splitFFA d []  []      x@(ACallForeign Nothing (ICon _ t (ICForeign { })) (Just ([], e:rest)))
                 | isActionFFWithFmtsT t =
       do x' <- splitFFA d [e] rest x
          x''  <- removeConcatA x'
          x''' <- reduceFmtA x''
          return x'''
splitFFA d []  []   y@(ACallForeign (Just av@(AVSel (ICon _ t' (ICSel { })) _)) (ICon fid t ff@(ICForeign { })) (Just ([], e:rest)))
                 | isAVFFWithFmtsT t && rt' == itAction =
       do x'    <- splitFFA d [e] rest y
          x''   <- removeConcatA x'
          x'''  <- reduceFmtA x''
          x'''' <- update d x'''
          return x''''
      where (_ , rt)  = itGetArrows (getInnerType t)
            (_ , rt') = itGetArrows (getInnerType t')
            update False r                            = return r
            -- (a join of one action does not exist)
            update _     r@(AJoin _ _) =
             do addDefs defs
                return (joinActionsA [r', f])
             where f            = ACallForeign (Just av) (ICon fid t'' ff) (Just ([], args))
                   vs           = createValueExprs r
                   tconcat (xs, ys, zs) = (concat xs, concat ys, concat zs)
                   (refs, defs, as) = tconcat (unzip3 (map createRefsAndDefsAndActions vs))
                   r'   = joinActionsA as
                   args        = refs
                   at''        = map iGetType args
                   t''  = foldr1 itFun (at'' ++ [rt])
            update _     r                            = return r
splitFFA _ es0 []      x@(ACallForeign Nothing (ICon _ t (ICForeign { })) (Just ([], _:_)))
                 | rt == itAction =
       return x
       where (_ , rt) = itGetArrows (getInnerType t)
splitFFA _ es0  []   y@(ACallForeign (Just (AVSel (ICon _ t' (ICSel { })) _)) (ICon _ t (ICForeign { })) (Just ([], _:_)))
                 | isAVFFWithFmtsT t && rt' == itAction =
       return y
       where (_ , rt') = itGetArrows (getInnerType t')
splitFFA _ es0 (e:rest)  (ACallForeign Nothing (ICon fid t f@(ICForeign { })) (Just ([], es)))
                 | iGetType e == itFmt =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- splitFFA False [] [] (ACallForeign Nothing (ICon fid t' (f {fcallNo = (Just n0)})) (Just ([], es0)))
          e1 <- splitFFA False [] [] (ACallForeign Nothing (ICon fid t'' (f {fcallNo = (Just n1)})) (Just ([], e:rest)))
          return (joinActionsA [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType (e:rest)
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
splitFFA _ es0 (e:rest)   (ACallForeign (Just av) (ICon fid t f@(ICForeign { })) (Just ([], es)))
                 | iGetType e == itFmt =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- splitFFA False [] [] (ACallForeign (Just av) (ICon fid t' (f {fcallNo = (Just n0)})) (Just ([], es0)))
          e1 <- splitFFA False [] [] (ACallForeign (Just av) (ICon fid t'' (f {fcallNo = (Just n1)})) (Just ([], e:rest)))
          return (joinActionsA [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType (e:rest)
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
splitFFA _ es0@(e:rest) es1 x@(ACallForeign Nothing (ICon fid t f@(ICForeign { })) (Just ([], es)))
                 | iGetType (last es0) == itFmt && isActionFFWithFmtsT t =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- splitFFA False [] [] (ACallForeign Nothing (ICon fid t' (f {fcallNo = (Just n0)})) (Just ([], es0)))
          e1 <- splitFFA False [] [] (ACallForeign Nothing (ICon fid t'' (f {fcallNo = (Just n1)})) (Just ([], es1)))
          return (joinActionsA [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType es1
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
splitFFA _ es0@(e:rest) es1 (ACallForeign (Just av) (ICon fid t f@(ICForeign { })) (Just ([], es)))
                 | iGetType (last es0) == itFmt =
       do n0 <- newFFCallNo
          n1 <- newFFCallNo
          e0 <- splitFFA False [] [] (ACallForeign (Just av) (ICon fid t' (f {fcallNo = (Just n0)})) (Just ([], es0)))
          e1 <- splitFFA False [] [] (ACallForeign (Just av) (ICon fid t'' (f {fcallNo = (Just n1)})) (Just ([], es1)))
          return (joinActionsA [e0, e1])
       where (_ , rt) = itGetArrows (getInnerType t)
             at'      = map iGetType es0
             at''     = map iGetType es1
             t'       = foldr1 itFun (at' ++ [rt])
             t''      = foldr1 itFun (at'' ++ [rt])
splitFFA d es0 (e:rest) x@(ACallForeign _ (ICon _ _ (ICForeign { })) (Just ([], _))) =
       splitFFA d (es0 ++ [e]) rest x
splitFFA d es0 es1 a = onActionArgsM (splitFF d es0 es1) (splitFFA d es0 es1) a

-- At this point, all Fmt types in action-ff will be the only argument

-- we find those single argument action-ff calls, eliminate
-- all the primFmtConcats from the associated fmt expression,
-- and split the associated action-ff in the process
-- top down processing
-- (the single-argument action-ff is removeConcatA's; the arm on an
-- ActionValue-ff under a selector stays here as well: its value half,
-- under avValue_, is an expression and meets it too)
removeConcat :: IExpr PostElab -> F PostElab (IExpr PostElab)
removeConcat y@(IAps c@(ICon _ _ (ICSel {})) ts [x@(IAps (ICon fid t f@(ICForeign { })) [] [e])])
                         | iGetType e == itFmt && isAVFFWithFmts x =
       do action_list <- mapM mkFF listoflists
          return (process action_list)
       where (_ , rt)    = itGetArrows (getInnerType t)
             mkFF es     = do n <- newFFCallNo
                              return  (IAps c ts [(IAps (ICon fid t' (f {fcallNo = (Just n)})) [] es)])
                           where at' = map iGetType es
                                 t' = foldr1 itFun (at' ++ [rt])
             listoflists = getLists e
             process [_] = y
             process zs = joinActions zs
removeConcat w@(IAps x@(ICon fid t f@(ICForeign { })) ts es)
                         | isFFWithFmts w =
       do es' <- mapM removeConcat es
          return (IAps x ts es')
removeConcat y@(IAps x ts es) =
       do es' <- mapM removeConcat es
          return (IAps x ts es')
removeConcat x = return x

removeConcatA :: IAction -> F PostElab IAction
removeConcatA x@(ACallForeign Nothing (ICon fid t f@(ICForeign { })) (Just ([], [e])))
                         | iGetType e == itFmt && isActionFFWithFmtsT t =
       do action_list <- mapM mkFF listoflists
          return (joinActionsA action_list)
       where (_ , rt)    = itGetArrows (getInnerType t)
             mkFF es     = do n <- newFFCallNo
                              return  (ACallForeign Nothing (ICon fid t' (f {fcallNo = (Just n)})) (Just ([], es)))
                           where at' = map iGetType es
                                 t' = foldr1 itFun (at' ++ [rt])
             listoflists = getLists e
removeConcatA y@(ACallForeign (Just av) (ICon fid t f@(ICForeign { })) (Just ([], [e])))
                         | iGetType e == itFmt && isAVFFWithFmtsT t =
       do action_list <- mapM mkFF listoflists
          return (process action_list)
       where (_ , rt)    = itGetArrows (getInnerType t)
             mkFF es     = do n <- newFFCallNo
                              return  (ACallForeign (Just av) (ICon fid t' (f {fcallNo = (Just n)})) (Just ([], es)))
                           where at' = map iGetType es
                                 t' = foldr1 itFun (at' ++ [rt])
             listoflists = getLists e
             process [_] = y
             process zs = joinActionsA zs
removeConcatA a = onActionArgsM removeConcat removeConcatA a

getLists :: IExpr PostElab -> [[IExpr PostElab]]
getLists (IAps (ICon _ _ (ICPrim PrimFmtConcat)) _ [e0, e1]) =
       (getLists e0) ++ (getLists e1)
getLists x = [[x]]

-- #############################################################################
-- #
-- #############################################################################

-- the value expressions of the action halves of a split ActionValue
-- task (a join of the pieces, each under avAction_, some under the
-- conditions reduceFmt hoisted)
createValueExprs :: IAction -> [IExpr PostElab]
createValueExprs (AJoin e1 e2)                            = (createValueExprs e1 ++ createValueExprs e2)
createValueExprs x | allStrings x                         = [createStringExpr x]
createValueExprs x                                        = [createValueExpr x]

-- #############################################################################
-- #
-- #############################################################################

createValueExpr :: IAction -> IExpr PostElab
createValueExpr (ACallForeign (Just (AVSel (ICon c _ (ICSel {})) [ITAp b (ITNum s)])) f@(ICon _ _ (ICForeign {})) (Just (fts, fes)))
                | c == idAVAction_, b == itBit
                = x
                where x = (IAps (ICon idAVValue_ tt (ICSel {selNo = 0, numSel = 2 })) [ITAp itBit $ ITNum s] [IAps f fts fes])
                      v0 = head tmpVarIds
                      tt = ITForAll v0 IKStar (ITAp (ITAp (ITCon (idArrow noPosition) (IKFun IKStar (IKFun IKStar IKStar)) TIabstract) (ITAp (ITCon idActionValue_ (IKFun IKStar IKStar) (TIstruct SStruct [idAVValue_,idAVAction_])) (ITVar v0)))
                                                   (ITVar v0) )
createValueExpr (AIf _ cond e0 e1)
                = x
                where x = (IAps icIf [rt] [cond, e0', e1'])
                      e0' = createValueExpr e0
                      e1' = createValueExpr e1
                      rt  = iGetType e0'
createValueExpr x = internalError ("createValueExpr: " ++ ppReadable x)


createActionExpr :: IExpr PostElab -> IAction
createActionExpr (IAps (ICon c _ (ICSel {})) [ITAp b (ITNum s)] [IAps f@(ICon _ _ (ICForeign {})) fts fes])
                | c == idAVValue_, b == itBit
                = x
                where x = ACallForeign (Just (AVSel (ICon idAVAction_ tt (ICSel {selNo = 1, numSel = 2 })) [ITAp itBit $ ITNum s])) f (Just (fts, fes))
                      v0 = head tmpVarIds
                      tt = ITForAll v0 IKStar (ITAp (ITAp (ITCon (idArrow noPosition) (IKFun IKStar (IKFun IKStar IKStar)) TIabstract) (ITAp (ITCon idActionValue_ (IKFun IKStar IKStar) (TIstruct SStruct [idAVValue_,idAVAction_])) (ITVar v0)))
                                                   itAction )
createActionExpr (IAps (ICon i _ (ICPrim PrimIf)) ts [cond, e0, e1])
                = x
                where x = AIf SplitDefault cond e0' e1'
                      e0' = createActionExpr e0
                      e1' = createActionExpr e1
createActionExpr x = joinActionsA []


allStrings :: IAction -> Bool
allStrings (ACallForeign (Just (AVSel (ICon c _ (ICSel {})) [ITAp b (ITNum s)])) (ICon _ _ (ICForeign {})) (Just (_, [e])))
           | c == idAVAction_ && b == itBit && iGetType e == itString
           = True
allStrings (AIf _ _ e0 e1)
           = allStrings e0 && allStrings e1
allStrings _ = False

createStringExpr :: IAction -> IExpr PostElab
createStringExpr (ACallForeign (Just (AVSel (ICon c _ (ICSel {})) [ITAp b (ITNum s)])) (ICon _ _ (ICForeign {})) (Just (_, [e])))
                | c == idAVAction_, b == itBit
                = e
createStringExpr (AIf _ cond e0 e1)
                = x
                where x = (IAps icIf [rt] [cond, e0', e1'])
                      e0' = createStringExpr e0
                      e1' = createStringExpr e1
                      rt  = iGetType e0'
createStringExpr x = internalError ("createStringExpr: " ++ ppReadable x)


createRefsAndDefsAndActions :: IExpr PostElab -> ([IExpr PostElab], [IDef PostElab], [IAction])
createRefsAndDefsAndActions (IAps (ICon c _ (ICSel {})) _ [(IAps (ICon _ _ (ICForeign {})) _ es)]) | c == idAVValue_
                 = (es, [], [])
createRefsAndDefsAndActions  e@(ICon _ _ ICString {}) = ([e], [], [])
createRefsAndDefsAndActions e | iGetType e == itString = ([(iMkString "%0s"), e], [], [])
createRefsAndDefsAndActions e = ([(iMkString "%0s"), (ICon i t (ICValue {iValDef = e}))],
                                 [(IDef i t e [])],
                                 removeConditions (createActionExpr e))
                where i = enumId "_ff" noPosition (fromInteger n)
                      t = iGetType e
                      n = head (getFCallNos e)

getFCallNos :: IExpr PostElab -> [Integer]
getFCallNos (IAps (ICon _ _ (ICForeign {fcallNo = (Just n)})) _ _) = [n]
getFCallNos (IAps _ _ es) = concatMap getFCallNos es
getFCallNos _ = []

removeConditions :: IAction -> [IAction]
removeConditions (AIf _ _ e0 e1) =
                 (removeConditions e0 ++ removeConditions e1)
removeConditions (AJoin e1 e2) = (removeConditions e1 ++ removeConditions e2)
removeConditions x = [x]


-- #############################################################################
-- #
-- #############################################################################

reduceFmtA :: IAction -> F PostElab IAction
reduceFmtA e =
  do  e' <- reduceA True e
      return (removeA e')

-- if we're only interested in the value part of an ActionValue foreign function
-- (i.e. $swrite etc) then don't bother with converting the
-- args from Fmts .... set the value of "rm_args" to True
reduce :: Bool -> Bool -> IExpr PostElab -> F PostElab (IExpr PostElab)
reduce False   first expr@(IAps (ICon m _ _) _ _) | m == idAVValue_ =
     do e' <- reduce True first expr
        return e'
-- if this is the first time (and a foreign function call) eliminate any type
-- variables (should this have been done in IExpand?) and recurse down into the arguments
reduce rm_args True   (IAps (ICon fid ict f@(ICForeign { })) ts es)
    | (rt == itFmt) || (any (== itFmt) at) =
    do es' <- mapM (reduce rm_args True) es
       f'  <- reduce rm_args True (ICon fid ict' f)
       e'  <- reduce rm_args False (IAps f' [] es')
       return e'
    where (_, rt) = itGetArrows (getInnerType ict)
          at = map iGetType es
          ict' = itInst ict ts
-- if this is the first time (and not a foreign function) recurse down into the arguments
reduce rm_args True  (IAps f ts es) =
    do es' <- mapM (reduce rm_args True) es
       f'  <- reduce rm_args True f
       e' <- reduce rm_args False (IAps f' ts es')
       return e'
-- if this is a foreign function call and we're removing args
-- (for the value half of of an AV expression), eliminate the args.
reduce True    False (IAps (ICon fid ict f@(ICForeign { })) ts es)
    | any (== itFmt) at =
    return (IAps (ICon fid rt f) [] [])
    where (at, rt) = itGetArrows (getInnerType ict)
-- (moving "if" conditions outside of AVAction_ calls is reduceA's:
-- the avAction_ application is an action)
-- eliminate Fmt ifs when one half is a don't care
-- we are treating Fmt like Integer or String rather than Bit#(n)
reduce rm_args False (IAps (ICon _ _ (ICPrim PrimIf)) _ [cond, e0, (ICon _ it (ICUndet _))]) | it == itFmt = return e0
reduce rm_args False (IAps (ICon _ _ (ICPrim PrimIf)) _ [cond, (ICon _ it (ICUndet _)), e1]) | it == itFmt = return e1
-- move "if" expressions outside of Fmt concat operations
reduce rm_args False x@(IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc
              [(IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e0, e1]), e2]) =
    do e0' <- reduce rm_args False (IAps cc tc [e0,e2])
       e1' <- reduce rm_args False (IAps cc tc [e1,e2])
       e'  <- reduce rm_args False (IAps ci ti [cond, e0', e1'])
       return e'
reduce rm_args False x@(IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc
              [e2, (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e0, e1])]) =
    do e0' <- reduce rm_args False (IAps cc tc [e2,e0])
       e1' <- reduce rm_args False (IAps cc tc [e2,e1])
       e'  <- reduce rm_args False (IAps ci ti [cond, e0', e1'])
       return e'
-- reduce a concat of two fmt calls to a single fmt call
reduce rm_args False x@(IAps (ICon _ _ (ICPrim PrimFmtConcat)) _
              [(IAps (ICon fid t0 fic@(ICForeign { })) [] es0),
               (IAps (ICon _       t1 (ICForeign { })) [] es1)]) =
    do let (at0, dt) = itGetArrows t0
           (at1, _ ) = itGetArrows t1
           t = foldr1 itFun (at0 ++ at1 ++ [dt])
       return (IAps (ICon fid t fic) [] (es0 ++ es1))
-- move "if" expressions (of type Fmt) outside of foreign function calls
reduce rm_args False (IAps (ICon fid t f@(ICForeign { })) []
              ((IAps ici@(ICon _ _ (ICPrim PrimIf)) [it] [cond, e0, e1]):rest)) | it == itFmt =
    do n0    <- newFFCallNo
       n1    <- newFFCallNo
       n2    <- newFFCallNo
       e0'   <- reduce rm_args False (IAps (ICon fid t f {fcallNo = (Just n0)}) [] [e0])
       e1'   <- reduce rm_args False (IAps (ICon fid t f {fcallNo = (Just n1)}) [] [e1])
       rest' <- reduce rm_args False (IAps (ICon fid t f {fcallNo = (Just n2)}) [] rest)
       e0''  <- addArg e0' rest'
       e1''  <- addArg e1' rest'
       e0''' <- reduce rm_args False e0''
       e1''' <- reduce rm_args False e1''
       return (IAps ici [rt] [cond, e0''', e1'''])
    where (_  , rt) = itGetArrows t
reduce rm_args False (IAps icf@(ICon fid ft f@(ICForeign {})) [] (first:rest))
    | any isIfFmt rest =
    do n  <- newFFCallNo
       e' <- reduce rm_args False (IAps (ICon fid ft f {fcallNo = (Just n)}) [] rest)
       e'' <- addArg (IAps icf [] [first]) e'
       return e''
reduce _ _ x = return x

-- The same over an action (rm_args is False throughout: it is set only
-- under avValue_, in a value).
reduceA :: Bool -> IAction -> F PostElab IAction
-- if this is the first time (and a foreign function call) eliminate any type
-- variables (should this have been done in IExpand?) and recurse down into the arguments
reduceA True (ACallForeign Nothing (ICon fid ict f@(ICForeign { })) (Just (ts, es)))
    | (rt == itFmt) || (any (== itFmt) at) =
    do es' <- mapM (reduce False True) es
       e'  <- reduceA False (ACallForeign Nothing (ICon fid ict' f) (Just ([], es')))
       return e'
    where (_, rt) = itGetArrows (getInnerType ict)
          at = map iGetType es
          ict' = itInst ict ts
-- the action half of an ActionValue call: the call itself is a value
-- and is reduced as one; then "if" conditions move outside of the
-- avAction_ selector (so the type of the if is action)
reduceA True (ACallForeign (Just av) f (Just (ts, es))) =
    do inner' <- reduce False True (IAps f ts es)
       avHalf inner'
    where avHalf (IAps (ICon _ _ (ICPrim PrimIf)) [_] [cond, e0, e1]) =
              do e0' <- avHalf e0
                 e1' <- avHalf e1
                 return (AIf SplitDefault cond e0' e1')
          avHalf (IAps f' ts' es') = return (ACallForeign (Just av) f' (Just (ts', es')))
          avHalf f' = return (ACallForeign (Just av) f' Nothing)
-- if this is the first time (and not a foreign function) recurse down into the arguments
reduceA True a =
    do a' <- onActionArgsM (reduce False True) (reduceA True) a
       reduceA False a'
-- move "if" expressions (of type Fmt) outside of foreign function calls
reduceA False (ACallForeign Nothing (ICon fid t f@(ICForeign { }))
              (Just ([], (IAps (ICon _ _ (ICPrim PrimIf)) [it] [cond, e0, e1]):rest))) | it == itFmt =
    do n0    <- newFFCallNo
       n1    <- newFFCallNo
       n2    <- newFFCallNo
       e0'   <- reduceA False (ACallForeign Nothing (ICon fid t f {fcallNo = (Just n0)}) (Just ([], [e0])))
       e1'   <- reduceA False (ACallForeign Nothing (ICon fid t f {fcallNo = (Just n1)}) (Just ([], [e1])))
       rest' <- reduceA False (ACallForeign Nothing (ICon fid t f {fcallNo = (Just n2)}) (Just ([], rest)))
       e0''  <- addArgA e0' rest'
       e1''  <- addArgA e1' rest'
       e0''' <- reduceA False e0''
       e1''' <- reduceA False e1''
       return (AIf SplitDefault cond e0''' e1''')
reduceA False (ACallForeign Nothing icf@(ICon fid ft f@(ICForeign {})) (Just ([], (first:rest))))
    | any isIfFmt rest =
    do n  <- newFFCallNo
       e' <- reduceA False (ACallForeign Nothing (ICon fid ft f {fcallNo = (Just n)}) (Just ([], rest)))
       e'' <- addArgA (ACallForeign Nothing icf (Just ([], [first]))) e'
       return e''
reduceA _ x = return x

-- finally turn args of type fmt into "real" $display args
remove :: IExpr PostElab -> IExpr PostElab
remove (IAps (ICon fid t f@(ICForeign { })) [] es)
        | any (== itFmt) at = expr
       where (at , rt) = itGetArrows t
             es' = map remove es
             es'' = concatMap eliminateFormat es'
             at' = map iGetType es''
             t' = foldr1 itFun (at' ++ [rt])
             expr = remove (IAps (ICon fid t' f) [] es'')
remove (IAps x ts es) = (IAps x ts (map remove es))
remove x = x

removeA :: IAction -> IAction
removeA (ACallForeign Nothing (ICon fid t (ICForeign { })) (Just ([], [])))
        | rt == itAction = ANoActions
       where (_ , rt) = itGetArrows t
removeA (ACallForeign mav (ICon fid t f@(ICForeign { })) (Just ([], es)))
        | any (== itFmt) at = expr
       where (at , rt) = itGetArrows t
             es' = map remove es
             es'' = concatMap eliminateFormat es'
             at' = map iGetType es''
             t' = foldr1 itFun (at' ++ [rt])
             expr = removeA (ACallForeign mav (ICon fid t' f) (Just ([], es'')))
removeA a = onActionArgs remove removeA a

addArg :: IExpr PostElab -> IExpr PostElab -> F PostElab (IExpr PostElab)
addArg (IAps ici@(ICon _ t (ICPrim PrimIf)) ts [cond, e0, e1]) rest =
    do e0' <- addArg e0 rest
       e1' <- addArg e1 rest
       return (IAps ici ts [cond, e0', e1'])

addArg (IAps icf@(ICon _ _ (ICForeign {})) [] [first]) rest =
    do e' <- addArg2 first rest
       return e'
    where addArg2 first (IAps ici@(ICon _ t (ICPrim PrimIf)) ts [cond, e0, e1]) =
              do e0' <- addArg2 first e0
                 e1' <- addArg2 first e1
                 return (IAps ici ts [cond, e0', e1'])
          addArg2 first (IAps (ICon fid ft f@(ICForeign {})) [] es) =
              do n <- newFFCallNo
                 return (IAps (ICon fid ft f {fcallNo = (Just n)}) [] (first:es))
          addArg2 _ e' = internalError("addArg2: unexpected expr: " ++ ppReadable e')
addArg e _ = internalError ("addArg: " ++ ppReadable e)

addArgA :: IAction -> IAction -> F PostElab IAction
addArgA (AIf m cond e0 e1) rest =
    do e0' <- addArgA e0 rest
       e1' <- addArgA e1 rest
       return (AIf m cond e0' e1')

addArgA (ACallForeign Nothing (ICon _ _ (ICForeign {})) (Just ([], [first]))) rest =
    do e' <- addArg2 first rest
       return e'
    where addArg2 first (AIf m cond e0 e1) =
              do e0' <- addArg2 first e0
                 e1' <- addArg2 first e1
                 return (AIf m cond e0' e1')
          addArg2 first (ACallForeign Nothing (ICon fid ft f@(ICForeign {})) (Just ([], es))) =
              do n <- newFFCallNo
                 return (ACallForeign Nothing (ICon fid ft f {fcallNo = (Just n)}) (Just ([], first:es)))
          addArg2 _ e' = internalError("addArg2: unexpected expr: " ++ ppReadable e')
addArgA e _ = internalError ("addArg: " ++ ppReadable e)

isIfFmt :: IExpr PostElab -> Bool
isIfFmt (IAps (ICon _ _ (ICPrim PrimIf)) [it] _) | it == itFmt = True
isIfFmt _ = False

eliminateFormat :: IExpr PostElab -> [IExpr PostElab]
eliminateFormat (IAps (ICon _ t ICForeign { }) [] es) | rt == itFmt = es
    where (_, rt) = itGetArrows t
-- also remove $format with no arguments
-- XXX perhaps the caller shouldn't have created this expression?
eliminateFormat (ICon _ t ICForeign { }) | rt == itFmt = []
    where (_, rt) = itGetArrows t
-- and remove don't-care value
-- XXX again, should this be fixed earlier than here?
-- XXX should we warn or error about a don't-care Fmt?
eliminateFormat (ICon _ t ICUndet { }) | t == itFmt = []
eliminateFormat x = [x]


-- a foreign function's type: an action, or an ActionValue, with a Fmt argument
isActionFFWithFmtsT :: IType -> Bool
isActionFFWithFmtsT t =
   (isitAction rt) &&  (any (== itFmt) at)
   where (at , rt) = itGetArrows (getInnerType t)

isAVFFWithFmtsT :: IType -> Bool
isAVFFWithFmtsT t =
   (isitActionValue_ rt) &&  (any (== itFmt) at)
   where (at , rt) = itGetArrows (getInnerType t)

isActionFFWithFmts :: IExpr PostElab -> Bool
isActionFFWithFmts (IAps (ICon _ t (ICForeign { })) _ _) = isActionFFWithFmtsT t
isActionFFWithFmts _                                          = False

isAVFFWithFmts :: IExpr PostElab -> Bool
isAVFFWithFmts (IAps (ICon _ t (ICForeign { })) _ _) = isAVFFWithFmtsT t
isAVFFWithFmts _                                              = False

isFFWithFmts :: IExpr PostElab -> Bool
isFFWithFmts e = isActionFFWithFmts e || isAVFFWithFmts e

-- A method's value and rules.  An ActionValue method's value and its
-- rule's body were the two fields of one ActionValue_ struct until
-- pDef split them; they are processed as that struct was, every phase
-- over the value, then over the bodies, so the cookies are allocated
-- in the same order.
ssplitFmt_ifc :: [IEFace PostElab] -> F PostElab [IEFace PostElab]
ssplitFmt_ifc ifc_list
    = do let updateIfc (IEFace i xs (Just (e,t)) Nothing wp fi) =
                 do e' <- ssplitFmt e
                    return (IEFace i xs (Just (e',t)) Nothing wp fi)
             updateIfc (IEFace i xs Nothing (Just rules) wp fi) =
                 do rules' <- irulesMapM ssplitFmt ssplitFmtA rules
                    return (IEFace i xs Nothing (Just rules') wp fi)
             updateIfc (IEFace i xs (Just (e,t)) (Just (IRules sps rs)) wp fi) =
                 do ps' <- mapM (ssplitFmt . irule_pred) rs
                    (e', as') <- ssplitFmtPair e (map irule_body rs)
                    let rs' = zipWith3 (\ r p' a' -> r { irule_pred = p', irule_body = a' }) rs ps' as'
                    return (IEFace i xs (Just (e',t)) (Just (IRules sps rs')) wp fi)
             updateIfc ief = return (internalError("ssplitFmt_ifc: expression not found: " ++ ppReadable ief))
         ifc_list' <- mapM updateIfc ifc_list
         return ifc_list'

-- each phase over the value, then over the actions
ssplitFmtPair :: IExpr PostElab -> [IAction] -> F PostElab (IExpr PostElab, [IAction])
ssplitFmtPair e as =
    do e0 <- fsplitFmt e
       as0 <- mapM fsplitFmtA as
       let e1 = replaceDisplays e0
           as1 = map replaceDisplaysA as0
       e2 <- unNestFmts [] [] e1
       as2 <- mapM unNestFmtsA as1
       let e3 = combineFmts e2
           as3 = map combineFmtsA as2
           e4 = promoteConcat False e3
           as4 = map promoteConcatA as3
       e5 <- splitFF True [] [] e4
       as5 <- mapM (splitFFA True [] []) as4
       e6 <- removeConcat e5
       as6 <- mapM removeConcatA as5
       return (e6, as6)

getInnerType :: IType -> IType
getInnerType (ITForAll id ik t) = (getInnerType t)
getInnerType t = t

-- #############################################################################
-- # Code to inline then eliminate Fmts from ISyntax
-- #############################################################################

iInlineFmts :: IModule PostElab -> IModule PostElab
iInlineFmts imod =
    let tst _ = True
        imod'  = iInlineFmtsPhase1 imod
        imod'' = iInlineFmtsT tst imod'
    in imod''

iInlineFmtsPhase1 :: IModule PostElab -> IModule PostElab
iInlineFmtsPhase1 imod =
    let tst (IAps (ICon _ _ (ICPrim PrimFmtConcat)) _ _) = True
        tst (IAps (ICon _ _ (ICForeign {})) _ _) = True
        tst e = False
        imod' = (iInlineFmtsT tst imod)
        (imod'', change) = (modPromoteSome imod')
    in if (change) then iInlineFmtsPhase1 imod'' else imod''

iInlineFmtsT :: ((IExpr PostElab) -> Bool) -> IModule PostElab -> IModule PostElab
iInlineFmtsT tst imod@(IModule { imod_local_defs = ds,
                                 imod_rules      = rs,
                                 imod_interface  = ifc}) =
    let smap = M.fromList [ (i, iSubstWhen tst smap dmap e) | IDef i t e _ <- ds, (t == itFmt) ] -- inline any def of type Fmt
        ds' = iDefsMap (iSubstWhen tst smap dmap) ds
        dmap = M.fromList [ (i, e) | IDef i t e _ <- ds' ]
        ifc' = map (iSubstIfc smap dmap) ifc
        rs' = irulesMap (iSubstWhen tst smap dmap) rs
        state_vars' = [ (name, sv { isv_iargs = es' })
                      | (name, sv@(IStateVar { isv_iargs = es }))
                            <- imod_state_insts imod,
                        let es' = map (iSubstWhen tst smap dmap) es ]
        ds'' = [ IDef id t e p | IDef id t e p <- ds', (t /= itFmt || (not (tst e))) ] -- remove any def of type Fmt

    in imod { imod_local_defs  = ds'',
              imod_rules       = rs',
              imod_interface   = ifc',
              imod_state_insts = state_vars' }


-- #############################################################################
-- #
-- #############################################################################



modPromoteSome :: IModule PostElab -> (IModule PostElab, Bool)
modPromoteSome imod@(IModule { imod_local_defs = ds,
                               imod_rules      = rs,
                               imod_interface  = ifc}) =
    let getFirst (a, b)   = a
        getSecond (a, b)  = b
        pDef (IDef id t e p) = ((IDef id t e' p), change)
            where (e', change) = promoteSome e
        pairs = map pDef ds
        ds'  = map getFirst pairs
        change [] = False
        change ps = (foldr1 (||) (map getSecond ps))
        ifc' = ifc
        rs'  = rs
        state_vars' = imod_state_insts imod
    in (imod { imod_local_defs  = ds',
               imod_rules       = rs',
               imod_interface   = ifc',
               imod_state_insts = state_vars' },
        (change pairs))

promoteSome :: IExpr PostElab -> (IExpr PostElab, Bool)
promoteSome e |  t /= itFmt = (e, False)
              where t = iGetType e

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e00, e01]),
                                                            (IAps    (ICon _ _ (ICPrim PrimFmtConcat)) _  [e10, e11])])
              | (pMatch e00 e10) = ((IAps cc tc [e00, (IAps ci ti [cond, e01, e11])]), True)

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e00, e01]),
                                                            (IAps    (ICon _ _ (ICPrim PrimFmtConcat)) _  [e10, e11])])
              | (pMatch e01 e11) = ((IAps cc tc [(IAps ci ti [cond, e00, e10]), e01]), True)

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e00, e01]),
                                                           e10])
             | (pMatch e00 e10) = promoteSome (IAps ci ti [cond, (IAps cc tc [e00, e01     ]),
                                                             (IAps cc tc [e10, emptyFmt])])

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e00, e01]),
                                                           e10])
             | (pMatch e01 e10) = promoteSome (IAps ci ti [cond, (IAps cc tc [e00,      e01]),
                                                             (IAps cc tc [emptyFmt, e10])])

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e00,
                                                            (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e10, e11])])
             | (pMatch e00 e10) = promoteSome (IAps ci ti [cond, (IAps cc tc [e00, emptyFmt]),
                                                             (IAps cc tc [e10, e11     ])])

promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e00,
                                                            (IAps cc@(ICon _ _ (ICPrim PrimFmtConcat)) tc [e10, e11])])
             | (pMatch e00 e11) = promoteSome (IAps ci ti [cond, (IAps cc tc [emptyFmt, e00]),
                                                             (IAps cc tc [e10,      e11])])


promoteSome (IAps ci@(ICon _ _ (ICPrim PrimIf)) ti [cond, e0, e1])
             | (pMatch e0 e1) = (e0, True)

promoteSome (IAps x ts es) =
              let pairs = map promoteSome es
                  getFirst  (a, b) = a
                  getSecond (a, b) = b
                  es' = map getFirst pairs
                  change [] = False
                  change ps = (foldr1 (||) (map getSecond ps))
              in ((IAps x ts es'), (change pairs))
promoteSome x = (x, False)

pMatch :: IExpr PostElab -> IExpr PostElab -> Bool
pMatch e0 e1 = e0 == e1
-- pMatch (IAps (ICon fid0 (ICForeign {ictForeign = t0})) [] [e0]) (IAps (ICon fid1 (ICForeign {ictForeign = t1})) [] [e1])
--        | fid0 == fid1 && pMatch e0 e1 = True
-- pMatch e0 e1 = e0 == e1
