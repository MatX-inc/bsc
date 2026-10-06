{-# OPTIONS_GHC -Werror=inaccessible-code -Werror=overlapping-patterns #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}
module ISplitIf (iSplitIf) where

import ISyntax
import ErrorUtil(internalError)
import PPrint
import PreIds(idAVValue_)
import qualified Flags(Flags, expandIf)
import Position
import ISyntaxUtil
import Id(Id, mkSplitId)
import Pragma(SPIdSplitMap, splitSchedPragmaIds)
import ITransform(iTransBoolExpr)
import PreStrings(fs_T, fs_F)
import FStringCompat(FString, concatFString, getFString, mkFString)
import Data.List(genericLength)
-- import Debug.Trace(trace)

-- --------------------------

-- Old comment from Ken:
--   Input there should be no dirty wrappers: check_dirty_if_wrappers
--   then push is called
--   Then, there should be no deepsplit wrapper
--   (Depending on exactly how I do it in push: there might be a point where
--   there are no shallow nosplit wrappers; any IF that is not wrapped is
--   assumed shallow nosplit)
--   (Otherwise, every If should be annotated)
--   After splitif is called, then there should be no wrappers.

iSplitIf :: Flags.Flags -> IModule PostElab -> IModule PostElab
iSplitIf flags imod@(IModule { imod_rules = rules,
                               imod_interface = methods })
  = --trace ("iSplitIf happens!") $
    let
       (smaps, new_methods) = unzip $ map (iSplitIface flags) methods
       -- provide the method split map, in case any rule pragmas mention methods
       (_, new_rules) = do_iExpandIfRules flags (concat smaps) rules
    in --trace ("============m1\n"++(show new_methods)) $
       imod { imod_rules = new_rules ,
              imod_interface = new_methods }

-- --------------------------

-- expand `if' in rules to multiple rules
do_iExpandIfRules :: Flags.Flags -> SPIdSplitMap -> IRules PostElab ->
                     (SPIdSplitMap, IRules PostElab)
do_iExpandIfRules flags method_split_map (IRules sps rs)
  = --trace ("ie before : " ++ (ppReadable rs)) $
    --trace ("ie kenta  : " ++ (ppReadable m1)) $
    (idsplitmap, IRules sps' rs')
  where xs = unzip m1
        m1 = map (iExpandIfRule flags) rs
        idsplitmap = concat (fst xs)
        rs' = concat (snd xs)
        sps' = splitSchedPragmaIds (method_split_map ++ idsplitmap) sps

data Branch_taken a =
       -- if expr:
       -- the predicate, the branch taken (indicated by True or False)
       -- (the branch value will be used to name the split rule)
       BranchIf (IExpr a) Bool
       -- array selection, in bounds:
       -- index expr, element index taken, bit-width of index, pos of the op
       | BranchArrSel (IExpr a) Integer Integer Position
       -- array selection, out of bounds:
       -- index expr, number of elements, bit-width of index, pos of the op
       | BranchArrSelOutOfBounds (IExpr a) Integer Integer Position
       -- case expr, explicit arm:
       -- case index, arm expr that matches, bit-width of index, and
       -- number of the arm (for rule naming purposes)
       | BranchCase (IExpr a) (IExpr a) Integer Integer
       -- case expr, default arm:
       -- case index, all arm exprs that didn't match, bit-width of index
       | BranchCaseDefault (IExpr a) [IExpr a] Integer
    deriving (Eq, Show)

type Path_through_actions a = ([Branch_taken a],[IAction])

run :: IAction -> [Path_through_actions PostElab]
-- this function does the work of splitting an if into two actions

-- this function uses the list as the nondeterminism monad.  I've heard
-- that there exist more efficient nondeterminism monads, which should be
-- looked into if performance is a problem.

--run l | flattensToNothing l = return ([], [])
run l
  = case l of
        -- a conditional marked for splitting: one branch set per arm
        -- when the condition can be lifted to the predicate, else the
        -- bare conditional
        (AIf SplitIf cond t_action f_action) ->
               if (canLiftCond cond) then
                 map (prepend_branch (BranchIf cond True)) (run t_action) ++
                 map (prepend_branch (BranchIf cond False)) (run f_action)
               else run (AIf SplitDefault cond t_action f_action)

        (AArrSel SplitIf i_sel i_arr idx_sz es_elems e_idx) ->
             if (canLiftCond e_idx) then
                  let sel_pos = getPosition i_sel
                      max_idx = (2^idx_sz) - 1
                      num_es = genericLength es_elems
                      -- funcs to make the branches
                      doElem e_elem n =
                          map (prepend_branch
                                   (BranchArrSel e_idx n idx_sz sel_pos))
                              (run e_elem)
                      doOutOfBounds =
                          map (prepend_branch
                                   (BranchArrSelOutOfBounds
                                        e_idx num_es idx_sz sel_pos))
                              (run ANoActions)
                      -- # of arms is the min of the elems and the max index
                      e_branches =
                          concat (zipWith doElem es_elems [0..max_idx])
                      dflt_branch =
                          if (max_idx > (num_es-1))
                          then doOutOfBounds
                          else []
                  in  e_branches ++ dflt_branch
             else run (AArrSel SplitDefault i_sel i_arr idx_sz es_elems e_idx)

        (AJoin e1 e2)
          -> do
                (a, x) <- run e1
                (b, y) <- run e2
                return (a ++ b, x ++ y)

        -- push removed every other annotation
        (AIf NoSplitIf _ _ _)
          -> internalError ("ISplitIf.run wrong kind of splitting.\n"
                            ++ (ppReadable l))
        (AArrSel NoSplitIf _ _ _ _ _)
          -> internalError ("ISplitIf.run wrong kind of splitting.\n"
                            ++ (ppReadable l))
        (ADeep _ _)
          -> internalError ("ISplitIf.run wrong kind of splitting.\n"
                            ++ (ppReadable l))

        -- an unsplit conditional: all combinations of the paths through
        -- its arms, the conditional rebuilt around each combination's
        -- actions
        (AIf SplitDefault cond t_action f_action)
          -> do (bt, xt) <- run t_action
                (bf, xf) <- run f_action
                return (bt ++ bf,
                        [AIf SplitDefault cond (joinActionsA xt) (joinActionsA xf)])

        (AArrSel SplitDefault i_sel i_arr idx_sz es_elems e_idx)
          -> do y <- (mapM run es_elems)
                return (concatMap fst y,
                        [AArrSel SplitDefault i_sel i_arr idx_sz
                                 (map (joinActionsA . snd) y) e_idx])

        -- a call, no actions, the undetermined action: one path
        _ -> return ([],[l])

push :: Bool -> IAction -> IAction
-- this function pushes SplitDeep and NosplitDeep down the tree
-- the "state" of whether we are in splitting mode or not is stored
-- and recursed down the tree in the argument do_split.  The initial
-- state of do_split is probably Flags.expandIf
push do_split e
  = let continue :: IAction -> IAction
        -- keeps pushing split or nosplit depending on the argument
        continue x = push do_split x
     in case e of
        (AJoin e1 e2)
          -> AJoin (continue e1) (continue e2)

        -- a split annotation is kept, and we continue in the arms
        (AIf SplitIf cond t_act f_act)
          -> AIf SplitIf cond (continue t_act) (continue f_act)
        (AArrSel SplitIf i_sel i_arr idx_sz es_elems e_idx)
          -> AArrSel SplitIf i_sel i_arr idx_sz (map continue es_elems) e_idx

        -- nosplit annotations are removed, and we continue in the arms
        (AIf NoSplitIf cond t_act f_act)
          -> AIf SplitDefault cond (continue t_act) (continue f_act)
        (AArrSel NoSplitIf i_sel i_arr idx_sz es_elems e_idx)
          -> AArrSel SplitDefault i_sel i_arr idx_sz (map continue es_elems) e_idx

        (ADeep True e1)
          -> push True e1
        (ADeep False e1)
          -> push False e1

        -- a bare conditional: do what do_split says
        (AIf SplitDefault cond t_act f_act)
          -> if_annotate do_split
                 (AIf SplitDefault cond (continue t_act) (continue f_act))
        (AArrSel SplitDefault i_sel i_arr idx_sz es_elems e_idx)
          -> if_annotate do_split
                 (AArrSel SplitDefault i_sel i_arr idx_sz (map continue es_elems) e_idx)

        -- a call, no actions, the undetermined action (an expression
        -- holds no conditional action, so there is nothing to push
        -- into a call's arguments)
        _ -> e


if_annotate :: Bool -> IAction -> IAction
if_annotate True (AIf _ cond t_act f_act) = AIf SplitIf cond t_act f_act
if_annotate True (AArrSel _ i_sel i_arr idx_sz es_elems e_idx) =
    AArrSel SplitIf i_sel i_arr idx_sz es_elems e_idx
if_annotate _ a = a

prepend_branch :: Branch_taken PostElab ->
                  Path_through_actions PostElab -> Path_through_actions PostElab
prepend_branch br (brs, action) = ((br:brs), action)

-- XXX are these names OK? make then PreStrings?
make_branch_name :: Branch_taken PostElab -> FString
make_branch_name (BranchIf _ True) = fs_T
make_branch_name (BranchIf _ False) = fs_F
make_branch_name (BranchArrSel _ n _ _) = mkFString ("_E" ++ show n)
make_branch_name (BranchArrSelOutOfBounds { }) = mkFString ("_OOB")
make_branch_name (BranchCase _ _ n _) = mkFString ("_A" ++ show n)
make_branch_name (BranchCaseDefault { }) = mkFString ("_DFL")

make_branch_cond :: Branch_taken PostElab -> IExpr PostElab
make_branch_cond (BranchIf c True) = c
make_branch_cond (BranchIf c False) = ieNot c
make_branch_cond (BranchArrSel e_idx n sz_idx pos_sel) =
    let ty_idx = itBitN sz_idx
        n_lit = iMkLitAt pos_sel ty_idx n
    in  iePrimEQ (ITNum sz_idx) e_idx n_lit
make_branch_cond (BranchArrSelOutOfBounds e_idx num_elems sz_idx pos_sel) =
    -- XXX construct "e_idx >= num_elems" instead?
    let ty_idx = itBitN sz_idx
        mkNEq n = let n_lit = iMkLitAt pos_sel ty_idx n
                  in  ieNot $ iePrimEQ (ITNum sz_idx) e_idx n_lit
    in  foldl ieAnd iTrue (map mkNEq [0..num_elems-1])
make_branch_cond (BranchCase e_idx e_arm sz_idx _) =
    iePrimEQ (ITNum sz_idx) e_idx e_arm
make_branch_cond (BranchCaseDefault e_idx es_arms sz_idx) =
    let mkNEq e_arm = ieNot $ iePrimEQ (ITNum sz_idx) e_idx e_arm
    in  foldl ieAnd iTrue (map mkNEq es_arms)

iExpandIfRule :: Flags.Flags -> IRule PostElab ->
                 (SPIdSplitMap, [IRule PostElab])
iExpandIfRule flags
    r@(IRule { irule_name = i
             , irule_description = description
             , irule_pred = predicate
             , irule_body = action
             , irule_original = orig
             })
  = let
        paths :: [Path_through_actions PostElab]
        paths = run (push (Flags.expandIf flags) action)

        splitorig :: Maybe Id
        splitorig = maybe (Just i) Just orig

        mkRule :: Path_through_actions PostElab -> IRule PostElab
        mkRule (branches, action_list)
          = let
                fs_suffix :: FString
                fs_suffix = concatFString (map make_branch_name branches)

                -- potential name collision here XXX
                new_name :: Id
                new_name = mkSplitId i fs_suffix

                new_description :: String
                new_description = description ++ (getFString fs_suffix)

                terms :: [IExpr PostElab]
                terms = map make_branch_cond branches

                -- andOpt :: IExpr a -> IExpr a -> IExpr a
                -- andOpt x y = iTransBoolExpr flags (ieAnd x y)
                -- there is no obvious reason to choose
                -- foldl over foldr here
                new_predicate :: IExpr PostElab
                new_predicate = iTransBoolExpr flags (foldr ieAndOpt predicate terms)

                new_action :: IAction
                new_action = joinActionsA action_list
             in
--trace ("mkRule " ++ new_description ++ (ppReadable branches) ++ " = " ++
--(ppReadable new_predicate) ++ " : " ++ (ppReadable action_list)) $
                r { irule_name = new_name
                  , irule_description = new_description
                  , irule_pred = new_predicate
                  , irule_body = new_action
                  , irule_original = splitorig
                  }

        mkSingleRule (_branches, action_list)
            = r { irule_body = joinActionsA action_list }
        new_rules :: [IRule PostElab]
        new_rules = case paths of
             [s] -> [mkSingleRule s]
             _   -> map mkRule paths

        sched = case paths of
             [_] -> []
             _   -> [(i,map getIRuleId new_rules)]

     in (sched , new_rules)


-- --------------------------

-- methods

-- An Action or ActionValue method arrives with its action as its one
-- rule (pDef's rebuild makes it, IExpand.methodBody) and that rule is
-- split like any other; a method with a value only (a value method, a
-- ready signal, a clock, a reset, an inout) is unchanged.
iSplitIface :: Flags.Flags -> IEFace PostElab -> (SPIdSplitMap, IEFace PostElab)
iSplitIface flags (IEFace i xargs me (Just irules) wp fi)
    = -- Don't call "optRules", since it only serves to remove rules!
      -- Methods should not be remove!  The one other function is to
      -- warn about never-ready methods, but AAddScheduleDefs does that
      -- for us, later.
      -- irules_opt <- optRules (iLiftThenExpandIfRules flags irules)
      let (smap, irules_opt) = do_iExpandIfRules flags [] irules
      in  (smap, IEFace i xargs me (Just irules_opt) wp fi)
iSplitIface _ ieface@(IEFace _ _ (Just _) Nothing _ _) = ([], ieface)
iSplitIface _ ieface = internalError ("iSplitIface: a method with neither value nor rules: " ++ ppReadable ieface)


-- --------------------------

-- Check whether a condition can be lifted to a predicate.
-- If the condition contains a module argument or the result of
-- an ActionValue method call, then it cannot be lifted.
--
canLiftCond :: IExpr PostElab -> Bool
-- conditions that can't be lifted
canLiftCond (IAps (ICon i _ (ICSel {})) _ _) | (i == idAVValue_) = False
canLiftCond (ICon _ _ (ICMethArg {})) = False
-- follow references
canLiftCond (ICon _ _ (ICValue { iValDef = e })) = canLiftCond e
-- recurse
canLiftCond (IAps f _ as) = canLiftCond f && all canLiftCond as
-- any other terminal is OK
canLiftCond (ICon {}) = True
-- all other expressions are unexpected after IExpand

-- --------------------------

