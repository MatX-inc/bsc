module IForeignResults(checkForeignResults) where

-- The uses of the results of foreign function calls that return
-- ActionValue_ (import "BDPI", Classic foreign functions, and the system
-- tasks with a value such as $time and $fgetc), checked on the module that
-- IExpand makes, before any other pass.
--
-- Such a call becomes a system task call in Verilog that writes its result
-- into a register; AConv pairs the value, avValue_ (F#k ...), with the
-- action, avAction_ (F#k ...), by the call's cookie k.  The result exists
-- only in a cycle in which the action that makes the call runs, so a use
-- of the value where that action does not run reads a stale register.
-- IExpand gives each execution of a call its own cookie, and an execution
-- that is not made (read by __value or impCondOf without running its
-- action) has a value and no action (see primMarkForeignCall in IExpand).
--
-- A result is used where its value is in the fan-in of state: the
-- arguments of an action (a method of a submodule, a system task, another
-- foreign call), a condition in an action tree, the condition of a rule,
-- the output or ready signal of a method, or an argument, clock or reset
-- of an instantiation or of the module.  The evaluator puts in the module
-- only what these reach, so a result that reaches no state is not in the
-- module.  A use is in the scope of the call when it is in the same rule
-- or method as the action that makes the call, under at least the
-- conditions of that action.
--
-- Conditions are compared by their structure (see condKey), so that the
-- result does not depend on which equal expressions the evaluator or a
-- later pass happens to share: a def is the same as its body, and two
-- equal expressions are the same condition, wherever they are.  There is
-- no reasoning about values: a condition that is equivalent to another
-- but built differently (c against not c with the arms swapped) is a
-- different condition.

import Control.Monad(zipWithM)
import Control.Monad.State(State, evalState, gets, modify)
import qualified Data.Array as Array
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List(minimumBy)
import Data.Ord(comparing)

import Error(internalError, EMsg, ErrMsg(..), ForeignCallMade(..))
import Id
import Position
import PPrint(ppReadable)
import PreIds(idAVValue_, idAVAction_, idActionValue_)
import Prim
import ISyntax
import IntLit
import Util(quote)
import IExpandUtils(HExpr, HeapData, HeapCell(..), PExpr(..),
                    unsafeDerefHeap, CallInfo(..), CallKind(..))

-- ---------------

-- The part of the module that a use or a call belongs to: the body or the
-- condition of a rule, a method or its ready signal, an instantiation, or
-- the module's own clocks and resets.  Calls are made only in the bodies
-- of rules and in the actions of methods.
data Owner = ORuleBody Id | ORuleCond Id | OMethod Id | OReady Id
           | OInst Id | OModule
    deriving (Eq, Ord)

-- A condition on the path to an expression, by the key of the expression
-- that chooses (see condKey): the condition of an if and the arm taken,
-- the index of a dynamic selection and the element, or the index of a case
-- and the value of the arm (Nothing for the default).
data Cond = CIf CKey Bool | CSel CKey Integer | CCase CKey (Maybe CKey)
    deriving (Eq, Ord)

type Path = S.Set Cond

-- For each cookie of a result that an expression uses, the conditions
-- common to every path from the expression to the result.
type Uses = M.Map Integer Path

-- What the walk of the module finds: the action of a call (by cookie), on
-- its path, or a use of the results in an expression, on its path,
-- leaving out the cookie of the call whose arguments the expression is (a
-- call cannot use its own result).
data Item = ICall Integer Owner Path
          | IUse Owner Path (Maybe Integer) HExpr

-- ---------------
-- The structure of conditions

-- Equal keys stand for equal structures: an expression unfolded through
-- its defs, compared by cmpC and the Ids of its constructors (Id equality
-- ignores positions, and the check runs after removeInlinedPositions).
-- Each occurrence of an undetermined value has a key of its own, because
-- later passes can choose a different value for each; a def is one
-- occurrence, however many places refer to it.
type CKey = Int

-- One node of a structure, with the keys of its parts (never an ICValue,
-- which stands for its def's body, nor an ICUndet).
data CNode = CNAp CKey [IType] [CKey]
           | CNCon Id (IConInfo HeapData)
    deriving (Eq, Ord)

-- The state of the check, local to one call of checkForeignResults:
-- the key of each def's body and the results that each def uses, both
-- computed once per def (the defs are acyclic), and the table that
-- interns nodes, with the next free key.
data KeyState = KeyState {
        ks_def_keys :: M.Map Id CKey,
        ks_def_uses :: M.Map Id Uses,
        ks_nodes :: M.Map CNode CKey,
        ks_next :: !CKey
    }

type KM = State KeyState

-- The key of a condition.  The module has no heap references (IExpand
-- has replaced every one by a def or by its value), and no lambdas or
-- variables in the expressions that choose.
condKey :: HExpr -> KM CKey
condKey (ICon i (ICValue { iValDef = d })) = do
    m_k <- gets (M.lookup i . ks_def_keys)
    case m_k of
      Just k -> return k
      Nothing -> do
        k <- condKey d
        modify (\ s -> s { ks_def_keys = M.insert i k (ks_def_keys s) })
        return k
condKey (ICon _ (ICUndet { })) = newKey
condKey e@(ICon _ (ICLazyArray { })) =
    internalError ("IForeignResults.condKey: " ++ ppReadable e)
condKey (ICon i ic) = intern (CNCon i ic)
condKey (IAps f ts es) = do
    k_f <- condKey f
    k_es <- mapM condKey es
    intern (CNAp k_f ts k_es)
condKey e = internalError ("IForeignResults.condKey: " ++ ppReadable e)

intern :: CNode -> KM CKey
intern n = do
    m_k <- gets (M.lookup n . ks_nodes)
    case m_k of
      Just k -> return k
      Nothing -> do
        k <- newKey
        modify (\ s -> s { ks_nodes = M.insert n k (ks_nodes s) })
        return k

newKey :: KM CKey
newKey = do
    k <- gets ks_next
    modify (\ s -> s { ks_next = k + 1 })
    return k

-- ---------------

-- The diagnostics for the results of the executions in infos (by cookie,
-- see primMarkForeignCall in IExpand) that the module uses outside the
-- scope of their calls.  meth_poss gives the definition of each method.
checkForeignResults :: M.Map Integer CallInfo -> M.Map Id Position ->
                       IModule HeapData -> [EMsg]
checkForeignResults infos meth_poss imod
    | M.null infos = []
    | otherwise = foreignResultMsgs meth_poss imod (evalState check ks0)
  where
    ks0 = KeyState { ks_def_keys = M.empty, ks_def_uses = M.empty,
                     ks_nodes = M.empty, ks_next = 0 }
    check = do
        items <- moduleItems imod
        let -- (in the order of the walk)
            calls = M.fromListWith (flip (++))
                        [ (k, [(o, p)]) | ICall k o p <- items ]
            inScope o p k c =
                any (\ (o', pk) -> o' == o && pk `S.isSubsetOf` S.union p c)
                    (M.findWithDefault [] k calls)
            bad o p m_self uses =
                [ (o, info, callMade o k_calls (ci_kind info))
                | (k, c) <- M.toList uses,
                  Just k /= m_self,
                  not (inScope o p k c),
                  Just info <- [M.lookup k infos],
                  let k_calls = M.findWithDefault [] k calls ]
            use (IUse o p m_self e) = bad o p m_self <$> exprUses infos e
            use (ICall { }) = return []
        concat <$> mapM use items

-- ---------------
-- The walk of the module

moduleItems :: IModule HeapData -> KM [Item]
moduleItems imod = do
    let IRules _ rs = imod_rules imod
        rule r = do
            let i = irule_name r
            body <- acts (ORuleBody i) S.empty (irule_body r)
            return ((IUse (ORuleCond i) S.empty Nothing (irule_pred r) :) . body)
        method f
            | isRdyId (ief_name f) =
                return (maybe id (\ (e, _) -> (IUse (OReady (ief_name f)) S.empty Nothing e :))
                              (ief_value f))
            | otherwise = do
                let o = OMethod (ief_name f)
                value <- maybe (return id) (\ (e, _) -> methodValue o S.empty e)
                               (ief_value f)
                -- (empty before iSplitIface, which moves the actions of
                -- ief_value here)
                body <- case ief_body f of
                          Just (IRules _ mrs) ->
                              composeAll <$>
                                mapM (\ r -> ((IUse o S.empty Nothing (irule_pred r) :) .) <$>
                                               acts o S.empty (irule_body r))
                                     mrs
                          Nothing -> return id
                return (value . body)
        inst (i, sv) =
            [ IUse (OInst i) S.empty Nothing e
            | e <- isv_iargs sv ++
                   concatMap (clockWires . snd) (isv_clocks sv) ++
                   concatMap (resetWires . snd) (isv_resets sv) ]
        mod_items = [ IUse OModule S.empty Nothing e
                    | e <- concatMap clockWires (concatMap snd (imod_clock_domains imod)) ++
                           concatMap resetWires (imod_resets imod) ]
    rule_items <- mapM rule rs
    method_items <- mapM method (imod_interface imod)
    return (composeAll rule_items
              (composeAll method_items
                 (concatMap inst (imod_state_insts imod) ++ mod_items)))

composeAll :: [a -> a] -> a -> a
composeAll = foldr (.) id

clockWires :: IClock HeapData -> [HExpr]
clockWires c = [ic_wires c]

resetWires :: IReset HeapData -> [HExpr]
resetWires r = ir_wire r : clockWires (ir_clock r)

-- The value of a method: an Action or ActionValue method is its output
-- and its action tree (before iSplitIface), a value method its output.
methodValue :: Owner -> Path -> HExpr -> KM ([Item] -> [Item])
methodValue o p (IAps (ICon c (ICTuple { })) _ [v, a])
    | c == idActionValue_ = ((IUse o p Nothing v :) .) <$> acts o p a
methodValue o p e
    | Just (idx, alts) <- alternatives e =
        ((IUse o p Nothing idx :) .) <$> choice (methodValue o) p idx alts
methodValue o p e = return (IUse o p Nothing e :)

-- The calls and the uses in an action tree, which AConv walks the same way
-- (an action is never a def).
acts :: Owner -> Path -> HExpr -> KM ([Item] -> [Item])
acts o p a =
    case a of
      IAps (ICon _ (ICPrim _ PrimJoinActions)) _ [a1, a2] -> do
          items1 <- acts o p a1
          items2 <- acts o p a2
          return (items1 . items2)
      IAps (ICon _ (ICPrim _ op)) _ [e] | isSplitWrapper op -> acts o p e
      -- (the condition guards the rule or method, not a path in it)
      IAps (ICon _ (ICPrim _ PrimWhen)) _ [c, e] ->
          ((IUse o p Nothing c :) .) <$> acts o p e
      ICon _ (ICPrim _ PrimNoActions) -> return id
      ICon _ (ICUndet { }) -> return id
      IAps (ICon _ (ICUndet { })) _ _ -> return id
      IAps (ICon s (ICSel { })) _ [call]
          | s == idAVAction_, Just (k, args) <- foreignCall call ->
              return ((ICall k o p :) . (map (IUse o p (Just k)) args ++))
      _ | Just (idx, alts) <- alternatives a ->
            ((IUse o p Nothing idx :) .) <$> choice (acts o) p idx alts
      -- any other action uses its arguments
      _ -> return (IUse o p Nothing a :)

-- Walk the alternatives of a choice with walk, each on its path.  The
-- expression that chooses gets one key, computed only when some
-- alternative depends on it.
choice :: (Path -> HExpr -> KM ([Item] -> [Item])) -> Path -> HExpr ->
          [(Maybe Choice, HExpr)] -> KM ([Item] -> [Item])
choice walk p idx alts = do
    m_k <- if any (isJustChoice . fst) alts
           then Just <$> condKey idx
           else return Nothing
    let alt (m_ch, e) = do
            p' <- case (m_ch, m_k) of
                    (Just ch, Just k) -> (\ c -> S.insert c p) <$> choiceCond k ch
                    _ -> return p
            walk p' e
    composeAll <$> mapM alt alts
  where isJustChoice (Just _) = True
        isJustChoice Nothing = False

isSplitWrapper :: PrimOp -> Bool
isSplitWrapper PrimExpIf = True
isSplitWrapper PrimNoExpIf = True
isSplitWrapper PrimSplitDeep = True
isSplitWrapper PrimNosplitDeep = True
isSplitWrapper _ = False

-- The arm of a choice that an alternative is (see Cond)
data Choice = ChIf Bool | ChSel Integer | ChCase (Maybe HExpr)

choiceCond :: CKey -> Choice -> KM Cond
choiceCond k (ChIf b) = return (CIf k b)
choiceCond k (ChSel n) = return (CSel k n)
choiceCond k (ChCase m_v) = CCase k <$> mapM condKey m_v

-- The alternatives of a choice (an if, a dynamic selection of an array
-- that the module builds, or a case), each with the arm it is, and the
-- expression that chooses, which is also used.  A literal condition or
-- index chooses one alternative, which then has no condition: IExpand can
-- prune the action of an arm whose condition folds to 0 but leave its
-- value under an if on 0, which is not a use.
alternatives :: HExpr -> Maybe (HExpr, [(Maybe Choice, HExpr)])
alternatives (IAps (ICon _ (ICPrim _ PrimIf)) _ [c, t, e]) =
    Just (c, case literalValue c of
               Just 0 -> [(Nothing, e)]
               Just _ -> [(Nothing, t)]
               Nothing -> [(Just (ChIf True), t), (Just (ChIf False), e)])
alternatives (IAps (ICon _ (ICPrim _ PrimArrayDynSelect)) [_, ITNum sz] [arr, idx])
    | Just elems <- buildArrayElems arr =
        -- (an index out of range selects no element)
        let numbered = zip [0 .. 2 ^ sz - 1] elems
        in  Just (idx, case literalValue idx of
                         Just n -> [ (Nothing, e) | (m, e) <- numbered, m == n ]
                         Nothing -> [ (Just (ChSel m), e) | (m, e) <- numbered ])
alternatives (IAps (ICon _ (ICPrim _ PrimCase)) _ (idx : dflt : ces)) =
    let arms = casePairs ces
    in  Just (idx, case literalValue idx of
                     Just n ->
                         case [ e | (v, e) <- arms, literalValue v == Just n ] of
                           (e : _) -> [(Nothing, e)]
                           [] -> [(Nothing, dflt)]
                     Nothing -> (Just (ChCase Nothing), dflt) :
                                [ (Just (ChCase (Just v)), e) | (v, e) <- arms ])
alternatives _ = Nothing

-- the arms of a case: pairs of a value and its arm
casePairs :: [HExpr] -> [(HExpr, HExpr)]
casePairs (v : e : ces) = (v, e) : casePairs ces
casePairs _ = []

buildArrayElems :: HExpr -> Maybe [HExpr]
buildArrayElems (ICon _ (ICValue { iValDef = d })) = buildArrayElems d
buildArrayElems (IAps (ICon _ (ICPrim _ PrimBuildArray)) _ es) = Just es
buildArrayElems _ = Nothing

literalValue :: HExpr -> Maybe Integer
literalValue (ICon _ (ICInt { iVal = IntLit { ilValue = n } })) = Just n
literalValue _ = Nothing

-- the cookie and the arguments of a tagged foreign function call
foreignCall :: HExpr -> Maybe (Integer, [HExpr])
foreignCall (ICon _ (ICForeign { fcallNo = Just k })) = Just (k, [])
foreignCall (IAps (ICon _ (ICForeign { fcallNo = Just k })) _ es) = Just (k, es)
foreignCall _ = Nothing

-- The results (of the executions in infos) that e uses, with the
-- conditions common to its paths to each, computed once for each def.
-- The value side of a call has no arguments in the hardware (AConv drops
-- them).  A condition gets its key only when a result is used under it.
exprUses :: M.Map Integer CallInfo -> HExpr -> KM Uses
exprUses infos = go
  where
    go (IAps (ICon s (ICSel { })) _ [call])
        | s == idAVValue_, Just (k, _) <- foreignCall call =
            return (if M.member k infos then M.singleton k S.empty else M.empty)
    go (ICon i (ICValue { iValDef = d })) = do
        m_uses <- gets (M.lookup i . ks_def_uses)
        case m_uses of
          Just uses -> return uses
          Nothing -> do
            uses <- go d
            modify (\ s -> s { ks_def_uses = M.insert i uses (ks_def_uses s) })
            return uses
    go e | Just (idx, alts) <- alternatives e = do
        idx_uses <- go idx
        alt_uses <- mapM (go . snd) alts
        if all M.null alt_uses
          then return idx_uses
          else do
            k <- condKey idx
            let under (Just ch, _) uses | not (M.null uses) = do
                    c <- choiceCond k ch
                    return (M.map (S.insert c) uses)
                under _ uses = return uses
            alt_uses' <- zipWithM under alts alt_uses
            return (meets (idx_uses : alt_uses'))
    go (IAps f _ es) = meets <$> mapM go (f : es)
    go _ = return M.empty
    -- (a cookie that only one side uses keeps its conditions)
    meets = foldr (M.unionWith S.intersection) M.empty

-- ---------------
-- Diagnostics

-- Where the call of a result used outside its scope is made: in the
-- owner of the use under other conditions, else in the first owner that
-- makes it, else nowhere (with the read that drops it, if known).
callMade :: Owner -> [(Owner, Path)] -> CallKind -> ForeignCallMade
callMade o calls kind
    | any ((== o) . fst) calls = ForeignCallMadeHere (ownerKind o)
    | ((o', _) : _) <- calls = ForeignCallMadeIn (ownerName o')
    | otherwise = ForeignCallNotMade (readPosition kind)

readPosition :: CallKind -> Maybe Position
readPosition (CKPeek pos) | usablePosition pos = Just pos
readPosition (CKRan (Just w)) = peekWorldPosition w
readPosition _ = Nothing

-- The position of the first read by __value (primPeekWorld) that the world
-- of an execution comes from, if any: the execution was made in a choice
-- of worlds inside that read, which dropped its action.  The walk follows
-- heap cells, each once; it runs only for a diagnostic.
peekWorldPosition :: HExpr -> Maybe Position
peekWorldPosition w0 = go IS.empty [w0]
  where
    go _ [] = Nothing
    go seen (e : es) =
      case e of
        IAps (ICon wi (ICPrim _ PrimPeekWorld)) _ _
            | usablePosition (getIdPosition wi) -> Just (getIdPosition wi)
        IAps f _ as -> go seen (f : as ++ es)
        ICon _ (ICLazyArray { iArray = arr }) ->
            let (seen', es') = foldr cell (seen, es)
                                 [ (p, r) | ArrayCell p r <- Array.elems arr ]
            in  go seen' es'
        IRefT _ p _ r -> let (seen', es') = cell (p, r) (seen, es)
                         in  go seen' es'
        _ -> go seen es
    cell (p, r) (seen, es)
        | IS.member p seen = (seen, es)
        | otherwise = (IS.insert p seen, cellExprs (unsafeDerefHeap r) ++ es)
    cellExprs (HUnev { hc_hexpr = e }) = [e]
    cellExprs (HWHNF { hc_pexpr = P _ e }) = [e]
    cellExprs (HNF { hc_pexpr = P _ e }) = [e]
    cellExprs (HLoop { }) = []

-- One error for each owner and call, at the owner, naming the first read
-- (in source order) when the call is not made; ordered by the owner's
-- position, then the call's.
foreignResultMsgs :: M.Map Id Position -> IModule HeapData ->
                     [(Owner, CallInfo, ForeignCallMade)] -> [EMsg]
foreignResultMsgs meth_poss imod bad =
    let groups = M.fromListWith (++)
                   [ ((o, ci_name info, ci_link info, posKey (ci_call_pos info)),
                      [(info, made)])
                   | (o, info, made) <- bad ]
        msg ((o, _, _, _), ims@((info, _) : _)) =
            let made = minimumBy (comparing madeKey) (map snd ims)
                pos = ownerPosition meth_poss imod o
                user = ownerName o
            in  [ ((posKey pos, posKey (ci_call_pos info), ci_name info,
                    ci_link info, user),
                   (pos, EForeignResultUsed (ci_name info) (ci_link info)
                                            (ci_call_pos info) user made)) ]
        msg (_, []) = []
    in  M.elems (M.fromList (concatMap msg (M.toList groups)))

-- the order in which the messages choose where the call is made
madeKey :: ForeignCallMade -> (Int, Maybe (String, Int, Int), String)
madeKey (ForeignCallNotMade (Just p)) = (0, Just (posKey p), "")
madeKey (ForeignCallNotMade Nothing) = (1, Nothing, "")
madeKey (ForeignCallMadeHere s) = (2, Nothing, s)
madeKey (ForeignCallMadeIn s) = (3, Nothing, s)

posKey :: Position -> (String, Int, Int)
posKey p = (getPositionFile p, getPositionLine p, getPositionColumn p)

ownerPosition :: M.Map Id Position -> IModule HeapData -> Owner -> Position
ownerPosition _ _ (ORuleBody i) = getPosition i
ownerPosition _ _ (ORuleCond i) = getPosition i
ownerPosition meth_poss _ (OMethod i) = M.findWithDefault (getPosition i) i meth_poss
ownerPosition meth_poss _ (OReady i) = M.findWithDefault (getPosition i) i meth_poss
ownerPosition _ _ (OInst i) = getPosition i
ownerPosition _ imod OModule = getPosition (imod_name imod)

ownerName :: Owner -> String
ownerName (ORuleBody i) = "rule " ++ quote (getIdBaseString (dropRulePrefixId i))
ownerName (ORuleCond i) =
    "the condition of rule " ++ quote (getIdBaseString (dropRulePrefixId i))
ownerName (OMethod i) = "method " ++ quote (getIdBaseString i)
ownerName (OReady i) =
    "the condition of method " ++ quote (getIdBaseString (dropReadyPrefixId i))
ownerName (OInst i) = "the instantiation of " ++ quote (getIdBaseString i)
ownerName OModule = "the clocks or resets of the module"

-- the kind of an owner that makes calls
ownerKind :: Owner -> String
ownerKind (OMethod _) = "method"
ownerKind _ = "rule"
