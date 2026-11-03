-- | Reach conditions (sitecond.Reach): under which path condition a step's
-- expressions reach a child read.  && and || pass their operands on, an arm of
-- an if / case / array selection is conditional on its select; in the readiness
-- mode an operand of && or || is reached under the earlier operand's outcome.
module Lean.Sched.Reach
    ( Lit(..), RC(..), RS(..), newRS, ReachM, litKey, reachTop, litExprs, encFormula, encFormulaShared, liftFS
    ) where

import Control.Monad (forM)
import Control.Monad.State.Strict (State, gets, modify, modify', evalStateT, MonadState(get, state),
    MonadTrans(lift), StateT)
import qualified Data.Map.Strict as M (Map, (!), empty, lookup, member, toList, insert, fromListWith,
    insertWith)
import qualified Data.Set as S (Set, member, intersection, null)

import Lean.Sched.Enc (encAnd, encB, encEqAtom, encFMemo, encOr, EncM)
import Lean.Sched.Expr (arrayElems, canonExpr, defReads, fsAnd, fsEmpty, fsF, fsLit, fsNode, fsOr, fsT, width,
    FS, LitKey, Node(NOr, NT, NF, NLit, NAnd), Port2, SD(sdDefs))
import Lean.Sched.Tree (E(..))

-- | A reach literal: an expression's truth, or an operand equal to a constant.
data Lit = LC E | LEq E E

litExprs :: Lit -> [E]
litExprs (LC e) = [e]
litExprs (LEq a b) = [a, b]

-- | The fixed part: the design's view, the reads looked for, the readiness mode.
data RC = RC { rcSD :: SD, rcTargets :: S.Set Port2, rcSeq :: Bool }

-- | The formulas, the literals by key (the first expressions seen), and R per definition.
data RS = RS { rsFS :: !FS, rsLit :: !(M.Map LitKey Lit), rsMemo :: !(M.Map String (M.Map Port2 Int))
             , rsHit :: !(M.Map String Bool) }   -- ^ per definition: does it read a target?

newRS :: RS
newRS = RS fsEmpty M.empty M.empty M.empty

type ReachM = State RS

liftFS :: (FS -> (Int, FS)) -> ReachM Int
liftFS = liftF

liftF :: (FS -> (Int, FS)) -> ReachM Int
liftF f = state (\r -> let (v, fs) = f (rsFS r) in (v, r { rsFS = fs }))

litKey :: String -> [E] -> ReachM LitKey
litKey kind xs = do
    let key = (kind, map canonExpr xs)
        l = case xs of { [a] -> LC a; [a, b] -> LEq a b; _ -> error "litKey" }
    modify' (\r -> r { rsLit = M.insertWith (\_ old -> old) key l (rsLit r) })
    return key

data EKey = KDef String | KPort Port2
    deriving (Eq, Ord)

-- | The path conditions under which @e@ (reached under @c@) reaches each target or definition.
edges :: RC -> [(E, Int)] -> M.Map EKey [Int] -> ReachM (M.Map EKey [Int])
edges _ [] acc = return acc
edges rc ((x, c) : st) acc
    | c == fsF = edges rc st acc
    | otherwise = case x of
        EId n | isWF n || not (M.member n (sdDefs sd)) -> edges rc st acc
              | otherwise -> do
                  hits <- gets rsHit
                  h <- case M.lookup n hits of
                      Just h -> return h
                      Nothing -> do
                          let h = not (S.null (S.intersection (defReads sd n) (rcTargets rc)))
                          modify' (\r -> r { rsHit = M.insert n h (rsHit r) })
                          return h
                  edges rc st (if h then add (KDef n) acc else acc)
        ECall i m as ->
            let acc' = if S.member (i, m) (rcTargets rc) then add (KPort (i, m)) acc else acc
            in edges rc ([ (a, c) | a <- as ] ++ st) acc'
        EIf a b d -> do
            k <- litKey "c" [a]
            t <- lit k True
            f <- lit k False
            edges rc ((a, c) : (b, t) : (d, f) : st) acc
        ECase idx arms d -> do
            ks <- mapM (\(lab, _) -> litKey "eq" [idx, lab]) arms
            vs <- forM (zip ks arms) $ \(k, (_, v)) -> do { t <- lit k True; return (v, t) }
            ds <- case d of
                Nothing -> return []
                Just dv -> do
                    ns <- mapM (\k -> liftF (fsLit k False)) ks
                    g <- liftF (fsAnd (c : ns))
                    return [(dv, g)]
            edges rc ((idx, c) : vs ++ ds ++ st) acc
        ESel a i -> case arrayElems sd a of
            Nothing -> edges rc ((i, c) : (a, c) : st) acc
            Just es -> do
                let w = width sd i
                    n = case w of
                        Just wv | wv < 20 -> min (length es) (2 ^ wv)
                        _ -> length es
                ys <- forM (zip [0 :: Int ..] (take n es)) $ \(kk, el) -> do
                    k <- litKey "eq" [i, ELit (show kk)]
                    t <- lit k True
                    return (el, t)
                edges rc ((i, c) : ys ++ st) acc
        EApp _ as -> edges rc ([ (a, c) | a <- as ] ++ st) acc
        EBin o a b
            | rcSeq rc && o `elem` ["&&", "||"] -> do
                ka <- litKey "c" [a]
                t <- lit ka (o == "&&")
                edges rc ((a, c) : (b, t) : st) acc
            | otherwise -> edges rc ((a, c) : (b, c) : st) acc
        EUn _ a -> edges rc ((a, c) : st) acc
        ETup xs -> edges rc ([ (a, c) | a <- xs ] ++ st) acc
        ELit _ -> edges rc st acc
  where
    sd = rcSD rc
    add k = M.insertWith (flip (++)) k [c]
    lit k p = do { l <- liftF (fsLit k p); liftF (fsAnd [c, l]) }
    isWF = (\s -> take 10 s == "WILL_FIRE_")

-- | Port -> the condition under which a read reaches definition @n@'s value.
reachDef :: RC -> String -> ReachM (M.Map Port2 Int)
reachDef rc n = do
    memo <- gets rsMemo
    case M.lookup n memo of
        Just r -> return r
        Nothing -> do
            modify' (\r -> r { rsMemo = M.insert n M.empty (rsMemo r) })
            acc <- edges rc [(sdDefs (rcSD rc) M.! n, fsT)] M.empty
            r <- collect rc acc
            modify' (\s -> s { rsMemo = M.insert n r (rsMemo s) })
            return r

collect :: RC -> M.Map EKey [Int] -> ReachM (M.Map Port2 Int)
collect rc acc = do
    parts <- forM (M.toList acc) $ \(key, cs) -> do
        c <- liftF (fsOr cs)
        case key of
            KPort p -> return [(p, c)]
            KDef n -> do
                r <- reachDef rc n
                forM (M.toList r) $ \(p, f) -> do { g <- liftF (fsAnd [c, f]); return (p, g) }
    let out = M.fromListWith (flip (++)) [ (p, [c]) | (p, c) <- concat parts ]
    traverse (\cs -> liftF (fsOr cs)) out

-- | items: (expression, path condition) -> port -> reach condition.
reachTop :: RC -> [(E, Int)] -> ReachM (M.Map Port2 Int)
reachTop rc items = edges rc items M.empty >>= collect rc

-- | A reach formula's literal in an encoder (site values where the encoder is on).
encFormula :: RS -> Int -> EncM Int
encFormula rs f0 = evalStateT' M.empty (go f0)
  where
    fs = rsFS rs
    go f = do
        memo <- getM
        case M.lookup f memo of
            Just v -> return v
            Nothing -> do
                v <- case fsNode fs f of
                    NT -> return 1
                    NF -> return (-1)
                    NLit k p -> do
                        x <- liftE (case rsLit rs M.! k of { LC e -> encB e; LEq a b -> encEqAtom a b })
                        return (if p then x else negate x)
                    NAnd ys -> mapM go ys >>= liftE . encAnd
                    NOr ys -> mapM go ys >>= liftE . encOr
                modify (M.insert f v)
                return v

-- | 'encFormula' with its node literals kept in the encoder, for an encoder that only
-- ever sees this formula store.
encFormulaShared :: RS -> Int -> EncM Int
encFormulaShared rs f0 = go f0
  where
    fs = rsFS rs
    go f = encFMemo f $ case fsNode fs f of
        NT -> return 1
        NF -> return (-1)
        NLit k p -> do
            x <- case rsLit rs M.! k of { LC e -> encB e; LEq a b -> encEqAtom a b }
            return (if p then x else negate x)
        NAnd ys -> mapM go ys >>= encAnd
        NOr ys -> mapM go ys >>= encOr

-- a memo threaded through EncM
type MemoM = StateT (M.Map Int Int) EncM

evalStateT' :: M.Map Int Int -> MemoM a -> EncM a
evalStateT' = flip evalStateT

getM :: MemoM (M.Map Int Int)
getM = get

liftE :: EncM a -> MemoM a
liftE = lift
