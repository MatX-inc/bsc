-- | satc's abstract Boolean skeleton as a "Lean.Solver" circuit: other terms free atoms by canonical
-- form, @x == k@ exclusive for distinct constants; UNSAT holds for the real values.
module Lean.Sched.Enc
    ( Enc, EncM, newEnc, encB, encEqAtom, encAtom, encAnd, encOr, encSat, encSatA, Assume, mkAssume, encFMemo, encSetSub, encSetOn, AKey(..), KP(..)
    , portAtom
    ) where

import Control.Monad (forM, foldM)
import Control.Monad.State.Strict (State, gets, modify', runState, MonadState(get, state))
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM (IntMap, (!), delete, empty, findWithDefault, lookup, fromList,
    fromListWith, insert, insertWith)
import qualified Data.IntSet as IS (member, IntSet, empty, fromList, insert, size, toAscList, toList)
import qualified Data.Map.Strict as M (Map, (!), elems, empty, keysSet, lookup, union, insert, fromList,
    fromListWith, insertWith, singleton)
import qualified Data.Set as S (Set, intersection, toAscList, union)

import Lean.Sched.Expr (boolDef, boolish, canon, exprReads, isBoolLit, litval, Port2, SD)
import Lean.Sched.Tree (E(..))
import Lean.Solver (cAnd, cAtom, cCone, cGate, cIte, cOr, newCircuit, solveC, Circuit, Solver, Verdict)

-- | A part of an atom's key, as satc's tuples: a string, a canonical form under a
-- skip set (a site value's fresh read), a skip set, or a constant.
data KP = KS String | KPair String [Port2] | KSkip [Port2] | KI Integer
    deriving (Eq, Ord, Show)

data AKey = AKey String [KP]
    deriving (Eq, Ord, Show)

-- | One CNF problem over a design's view (EncS: while 'enOn', an atom over a
-- conditional read y stands for its site value ite(cond_y, port_y, fresh)).
data Enc = Enc
    { enC    :: !(Circuit AKey)
    , enSD   :: SD
    , enMemo :: !(M.Map (String, Bool) Int)
    , enEqs  :: !(M.Map (KP) (M.Map Integer Int))
    , enSub  :: !(M.Map Port2 Int)
    , enOn   :: !Bool
    , enEqVar :: !(IM.IntMap (KP, Integer))   -- each 'eq' atom's operand and constant
    , enInfo :: !(IM.IntMap GInfo)            -- 'reduceRoots' facts per gate
    , enFMemo :: !(IM.IntMap Int)             -- formula literals ('encFMemo'), one formula store per encoder
    }

-- | A gate's literals through its positive AND operands, the positive 'eq' atoms and the positive atoms among them.
data GInfo = GInfo { giSet :: !IS.IntSet, giEq :: [(KP, Integer)], giAtoms :: [Int] }

type EncM = State Enc

newEnc :: Solver -> SD -> IO Enc
newEnc sv sd = do
    c <- newCircuit sv
    return (Enc c sd M.empty M.empty M.empty False IM.empty IM.empty IM.empty)

liftC :: (Circuit AKey -> (Int, Circuit AKey)) -> EncM Int
liftC f = state (\e -> let (v, c) = f (enC e) in (v, e { enC = c }))

encAtom :: AKey -> EncM Int
encAtom k = liftC (cAtom k)

encAnd, encOr :: [Int] -> EncM Int
encAnd xs = liftC (cAnd xs)
encOr xs = liftC (cOr xs)

encIte :: Int -> Int -> Int -> EncM Int
encIte i a b = liftC (cIte i a b)

encSetSub :: Port2 -> Int -> EncM ()
encSetSub y v = modify' (\e -> e { enSub = M.insert y v (enSub e) })

encSetOn :: Bool -> EncM ()
encSetOn b = modify' (\e -> e { enOn = b })

-- | satc's _wrap: over the site values of the conditional reads among @reads@.
wrap :: S.Set Port2 -> ([Port2] -> EncM Int) -> EncM Int
wrap reads mkA = do
    e <- get
    let ys = if enOn e then S.toAscList (S.intersection reads (M.keysSet (enSub e))) else []
    if null ys then mkA [] else do
        outs <- forM [0 .. (2 :: Int) ^ length ys - 1] $ \mask -> do
            let inS i = odd (mask `div` (2 ^ i))
                sk = [ y | (i, y) <- zip [0 :: Int ..] ys, inS i ]
                lits = [ if inS i then negate (enSub e M.! y) else enSub e M.! y | (i, y) <- zip [0 :: Int ..] ys ]
            a <- mkA sk
            encAnd (lits ++ [a])
        encOr outs

atomS :: AKey -> [Port2] -> EncM Int
atomS (AKey t ps) sk = encAtom (AKey t (if null sk then ps else ps ++ [KSkip sk]))

eqAtomS :: E -> E -> [Port2] -> EncM Int
eqAtomS x kexpr sk = do
    sd <- gets enSD
    let k = case kexpr of { ELit s -> litval s; _ -> Nothing }
        cx = if null sk then KS (canon sd x) else KPair (canon sd x) sk
    case k of
        Nothing -> encAtom (AKey "eqv" [cx, KS (canon sd kexpr)])
        Just kv -> do
            eqs <- gets enEqs
            case M.lookup cx eqs >>= M.lookup kv of
                Just v -> return v
                Nothing -> do
                    v <- encAtom (AKey "eq" [cx, KI kv])
                    modify' (\e -> e { enEqs = M.insertWith M.union cx (M.singleton kv v) (enEqs e)
                                     , enEqVar = IM.insert v (cx, kv) (enEqVar e) })
                    return v

-- | An 'eq' literal (case arm, array selection), reading site values while on.
encEqAtom :: E -> E -> EncM Int
encEqAtom x kexpr = do
    sd <- gets enSD
    wrap (S.union (exprReads sd x) (exprReads sd kexpr)) (eqAtomS x kexpr)

-- | The literal of a Boolean expression.
encB :: E -> EncM Int
encB e = do
    sd <- gets enSD
    on <- gets enOn
    case e of
        ELit s -> case isBoolLit e of
            Just b -> return (if b then 1 else -1)
            Nothing -> encAtom (AKey "lit" [KS s])
        EId n -> do
            memo <- gets enMemo
            case M.lookup (n, on) memo of
                Just v -> return v
                Nothing -> do
                    v <- case boolDef sd n of
                        Nothing -> wrap (exprReads sd e) (atomS (AKey "id" [KS n]))
                        Just de -> do
                            g <- encAtom (AKey "cyc" [KS n, KS (show on)])
                            modify' (\st -> st { enMemo = M.insert (n, on) g (enMemo st) })
                            encB de
                    modify' (\st -> st { enMemo = M.insert (n, on) v (enMemo st) })
                    return v
        EUn o x | o `elem` ["!", "~"] -> negate <$> encB x
        EBin o a b
            | o == "&&" || (o == "&" && boolish sd a && boolish sd b) -> do { x <- encB a; y <- encB b; encAnd [x, y] }
            | o == "||" || (o == "|" && boolish sd a && boolish sd b) -> do { x <- encB a; y <- encB b; encOr [x, y] }
            | o `elem` ["==", "!="] -> do
                let (bl, l, r) = case isBoolLit b of
                        Just v -> (Just v, a, b)
                        Nothing -> (isBoolLit a, b, a)
                    pol x = if o == "==" then x else negate x
                case bl of
                    Just v | boolish sd l -> do { x <- encB l; return (pol (if v then x else negate x)) }
                    _ -> do
                        let rd = exprReads sd e
                        x <- case (r, l) of
                            (ELit _, _) -> wrap rd (eqAtomS l r)
                            (_, ELit _) -> wrap rd (eqAtomS r l)
                            _ -> let [c1, c2] = sortPair (canon sd l) (canon sd r)
                                 in wrap rd (atomS (AKey "eqv" [KS c1, KS c2]))
                        return (pol x)
        EIf c a b -> do { i <- encB c; x <- encB a; y <- encB b; encIte i x y }
        ECase idx arms d -> do
            let rd = exprReads sd idx
                go [] prev acc = return (reverse acc, prev)
                go ((lab, val) : rest) prev acc = do
                    q <- wrap rd (eqAtomS idx lab)
                    v <- encB val
                    s <- encAnd ([q] ++ map negate prev ++ [v])
                    go rest (prev ++ [q]) (s : acc)
            (sels, prev) <- go arms [] []
            dsel <- case d of
                Nothing -> return []
                Just dv -> do
                    v <- encB dv
                    s <- encAnd (map negate prev ++ [v])
                    return [s]
            encOr (sels ++ dsel)
        ECall {} -> wrap (exprReads sd e) (atomS (AKey "port" [KS (canon sd e)]))
        _ -> wrap (exprReads sd e) (atomS (AKey "t" [KS (canon sd e)]))
  where sortPair x y = if x <= y then [x, y] else [y, x]

-- | A formula node's literal kept across queries (only while site values are off).
encFMemo :: Int -> EncM Int -> EncM Int
encFMemo f mk = do
    on <- gets enOn
    m <- gets enFMemo
    case IM.lookup f m of
        Just v | not on -> return v
        _ -> do
            v <- mk
            if on then return () else modify' (\e -> e { enFMemo = IM.insert f v (enFMemo e) })
            return v

-- | The atom of a child port read with no arguments (satc's ('port', canon)).
portAtom :: Port2 -> EncM Int
portAtom (i, m) = do
    sd <- gets enSD
    encAtom (AKey "port" [KS (canon sd (ECall i m []))])

-- | Assumptions encoded once and drawn into a query only where they can change its verdict.
data Assume = Assume { asLit :: IM.IntMap Int, asFree :: [Int], asIdx :: IM.IntMap [Int], asInfo :: IM.IntMap GInfo }

noAssume :: Assume
noAssume = Assume IM.empty [] IM.empty IM.empty

-- | Assumptions by number: a @not g@ is indexed by the positive atoms of g, any other literal always drawn.
mkAssume :: [Int] -> EncM Assume
mkAssume lits = do
    c <- gets enC
    as <- forM (zip [0 ..] lits) $ \(i, l) -> case cGate c l of
        Just _ | l < 0 -> do { gi <- ginfo (negate l); return (i, l, Just gi) }
        _ -> return (i, l, Nothing)
    let atoms = maybe [] giAtoms
    return (Assume (IM.fromList [ (i, l) | (i, l, _) <- as ]) [ i | (i, l, g) <- as, null (atoms g), l /= 1 ]
                   (IM.fromListWith (++) [ (a, [i]) | (i, _, g) <- as, a <- atoms g ])
                   (IM.fromList [ (i, gi) | (i, _, Just gi) <- as ]))

-- | Is the conjunction of @roots@ satisfiable?
encSat :: Solver -> [Int] -> Enc -> IO (Verdict, Enc)
encSat sv roots = encSatA sv roots noAssume (const False)

-- | Is the conjunction of @roots@ and the assumptions @sel@ admits satisfiable?  Each call adds
-- the exclusions between the constants of one operand its cone reaches (an at-most-one chain).
encSatA :: Solver -> [Int] -> Assume -> (Int -> Bool) -> Enc -> IO (Verdict, Enc)
encSatA sv roots asm sel e0 = do
    let ((rs, cls), e1) = runState (do { rs <- reduceRoots roots asm sel; k <- gets (\e -> cCone (enC e) rs); cs <- chains k; return (rs, cs) }) e0
    v <- solveC sv (enC e1) cls rs
    return (v, e1)
  where
    chains cone = do
        ev <- gets enEqVar
        let byOp = M.fromListWith M.union [ (o, M.singleton k v) | v <- IS.toList cone, Just (o, k) <- [IM.lookup v ev] ]
        fmap concat $ forM (M.elems byOp) $ \d -> case M.elems d of
            (v0 : vs@(_ : _)) -> do
                let step (pre, acc) v = do
                        g <- encAnd [negate pre, negate v]
                        return (negate g, [negate pre, negate v] : acc)
                (_, cs) <- foldM step (v0, []) vs
                return cs
            _ -> return []

-- | Roots and assumptions without each @not g@ every model of the rest extends to (g has a unit negated,
-- another constant of a unit's operand, or an unreached positive atom set false): the verdict is unchanged.
reduceRoots :: [Int] -> Assume -> (Int -> Bool) -> EncM [Int]
reduceRoots roots asm sel = do
    c <- gets enC
    ev <- gets enEqVar
    let gate v = cGate c v
        flat = go (roots ++ [ asLit asm IM.! i | i <- asFree asm, sel i ]) IS.empty IS.empty []
          where go [] _ _ acc = reverse acc
                go (l : st) seenG seenL acc
                    | l == 1 || IS.member l seenL = go st seenG seenL acc
                    | l > 0, Just xs <- gate l = if IS.member l seenG then go st seenG seenL acc else go (xs ++ st) (IS.insert l seenG) seenL acc
                    | otherwise = go st seenG (IS.insert l seenL) (l : acc)
        units = [ l | l <- flat, not (isJust (gate l)) ]
        cands = [ l | l <- flat, isJust (gate l) ]
        nC = length cands
        unitSet = IS.fromList units
        eqUnits = M.fromList [ (o, k) | u <- units, u > 0, Just (o, k) <- [IM.lookup u ev] ]
        implied gi = (if IS.size unitSet <= IS.size (giSet gi) then any (\u -> IS.member (negate u) (giSet gi)) units
                      else any (\l -> IS.member (negate l) unitSet) (IS.toList (giSet gi)))
                     || any (\(o, k) -> maybe False (/= k) (M.lookup o eqUnits)) (giEq gi)
    infos <- mapM (ginfo . negate) cands
    let live = [ (j, l, gi) | (j, l, gi) <- zip3 [0 :: Int ..] cands infos, not (implied gi) ]
        candArr = IM.fromList (zip [0 ..] cands)
        litOf j = if j < nC then candArr IM.! j else asLit asm IM.! (j - nC)
        candLit = IM.fromList [ (j, l) | (j, l, _) <- live ]
        -- pending conjuncts: atoms still unreached, and who waits on each atom
        pend0 = IM.fromList [ (j, length (giAtoms gi)) | (j, _, gi) <- live, not (null (giAtoms gi)) ]
        widx0 = IM.fromListWith (++) [ (a, [j]) | (j, _, gi) <- live, a <- giAtoms gi ]
        keep0 = [ j | (j, _, gi) <- live, null (giAtoms gi) ]
        -- add literals' cones; each new variable releases waiters and draws indexed assumptions
        run k pend widx seen keep [] = (k, keep, pend, widx, seen)
        run k pend widx seen keep (l : q) =
            let new = coneNew k l
                k' = foldl (flip IS.insert) k new
                step (pd, wx, sn, kp, qq) v =
                    let (pd1, kp1, q1) = foldl (\(p, kk, q2) j -> case IM.lookup j p of
                                            Just 1 -> (IM.delete j p, j : kk, litOfJ j : q2)
                                            Just n -> (IM.insert j (n - 1) p, kk, q2)
                                            Nothing -> (p, kk, q2)) (pd, kp, qq) (IM.findWithDefault [] v wx)
                        fresh = [ i | i <- IM.findWithDefault [] v (asIdx asm), sel i, not (IS.member i sn) ]
                        (pd2, wx2, kp2, q3) = foldl (\(p, x, kk, q2) i ->
                            let j = nC + i
                                ats = giAtoms (asInfo asm IM.! i)
                                miss = filter (not . (`IS.member` k')) ats
                            in if impliedA i then (p, x, kk, q2)
                               else if null miss then (p, x, j : kk, litOfJ j : q2)
                               else (IM.insert j (length miss) p, foldl (\xx a -> IM.insertWith (++) a [j] xx) x miss, kk, q2))
                            (pd1, wx, kp1, q1) fresh
                    in (pd2, wx2, foldl (flip IS.insert) sn fresh, kp2, q3)
                (pend', widx', seen', keep', q') = foldl step (pend, widx, seen, keep, q) new
            in run k' pend' widx' seen' keep' q'
        litOfJ = litOf
        impliedA i = implied (asInfo asm IM.! i)
        coneNew k l = goC [abs l] IS.empty []
          where goC [] _ acc = acc
                goC (v : st) seen acc
                    | IS.member v seen || IS.member v k = goC st seen acc
                    | otherwise = goC (maybe [] (map abs) (gate v) ++ st) (IS.insert v seen) (v : acc)
        (_, keepL, _, _, _) = run IS.empty pend0 widx0 IS.empty keep0 (units ++ [ candLit IM.! j | j <- keep0 ])
        keepS = IS.fromList keepL
    return (units ++ [ l | (j, l, _) <- live, IS.member j keepS ] ++ [ asLit asm IM.! (j - nC) | j <- IS.toAscList keepS, j >= nC ])

-- | 'reduceRoots' facts of a gate, computed once per encoder.
ginfo :: Int -> EncM GInfo
ginfo g = do
    memo <- gets enInfo
    case IM.lookup g memo of
        Just gi -> return gi
        Nothing -> do
            c <- gets enC
            ev <- gets enEqVar
            let lits = go [g] IS.empty IS.empty
                go [] _ acc = acc
                go (l : st) seen acc
                    | l > 0, Just xs <- cGate c l = if IS.member l seen then go st seen acc else go (xs ++ st) (IS.insert l seen) acc
                    | otherwise = go st seen (IS.insert l acc)
                pos = [ l | l <- IS.toList lits, l > 1, not (isJust (cGate c l)) ]
                gi = GInfo lits [ ok | l <- pos, Just ok <- [IM.lookup l ev] ] pos
            modify' (\e -> e { enInfo = IM.insert g gi (enInfo e) })
            return gi
