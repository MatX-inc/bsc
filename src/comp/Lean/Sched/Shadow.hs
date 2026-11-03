-- | Action shadowing with last caller (shadow.py): each child action port two
-- steps call is classed once, by the child, with one solver call per port.
--
--   L  a decided (primitive, port) with a proved last-caller law, m SBR m and
--      lcKey on the instance's schedule; then only the static order is checked
--   U  self-SBR outside the decided list, or failing lcKey
--   I  self-CF/SB with identical calls (I-fail for Counter's adds)
--   S  a synthesized child
--   X  anything else;  ?  the solver gave no verdict
module Lean.Sched.Shadow
    ( shadowReport, ShadowOut(..)
    ) where

import Control.Monad (foldM, forM)
import Control.Monad.State.Strict (runState)
import Data.IORef (modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, isJust)
import PPrint (ppString)
import qualified Data.IntMap.Strict as IM (IntMap, (!), fromList)
import qualified Data.Map.Strict as M (null, (!), elems, filter, findWithDefault, lookup, member, toList,
    fromList, fromListWith)
import qualified Data.Set as S (Set, member, delete, difference, empty, filter, fromList, insert, intersection,
    null, singleton, size, toAscList, toList, union, unions)

import Lean.Sched.Enc (encAnd, encB, encOr, encSatA, mkAssume, newEnc, Assume, EncM)
import Lean.Sched.Expr (SD(..), canonExpr, aexprE)
import Lean.Sched.MECheck (meParse, samePairs, suffixGroups, mePairs, cfNames)
import Lean.Sched.Model (DView(..))
import Lean.Sched.Py (omLookup, omToList)
import Lean.Sched.Sites (Sites(..), annOf)
import Lean.Sched.Tree (APkg(apRules, apInsts), Act(ACallA), E(..))
import Lean.Sched.Types (Child(chSched, chModule, chStmts, chOrder), Design(dUserME, dSteps, dChildren,
    dExportedC, dExportedSB, dDisjoint, dBlocks), Step(stName, stChecks, stDescr))
import Lean.Solver (Solver, Verdict(Unknown, Unsat))

-- | (bsc Verilog primitive, port) with a last-caller law proved in LastCaller.lean.
decided :: S.Set (String, String)
decided = S.fromList ([ (t, "write") | t <- ["RegN", "RegUN", "RegA", "ConfigRegN", "ConfigRegUN", "ConfigRegA"] ]
                      ++ [ (t, "port" ++ show k ++ "__write") | t <- ["CRegN5", "CRegUN5", "CRegA5"], k <- [0 .. 4 :: Int] ]
                      ++ [("RWire", "wset"), ("RWire0", "wset"), ("FIFO2", "clear"), ("Counter", "setF")])

-- | The ones the emitter certifies (its kinds).
wired :: S.Set (String, String)
wired = S.filter (\(t, _) -> t `elem` ["RegN", "RegUN", "RegA", "ConfigRegN"]) decided

notIdempotent :: S.Set (String, String)
notIdempotent = S.fromList [("Counter", "addA"), ("Counter", "addB")]

-- | The summary line and port lines (witnesses left out), class counts, last-caller callers,
-- ports relying on assertions (me, cf), and the blocked ports with their witness pair.
data ShadowOut = ShadowOut { shLines :: [String], shOk :: Bool, shClasses :: [(String, Int)], shLcMax :: Int
                           , shRely :: (Int, Int), shBlocked :: [String] }

shadowReport :: Solver -> Design -> DView -> Sites -> S.Set String -> String -> IO ShadowOut
shadowReport sv d dv sites prims name = do
  qref <- newEnc sv (sxDX sites) >>= newIORef
  aref <- newIORef Nothing
  -- a query with the conflict_free lines (encoded once) that name one of @names@ as assumptions
  let query mk cfs names = do
        e0 <- readIORef qref
        ma <- readIORef aref
        let ((rs, (asm, nms)), e1) = runState (do { a <- maybe cfs return ma; r <- mk; return (r, a) }) e0
            sel i = not (S.null (S.intersection (nms IM.! i) names))
        writeIORef aref (Just (asm, nms))
        (v, e2) <- encSatA sv rs asm sel e1
        writeIORef qref e2
        return v
  shadowReport' query d dv sites prims name

shadowReport' :: (EncM [Int] -> EncM (Assume, IM.IntMap (S.Set String)) -> S.Set String -> IO Verdict)
              -> Design -> DView -> Sites -> S.Set String -> String -> IO ShadowOut
shadowReport' query0 d dv sites prims name =
    case [ err | (_, _, c) <- clines, Left err <- [meParse c] ] of
        (err : _) -> return (bad (name ++ " ERROR " ++ err))
        [] | not (samePairs mel (dUserME d)) ->
                return (bad (name ++ " ERROR mutually_exclusive check lines (" ++ show (pairCount (concatMap mePairs mel))
                        ++ " pairs) differ from the dumper's list (" ++ show (pairCount (dUserME d)) ++ " pairs)"))
           | otherwise -> run
  where
    bad l = ShadowOut [l] False [] 0 (0, 0) []
    pairCount ps = S.size (S.fromList [ S.fromList [a, b] | (a, b) <- ps ])
    ap = dvA dv
    dx = sxDX sites
    clines = [ (stName st, ppString c, c) | st <- dSteps d, c <- stChecks st ]
    mel = [ x | (_, _, c) <- clines, Right (Just x) <- [meParse c] ]
    -- the site tool's steps: kind and method from the step's description
    stepInfo st = case words (filter (/= ']') (stDescr st)) of
        ("value" : "method" : m : _) -> ("value", Just m)
        ("value" : "method" : _) -> ("value", Just "")
        ("method" : m : _) -> ("method", Just m)
        ("method" : _) -> ("method", Just "")
        _ -> ("rule", Nothing)
    steps = [ (stName st, fst (stepInfo st), snd (stepInfo st)) | st <- dSteps d ]
    pos = M.fromList [ (n, i) | (i, (n, _, _)) <- zip [0 :: Int ..] steps ]
    mname = M.fromList [ (n, m) | (n, "method", m) <- steps ]
    kids = M.fromList [ (k, (chModule c, annOf (chStmts c), chOrder c)) | (k, c) <- M.toList (dChildren d), validKid c ]
    validKid c = case unwords (words (chSched c)) of { ('[' : r) -> not (null r) && last r == ']'; _ -> False }
    expC = S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- dExportedC d ])
    expO = M.fromList [ ((a, b), r) | (a, r, b) <- dExportedSB d ]
    exc = S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- dDisjoint d ++ S.toList (dBlocks d) ])
    (groups, gpairs) = suffixGroups mel
    grpOf = M.fromListWith S.union [ (x, S.singleton g) | (g, mem) <- zip [0 :: Int ..] groups, x <- mem ]
    excMe = S.difference (S.fromList (concat [ [(a, b), (b, a)] | (a, b) <- gpairs ])) exc
    wf n = case [ x | x <- ["WILL_FIRE_" ++ n, "CAN_FIRE_" ++ n, "EN_" ++ fromMaybe "" (M.findWithDefault Nothing n mname)]
                    , M.member x (sdDefs dx) ] of
        (x : _) -> EId x
        [] -> case M.findWithDefault Nothing n mname of
            Just m | not (null m) -> EId ("EN_" ++ m)
            _ -> EId ("CAN_FIRE_" ++ n)
    calls = M.fromListWith (flip (++))
        [ ((i, m), [(n, c, map canonExpr as)])
        | (n, k, _) <- steps, k `elem` ["rule", "method"], ACallA i m c as@(_ : _) <- maybe [] snd (omLookup n (apRules ap)) ]
    msteps = [ n | (n, Just m) <- M.toList mname, not (null m) ]
    methSteps = M.fromListWith (++) [ (m, [n]) | n <- msteps, Just (Just m) <- [M.lookup n mname] ]
    adjX = M.fromListWith S.union ([ (a, S.singleton b) | (a, b) <- S.toList exc ]
                                   ++ [ (a, S.singleton b) | (ma, mb) <- S.toList expC, a <- M.findWithDefault [] ma methSteps
                                                           , b <- M.findWithDefault [] mb methSteps, a /= b ])
    adjM = M.fromListWith S.union [ (a, S.singleton b) | (a, b) <- S.toList excMe ]
    adjOf n me = if me then S.union (M.findWithDefault S.empty n adjX) (M.findWithDefault S.empty n adjM)
                 else M.findWithDefault S.empty n adjX
    excluded a b = S.member b (adjOf a True) || not (S.null (S.intersection (M.findWithDefault S.empty a grpOf) (M.findWithDefault S.empty b grpOf)))
    -- the conflict_free lines: names by the line's WILL_FIRE signals, whas read as the writer's WILL_FIRE
    cfaRaw = [ (rn, S.fromList (cfNames rn c), ex) | ((rn, _, c), ex) <- zip cfLines cfExprs ]
    cfLines = [ x | x@(_, _, c) <- clines, Right Nothing <- [meParse c] ]
    cfExprs = [ either (const (ELit "1'd1")) (cfResolveW) (aexprE (apInsts ap) c) | (_, _, c) <- cfLines ]
    cfResolveW = resolveW
    resolveW e = res e
      where
        writers = M.fromListWith (flip (++)) [ (i, [(rn, c, as)]) | (rn, (_, acts)) <- omToList (apRules ap), ACallA i "wset" c as <- acts ]
        true c = case c of { Nothing -> True; Just (ELit s) -> s `elem` ["1'd1", "1'b1"]; _ -> False }
        res x = case x of
            ECall i mt [] | mt `elem` ["wget", "whas"] -> case M.findWithDefault [] i writers of
                [(rn, c, a : _)] | true c -> if mt == "wget" then a else EId ("WILL_FIRE_" ++ rn)
                _ -> x
            ECall i mt as -> ECall i mt (map res as)
            EIf a b c -> EIf (res a) (res b) (res c)
            ECase y arms dd -> ECase (res y) [ (res l, res v) | (l, v) <- arms ] (fmap res dd)
            ESel a b -> ESel (res a) (res b)
            EApp h as -> EApp (res h) (map res as)
            EBin o a b -> EBin o (res a) (res b)
            EUn o a -> EUn o (res a)
            ETup xs -> ETup (map res xs)
            _ -> x
    cfAsm = do
        ls <- forM cfaRaw $ \(rn, _, ex) -> do { a <- encB (wf rn); b <- encB ex; x <- encAnd [a, b]; return (negate x) }
        asm <- mkAssume ls
        return (asm, IM.fromList (zip [0 ..] [ nm | (_, nm, _) <- cfaRaw ]))
    query mk names = query0 mk cfAsm names

    mayBoth (na, ca, _) (nb, cb, _) = query (do
        a <- encB (wf na)
        b <- encB (wf nb)
        x <- maybe (return []) (fmap pure . encB) ca
        y <- maybe (return []) (fmap pure . encB) cb
        return ([a, b] ++ x ++ y)) (S.fromList [na, nb])
    twoCall cl us me cf = query (do
        let uset = S.fromList us
        ents <- forM cl $ \(n, c, _) -> do
            w <- encB (wf n)
            cc <- maybe (return []) (fmap pure . encB) c
            l <- encAnd (w : cc)
            return (n, l)
        ys <- fmap M.fromList $ forM us $ \n -> do { y <- encOr [ l | (n', l) <- ents, n' == n ]; return (n, y) }
        ex <- fmap concat $ forM us $ \na -> forM [ nb | nb <- S.toList (adjOf na me), S.member nb uset, pos M.! na < pos M.! nb ] $ \nb ->
            negate <$> encAnd [ys M.! na, ys M.! nb]
        gr <- if not me then return [] else fmap concat $ forM (S.toAscList (S.unions [ M.findWithDefault S.empty n grpOf | n <- us ])) $ \g -> do
            (_, cs) <- foldM (\(s1, acc) n -> if S.member g (M.findWithDefault S.empty n grpOf)
                                then do { x <- encAnd [s1, ys M.! n]; s1' <- encOr [s1, ys M.! n]; return (s1', acc ++ [negate x]) }
                                else return (s1, acc)) (-1, []) us
            return cs
        (_, t) <- foldM (\(s, t) n -> do { a <- encAnd [s, ys M.! n]; t' <- encOr [t, a]; s' <- encOr [s, ys M.! n]; return (s', t') }) (-1, -1) us
        return (ex ++ gr ++ [t])) (if cf then S.fromList us else S.empty)
    portClass (kind, ann, ports) m synth bvi
        | synth = "S"
        | otherwise = case M.lookup (m, m) ann of
            Just "SBR" -> if not bvi && S.member kind prims && S.member (kind, m) decided && lcKey then "L" else "U"
            Just r | r `elem` ["CF", "SB"] -> "CF"
            _ -> "X"
      where lcKey = M.lookup (m, m) ann == Just "SBR" && all (\q -> M.lookup (m, q) ann /= Just "SB") [ q | q <- ports, q /= m ]
    orderOk na nb =
        let (first, second) = if pos M.! na < pos M.! nb then (na, nb) else (nb, na)
            fm = M.findWithDefault Nothing first mname
            sm = M.findWithDefault Nothing second mname
            has = maybe False (not . null)
        in if has fm && has sm then M.lookup (fromMaybe "" fm, fromMaybe "" sm) expO == Just "SBR"
           else not (has sm && not (has fm))
    pr a b = if pos M.! a < pos M.! b then a ++ ", " ++ b else b ++ ", " ++ a
    run = do
        ncalls <- newIORef (0 :: Int)
        let counted act = modifyIORef' ncalls (+ 1) >> act
        res <- forM (M.toList calls) $ \(port@(pinst, pmeth), cl) -> case M.lookup pinst kids of
            Nothing -> return Nothing
            Just kid@(kkind, _, _) -> do
                let us = sortOn (pos M.!) (S.toList (S.fromList [ n | (n, _, _) <- cl ]))
                    uset = S.fromList us
                if length us < 2 then return Nothing else do
                    let memG = M.fromListWith S.union [ (g, S.singleton n) | n <- us, g <- S.toList (M.findWithDefault S.empty n grpOf) ]
                        nExcl n = S.size (S.delete n (S.union (S.unions [ memG M.! g | g <- S.toList (M.findWithDefault S.empty n grpOf) ])
                                                              (S.intersection (adjOf n True) uset)))
                        free = any (\n -> nExcl n < length us - 1) us
                        pc = portClass kid pmeth (S.member pinst (dvSynth dv)) (S.member pinst (dvBvi dv))
                    r <- counted (twoCall cl us True True)
                    case r of
                        Unsat -> do
                            relyMe <- if any (\a -> not (S.null (S.intersection (M.findWithDefault S.empty a adjM) uset))
                                                    || any (\g -> S.size (memG M.! g) > 1) (S.toList (M.findWithDefault S.empty a grpOf))) us
                                      then (/= Unsat) <$> counted (twoCall cl us False True) else return False
                            relyCf <- if any (\(_, nm, _) -> not (S.null (S.intersection nm uset))) cfaRaw
                                      then (/= Unsat) <$> counted (twoCall cl us True False) else return False
                            return (Just (port, Nothing, relyMe, relyCf))
                        _ | not free -> return (Just (port, Nothing, False, False))
                        _ -> do
                            let pairs = [ (a, b) | (i, a@(na, _, _)) <- zip [0 :: Int ..] cl, b@(nb, _, _) <- drop (i + 1) cl, na /= nb, not (excluded na nb) ]
                            wit <- if r == Unknown then return (head (pairs ++ [(head cl, head cl)])) else do
                                found <- foldM (\acc (a, b) -> if isJust acc then return acc else do
                                                    v <- mayBoth a b
                                                    return (if v /= Unsat then Just (a, b) else Nothing)) Nothing pairs
                                return (fromMaybe (head (pairs ++ [(head cl, head cl)])) found)
                            let ((xn, _, xa), (yn, _, ya)) = wit
                            if pc == "L" then do
                                let idxd = zip [0 :: Int ..] cl
                                    mclI = [ (i, c) | (i, c@(n, _, _)) <- idxd, isJust (M.findWithDefault Nothing n mname) ]
                                    bad = dedupPairs [ ((ia, a), (ib, b)) | (ia, a@(na, _, _)) <- mclI, (ib, b@(nb, _, _)) <- idxd
                                                                          , na /= nb, not (excluded na nb), not (orderOk na nb) ]
                                badsat <- fmap concat $ forM bad $ \((_, a), (_, b)) -> do
                                    v <- counted (mayBoth a b)
                                    return [ (a, b) | v /= Unsat ]
                                let ln = "L " ++ pinst ++ "." ++ pmeth ++ " [" ++ kkind ++ "] callers " ++ show (length us)
                                         ++ (if S.member (kkind, pmeth) wired then "" else " (law proved, not wired)")
                                         ++ (if r == Unknown then " (timeout: unknown, taken as may both call)" else "")
                                    ord' = [ "L!order " ++ pinst ++ "." ++ pmeth ++ " [" ++ kkind ++ "]: " ++ pr na nb | ((na, _, _), (nb, _, _)) <- badsat ]
                                    blk = [ "L!order " ++ pinst ++ "." ++ pmeth ++ " [" ++ kkind ++ "] " ++ pr na nb | ((na, _, _), (nb, _, _)) <- badsat ]
                                return (Just (port, Just ("L", length us, ln : ord', length badsat, blk), False, False))
                            else do
                                let c | r == Unknown = "?"
                                      | pc `elem` ["S", "U", "X"] = pc
                                      | otherwise = if xa == ya then (if S.member (kkind, pmeth) notIdempotent then "I-fail" else "I") else "X"
                                    ln = c ++ " " ++ pinst ++ "." ++ pmeth ++ " [" ++ kkind ++ "]" ++ (if xa == ya then " (identical call)" else "")
                                    blk = c ++ " " ++ pinst ++ "." ++ pmeth ++ " [" ++ kkind ++ "] " ++ pr xn yn ++ (if xa == ya then " (identical call)" else "")
                                return (Just (port, Just (c, length us, [ln], 0, [blk]), False, False))
        n <- readIORef ncalls
        let rs = [ x | Just x <- res ]
            nport = length rs
            classes = M.fromListWith (+) (concat [ (c, 1 :: Int) : [ ("L!order", k) | k > 0 ] | (_, Just (c, _, _, k, _), _, _) <- rs ])
            byc = M.filter (> 0) classes
            lcMax = maximum (0 : [ u | (_, Just ("L", u, _, _, _), _, _) <- rs ])
            relyMe = length [ () | (_, _, True, _) <- rs ]
            relyCf = length [ () | (_, _, _, True) <- rs ]
            cls = intercalate ", " [ k ++ " " ++ show v | (k, v) <- M.toList byc ]
            blocked = sum [ v | (k, v) <- M.toList byc, k /= "L" ]
            verdict | not (M.null byc) && blocked == 0 = "last caller admits all"
                    | M.null byc = "no shadowing"
                    | otherwise = "still blocked: " ++ show blocked
            hdr = name ++ " ports with two callers " ++ show nport ++ ", solver calls " ++ show n ++ ", shadowing ports "
                  ++ show (sum (M.elems byc)) ++ " [" ++ (if null cls then "-" else cls) ++ "]; lc callers max " ++ show lcMax
                  ++ "; ports relying on mutually_exclusive " ++ show relyMe ++ ", conflict_free " ++ show relyCf ++ "; " ++ verdict
        return (ShadowOut (hdr : concat [ ls | (_, Just (_, _, ls, _, _), _, _) <- rs ]) True (M.toList byc) lcMax (relyMe, relyCf)
                          (concat [ b | (_, Just (_, _, _, _, b), _, _) <- rs ]))
    dedupPairs = go S.empty
      where go _ [] = []
            go seen (p@((ia, _), (ib, _)) : rest) = let k = (min ia ib, max ia ib)
                                                    in if S.member k seen then go seen rest else p : go (S.insert k seen) rest
