-- | A synthesized module's exported interface (design.md §3.1): its methods in its file's order
-- and their relations, as its own file states them and its parents' kinds take them.
module Lean.Sched.Export
    ( XMeth(..), Export(exModule, exNS, exMeths), SynthKid(..), mkExport, exportOf, enableStop, forceExport, exMethAR, exPrecB, exSbrB, exConfB
    , synthKind, viewCheck, viewKind, viewInHigh, isSynthKind, capital, portFn
    ) where

import ASyntax (ADef(..), AExpr(..), APackage(apkg_interface), AIFace(..), aIfaceArgs, isTrue)
import Data.Array (Array, listArray, (!))
import Data.Array.Unboxed (UArray, accumArray)
import qualified Data.Array.Unboxed as U ((!))
import Data.Bits (testBit, (.|.))
import Data.Char (toUpper)
import Data.List (isPrefixOf)
import Data.Word (Word8)
import Id (getIdString)
import Pragma (PProp, isEnWhenRdy)
import VModInfo (VFieldInfo(Method, vf_enable), getVNameString)
import qualified Data.Map.Strict as M (Map, findWithDefault, fromList, lookup, member, toList)
import qualified Data.Set as S (fromList)

import Lean.Sched.Kinds (KindDef(..), PM(..), codedKind, schedCodes)
import Lean.Sched.Types (Child(chMethods, chOrder, chStmts), Design(dModule, dSteps, dMethodSteps, dReadyDef,
    dSeqDefs, dExportedCF, dExportedSB), Meth(mtKind, mtConst, mtCopies, mtInHigh), MethKind(ValueMethod), Port(ptArgs),
    Step(stKind, stMethod), StepKind(ValueMethodStep, ActionMethodStep))

-- | An exported method: value method (no enable, one argument wire), with arguments, always ready.
data XMeth = XMeth { xmName :: String, xmValue :: Bool, xmArg :: Bool, xmAR :: Bool }
    deriving (Eq, Show)

-- | Methods in the file's order (its @Fin nM@) and the relations of distinct ones by index pair (bit 0:
-- @a SB b@ or @a SBR b@, prec; bit 1: SBR either way, sbr; bit 2: C or no statement, conf).
data Export = Export
    { exModule :: String, exNS :: String, exMeths :: [XMeth]
    , exN :: Int, exMethA :: Array Int XMeth, exIx :: M.Map String Int, exRel :: UArray Int Word8 }

mkExport :: String -> [XMeth] -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> (Int -> Int -> Bool) -> Export
mkExport name ms prec sbr conf = Export name (capital name) ms n (listArray (0, n - 1) ms) (M.fromList (zip (map xmName ms) [0 ..]))
    (accumArray (.|.) 0 (0, n * n - 1) [ (a * n + b, code a b) | a <- [0 .. n - 1], b <- [0 .. n - 1], a /= b ])
  where
    n = length ms
    code a b = (if prec a b then 1 else 0) .|. (if sbr a b then 2 else 0) .|. (if conf a b then 4 else 0)

-- | An export evaluated in full, so that it holds nothing of the design it came from.
forceExport :: Export -> ()
forceExport ex = sum [ length (xmName x) + fromEnum (xmValue x) + fromEnum (xmArg x) + fromEnum (xmAR x) | x <- exMeths ex ]
    `seq` exMethA ex `seq` exIx ex `seq` exRel ex `seq` ()

-- | A synthesized child in a parent: its kind, its export, why it is an assumed leaf (Nothing: path A),
-- and why the parent cannot state it at all (a method not fired by its enable: the parent stops).
data SynthKid = SynthKid { skKind :: String, skExport :: Export, skOff :: Maybe String, skStop :: Maybe String
                         , skAssume :: [String] }  -- ^ skAssume: what its file assumes (path A)

-- | Relations by method index; a method's own: action methods C with themselves (§3).
exPrecB, exSbrB, exConfB :: Export -> Int -> Int -> Bool
exPrecB ex a b = a /= b && testBit (relOf ex a b) 0
exSbrB ex a b = a /= b && testBit (relOf ex a b) 1
exConfB ex a b
    | a == b = not (xmValue (exMethA ex ! a))
    | otherwise = testBit (relOf ex a b) 2

relOf :: Export -> Int -> Int -> Word8
relOf ex a b = exRel ex U.! (a * exN ex + b)

-- | Whether the named method is exported always ready.
exMethAR :: Export -> String -> Bool
exMethAR ex m = maybe False (xmAR . (exMethA ex !)) (M.lookup m (exIx ex))

-- | The export of a module from its own design: every interface method, each one step, an action method
-- firing on its enable (a value method with arguments is refused: its result would read an input).
exportOf :: [PProp] -> Design -> APackage -> Either String Export
exportOf pps d apkg = do
    let msteps = [ (m, st) | st <- dSteps d, stKind st `elem` [ActionMethodStep, ValueMethodStep], Just m <- [stMethod st] ]
        names = map fst msteps
    case [ m | (m, ss) <- M.toList (dMethodSteps d), length ss > 1 ] of
        (m : _) -> Left ("method " ++ m ++ " has several steps (bsc split it): not exported yet")
        [] -> return ()
    maybe (return ()) Left (enableStop pps d apkg)
    let ifc = apkg_interface apkg
        args = M.fromList [ (getIdString (aif_name f), not (null (aIfaceArgs f))) | f <- ifc ]
        meths = [ XMeth m (stKind st == ValueMethodStep) (M.findWithDefault False m args) (alwaysReady m)
                | (m, st) <- msteps ]
    case [ xmName x | x <- meths, xmValue x, xmArg x ] of
        (m : _) -> Left ("value method " ++ m ++ " has arguments: not exported yet (its result reads an input)")
        [] -> return ()
    let ix = M.fromList (zip names [0 :: Int ..])
        n = length meths
        pairsOf ps = accumArray (\_ x -> x) False (0, n * n - 1) [ (a * n + b, True) | (a, b) <- ps ] :: UArray Int Bool
        idx x = M.lookup x ix
        cf = pairsOf (concat [ [(a, b), (b, a)] | (x, y) <- dExportedCF d, Just a <- [idx x], Just b <- [idx y] ])
        sb = pairsOf [ (a, b) | (x, r, y) <- dExportedSB d, r `elem` ["SB", "SBR"], Just a <- [idx x], Just b <- [idx y] ]
        sbr = pairsOf (concat [ [(a, b), (b, a)] | (x, "SBR", y) <- dExportedSB d, Just a <- [idx x], Just b <- [idx y] ])
        at arr a b = arr U.! (a * n + b)
        related a b = at cf a b || at sb a b || at sb b a
    return (mkExport (dModule d) meths (at sb) (at sbr) (\a b -> not (related a b)))
  where
    -- bsc's wrapper assumes a constant-true ready signal is 1 (bsc.hs true_ifc_ids); always_ready drops it;
    -- the guard must be the constant too, as the model states it
    alwaysReady m = maybe True (constTrue (8 :: Int)) (M.lookup m (dReadyDef d))
                    && (not (M.member ("CAN_FIRE_" ++ m) (dSeqDefs d)) || constTrue (8 :: Int) ("CAN_FIRE_" ++ m))
    constTrue 0 _ = False
    constTrue k nm = case M.lookup nm (dSeqDefs d) of
        Just (ADef _ _ e _) -> case e of
            ASDef _ i -> constTrue (k - 1) (getIdString i)
            _ -> isTrue e
        Nothing -> False

-- | Why some action method of a module does not fire exactly on its parent's enable, so that no
-- parent can state the module (always_enabled runs it every cycle, enabled_when_ready on its RDY).
enableStop :: [PProp] -> Design -> APackage -> Maybe String
enableStop pps d apkg = case ([ m | (m, f) <- acts, isEnWhenRdy pps (aif_name f) ], [ m | (m, f) <- acts, not (firesOnEn m (aif_fieldinfo f)) ]) of
    (m : _, _) -> Just ("method " ++ m ++ " is always_enabled or enabled_when_ready: it does not fire on its parent's enable"
                        ++ " (a parent's alw obligation is not emitted yet)")
    (_, m : _) -> Just ("WILL_FIRE_" ++ m ++ " is not method " ++ m ++ "'s enable")
    _ -> Nothing
  where
    acts = [ (getIdString (aif_name f), f) | f <- apkg_interface apkg, isAct f ]
    isAct f = case f of { AIAction {} -> True; AIActionValue {} -> True; _ -> False }
    -- WILL_FIRE_m is the method's enable port (bsc's own form; an always_enabled method's is 1)
    firesOnEn m fi = case (fi, M.lookup ("WILL_FIRE_" ++ m) (dSeqDefs d)) of
        (Method { vf_enable = Just (en, _) }, Just (ADef _ _ (ASPort _ i) _)) -> getIdString i == getVNameString en
        _ -> False

-- | A module's Lean namespace and file stem: its name capitalised.
capital :: String -> String
capital (c : cs) = toUpper c : cs
capital [] = "Module"

isSynthKind :: String -> Bool
isSynthKind = ("syn" `isPrefixOf`)

-- | The kind of a synthesized child on path A: its file's interface (@NS.X@), ports its methods.
synthKind :: String -> Export -> KindDef
synthKind name ex = codedKind name (Just (exNS ex ++ ".X")) (Just ("Fin " ++ exNS ex ++ ".nM")) (map (PS . xmName) ms)
    (Just [ "(" ++ exNS ex ++ ".mM " ++ show i ++ ")" | i <- [0 .. length ms - 1] ])
    (exPrecB ex, exSbrB ex, exConfB ex) (meth xmValue, meth (not . xmValue), meth xmAR, meth xmArg)
  where
    ms = exMeths ex
    meth f i = f (exMethA ex ! i)

-- | The parent's view of a child (bsc's schedule in the parent's .ba) against the child's export:
-- the same methods, kinds and always-ready methods, and the same relations but for a method's own.
viewCheck :: Export -> Child -> Maybe String
viewCheck ex c
    | S.fromList names /= S.fromList pnames = Just ("its methods differ from " ++ exModule ex ++ ".ba's")
    | or [ maybe True ((/= xmValue x) . (== ValueMethod) . mtKind) (M.lookup (xmName x) (chMethods c)) | x <- ms ] =
        Just ("a method's kind differs from " ++ exModule ex ++ ".ba's")
    | (m : _) <- [ xmName x | x <- ms, viewAR (xmName x) /= xmAR x ] =
        Just ("its always-ready methods differ from " ++ exModule ex ++ ".ba's (" ++ m ++ ")")
    | (b : _) <- bad = Just ("its schedule differs from " ++ exModule ex ++ ".ba's (" ++ b ++ ")")
    | otherwise = Nothing
  where
    ms = exMeths ex
    names = map xmName ms
    pnames = [ m | m <- chOrder c, not ("RDY_" `isPrefixOf` m && M.member (drop 4 m) (chMethods c)) ]
    -- as viewKind: m is always ready unless its RDY_m exists and is not bsc's constant
    viewAR m = maybe True mtConst (M.lookup ("RDY_" ++ m) (chMethods c))
    (pp, ps, pc) = maybe (error "schedlean: a view kind without code relations") id (kdCodeRel (viewKind "view" c))
    vix = M.fromList (zip (chOrder c) [0 :: Int ..])
    n = length ms
    ca = listArray (0, n - 1) [ M.findWithDefault (-1) (xmName x) vix | x <- ms ] :: Array Int Int
    bad = [ xmName (exMethA ex ! a) ++ ", " ++ xmName (exMethA ex ! b)
          | a <- [0 .. n - 1], b <- [0 .. n - 1], a /= b
          , let (p, q) = (ca ! a, ca ! b)
          , pp p q /= exPrecB ex a b || (ps p q || ps q p) /= exSbrB ex a b || pc p q /= exConfB ex a b ]

-- | An assumed synthesized leaf's kind, from the parent's view: every method a port, RDY_m
-- (a value port) aliased to m's ready output; action methods C with themselves.
viewKind :: String -> Child -> KindDef
viewKind name c = codedKind name (Just ("sX_" ++ name)) (Just ("Fin " ++ show n)) (map PS ports)
    (Just [ "(" ++ portFn name ++ " " ++ show i ++ ")" | i <- [0 .. n - 1] ])
    (prec, sbr, conf) (not . (actA U.!), (actA U.!), (arA U.!), (hargA U.!))
  where
    ports = chOrder c
    n = length ports
    ua f = accumArray (\_ x -> x) False (0, n - 1) (zip [0 ..] (map f ports)) :: UArray Int Bool
    meth p = M.lookup p (chMethods c)
    actA = ua (\p -> maybe False ((/= ValueMethod) . mtKind) (meth p))
    hargA = ua (\p -> maybe False (any (not . null . ptArgs) . mtCopies) (meth p))
    isRdy p = "RDY_" `isPrefixOf` p && M.member (drop 4 p) (chMethods c)
    rdyA = ua isRdy
    -- m is ready when its RDY_m exists and is not bsc's constant
    arA = ua (\m -> isRdy m || maybe True mtConst (M.lookup ("RDY_" ++ m) (chMethods c)))
    rel = schedCodes ports (chStmts c)
    r a b = rel U.! (a * n + b)
    rdy2 a b = rdyA U.! a || rdyA U.! b
    prec a b = a /= b && not (rdy2 a b) && r a b `elem` [2, 3]
    sbr a b = a /= b && not (rdy2 a b) && r a b == 3
    conf a b
        | a == b = actA U.! a
        | rdy2 a b = False
        | otherwise = r a b == 4 || r b a == 4 || (r a b == 0 && r b a == 0)

-- | An assumed synthesized leaf's port by index (@pSynK i@), as the parent's file defines it.
portFn :: String -> String
portFn name = 'p' : capital name

-- | The parent's view of a child's always_enabled methods (an inhigh enable: bsc runs them every cycle).
viewInHigh :: Child -> [String]
viewInHigh c = [ m | m <- chOrder c, Just mt <- [M.lookup m (chMethods c)], mtInHigh mt ]
