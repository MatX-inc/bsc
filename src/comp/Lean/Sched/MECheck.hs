-- | bsc's run-time check lines read by their structure (mecheck.py).  A
-- mutually_exclusive check is @aAnd (aOrs WF as) (aOrs WF bs)@ (ASchedule's
-- mkMEAssump): no rule of @as@ fires with a rule of @bs@.  Any other line of
-- WILL_FIRE signals is an error; every other line is a conflict_free check.
module Lean.Sched.MECheck
    ( meParse, mePairs, samePairs, suffixGroups, render, cfNames, cfResolve
    ) where

import ASyntax (AExpr(ASParam, APrim, AMethCall, ANoInlineFunCall, AFunCall, ATuple, ATupleSel, ASDef, ASPort))
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (isPrefixOf)
import Id (getIdString)
import PPrint (ppString)
import Prim (PrimOp(..))
import qualified Data.Map.Strict as M (Map, findWithDefault, member, fromListWith)
import qualified Data.Set as S (member, empty, fromList, insert, toAscList, union)

import Lean.Sched.Py (omToList)
import Lean.Sched.Tree (E(..), Act(..), APkg(..))

-- | The rule a WILL_FIRE signal names (WILL_FIRE_ and letters, digits, _ or $).
wfName :: AExpr -> Maybe String
wfName e = case e of
    ASDef _ i -> nameOf (getIdString i)
    ASPort _ i -> nameOf (getIdString i)
    _ -> Nothing
  where nameOf s | "WILL_FIRE_" `isPrefixOf` s, r@(_ : _) <- drop 10 s, all ok r = Just r
                 | otherwise = Nothing
        ok c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_' || c == '$'

-- | Only WILL_FIRE signals under && and ||, at least two of them.
wfOnly :: AExpr -> Bool
wfOnly e0 = go e0 && count e0 >= (2 :: Int)
  where go e = case e of
            APrim _ _ o xs | o `elem` [PrimBAnd, PrimBOr] -> all go xs
            _ -> wfName e /= Nothing
        count e = case e of
            APrim _ _ _ xs -> sum (map count xs)
            _ -> 1

render :: [String] -> [String] -> String
render as bs = side as ++ " && " ++ side bs
  where side [x] = "WILL_FIRE_" ++ x
        side xs = "(" ++ foldr1 (\a b -> a ++ " || " ++ b) (map ("WILL_FIRE_" ++) xs) ++ ")"

-- | (as, bs) for a mutually_exclusive line, Nothing for a conflict_free one.
meParse :: AExpr -> Either String (Maybe ([String], [String]))
meParse e
    | not (wfOnly e) = Right Nothing
    | APrim _ _ PrimBAnd [a, b] <- e, Just as <- side a, Just bs <- side b = Right (Just (as, bs))
    | otherwise = Left ("a run-time check line of WILL_FIRE signals in an unknown form: " ++ take 120 (ppString e))
  where side x = case wfName x of
            Just n -> Just [n]
            Nothing -> case x of
                APrim _ _ PrimBOr xs@(_ : _ : _) -> mapM wfName xs
                _ -> Nothing

mePairs :: ([String], [String]) -> [(String, String)]
mePairs (as, bs) = [ (a, b) | a <- as, b <- bs ]

-- | Generator check: the lines' exclusions are bsc's mutually_exclusive pairs.
samePairs :: [([String], [String])] -> [(String, String)] -> Bool
samePairs ls theirs = S.fromList (map un (concatMap mePairs ls)) == S.fromList (map un theirs)
  where un (a, b) = S.fromList [a, b]

-- | bsc's groups: one-head lines chained so that each list is the suffix of one
-- sequence after its head; and the pairs of the other lines.
suffixGroups :: [([String], [String])] -> ([[String]], [(String, String)])
suffixGroups ls = (reverse groups, multi ++ rest)
  where
    stars = [ (h, bs) | ([h], bs) <- ls ]
    multi = concat [ mePairs l | l@(as, _) <- ls, length as /= 1 ]
    starA = zip [0 :: Int ..] stars
    bylst = M.fromListWith (flip (++)) [ (lst, [c]) | (c, (_, lst)) <- starA ]
    (used, groups) = foldl' grow (S.empty, []) starA
    grow (u, gs) (c0, (h0, l0))
        | length l0 /= 1 || S.member c0 u = (u, gs)
        | otherwise =
            let chase sq chain = case [ c | c <- M.findWithDefault [] sq bylst, not (S.member c u), c `notElem` chain ] of
                    [] -> (sq, chain)
                    (c : _) -> chase (fst (stars !! c) : sq) (chain ++ [c])
                (sq, chain) = chase (h0 : l0) [c0]
            in if length sq > 2 then (S.union u (S.fromList chain), sq : gs) else (u, gs)
    rest = concat [ [ (h, b) | b <- lst ] | (c, (h, lst)) <- starA, not (S.member c used) ]

-- | The rules a conflict_free line names: its WILL_FIRE signals, and its own rule.
cfNames :: String -> AExpr -> [String]
cfNames rule e = S.toAscList (S.insert rule (S.fromList (go e)))
  where go x = case x of
            APrim _ _ _ xs -> concatMap go xs
            AMethCall _ _ _ xs -> concatMap go xs
            ANoInlineFunCall _ _ _ xs -> concatMap go xs
            AFunCall _ _ _ _ xs -> concatMap go xs
            ATuple _ xs -> concatMap go xs
            ATupleSel _ y _ -> go y
            ASDef _ i -> nm (getIdString i)
            ASPort _ i -> nm (getIdString i)
            ASParam _ i -> nm (getIdString i)
            _ -> []
        nm s = case breakOn "WILL_FIRE_" s of { Just r | not (null r) -> [r]; _ -> [] }
        breakOn p s | p `isPrefixOf` s = Just (drop (length p) s)
                    | (_ : r) <- s = breakOn p r
                    | otherwise = Nothing

-- | A conflict_free line as the gate assumes it: each instrumentation wire's
-- wget read as its one unconditional writer's argument, whas as its WILL_FIRE.
cfResolve :: APkg -> M.Map String a -> E -> E
cfResolve ap defs = res
  where
    writers = M.fromListWith (flip (++)) [ (i, [(rn, c, as)]) | (rn, (_, acts)) <- omToList (apRules ap)
                                                              , ACallA i "wset" c as <- acts ]
    true c = case c of { Nothing -> True; Just (ELit s) -> s `elem` ["1'd1", "1'b1"]; _ -> False }
    wfe n = let x = "WILL_FIRE_" ++ n in if M.member x defs then EId x else EId ("CAN_FIRE_" ++ n)
    res e = case e of
        ECall i m [] | m `elem` ["wget", "whas"] -> case M.findWithDefault [] i writers of
            [(rn, c, a : _)] | true c -> if m == "wget" then a else wfe rn
            _ -> e
        ECall i m as -> ECall i m (map res as)
        EIf a b c -> EIf (res a) (res b) (res c)
        ECase x arms d -> ECase (res x) [ (res l, res v) | (l, v) <- arms ] (fmap res d)
        ESel a b -> ESel (res a) (res b)
        EApp h as -> EApp (res h) (map res as)
        EBin o a b -> EBin o (res a) (res b)
        EUn o a -> EUn o (res a)
        ETup xs -> ETup (map res xs)
        _ -> e
