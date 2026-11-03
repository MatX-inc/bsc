-- | The prototype's trees (bapkg.py's tuples): expressions, rule actions and the
-- package view, with Python's repr and order, and its tokenizer's word classes.
module Lean.Sched.Tree
    ( E(..), Act(..), APkg(..), eKids, reprE, reprAct, Tok, tokenize, binPrec, classifyAct
    ) where

import Data.Char (isAsciiLower, isAsciiUpper, isDigit, isHexDigit)
import Data.List (isPrefixOf)
import qualified Data.Map.Strict as M (Map, fromList)
import qualified Data.Set as S (Set, member)

import Lean.Sched.Py (isPySpace, pyReprStr, OMap)

-- | bapkg's expression tuples.
data E = EId String | ELit String | ECall String String [E] | EIf E E E
       | ECase E [(E, E)] (Maybe E) | ESel E E | EApp E [E] | EBin String E E
       | EUn String E | ETup [E]
    deriving (Eq, Show)

-- Python orders tuples by their first field (the tag) and then the rest.
instance Ord E where
    compare a b = compare (key a) (key b)
      where key x = (tag x, x')
              where x' = map Left (strs x) ++ map Right (kids x)
            tag x = case x of
                EId _ -> "id"; ELit _ -> "lit"; ECall {} -> "call"; EIf {} -> "if"; ECase {} -> "case"
                ESel {} -> "sel"; EApp {} -> "app"; EBin {} -> "bin"; EUn {} -> "un"; ETup _ -> "tup"
            strs x = case x of
                EId s -> [s]; ELit s -> [s]; ECall i m _ -> [i, m]; EBin o _ _ -> [o]; EUn o _ -> [o]; _ -> []
            kids = eKids

-- | The subexpressions, in tuple order.
eKids :: E -> [E]
eKids x = case x of
    ECall _ _ as -> as
    EIf c a b -> [c, a, b]
    ECase i arms d -> i : concat [ [l, v] | (l, v) <- arms ] ++ maybe [] pure d
    ESel a i -> [a, i]
    EApp h as -> h : as
    EBin _ a b -> [a, b]
    EUn _ a -> [a]
    ETup xs -> xs
    _ -> []

-- | Python's repr of the tuple.
reprE :: E -> String
reprE x = case x of
    EId s -> t2 "id" (pyReprStr s)
    ELit s -> t2 "lit" (pyReprStr s)
    ECall i m as -> tup [pyReprStr "call", pyReprStr i, pyReprStr m, lst (map reprE as)]
    EIf c a b -> tup [pyReprStr "if", reprE c, reprE a, reprE b]
    ECase i arms d -> tup [pyReprStr "case", reprE i, lst [ tup [reprE l, reprE v] | (l, v) <- arms ], maybe "None" reprE d]
    ESel a i -> tup [pyReprStr "sel", reprE a, reprE i]
    EApp h as -> tup [pyReprStr "app", reprE h, lst (map reprE as)]
    EBin o a b -> tup [pyReprStr "bin", pyReprStr o, reprE a, reprE b]
    EUn o a -> tup [pyReprStr "un", pyReprStr o, reprE a]
    ETup xs -> tup [pyReprStr "tup", lst (map reprE xs)]
  where t2 t s = tup [pyReprStr t, s]
        tup xs = "(" ++ commas xs ++ ")"
        lst xs = "[" ++ commas xs ++ "]"
        commas = foldr1' (\a b -> a ++ ", " ++ b)
        foldr1' _ [] = ""
        foldr1' f ys = foldr1 f ys

-- | A rule action: a child call, a task (or other function) call, or anything else.
data Act = ACallA String String (Maybe E) [E] | ATask String (Maybe E) [E] | AOther String (Maybe E)
    deriving (Eq, Show)

reprAct :: Act -> String
reprAct a = case a of
    ACallA i m c as -> "('call', " ++ pyReprStr i ++ ", " ++ pyReprStr m ++ ", " ++ mc c ++ ", " ++ l as ++ ")"
    ATask n c as -> "('task', " ++ pyReprStr n ++ ", " ++ mc c ++ ", " ++ l as ++ ")"
    AOther s c -> "('other', " ++ pyReprStr s ++ ", " ++ mc c ++ ", [])"
  where mc = maybe "None" reprE
        l as = "[" ++ intercalate' (map reprE as) ++ "]"
        intercalate' [] = ""
        intercalate' xs = foldr1 (\p q -> p ++ ", " ++ q) xs

-- ---------------------------------------------------------------- tokens

type Tok = (String, String)

ops :: [String]
ops = [".<=", ".>=", ".<", ".>", "&&", "||", "==", "!=", "<=", ">=", "<<", ">>", "++", "->"]
      ++ map pure "-+*/%<>&|^!~()[]{},;?:="

tokenize :: String -> Either String [Tok]
tokenize = go
  where
    go [] = Right []
    go s@(c : cs)
        | isPySpace c = go (dropWhile isPySpace cs)
        | c == '"', Just (lit, rest) <- strLit cs "\"" = ((("str", lit)) :) <$> go rest
        | isDigit c, Just (lit, rest) <- numLit s = (("lit", lit) :) <$> go rest
        | idStart c = let (v, rest) = ident s in (("id", v) :) <$> go rest
        | (o : _) <- [ o | o <- ops, o `isPrefixOf` s ] = (("op", o) :) <$> go (drop (length o) s)
        | otherwise = Left ("cannot tokenize at " ++ pyReprStr (take 40 s))
    strLit ('"' : r) acc = Just (reverse ('"' : acc), r)
    strLit ('\\' : x : r) acc | x /= '\n' = strLit r (x : '\\' : acc)
    strLit ('\\' : _) _ = Nothing
    strLit (x : r) acc = strLit r (x : acc)
    strLit [] _ = Nothing
    idStart x = isAsciiLower x || isAsciiUpper x || x == '_' || x == '$'
    idChar x = idStart x || isDigit x || x == '\''
    ident s = let (a, r) = span idChar s
                  (b, r') = dots r
              in (a ++ b, r')
    dots ('.' : x : r) | idStart x = let (a, r1) = span idChar (x : r)
                                         (b, r2) = dots r1
                                     in ('.' : a ++ b, r2)
    dots r = ("", r)
    numLit s =
        let (ds, r) = span isDigit s
            sized = case r of
                ('\'' : r1) ->
                    let (sg, r2) = case r1 of { (x : y) | x `elem` "sS" -> ([x], y); _ -> ("", r1) }
                    in case r2 of
                        (b : r3) | b `elem` "bdhoBDHO" ->
                            let (v, r4) = span (`elem` "0123456789abcdefABCDEFxXzZ_?") r3
                            in if null v then Nothing else Just (ds ++ "'" ++ sg ++ [b] ++ v, r4)
                        _ -> Nothing
                _ -> Nothing
            hex = case s of
                ('0' : 'x' : r1) -> let (v, r2) = span isHexDigit r1
                                    in if null v then Nothing else Just ("0x" ++ v, r2)
                _ -> Nothing
            plain = case r of
                ('.' : r1) | (f@(_ : _), r2) <- span isDigit r1 -> Just (ds ++ "." ++ f, r2)
                _ -> Just (ds, r)
        in case sized of { Just x -> Just x; Nothing -> case hex of { Just x -> Just x; Nothing -> plain } }

binPrec :: M.Map String Int
binPrec = M.fromList
    [ ("||", 1), ("&&", 2), ("|", 3), ("^", 4), ("&", 5), ("==", 6), ("!=", 6)
    , ("<", 7), ("<=", 7), (">", 7), (">=", 7), (".<", 7), (".<=", 7), (".>", 7), (".>=", 7)
    , ("<<", 8), (">>", 8), ("++", 8), ("+", 9), ("-", 9), ("*", 10), ("/", 10), ("%", 10) ]

classifyAct :: S.Set String -> Maybe E -> E -> Act
classifyAct insts cond act = case act of
    ECall i m as -> ACallA i m cond as
    EApp (EId n) as
        | '.' `elem` n, (inst, _ : meth) <- break (== '.') n, inst `S.member` insts -> ACallA inst meth cond as
        | otherwise -> ATask n cond as
    EId n
        | '.' `elem` n, (inst, _ : meth) <- break (== '.') n, inst `S.member` insts -> ACallA inst meth cond []
        | otherwise -> ATask n cond []
    _ -> AOther (take 60 (reprE act)) cond

data APkg = APkg
    { apInsts   :: S.Set String
    , apDefs    :: OMap String E
    , apTypes   :: OMap String String
    , apRules   :: OMap String (E, [Act])
    , apIfcVals :: OMap String E
    , apMethArgs :: OMap String [String]
    , apErrors  :: [String]
    , apUsesMap :: OMap String [(String, [[String]])]
    , apRalloc  :: OMap String [(String, Integer)]
    , apGuarded :: OMap String Bool
    }

