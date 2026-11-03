-- | Helpers that reproduce the prototype's Python behaviour exactly where the
-- emitted text depends on it: insertion-ordered maps, repr, string splits.
module Lean.Sched.Py
    ( OMap, omEmpty, omInsert, omInsertWith, omLookup, omMember, omToList, omKeys, omElems
    , omFromList, omSize, omAdjust, omSetDefault
    , pyReprStr, strip, lstrip, rstrip, splitOn1, pySplit, pyWords, isPrefix, startsWith, endsWith
    , findSub, rfindSub, isPySpace
    ) where

import Data.Char (isSpace, ord)
import Data.List (isPrefixOf, isSuffixOf, isInfixOf, tails)
import Numeric (showHex)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, (!), empty, lookup, member, size, insert, adjust, insertWith)
import qualified Data.Sequence as Sq (Seq, empty, (|>))

-- | A Python dict: lookups by key, iteration in first-insertion order.
data OMap k v = OMap !(M.Map k v) !(Sq.Seq k)

omEmpty :: OMap k v
omEmpty = OMap M.empty Sq.empty

omInsert :: Ord k => k -> v -> OMap k v -> OMap k v
omInsert k v (OMap m o) = OMap (M.insert k v m) (if M.member k m then o else o Sq.|> k)

-- | @f new old@, as 'M.insertWith'.
omInsertWith :: Ord k => (v -> v -> v) -> k -> v -> OMap k v -> OMap k v
omInsertWith f k v (OMap m o) = OMap (M.insertWith f k v m) (if M.member k m then o else o Sq.|> k)

omAdjust :: Ord k => (v -> v) -> k -> OMap k v -> OMap k v
omAdjust f k (OMap m o) = OMap (M.adjust f k m) o

-- | Python's setdefault followed by an update of the value.
omSetDefault :: Ord k => k -> v -> (v -> v) -> OMap k v -> OMap k v
omSetDefault k d f om@(OMap m _) = case M.lookup k m of
    Just _  -> omAdjust f k om
    Nothing -> omInsert k (f d) om

omLookup :: Ord k => k -> OMap k v -> Maybe v
omLookup k (OMap m _) = M.lookup k m

omMember :: Ord k => k -> OMap k v -> Bool
omMember k (OMap m _) = M.member k m

omToList :: Ord k => OMap k v -> [(k, v)]
omToList (OMap m o) = [ (k, m M.! k) | k <- F.toList o ]

omKeys :: OMap k v -> [k]
omKeys (OMap _ o) = F.toList o

omElems :: Ord k => OMap k v -> [v]
omElems = map snd . omToList

omFromList :: Ord k => [(k, v)] -> OMap k v
omFromList = foldl (\m (k, v) -> omInsert k v m) omEmpty

omSize :: OMap k v -> Int
omSize (OMap m _) = M.size m

-- | Python's repr of a str.
pyReprStr :: String -> String
pyReprStr s = q : concatMap esc s ++ [q]
  where q = if '\'' `elem` s && '"' `notElem` s then '"' else '\''
        esc c | c == q = ['\\', c]
              | c == '\\' = "\\\\"
              | c == '\n' = "\\n"
              | c == '\t' = "\\t"
              | c == '\r' = "\\r"
              | n < 0x20 || n == 0x7f || (n >= 0x80 && n <= 0xa0) || n == 0xad = "\\x" ++ hex2 n
              | otherwise = [c]
          where n = ord c
        hex2 n = let h = showHex n "" in if length h < 2 then '0' : h else h

-- | Python's str.isspace on one character.
isPySpace :: Char -> Bool
isPySpace c = isSpace c || c `elem` "\x1c\x1d\x1e\x1f\x85"

strip, lstrip, rstrip :: String -> String
strip = rstrip . lstrip
lstrip = dropWhile isPySpace
rstrip = reverse . dropWhile isPySpace . reverse

-- | Python's s.split(sep, 1).
splitOn1 :: String -> String -> [String]
splitOn1 sep s = case findSub sep s of
    Nothing -> [s]
    Just i  -> [take i s, drop (i + length sep) s]

-- | Python's s.split(sep) with a non-empty separator.
pySplit :: String -> String -> [String]
pySplit sep s = case findSub sep s of
    Nothing -> [s]
    Just i  -> take i s : pySplit sep (drop (i + length sep) s)

-- | Python's s.split() (runs of whitespace).
pyWords :: String -> [String]
pyWords s = case dropWhile isPySpace s of
    "" -> []
    s' -> let (w, r) = break isPySpace s' in w : pyWords r

isPrefix, startsWith, endsWith :: String -> String -> Bool
isPrefix = isPrefixOf
startsWith s p = p `isPrefixOf` s
endsWith s p = p `isSuffixOf` s

-- | Python's s.find(sub) as a Maybe.
findSub :: String -> String -> Maybe Int
findSub sub s | null sub = Just 0
              | not (sub `isInfixOf` s) = Nothing
              | otherwise = Just (length (takeWhile (not . (sub `isPrefixOf`)) (tails s)))

rfindSub :: String -> String -> Maybe Int
rfindSub sub s = case [ i | (i, t) <- zip [0 ..] (tails s), sub `isPrefixOf` t ] of
    [] -> Nothing
    is -> Just (last is)
