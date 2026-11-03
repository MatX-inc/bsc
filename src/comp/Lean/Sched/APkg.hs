-- | The APackage as the prototype read it: bsc's own printout of the .ba
-- (dumpba's text, made in memory) parsed into expression trees, exactly as bapkg.py.
module Lean.Sched.APkg
    ( parseAPkg, parseExpr, parseAction, showAPkg, showAPkgNoSched, parseToks, actionToks, exprToks, memAPkgText
    ) where

import ASyntax (binOp, AAction(ATaskAction, ACall, AFCall), AExpr(ASStr, APrim, ANoInlineFunCall, AFunCall,
    ATaskValue, AMethCall, AMethValue, ATuple, ATupleSel, ASPort, ASParam, ASDef, ASInt), APackage,
    AType(ATBit))
import Data.Char (isDigit, isAsciiLower, isAsciiUpper)
import Data.List (intercalate, isPrefixOf)
import Id (getIdString, unQualId)
import IntLit (showSizedVeriIntLit, showVeriIntLit)
import PPrint (ppString)
import Prim (PrimOp(..))
import qualified Data.Array as A (Array, (!), listArray)
import qualified Data.Map.Strict as M (lookup)
import qualified Data.Set as S (Set, member, fromList, toAscList)

import Lean.Sched.Expr (memAPkgWith, isOne)
import Lean.Sched.Py (endsWith, findSub, isPySpace, omAdjust, omEmpty, omFromList, omInsert, omSetDefault,
    omToList, pyReprStr, pySplit, pyWords, rfindSub, rstrip, startsWith, strip, OMap)
import Lean.Sched.Tree (binPrec, classifyAct, reprAct, reprE, tokenize, APkg(..), Act, E(..), Tok)

-- ---------------------------------------------------------------- parser

data PS = PS { psToks :: A.Array Int Tok, psN :: Int, psInsts :: S.Set String }

type P a = PS -> Int -> Either String (a, Int)

peekAt :: PS -> Int -> Tok
peekAt ps j = if j < psN ps then psToks ps A.! j else ("eof", "")

eat :: Maybe String -> P Tok
eat v ps i = let t = peekAt ps i in case v of
    Just w | snd t /= w -> Left ("expected " ++ pyReprStr w ++ " got " ++ show t ++ " at " ++ show i)
    _ -> Right (t, i + 1)

pExpr :: Int -> P E
pExpr minbp ps i0 = do
    (lhs, i1) <- pUnary ps i0
    loop lhs i1
  where
    loop lhs i = case peekAt ps i of
        ("op", v) | Just bp <- M.lookup v binPrec, bp >= minbp -> do
            (rhs, i') <- pExpr (bp + 1) ps (i + 1)
            loop (EBin v lhs rhs) i'
        _ -> Right (lhs, i)

pUnary :: P E
pUnary ps i = case peekAt ps i of
    ("op", v) | v `elem` ["!", "~", "-"] -> do
        (a, i') <- pUnary ps (i + 1)
        return (EUn v a, i')
    _ -> pApp ps i

startsPrimary :: PS -> Int -> Bool
startsPrimary ps i = case peekAt ps i of
    (k, _) | k `elem` ["lit", "str"] -> snd (peekAt ps (i + 1)) /= "->"
    ("id", v) -> v `notElem` ["of", "then", "else", "_"] && snd (peekAt ps (i + 1)) /= "->"
    ("op", "(") -> True
    _ -> False

pApp :: P E
pApp ps i0 = do
    (h, i1) <- pPrimary ps i0
    case h of
        EIf {} -> Right (h, i1)
        ECase {} -> Right (h, i1)
        ESel {} -> Right (h, i1)
        _ -> do
            (args, i2) <- many i1 []
            let r = case h of
                    ECall a m [] | not (null args) -> ECall a m args
                    EId _ -> mk ps h args
                    _ -> if null args then h else EApp h args
            return (r, i2)
  where many i acc | startsPrimary ps i = do { (a, i') <- pPrimary ps i; many i' (a : acc) }
                   | otherwise = Right (reverse acc, i)

mk :: PS -> E -> [E] -> E
mk ps h@(EId n) args
    | '.' `elem` n, (inst, _ : meth) <- break (== '.') n, inst `S.member` psInsts ps = ECall inst meth args
    | n == "PrimArrayDynSelect", [a, b] <- args = ESel a b
    | null args = h
    | otherwise = EApp h args
mk _ h args = if null args then h else EApp h args

pPrimary :: P E
pPrimary ps i = case peekAt ps i of
    ("op", "(") -> do
        (x, i1) <- pExpr 0 ps (i + 1)
        let more j acc = if snd (peekAt ps j) == ","
                         then do { (y, j') <- pExpr 0 ps (j + 1); more j' (y : acc) }
                         else Right (reverse acc, j)
        (xs, i2) <- more i1 [x]
        (_, i3) <- eat (Just ")") ps i2
        return (case xs of { [y] -> y; _ -> ETup xs }, i3)
    (k, v) | k `elem` ["lit", "str"] -> Right (ELit v, i + 1)
    ("id", "_if_") -> do
        (c, i1) <- pPrimary ps (i + 1)
        (a, i2) <- pPrimary ps i1
        (b, i3) <- pPrimary ps i2
        return (EIf c a b, i3)
    ("id", "case") -> do
        (idx, i1) <- pExpr 0 ps (i + 1)
        (_, i2) <- eat (Just "of") ps i1
        let arms j acc = case peekAt ps j of
                ("id", "_") -> do
                    (_, j1) <- eat (Just "->") ps (j + 1)
                    (d, j2) <- pExpr 0 ps j1
                    return (reverse acc, Just d, j2)
                (k2, v2) | k2 == "eof" || v2 `elem` [";", ")"] -> Right (reverse acc, Nothing, j)
                _ -> do
                    (l, j1) <- pPrimary ps j
                    (_, j2) <- eat (Just "->") ps j1
                    (val, j3) <- pExpr 0 ps j2
                    arms j3 ((l, val) : acc)
        (as, d, i3) <- arms i2 []
        return (ECase idx as d, i3)
    ("id", "PrimArrayDynSelect") -> do
        (a, i1) <- pPrimary ps (i + 1)
        (b, i2) <- pPrimary ps i1
        return (ESel a b, i2)
    ("id", v) -> Right (mk ps (EId v) [], i + 1)
    t -> Left ("unexpected " ++ show t ++ " at " ++ show i)

mkPS :: S.Set String -> [Tok] -> PS
mkPS insts ts = PS (A.listArray (0, length ts - 1) ts) (length ts) insts

parseExpr :: S.Set String -> String -> Either String E
parseExpr insts s = do
    ts <- tokenize s
    let ps = mkPS insts ts
    (e, i) <- pExpr 0 ps 0
    if fst (peekAt ps i) /= "eof"
        then Left ("trailing tokens in " ++ pyReprStr (take 80 s))
        else Right e

-- | 'parseExpr' on tokens already made (the in-memory route, "Lean.Sched.Expr").
parseToks :: S.Set String -> [Tok] -> Either String E
parseToks insts ts = do
    let ps = mkPS insts ts
    (e, i) <- pExpr 0 ps 0
    if fst (peekAt ps i) /= "eof" then Left "trailing tokens" else Right e

-- | 'parseAction' on the tokens of the condition (when not 1) and of the call.
actionToks :: S.Set String -> Maybe [Tok] -> [Tok] -> Either String Act
actionToks insts mc ts = do
    cond <- case mc of
        Nothing -> Right Nothing
        Just cs -> (Just . fst) <$> pExpr 0 (mkPS insts cs) 0
    (act, _) <- pApp (mkPS insts ts) 0
    return (classifyAct insts cond act)

parseAction :: S.Set String -> String -> Either String Act
parseAction insts s = do
    ts <- tokenize s
    (cond, act) <- if "if" `isPrefixOf` s && length s > 2 && isPySpace (s !! 2)
        then do
            let k = thenAt ts
            (c, _) <- pExpr 0 (mkPS insts (take (k - 1) (drop 1 ts))) 0
            (a, _) <- pApp (mkPS insts (drop (k + 1) ts)) 0
            return (Just c, a)
        else do
            (a, _) <- pApp (mkPS insts ts) 0
            return (Nothing, a)
    return (classifyAct insts cond act)
  where
    -- the first top-level 'then', or the last token (as the prototype's loop leaves k)
    thenAt ts = go (0 :: Int) (zip [0 ..] ts)
      where go _ [] = length ts - 1
            go d ((k, (kind, v)) : r)
                | v `elem` ["(", "[", "{"] = go (d + 1) r
                | v `elem` [")", "]", "}"] = go (d - 1) r
                | kind == "id" && v == "then" && d == 0 = k
                | otherwise = go d r

-- ---------------------------------------------------------------- the package

-- | bapkg's split_top: at top-level separators, outside brackets and strings.
splitTopB :: Char -> String -> [String]
splitTopB sep = finish . go (0 :: Int) False False [] []
  where
    go _ _ _ cur out [] = (cur, out)
    go d True esc cur out (ch : r)
        | esc = go d True False (ch : cur) out r
        | ch == '\\' = go d True True (ch : cur) out r
        | ch == '"' = go d False False (ch : cur) out r
        | otherwise = go d True False (ch : cur) out r
    go d False _ cur out (ch : r)
        | ch == '"' = go d True False (ch : cur) out r
        | ch == sep && d' == 0 = go d' False False [] (reverse cur : out) r
        | otherwise = go d' False False (ch : cur) out r
      where d' | ch `elem` "([{" = d + 1
               | ch `elem` ")]}" = d - 1
               | otherwise = d
    finish (cur, out) = [ strip x | x <- reverse (if null (strip (reverse cur)) then out else reverse cur : out)
                                  , not (null (strip x)) ]

-- | Index just past the brace matching the first character ('{'), strings skipped.
matchBrace :: String -> Maybe Int
matchBrace = go 0 (0 :: Int) False False
  where
    go _ _ _ _ [] = Nothing
    go j d True esc (ch : r)
        | esc = go (j + 1) d True False r
        | ch == '\\' = go (j + 1) d True True r
        | ch == '"' = go (j + 1) d False False r
        | otherwise = go (j + 1) d True False r
    go j d False _ (ch : r)
        | ch == '"' = go (j + 1) d True False r
        | ch == '{' = go (j + 1) (d + 1) False False r
        | ch == '}' = if d - 1 == 0 then Just (j + 1) else go (j + 1) (d - 1) False False r
        | otherwise = go (j + 1) d False False r

-- | A definition line @  name :: type;@.
defLine :: String -> Maybe (String, String)
defLine ln = case ln of
    (' ' : ' ' : r@(c : _)) | not (isPySpace c) ->
        let (nm, r1) = break isPySpace r
        in case r1 of
            (' ' : ':' : ':' : ' ' : ty) | endsWith ty ";" -> Just (nm, init ty)
            _ -> Nothing
    _ -> Nothing

-- | Two spaces and a non-space.
indent2 :: String -> Bool
indent2 (' ' : ' ' : c : _) = not (isPySpace c)
indent2 _ = False

-- | _def_block: the definitions in some lines, with the types (always) and errors seen.
defBlock :: S.Set String -> [String] -> ([(String, E)], [(String, String)], [String])
defBlock insts = go [] [] []
  where
    go ds ts es ls = case ls of
        (l1 : l2 : rest) | Just (name, ty) <- defLine l1, ("  " ++ name ++ "  = ") `isPrefixOf` l2 ->
            let (more, rest') = break (\x -> "  --" `isPrefixOf` x || indent2 x) rest
                body = drop (length ("  " ++ name ++ "  = ")) l2 : more
                src0 = rstrip (unwords' (map strip body))
                src = if endsWith src0 ";" then init src0 else src0
                (e, es') = case parseExpr insts src of
                    Right x -> (x, es)
                    Left ex -> (ELit "?", ("def " ++ name ++ ": " ++ ex) : es)
            in go ((name, e) : ds) ((name, ty) : ts) es' rest'
        (_ : rest) -> go ds ts es rest
        [] -> (reverse ds, reverse ts, reverse es)
    unwords' = foldr1'' (\a b -> a ++ " " ++ b)
    foldr1'' _ [] = ""
    foldr1'' f xs = foldr1 f xs

parseAPkg :: String -> APkg
parseAPkg text =
    let ls = lines' text
        insts = S.fromList [ n | l <- ls, Just n <- [abstractInst l] ]
        -- _defs
        (dDefs, dTypes, dErrs) = case break (== "  -- AP local definitions") ls of
            (_, _ : rest) -> defBlock insts (takeWhile (not . ("  -- AP rules" `isPrefixOf`)) rest)
            _ -> ([], [], [])
        (rules, rErrs) = parseRules insts text
        (ifcVals, methArgs, iTypes, iErrs) = parseIfc insts ls
        (uses, ralloc) = parseSched text
    in APkg { apInsts = insts
            , apDefs = omFromList dDefs
            , apTypes = omFromList (dTypes ++ iTypes)
            , apRules = rules
            , apIfcVals = ifcVals
            , apMethArgs = methArgs
            , apErrors = dErrs ++ rErrs ++ iErrs
            , apUsesMap = uses
            , apRalloc = ralloc
            , apGuarded = parseGuarded text }
  where
    abstractInst l = case l of
        (' ' : ' ' : c : r) | isAsciiLower c || isAsciiUpper c || c == '_' ->
            let (n, r1) = span (\x -> isAsciiLower x || isAsciiUpper x || isDigit x || x == '_' || x == '$') r
            in if " :: ABSTRACT:" `isPrefixOf` r1 then Just (c : n) else Nothing
        _ -> Nothing

-- | Python's s[1:-1].
mid :: String -> String
mid s = if length s >= 2 then init (tail s) else ""

-- | Python's text.split('\n').
lines' :: String -> [String]
lines' s = case break (== '\n') s of
    (a, []) -> [a]
    (a, _ : r) -> a : lines' r

parseRules :: S.Set String -> String -> (OMap String (E, [Act]), [String])
parseRules insts = go omEmpty []
  where
    go acc errs t = case t of
        [] -> (acc, reverse errs)
        _ | Just (name, after) <- ruleHead t ->
              let (acc', errs') = rule name after acc errs
              in go acc' errs' after
        (_ : r) -> go acc errs r
    rule name after acc errs =
        case findSub "==>" after of
            Nothing -> (acc, errs)
            Just k ->
                let pred_ = strip (take k after)
                    fromK = drop k after
                in case findSub "{" fromK of
                    Nothing -> (acc, errs)
                    Just b0 ->
                        let blk = drop b0 fromK
                            b1 = maybe (length blk) id (matchBrace blk)
                            body = take (b1 - 2) (drop 1 blk)
                            (p, e1) = case parseExpr insts pred_ of
                                Right x -> (x, errs)
                                Left ex -> (ELit "?", ("pred " ++ name ++ ": " ++ ex) : errs)
                            step (as, es) a = case parseAction insts a of
                                Right x -> (x : as, es)
                                Left ex -> (as, ("action in " ++ name ++ ": " ++ ex ++ ": " ++ take 80 a) : es)
                            (acts, e2) = foldl step ([], e1) (splitTopB ';' body)
                        in (omInsert name (p, reverse acts) acc, e2)
    -- rule (\S+) "(?:[^"\\]|\\.)*":\s*\n\s*when
    ruleHead t = do
        r0 <- stripP "rule " t
        let (nm, r1) = break isPySpace r0
        if null nm then Nothing else Just ()
        r2 <- stripP " \"" r1
        r3 <- strBody r2
        r4 <- stripP "\":" r3
        let (ws, r5) = span isPySpace r4
        if '\n' `elem` ws then Just () else Nothing
        r6 <- stripP "when " r5
        return (nm, r6)
    stripP p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing
    strBody s = case s of
        ('"' : _) -> Just s
        ('\\' : x : r) | x /= '\n' -> strBody r
        ('\\' : _) -> Nothing
        (_ : r) -> strBody r
        [] -> Nothing

parseIfc :: S.Set String -> [String] -> (OMap String E, OMap String [String], [(String, String)], [String])
parseIfc insts ls0 = go (zip [0 :: Int ..] ls0) Nothing Nothing omEmpty omEmpty [] []
  where
    arr = A.listArray (0, length ls0 - 1) ls0
    n = length ls0
    go [] _ _ vals args ts es = (vals, args, reverse ts, reverse es)
    go ((i, ln) : rest) kind cur vals args ts es
        | Just (k, c) <- ifcHead ln = go rest (Just k) (Just c) vals (omSetDefault c [] id args) ts es
        | Nothing <- cur = go rest kind cur vals args ts es
        | "  -- AP" `isPrefixOf` ln || "  AScheduleInfo" `isPrefixOf` ln = go rest kind Nothing vals args ts es
        | Just (nm, _) <- defLine ln, Just c <- cur, Just k <- kind =
            if i + 1 < n && ("  " ++ nm ++ "  = ") `isPrefixOf` (arr A.! (i + 1))
            then if k `elem` ["AIDef", "AIActionValue"] && nm == c
                 then let (ds, ts', es') = defBlock insts [ arr A.! j | j <- [i .. min (n - 1) (i + 39)] ]
                          vals' = maybe vals (\e -> omInsert c e vals) (lookup nm ds)
                      in go rest kind cur vals' args (reverse ts' ++ ts) (reverse es' ++ es)
                 else go rest kind cur vals args ts es
            else if k `elem` ["AIAction", "AIActionValue"]
                 then go rest kind cur vals (omAdjust (++ [nm]) c args) ts es
                 else go rest kind cur vals args ts es
        | otherwise = go rest kind cur vals args ts es
    ifcHead ln = case ln of
        (' ' : ' ' : '-' : '-' : r) ->
            let (k, r1) = break (== ' ') r
            in case r1 of
                (' ' : c) | k `elem` ["AIDef", "AIAction", "AIActionValue"], not (null c), not (any isPySpace c) -> Just (k, c)
                _ -> Nothing
        _ -> Nothing

-- | Instance -> its 'guarded' parameter, where it has one.
parseGuarded :: String -> OMap String Bool
parseGuarded = go omEmpty
  where
    go acc t = case t of
        [] -> acc
        ('\n' : r) | Just (inst, after) <- absLine r ->
            let acc' = case findSub "meth types" after of
                    Nothing -> acc
                    Just j ->
                        let blk = take j after
                        in if findSub "param guarded;" blk == Nothing then acc
                           else let tl = case rfindSub ")\n" blk of
                                        Just k -> drop k blk
                                        Nothing -> if null blk then "" else [last blk]
                                    ls = bitLits tl
                                in if null ls then acc else omInsert inst (last (last ls) == '1') acc
            in go acc' after
        (_ : r) -> go acc r
    -- "  (\S+) :: ABSTRACT:  \S+ = \S+\n", the newline consumed
    absLine r = do
        r1 <- sp "  " r
        let (inst, r2) = break isPySpace r1
        if null inst then Nothing else Just ()
        r3 <- sp " :: ABSTRACT:  " r2
        let (ty, r4) = break isPySpace r3
        if null ty then Nothing else Just ()
        r5 <- sp " = " r4
        let (md, r6) = break isPySpace r5
        if null md then Nothing else Just ()
        r7 <- sp "\n" r6
        return (inst, r7)
    sp p s = if p `isPrefixOf` s then Just (drop (length p) s) else Nothing
    bitLits s = case s of
        ('1' : '\'' : b : v : r) | b `elem` "db", v `elem` "01" -> ['1', '\'', b, v] : bitLits r
        (_ : r) -> bitLits r
        [] -> []

parseSched :: String -> (OMap String [(String, [[String]])], OMap String [(String, Integer)])
parseSched t = (uses, ralloc)
  where
    uses = case findSub "-- method uses map" t of
        Nothing -> omEmpty
        Just k ->
            let fromK = drop k t
                blk = case findSub "-- rule uses map" fromK of
                    Just e -> take e fromK
                    Nothing -> take (length t - 1 - k) fromK
            in useEntries omEmpty blk
    useEntries acc b = case b of
        [] -> acc
        ('\n' : ' ' : ' ' : ' ' : ' ' : r@(c : _)) | not (isPySpace c) ->
            let (key, r1) = break isPySpace r
            in if " -> [" `isPrefixOf` r1
               then let lst = drop 4 r1          -- starts at the '['
                        q = closeBracket lst
                        inner = take (q - 1) (drop 1 lst)
                        items = [ (unwords (pyWords (head parts)), rr (strip (parts !! 1)))
                                | it <- splitTopB ',' inner, let it' = strip it
                                , startsWith it' "(" && endsWith it' ")"
                                , let parts = splitTopB ',' (mid it'), length parts >= 2 ]
                    in useEntries (omInsert key items acc) (drop 5 r1)
               else useEntries acc (tail b)
        (_ : r) -> useEntries acc r
    rr p = let rs = splitTopB ',' (mid p)
           in [ [ strip x | x <- pySplit "," (mid r), not (null (strip x)) ]
              | r0 <- rs, let r = strip r0, startsWith r "[" && endsWith r "]" ]
    -- index of the bracket closing the one at position 0 (or the last index)
    closeBracket s = go' 0 (0 :: Int) s
      where go' j _ [] = j - 1
            go' j d (c : r) | c == '[' = go' (j + 1) (d + 1) r
                            | c == ']' = if d - 1 == 0 then j else go' (j + 1) (d - 1) r
                            | otherwise = go' (j + 1) d r
    ralloc = case findSub "-- resource allocation table" t of
        Nothing -> omEmpty
        Just k -> case findSub "[" (drop k t) of
            Nothing -> omEmpty
            Just j0 ->
                let lst0 = drop (k + j0) t
                    q = closeBracket lst0
                    inner = take (q - 1) (drop 1 lst0)
                in foldl entry omEmpty (splitTopB ',' inner)
    entry acc it0 =
        let it = strip it0
        in if not (startsWith it "(" && endsWith it ")") then acc
           else let parts = splitTopB ',' (mid it)
                in if length parts < 2 then acc
                   else let key = strip (head parts)
                            lst = strip (parts !! 1)
                        in if not (startsWith lst "[" && endsWith lst "]") then acc
                           else omInsert key
                                  [ (unwords (pyWords (head pp)), read (strip (pp !! 1)))
                                  | x0 <- splitTopB ',' (mid lst), let x = strip x0
                                  , startsWith x "(" && endsWith x ")"
                                  , let pp = splitTopB ',' (mid x), length pp == 2 ] acc

-- | --show-model=apkg-tree: the parse, for comparison with bapkg.py's.
showAPkg :: APkg -> [String]
showAPkg ap = let (a, b) = break (\l -> startsWith l "guarded " || startsWith l "errors ") (showAPkgNoSched ap)
              in a ++ schedLines ap ++ b

schedLines :: APkg -> [String]
schedLines ap =
       [ "uses " ++ n ++ " = " ++ "[" ++ commas [ "(" ++ pyReprStr s ++ ", [" ++ commas [ "[" ++ commas (map pyReprStr r) ++ "]" | r <- rr ] ++ "])" | (s, rr) <- us ] ++ "]" | (n, us) <- omToList (apUsesMap ap) ]
    ++ [ "ralloc " ++ n ++ " = " ++ "[" ++ commas [ "(" ++ pyReprStr s ++ ", " ++ show k ++ ")" | (s, k) <- al ] ++ "]" | (n, al) <- omToList (apRalloc ap) ]
  where commas [] = ""
        commas xs = foldr1 (\a b -> a ++ ", " ++ b) xs

-- | The same without the schedule tables, which the in-memory route keeps as bsc's own records.
showAPkgNoSched :: APkg -> [String]
showAPkgNoSched ap =
    [ "insts " ++ unwords (S.toAscList (apInsts ap)) ]
    ++ [ "def " ++ n ++ " = " ++ reprE e | (n, e) <- omToList (apDefs ap) ]
    ++ [ "type " ++ n ++ " = " ++ t | (n, t) <- omToList (apTypes ap) ]
    ++ [ "rule " ++ n ++ " " ++ reprE p ++ " " ++ "[" ++ commas (map reprAct as) ++ "]" | (n, (p, as)) <- omToList (apRules ap) ]
    ++ [ "ifc " ++ n ++ " = " ++ reprE e | (n, e) <- omToList (apIfcVals ap) ]
    ++ [ "args " ++ n ++ " = " ++ "[" ++ commas (map pyReprStr as) ++ "]" | (n, as) <- omToList (apMethArgs ap) ]
    ++ [ "guarded " ++ n ++ " = " ++ (if b then "True" else "False") | (n, b) <- omToList (apGuarded ap) ]
    ++ [ "errors " ++ show (length (apErrors ap)) ]
  where commas [] = ""
        commas xs = foldr1 (\a b -> a ++ ", " ++ b) xs

-- ---------------------------------------------------------------- the in-memory package, through text

-- | The tokens of bsc's printout of an expression at precedence @p@ (ASyntax's
-- PPrint AExpr), as the prototype's tokenizer splits that text.
exprToks :: Int -> AExpr -> Either String [Tok]
exprToks p e = case e of
    APrim _ _ o es@(_ : _ : _) | binOp o -> do
        ops <- tokenize (ppString o)
        xs <- mapM (exprToks 1) es
        paren (p > 0) (intercalate ops xs)
    APrim _ _ PrimCase (x : dd : ces) -> do
        xi <- exprToks 0 x
        arms <- pairsOf ces
        d <- exprToks 0 dd
        return ([idT "case"] ++ xi ++ [idT "of"] ++ concat arms ++ [idT "_", opT "->"] ++ d)
    APrim _ _ PrimPriMux es -> muxT "primux" es
    APrim _ _ PrimMux es -> muxT "mux" es
    APrim _ _ o es -> do
        ops <- tokenize (ppString o)
        xs <- mapM (exprToks 1) es
        paren (p > 0) (ops ++ concat xs)
    ANoInlineFunCall _ i _ es -> app i es
    AFunCall _ i _ _ es -> app i es
    ATaskValue _ i _ _ n -> paren (p > 0) =<< tokenize (getIdString i ++ "#" ++ show n)
    AMethCall _ i m es -> do
        h <- tokenize (getIdString i ++ "." ++ getIdString (unQualId m))
        xs <- mapM (exprToks 1) es
        paren (p > 0 && not (null es)) (h ++ concat xs)
    AMethValue _ i m -> paren (p > 0) =<< tokenize (getIdString i ++ "." ++ getIdString (unQualId m))
    ATuple _ es -> do
        xs <- mapM (exprToks 0) es
        return ([opT "("] ++ intercalate [opT ","] xs ++ [opT ")"])
    ATupleSel _ x k -> do
        xs <- exprToks 1 x
        ks <- tokenize (show k)
        return (xs ++ [opT "["] ++ ks ++ [opT "]"])
    ASPort _ i -> tokenize (getIdString i)
    ASParam _ i -> tokenize (getIdString i)
    ASDef _ i -> tokenize (getIdString i)
    ASInt _ (ATBit sz) il -> tokenize (showSizedVeriIntLit sz il)
    ASInt _ _ il -> tokenize (showVeriIntLit il)
    ASStr _ _ s -> tokenize (show s)
    _ -> tokenize (ppString e)      -- reals, don't-cares, clocks, resets, inouts, gates
  where
    idT s = ("id", s)
    opT s = ("op", s)
    paren b ts = Right (if b then opT "(" : ts ++ [opT ")"] else ts)
    app i es = do
        h <- tokenize (getIdString i)
        xs <- mapM (exprToks 1) es
        paren (p > 0) (h ++ concat xs)
    pairsOf (x : y : r) = do
        a <- exprToks 0 x
        b <- exprToks 0 y
        rest <- pairsOf r
        return ((a ++ [opT "->"] ++ b) : rest)
    pairsOf [] = Right []
    pairsOf _ = Left "odd case arms"
    muxT nm es = do
        let grp (x : y : r) = do
                a <- exprToks 0 x
                b <- exprToks 0 y
                rest <- grp r
                return (([opT "("] ++ a ++ [opT ","] ++ b ++ [opT ")"]) : rest)
            grp [] = Right []
            grp _ = Left "odd mux arms"
        gs <- grp es
        paren (p > 0) (idT nm : concat gs)

-- | 'memAPkg' through bsc's printer's tokens and the prototype's parser (the parity oracle).
memAPkgText :: APackage -> APkg
memAPkgText = memAPkgWith (\insts e -> exprToks 0 e >>= parseToks insts) actText
  where
    actText insts a = case a of
        ACall i m (c : es) -> do
            h <- tokenize (getIdString i ++ "." ++ getIdString (unQualId m))
            call h c es
        AFCall i _ _ (c : es) _ -> do
            h <- tokenize (getIdString i)
            call h c es
        ATaskAction i _ _ n (c : es) _ _ _ -> do
            h <- tokenize (getIdString i ++ "#" ++ show n)
            call h c es
        _ -> Left "an action without a condition"
      where
        call h c es = do
            mc <- if isOne c then Right Nothing else Just <$> exprToks 0 c
            xs <- mapM (exprToks 1) es
            actionToks insts mc (h ++ concat xs)

