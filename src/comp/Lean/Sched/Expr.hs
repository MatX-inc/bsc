-- | The expression layer: bsc's in-memory expressions as the prototype's trees,
-- and the site analysis's view of a design over them.
module Lean.Sched.Expr
    ( memAPkg, memAPkgWith, isOne, aexprE, actE
    , SD(..), mkSD, exactSD, Port2, canonExpr, canon, boolish, boolDef
    , exprReads, exprSources, exprHw, exprCalls, exprCallsMany, defReads, arrayElems, width, litval, isBoolLit
    , FS, Node(..), LitKey, fsEmpty, fsT, fsF, fsNode, fsLit, fsAnd, fsOr, fsLits, fsSummary, fsSummaryFrom, fsSubst, fsText
    ) where

import ASyntax (aIfaceArgs, binOp, AAction(ATaskAction, ACall, AFCall), ADef(adef_type, adef_objid, adef_expr),
    AExpr(ATaskValue, APrim, ANoInlineFunCall, AFunCall, AMethCall, AMethValue, ATuple, ASPort, ASParam, ASDef,
    ASInt, ASStr, ATupleSel), AIFace(aif_body, aif_value, aif_name, AIDef, AIAction, AIActionValue),
    APackage(apkg_state_instances, apkg_interface, apkg_rules, apkg_local_defs), ARule(arule_pred, arule_id,
    arule_actions), AType(ATBit), AVInst(avi_vname, avi_vmi, avi_iargs))
import Data.Char (isDigit, isHexDigit, digitToInt)
import Data.List (intercalate, sort)
import Id (getIdString, unQualId)
import IntLit (IntLit(..), showSizedVeriIntLit, showVeriIntLit)
import PPrint (ppString)
import Prim (PrimOp(..))
import VModInfo (vArgs, VArgInfo(..), getVNameString)
import qualified Data.Array as A ((!), listArray)
import qualified Data.IntMap.Strict as IM (IntMap, (!), empty, lookup, fromList, insert)
import qualified Data.IntSet as IS (member, empty, insert, toList)
import qualified Data.Map.Strict as M (Map, (!), empty, findWithDefault, keys, lookup, member, toList, union, unionsWith,
    insert, fromList, insertWith, unionWith)
import qualified Data.Map.Lazy as ML (Map, (!), findWithDefault, fromList, lookup, map, mapWithKey, member)
import qualified Data.Set as S (Set, member, empty, filter, fromList, insert, isProperSubsetOf, singleton,
    size, toList, union, unions)

import Lean.Sched.Py (omEmpty, omFromList, omInsert, omToList, startsWith, strip)
import Lean.Sched.Tree (binPrec, classifyAct, eKids, tokenize, APkg(..), Act, E(..))
import Lean.Sched.Types (Child(chMethods), Design(dInputOrder, dChildren))

-- ---------------------------------------------------------------- tokens

isOne :: AExpr -> Bool
isOne (ASInt _ _ il) = ilValue il == 1
isOne _ = False

-- ---------------------------------------------------------------- trees from AExpr

-- | The prototype's tree of an expression, built from the AExpr itself: the tree
-- bapkg's parser gives bsc's printout, where that parse is right; Left where it fails.
aexprE :: S.Set String -> AExpr -> Either String E
aexprE insts = go
  where
    go e = case e of
        APrim _ _ o es@(_ : _ : _) | binOp o -> do
            op <- binTok (toStr o)
            xs <- mapM go es
            return (foldl1 (EBin op) xs)
        APrim _ _ PrimCase (x : dd : ces) -> do
            xi <- go x
            arms <- pairs ces
            d <- go dd
            return (ECase xi arms (Just d))
        APrim _ _ PrimPriMux es -> mux "primux" es
        APrim _ _ PrimMux es -> mux "mux" es
        APrim _ _ o es -> do
            xs <- mapM go es
            case oneTok (toStr o) of
                Just ("op", v) | v `elem` ["!", "~", "-"], [x] <- xs -> Right (EUn v x)
                Just ("id", "_if_") | [c, a, b] <- xs -> Right (EIf c a b)
                Just ("id", "PrimArrayDynSelect") | [a, b] <- xs -> Right (ESel a b)
                Just ("id", v) | plainId v -> Right (applyTo (mkId v) xs)
                _ -> Left ("primitive " ++ toStr o)
        ANoInlineFunCall _ i _ es -> fun (getIdString i) es
        AFunCall _ i _ _ es -> fun (getIdString i) es
        AMethCall _ i m es -> fun (getIdString i ++ "." ++ getIdString (unQualId m)) es
        AMethValue _ i m -> fun (getIdString i ++ "." ++ getIdString (unQualId m)) []
        ATuple _ [x] -> go x
        ATuple _ es@(_ : _ : _) -> ETup <$> mapM go es
        ASPort _ i -> leaf (getIdString i)
        ASParam _ i -> leaf (getIdString i)
        ASDef _ i -> leaf (getIdString i)
        ASInt _ (ATBit sz) il -> leaf (showSizedVeriIntLit sz il)
        ASInt _ _ il -> leaf (showVeriIntLit il)
        ASStr _ _ s -> leaf (show s)
        ATupleSel {} -> Left "tuple selection"
        ATaskValue {} -> Left "task value"
        _ -> leaf (ppString e)
    toStr o = ppString o
    binTok s = case oneTok s of
        Just ("op", v) | M.member v binPrec -> Right v
        _ -> Left ("operator " ++ s)
    oneTok s = case tokenize s of { Right [t] -> Just t; _ -> Nothing }
    plainId v = v `notElem` ["case", "of", "then", "else", "_", "_if_", "PrimArrayDynSelect"]
    leaf s = case oneTok s of
        Just ("id", v) | plainId v -> Right (mkId v)
        Just (k, v) | k `elem` ["lit", "str"] -> Right (ELit v)
        _ -> Left ("leaf " ++ s)
    fun s es = case oneTok s of
        Just ("id", v) | plainId v -> applyTo (mkId v) <$> mapM go es
        _ -> Left ("name " ++ s)
    -- pApp's result for a head and its arguments
    applyTo h args = case h of
        ECall a m [] | not (null args) -> ECall a m args
        EId n -> mkWith n args
        _ -> if null args then h else EApp h args
    mkId n = mkWith n []
    mkWith n args
        | '.' `elem` n, (inst, _ : meth) <- break (== '.') n, inst `S.member` insts = ECall inst meth args
        | n == "PrimArrayDynSelect", [a, b] <- args = ESel a b
        | null args = EId n
        | otherwise = EApp (EId n) args
    pairs (x : y : r) = do { a <- go x; b <- go y; rest <- pairs r; return ((a, b) : rest) }
    pairs [] = Right []
    pairs _ = Left "odd case arms"
    mux nm es = do
        let grp (x : y : r) = do { a <- go x; b <- go y; rest <- grp r; return (ETup [a, b] : rest) }
            grp [] = Right []
            grp _ = Left "odd mux arms"
        gs <- grp es
        return (mkWith nm gs)

-- | An action as the prototype's parser reads its printout: the condition (none when 1)
-- and the call, from the action itself.
actE :: S.Set String -> AAction -> Either String Act
actE insts a = case a of
    ACall i m (c : es) -> call (getIdString i ++ "." ++ getIdString (unQualId m)) c es
    AFCall i _ _ (c : es) _ -> call (getIdString i) c es
    ATaskAction {} -> Left "task action"
    _ -> Left "an action without a condition"
  where
    call n c es = do
        cond <- if isOne c then Right Nothing else Just <$> aexprE insts c
        v <- case tokenize n of
            Right [("id", v)] | v `notElem` ["case", "of", "then", "else", "_", "_if_", "PrimArrayDynSelect"] -> Right v
            _ -> Left ("name " ++ n)
        xs <- mapM (aexprE insts) es
        return (classifyAct insts cond (applyName v xs))
    applyName v xs
        | '.' `elem` v, (inst, _ : meth) <- break (== '.') v, inst `S.member` insts = ECall inst meth xs
        | v == "PrimArrayDynSelect", [x, y] <- xs = ESel x y
        | null xs = EId v
        | otherwise = EApp (EId v) xs

-- ---------------------------------------------------------------- the package

-- | The prototype's view of the APackage (bapkg.APkg), built from the package in
-- memory; the schedule tables stay bsc's records ('apUsesMap', 'apRalloc' empty).
memAPkg :: APackage -> APkg
memAPkg apkg = memAPkgWith aexprE actE apkg

memAPkgWith :: (S.Set String -> AExpr -> Either String E) -> (S.Set String -> AAction -> Either String Act) -> APackage -> APkg
memAPkgWith conv convAct apkg = APkg
    { apInsts = insts
    , apDefs = omFromList defs
    , apTypes = omFromList ([ (nm d, ppString (adef_type d)) | d <- apkg_local_defs apkg ] ++ ifcTypes)
    , apRules = foldl' (\m (n, r) -> omInsert n r m) omEmpty rules
    , apIfcVals = omFromList ifcVals
    , apMethArgs = omFromList ifcArgs
    , apErrors = [ n | (n, Left _) <- defRes ] ++ ruleErrs
    , apUsesMap = omEmpty
    , apRalloc = omEmpty
    , apGuarded = omFromList [ (getIdString (avi_vname v), g) | v <- apkg_state_instances apkg, Just g <- [guardedOf v] ]
    }
  where
    insts = S.fromList [ getIdString (avi_vname v) | v <- apkg_state_instances apkg ]
    nm = getIdString . adef_objid
    pe = conv insts
    orQ = either (const (ELit "?")) id
    defRes = [ (nm d, pe (adef_expr d)) | d <- apkg_local_defs apkg ]
    defs = [ (n, orQ r) | (n, r) <- defRes ]
    ifc = apkg_interface apkg
    isMeth f = case f of { AIDef {} -> True; AIAction {} -> True; AIActionValue {} -> True; _ -> False }
    valueOf f = case f of
        AIDef {} -> Just (aif_value f)
        AIActionValue {} -> Just (aif_value f)
        _ -> Nothing
    named f = [ d | Just d <- [valueOf f], nm d == getIdString (aif_name f) ]
    ifcVals = [ (getIdString (aif_name f), orQ (pe (adef_expr d))) | f <- ifc, d <- named f ]
    ifcTypes = [ (nm d, ppString (adef_type d)) | f <- ifc, d <- named f ]
    ifcArgs = [ (getIdString (aif_name f), case f of { AIDef {} -> []; _ -> map (getIdString . fst) (aIfaceArgs f) })
              | f <- ifc, isMeth f ]
    bodyRules = concat [ case f of { AIAction { aif_body = b } -> b; AIActionValue { aif_body = b } -> b; _ -> [] } | f <- ifc ]
    ruleRes = [ (getIdString (arule_id r), orQ (pe (arule_pred r)), map act (arule_actions r), r)
              | r <- apkg_rules apkg ++ bodyRules ]
    rules = [ (n, (p, [ a | Right a <- as ])) | (n, p, as, _) <- ruleRes ]
    ruleErrs = [ n | (n, _, as, r) <- ruleRes, Left _ <- (pe (arule_pred r) >> Right ()) : map (fmap (const ())) as ]
    act = convAct insts
    -- bapkg reads the instance's last one-bit literal (1'd0, 1'b1, ...) when it has a 'guarded' parameter
    guardedOf v
        | not (any isGuarded (vArgs (avi_vmi v))) = Nothing
        | otherwise = case bitLits (intercalate ", " (map ppString (avi_iargs v))) of
            [] -> Nothing
            ls -> Just (last (last ls) == '1')
    isGuarded va = case va of { Param n -> getVNameString n == "guarded"; _ -> False }
    bitLits str = case str of
        ('1' : '\'' : b : x : r) | b `elem` "db", x `elem` "01" -> ['1', '\'', b, x] : bitLits r
        (_ : r) -> bitLits r
        [] -> []

-- ---------------------------------------------------------------- the design's view

-- | A child port, (instance, method).
type Port2 = (String, String)

-- | sitecond.Design: definitions, types and inputs, and per-definition tables
-- computed once each (lazily, in dependency order).
data SD = SD
    { sdDefs   :: M.Map String E
    , sdTypes  :: M.Map String String
    , sdInputs :: S.Set String
    , sdAlias  :: M.Map Port2 String
    , sdReads  :: ML.Map String (S.Set Port2)
    , sdHw     :: ML.Map String (S.Set String)
    , sdCalls  :: ML.Map String (M.Map Port2 (S.Set Port2))
    , sdCanon  :: ML.Map String String
    }

-- | The view: the package's definitions, bsc's use conditions (recomputed) as
-- use_cond_k and the scheduler's as bsc_uses_k, and FIFO2's output aliases.
mkSD :: APkg -> Design -> [(String, E)] -> SD
mkSD ap d useConds = build (M.fromList (omToList (apDefs ap)) `M.union` extra) types (S.fromList (dInputOrder d)) alias
  where
    extra = M.fromList (useConds ++ [ ("bsc_uses_" ++ drop 7 n, e) | (n, e) <- omToList (apDefs ap), startsWith n "__duses" ])
    types = M.fromList (omToList (apTypes ap)) `M.union`
            M.fromList [ (n, "Bit 1") | n <- M.keys extra ]
    alias = M.fromList [ ((k, a), b) | (k, c) <- M.toList (dChildren d), (a, b) <- [("notEmpty", "i_notEmpty"), ("notFull", "i_notFull")]
                                     , M.member a (chMethods c), M.member b (chMethods c) ]

-- | Every port its own atom (no output aliases).
exactSD :: SD -> SD
exactSD sd = build (sdDefs sd) (sdTypes sd) (sdInputs sd) M.empty

build :: M.Map String E -> M.Map String String -> S.Set String -> M.Map Port2 String -> SD
build defs types inputs alias = sd
  where
    sd = SD defs types inputs alias rd hw cl cn
    rd = ML.map (exprReads sd) defs
    hw = ML.map (exprHw sd) defs
    cl = ML.map (exprCalls sd) defs
    cn = ML.mapWithKey canonDef defs
    canonDef n de = case de of
        EId _ -> canon sd de
        ECall {} -> canon sd de
        _ -> n

isWF :: String -> Bool
isWF = (`startsWith` "WILL_FIRE_")

-- | The ports an expression reads, through definitions (not WILL_FIRE).
exprReads :: SD -> E -> S.Set Port2
exprReads sd = go S.empty . pure
  where
    go acc [] = acc
    go acc (x : st) = case x of
        EId n | isWF n -> go acc st
              | Just r <- ML.lookup n (sdReads sd) -> go (S.union r acc) st
              | otherwise -> go acc st
        ECall i m as -> go (S.insert (i, m) acc) (as ++ st)
        _ -> go acc (eKids x ++ st)

-- | What 'exprReads' unions: the ports called directly and the definitions read through.
exprSources :: SD -> E -> ([Port2], [String])
exprSources sd = go [] [] . pure
  where
    go ps ns [] = (ps, ns)
    go ps ns (x : st) = case x of
        EId n | isWF n -> go ps ns st
              | ML.member n (sdReads sd) -> go ps (n : ns) st
              | otherwise -> go ps ns st
        ECall i m as -> go ((i, m) : ps) ns (as ++ st)
        _ -> go ps ns (eKids x ++ st)

defReads :: SD -> String -> S.Set Port2
defReads sd n = ML.findWithDefault S.empty n (sdReads sd)

-- | The hardware-only names an expression reaches: WILL_FIRE signals and inputs.
exprHw :: SD -> E -> S.Set String
exprHw sd = go S.empty . pure
  where
    go acc [] = acc
    go acc (x : st) = case x of
        EId n | isWF n -> go (S.insert n acc) st
              | Just r <- ML.lookup n (sdHw sd) -> go (S.union r acc) st
              | S.member n (sdInputs sd) -> go (S.insert n acc) st
              | otherwise -> go acc st
        ECall _ _ as -> go acc (as ++ st)
        _ -> go acc (eKids x ++ st)

-- | Value-method calls: port -> the ports its arguments read.
exprCalls :: SD -> E -> M.Map Port2 (S.Set Port2)
exprCalls sd = go M.empty . pure
  where
    go acc [] = acc
    go acc (x : st) = case x of
        EId n | isWF n -> go acc st
              | Just r <- ML.lookup n (sdCalls sd) -> go (M.unionWith S.union acc r) st
              | otherwise -> go acc st
        ECall i m as -> go (M.insertWith S.union (i, m) (S.unions (map (exprReads sd) as)) acc) (as ++ st)
        _ -> go acc (eKids x ++ st)

-- | The union of 'exprCalls' over expressions, each definition's table taken once.
exprCallsMany :: SD -> [E] -> M.Map Port2 (S.Set Port2)
exprCallsMany sd es0 = M.unionsWith S.union (direct : [ ML.findWithDefault M.empty n (sdCalls sd) | n <- S.toList defs ])
  where
    (direct, defs) = go M.empty S.empty es0
    go acc ds [] = (acc, ds)
    go acc ds (x : st) = case x of
        EId n | isWF n -> go acc ds st
              | ML.member n (sdCalls sd) -> go acc (S.insert n ds) st
              | otherwise -> go acc ds st
        ECall i m as -> go (M.insertWith S.union (i, m) (S.unions (map (exprReads sd) as)) acc) ds (as ++ st)
        _ -> go acc ds (eKids x ++ st)

-- | The elements of an array (PrimBuildArray), through up to 50 definitions.
arrayElems :: SD -> E -> Maybe [E]
arrayElems sd = go (0 :: Int)
  where
    go k (EId n) | k < 50, Just de <- M.lookup n (sdDefs sd) = go (k + 1) de
    go _ (EApp (EId "PrimBuildArray") as) = Just as
    go _ _ = Nothing

width :: SD -> E -> Maybe Integer
width sd e = case e of
    EId n -> case words (M.findWithDefault "" n (sdTypes sd)) of
        ["Bit", k] | all isDigit k, not (null k) -> Just (read k)
        _ -> Nothing
    ELit s -> case span isDigit s of
        (k@(_ : _), '\'' : _) -> Just (read k)
        _ -> Nothing
    _ -> Nothing

boolType :: String -> Bool
boolType t = strip t `elem` ["Bit 1", "Bool", "Prelude.Bool"]

boolDef :: SD -> String -> Maybe E
boolDef sd n = case M.lookup n (sdDefs sd) of
    Just de | boolType (M.findWithDefault "" n (sdTypes sd)) -> Just de
    _ -> Nothing

boolish :: SD -> E -> Bool
boolish sd e = case e of
    EId n -> boolType (M.findWithDefault "" n (sdTypes sd))
    EBin o _ _ -> o `elem` ["&&", "||", "==", "!=", "<", "<=", ">", ">=", ".<", ".<=", ".>", ".>="]
    EUn "!" _ -> True
    ELit s -> startsWith s "1'"
    _ -> False

-- | sitecond.canon_expr.
canonExpr :: E -> String
canonExpr e = case e of
    EId s -> s
    ELit s -> s
    ECall i m as -> i ++ "." ++ m ++ "(" ++ intercalate "," (map canonExpr as) ++ ")"
    EIf a b c -> "(if " ++ canonExpr a ++ " " ++ canonExpr b ++ " " ++ canonExpr c ++ ")"
    ECase x arms d -> "(case " ++ canonExpr x ++ " " ++ unwords [ canonExpr a ++ ":" ++ canonExpr b | (a, b) <- arms ]
                      ++ " " ++ maybe "" canonExpr d ++ ")"
    ESel a i -> canonExpr a ++ "[" ++ canonExpr i ++ "]"
    EApp h as -> "(" ++ canonExpr h ++ " " ++ unwords (map canonExpr as) ++ ")"
    EBin o a b -> "(" ++ canonExpr a ++ " " ++ o ++ " " ++ canonExpr b ++ ")"
    EUn o a -> "(" ++ o ++ canonExpr a ++ ")"
    ETup xs -> "(" ++ intercalate "," (map canonExpr xs) ++ ")"

-- | Design.canon: alias definitions (of a name or a call) resolved, and output aliases.
canon :: SD -> E -> String
canon sd e = case e of
    EId n -> ML.findWithDefault n n (sdCanon sd)
    ECall i m as -> i ++ "." ++ M.findWithDefault m (i, m) (sdAlias sd) ++ "(" ++ intercalate "," (map (canon sd) as) ++ ")"
    ELit s -> s
    _ -> canonExpr e

-- | satc.litval.
litval :: String -> Maybe Integer
litval s = case span isDigit s of
    (_ : _, '\'' : r0) ->
        let r1 = case r0 of { (c : r) | c `elem` "sS" -> r; _ -> r0 }
        in case r1 of
            (b : ds) | b `elem` "bdhoBDHO", not (null ds), all (\c -> isHexDigit c || c == '_') ds ->
                let base = case b of { 'b' -> 2; 'B' -> 2; 'd' -> 10; 'D' -> 10; 'h' -> 16; 'H' -> 16; _ -> 8 }
                    digs = filter (/= '_') ds
                in if null digs || any (\c -> toInteger (digitToInt c) >= base) digs then Nothing
                   else Just (foldl' (\a c -> a * base + toInteger (digitToInt c)) 0 digs)
            _ -> Nothing
    _ | ('0' : 'x' : h@(_ : _)) <- s, all isHexDigit h -> Just (foldl' (\a c -> a * 16 + toInteger (digitToInt c)) 0 h)
      | not (null s), all isDigit s -> Just (read s)
      | otherwise -> Nothing

-- | satc.is_bool_lit.
isBoolLit :: E -> Maybe Bool
isBoolLit (ELit ['1', '\'', b, v]) | b `elem` "bdhBDH", v `elem` "01" = Just (v == '1')
isBoolLit _ = Nothing

-- ---------------------------------------------------------------- formulas

-- | A reach literal's key: its kind ("c" or "eq") and its expressions' canonical forms.
type LitKey = (String, [String])

data Node = NT | NF | NLit LitKey Bool | NAnd [Int] | NOr [Int]
    deriving (Eq, Ord)

-- | sitecond.FS: hash-consed monotone formulas over path literals.
data FS = FS { fsNodes :: !(IM.IntMap Node), fsIdx :: !(M.Map Node Int), fsNext :: !Int }

fsT, fsF :: Int
fsT = 0
fsF = 1

fsEmpty :: FS
fsEmpty = FS (IM.fromList [(0, NT), (1, NF)]) (M.fromList [(NT, 0), (NF, 1)]) 2

fsNode :: FS -> Int -> Node
fsNode fs i = fsNodes fs IM.! i

mk :: Node -> FS -> (Int, FS)
mk n fs = case M.lookup n (fsIdx fs) of
    Just i -> (i, fs)
    Nothing -> let i = fsNext fs in (i, FS (IM.insert i n (fsNodes fs)) (M.insert n i (fsIdx fs)) (i + 1))

fsLit :: LitKey -> Bool -> FS -> (Int, FS)
fsLit k p = mk (NLit k p)

fsAnd :: [Int] -> FS -> (Int, FS)
fsAnd xs fs
    | fsF `elem` xs = (fsF, fs)
    | otherwise =
        let out = S.fromList (concat [ case fsNode fs x of { NAnd ys -> ys; _ -> [x] } | x <- xs, x /= fsT ])
        in case S.toList out of
            [] -> (fsT, fs)
            [x] -> (x, fs)
            ys -> mk (NAnd ys) fs

fsOr :: [Int] -> FS -> (Int, FS)
fsOr xs fs
    | fsT `elem` xs = (fsT, fs)
    | otherwise =
        let out0 = S.fromList (concat [ case fsNode fs x of { NOr ys -> ys; _ -> [x] } | x <- xs, x /= fsF ])
            conj x = case fsNode fs x of { NAnd ys -> S.fromList ys; _ -> S.singleton x }
            -- absorption, as schedcheck's orAll: x || (c && x) is x
            out | S.size out0 > 1 && S.size out0 <= 64 =
                    let cs = M.fromList [ (x, conj x) | x <- S.toList out0 ]
                    in S.filter (\x -> not (any (\y -> y /= x && (cs M.! y) `S.isProperSubsetOf` (cs M.! x)) (S.toList out0))) out0
                | otherwise = out0
        in case S.toList out of
            [] -> (fsF, fs)
            [x] -> (x, fs)
            ys -> mk (NOr ys) fs

-- | The (key, polarity) literals of a formula.
fsLits :: FS -> Int -> S.Set (LitKey, Bool)
fsLits fs f0 = snd (go f0 IM.empty)
  where
    go f memo = case IM.lookup f memo of
        Just r -> (memo, r)
        Nothing ->
            let (memo', r) = case fsNode fs f of
                    NLit k p -> (memo, S.singleton (k, p))
                    NAnd ys -> kids ys memo
                    NOr ys -> kids ys memo
                    _ -> (memo, S.empty)
            in (IM.insert f r memo', r)
    kids ys memo = foldl' (\(m, acc) y -> let (m', r) = go y m in (m', S.union acc r)) (memo, S.empty) ys

-- | Per-node summaries of a store's formulas, each node's from its children's (lazily, once per node).
fsSummary :: (LitKey -> Bool -> a) -> ([a] -> a) -> a -> FS -> Int -> a
fsSummary lit comb triv fs = (arr A.!)
  where arr = A.listArray (0, fsNext fs - 1) [ val i | i <- [0 .. fsNext fs - 1] ]
        val i = case fsNode fs i of
            NLit k p -> lit k p
            NAnd ys -> comb (map (arr A.!) ys)
            NOr ys -> comb (map (arr A.!) ys)
            _ -> triv

-- | 'fsSummary' over the nodes reachable from @roots@ only.
fsSummaryFrom :: (LitKey -> Bool -> a) -> ([a] -> a) -> a -> FS -> [Int] -> Int -> a
fsSummaryFrom lit comb triv fs roots = (memo ML.!)
  where memo = ML.fromList [ (i, val i) | i <- IS.toList (reach roots IS.empty) ]
        reach [] seen = seen
        reach (i : st) seen
            | IS.member i seen = reach st seen
            | otherwise = reach (kids i ++ st) (IS.insert i seen)
        kids i = case fsNode fs i of { NAnd ys -> ys; NOr ys -> ys; _ -> [] }
        val i = case fsNode fs i of
            NLit k p -> lit k p
            NAnd ys -> comb (map (memo ML.!) ys)
            NOr ys -> comb (map (memo ML.!) ys)
            _ -> triv

-- | The formula with every literal whose key is in @drop@ replaced by true.
fsSubst :: S.Set LitKey -> Int -> FS -> (Int, FS)
fsSubst drop f0 fs0 = let (r, fs', _) = go f0 fs0 IM.empty in (r, fs')
  where
    go f fs memo = case IM.lookup f memo of
        Just r -> (r, fs, memo)
        Nothing ->
            let (r, fs', memo') = case fsNode fs f of
                    NLit k _ -> (if S.member k drop then fsT else f, fs, memo)
                    NAnd ys -> let (rs, fs1, m1) = kids ys fs memo; (r', fs2) = fsAnd rs fs1 in (r', fs2, m1)
                    NOr ys -> let (rs, fs1, m1) = kids ys fs memo; (r', fs2) = fsOr rs fs1 in (r', fs2, m1)
                    _ -> (f, fs, memo)
            in (r, fs', IM.insert f r memo')
    kids ys fs memo = foldl' (\(acc, fs', m) y -> let (r, fs'', m') = go y fs' m in (acc ++ [r], fs'', m')) ([], fs, memo) ys

-- | A formula's text, independent of node numbering (for --show-model=sites).
fsText :: FS -> Int -> String
fsText fs f = case fsNode fs f of
    NT -> "T"
    NF -> "F"
    NLit (k, xs) p -> (if p then "" else "!") ++ k ++ "{" ++ intercalate "|" xs ++ "}"
    NAnd ys -> "&(" ++ intercalate "," (sort (map (fsText fs) ys)) ++ ")"
    NOr ys -> "|(" ++ intercalate "," (sort (map (fsText fs) ys)) ++ ")"
