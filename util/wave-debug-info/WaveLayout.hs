{-# LANGUAGE ScopedTypeVariables #-}
-- How a type's values lay out in bits, recovered from its Bits instance.
--
-- A caller poses queries: functions from the packed bits of a type to
-- some bits of interest (is the value this constructor? what are the
-- bits of this field?), written in source syntax against the type's
-- own pack and unpack.  The queries are type checked and then reduced
-- by the compiler's evaluator over a symbolic argument, leaving an
-- expression in which only what depends on the argument remains.  A
-- derived instance reduces to bit extracts and tag comparisons; a
-- hand-written instance reduces to whatever its code computes, which
-- is reported as an expression the reader can evaluate.
module WaveLayout(Query(..), BitExpr(..), reduceQueries,
                  bitExprWidth, evalBitExpr, bitExprSupport, distinct) where

import qualified Control.Exception as CE
import Control.Monad(forM, when)
import Data.Bits((.&.), (.|.), xor, shiftL, shiftR, testBit, complement)
import Data.List(foldl')
import Data.Maybe(catMaybes)
import qualified Data.Map as M
import qualified Data.Set as S
import System.Exit(ExitCode)
import System.IO(hPutStrLn, stderr)

import Error(ErrorHandle)
import Flags(Flags)
import Position(noPosition)
import FStringCompat(mkFString)
import Id
import PreIds(idBit, idBool)
import IntLit(IntLit(..))
import PPrint(ppReadable)
import CSyntax
import CType(cTNum)
import Type(fn, tBit)
import Prim(PrimOp(..))
import ISyntax
import ISyntaxUtil(iGetType)
import BinUtil(BinMap, HashMap, readImports, replaceImportedSignatures)
import MakeSymTab(mkSymTab, cConvInst)
import CtxRed(cCtxReduceIO)
import TypeCheck(cTypeCheck)
import TIMonad(mergeCATFCaches)
import IConv(iConvPackage)
import FixupDefs(fixupDefs, mkDictBuckets, mkDictRedirects)
import ISimpDicts(iSimpDicts)
import ISimplify(iSimplify)
import IExpand(iExpand)
import IExpandUtils(HeapData)
import GenFuncWrap(genFuncWrap, addFuncWrap)
import GenWrap(genWrap, WrapInfo(..))
import Deriving(derive)
import Pragma(Pragma(..))

-- A function of the packed bits: its name, the width of its argument
-- and result, and its body given the argument expression
data Query = Query { q_name :: String
                   , q_argWidth :: Integer
                   , q_resWidth :: Integer
                   , q_body :: CExpr -> CExpr }

-- An expression over the packed bits of a value.  Widths are in bits;
-- the argument and every constant are unsigned bit vectors.
data BitExpr
    = BArg Integer                        -- the packed value
    | BConst Integer Integer              -- width, value
    | BUndet Integer                      -- width; a don't-care
    | BExtract Integer Integer BitExpr    -- bits hi down to lo, inclusive
    | BConcat [BitExpr]                   -- most significant first
    | BIf BitExpr BitExpr BitExpr
    | BCase BitExpr BitExpr [(BitExpr, BitExpr)]  -- scrutinee, default, arms
    | BOp String Integer [BitExpr]        -- operator, result width, operands
    deriving (Eq, Show)

bitExprWidth :: BitExpr -> Integer
bitExprWidth (BArg w) = w
bitExprWidth (BConst w _) = w
bitExprWidth (BUndet w) = w
bitExprWidth (BExtract hi lo _) = hi - lo + 1
bitExprWidth (BConcat es) = sum (map bitExprWidth es)
bitExprWidth (BIf _ t _) = bitExprWidth t
bitExprWidth (BCase _ d _) = bitExprWidth d
bitExprWidth (BOp _ w _) = w

-- The bits of the argument an expression can depend on
bitExprSupport :: BitExpr -> S.Set Integer
bitExprSupport (BArg w) = S.fromList [0 .. w - 1]
bitExprSupport (BConst _ _) = S.empty
bitExprSupport (BUndet _) = S.empty
bitExprSupport (BExtract hi lo (BArg _)) = S.fromList [lo .. hi]
bitExprSupport (BExtract _ _ e) = bitExprSupport e
bitExprSupport (BConcat es) = S.unions (map bitExprSupport es)
bitExprSupport (BIf c t f) = S.unions (map bitExprSupport [c, t, f])
bitExprSupport (BCase s d arms) =
    S.unions (bitExprSupport s : bitExprSupport d :
              [ S.union (bitExprSupport c) (bitExprSupport e) | (c, e) <- arms ])
bitExprSupport (BOp _ _ es) = S.unions (map bitExprSupport es)

-- The value of an expression for the given argument; Nothing when the
-- result depends on a don't-care.  Comparison and shift operators take
-- their operands' widths from the operands; signed operators read the
-- top bit as the sign.
evalBitExpr :: BitExpr -> Integer -> Maybe Integer
evalBitExpr e0 arg = go e0
  where
    go (BArg _) = Just arg
    go (BConst _ v) = Just v
    go (BUndet _) = Nothing
    go (BExtract hi lo e) = do
        v <- go e
        return ((v `shiftR` fromInteger lo) .&. mask (hi - lo + 1))
    go (BConcat es) =
        let cat acc e = do a <- acc
                           v <- go e
                           return ((a `shiftL` fromInteger (bitExprWidth e)) .|. v)
        in  foldl' cat (Just 0) es
    go (BIf c t f) = do
        cv <- go c
        if cv /= 0 then go t else go f
    go (BCase s d arms) = do
        sv <- go s
        let pick [] = go d
            pick ((c, e) : rest) = do
                cv <- go c
                if cv == sv then go e else pick rest
        pick arms
    go (BOp op w es) = do
        vs <- mapM go es
        v <- apply op (zip vs (map bitExprWidth es))
        return (v .&. mask w)

    mask w = (1 `shiftL` fromInteger w) - 1
    signed (v, w) = if w > 0 && testBit v (fromInteger (w - 1))
                    then v - (1 `shiftL` fromInteger w)
                    else v
    bool b = if b then 1 else 0

    apply "eq"  [(a, _), (b, _)] = Just (bool (a == b))
    apply "ult" [(a, _), (b, _)] = Just (bool (a < b))
    apply "ule" [(a, _), (b, _)] = Just (bool (a <= b))
    apply "slt" [a, b] = Just (bool (signed a < signed b))
    apply "sle" [a, b] = Just (bool (signed a <= signed b))
    apply "and" [(a, _), (b, _)] = Just (a .&. b)
    apply "or"  [(a, _), (b, _)] = Just (a .|. b)
    apply "xor" [(a, _), (b, _)] = Just (a `xor` b)
    apply "not" [(a, _)] = Just (complement a)
    apply "neg" [(a, _)] = Just (negate a)
    apply "add" [(a, _), (b, _)] = Just (a + b)
    apply "sub" [(a, _), (b, _)] = Just (a - b)
    apply "mul" [(a, _), (b, _)] = Just (a * b)
    apply "sl"  [(a, _), (b, _)] = Just (a `shiftL` fromInteger b)
    apply "srl" [(a, _), (b, _)] = Just (a `shiftR` fromInteger b)
    apply "sra" [a, (b, _)] = Just (signed a `shiftR` fromInteger b)
    apply "zext" [(a, _)] = Just a
    apply "sext" [a] = Just (signed a)
    apply "trunc" [(a, _)] = Just a
    apply _ _ = Nothing

-- ---------------

-- Reduce batches of queries, each batch over types from the given
-- packages, to expressions over the argument: one result list per
-- batch, in order, holding the queries that reduced.  All batches go
-- through the front end together, since its cost is in the imported
-- packages, not the queries; a query the evaluator cannot reduce is
-- left out of its batch's list, with the reason on stderr.  Should the
-- combined package itself fail (a type error in a generated query),
-- the batches are reduced one at a time instead, and a failing batch
-- is reported as a whole.
reduceQueries :: ErrorHandle -> Flags -> [([Id], [Query])]
              -> IO [Either String [(String, BitExpr)]]
reduceQueries errh flags batches = do
    let pkgs = distinct (concatMap fst batches)
        -- one name space for the queries of all batches
        tagged = [ (k, q { q_name = "q" ++ show k ++ "_" ++ q_name q }, q_name q)
                 | (k, (_, qs)) <- zip [0 :: Int ..] batches, q <- qs ]
    r <- if null tagged then return (Right (M.empty, M.empty, []))
         else attempt (reduce errh flags M.empty M.empty pkgs [ q | (_, q, _) <- tagged ])
    case r of
      Right (_, _, results) -> do
          let byName = M.fromList results
          return [ Right [ (name, e) | (k', q, name) <- tagged, k' == k
                                     , Just e <- [M.lookup (q_name q) byName] ]
                 | (k, _) <- zip [0 :: Int ..] batches ]
      Left msg -> do
          hPutStrLn stderr ("wavedebuginfo: reducing every type's layout at once failed (" ++
                            msg ++ "); reducing them one at a time")
          go M.empty M.empty batches
  where
    -- the packages read so far are reused by the next batch
    go _ _ [] = return []
    go binmap hashmap ((pkgs, qs) : rest) = do
        r <- attempt (reduce errh flags binmap hashmap pkgs qs)
        case r of
          Right (binmap', hashmap', res) -> do
              rest' <- go binmap' hashmap' rest
              return (Right res : rest')
          Left msg -> do
              rest' <- go binmap hashmap rest
              return (Left msg : rest')

-- The list without repeats, first occurrences in their order (nub is
-- quadratic; a design's signal list runs to hundreds of thousands)
distinct :: Ord a => [a] -> [a]
distinct = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
      | S.member x seen = go seen xs
      | otherwise = x : go (S.insert x seen) xs

-- A computation's result, or the compiler's message when it fails
attempt :: IO a -> IO (Either String a)
attempt act =
    (fmap Right act)
      `CE.catches`
        [ CE.Handler (\ (CE.ErrorCall msg) -> return (Left msg))
        , CE.Handler (\ (e :: ExitCode) -> return (Left (show e))) ]

reduce :: ErrorHandle -> Flags -> BinMap HeapData -> HashMap -> [Id] -> [Query]
       -> IO (BinMap HeapData, HashMap, [(String, BitExpr)])
reduce errh flags binmap0 hashmap0 pkgs qs = do
    let pos = noPosition
        pkgId = mkId pos (mkFString "WaveLayout")
        argId = mkId pos (mkFString "packed")
        tBits n = TAp tBit (cTNum n pos)
        qId q = mkId pos (mkFString (q_name q))
        qdef q = CValueSign
                   (CDef (qId q)
                         (CQType [] (tBits (q_argWidth q) `fn` tBits (q_resWidth q)))
                         [CClause [CPVar argId] [] (q_body q (CVar argId))])
        -- each query is a noinline function: the compiler wraps it as a
        -- module whose one method is the function, and the evaluator
        -- reduces that method over a symbolic argument
        cpkg0 = CPackage pkgId (Right []) [ CImpId False p | p <- pkgs ]
                         [] [] (CPragma (Pnoinline (map qId qs)) : map qdef qs) []

    -- the front end, as bsc runs it on a source package, except that the
    -- imports show every definition rather than their exports: a type's
    -- pack and unpack may name constructors and types their package keeps
    -- to itself
    (cpkg1e, binmap, hashmap) <- readImports errh flags binmap0 hashmap0 cpkg0
    let bins = M.elems binmap
        cpkg1 = replaceImportedSignatures cpkg1e [ bo | (_, _, bo, _, _) <- bins ]
    symt00 <- mkSymTab errh cpkg1
    (cpkgF, symt0, funcs) <- genFuncWrap errh flags True cpkg1 symt00
    (cpkgW, gens) <- genWrap errh flags [] True cpkgF symt0
    symt1 <- mkSymTab errh cpkgW
    cpkgA <- addFuncWrap errh symt1 funcs cpkgW
    cpkgD <- derive errh flags symt1 cpkgA
    symt11 <- mkSymTab errh cpkgD
    (cpkg2, _, atfC) <- cCtxReduceIO errh flags symt11 cpkgD
    symt <- mkSymTab errh cpkg2
    (cpkg3, tcErrors, _, atfT) <- cTypeCheck errh flags symt (cConvInst errh symt cpkg2)
    when tcErrors $ CE.throwIO (CE.ErrorCall "type errors in layout queries")
    imod0 <- iConvPackage errh flags symt (mergeCATFCaches atfT atfC) [] cpkg3

    -- link against the imported packages
    let binmods = [ (ipkg, hash) | (_, _, _, ipkg, hash) <- bins ]
        redirects = mkDictRedirects (mkDictBuckets binmods) imod0 binmods
        (imod1, alldefsList) = fixupDefs redirects imod0 binmods
        imod = iSimplify (iSimpDicts imod1)
        alldefs = M.fromList [ (i, e) | IDef i _ e _ <- alldefsList ]
        atf = foldl' mergeIATFCaches (ipkg_atf_cache imod)
                     [ ipkg_atf_cache m | (m, _) <- binmods ]
    -- evaluate each query's wrapper module; its method's value is the
    -- normal form over the method's argument
    let wrappers = M.fromList [ (getIdBaseString (mod_nm wi), wi) | wi <- gens ]
        defs = M.fromList [ (getIdBaseString i, d) | d@(IDef i _ _ _) <- ipkg_defs imod ]
    results <- forM qs $ \ q -> do
        let name = q_name q
        r <- attempt $
          case M.lookup ("module_" ++ name) wrappers >>= \ wi ->
               fmap ((,) wi) (M.lookup (getIdBaseString (wrapped_mod wi)) defs) of
            Just (wi, def) -> do
                em <- iExpand errh flags symt alldefs atf True (wi_prags wi) def
                let dmap = M.fromList [ (i, e) | IDef i _ e _ <- imod_local_defs em ]
                    vals = [ inlineDefs dmap e
                           | IEFace { ief_name = i, ief_value = Just (e, _) } <- imod_interface em
                           , not (isRdyId i), getIdBaseString i == name ]
                case vals of
                  [e] -> either (CE.throwIO . CE.ErrorCall) (return . simplify) (toBitExpr e)
                  _ -> CE.throwIO (CE.ErrorCall "the wrapper has no value")
            Nothing -> CE.throwIO (CE.ErrorCall "the query was not wrapped")
        case r of
          Right e -> return (Just (name, e))
          Left msg -> do
              hPutStrLn stderr ("wavedebuginfo: layout query " ++ name ++ ": " ++ msg)
              return Nothing
    return (binmap, hashmap, catMaybes results)

-- The expression with the module's local definitions substituted in
inlineDefs :: M.Map Id (IExpr HeapData) -> IExpr HeapData -> IExpr HeapData
inlineDefs dmap = go
  where
    go (ICon i (ICValue {})) | Just e <- M.lookup i dmap = go e
    go (IAps f ts es) = IAps (go f) ts (map go es)
    go e = e

-- The normal form the evaluator produced, as a BitExpr
toBitExpr :: IExpr HeapData -> Either String BitExpr
toBitExpr = go
  where
    widthOf :: IExpr HeapData -> Either String Integer
    widthOf e = case bitWidth (iGetType e) of
                  Just w -> Right w
                  Nothing -> Left ("not a bit vector: " ++ ppReadable e)

    go e@(ICon _ (ICMethArg {})) = BArg <$> widthOf e
    go e@(ICon _ (ICInt { iVal = il })) = do
        w <- widthOf e
        return (BConst w (ilValue il .&. ((1 `shiftL` fromInteger w) - 1)))
    go e@(ICon _ (ICUndet {})) = BUndet <$> widthOf e
    go e@(IAps (ICon _ (ICPrim _ p)) ts es) = do
        w <- widthOf e
        prim w p ts es
    go e = Left ("unsupported expression: " ++ ppReadable e)

    prim _ PrimIf _ [c, t, f] = BIf <$> go c <*> go t <*> go f
    prim _ PrimCase _ (s : d : arms) = do
        s' <- go s
        d' <- go d
        arms' <- pairs arms
        return (BCase s' d' arms')
    prim _ PrimSelect [ITNum k, ITNum m, _] [e] = extract (m + k - 1) m <$> go e
    prim _ PrimExtract _ [e, ICon _ (ICInt { iVal = hi }), ICon _ (ICInt { iVal = lo })] =
        extract (ilValue hi) (ilValue lo) <$> go e
    prim _ PrimConcat _ [a, b] = do
        a' <- go a
        b' <- go b
        return (BConcat (parts a' ++ parts b'))
    prim w p _ es | Just name <- lookup p ops = BOp name w <$> mapM go es
    prim _ p _ _ = Left ("unsupported primitive: " ++ show p)

    pairs (c : e : rest) = do
        c' <- go c
        e' <- go e
        rest' <- pairs rest
        return ((c', e') : rest')
    pairs [] = Right []
    pairs _ = Left "odd case arms"

    parts (BConcat es) = es
    parts e = [e]

    extract hi lo e = BExtract hi lo e

    ops = [ (PrimEQ, "eq"), (PrimULT, "ult"), (PrimULE, "ule")
          , (PrimSLT, "slt"), (PrimSLE, "sle")
          , (PrimBAnd, "and"), (PrimBOr, "or"), (PrimBNot, "not")
          , (PrimAnd, "and"), (PrimOr, "or"), (PrimXor, "xor"), (PrimInv, "not")
          , (PrimNeg, "neg"), (PrimAdd, "add"), (PrimSub, "sub"), (PrimMul, "mul")
          , (PrimSL, "sl"), (PrimSRL, "srl"), (PrimSRA, "sra")
          , (PrimZeroExt, "zext"), (PrimSignExt, "sext"), (PrimTrunc, "trunc") ]

-- What the evaluator leaves for later passes to tidy: a choice whose
-- other branch is a don't-care is the one branch; a double negation is
-- its operand; adjacent slices of the value are one slice
simplify :: BitExpr -> BitExpr
simplify e = case e of
    BExtract hi lo x -> extract hi lo (simplify x)
    BConcat es -> case merge (concatMap parts (map simplify es)) of
                    [x] -> x
                    es' -> BConcat es'
    BIf c t f -> case (simplify c, simplify t, simplify f) of
                   (_, t', BUndet _) -> t'
                   (_, BUndet _, f') -> f'
                   (BConst _ 1, t', _) -> t'
                   (BConst _ 0, _, f') -> f'
                   (c', t', f') -> BIf c' t' f'
    BCase s d arms -> BCase (simplify s) (simplify d) [ (simplify c, simplify x) | (c, x) <- arms ]
    BOp "not" _ [x] | BOp "not" _ [y] <- simplify x -> y
    BOp op w es -> BOp op w (map simplify es)
    _ -> e
  where
    parts (BConcat xs) = xs
    parts x = [x]
    merge (BExtract h1 l1 a@(BArg _) : BExtract h2 l2 (BArg _) : rest)
      | l1 == h2 + 1 = merge (extract h1 l2 a : rest)
    merge (x : rest) = x : merge rest
    merge [] = []
    extract hi lo x
      | lo == 0 && hi + 1 == bitExprWidth x = x
    extract hi lo (BExtract _ lo' x) = BExtract (lo' + hi) (lo' + lo) x
    extract hi lo x = BExtract hi lo x

bitWidth :: IType -> Maybe Integer
bitWidth (ITAp (ITCon i _ _) (ITNum n)) | i == idBit = Just n
bitWidth (ITCon i _ _) | i == idBool = Just 1
bitWidth _ = Nothing
