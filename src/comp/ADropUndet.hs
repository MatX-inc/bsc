{-# LANGUAGE CPP #-}
module ADropUndet(aDropUndet) where

import qualified Data.Map.Lazy as M
import Data.Maybe(fromMaybe, isNothing)
import Error(ErrMsg(..), ErrorHandle, bsErrorUnsafe, internalError)
import PPrint
import Position
import Backend
import ASyntax
import ASyntaxUtil
import IntLit(IntLit(..), ilHex)
import IntegerUtil
import Flags
import Prim


-- During elaboration, undetermined values may have been annotated with
-- recommended values (but left as undetermined).  Here we replace them
-- with the recommended value.
--
-- For undetermined values without recommendations, we have two options:
-- leave it for the backend to optimize or pick a value now (so that all
-- backends see the same value, to produce identical simulation results).
-- This choice is specified by the user with the -opt-undetermined-vals flag.
-- If a value is picked, use to value given by the -unspecified-to flag.
--
-- This stage also applies an if-expr optimization [see XXX comments].
-- That is, if one branch of an if-expr is Undet, then it picks the value
-- to be the same as the other branch.  This is not safe to do when the
-- other branch contains the result of an ActionValue method or foreign
-- task, unless -opt-undetermined-vals is given.  (See the "canFixUndet"
-- function in ITransform, which is doing a simillar check).
--
-- Before that, under -split-undetermined-vals, undetermined bits inside an
-- arm of an if-expression take the value of the same bits in the other arm
-- (see fillUndetBits).
--
aDropUndet :: ErrorHandle -> Flags -> APackage -> APackage
aDropUndet errh flags apkg0 =
    let apkg = if fillUndetBitsOn flags
               then fillUndetBits flags apkg0
               else apkg0
        defmap = M.fromList $
                   map (\d -> (adef_objid d, adef_expr d)) $
                     apkg_local_defs apkg
    in  mapAExprs (fixUndet errh flags defmap) apkg


fixUndet :: ErrorHandle -> Flags -> M.Map AId AExpr -> AExpr -> AExpr
fixUndet errh flags defmap = g
  where
        -- whether to pick values for undets without a recommended value
        b = optUndet flags
        -- the value that undets should be set to
        tgt = unSpecTo flags

        -- construct a map of whether each def contains an AVValue
        avmap :: M.Map AId Bool
        avmap = M.map (hasNoActionValue avmap) defmap

        -- recursively apply "f" to an expression
        g = exprMap f

        -- handle an expression
        f :: AExpr -> Maybe AExpr
        -- if the Undet has a recommended value, use it
        f (ASAny _ (Just e)) = Just (g e)
        -- if it doesn't, and the backends are not allowed to diverge,
        -- then replace it with a constant
        f (ASAny t Nothing) | (not b) = Just (mkUnspec errh tgt t)
        -- XXX This is a fixup for ITransform not picking the best value.
        -- XXX The long-term solution is to not pick values in ITransform,
        -- XXX but to wait until the Path Graph is available.
        f (APrim i t PrimIf [_, tt, ff]) =
            let tt'  = g tt
                ff'  = g ff
            in  case (tt,ff) of
                  ((ASAny _ _), _) | canFixUndet flags avmap ff' -> Just ff'
                  (_, (ASAny _ _)) | canFixUndet flags avmap tt' -> Just tt'
                  _ -> Nothing
        -- otherwise, don't change it
        f _ = Nothing


mkUnspec :: ErrorHandle -> String -> AType -> AExpr
mkUnspec errh tgt t =
    let sz = aSize t
        val = case tgt of
                "0" -> 0
                "1" -> (2^sz) -1
                "A" -> aaaa sz
                _   -> -- this situation should have been rejected when
                       -- we checked command-line flags
                       bsErrorUnsafe errh
                           [(noPosition, ENoOptUndetNoXZ tgt)]
    in
        ASInt defaultAId t (ilHex val)


canFixUndet :: Flags -> M.Map AId Bool -> AExpr -> Bool
canFixUndet flags avmap e =
    if (optUndet flags && (backend flags == Just Verilog))
    then True
    else hasNoActionValue avmap e

hasNoActionValue :: M.Map AId Bool -> AExpr -> Bool
hasNoActionValue avm (APrim { ae_args = es }) = all (hasNoActionValue avm) es
hasNoActionValue avm (AMethCall { ae_args = es }) = all (hasNoActionValue avm) es
hasNoActionValue avm (AMethValue {}) = False
hasNoActionValue avm (ATuple _ es) = all (hasNoActionValue avm) es
hasNoActionValue avm (ATupleSel { ae_exp = e }) = hasNoActionValue avm e
hasNoActionValue avm (ANoInlineFunCall { ae_args = es }) = all (hasNoActionValue avm) es
hasNoActionValue avm (AFunCall { ae_args = es }) = all (hasNoActionValue avm) es
hasNoActionValue avm (ATaskValue {}) = False
hasNoActionValue avm (ASPort {}) = True
hasNoActionValue avm (ASParam {}) = True
hasNoActionValue avm (ASDef { ae_objid = i }) =
    case (M.lookup i avm) of
      Just b -> b
      Nothing -> internalError ("hasNoActionValue: ASDef: " ++ ppReadable i)
hasNoActionValue avm (ASInt {}) = True
hasNoActionValue avm (ASReal {}) = True
hasNoActionValue avm (ASStr {}) = True
hasNoActionValue avm (ASAny { ae_val = Nothing }) = True
-- XXX if the recommended value has an AV, we could just drop it
hasNoActionValue avm (ASAny { ae_val = Just e }) = hasNoActionValue avm e
hasNoActionValue avm (ASClock {}) = True
hasNoActionValue avm (ASReset {}) = True
hasNoActionValue avm (ASInout {}) = True
hasNoActionValue avm (AMGate {}) = True

-- -----

-- Under -split-undetermined-vals, undetermined bits that feed an
-- if-expression take the value of the same bits in the other arm.  Those
-- bits then agree, so they leave the mux:
--
--   if c (k ++ _) e  -->  (if c k e[n-1:m]) ++ e[m-1:0]
--
-- This is the whole-arm rule in fixUndet applied to bit ranges.  Each arm is
-- viewed as a concatenation of constants, undetermined bits and bit ranges of
-- other expressions, looking through concatenations, constant extracts,
-- references to defs, and if-expressions whose arms agree.  An arm is only
-- filled from bits that canFixUndet accepts.  Undetermined bits that no arm
-- supplies are left for fixUndet.
--
-- The values are picked here, once, before the backends diverge, and they
-- are 2-state.  A filled bit is a copy of the other arm's bit, and that bit
-- can itself be invalid when the arm is not selected (an RWire that is not
-- written, a RegFile read out of range, the first element of an empty FIFO,
-- uninitialised memory).  So simulation can show X in a filled bit, or
-- Bluesim and Verilog can disagree on it, as they already can for a whole
-- undetermined arm.
--
-- A def that holds undetermined bits can feed several if-expressions; each
-- use is filled separately, by inlining the bits of the def into the mux.
-- Every def is rewritten once and viewed at most once (lazy maps), and an
-- expression without undetermined bits under an if-expression is returned
-- unchanged.
--
fillUndetBits :: Flags -> APackage -> APackage
fillUndetBits flags apkg =
    let defs = apkg_local_defs apkg
        defmap = M.fromList [ (adef_objid d, adef_expr d) | d <- defs ]
        avmap = M.map (hasNoActionValue avmap) defmap

        -- the rewritten defs, and what is known about their bits
        newmap = M.map fillWith defmap
        undetmap = M.map (mayHaveUndetBits env) newmap
        viewmap = M.mapWithKey (viewDef env) newmap

        env = FillEnv { fe_canFill = canFixUndet flags avmap,
                        fe_defUndet = \i -> M.findWithDefault False i undetmap,
                        fe_defView = \i -> M.lookup i viewmap >>= id }

        fillWith e = fromMaybe e (fillExpr env e)

        newDef d = case M.lookup (adef_objid d) newmap of
                     Just e -> d { adef_expr = e }
                     Nothing -> internalError ("fillUndetBits: " ++
                                               ppReadable (adef_objid d))
        apkg' = mapAExprs fillWith (apkg { apkg_local_defs = [] })
    in  apkg' { apkg_local_defs = map newDef defs }

-- The fill runs under -split-undetermined-vals, unless the undetermined
-- values that remain are given to synthesis as X or Z (-opt-undetermined-vals
-- with -unspecified-to X or Z), which can do better with that freedom.
fillUndetBitsOn :: Flags -> Bool
fillUndetBitsOn flags =
    splitUndetVals flags &&
    not (optUndet flags && (unSpecTo flags == "X" || unSpecTo flags == "Z"))

data FillEnv = FillEnv {
        -- whether the bits of an expression may fill undetermined bits
        fe_canFill :: AExpr -> Bool,
        -- whether a def may have undetermined bits (mayHaveUndetBits)
        fe_defUndet :: AId -> Bool,
        -- the bits of a def of Bit type, after filling
        fe_defView :: AId -> Maybe [BitPiece]
    }

-- A run of bits of an expression, as seen by viewBits.
data BitPiece = BPUndet Integer                 -- width
              | BPConst Integer Integer Integer -- width, value, base
              | BPSlice AExpr Integer Integer   -- bits hi..lo of an expression

instance Eq BitPiece where
    BPUndet w == BPUndet w' = w == w'
    BPConst w v _ == BPConst w' v' _ = w == w' && v == v'
    BPSlice e hi lo == BPSlice e' hi' lo' = hi == hi' && lo == lo' && e == e'
    _ == _ = False

pieceWidth :: BitPiece -> Integer
pieceWidth (BPUndet w) = w
pieceWidth (BPConst w _ _) = w
pieceWidth (BPSlice _ hi lo) = hi - lo + 1

isUndetPiece :: BitPiece -> Bool
isUndetPiece (BPUndet _) = True
isUndetPiece _ = False

-- Split a piece into its high k bits and the rest (if any).
splitPiece :: Integer -> BitPiece -> (BitPiece, Maybe BitPiece)
splitPiece k p | k >= pieceWidth p = (p, Nothing)
splitPiece k (BPUndet w) = (BPUndet k, Just (BPUndet (w - k)))
splitPiece k (BPConst w v b) =
    let (hv, lv) = v `divMod` (2 ^ (w - k))
    in  (BPConst k hv b, Just (BPConst (w - k) lv b))
splitPiece k (BPSlice e hi lo) = (BPSlice e hi (hi - k + 1),
                                  Just (BPSlice e (hi - k) lo))

-- Drop or take the high n bits of a list of pieces (high bits first).
dropBits :: Integer -> [BitPiece] -> [BitPiece]
dropBits n ps | n <= 0 = ps
dropBits _ [] = []
dropBits n (p:ps) =
    case splitPiece n p of
      (_, Just rest) -> rest : ps
      (_, Nothing) -> dropBits (n - pieceWidth p) ps

takeBits :: Integer -> [BitPiece] -> [BitPiece]
takeBits n _ | n <= 0 = []
takeBits _ [] = []
takeBits n (p:ps) =
    case splitPiece n p of
      (h, Just _) -> [h]
      (_, Nothing) -> p : takeBits (n - pieceWidth p) ps

-- Join neighbouring pieces of the same kind, and drop empty ones.
mergePieces :: [BitPiece] -> [BitPiece]
mergePieces (p:ps) | pieceWidth p == 0 = mergePieces ps
mergePieces (BPUndet w : BPUndet w' : ps) = mergePieces (BPUndet (w + w') : ps)
mergePieces (BPConst w v b : BPConst w' v' _ : ps) =
    mergePieces (BPConst (w + w') (v * 2 ^ w' + v') b : ps)
mergePieces (BPSlice e hi lo : BPSlice e' hi' lo' : ps)
    | lo == hi' + 1 && e == e' = mergePieces (BPSlice e hi lo' : ps)
mergePieces (p:ps) = p : mergePieces ps
mergePieces [] = []

-- Split two views of the same width at the boundaries of both.
alignPieces :: [BitPiece] -> [BitPiece] -> [(BitPiece, BitPiece)]
alignPieces [] [] = []
alignPieces (p:ps) (q:qs) =
    let k = min (pieceWidth p) (pieceWidth q)
        (p1, mp2) = splitPiece k p
        (q1, mq2) = splitPiece k q
        cons2 = maybe id (:)
    in  (p1, q1) : alignPieces (cons2 mp2 ps) (cons2 mq2 qs)
alignPieces ps qs =
    internalError ("ADropUndet.alignPieces: width mismatch " ++
                   show (map pieceWidth ps, map pieceWidth qs))

pieceExpr :: BitPiece -> AExpr
pieceExpr (BPUndet w) = ASAny (ATBit w) Nothing
pieceExpr (BPConst w v b) = ASInt defaultAId (ATBit w) (IntLit (Just w) b v)
pieceExpr (BPSlice e hi lo)
    | lo == 0 && hi + 1 == aSize e = e
    | otherwise = APrim defaultAId (ATBit (hi - lo + 1)) PrimExtract
                      [e, aNat hi, aNat lo]

piecesExpr :: [BitPiece] -> AExpr
piecesExpr ps =
    case map pieceExpr (mergePieces ps) of
      [e] -> e
      es -> APrim defaultAId (ATBit (sum (map pieceWidth ps))) PrimConcat es

-- Whether viewBits of an expression can contain undetermined bits.
-- It follows the same constructs as viewBits.
mayHaveUndetBits :: FillEnv -> AExpr -> Bool
mayHaveUndetBits _ (ASAny _ Nothing) = True
mayHaveUndetBits env (ASAny _ (Just e)) = mayHaveUndetBits env e
mayHaveUndetBits env (ASDef _ i) = fe_defUndet env i
mayHaveUndetBits env (APrim _ _ PrimConcat es) = any (mayHaveUndetBits env) es
mayHaveUndetBits env (APrim _ _ PrimExtract (e:_)) = mayHaveUndetBits env e
mayHaveUndetBits env (APrim _ _ PrimIf [_, tt, ff]) =
    mayHaveUndetBits env tt || mayHaveUndetBits env ff
mayHaveUndetBits _ _ = False

-- The bits of an expression of type Bit n (n > 0), high bits first.
viewBits :: FillEnv -> AExpr -> [BitPiece]
viewBits _ (ASInt _ (ATBit w) il) =
    [BPConst w (ilValue il `mod` (2 ^ w)) (ilBase il)]
viewBits _ (ASAny (ATBit w) Nothing) = [BPUndet w]
viewBits env (ASAny _ (Just e)) = viewBits env e
viewBits env e@(ASDef (ATBit w) i) =
    fromMaybe [BPSlice e (w - 1) 0] (fe_defView env i)
viewBits env (APrim _ _ PrimConcat es) =
    mergePieces (concatMap (viewBits env) es)
viewBits env (APrim _ (ATBit w) PrimExtract
                  [e, ASInt _ _ hi, ASInt _ _ lo])
    | ilValue hi - ilValue lo + 1 == w =
    takeBits w (dropBits (aSize e - 1 - ilValue hi) (viewBits env e))
viewBits env (APrim i (ATBit _) PrimIf [c, tt, ff]) =
    case (tt, ff) of
      -- fixUndet will replace the if-expression by the other arm
      (ASAny {}, _) | fe_canFill env ff -> viewBits env ff
      (_, ASAny {}) | fe_canFill env tt -> viewBits env tt
      -- bits where both arms agree are seen through the mux,
      -- and the other bits are a narrower if-expression
      _ -> let seg (p, q) | p == q = FSBoth p False
                          | otherwise = FSMux p q
               segs = map seg (alignPieces (viewBits env tt) (viewBits env ff))
               runPieces r@(FSMux {} : _) =
                   let e = muxRunExpr i c r
                   in  [BPSlice e (aSize e - 1) 0]
               runPieces r = [ p | FSBoth p _ <- r ]
           in  mergePieces (concatMap runPieces (segRuns segs))
viewBits _ e = [BPSlice e (aSize e - 1) 0]

-- The view of a def of type Bit n (n > 0).  Bits of expressions other than
-- variables are given as bits of the def, so that filling never copies logic.
viewDef :: FillEnv -> AId -> AExpr -> Maybe [BitPiece]
viewDef env i e =
    case aType e of
      t@(ATBit w) | w > 0 ->
          let ref = ASDef t i
              local hi p@(BPSlice x _ _) | not (isVarExpr x) =
                  BPSlice ref hi (hi - pieceWidth p + 1)
              local _ p = p
              ps = viewBits env e
              his = scanl (\hi p -> hi - pieceWidth p) (w - 1) ps
          in  Just (mergePieces (zipWith local his ps))
      _ -> Nothing

isVarExpr :: AExpr -> Bool
isVarExpr (ASDef {}) = True
isVarExpr (ASPort {}) = True
isVarExpr (ASParam {}) = True
isVarExpr _ = False

-- Fill the undetermined bits under if-expressions; Nothing if unchanged.
fillExpr :: FillEnv -> AExpr -> Maybe AExpr
fillExpr env (APrim i t PrimIf [c, tt, ff]) =
    let mc = fillExpr env c
        mt = fillExpr env tt
        mf = fillExpr env ff
        c' = fromMaybe c mc
        tt' = fromMaybe tt mt
        ff' = fromMaybe ff mf
        rebuilt | all isNothing [mc, mt, mf] = Nothing
                | otherwise = Just (APrim i t PrimIf [c', tt', ff'])
    in  case fillIf env i t c' tt' ff' of
          Just e' -> Just e'
          Nothing -> rebuilt
fillExpr env (APrim i t op es) = fmap (APrim i t op) (fillExprs env es)
fillExpr env (AMethCall t i m es) = fmap (AMethCall t i m) (fillExprs env es)
fillExpr env (ATuple t es) = fmap (ATuple t) (fillExprs env es)
fillExpr env (ATupleSel t e n) = fmap (\e' -> ATupleSel t e' n) (fillExpr env e)
fillExpr env (ANoInlineFunCall t i f es) =
    fmap (ANoInlineFunCall t i f) (fillExprs env es)
fillExpr env (AFunCall t i f isC es) =
    fmap (AFunCall t i f isC) (fillExprs env es)
fillExpr env (ASAny t (Just e)) = fmap (ASAny t . Just) (fillExpr env e)
fillExpr _ _ = Nothing

fillExprs :: FillEnv -> [AExpr] -> Maybe [AExpr]
fillExprs env es =
    let mes = map (fillExpr env) es
    in  if all isNothing mes then Nothing else Just (zipWith fromMaybe es mes)

-- The segments of the result of an if-expression whose undetermined bits
-- are being filled: bits given by both arms, or a mux of the two arms.
data FillSeg = FSBoth BitPiece Bool        -- the bits; whether they were filled
             | FSMux BitPiece BitPiece

-- Group neighbouring segments of the same kind.
segRuns :: [FillSeg] -> [[FillSeg]]
segRuns [] = []
segRuns ss@(s:_) = let (r, rest) = span (\s' -> isMux s' == isMux s) ss
                   in  r : segRuns rest
  where isMux (FSMux {}) = True
        isMux _ = False

-- The if-expression for a run of FSMux segments.
muxRunExpr :: AId -> AExpr -> [FillSeg] -> AExpr
muxRunExpr i c r =
    let ps = [ p | FSMux p _ <- r ]
        qs = [ q | FSMux _ q <- r ]
    in  APrim i (ATBit (sum (map pieceWidth ps))) PrimIf
            [c, piecesExpr ps, piecesExpr qs]

fillIf :: FillEnv -> AId -> AType -> AExpr -> AExpr -> AExpr -> Maybe AExpr
fillIf env i t@(ATBit w) c tt ff
    | w > 0
    -- whole undetermined arms are fixUndet's case
    , not (isASAny tt || isASAny ff)
    , mayHaveUndetBits env tt || mayHaveUndetBits env ff =
    let canFill (BPConst {}) = True
        canFill (BPSlice e _ _) = fe_canFill env e
        canFill (BPUndet _) = False
        seg (p, q) | isUndetPiece p && canFill q = FSBoth q True
                   | isUndetPiece q && canFill p = FSBoth p True
                   | p == q = FSBoth p False
                   | otherwise = FSMux p q
        segs = map seg (alignPieces (viewBits env tt) (viewBits env ff))
        filled (FSBoth _ b) = b
        filled _ = False
        runExpr r@(FSMux {} : _) = muxRunExpr i c r
        runExpr r = piecesExpr [ p | FSBoth p _ <- r ]
    in  if any filled segs
        then case map runExpr (segRuns segs) of
               [e] -> Just e
               es -> Just (APrim defaultAId t PrimConcat es)
        else Nothing
fillIf _ _ _ _ _ _ = Nothing
