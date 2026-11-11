-- IExpand's rebuild of the capture's type events (step 1.2; CaptureEvents):
-- the evaluator's frozen event log becomes type observations (ObsFact) on
-- the defs (and ports) of the expanded module, which the names' operations
-- then carry through every later merge (CaptureLive).
--
-- An event names a cell.  The cells that hold the same value (joined by
-- materialisation and copy links) form a class; each member that
-- collPtrs reached was CSE'd by eqPtrs to a canonical cell, which pDef made
-- a def, or inlined (a port's value goes to the port's record; any other
-- inlined value is only counted).  This is identity, not propagation: no
-- type moves through an expression.  Windows (concat, select, extract and
-- extensions of a def's cells) are read from the canonical cells' values,
-- and so are the declared types of step 1.4: the arrays a def's value holds
-- (on the def and on each element's def), dynamic-select results and Bools
-- built by a comparison, a Bool operator or a constructor test.
--
-- Pure, and it forces nothing production did not: the cells' constructors
-- and pDef's results (forced by the module's dump, ahead of the capture
-- field), and the type arguments and literals of the window primitives,
-- which the def bodies share and the dump deepseq'd.  A def's value is
-- walked as hToDef walks it (an application's arguments, stopping at
-- references and constants), so each head and type argument read is one
-- the def's body shares, and each array is one hToDef turned into a
-- PrimBuildArray (its element pointers collPtrs forced).  A move's argument
-- and result types are read off its function type behind evaluatedness
-- checks (as CaptureEvents.moveSpec).  The log is folded strictly, chunk by
-- chunk, so that the rebuild holds only the classes and the observations.
module CaptureRebuild(typeObservations) where

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map.Strict as M
import qualified Data.Array as Array
import Data.Array.Unboxed(UArray, (!), bounds)
import Data.Array.ST(STUArray, newArray, readArray, writeArray, runSTUArray)
import Control.Monad.ST(ST)
import Control.Monad(when, forM_)
import Data.Bits((.&.), shiftL, shiftR)
import Data.List(foldl')

import Id(Id)
import IntLit(IntLit(..))
import Prim(PrimOp(..))
import ISyntax
import IType(tCode)
import IExpandUtils(HeapCell(..), HeapData, PExpr(..), HExpr, HDef, HeapPointer)
import CaptureLive(NameFact(..), ObsL(..), CapTy(..), perturbing, ptWindows, ptDecl,
                   rotate)
import CaptureEvents
import DictPool(isEvaluated)

-- an observation's folding key: kind, tier, type code, second type code,
-- lo, width, src, link
type ObsKey = (Int, Int, Int, Int, Int, Int, Int, Int)

-- the observations with the declared types added, the types they name,
-- and the number of declared rows of each kind
data Decls = Decls !(IM.IntMap (M.Map ObsKey Int)) !(IM.IntMap IType) !(IM.IntMap Int)

-- the observations so far, by canonical cell, the types they name, and
-- counters: attachments, entries reaching no cell collPtrs reached
data Acc = Acc !(IM.IntMap (M.Map ObsKey Int)) !(IM.IntMap IType) !Int !Int

typeObservations :: CapLogF -> IM.IntMap HeapCell -> IM.IntMap HeapPointer ->
                    [(HeapPointer, (HExpr, Maybe HDef))] ->
                    ([(Id, [NameFact])], [(Int, Int)])
typeObservations lg iheap ptr_map ptr_info =
    let ptran p = IM.findWithDefault p p ptr_map
        reached p = IM.member p iheap
        cellExpr c = case IM.lookup c iheap of
                       Just (HNF { hc_pexpr = P _ e }) -> Just e
                       _ -> Nothing
        -- where a canonical cell's value went: a def, or a port it reads
        target _ (Just (IDef i _ _ _)) = Just i
        target c Nothing = case cellExpr c of
                             Just (ICon pid ic) | isPortCon ic -> Just pid
                             _ -> Nothing
        isPortCon (ICModPort {}) = True
        isPortCon (ICMethArg {}) = True
        isPortCon (ICModParam {}) = True
        isPortCon _ = False

        -- classes of cells holding the same value, and the canonical cells
        -- of each class's reached members
        classes = linkClasses (clf_links lg)
        nlinks = sum [ n `div` 2 | (n, _) <- clf_links lg ]
        classCanon = IM.fromListWith IS.union
                       [ (r, IS.singleton (ptran p)) | (p, r) <- classMembers classes, reached p ]
        -- the canonical cells an event on p reaches, each with its tier
        reach p =
            let own = if reached p then ptran p else -1
                r = classOf classes p
                linked = if r < 0 then IS.empty else IM.findWithDefault IS.empty r classCanon
                cs = if own >= 0 then IS.insert own linked else linked
                tier c | c == own = if p == c then 0 else 1
                       | otherwise = 2
            in  [ (c, tier c) | c <- IS.toList cs ]

        -- attach the observation key on the cell p, with the types it names
        attach :: Acc -> Int -> (Int -> ObsKey) -> [(Int, IType)] -> Acc
        attach acc p key tys = attachTo acc (reach p) key tys
        attachTo (Acc m ts a u) rs key tys =
            case rs of
              [] -> Acc m ts a (u + 1)
              cs -> let m' = foldl' (\ mm (c, t) -> IM.insertWith (M.unionWith (+)) c
                                                           (M.singleton (key t) 1) mm) m cs
                        ts' = foldl' (\ tm (c, t) -> IM.insert c t tm) ts tys
                    in  m' `seq` ts' `seq` Acc m' ts' (a + length cs) u
        acc0 = Acc IM.empty IM.empty 0 0

        types = clf_types lg
        tyOf c = if c < 0 then [] else maybe [] (\ t -> [(c, t)]) (IM.lookup c types)

        -- events: (kind .|. ptr << 8, code); a repack is two entries
        evChunk acc0' (n, arr) = go acc0' 0
          where
            go a i
              | i + 1 >= n = a
              | otherwise =
                  let w = arr ! i
                      kind = w .&. 0xff
                      p = w `shiftR` 8
                      c = arr ! (i + 1)
                  in  if kind == evRepack && i + 3 < n &&
                         (arr ! (i + 2)) .&. 0xff == evRepackFrom
                      then let c2 = arr ! (i + 3)
                               a' = attach a p (\ t -> (kind, t, c, c2, -1, -1, p, -1))
                                           (tyOf c ++ tyOf c2)
                           in  a' `seq` go a' (i + 4)
                      else let a' = attach a p (\ t -> (kind, t, c, -1, -1, -1, p, -1)) (tyOf c)
                           in  a' `seq` go a' (i + 2)
        Acc evm evts eatt eunr = foldl' evChunk acc0 (clf_events lg)
        nevents = sum [ n `div` 2 | (n, _) <- clf_events lg ]

        -- moves: (child .|. k << 40, parent .|. move type << 40)
        mtypes = clf_mtypes lg
        mvChunk acc0' (n, arr) = go acc0' 0
          where
            go a i
              | i + 1 >= n = a
              | otherwise =
                  let w1 = arr ! i
                      w2 = arr ! (i + 1)
                      child = w1 .&. mask40
                      k = w1 `shiftR` 40
                      parent = w2 .&. mask40
                      (fc, ar) = IM.findWithDefault (-1, 0) (w2 `shiftR` 40) mtypes
                      a' = case reach child of
                             [] -> attachTo a [] (const (0, 0, 0, 0, 0, 0, 0, 0)) []
                             rs -> let (ac, at, rc, rt) = moveTypes fc ar k
                                       link = if reached parent then ptran parent else -1
                                   in  attachTo a rs
                                                (\ t -> (evMove, t, ac, rc, k, -1, child, link))
                                                (at ++ rt)
                  in  a' `seq` go a' (i + 2)
        mask40 = (1 `shiftL` 40) - 1 :: Int
        -- argument k's type and the result type, off the function type
        moveTypes fc n k =
            case IM.lookup fc types of
              Nothing -> (-1, [], -1, [])
              Just ft -> let (ac, at) = known (arrowArg k ft)
                             (rc, rt) = known (arrowRes n ft)
                         in  (ac, at, rc, rt)
        known (Just t) | isEvaluated t = let c = tCode t in (c, if c < 0 then [] else [(c, t)])
        known _ = (-1, [])
        Acc obsm obsts matt munr = foldl' mvChunk (Acc evm evts 0 0) (clf_moves lg)
        nmoves = sum [ n `div` 2 | (n, _) <- clf_moves lg ]

        -- windows
        winObs = concat [ windows c e | (c, (_, Just (IDef {}))) <- ptr_info,
                                        Just e <- [cellExpr c] ]
        windows c e =
            [ (c, (kind, 0, -1, -1, lo', w', c, ptran q))
            | (kind, IRefT _ q _ _, lo, w) <- winArgs e,
              let (lo', w') = if perturbing ptWindows then (w, lo) else (lo, w) ]
        -- declared types (step 1.4), each on the def or element cell's
        -- canonical cell
        decls0 = concat [ declared c e | (c, (_, Just (IDef {}))) <- ptr_info,
                                         Just e <- [cellExpr c] ]
        declared c e =
            rootDecl c e ++
            concat [ (c, evArrayOf, 0, Just et, -1, n, c, -1) :
                     [ (c, nestedSel, 0, Nothing, -1, -1, c, -1) | nsel ] ++
                     [ (ptran p, evArrayElem, if ptran p == p then 0 else 1, Just et, k, n, p, c)
                     | (k, ArrayCell p _) <- zip [0 ..] cells, reached p ]
                   | (et, nsel, cells) <- arraysOf True e, let n = length cells ]
        -- a dynamic select below a def's root: counted, not recorded
        nestedSel = -1
        decls | perturbing ptDecl =
                  let tys = rotate [ t | (_, _, _, Just t, _, _, _, _) <- decls0 ]
                      retype (r@(_, _, _, Nothing, _, _, _, _) : rs) ts = r : retype rs ts
                      retype ((c, k, tr, Just _, lo, w, src, l) : rs) (t : ts) =
                          (c, k, tr, Just t, lo, w, src, l) : retype rs ts
                      retype rs _ = rs
                      wrong (c, k, tr, t, lo, w, src, l)
                        | k == evBoolBy = (c, k, tr, t, lo `mod` nBoolOps + 1, w, src, l)
                        | k == evArrayElem = (c, k, tr, t, w - 1 - lo, w, src, l)
                        | otherwise = (c, k, tr, t, lo, w, src, l)
                  in  map wrong (retype decls0 tys)
              | otherwise = decls0
        addObs mm c key = IM.insertWith (M.unionWith (+)) c (M.singleton key 1) mm
        addDecl (Decls mm tm cn) (_, k, _, _, _, _, _, _)
          | k == nestedSel = let cn' = IM.insertWith (+) k 1 cn in cn' `seq` Decls mm tm cn'
        addDecl (Decls mm tm cn) (c, k, tr, t, lo, w, src, l) =
            let tc = maybe (-1) tCode t
                mm' = addObs mm c (k, tr, tc, -1, lo, w, src, l)
                tm' = case t of
                        Just t' | tc >= 0 -> IM.insert tc t' tm
                        _ -> tm
                cn' = IM.insertWith (+) k 1 cn
            in  mm' `seq` tm' `seq` cn' `seq` Decls mm' tm' cn'
        Decls allm allts declCounts =
            foldl' addDecl (Decls (foldl' (\ mm (c, k) -> addObs mm c k) obsm winObs) obsts IM.empty)
                   decls
        declCount k = IM.findWithDefault 0 k declCounts

        ty c | c < 0 = NoTy
             | otherwise = maybe NoTy Ty (IM.lookup c allts)
        obsFact ((kind, tier, c1, c2, lo, w, src, link), cnt) =
            ObsFact (ObsL kind tier (ty c1) (ty c2) lo w src link cnt)
        -- each observed canonical cell's def or port (one pass over ptr_info)
        targeted = [ (i, m) | (c, (_, md)) <- ptr_info, Just m <- [IM.lookup c allm],
                              Just i <- [target c md] ]
        recs = [ (i, map obsFact (M.toList m))
               | (i, m) <- M.toList (M.fromListWith (M.unionWith (+)) targeted) ]
        ninl = IM.size allm - length targeted

        counts = clf_counts lg
        stats = [ (10, nevents), (11, eatt), (12, eunr), (13, ninl)
                , (14, nlinks)
                , (16, nmoves), (17, matt), (18, munr)
                , (20, length winObs)
                , (30, declCount evArrayOf), (31, declCount evArrayElem)
                , (32, declCount evDynSelResult), (33, declCount evBoolBy)
                , (34, declCount nestedSel)
                , (21, counts ! ctNoCell), (22, counts ! ctDictMove)
                , (23, counts ! ctMoveUnknown), (24, counts ! ctBigMove) ]
    in  (recs, stats)

-- argument k (0-based) of a function type, and its result after n
-- arrows, when the arrows on the way are evaluated
arrowArg :: Int -> IType -> Maybe IType
arrowArg k t
  | isEvaluated t, ITAp l r <- t, isEvaluated l, ITAp _ a <- l =
      if k == 0 then Just a else arrowArg (k - 1) r
  | otherwise = Nothing

arrowRes :: Int -> IType -> Maybe IType
arrowRes 0 t = Just t
arrowRes n t
  | isEvaluated t, ITAp l r <- t, isEvaluated l, ITAp _ _ <- l = arrowRes (n - 1) r
  | otherwise = Nothing

-- The window primitives: for each argument, its kind, offset and width.
winArgs :: HExpr -> [(Int, HExpr, Int, Int)]
winArgs (IAps (ICon _ (ICPrim _ op)) ts es) =
    case (op, ts, es) of
      (PrimConcat, [ITNum a, ITNum b, _], [x, y]) ->
          [(evWinConcat, x, int b, int a), (evWinConcat, y, 0, int b)]
      (PrimSelect, [ITNum w, ITNum lo, _], [x]) ->
          [(evWinSelect, x, int lo, int w)]
      (PrimExtract, [_, _, ITNum w], [x, ICon _ (ICInt { iVal = IntLit { ilValue = h } }),
                                         ICon _ (ICInt { iVal = IntLit { ilValue = l } })])
          | h - l + 1 == w -> [(evWinSelect, x, int l, int w)]
      (PrimZeroExt, [_, ITNum n, _], [x]) -> [(evWinZeroExt, x, 0, int n)]
      (PrimSignExt, [_, ITNum n, _], [x]) -> [(evWinSignExt, x, 0, int n)]
      _ -> []
  where int :: Integer -> Int
        int = fromInteger
winArgs _ = []

-- A def's own declared type, from the head of its value: a dynamic
-- select's element type, or Bool for a comparison, a Bool operator or a
-- constructor test.
rootDecl :: Int -> HExpr -> [(Int, Int, Int, Maybe IType, Int, Int, Int, Int)]
rootDecl c (IAps (ICon _ (ICPrim _ PrimArrayDynSelect)) (elty : ITNum isz : _) _) =
    [(c, evDynSelResult, 0, Just elty, -1, fromInteger isz, c, -1)]
rootDecl c (IAps (ICon _ (ICPrim _ op)) _ _) | Just b <- boolOp op =
    [(c, evBoolBy, 0, Nothing, b, -1, c, -1)]
rootDecl c (IAps (ICon _ (ICIs {})) _ _) = [(c, evBoolBy, 0, Nothing, boolIs, -1, c, -1)]
rootDecl _ _ = []

boolOp :: PrimOp -> Maybe Int
boolOp PrimEQ = Just boolEQ
boolOp PrimULT = Just boolULT
boolOp PrimULE = Just boolULE
boolOp PrimSLT = Just boolSLT
boolOp PrimSLE = Just boolSLE
boolOp PrimBNot = Just boolBNot
boolOp PrimBAnd = Just boolBAnd
boolOp PrimBOr = Just boolBOr
boolOp _ = Nothing

-- The arrays in a def's value (at its root when the flag is set), walked
-- as hToDef walks it, each with its element type, whether a dynamic select
-- below the root selects from it (that result has no def of its own, so it
-- is only counted), and its elements.  The element type is a dynamic
-- select's (the type of its elements' cells; the array's own type is the
-- one before the select's arguments were applied), else the array type's
-- argument, which hToDef matched.
arraysOf :: Bool -> HExpr -> [(IType, Bool, [ArrayCell HeapData])]
arraysOf root (IAps (ICon _ (ICPrim _ PrimArrayDynSelect)) (elty : _)
                    (ICon _ (ICLazyArray { iArray = arr, uninit = Nothing }) : es)) =
    (elty, not root, Array.elems arr) : concatMap (arraysOf False) es
arraysOf _ (IAps _ _ es) = concatMap (arraysOf False) es
arraysOf _ (ICon _ (ICLazyArray { iConType = ITAp _ elty, iArray = arr, uninit = Nothing })) =
    [(elty, False, Array.elems arr)]
arraysOf _ _ = []

-- -----
-- Classes of linked cells: union-find over the links' cells, numbered by
-- their rank among the distinct cells (unboxed: two Ints a cell).

data Classes = Classes !(UArray Int Int)   -- the distinct cells, ascending
                       !(UArray Int Int)   -- each one's class (a rank)

linkClasses :: [(Int, UArray Int Int)] -> Classes
linkClasses chunks =
    let nends = sum [ n | (n, _) <- chunks ]
        -- the distinct cells, ascending
        cells = runSTUArray $ do
                  a <- newArray (0, nends - 1) 0
                  let fill j [] = return j
                      fill j ((n, arr) : rest) = do
                        let go j' i | i + 1 >= n = return j'
                                    | otherwise = do
                                        writeArray a j' ((arr ! i) `shiftR` 8)
                                        writeArray a (j' + 1) (arr ! (i + 1))
                                        go (j' + 2) (i + 2)
                        j2 <- go j 0
                        fill j2 rest
                  _ <- fill 0 chunks
                  heapSort a nends
                  m <- dedup a nends
                  b <- newArray (0, m - 1) 0
                  forM_ [0 .. m - 1] $ \ i -> readArray a i >>= writeArray b i
                  return b
        (_, hi) = bounds cells
        m = hi + 1
        roots = runSTUArray $ do
                  parent <- newArray (0, m - 1) 0
                  forM_ [0 .. m - 1] $ \ i -> writeArray parent i i
                  forM_ chunks $ \ (n, arr) ->
                    let go i | i + 1 >= n = return ()
                             | otherwise = do
                                 let d = search cells ((arr ! i) `shiftR` 8)
                                     s = search cells (arr ! (i + 1))
                                 when (d >= 0 && s >= 0) $ union parent d s
                                 go (i + 2)
                    in  go 0
                  forM_ [0 .. m - 1] $ \ i -> find parent i >>= writeArray parent i
                  return parent
    in  Classes cells roots

-- the class of the cell p, -1 when no link names it
classOf :: Classes -> Int -> Int
classOf (Classes cells roots) p =
    let i = search cells p in if i < 0 then -1 else roots ! i

-- every linked cell with its class
classMembers :: Classes -> [(Int, Int)]
classMembers (Classes cells roots) =
    let (_, hi) = bounds cells in [ (cells ! i, roots ! i) | i <- [0 .. hi] ]

-- the index of p in the ascending array, or -1
search :: UArray Int Int -> Int -> Int
search a p = go 0 hi
  where (_, hi) = bounds a
        go lo h | lo > h = -1
                | otherwise = let mid = (lo + h) `div` 2
                                  v = a ! mid
                              in  if v == p then mid
                                  else if v < p then go (mid + 1) h else go lo (mid - 1)

find :: STUArray s Int Int -> Int -> ST s Int
find parent i = do
  p <- readArray parent i
  if p == i then return i
  else do g <- readArray parent p
          writeArray parent i g      -- path halving
          if g == p then return p else find parent g

union :: STUArray s Int Int -> Int -> Int -> ST s ()
union parent a b = do
  ra <- find parent a
  rb <- find parent b
  when (ra /= rb) $ if ra < rb then writeArray parent rb ra else writeArray parent ra rb

-- the first n elements in place, ascending
heapSort :: STUArray s Int Int -> Int -> ST s ()
heapSort a n = do
  let sift root end = do
        let child = 2 * root + 1
        when (child <= end) $ do
          c1 <- readArray a child
          (sw, sv) <- if child + 1 <= end
                      then do c2 <- readArray a (child + 1)
                              return (if c2 > c1 then (child + 1, c2) else (child, c1))
                      else return (child, c1)
          rv <- readArray a root
          when (sv > rv) $ do
            writeArray a root sv
            writeArray a sw rv
            sift sw end
  forM_ [(n - 2) `div` 2, (n - 2) `div` 2 - 1 .. 0] $ \ s -> sift s (n - 1)
  forM_ [n - 1, n - 2 .. 1] $ \ e -> do
    x <- readArray a 0
    y <- readArray a e
    writeArray a 0 y
    writeArray a e x
    sift 0 (e - 1)

-- remove repeats from the first n (sorted) elements; their number
dedup :: STUArray s Int Int -> Int -> ST s Int
dedup _ 0 = return 0
dedup a n = go 1 1
  where go r w | r >= n = return w
               | otherwise = do
                   v <- readArray a r
                   prev <- readArray a (w - 1)
                   if v == prev then go (r + 1) w
                   else do writeArray a w v
                           go (r + 1) (w + 1)
