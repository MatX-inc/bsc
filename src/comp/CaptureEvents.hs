-- The evaluator's type-event log (step 1.2 of the capture; CaptureLive).
--
-- IExpand records, as it runs, the pack/unpack coercions it holds, cancels
-- or applies eagerly, the materialisation and copy links that put one value
-- in two cells, and the constructor moves that give a tuple or constructor
-- argument a cell of its own.  The log lives in GRefs beside the counters,
-- so that a push reads and forces no evaluator state; it is unboxed and
-- append-only (16 bytes an event, link or move), and is frozen after
-- runG for the rebuild (CaptureRebuild), which turns it into observations
-- on the module's defs.
--
-- Forcing.  A push takes only Ints in hand (heap pointers from the hp
-- counter, from references the evaluator has already matched, or from the
-- held nodes' strict pointer fields) and types the evaluator has already
-- compared, so evaluated; tCode reads a strict field.  The one exception
-- is a move's result type (and its head), which production does not
-- always force: it is read only behind an evaluatedness check
-- (moveSpec), and an unevaluated one is logged as unknown.
module CaptureEvents(
                     CapLog, newCapLog, CapLogF(..), freezeCapLog,
                     logEvent, logRepack, logLink, MoveSpec(..), moveSpec, logMove,
                     logBump,
                     -- event kinds (also the frozen or_kind codes)
                     evPackHold, evPackCancel, evPackUndet, evUnpackHold,
                     evUnpackCancel, evUnpackUndet, evUnpackConst, evRepack,
                     evPackMerge, evUnpackMerge, evMove,
                     evWinConcat, evWinSelect, evWinZeroExt, evWinSignExt,
                     evRepackFrom,
                     evArrayElem, evArrayOf, evDynSelResult, evBoolBy,
                     -- bool-by operators
                     boolEQ, boolULT, boolULE, boolSLT, boolSLE, boolBNot, boolBAnd,
                     boolBOr, boolIs, nBoolOps,
                     -- link kinds
                     lkMat, lkCopy,
                     -- log counters
                     ctNoCell, ctDictMove, ctMoveUnknown, ctBigMove
                    ) where

import Data.IORef
import Data.Bits((.|.), shiftL)
import Data.Array.IO(IOUArray)
import Data.Array.Unboxed(UArray)
import Data.Array.MArray(newArray, freeze, readArray, writeArray)
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Control.Monad(when)

import IType(IType(..), tCode)
import CType(TISort(..), StructSubType(..))
import DictPool(isEvaluated)
import CaptureLive(perturbing, ptEvents, ptCopylinks, ptMoves)

-- -----
-- Kinds

-- Events on a cell, each with the type in hand (the frozen ObsRec kinds,
-- with the moves and the rebuild's windows; Capture.obsKindNames spells
-- them).  The bits are "packed from" or "unpacked to" the type.
evPackHold, evPackCancel, evPackUndet, evUnpackHold, evUnpackCancel,
  evUnpackUndet, evUnpackConst, evRepack, evPackMerge, evUnpackMerge,
  evMove, evWinConcat, evWinSelect, evWinZeroExt,
  evWinSignExt, evRepackFrom :: Int
evPackHold = 1      -- a held pack's applied cell (the bits), packed from ta
evPackCancel = 2    -- pack at ta of an unpack at ta: the unpack's bits
evPackUndet = 3     -- pack at ta of a don't-care, applied at once: its cell
evUnpackHold = 4    -- a held unpack's bits, unpacked to ta
evUnpackCancel = 5  -- unpack at ta of a pack at ta: the pack's bits
evUnpackUndet = 6   -- unpack at ta of a don't-care, applied at once
evUnpackConst = 7   -- unpack at ta of a constant, applied at once
evRepack = 8        -- unpack at ta of the bits of a pack from another type
evPackMerge = 9     -- improveIf merged two held packs at ta: the merged bits
evUnpackMerge = 10  -- improveIf merged two held unpacks at ta: the merged bits
evMove = 11         -- a constructor move: the argument's cell
evWinConcat = 12    -- rebuild: bits [lo, lo+w) of this def are the cell's
evWinSelect = 13    -- rebuild: this def is bits [lo, lo+w) of the cell
evWinZeroExt = 14   -- rebuild: zero-extension of the cell (its w bits at 0)
evWinSignExt = 15   -- rebuild: sign-extension of the cell (its w bits at 0)
evRepackFrom = 16   -- log only: the second entry of a repack, the pack's type

-- Declared types the rebuild reads off a def's own cell (step 1.4)
evArrayElem, evArrayOf, evDynSelResult, evBoolBy :: Int
evArrayElem = 17     -- the def is element lo of a width-element array of the type
evArrayOf = 18       -- the def's value holds a width-element array of the type
evDynSelResult = 19  -- the def is a dynamic select (width-bit index) of an element of the type
evBoolBy = 20        -- the def is a Bool, built by the operator lo (boolOpNames)

-- the operators of bool-by
boolEQ, boolULT, boolULE, boolSLT, boolSLE, boolBNot, boolBAnd, boolBOr,
  boolIs, nBoolOps :: Int
boolEQ = 1
boolULT = 2
boolULE = 3
boolSLT = 4
boolSLE = 5
boolBNot = 6
boolBAnd = 7
boolBOr = 8
boolIs = 9      -- a constructor test
nBoolOps = 9

-- Links between cells that hold the same value, which the rebuild unions
-- (a pack's payload is never linked: it is not a bits value, so never a
-- def's).
lkMat, lkCopy :: Int
lkMat = 1      -- walkNF materialised a held node: holder <- applied cell
lkCopy = 2     -- the cell got another cell's value: cell <- source

-- Counters kept with the log (CaptureRebuild writes them to cap_stats)
ctNoCell, ctDictMove, ctMoveUnknown, ctBigMove :: Int
ctNoCell = 0       -- events and links on a value with no cell (a constant)
ctDictMove = 1     -- moved arguments of dictionary tuples, not logged
ctMoveUnknown = 2  -- logged moved arguments whose function type was unevaluated
ctBigMove = 3      -- moves too big to log (argument 2^23, cell 2^40, 2^23 move types)

nCounters :: Int
nCounters = 4

-- -----
-- The log

-- Each log is a list of unboxed chunks of chunkSize Ints, so that it never
-- copies itself to grow and holds at most one chunk of slack; an entry
-- never straddles two chunks.
chunkSize :: Int
chunkSize = 16384

data LogBuf = LogBuf {-# UNPACK #-} !Int       -- Ints in the current chunk
                     !(IOUArray Int Int)        -- the current chunk
                     [(Int, UArray Int Int)]    -- earlier chunks, newest first

data CapLog = CapLog {
    cl_events :: !(IORef LogBuf),   -- (kind .|. ptr << 8, type code)
    cl_links  :: !(IORef LogBuf),   -- (kind .|. dst << 8, src)
    cl_moves  :: !(IORef LogBuf),   -- (child .|. k << 40, parent .|. move type << 40)
    cl_types  :: !(IORef (IM.IntMap IType)),  -- type code -> type, first sighting
    -- the moves' (function type code, arity) pairs, numbered from 1
    cl_mtypes :: !(IORef (M.Map (Int, Int) Int)),
    cl_prev   :: !(IORef Int),      -- the last event's code (perturbation)
    cl_counts :: !(IOUArray Int Int)
  }

-- The frozen log: each log's chunks in order, with the Ints each holds.
data CapLogF = CapLogF {
    clf_events :: [(Int, UArray Int Int)],
    clf_links  :: [(Int, UArray Int Int)],
    clf_moves  :: [(Int, UArray Int Int)],
    clf_types  :: !(IM.IntMap IType),
    clf_mtypes :: !(IM.IntMap (Int, Int)),   -- move type -> (function type code, arity)
    clf_counts :: !(UArray Int Int)
  }

newBuf :: IO (IORef LogBuf)
newBuf = do a <- newArray (0, chunkSize - 1) 0
            newIORef (LogBuf 0 a [])

newCapLog :: IO CapLog
newCapLog = do
  e <- newBuf; l <- newBuf; m <- newBuf
  ts <- newIORef IM.empty
  p <- newIORef (-1)
  c <- newArray (0, nCounters - 1) 0
  mt <- newIORef M.empty
  return (CapLog e l m ts mt p c)

freezeCapLog :: CapLog -> IO CapLogF
freezeCapLog l = do
  es <- freezeBuf (cl_events l)
  ls <- freezeBuf (cl_links l)
  ms <- freezeBuf (cl_moves l)
  ts <- readIORef (cl_types l)
  mt <- readIORef (cl_mtypes l)
  cs <- freeze (cl_counts l)
  return (CapLogF es ls ms ts (IM.fromList [ (ix, k) | (k, ix) <- M.toList mt ]) cs)
 where freezeBuf r = do LogBuf n a done <- readIORef r
                        fa <- freeze a
                        return (reverse ((n, fa) : done))

-- append k Ints (k <= 4)
push :: IORef LogBuf -> Int -> Int -> Int -> Int -> Int -> IO ()
push ref k a b c d = do
  LogBuf n0 arr0 done0 <- readIORef ref
  (n, arr, done) <-
      if n0 + k <= chunkSize then return (n0, arr0, done0)
      else do fa <- freeze arr0
              arr1 <- newArray (0, chunkSize - 1) 0
              return (0, arr1, (n0, fa) : done0)
  writeArray arr n a
  when (k > 1) $ writeArray arr (n + 1) b
  when (k > 2) $ writeArray arr (n + 2) c
  when (k > 3) $ writeArray arr (n + 3) d
  writeIORef ref $! LogBuf (n + k) arr done

logBump :: CapLog -> Int -> Int -> IO ()
logBump l k n = do
  v <- readArray (cl_counts l) k
  writeArray (cl_counts l) k (v + n)

-- the type's code, keeping the type at its first sighting (-1: none)
typeCode :: CapLog -> IType -> IO Int
typeCode l t = do
  let c = tCode t
  when (c >= 0) $ do
    m <- readIORef (cl_types l)
    when (not (IM.member c m)) $ writeIORef (cl_types l) $! IM.insert c t m
  return c

-- under "events": pack and unpack kinds trade places
flipKind :: Int -> Int
flipKind k
  | k == evPackHold = evUnpackHold
  | k == evUnpackHold = evPackHold
  | k == evPackCancel = evUnpackCancel
  | k == evUnpackCancel = evPackCancel
  | k == evPackUndet = evUnpackUndet
  | k == evUnpackUndet = evPackUndet
  | k == evPackMerge = evUnpackMerge
  | k == evUnpackMerge = evPackMerge
  | otherwise = k

-- An event on the cell at ptr (-1: the value has no cell, only counted).
-- The type was compared by the evaluator, so it is evaluated.
logEvent :: CapLog -> Int -> Int -> IType -> IO ()
logEvent l kind ptr t
  | ptr < 0 = logBump l ctNoCell 1
  | otherwise = do
      c <- typeCode l t
      if perturbing ptEvents
        then do prev <- readIORef (cl_prev l)
                writeIORef (cl_prev l) c
                push (cl_events l) 2 (flipKind kind .|. (ptr `shiftL` 8)) prev 0 0
        else push (cl_events l) 2 (kind .|. (ptr `shiftL` 8)) c 0 0

-- The cell at ptr holds the bits of a pack from tFrom, unpacked to tTo.
logRepack :: CapLog -> Int -> IType -> IType -> IO ()
logRepack l ptr tTo tFrom
  | ptr < 0 = logBump l ctNoCell 1
  | otherwise = do
      cTo <- typeCode l tTo
      cFrom <- typeCode l tFrom
      let (a, b) = if perturbing ptEvents then (cFrom, cTo) else (cTo, cFrom)
      push (cl_events l) 4 (evRepack .|. (ptr `shiftL` 8)) a
                           (evRepackFrom .|. (ptr `shiftL` 8)) b

logLink :: CapLog -> Int -> Int -> Int -> IO ()
logLink l kind dst src
  | dst < 0 || src < 0 = logBump l ctNoCell 1
  | kind == lkCopy && perturbing ptCopylinks =
      push (cl_links l) 2 (kind .|. (src `shiftL` 8)) (dst + 1) 0 0
  | kind /= lkCopy && perturbing ptEvents =
      push (cl_links l) 2 (kind .|. (src `shiftL` 8)) dst 0 0
  | otherwise = push (cl_links l) 2 (kind .|. (dst `shiftL` 8)) src 0 0

-- -----
-- Moves (evalHeap)

-- What a move logs: the parent cell, the arity and the code of the
-- function type the arguments' types come from (-1: not evaluated), or
-- that the result is a dictionary (not logged), or that it is not a move.
-- The rebuild reads the argument and result types off the function type.
data MoveSpec = NoMove
              | DictMove
              | MoveSpec !Int !Int !Int IType

-- The spec of a move of n arguments of the function type ft at the cell
-- parent.  Reads ft's arrows (which takeArgTypes matched) and the result
-- type's head and sort only behind evaluatedness checks (spec 1.2, review
-- correction 13: the result type is sometimes still a thunk).
moveSpec :: Int -> Int -> IType -> MoveSpec
moveSpec parent n ft
  | not (isEvaluated ft) = MoveSpec parent n (-1) ft
  | dict (result n ft) = DictMove
  | otherwise = MoveSpec parent n (tCode ft) ft
  where
    -- the type after k arrows, if they are evaluated
    result :: Int -> IType -> Maybe IType
    result 0 t = Just t
    result k t
      | isEvaluated t, ITAp l r <- t, isEvaluated l, ITAp _ _ <- l = result (k - 1) r
      | otherwise = Nothing
    dict (Just r) = case headOf r of
                      Just (ITCon _ _ s) | isEvaluated s, TIstruct ss _ <- s,
                                           isEvaluated ss, SClass <- ss -> True
                      _ -> False
    dict Nothing = False
    headOf t | not (isEvaluated t) = Nothing
             | ITAp f _ <- t = headOf f
             | otherwise = Just t

-- Argument k of a move went to the cell child (-1: a constant, not logged).
logMove :: CapLog -> MoveSpec -> Int -> Int -> IO ()
logMove _ NoMove _ _ = return ()
logMove l DictMove _ _ = logBump l ctDictMove 1
logMove l (MoveSpec parent n fc ft) k child
  | child < 0 = logBump l ctNoCell 1
  | k >= 1 `shiftL` 23 = logBump l ctBigMove 1
  | otherwise = do
      fc' <- if fc < 0 then do logBump l ctMoveUnknown 1; return (-1)
             else typeCode l ft
      -- the move's type, numbered at its first sighting (0: unknown)
      mt <- if fc' < 0 then return 0
            else do tm <- readIORef (cl_mtypes l)
                    case M.lookup (fc', n) tm of
                      Just ix -> return ix
                      Nothing -> do let ix = M.size tm + 1
                                    writeIORef (cl_mtypes l) $! M.insert (fc', n) ix tm
                                    return ix
      let (parent', k') | perturbing ptMoves = (parent + 1, (k + 1) `mod` n)
                        | otherwise = (parent, k)
      if child >= 1 `shiftL` 40 || parent' >= 1 `shiftL` 40 || mt >= 1 `shiftL` 23
        then logBump l ctBigMove 1
        else push (cl_moves l) 2 (child .|. (k' `shiftL` 40)) (parent' .|. (mt `shiftL` 40)) 0 0
