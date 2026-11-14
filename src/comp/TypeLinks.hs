{-# LANGUAGE ScopedTypeVariables #-}
-- Type links (prototype, not mirror-safe): the evaluator's log of facts
-- about heap cells, written as text to the module's .tl file while the
-- module is elaborated.  A separate tool reads the file and infers which
-- source types apply to which bits.
--
-- Records (one a line): N a cell's own type at creation; C two cells with
-- the same value; K a pack or unpack between a bits end and a typed end;
-- A an application (R: a cell's own expression, S: the value stored in a
-- cell, F: the final expression of a surviving cell); V a cell whose value
-- is a constant or a leaf; D, M the defs and merges at the end of
-- elaboration; P the named ends; T the types.
--
-- Forcing.  A hook here may evaluate a type to weak head normal form, match
-- an expression's constructor and read lazy fields of a constructor's info.
-- Each such read runs under an exception handler, and a read that fails is
-- written as absent.  Two things are never done while the evaluator runs:
-- no heap cell is made, and no identifier is read.  A leaf (port, method
-- argument, parameter, state instance) is held as its unread Id and type; a
-- record that names one waits in the log and is written after elaboration,
-- when the Id has been forced by the compiler itself.  Types are numbered
-- at their first use and spelled after elaboration.
--
-- Nothing in the compile reads the log, and a hook never fails the compile.
module TypeLinks(
        TLog, tlNewLog, tlOn,
        -- cells
        tlCell, tlCellNoType, tlCellWHNF,
        -- links
        tlCopy, tlCoerce, tlValue, tlStoredValue, tlFixValue,
        tlArgsReset, tlArgPush, tlStored, tlRedex,
        -- the end of elaboration
        End, tlCellEnd,
        tlFinalBegin, tlDef, tlMerge, tlFinalExpr, tlNamed, tlFinalEnd
    ) where

import Control.Exception(SomeException, evaluate, try, catch)
import Control.Monad(when, unless, forM_)
import Data.Bits((.&.), (.|.), shiftL, shiftR)
import Data.IORef
import Data.Word(Word8)
import Data.Array.IO(IOUArray)
import Data.Array.MArray(newArray, readArray, writeArray, getBounds)
import qualified Data.Array as Array
import qualified Data.ByteString.Builder as B
import qualified Data.IntMap.Strict as IM
import qualified Data.Map.Strict as M
import Numeric(showHex)
import System.IO(Handle, IOMode(..), BufferMode(..), openBinaryFile,
                 hSetBuffering, hClose)

import FStringCompat(getFString)
import Position(noPosition)
import Id(Id, getIdString, getIdBaseString)
import PreIds(idArrow, idBit)
import IntLit(IntLit(..))
import Prim(PrimOp(..))
import CType(TISort(..), StructSubType(..))
import IType(tCode)
import ISyntax
import ISyntaxSubst(tSubst)

-- -----
-- Ends and records

-- One side of a fact.
data End = ECell !Int             -- a heap cell
         | ENode !Int             -- an inline sub-expression
         | ENamed !Int            -- a named end that has its id
         | ELeaf !Int Id IType    -- a leaf (kind), held unread
         | ELit !Int !Integer     -- a Bit literal: width, value
         | EConst                 -- any other constant
         | EUndet                 -- a don't-care
         | ENone                  -- absent or unreadable

tlCellEnd :: Int -> End
tlCellEnd = ECell

-- kinds of named ends
lkPort, lkArg, lkParam, lkState :: Int
lkPort = 0
lkArg = 1
lkParam = 2
lkState = 3

leafKindName :: Int -> String
leafKindName 0 = "port"
leafKindName 1 = "arg"
leafKindName 2 = "param"
leafKindName 3 = "state"
leafKindName _ = "other"

-- an application's name: text, or a method call's instance and method,
-- spelled after elaboration
data Name = NmStr String
          | NmCall Id Id

-- The records that can name a leaf (N, C, D and M hold only numbers and
-- are written at once).
data Rec = RK !Char String !Int !Int End End
           -- view, node, kind, name, type, type arguments, ends
         | RA !Char End String Name !Int [String] [End]
         | RV !Int End

endHeld :: End -> Bool
endHeld (ELeaf _ _ _) = True
endHeld _ = False

recHeld :: Rec -> Bool
recHeld (RK _ _ _ _ b v) = endHeld b || endHeld v
recHeld (RA _ n _ nm _ _ es) = endHeld n || nameHeld nm || any endHeld es
  where nameHeld (NmCall _ _) = True
        nameHeld _ = False
recHeld (RV _ e) = endHeld e

-- -----
-- Classes of types (the T record's cls)

clsBit, clsData, clsStruct, clsDcon, clsIface, clsClass, clsPoly, clsFun,
  clsPrim, clsOther, nClasses :: Int
clsBit = 0
clsData = 1
clsStruct = 2
clsDcon = 3
clsIface = 4
clsClass = 5
clsPoly = 6
clsFun = 7
clsPrim = 8
clsOther = 9
nClasses = 10

clsName :: Int -> String
clsName 0 = "bit"
clsName 1 = "data"
clsName 2 = "struct"
clsName 3 = "dcon"
clsName 4 = "iface"
clsName 5 = "class"
clsName 6 = "poly"
clsName 7 = "fun"
clsName 8 = "prim"
clsName _ = "other"

-- Cells of these classes are not logged (volume): functions, dictionaries
-- and primitive types (Integer, String, Module, Action and so on), none of
-- which is a typed value with a bit representation.  A method call that is
-- the value of an Action cell is still in the final view.
clsSkipped :: Int -> Bool
clsSkipped c = c == clsFun || c == clsClass || c == clsPrim

isArrow :: IType -> Bool
isArrow (ITCon i _ _) = i == idArrow noPosition
isArrow _ = False

headOf :: IType -> IType
headOf (ITAp f _) = headOf f
headOf t = t

-- the class of a type in weak head normal form (reads the head's sort and,
-- for a struct, its sub-kind)
classOf :: IType -> Int
classOf (ITForAll _ _ _) = clsFun
classOf (ITAp (ITAp arr _) _) | isArrow arr = clsFun
classOf t =
    case headOf t of
      ITCon i _ s | i == idBit -> clsBit
                  | otherwise -> sortClass s
      _ -> clsOther
  where sortClass (TIdata {}) = clsData
        sortClass (TIstruct ss _) =
            case ss of
              SStruct -> clsStruct
              SClass -> clsClass
              SDataCon {} -> clsDcon
              SInterface {} -> clsIface
              SPolyWrap {} -> clsPoly
        sortClass TIabstract = clsPrim
        sortClass _ = clsOther

-- n of Bit n
bitWidth :: IType -> Int
bitWidth (ITAp (ITCon i _ _) (ITNum n)) | i == idBit = fromInteger n
bitWidth _ = -1

arrowParts :: IType -> Maybe (IType, IType)
arrowParts (ITAp (ITAp arr a) r) | isArrow arr = Just (a, r)
arrowParts _ = Nothing

-- the type after n arrows
dropArr :: Int -> IType -> Maybe IType
dropArr 0 t = Just t
dropArr n t = case arrowParts t of
                Just (_, r) -> dropArr (n - 1) r
                Nothing -> Nothing

-- instantiate the leading quantifiers, without normalizing
instLead :: IType -> [IType] -> IType
instLead (ITForAll i _ t) (a : as) = instLead (tSubst i a t) as
instLead t _ = t

-- -----
-- Counters (written as trailer comments)

ctN, ctC, ctK, ctAS, ctAR, ctAF, ctV, ctD, ctM, ctP, ctT, ctErr, ctDeferred,
  ctSkipCell, ctSkipLink, ctSkipDictSel, ctSkipPlain, ctSelfLink, ctNoHead,
  ctClass0, nCounters :: Int
ctN = 0
ctC = 1
ctK = 2
ctAS = 3
ctAR = 4
ctAF = 5
ctV = 6
ctD = 7
ctM = 8
ctP = 9
ctT = 10
ctErr = 11          -- reads that failed
ctDeferred = 12     -- records that waited for a leaf's name
ctSkipCell = 13     -- cells not logged (function, dictionary or primitive type)
ctSkipLink = 14     -- links and applications on such a cell
ctSkipDictSel = 15  -- selections from a dictionary cell
ctSkipPlain = 16    -- redexes that are plain calls
ctSelfLink = 17     -- links from a cell to itself
ctNoHead = 18       -- applications with a head that has no kind
ctClass0 = 19       -- cells by class: ctClass0 + class
nCounters = ctClass0 + nClasses

counterNames :: [(Int, String)]
counterNames =
    [ (ctN, "N"), (ctC, "C"), (ctK, "K"), (ctAS, "A-S"), (ctAR, "A-R"),
      (ctAF, "A-F"), (ctV, "V"), (ctD, "D"), (ctM, "M"), (ctP, "P"),
      (ctT, "T"), (ctErr, "read-failed"), (ctDeferred, "deferred"),
      (ctSkipCell, "skip-cell"), (ctSkipLink, "skip-on-cell"),
      (ctSkipDictSel, "skip-dict-sel"), (ctSkipPlain, "skip-plain-call"),
      (ctSelfLink, "skip-self-link"), (ctNoHead, "no-kind") ] ++
    [ (ctClass0 + c, "cells-" ++ clsName c) | c <- [0 .. nClasses - 1] ]

-- -----
-- The log

data TLog = TLog {
    tlOn      :: !Bool,
    tl_h      :: Handle,
    -- type code -> (type id << 4) + class; the type id is the type's rank
    -- by first use, so nothing written depends on the code
    tl_tids   :: !(IORef (IM.IntMap Int)),
    tl_types  :: !(IORef [IType]),               -- by type id, newest first
    -- cell -> 1 + the class of its own type (0: not known)
    tl_cls    :: !(IORef (IOUArray Int Word8)),
    tl_node   :: !(IORef Int),                   -- the next inline node
    tl_defer  :: !(IORef [Rec]),                 -- held records, newest first
    tl_args   :: !(IORef [End]),                 -- evalHeap's moved arguments
    tl_final  :: !(IORef Bool),                  -- elaboration has ended
    -- named ends: (kind, name) -> id; and the list, newest first
    tl_leafs  :: !(IORef (M.Map (String, String) Int)),
    tl_leafl  :: !(IORef [(Int, String, String, Maybe IType, Int)]),
    tl_counts :: !(IOUArray Int Int)
  }

-- A log writing to the file (with its header), or a log that is off.
tlNewLog :: Maybe (FilePath, String, String) -> IO TLog
tlNewLog mfile = do
  mh <- case mfile of
          Nothing -> return Nothing
          Just (path, pkg, modname) -> do
            r <- try (do h <- openBinaryFile path WriteMode
                         hSetBuffering h (BlockBuffering (Just 1048576))
                         B.hPutBuilder h (B.string7 "tl 1 " <> B.stringUtf8 (word pkg) <>
                                          B.char7 ' ' <> B.stringUtf8 (word modname) <>
                                          B.char7 '\n')
                         return h)
            case r of
              Left (_ :: SomeException) -> return Nothing
              Right h -> return (Just h)
  tids <- newIORef IM.empty
  types <- newIORef []
  cls <- newArray (0, 65535) 0 >>= newIORef
  node <- newIORef 0
  defer <- newIORef []
  args <- newIORef []
  final <- newIORef False
  leafs <- newIORef M.empty
  leafl <- newIORef []
  counts <- newArray (0, nCounters - 1) 0
  return (TLog { tlOn = maybe False (const True) mh,
                 tl_h = maybe (error "TypeLinks: no file") id mh,
                 tl_tids = tids, tl_types = types, tl_cls = cls,
                 tl_node = node, tl_defer = defer, tl_args = args,
                 tl_final = final, tl_leafs = leafs, tl_leafl = leafl,
                 tl_counts = counts })

bump :: TLog -> Int -> IO ()
bump l k = do
  v <- readArray (tl_counts l) k
  writeArray (tl_counts l) k (v + 1)

-- run a hook; a failure is counted and writes nothing
safely :: TLog -> IO () -> IO ()
safely l act = act `catch` \ (_ :: SomeException) -> bump l ctErr

-- a read that may fail
reading :: TLog -> a -> IO a -> IO a
reading l dflt act =
    act `catch` \ (_ :: SomeException) -> bump l ctErr >> return dflt

-- text with no spaces or line ends (a token)
word :: String -> String
word "" = "-"
word s = map (\ c -> if c == ' ' || c == '\n' || c == '\t' || c == '\r' then '_' else c) s

-- -----
-- Types

-- The id and class of a type, which is evaluated to weak head normal form
-- here ((-1, other) when that fails or the type has no code).
tlType :: TLog -> IType -> IO (Int, Int)
tlType l t = reading l (-1, clsOther) $ do
  t' <- evaluate t
  let c = tCode t'
  if c < 0 then return (-1, clsOther) else do
    m <- readIORef (tl_tids l)
    case IM.lookup c m of
      Just v -> return (v `shiftR` 4, v .&. 15)
      Nothing -> do
        cls <- reading l clsOther (evaluate (classOf t'))
        let tid = IM.size m
        writeIORef (tl_tids l) $! IM.insert c ((tid `shiftL` 4) .|. cls) m
        modifyIORef' (tl_types l) (t' :)
        return (tid, cls)

tidOf :: TLog -> Maybe IType -> IO Int
tidOf _ Nothing = return (-1)
tidOf l (Just t) = fst <$> tlType l t

-- a numeric type argument: a number, or "?"
numArg :: TLog -> IType -> IO String
numArg l t = reading l "?" $ do
  t' <- evaluate t
  case t' of
    ITNum n -> return (show n)
    _ -> return "?"

numOf :: TLog -> IType -> IO Int
numOf l t = reading l (-1) $ do
  t' <- evaluate t
  case t' of
    ITNum n -> return (fromInteger n)
    _ -> return (-1)

-- one line for a type
spell :: IType -> String
spell t0 = go (0 :: Int) t0 ""
  where
    go p t = case t of
      ITForAll i _ b -> showParen (p > 0) (showString "forall " . showString (getIdString i) .
                                            showString " . " . go 0 b)
      ITAp (ITAp arr a) r | isArrow arr ->
          showParen (p > 8) (go 9 a . showString " -> " . go 8 r)
      ITAp f a -> showParen (p > 9) (go 9 f . showChar ' ' . go 10 a)
      ITVar i -> showString (getIdString i)
      ITCon i _ _ -> showString (getIdString i)
      ITNum n -> shows n
      ITStr s -> shows (getFString s)

spellMax :: Int
spellMax = 300

-- -----
-- The cells' classes

setCellClass :: TLog -> Int -> Int -> IO ()
setCellClass l p cls
  | p < 0 = return ()
  | otherwise = do
      arr <- readIORef (tl_cls l)
      (_, hi) <- getBounds arr
      arr' <- if p <= hi then return arr else do
                let hi' = max p (2 * hi + 1)
                a <- newArray (0, hi') 0
                let copy i | i > hi = return ()
                           | otherwise = do readArray arr i >>= writeArray a i
                                            copy (i + 1)
                copy 0
                writeIORef (tl_cls l) a
                return a
      writeArray arr' p (fromIntegral (cls + 1))

-- the class of a cell's own type, -1 when it has none in the log
cellClass :: TLog -> Int -> IO Int
cellClass l p
  | p < 0 = return (-1)
  | otherwise = do
      arr <- readIORef (tl_cls l)
      (_, hi) <- getBounds arr
      if p > hi then return (-1) else do
        v <- readArray arr p
        return (fromIntegral v - 1)

skippedCell :: TLog -> Int -> IO Bool
skippedCell l p = do
  c <- cellClass l p
  return (c >= 0 && clsSkipped c)

-- -----
-- Writing

put :: TLog -> Int -> B.Builder -> IO ()
put l ct b = do
  B.hPutBuilder (tl_h l) (b <> B.char7 '\n')
  bump l ct

sp :: B.Builder
sp = B.char7 ' '

cellB :: Int -> B.Builder
cellB p = B.char7 'c' <> B.intDec p

-- a named end's id, made at its first sighting (after elaboration only:
-- this reads the Id)
leafId :: TLog -> Int -> Id -> IType -> IO Int
leafId l k i t = do
  name <- reading l "?" (evaluate (let s = word (getIdString i) in length s `seq` s))
  namedId l (leafKindName k) name (Just t) (-2)

-- the id of the named end (kind, name); own: a leaf's own type with the
-- width -2, or another named end's declared type with its width
namedId :: TLog -> String -> String -> Maybe IType -> Int -> IO Int
namedId l kname name own width = do
  m <- readIORef (tl_leafs l)
  case M.lookup (kname, name) m of
    Just n -> return n
    Nothing -> do
      let n = M.size m
      writeIORef (tl_leafs l) $! M.insert (kname, name) n m
      modifyIORef' (tl_leafl l) ((n, kname, name, own, width) :)
      return n

endB :: TLog -> End -> IO B.Builder
endB _ (ECell p) = return (cellB p)
endB _ (ENode n) = return (B.char7 'a' <> B.intDec n)
endB _ (ENamed n) = return (B.char7 'n' <> B.intDec n)
endB l (ELeaf k i t) = do n <- leafId l k i t
                          return (B.char7 'n' <> B.intDec n)
endB _ (ELit w v) =
    let v' = if v < 0 && w > 0 then v `mod` (2 ^ w) else v
    in  return (if v' < 0 then B.char7 'k'
                else B.char7 'k' <> B.intDec w <> B.char7 '.' <> B.string7 (showHex v' ""))
endB _ EConst = return (B.char7 'k')
endB _ EUndet = return (B.char7 'x')
endB _ ENone = return (B.char7 '-')

endsB :: TLog -> [End] -> IO B.Builder
endsB l es0 = go mempty es0
  where go acc [] = return acc
        go acc (e : es) = do b <- endB l e
                             go (acc <> sp <> b) es

writeRec :: TLog -> Rec -> IO ()
writeRec l (RK dir how tid w bits val) = do
  bb <- endB l bits
  vb <- endB l val
  put l ctK (B.string7 "K " <> B.char7 dir <> sp <> B.string7 how <> sp <> B.intDec tid <>
             sp <> B.intDec w <> sp <> bb <> sp <> vb)
writeRec l (RA vw node kind name tid nts ends) = do
  nb <- endB l node
  nm <- case name of
          NmStr s -> return (word s)
          NmCall inst meth ->
              reading l "?" (evaluate (let s = word (getIdString inst ++ "." ++
                                                     getIdBaseString meth)
                                       in  length s `seq` s))
  eb <- endsB l ends
  put l (case vw of
           'R' -> ctAR
           'S' -> ctAS
           _ -> ctAF)
        (B.string7 "A " <> B.char7 vw <> sp <> nb <> sp <> B.string7 kind <> sp <>
         B.stringUtf8 nm <> sp <> B.intDec tid <> sp <> B.intDec (length nts) <>
         mconcat [ sp <> B.string7 t | t <- nts ] <> sp <> B.intDec (length ends) <> eb)
writeRec l (RV p e) = do
  eb <- endB l e
  put l ctV (B.string7 "V " <> cellB p <> sp <> eb)

-- write a record, or hold it while a leaf it names cannot be read
emit :: TLog -> Rec -> IO ()
emit l r = do
  fin <- readIORef (tl_final l)
  if fin || not (recHeld r) then writeRec l r
    else do modifyIORef' (tl_defer l) (r :)
            bump l ctDeferred

newNode :: TLog -> IO Int
newNode l = do
  n <- readIORef (tl_node l)
  writeIORef (tl_node l) $! n + 1
  return n

-- -----
-- Ends of expressions

-- The end an expression is, without looking inside it: Right an end, or
-- Left whether it is an application (an inline sub-expression).
endOrInline :: TLog -> IExpr a -> IO (Either Bool End)
endOrInline l e0 = reading l (Right ENone) $ do
  e <- evaluate e0
  case e of
    IRefT _ p _ _ -> return (Right (ECell p))
    ICon i ic -> Right <$> conEnd l i ic
    IAps _ _ _ -> return (Left True)
    _ -> return (Left False)

endOf :: TLog -> IExpr a -> IO End
endOf l e = do
  r <- endOrInline l e
  case r of
    Right en -> return en
    Left _ -> return ENone

-- the end a constant or leaf node is
conEnd :: TLog -> Id -> IConInfo a -> IO End
conEnd l i ic0 = reading l ENone $ do
  ic <- evaluate ic0
  case ic of
    ICModPort { iConType = t } -> return (ELeaf lkPort i t)
    ICMethArg { iConType = t } -> return (ELeaf lkArg i t)
    ICModParam { iConType = t } -> return (ELeaf lkParam i t)
    ICStateVar { iConType = t } -> return (ELeaf lkState i t)
    ICInt { iConType = t, iVal = v } -> do
        w <- reading l (-1) (bitWidth <$> evaluate t)
        if w < 0 then return EConst else do
          mx <- reading l Nothing (Just <$> evaluate (ilValue v))
          case mx of
            Just x -> return (ELit w x)
            Nothing -> return EConst
    ICUndet {} -> return EUndet
    -- a held coercion is the value of its applied cell
    ICLazyPack { lzAppliedPtr = a } -> return (if a >= 0 then ECell a else ENone)
    ICLazyUnpack { lzAppliedPtr = a } -> return (if a >= 0 then ECell a else ENone)
    ICReal {} -> return EConst
    ICString {} -> return EConst
    ICChar {} -> return EConst
    _ -> return ENone

-- a pointer when there is one, else the end of the expression
ptrOrEnd :: TLog -> Int -> Maybe (IExpr a) -> IO End
ptrOrEnd l p me
  | p >= 0 = return (ECell p)
  | otherwise = case me of
                  Just e -> endOf l e
                  Nothing -> return ENone

-- -----
-- Applications

flatAp :: IExpr a -> (IExpr a, [IType], [IExpr a])
flatAp (IAps f ts es) = let (h, ts0, es0) = flatAp f
                        in  (h, ts0 ++ ts, es0 ++ es)
flatAp e = (e, [], [])

-- The ends of the arguments: a cell, leaf or constant as it is; an inline
-- application gets a node of its own, described to the given depth (not at
-- all when stop is set: the arm of a conditional).
argEnds :: TLog -> Char -> Int -> Bool -> [IExpr a] -> IO [End]
argEnds l vw depth stop es0 = go [] es0
  where go acc [] = return (reverse acc)
        go acc (e : es) = do en <- argEnd l vw depth stop e
                             go (en : acc) es

argEnd :: TLog -> Char -> Int -> Bool -> IExpr a -> IO End
argEnd l vw depth stop e = do
  r <- endOrInline l e
  case r of
    Right en -> return en
    Left False -> return ENone
    Left True -> do
      n <- newNode l
      let other s = emit l (RA vw (ENode n) "other" (NmStr s) (-1) [] [])
      if stop then other "inline"
        else if depth <= 0 then other "deep"
        else do done <- reading l False (describe l vw (depth - 1) (ENode n) Nothing Nothing e)
                unless done (other "ap")
      return (ENode n)

-- Describe an application as the value of node, in a view; True when a
-- record was written.  mft is the head's instantiated function type when
-- the caller has it; mends the ends of the expression arguments when the
-- caller has them (evalHeap's moves; the arguments are then only read for
-- an array's elements).
describe :: TLog -> Char -> Int -> End -> Maybe IType -> Maybe [End] -> IExpr a -> IO Bool
describe l vw depth node mft mends e0 = do
  let (h0, ts, es) = flatAp e0
      n = length es
      -- every argument's end; the conditional kinds stop at their arms
      ends stop = case mends of
                    Just xs -> return xs
                    Nothing -> argEnds l vw depth stop es
      -- the first k arguments described, the rest as arms
      endsArms k = case mends of
                     Just xs -> return xs
                     Nothing -> do a <- argEnds l vw depth False (take k es)
                                   b <- argEnds l vw depth (vw /= 'F') (drop k es)
                                   return (a ++ b)
      out kind name tid nts xs = do emit l (RA vw node kind name tid nts xs)
                                    return True
      tyArg k = case drop k ts of
                  (t : _) -> Just t
                  [] -> Nothing
      -- the head's function type: the caller's, or its own type at the
      -- type arguments (not normalized)
      funType ic = case mft of
                     Just ft -> Just ft
                     Nothing -> Just (instLead (iConType ic) ts)
      resTid ic = reading l (-1) (tidOf l (funType ic >>= dropArr n))
      scrutTid ic = reading l (-1) (tidOf l (fmap fst (funType ic >>= arrowParts)))
      ctiName cti = reading l "-" (evaluate (let s = show (conNo cti) ++ "." ++ show (numCon cti)
                                              in  length s `seq` s))
  h <- evaluate h0
  case h of
    ICon hi ic0 -> do
      ic <- evaluate ic0
      case ic of
        ICCon { conTagInfo = cti } -> do
            nm <- ctiName cti
            tid <- resTid ic
            xs <- ends False
            out "con" (NmStr nm) tid [] xs
        ICTuple {} -> do
            tid <- resTid ic
            xs <- ends False
            out "tup" (NmStr (show n)) tid [] xs
        ICSel { selNo = k, numSel = m } -> do
            -- the selected value first: a selection from a dictionary cell
            -- (a class method) is not logged
            first <- case mends of
                       Just (x : _) -> return (Just x)
                       Just [] -> return Nothing
                       Nothing -> case es of
                                    (x : _) -> Just <$> argEnd l vw depth False x
                                    [] -> return Nothing
            dict <- case first of
                      Just (ECell p) -> (== clsClass) <$> cellClass l p
                      _ -> return False
            if dict then do bump l ctSkipDictSel
                            return True
              else do
                rest <- case mends of
                          Just xs -> return (drop 1 xs)
                          Nothing -> argEnds l vw depth False (drop 1 es)
                case first of
                  Just (ELeaf lk inst _) | lk == lkState ->
                      out "call" (NmCall inst hi) (-1) [] rest
                  Just x -> sel k m ic (x : rest)
                  Nothing -> sel k m ic []
        ICOut { conTagInfo = cti } -> do
            nm <- ctiName cti
            tid <- scrutTid ic
            xs <- ends False
            out "out" (NmStr nm) tid [] xs
        ICIs { conTagInfo = cti } -> do
            nm <- ctiName cti
            tid <- scrutTid ic
            xs <- ends False
            out "is" (NmStr nm) tid [] xs
        ICPrim { primOp = op } ->
            case op of
              PrimChr -> do
                  tid <- tidOf l (tyArg 1)
                  xs <- ends False
                  out "chr" (NmStr "-") tid [] xs
              PrimOrd -> do
                  tid <- tidOf l (tyArg 0)
                  xs <- ends False
                  out "ord" (NmStr "-") tid [] xs
              PrimIf | n == 3 -> do
                  tid <- tidOf l (tyArg 0)
                  xs <- endsArms 1
                  out "if" (NmStr "-") tid [] xs
              PrimCase -> do
                  tid <- tidOf l (tyArg 1)
                  nts <- mapM (numArg l) (take 1 ts)
                  xs <- endsArms 1
                  out "case" (NmStr "-") tid nts xs
              PrimArrayDynSelect | [arr, idx] <- es -> do
                  tid <- tidOf l (tyArg 0)
                  nts <- mapM (numArg l) (take 1 (drop 1 ts))
                  ix <- case mends of
                          Just [_, x] -> return x
                          _ -> argEnd l vw depth False idx
                  els <- arrayEnds arr
                  out "asel" (NmStr "-") tid nts (ix : els)
              PrimPack | n >= 2 -> coercion "pack" 'p'
              PrimUnpack | n >= 2 -> coercion "unpack" 'u'
              _ -> do
                  nts <- mapM (numArg l) ts
                  xs <- ends False
                  out "prim" (NmStr (show op)) (-1) nts xs
        ICForeign { fName = f } -> do
            nm <- reading l "?" (evaluate (let s = "foreign." ++ f in length s `seq` s))
            xs <- ends False
            out "other" (NmStr nm) (-1) [] xs
        _ -> do bump l ctNoHead
                return False
     where
      sel k m ic xs = do
          nm <- reading l "-" (evaluate (let s = show k ++ "." ++ show m in length s `seq` s))
          tid <- scrutTid ic
          out "sel" (NmStr nm) tid [] xs
      -- pack or unpack at the type argument, of the argument after the
      -- dictionary; a one-bit (or narrower) coercion that is a cell's own
      -- expression is also a K record, since no hold arm sees it
      coercion kind dir = do
          let mta = tyArg 0
          tid <- tidOf l mta
          operand <- case mends of
                       Just xs -> return (drop 1 xs)
                       Nothing -> argEnds l vw depth False (drop 1 es)
          w <- maybe (return (-1)) (numOf l) (tyArg 1)
          isBit <- case mta of
                     Just ta -> reading l False (do ta' <- evaluate ta
                                                    return (bitWidth ta' >= 0))
                     Nothing -> return True
          when (vw == 'R' && (w == 0 || w == 1) && not isBit) $
              case (node, operand) of
                (ECell _, x : _) ->
                    emit l (if dir == 'p' then RK 'p' "fall" tid w node x
                            else RK 'u' "fall" tid w x node)
                _ -> return ()
          out kind (NmStr "-") tid [] operand
      -- the element cells of an array argument
      arrayEnds arr = reading l [] $ do
          a <- evaluate arr
          case a of
            ICon _ (ICLazyArray { iArray = cells }) ->
                return [ ECell (ac_ptr c) | c <- Array.elems cells ]
            IAps (ICon _ (ICPrim { primOp = PrimBuildArray })) _ els ->
                argEnds l vw depth False els
            _ -> do en <- argEnd l vw depth False arr
                    return [en]
    _ -> return False

-- -----
-- Hooks: cells

-- A cell was made at p with the type t by the creator how (N).
tlCell :: TLog -> Int -> IType -> String -> IO ()
tlCell l p t how = safely l $ do _ <- newCell l p t how
                                 return ()

-- N; False when the cell's class is not logged
newCell :: TLog -> Int -> IType -> String -> IO Bool
newCell l p t how = do
  (tid, cls) <- tlType l t
  setCellClass l p cls
  bump l (ctClass0 + cls)
  if clsSkipped cls then do bump l ctSkipCell
                            return False
    else do put l ctN (B.string7 "N " <> cellB p <> sp <> B.intDec tid <> sp <> B.string7 (word how))
            return True

-- a cell made with no type in hand
tlCellNoType :: TLog -> Int -> String -> IO ()
tlCellNoType l p how = safely l $
  put l ctN (B.string7 "N " <> cellB p <> B.string7 " -1 " <> B.string7 (word how))

-- A cell was made already evaluated: its type, and the value it holds.
tlCellWHNF :: TLog -> Int -> IType -> String -> Bool -> IExpr a -> IO ()
tlCellWHNF l p t how withValue e = safely l $ do
  logged <- newCell l p t how
  when (logged && withValue) $ storedValue l p e

-- -----
-- Hooks: links

-- dst got the value of src (C)
tlCopy :: TLog -> String -> Int -> Int -> IO ()
tlCopy l how dst src = safely l $
  if dst < 0 || src < 0 then return ()
  else if dst == src then bump l ctSelfLink
  else do
    skip <- skippedCell l dst
    if skip then bump l ctSkipLink
      else put l ctC (B.string7 "C " <> B.string7 how <> sp <> cellB dst <> sp <> cellB src)

-- A pack (dir p) or unpack (u) at the type ta of width tn, between the
-- bits and the typed value; each side is a cell pointer, or else an
-- expression whose end is read (K).
tlCoerce :: TLog -> Char -> String -> IType -> IType ->
            (Int, Maybe (IExpr a)) -> (Int, Maybe (IExpr a)) -> IO ()
tlCoerce l dir how ta tn (bp, be) (vp, ve) = safely l $ do
  (tid, _) <- tlType l ta
  w <- numOf l tn
  bits <- ptrOrEnd l bp be
  val <- ptrOrEnd l vp ve
  emit l (RK dir how tid w bits val)

-- The cell p was evaluated to a value that is not an application: a
-- constant or leaf (V), or a held coercion (the value of its applied
-- cell).
tlValue :: TLog -> Int -> IExpr a -> IO ()
tlValue l p e = safely l $ do
  skip <- skippedCell l p
  unless skip $ storedValue l p e

-- The value held by the cell p: an application (A S), a constant or leaf
-- (V), or a held coercion (C hold).
tlStoredValue :: TLog -> Int -> IExpr a -> IO ()
tlStoredValue l p e = safely l $ do
  skip <- skippedCell l p
  unless skip $ storedValue l p e

storedValue :: TLog -> Int -> IExpr a -> IO ()
storedValue l p e0 = do
  e <- evaluate e0
  case e of
    IAps _ _ _ -> do _ <- describe l 'S' 2 (ECell p) Nothing Nothing e
                     return ()
    ICon i ic0 -> do
      ic <- evaluate ic0
      case ic of
        ICLazyPack { lzAppliedPtr = a } -> tlCopy l "hold" p a
        ICLazyUnpack { lzAppliedPtr = a } -> tlCopy l "hold" p a
        _ -> do en <- conEnd l i ic
                case en of
                  ENone -> return ()
                  _ -> emit l (RV p en)
    _ -> return ()

-- The module-fix cell p was written with the value e: a reference (C fix)
-- or a value.
tlFixValue :: TLog -> Int -> IExpr a -> IO ()
tlFixValue l p e0 = safely l $ do
  e <- evaluate e0
  case e of
    IRefT _ src _ _ -> tlCopy l "fix" p src
    _ -> storedValue l p e

-- evalHeap's moved arguments: start a list
tlArgsReset :: TLog -> IO ()
tlArgsReset l = writeIORef (tl_args l) []

-- One argument was heaped: the counter before and after, and what the
-- heaping action returned.  If the counter moved by one the argument's
-- cell is the old counter value; else the returned value is matched.
tlArgPush :: TLog -> Int -> Int -> IExpr a -> IO ()
tlArgPush l h0 h1 r = do
  en <- if h1 == h0 + 1 then return (ECell h0) else endOf l r
  modifyIORef' (tl_args l) (en :)

-- The application f ts as was stored into the cell p, its arguments heaped
-- (their ends are the list since tlArgsReset); ft is f's instantiated type.
tlStored :: TLog -> Int -> IExpr a -> [IType] -> IType -> [IExpr a] -> IO ()
tlStored l p f ts ft as = safely l $ do
  skip <- skippedCell l p
  if skip then bump l ctSkipLink else do
    xs <- reverse <$> readIORef (tl_args l)
    _ <- describe l 'S' 0 (ECell p) (Just ft) (Just xs) (IAps f ts as)
    return ()

-- The cell p's own expression e, read after it was evaluated (A R).
tlRedex :: TLog -> Int -> IExpr a -> IO ()
tlRedex l p e0 = safely l $ do
  skip <- skippedCell l p
  if skip then bump l ctSkipLink else do
    e <- evaluate e0
    case e of
      IAps _ _ _ -> do done <- describe l 'R' 2 (ECell p) Nothing Nothing e
                       unless done (bump l ctSkipPlain)
      _ -> return ()

-- -----
-- The end of elaboration

-- Elaboration has ended and the compiler has forced the module: write the
-- held records.  From here a leaf is read as it is met.
tlFinalBegin :: TLog -> IO ()
tlFinalBegin l = safely l $ do
  writeIORef (tl_final l) True
  rs <- readIORef (tl_defer l)
  writeIORef (tl_defer l) []
  forM_ (reverse rs) $ \ r -> safely l (writeRec l r)

-- the canonical cell p is the def named name, of width w (D)
tlDef :: TLog -> Int -> IType -> String -> IO ()
tlDef l p t name = safely l $ do
  w <- reading l (-1) (bitWidth <$> evaluate t)
  put l ctD (B.string7 "D " <> cellB p <> sp <> B.intDec w <> sp <> B.stringUtf8 (word name))

-- the cell p was merged into the canonical cell c (M)
tlMerge :: TLog -> Int -> Int -> Bool -> IO ()
tlMerge l p c isConst = safely l $
  put l ctM (B.string7 "M " <> cellB p <> sp <> cellB c <> sp <>
             B.char7 (if isConst then '1' else '0'))

-- The final expression of a node (A F); a constant or leaf is a V record
-- for a cell.  For a named node, a single cell is "V cell node" (the same
-- value), and a constant or leaf is "A F node other alias".
tlFinalExpr :: TLog -> End -> IExpr a -> IO ()
tlFinalExpr l node e0 = safely l $ do
  e <- evaluate e0
  case e of
    IAps _ _ _ -> do done <- describe l 'F' 8 node Nothing Nothing e
                     unless done $
                       emit l (RA 'F' node "other" (NmStr "ap") (-1) [] [])
    IRefT _ p _ _ ->
        case node of
          ECell c -> tlCopy l "final" c p
          _ -> emit l (RV p node)
    ICon i ic -> do
        en <- conEnd l i ic
        case (node, en) of
          (_, ENone) -> return ()
          (ECell c, _) -> emit l (RV c en)
          _ -> emit l (RA 'F' node "other" (NmStr "alias") (-1) [] [en])
    _ -> return ()

-- A named end that is not a leaf (a rule's condition, a method's value):
-- its kind, declared type, width and name.
tlNamed :: TLog -> String -> Maybe IType -> Int -> String -> IO End
tlNamed l kname declared width name =
  reading l ENone $ do
    n <- namedId l kname (word name) declared width
    return (ENamed n)

-- Write the named ends (P), with the declared source type of a leaf where
-- the lookup has one, then the types (T) and the counts, and close the
-- file.
tlFinalEnd :: TLog -> (String -> String -> Maybe IType) -> IO ()
tlFinalEnd l declared = do
  safely l $ do
    leafs <- reverse <$> readIORef (tl_leafl l)
    forM_ leafs $ \ (n, kname, name, own, width) -> safely l $ do
      -- a leaf has its own type (for the width); another named end was
      -- given its declared type and width
      (mdecl, w) <-
          if width == -2
          then do w <- case own of
                         Just t -> reading l (-1) (bitWidth <$> evaluate t)
                         Nothing -> return (-1)
                  return (declared kname name, w)
          else return (own, width)
      tid <- tidOf l mdecl
      put l ctP (B.string7 "P " <> B.intDec n <> sp <> B.string7 kname <> sp <> B.intDec tid <>
                 sp <> B.intDec w <> sp <> B.stringUtf8 name)
  safely l $ do
    ts <- reverse <$> readIORef (tl_types l)
    forM_ (zip [0 :: Int ..] ts) $ \ (tid, t) -> safely l $ do
      cls <- reading l clsOther (evaluate (classOf t))
      let w = if cls == clsBit then bitWidth t else -1
      (ncon, nfield, enum) <- reading l (-1, -1, -1) $ evaluate $
          case headOf t of
            ITCon _ _ (TIdata cs en) | cls == clsData ->
                let k = length cs
                    b = if en then 1 else 0 :: Int
                in  k `seq` b `seq` (k, 0, b)
            ITCon _ _ (TIstruct _ fs) | cls /= clsFun ->
                let k = length fs in k `seq` (0, k, 0)
            _ -> (0, 0, 0)
      s <- reading l "?" (evaluate (let x = take spellMax (spell t) in length x `seq` x))
      put l ctT (B.string7 "T " <> B.intDec tid <> sp <> B.string7 (clsName cls) <> sp <>
                 B.intDec w <> sp <> B.intDec ncon <> sp <> B.intDec nfield <> sp <>
                 B.intDec enum <> sp <> B.stringUtf8 s)
  safely l $ do
    forM_ counterNames $ \ (k, name) -> do
      v <- readArray (tl_counts l) k
      B.hPutBuilder (tl_h l) (B.string7 "# count " <> B.string7 name <> sp <> B.intDec v <>
                              B.char7 '\n')
  hClose (tl_h l) `catch` \ (_ :: SomeException) -> return ()
