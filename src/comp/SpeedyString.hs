{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, (++), concat, filter,
                    InternSnapshot(..), snapshotInternState, restoreInternState) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import IOMutVar(MutableVar, newVar, readVar, writeVar)
import System.IO.Unsafe(unsafePerformIO)
import Control.Exception(evaluate)
import Data.Maybe(fromMaybe)
import qualified Data.IntMap.Strict as M
import qualified Data.Set as Set
import Control.Monad(unless)
-- import qualified NotSoSpeedyString
import ErrorUtil (internalError)
import qualified Data.Generics as Generic


data SString = SString !Int -- unique id
   deriving (Generic.Data, Generic.Typeable)

instance Eq SString where
    (SString i) == (SString i') = i == i'

-- note that Ord is not the usual string ordering
instance Ord SString where
    compare (SString i) (SString i') = compare i i'

instance Show SString where
    show = show . toString

-- public

toString :: SString -> String
toString (SString id) = unsafePerformIO $
                        do m <- readVar strings
                           return $ M.findWithDefault err id m

-- s is hashed (forcing all of it) before the table is read, because
-- forcing s can intern other strings, even one equal to s
fromString :: String -> SString
fromString s = unsafePerformIO $
               do h <- evaluate (hashStr s)
                  m <- readVar sstrings
                  return $ fromMaybe (newSString s) (M.lookup h m >>= lookup s)

(++) :: SString -> SString -> SString
s ++ s' = fromString $ (toString s) Prelude.++ (toString s')

concat :: [SString] -> SString
concat = fromString . concatMap toString

filter :: (Char -> Bool) -> SString -> SString
filter pred s = fromString $ Prelude.filter pred (toString s)

-- private

newSString :: String -> SString
newSString s = unsafePerformIO $
               do id <- freshInt
                  let ss = SString id
                  sm <- readVar strings
                  ssm <- readVar sstrings
                  let !sm'  = M.insert id s sm
                      !ssm' = M.insertWith (Prelude.++)
                                          (hashStr s) [(s,ss)] ssm
                  writeVar strings sm'
                  writeVar sstrings ssm'
                  return ss

err :: a
err = internalError "SpeedyString: inconsistent representation"

--toNotSoSpeedyString :: SString -> NotSoSpeedyString.SString
--toNotSoSpeedyString speedy = NotSoSpeedyString.fromString (toString speedy)

--fromNotSoSpeedyString :: NotSoSpeedyString.SString -> SString
--fromNotSoSpeedyString not_so_speedy =
--    fromString (NotSoSpeedyString.toString not_so_speedy)

-- internal representation

strings :: MutableVar (M.IntMap String)
strings = unsafePerformIO $ newVar (M.empty)

sstrings :: MutableVar (M.IntMap [(String, SString)])
sstrings = unsafePerformIO $ newVar (M.empty)


-- string hash function, stolen from FString

hashStr :: String -> Int
hashStr s = f s 0
        where f "" r = r
              f (c:cs) r = f cs (r*16+r+fromEnum c)


-- unique id factory

-- Nothing denotes the untouched interner. Allocation or restoration starts
-- its lifecycle, including restoration of an empty snapshot (Just 0). Keep
-- that distinction in the existing counter rather than a second global cell.
nextInt :: MutableVar (Maybe Int)
nextInt = unsafePerformIO $ (newVar Nothing)

freshInt :: IO Int
freshInt = do fresh <- fmap (fromMaybe 0) (readVar nextInt)
              writeVar nextInt (Just (fresh + 1))
              return fresh


-- A capture contains every spelling, including entries no longer reachable
-- from the scheduled package. Numeric IDs must be preserved: SString's Ord
-- instance compares interned IDs, not string contents.
data InternSnapshot = InternSnapshot
    { internNextId :: Int
    , internEntries :: [(Int, String)]
    } deriving (Eq, Read, Show)

-- Call only once ordinary output has been fully written.  These operations,
-- like the existing interner, require a single compiler thread.
snapshotInternState :: IO InternSnapshot
snapshotInternState = do
    n <- fmap (fromMaybe 0) (readVar nextInt)
    sm <- readVar strings
    return (InternSnapshot n (M.toAscList sm))

-- Restore must precede flag parsing, native .ba decoding and evaluation of any
-- interned CAF.  Replacing a populated table would invalidate live SStrings;
-- even restoring an empty snapshot consumes the one startup restore slot.
restoreInternState :: InternSnapshot -> IO ()
restoreInternState snapshot@(InternSnapshot n entries) = do
    sm <- readVar strings
    ssm <- readVar sstrings
    next <- readVar nextInt
    -- Use catchable plain-string IO errors: compiler diagnostics can force
    -- interned positions (even noPosition). Rejected restores must leave the
    -- table untouched; invalid input is not an internal compiler error.
    unless (next == Nothing && M.null sm && M.null ssm) $
        ioError (userError "ECO intern restore must run once in a fresh process before interning")
    unless (validSnapshot snapshot) $
        ioError (userError "ECO intern snapshot has invalid, missing or duplicate entries")
    let !sm' = M.fromDistinctAscList entries
        !ssm' = M.fromListWith (Prelude.++)
                    [(hashStr s, [(s, SString i)]) | (i,s) <- entries]
    writeVar strings sm'
    writeVar sstrings ssm'
    writeVar nextInt (Just n)

validSnapshot :: InternSnapshot -> Bool
validSnapshot (InternSnapshot n entries) =
    n >= 0 && map fst entries == [0 .. n - 1] &&
    Set.size (Set.fromList (map snd entries)) == n
