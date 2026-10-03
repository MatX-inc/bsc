{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, (++), concat, filter,
                    internTable) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import IOMutVar(MutableVar, newVar, readVar, writeVar)
import System.IO.Unsafe(unsafePerformIO)
import System.Environment(getArgs, lookupEnv)
import qualified Data.IntMap.Strict as M
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

fromString :: String -> SString
fromString s = unsafePerformIO $
               do m <- readVar sstrings
                  return $ maybe (newSString s) id $ M.lookup (hashStr s) m >>= lookup s

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

nextInt :: MutableVar Int
nextInt = unsafePerformIO $ (newVar (if reverseIntern then maxBound else 0))

freshInt :: IO Int
freshInt = do fresh <- readVar nextInt
              writeVar nextInt (if reverseIntern then fresh - 1 else fresh + 1)
              return fresh

-- With -reverse-intern-order (on the command line or in BSC_OPTIONS) the
-- ids are handed out from maxBound down instead of from 0 up, so every
-- order taken from Ord on an interned string (FString, Id) is reversed.
-- Nothing else changes, so an output that differs between a build with
-- the flag and one without depends on string-intern order: the order in
-- which the process first saw the strings, which a `bsc -u` batch or an
-- unrelated edit can change.  The cost is one test per new string.  The
-- flag is a (hidden) member of Flags, where it is decoded like any other,
-- but interning begins before the flags are decoded, so the switch is
-- read here from the raw command line and environment.
-- every interned string with its id, in id order (for -print-intern-order)
internTable :: IO [(Int, String)]
internTable = fmap M.toAscList (readVar strings)

reverseIntern :: Bool
reverseIntern = unsafePerformIO $ do
    args <- getArgs
    env <- lookupEnv "BSC_OPTIONS"
    let flag = "-reverse-intern-order"
    return (flag `elem` args || flag `elem` maybe [] words env)
{-# NOINLINE reverseIntern #-}
