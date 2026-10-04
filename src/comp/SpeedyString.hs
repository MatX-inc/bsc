{-# LANGUAGE BangPatterns #-}
module SpeedyString(SString, toString, fromString, (++), concat, filter,
                    internTable) where

import Prelude hiding((++), concat, filter)
import qualified Prelude((++), filter)
import IOMutVar(MutableVar, newVar, readVar, writeVar)
import System.IO.Unsafe(unsafePerformIO)
import System.Environment(getArgs, lookupEnv)
import qualified Data.IntMap.Strict as M
import Data.List(sortOn)
-- import qualified NotSoSpeedyString
import ErrorUtil (internalError)
import qualified Data.Generics as Generic


-- An interned string.  There is one SString per distinct string in the
-- process (fromString returns the one made when the string was first
-- seen), so each field is stored once per string, and an SString never
-- consults the intern table to be read or compared.
--
--   ss_id   the unique id, handed out in first-sighting order (or from
--           maxBound down under -reverse-intern-order); Eq and Ord
--           compare it and nothing else
--   ss_str  the string itself, for toString
--
-- Two Word64 fields are reserved after ss_id, before ss_str, for the
-- fingerprint stream (ss_hash, ss_fp), so that change only appends
-- them to this constructor; the pattern sites it then widens are Eq,
-- Ord, toString, newSString and internTable below, and no other.
data SString = SString {-# UNPACK #-} !Int  -- ss_id
                       !String              -- ss_str

instance Eq SString where
    (SString i _) == (SString i' _) = i == i'

-- note that Ord is not the usual string ordering
instance Ord SString where
    compare (SString i _) (SString i' _) = compare i i'

instance Show SString where
    show = show . toString

-- Generic traversals (everywhere, listify) treat an interned string as a
-- leaf: there is nothing inside it to rewrite, a derived instance would
-- walk every character of every identifier the Verilog AST passes have
-- to visit, and a rewrite of the cached string would not re-intern it.
instance Generic.Data SString where
    gfoldl _ z s = z s
    gunfold _ _ _ = internalError "SpeedyString: gunfold"
    toConstr _ = sstringConstr
    dataTypeOf _ = sstringDataType

sstringDataType :: Generic.DataType
sstringDataType = Generic.mkDataType "SpeedyString.SString" [sstringConstr]

sstringConstr :: Generic.Constr
sstringConstr = Generic.mkConstr sstringDataType "SString" [] Generic.Prefix

-- public

toString :: SString -> String
toString (SString _ s) = s

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
                  let ss = SString id s
                  ssm <- readVar sstrings
                  let !ssm' = M.insertWith (Prelude.++)
                                          (hashStr s) [(s,ss)] ssm
                  writeVar sstrings ssm'
                  return ss

--toNotSoSpeedyString :: SString -> NotSoSpeedyString.SString
--toNotSoSpeedyString speedy = NotSoSpeedyString.fromString (toString speedy)

--fromNotSoSpeedyString :: NotSoSpeedyString.SString -> SString
--fromNotSoSpeedyString not_so_speedy =
--    fromString (NotSoSpeedyString.toString not_so_speedy)

-- internal representation

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
internTable = do
    ssm <- readVar sstrings
    return (sortOn fst [ (i, s) | bucket <- M.elems ssm, (s, SString i _) <- bucket ])

reverseIntern :: Bool
reverseIntern = unsafePerformIO $ do
    args <- getArgs
    env <- lookupEnv "BSC_OPTIONS"
    let flag = "-reverse-intern-order"
    return (flag `elem` args || flag `elem` maybe [] words env)
{-# NOINLINE reverseIntern #-}
