{-# LANGUAGE DeriveDataTypeable #-}
module FStringCompat(FString, getFString,
                     tmpFString, enumFString, cloneFString, concatFString,
                     mkNumFString, mkStrFString, mkFString,
                     filterFString, fstringId,
                     fstringUtf8, mkFStringUtf8
                    ) where

-- wrapper to make SStrings look like FStrings

import Prelude hiding((++))
import qualified SpeedyString as S
import qualified Data.ByteString as BS
import qualified Data.ByteString.Short as SBS
import PPrint(PPrint(..), text)
import Util(itos)
import qualified Data.Generics as Generic


newtype FString = FString S.SString deriving (Eq,Ord,Generic.Data, Generic.Typeable)

-- The string's id (SpeedyString.sstringId): equal ids are equal strings.
fstringId :: FString -> Int
fstringId (FString s) = S.sstringId s

fromString :: String -> FString
fromString = FString . S.fromString

toString :: FString -> String
toString (FString s) = S.toString s

(++) :: FString -> FString -> FString
(FString s) ++ (FString s') = FString $ s S.++ s'

instance Show FString where
    show (FString s) = show s

instance PPrint FString where
    pPrint _ _ x = text (show x)

getFString :: FString -> String
getFString = toString

mkFString :: String -> FString
mkFString s = fromString s

-- | The UTF-8 encoding of getFString f, when it is f's stored spelling
-- (SpeedyString.utf8Bytes).
fstringUtf8 :: FString -> Maybe SBS.ShortByteString
fstringUtf8 (FString s) = S.utf8Bytes s

-- | mkFString of the String that well-formed UTF-8 bytes decode to
-- (SpeedyString.fromUtf8): checked now, interned when forced.
mkFStringUtf8 :: BS.ByteString -> Maybe FString
mkFStringUtf8 b = fmap FString (S.fromUtf8 b)

cloneFString :: [FString] -> FString -> FString
cloneFString fs f = head [f' | n <- [1..] :: [Integer],
                          let f' = f ++ fromString ('_':'_':'_':itos n),
                          f' `notElem` fs]

tmpFString :: Int -> String -> FString
tmpFString _ = fromString

-- "_" ++ str ++ itos n
enumFString :: String -> Int -> FString
enumFString str n = FString (S.fromEnumName str n)

concatFString :: [FString] -> FString
concatFString fs = FString $ S.concat [s | FString s <- fs]

filterFString :: (Char -> Bool) -> FString -> FString
filterFString pred (FString s) = FString $ S.filter pred s

mkNumFString :: Integer -> FString
mkNumFString i = fromString (itos i)

mkStrFString :: String -> FString
mkStrFString s = fromString (show s)
