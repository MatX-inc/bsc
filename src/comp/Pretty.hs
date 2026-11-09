module Pretty (module GHCPretty, pretty, prettyBuilder,
               -- utils
               s2par, s2docs
              ) where

import GHCPretty
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BB

--
-- The follow definitions used to be in PPrint, but this caused a
-- dependency cycle from ErrorUtil to PPrint to Util to ErrorUtil.
-- Since ErrorUtil only needed core Pretty routines and not the PPrint
-- functionality, this module was created.
--

-- XXX old comment from PPrint: pretty and string_txt should be replaced

-- Produces a string from a text "x" in Normal mode with "w" line
-- length, "w/m" ribbons per line.
pretty :: Int -> Int -> Doc -> String
pretty w m x = fullRender PageMode w (toEnum w / toEnum m) string_txt "\n" x

-- The same characters as pretty, UTF-8 encoded, for writing large files:
-- no intermediate String, and indentation (which can be most of a deeply
-- nested file) is copied from a block of tabs.
prettyBuilder :: Int -> Int -> Doc -> BB.Builder
prettyBuilder w m x =
    fullRenderWith PageMode w (toEnum w / toEnum m) builder_txt indentB
                   (BB.charUtf8 '\n') x

builder_txt :: TextDetails -> BB.Builder -> BB.Builder
builder_txt (Chr c)  b = BB.charUtf8 c `mappend` b
builder_txt (Str s)  b = BB.stringUtf8 s `mappend` b
builder_txt (PStr s) b = BB.stringUtf8 s `mappend` b

-- GHCPretty's indent: one tab per 8 columns, then spaces.
indentB :: Int -> BB.Builder -> BB.Builder
indentB k b = tabs (k `quot` 8) `mappend` BB.byteString (BS.take (k `rem` 8) spaces8) `mappend` b
  where tabs n | n <= BS.length tabBlock = BB.byteString (BS.take n tabBlock)
               | otherwise = BB.byteString tabBlock `mappend` tabs (n - BS.length tabBlock)

tabBlock, spaces8 :: BS.ByteString
tabBlock = BS.replicate 4096 9
spaces8 = BS.replicate 8 32

-- The function which tells fullRender how to compose Doc elements
-- into a String.
string_txt :: TextDetails -> String -> String
string_txt (Chr c)   s  = c:s
string_txt (Str s1)  s2 = s1 ++ s2
string_txt (PStr s1) s2 = s1 ++ s2

----

-- Pretty printing utilities

-- Creates a paragraph
s2par :: String -> Doc
s2par str = fsep $ map text (words str)

-- Creates a list of Doc, to later be joined
s2docs :: String -> [Doc]
s2docs str = map text (words str)

----
