module Main where
import Lex
import FStringCompat(mkFString)
main :: IO ()
main = do
  let lf = LFlags { lf_is_stdlib = False, lf_allow_sv_kws = True }
  print (length (lexStart lf (mkFString "t.bs") "x = 1 + 2\n"))
