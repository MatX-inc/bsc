module Assump(Assump(..)) where
import Id
import Scheme
import Subst
import Changed
import PPrint
import Eval

data Assump
        = Id :>: Scheme
        deriving (Show, Eq)

instance PPrint Assump where
    pPrint d p (i :>: s) = pparen (p > 0) $ pPrint d 0 i <+> text ":>:" <+> pPrint d 0 s

instance Types Assump where
    apSubC s (i :>: sc) = changed1 (i :>:) (apSubC s sc)
    tv      (i :>: sc) = tv sc

instance NFData Assump where
    rnf (i :>: sc) = rnf2 i sc
