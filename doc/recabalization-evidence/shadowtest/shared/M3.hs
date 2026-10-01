module M3 (three) where
import M1
import M2
three :: T
three = case two of T n -> T (n + 1)
