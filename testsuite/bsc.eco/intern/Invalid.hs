module Main(main) where

import InternTest(mustReject)
import qualified SpeedyString as S

main :: IO ()
main = do
    mapM_ mustReject [ S.InternSnapshot (-1) []
                    , S.InternSnapshot 2 [(0,"a")]
                    , S.InternSnapshot 2 [(0,"a"),(0,"b")]
                    , S.InternSnapshot 2 [(1,"a"),(0,"b")]]
    S.restoreInternState (S.InternSnapshot 1 [(0,"valid")])
