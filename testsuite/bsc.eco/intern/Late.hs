module Main(main) where

import Control.Exception(evaluate)
import InternTest(mustReject)
import qualified SpeedyString as S

main :: IO ()
main = do
    _ <- evaluate (S.fromString "already-live")
    mustReject (S.InternSnapshot 1 [(0,"different")])
