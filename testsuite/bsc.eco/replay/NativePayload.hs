module Main(main) where

import Control.Monad(unless)
import Data.Maybe(isJust)
import System.Environment(getArgs)

import ABin(ABin(..), ABinModInfo(..))
import ABinUtil(readAndCheckABin)
import Backend(Backend(..))
import Error(initErrorHandle)

-- Inspect native retention without replaying or restoring the captured table.
-- The compiler tests separately prove exact replay of this same payload.
main :: IO ()
main = do
    args <- getArgs
    case args of
      [path, expected] | expected `elem` ["absent", "retained"] -> do
        errh <- initErrorHandle
        (_, abin) <- readAndCheckABin errh (Just Verilog) path
        case abin of
          ABinMod info _ ->
            unless (isJust (abmi_vprogram info) == (expected == "retained")) $
              ioError (userError ("native VProgram should be " ++ expected))
          _ -> ioError (userError "native payload is not a scheduled module")
      _ -> ioError (userError "NativePayload.hs MODULE.ba absent|retained")
