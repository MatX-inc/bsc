{-# LANGUAGE CPP #-}
module IOUtil(progArgs, capturePerturbEnv, getEnvDef) where

import Warmup ()
import System.Environment(getArgs,getEnv)
import System.IO.Unsafe
import qualified Control.Exception as CE

{-# NOINLINE progArgs #-}
progArgs :: [String]
progArgs = unsafePerformIO $ do
  args <- getArgs
  bscopts <- getEnvDef "BSC_OPTIONS" ""
  return $ (words bscopts) ++ args

-- BSC_CAPTURE_PERTURB: the capture's perturbation tokens (CaptureLive),
-- read once like progArgs; a diagnostic setting that only changes the
-- facts written to a .ba's capture field
{-# NOINLINE capturePerturbEnv #-}
capturePerturbEnv :: String
capturePerturbEnv = unsafePerformIO $ getEnvDef "BSC_CAPTURE_PERTURB" ""

getEnvDef :: String -> String -> IO String
getEnvDef e d = CE.catch (getEnv e) handler
  where
    handler :: CE.SomeException -> IO String
    handler _ = return d
