-- | Construct the compiler's phase plan before choosing its interpreter.
-- Argument decoding and presentation stay in the driver; each invocation
-- supplies one plan for both execution and dependency discovery.
module CompilerInvocation
    ( CompilerInvocation(..), compilerInvocation
    ) where

import Control.Monad (unless)

import Backend (Backend(..))
import qualified BuildPlan as BP
import DependencyArtifacts (artifactMode)
import Error (ErrorHandle, exitFail)
import Flags (Flags)
import FlagsDecode (Decoded(..))
import SAT (checkSATFlags)
import SimLink (simLinkPlan)
import SourceCompile (sourceInvocationPlan)
import SystemCheck (doSystemCheck)
import TopUtils (getNow, timestampStr)
import VerilogLink (vLinkPlan)

data CompilerInvocation = CompilerInvocation
    { invocationFlags :: Flags
    , invocationMode :: String
    , invocationPlan :: BP.BuildPlan ()
    }

-- The driver unwraps a dependency-output request before calling this factory.
-- Help, diagnostics, and invocations without work do not have a compiler plan.
compilerInvocation :: ErrorHandle -> Decoded -> Maybe CompilerInvocation
compilerInvocation errh operation = case operation of
    DBlueSrc flags name -> Just $
        CompilerInvocation flags "source" (sourcePlan flags name)
    DSimLink flags top abins cs -> Just $
        CompilerInvocation flags (artifactMode flags Bluesim)
            (simLinkPlan errh flags top abins cs)
    DVerLink flags top vs abins cs -> Just $
        CompilerInvocation flags (artifactMode flags Verilog)
            (vLinkPlan errh flags top vs abins cs)
    _ -> Nothing
  where
    sourcePlan flags name = do
        started <- BP.performResult (pure getNow)
        -- checkSATFlags validates availability and returns its input unchanged.
        BP.perform (checkSATFlags errh flags >> doSystemCheck errh)
        status <- sourceInvocationPlan errh flags name
        BP.withResult ((,) <$> started <*> status) $ \(tStart, ok) -> do
            _ <- timestampStr flags "total" tStart
            unless ok (exitFail errh)
