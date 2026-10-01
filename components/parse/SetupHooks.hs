module SetupHooks (setupHooks) where
import BscSetupHooks (bscSetupHooks)
import Distribution.Simple.SetupHooks (SetupHooks)
setupHooks :: SetupHooks
setupHooks = bscSetupHooks
