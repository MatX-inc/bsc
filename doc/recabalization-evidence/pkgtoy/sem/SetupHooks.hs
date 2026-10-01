module SetupHooks (setupHooks) where
import ToyHooks (toyHooks)
import Distribution.Simple.SetupHooks (SetupHooks)
setupHooks :: SetupHooks
setupHooks = toyHooks
