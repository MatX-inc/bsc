-- The Setup script of the package, for a build tool that runs one rather
-- than Cabal-hooks' external hooks program: the same entry point
-- cabal-install generates for a Hooks package. cabal check requires it.
import Distribution.Simple (defaultMainWithSetupHooks)
import SetupHooks (setupHooks)

main :: IO ()
main = defaultMainWithSetupHooks setupHooks
