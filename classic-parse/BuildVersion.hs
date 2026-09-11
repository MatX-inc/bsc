-- | Stand-in for the @BuildVersion@ module that bsc's cabal Setup hooks
-- generate (via @src/comp/update-build-version.sh@).  Only
-- 'Version.bscVersionStr', which shows up in internal-error messages, looks
-- at it.
module BuildVersion(buildVersion, buildVersionNum, buildVersionName) where

buildVersion :: String
buildVersion = "0000000"

buildVersionNum :: Integer
buildVersionNum = 0

buildVersionName :: String
buildVersionName = "classic-parse"
