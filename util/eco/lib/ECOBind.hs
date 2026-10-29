-- The one bound-artifact helper set shared by the external verifiers. Every
-- bundle member is contained, read once and hashed; a stored digest is a
-- binding to check, never a grant.
module ECOBind (failWith, readRecord, unchanged, boundMember, closureIdentities) where
import Control.Exception (evaluate)
import Control.Monad (forM, unless)
import Data.List (sort)
import System.Directory
import System.FilePath
import Text.Read (readMaybe)
import ECO.Replay (FileIdentity(..), fileIdentity)

failWith :: String -> String -> IO a
failWith component message = ioError (userError (component ++ ": " ++ message))

readRecord :: Read a => FilePath -> IO a
readRecord path = do
  text <- readFile path
  _ <- evaluate (length text)
  maybe (failWith "bound artifact" ("invalid typed record: " ++ path)) return (readMaybe text)

-- The recorded path is re-hashed; a capture directory that moved behind a
-- compatibility symlink keeps its digest and stays bound.
unchanged :: FileIdentity -> IO ()
unchanged expected = do
  actual <- fileIdentity (fiPath expected)
  unless (fiSHA256 actual == fiSHA256 expected) (failWith "bound artifact" ("changed bound input: " ++ fiPath expected))

-- Containment forbids traversal and symlink escape before the digest check.
boundMember :: FilePath -> FilePath -> String -> IO FilePath
boundMember bundle relative expected = do
  unless (isRelative relative && ".." `notElem` splitDirectories relative)
    (failWith "bound artifact" ("artifact path escapes bundle: " ++ relative))
  root <- canonicalizePath bundle
  path <- canonicalizePath (root </> relative)
  let prefix = addTrailingPathSeparator root
  unless (take (length prefix) path == prefix)
    (failWith "bound artifact" ("artifact symlink escapes bundle: " ++ relative))
  identity <- fileIdentity path
  unless (fiSHA256 identity == expected)
    (failWith "bound artifact" ("candidate artifact changed: artifact digest changed: " ++ relative))
  return path

closureIdentities :: FilePath -> IO [FileIdentity]
closureIdentities root = do
  names <- fmap sort (listDirectory root)
  fmap concat $ forM names $ \name -> do
    let path = root </> name
    isDirectory <- doesDirectoryExist path
    if isDirectory then closureIdentities path else fmap (: []) (fileIdentity path)
