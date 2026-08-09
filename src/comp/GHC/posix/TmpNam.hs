module TmpNam(tmpNam, localTmpNam) where
import System.Directory (getTemporaryDirectory)
import System.FilePath ((</>))
import System.Posix (getProcessID)

tmpNam :: IO String
tmpNam = do
         dir <- getTemporaryDirectory
         x <- localTmpNam
         return $ dir </> ("bsc" ++ x)

localTmpNam :: IO String
localTmpNam = do
         x <- getProcessID
         return $ show x
