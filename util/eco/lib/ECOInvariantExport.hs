-- Optional export status is separate from candidate acceptance.
module ECOInvariantExport (InvariantExportPolicy(..), runOptionalInvariantExport) where
import Control.Exception (IOException, try)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import ECOASTInvariant
import ECO.Replay (FileIdentity(..))

-- Export is independent of candidate correctness; deferral never supplies facts
-- to the proof gate. Keep CaptureFlow's serialized format backward compatible.
data InvariantExportPolicy = ExportInvariants | DeferInvariants deriving (Eq,Read,Show)

runOptionalInvariantExport :: InvariantExportPolicy -> FilePath -> IO FileIdentity -> IO ()
runOptionalInvariantExport policy directory action = do
    createDirectoryIfMissing True directory
    let status value = writeFile (directory </> "status.json") (renderEvidenceJSON value ++ "\n")
        unfinished label reason = JObject [("status",JString label),("reason",JString reason),
                                           ("assumptionsExported",JNumber "0")]
    case policy of
      DeferInvariants -> status (unfinished "deferred" "explicit caller policy; RTL proof and replay completed independently")
      ExportInvariants -> do
        status (unfinished "pending" "optional invariant export in progress")
        result <- try action :: IO (Either IOException FileIdentity)
        status $ case result of
          Right ident -> JObject [("status",JString "completed"),("report",JString (fiPath ident)),
                                   ("sha256",JString (fiSHA256 ident))]
          Left err -> unfinished "unresolved" (show err)
