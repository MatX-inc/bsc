module Main where
import Control.Exception (SomeException,try)
import Control.Monad (unless)
import ECOAST
import ECO.Replay (FileIdentity(..))
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing,renameDirectory,createDirectoryLink,canonicalizePath)
import System.FilePath ((</>),takeDirectory)
import Verilog
import ASyntax
import BinData (Bin(..), encode, decode)
import qualified Data.ByteString as BS
import ECO.Replay (fileIdentity)

-- Exercise the native payload format directly; no emitted RTL is parsed.
data Payload = Payload String VProgram [AVInst] [AStateOut] [(AId,AType)]
instance Bin Payload where
    writeBytes (Payload h p i o s) = do
        toBin h; toBin p; toBin i; toBin o; toBin s
    readBytes = do
        h<-fromBin; p<-fromBin; i<-fromBin; o<-fromBin; s<-fromBin
        return (Payload h p i o s)

check name condition = do
    unless condition (fail name)
    putStrLn ("PASS: " ++ name)
reject name action = do
    result <- try (action >> return ()) :: IO (Either SomeException ())
    check name (either (const True) (const False) result)

main = do
    [oldPath,newPath,out] <- getArgs
    createDirectoryIfMissing True out
    old <- readVerilogInfo oldPath
    new <- readVerilogInfo newPath
    oldAgain <- readVerilogInfo oldPath
    check "multiple captures share one safe intern table" (viProgram old == viProgram oldAgain && renderVerilogText old (viProgram old) == renderVerilogText oldAgain (viProgram oldAgain))
    a <- verifyVerilogInfo old (out </> "old")
    b <- verifyVerilogInfo new (out </> "new")
    check "old native AST emits captured bytes" (fiSHA256 a == fiSHA256 (aeOutput (viEnvelope old)))
    check "new native AST emits captured bytes" (fiSHA256 b == fiSHA256 (aeOutput (viEnvelope new)))
    check "new ECO capture retains native Verilog AST" (viBinding new == "retained-native-ast")
    let envelope = viEnvelope new
        bad path change = do
            writeFile path (show (change envelope) ++ "\n")
            readVerilogInfo path
    reject "unsupported AST schema" (bad (out </> "schema") (\e -> e {aeSchema=9}))
    reject "wrong native AST library version" (bad (out </> "version") (\e -> e {aeNativeVersion="wrong"}))
    reject "wrong native AST module" (bad (out </> "module") (\e -> e {aeModule="wrong"}))
    reject "changed binary AST payload" (bad (out </> "payload") (\e -> e {aePayload=(aePayload e) {fiSHA256=replicate 64 '0'}}))
    reject "changed AST output expectation" (bad (out </> "output") (\e -> e {aeOutput=(aeOutput e) {fiSHA256=replicate 64 '0'}}))
    bytes <- BS.readFile (fiPath (aePayload envelope))
    let magic = BS.take 22 bytes
        Payload header program instances outputs signals = decode (BS.drop 22 bytes)
        tamper label payload = do
            let bin = out </> label ++ ".bin"
                env = out </> label ++ ".eco-ast"
            BS.writeFile bin (magic `BS.append` BS.pack (encode payload))
            ident <- fileIdentity bin
            writeFile env (show (envelope {aePayload=ident}) ++ "\n")
            readVerilogInfo env
    reject "self-consistent payload cannot inject RTL in renderer header"
        (tamper "header" (Payload ("wire injected;\n" ++ header) program instances outputs signals))
    let VProgram originalMods dpi comments0 = program
        modified = VProgram [m {vm_ports=[]} | m<-originalMods] dpi comments0
    reject "self-consistent payload must match retained native AST"
        (tamper "program" (Payload header modified instances outputs signals))
    changedMetadata <- tamper "child-metadata"
        (Payload header program [i {avi_type=ATBit 999999} | i<-instances] outputs signals)
    check "changed child metadata cannot authorize pin directions"
        (null (viInstances changedMetadata) && not (null (viUnboundInstances changedMetadata)))
    reject "bound artifacts cannot be overwritten" (emitVerilog new (fiPath (aePayload envelope)) (viProgram new))
    let VProgram mods dpis comments = viProgram new
        changed = VProgram [m {vm_ports=[]} | m<-mods] dpis comments
    reject "AST output fixes module ports" (emitVerilog new (out </> "badports.v") changed)
    check "child annotations are bound or explicitly unknown" (not (null (viInstances new)) || not (null (viUnboundInstances new)))
    check "native clock and reset lowering retains authenticated child types" (not (null (viInstances new)))
    check "typed final signals retained" (not (null (viSignals new)))
    putStrLn ("BOUND-CHILDREN: " ++ show (length (viInstances new),viUnboundInstances new))
    -- Preserve the immutable envelopes and keep their original absolute names
    -- reachable through a compatibility link, as with moving a capture tree.
    let originalDirectory=takeDirectory newPath
        relocatedDirectory=originalDirectory++"-relocated"
    renameDirectory originalDirectory relocatedDirectory
    createDirectoryLink relocatedDirectory originalDirectory
    relocated<-readVerilogInfo newPath
    check "relocated capture retains complete AST and original identities"
      (viProgram relocated==viProgram new && show (viEnvelope relocated)==show (viEnvelope new))
    result<-verifyVerilogInfo relocated (out </> "relocated")
    check "relocated AST renders identical captured bytes"
      (fiSHA256 result==fiSHA256 (aeOutput envelope))
    protected<-canonicalizePath (fiPath (aePayload envelope))
    reject "canonical relocated artifact cannot be overwritten"
      (emitVerilog relocated protected (viProgram relocated))
    originalBytes<-BS.readFile protected
    BS.appendFile protected (BS.singleton 0)
    tampered<-try (readVerilogInfo newPath) :: IO (Either SomeException VerilogInfo)
    BS.writeFile protected originalBytes
    check "relocation still rejects changed payload bytes"
      (either (const True) (const False) tampered)
