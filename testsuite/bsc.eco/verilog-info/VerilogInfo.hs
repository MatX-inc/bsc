module Main where
import Control.Exception (SomeException,try)
import Control.Monad (unless)
import ECO.VerilogInfo
import ECO.Replay (FileIdentity(..))
import System.Environment (getArgs)
import System.Directory (createDirectoryIfMissing,renameDirectory,createDirectoryLink,canonicalizePath,doesFileExist)
import System.FilePath ((</>),takeDirectory)
import System.Posix.Files (createLink)
import Verilog
import ASyntax
import BinData (Bin(..), encode, decode)
import qualified Data.ByteString as BS
import qualified Data.Generics as G
import ECO.Replay (fileIdentity)

-- Exercise the native payload format directly; no emitted RTL is parsed.
data Payload = Payload String VProgram [AVInst]
instance Bin Payload where
    writeBytes (Payload h p i) = do
        toBin h; toBin p; toBin i
    readBytes = do
        h<-fromBin; p<-fromBin; i<-fromBin
        return (Payload h p i)

check name condition = do
    unless condition (fail name)
    putStrLn ("PASS: " ++ name)
reject name action = do
    result <- try (action >> return ()) :: IO (Either SomeException ())
    check name (either (const True) (const False) result)
-- A rejected emission must have written nothing: the gates run first.
rejectUnwritten name path action = do
    reject name action
    written <- doesFileExist path
    check (name ++ " before any byte is written") (not written)

-- Remove the items satisfying p from a body, looking through groups.
pull p = foldr step ([],[])
  where
    step x (ks,rs) | p x = (ks,x:rs)
    step (VMGroup t xss) (ks,rs) = let (kss,rss) = unzip (map (pull p) xss) in (VMGroup t kss:ks,concat rss++rs)
    step x (ks,rs) = (x:ks,rs)
regGroup n (VMComment _ x) = regGroup n x
regGroup n (VMRegGroup i _ _ _) = getVIdString i == n
regGroup _ _ = False
rename from to = G.everywhere (G.mkT (\(VId s i m) -> VId (if s == from then to else s) i m))

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
    check "ordinary capture renders exactly" (viExactRendering new && aePlan (viEnvelope new) == Nothing)
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
        Payload header program instances = decode (BS.drop 22 bytes)
        tamper label payload = do
            let bin = out </> label ++ ".bin"
                env = out </> label ++ ".eco-ast"
            BS.writeFile bin (magic `BS.append` BS.pack (encode payload))
            ident <- fileIdentity bin
            writeFile env (show (envelope {aePayload=ident}) ++ "\n")
            readVerilogInfo env
    check "payload magic names the current format" (magic == BS.pack (map (fromIntegral . fromEnum) "bsc-eco-verilog-ast-2\n"))
    reject "self-consistent payload cannot inject RTL in renderer header"
        (tamper "header" (Payload ("wire injected;\n" ++ header) program instances))
    let VProgram originalMods dpi comments0 = program
        modified = VProgram [m {vm_ports=[]} | m<-originalMods] dpi comments0
    reject "self-consistent payload must match retained native AST"
        (tamper "program" (Payload header modified instances))
    changedMetadata <- tamper "child-metadata"
        (Payload header program [i {avi_type=ATBit 999999} | i<-instances])
    check "changed child metadata cannot authorize pin directions"
        (null (viInstances changedMetadata) && not (null (viUnboundInstances changedMetadata)))
    reject "bound artifacts cannot be overwritten" (emitVerilog new (fiPath (aePayload envelope)) (viProgram new))
    let payloadAlias = out </> "payload-hardlink.v"
    createLink (fiPath (aePayload envelope)) payloadAlias
    reject "hard links cannot overwrite bound artifacts"
        (emitVerilog new payloadAlias (viProgram new))
    payloadAfter <- fileIdentity (fiPath (aePayload envelope))
    check "rejected hard-link output preserved payload bytes" (payloadAfter == aePayload envelope)
    let VProgram mods dpis comments = viProgram new
        changed = VProgram [m {vm_ports=[]} | m<-mods] dpis comments
    reject "AST output fixes module ports" (emitVerilog new (out </> "badports.v") changed)
    check "child annotations are bound or explicitly unknown" (not (null (viInstances new)) || not (null (viUnboundInstances new)))
    check "native clock and reset lowering retains authenticated child types" (not (null (viInstances new)))
    putStrLn ("BOUND-CHILDREN: " ++ show (length (viInstances new),viUnboundInstances new))

    -- Lexical and declared-before-use gates: the ordinary programs pass; a
    -- comment element with a line break, an identifier that ends a statement
    -- and a declaration moved after its uses are refused before emission.
    check "ordinary programs pass both emission gates"
        (all (\p -> lexicallyEmittable p == Right () && declaredBeforeUse p == Right ()) [viProgram old,viProgram new])
    let m:ms = mods
        wrap item = VProgram (m {vm_body=item:vm_body m}:ms) dpis comments
        annotated = wrap (VMComment ["ECO note"] (VMGroup False []))
        escaped = "note\nassign result = 8'd0;"
        injected = wrap (VMComment ["ECO note",escaped] (VMGroup False []))
    emitVerilog new (out </> "annotated.v") annotated
    check "a changed program with a one-line comment is emitted" True
    check "comment element with an embedded newline is refused" (lexicallyEmittable injected == Left escaped)
    rejectUnwritten "emission refuses a comment element with an embedded newline" (out </> "comment-newline.v")
        (emitVerilog new (out </> "comment-newline.v") injected)
    let badName = "a; assign result = 8'd0; wire junk"
        renamed = rename "a" badName (viProgram new)
    check "fixture declares the renamed register" (renamed /= viProgram new)
    check "identifier containing ';' is refused" (lexicallyEmittable renamed == Left badName)
    rejectUnwritten "emission refuses an identifier containing ';'" (out </> "bad-identifier.v")
        (emitVerilog new (out </> "bad-identifier.v") renamed)
    let (kept,moved) = pull (regGroup "a") (vm_body m)
        late = VProgram (m {vm_body=kept++moved}:ms) dpis comments
    check "fixture declares register a in a reg group" (not (null moved))
    check "a wire used before its declaration is refused"
        (lexicallyEmittable late == Right () && either (`elem` ["a","a$D_IN","a$EN"]) (const False) (declaredBeforeUse late))
    rejectUnwritten "emission refuses a program using a wire before its declaration" (out </> "use-before-declare.v")
        (emitVerilog new (out </> "use-before-declare.v") late)

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
