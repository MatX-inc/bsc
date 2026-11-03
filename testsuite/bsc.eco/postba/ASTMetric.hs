module Main where
import Control.Exception (SomeException,try,finally)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import Data.List (isInfixOf)
import System.Directory (createDirectory, withCurrentDirectory, doesFileExist, removeFile)
import System.Environment (getArgs, setEnv, unsetEnv)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (callProcess, readProcessWithExitCode)
import BinData (encode)
import ECO.VerilogInfo
import ECOASTMetric
import ECOJSON (parseEvidenceJSON)
import ECOASTCost (LineCost(..),lineCost,changedLines)
import ECO.Replay (FileIdentity(..))
import PPrint
import Verilog

v=mkVId
e=VEVar . v
range=Just (VEConst 7,VEConst 0)
assignment n rhs=VMAssign (VLId (v n)) rhs
operation op=VEOp defaultVId (e "a") op (e "b")
program body comments=VProgram
    [VModule (v "M") comments
      [([VAInput (v "a") range,VAInput (v "b") range,VAOutput (v "o") range],comments)]
      body] [] comments
base=program [assignment "o" (operation VAdd)] []
check label value=do
    unless value (fail label)
    putStrLn ("PASS: "++label)
reject label action=do
    result<-try (action >> return ()) :: IO (Either SomeException ())
    check label (either (const True) (const False) result)

pureChecks out=do
    createDirectory out
    let info=["Combinational paths from inputs to outputs:","(a, b) -> o"]
        x=assignment "x" (e "a")
        y=assignment "y" (e "b")
        wire names=VMDecl (VVDecl VDWire range (map (VVar . v) names))
    check "information fields ignored"
      (normalizeProgram base==normalizeProgram (program [VMComment info (assignment "o" (operation VAdd))] info))
    check "register information wrapper ignored"
      (normalizeProgram base==normalizeProgram (program [VMRegGroup (v "info") "Reg" info (assignment "o" (operation VAdd))] []))
    check "blank group separation ignored"
      (normalizeProgram (program [x,y] [])==normalizeProgram (program [VMGroup False [[x],[y]]] []))
    check "prettyprinter soft wrapping differs before normalization"
      (pretty 10 10 (pPrint PDReadable 0 base)/=pretty 1000000 1000000 (pPrint PDReadable 0 base))
    check "normalized lines have no indentation or blanks"
      (all (\line->not (null line) && head line/=' ') (lines (normalizeProgram base)))
    check "operator edits counted"
      (normalizeProgram base/=normalizeProgram (program [assignment "o" (operation VSub)] []))
    check "name edits counted"
      (normalizeProgram base/=normalizeProgram (program [assignment "different" (operation VAdd)] []))
    check "item order counted"
      (normalizeProgram (program [x,y] [])/=normalizeProgram (program [y,x] []))
    check "declaration partitions counted"
      (normalizeProgram (program [wire ["x","y"],x,y] [])/=normalizeProgram (program [wire ["x"],wire ["y"],x,y] []))
    check "declaration placement counted"
      (normalizeProgram (program [wire ["x"],x,wire ["y"],y] [])/=normalizeProgram (program [wire ["x"],wire ["y"],x,y] []))
    check "native translate-off preserved"
      (isInfixOf "translate_off" (normalizeProgram (program [VMGroup True [[x]]] [])))
    check "synthesis comment retained"
      (isInfixOf "synopsys keep" (normalizeProgram (program [VMComment ["synopsys keep"] x] [])))
    check "attribute comment retained"
      (isInfixOf "(* keep = 1 *)" (normalizeProgram (program [VMComment ["(* keep = 1 *)"] x] [])))
    check "whole directive field retained"
      (isInfixOf "continuation" (normalizeProgram (program [VMComment ["pragma custom","continuation"] x] [])))
    check "semantic string remains"
      (isInfixOf "informational string" (normalizeProgram (program [assignment "o" (VEString "informational string")] [])))
    check "macro remains"
      (isInfixOf "MODE" (normalizeProgram (program [assignment "o" (VEMacro "MODE")] [])))
    let namedInstanceWith parameter value=VMInst (v "Child") (v "child") [(v parameter,Just (VEConst value))] []
    check "named parameter identity retained"
      (normalizeProgram (program [namedInstanceWith "WIDTH" 8] [])/=normalizeProgram (program [namedInstanceWith "DEPTH" 8] []))
    check "named parameter value retained"
      (normalizeProgram (program [namedInstanceWith "WIDTH" 8] [])/=normalizeProgram (program [namedInstanceWith "WIDTH" 9] []))
    check "native case flags remain"
      (isInfixOf "full_case" (normalizeProgram (program [VMStmt False (Vcase (e "a") [] False True)] [])))
    check "literal edits counted"
      (normalizeProgram (program [assignment "o" (VEWConst defaultVId 8 10 1)] [])/=
       normalizeProgram (program [assignment "o" (VEWConst defaultVId 8 10 2)] []))
    let compareMetric label lhs rhs expected=do
          let left=out </> label++"-old.v";right=out </> label++"-new.v"
          writeFile left (normalizeProgram lhs);writeFile right (normalizeProgram rhs)
          actual<-lineCost left right
          check (label++" Git Myers count") (changedLines actual==expected)
    compareMetric "information" base (program [VMComment info (assignment "o" (operation VAdd))] info) 0
    compareMetric "operator" base (program [assignment "o" (operation VSub)] []) 2
    compareMetric "name" base (program [assignment "different" (operation VAdd)] []) 2
    compareMetric "order" (program [x,y] []) (program [y,x] []) 2
    compareMetric "declaration-partition" (program [wire ["x","y"],x,y] []) (program [wire ["x"],wire ["y"],x,y] []) 3

    let escaped = "quoted\" path\\with\ncontrols\t" ++ [toEnum 1] ++ " λ"
        identity = FileIdentity escaped (replicate 64 'a')
        report = MetricReport (SnapshotSource identity) (CandidateSource identity identity identity)
                   (LineCost 3 2) (LineCost 1 1) identity identity [identity]
    check "typed metric JSON preserves escaped identity and complete fields"
      (parseEvidenceJSON (renderMetricReport report) == Right (metricReportJSON report))
    configuredGitChecks (out </> "configured-git")

-- Demonstrate the hooks are active in a private repository, then verify the
-- shared objective/reporting helper bypasses them. No user Git config changes.
configuredGitChecks out=do
    createDirectory out
    callProcess "git" ["init","--quiet",out]
    withCurrentDirectory out $ do
      writeFile "old.v" "old value\n"
      writeFile "new.v" "new value\n"
      writeFile "external.sh" "printf called >> external.called\n"
      callProcess "git" ["config","diff.external","sh ./external.sh"]
      _<-readProcessWithExitCode "git" ["diff","--no-index","--ext-diff","--","old.v","new.v"] ""
      active<-doesFileExist "external.called"
      check "external-diff sentinel is actually configured" active
      removeFile "external.called"
      raw<-lineCost "old.v" "new.v"
      invoked<-doesFileExist "external.called"
      check "line metric ignores configured external diff" (changedLines raw==2 && not invoked)
      callProcess "git" ["config","--unset","diff.external"]
      writeFile ".gitattributes" "*.v diff=ecoMetric\n"
      writeFile "textconv.sh" "printf called >> textconv.called\nprintf 'rewritten\\n'\n"
      callProcess "git" ["config","diff.ecoMetric.textconv","sh ./textconv.sh"]
      _<-readProcessWithExitCode "git" ["diff","--no-index","--textconv","--","old.v","new.v"] ""
      converted<-doesFileExist "textconv.called"
      check "textconv sentinel is actually configured" converted
      removeFile "textconv.called"
      literal<-lineCost "old.v" "new.v"
      transformed<-doesFileExist "textconv.called"
      check "line metric ignores configured text conversion" (changedLines literal==2 && not transformed)
      callProcess "git" ["config","--unset","diff.ecoMetric.textconv"]
      -- Attributes and end-of-line settings change what git compares inside
      -- a repository; the metric counts the bytes under every one of them.
      writeFile "lf.v" "a\nb\nc\n"
      writeFile "crlf.v" "a\r\nb\r\nc\r\n"
      let sentinel = do
            (code,out,_)<-readProcessWithExitCode "git" ["diff","--no-index","--numstat","--","lf.v","crlf.v"] ""
            return (code,out)
          counted label = do
            actual<-lineCost "lf.v" "crlf.v"
            check ("line metric ignores "++label) (changedLines actual==6)
      writeFile ".gitattributes" "*.v text eol=lf\n"
      (eolCode,_)<-sentinel
      check "end-of-line attribute sentinel is actually configured" (eolCode==ExitSuccess)
      counted "repository end-of-line attributes"
      writeFile ".gitattributes" "*.v binary\n"
      (_,binaryOut)<-sentinel
      check "binary attribute sentinel is actually configured" (take 1 binaryOut=="-")
      counted "repository binary attributes"
      removeFile ".gitattributes"
      callProcess "git" ["config","core.autocrlf","true"]
      (localCode,_)<-sentinel
      check "local end-of-line conversion sentinel is actually configured" (localCode==ExitSuccess)
      counted "local end-of-line configuration"
      callProcess "git" ["config","--unset","core.autocrlf"]
      writeFile "global.gitconfig" "[core]\n\tautocrlf = true\n"
      setEnv "GIT_CONFIG_GLOBAL" (out </> "global.gitconfig")
      (globalCode,_)<-sentinel
      check "global end-of-line conversion sentinel is actually configured" (globalCode==ExitSuccess)
      counted "global end-of-line configuration"
      unsetEnv "GIT_CONFIG_GLOBAL"
      setEnv "GIT_CONFIG_PARAMETERS" "'core.autocrlf=true'"
      (ambientCode,_)<-sentinel
      check "ambient configuration sentinel is actually configured" (ambientCode==ExitSuccess)
      counted "ambient command-line configuration"
      unsetEnv "GIT_CONFIG_PARAMETERS"

captureChecks oldPath newPath filteredPath out=do
    createDirectory out
    old<-readVerilogInfo oldPath
    new<-readVerilogInfo newPath
    actual<-measureAST (out </> "ordinary") (Snapshot oldPath) (Snapshot newPath)
    expected<-lineCost (fiPath (aeOutput (viEnvelope old))) (fiPath (aeOutput (viEnvelope new)))
    check "raw count is actual same-OLD byte diff" (metricRaw actual==expected)
    let VProgram ms dpi cs=viProgram new
        infoProgram=VProgram ms dpi ("Additional ordinary information":cs)
        ast=out </> "candidate.ast";rtl=out </> "candidate.v"
    BS.writeFile ast (BS.pack (encode infoProgram))
    writeFile rtl (renderVerilogText new infoProgram)
    qualified<-measureAST (out </> "candidate") (Snapshot oldPath) (ContextAST newPath ast rtl)
    check "candidate and ordinary retain the same OLD baseline" (metricOldRTL actual==metricOldRTL qualified)
    check "informational candidate normalized count is unchanged" (metricNormalized actual==metricNormalized qualified)
    check "informational candidate raw lines remain counted" (metricRaw actual/=metricRaw qualified)
    let candidateInput = ContextAST newPath ast rtl
    shared <- withMetricInputs (Snapshot oldPath) candidateInput $ \inputs -> do
        let ((_,oldProgram),(_,candidateProgram)) = metricInputPrograms inputs
        check "shared metric context retains the authenticated programs"
          (encode oldProgram == encode (viProgram old) && encode candidateProgram == encode infoProgram)
        measureMetricInputs (out </> "shared-context") inputs
    check "shared context preserves raw and normalized metrics"
      (metricRaw shared == metricRaw qualified && metricNormalized shared == metricNormalized qualified)
    retained <- withMetricInputs (Snapshot oldPath) candidateInput pure
    let restoredFile path action = do
          original <- BS.readFile path
          action `finally` BS.writeFile path original
    restoredFile rtl $ do
        appendFile rtl "// changed after authentication\n"
        reject "retained metric context rechecks candidate RTL on entry"
          (measureMetricInputs (out </> "stale-context") retained)
    restoredFile ast $
        reject "shared operation rechecks candidate AST at completion" $
          withMetricInputs (Snapshot oldPath) candidateInput $ \_ -> BS.appendFile ast (BS.pack [0])
    let payload = fiPath (aePayload (viEnvelope new))
    restoredFile payload $
        reject "shared operation revalidates native snapshot payload at completion" $
          withMetricInputs (Snapshot oldPath) candidateInput $ \_ -> BS.appendFile payload (BS.pack [0])
    check "typed report preserves authenticated report fields"
      (parseEvidenceJSON (renderMetricReport qualified) == Right (metricReportJSON qualified))
    reject "fresh output directory required" (measureAST (out </> "candidate") (Snapshot oldPath) (Snapshot newPath))
    appendFile rtl "// mismatched candidate bytes\n"
    reject "mismatched candidate RTL rejected" (measureAST (out </> "mismatch") (Snapshot oldPath) (ContextAST newPath ast rtl))
    writeFile rtl (renderVerilogText new infoProgram)
    let wrong=VProgram [m {vm_ports=[]}|m<-ms] dpi cs
    BS.writeFile ast (BS.pack (encode wrong))
    writeFile rtl (renderVerilogText new wrong)
    reject "candidate cannot change context ports" (measureAST (out </> "ports") (Snapshot oldPath) (ContextAST newPath ast rtl))
    BS.writeFile ast (BS.pack (encode infoProgram))
    writeFile rtl (renderVerilogText new infoProgram)
    let envelope=viEnvelope new
        bad=out </> "bad.eco-ast"
    writeFile bad (show (envelope {aePayload=(aePayload envelope) {fiSHA256=replicate 64 '0'}}))
    reject "tampered authenticated snapshot rejected" (measureAST (out </> "tampered") (Snapshot oldPath) (ContextAST bad ast rtl))
    reject "filter-changed ordinary rendering is unqualified"
      (measureAST (out </> "filtered") (Snapshot oldPath) (Snapshot filteredPath))
    check "metric is explicitly not a proof"
      (isInfixOf "\"equivalence_proof_performed\":false" (renderMetricReport qualified))
    check "report retains actual OLD and NEW raw identities"
      (isInfixOf (fiSHA256 (metricOldRTL qualified)) (renderMetricReport qualified) &&
       isInfixOf (fiSHA256 (metricNewRTL qualified)) (renderMetricReport qualified))

main=do
    args<-getArgs
    case args of
      ["pure",out]->pureChecks out
      ["captures",oldPath,newPath,filteredPath,out]->captureChecks oldPath newPath filteredPath out
      _->fail "ASTMetric.hs pure OUT | captures OLD NEW FILTERED OUT"
