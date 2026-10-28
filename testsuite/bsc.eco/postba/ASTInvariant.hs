-- Typed metadata is lowered to assertions and independently checked on the
-- native AST. No compiler proof annotation is accepted as an assumption.
module Main where
import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import ECOAST (readVerilogInfo,viProgram,renderVerilogText)
import ECOInvariantExport (InvariantExportPolicy(..),runOptionalInvariantExport)
import ECOASTInvariant
import Data.IORef
import ECOASTProof (readASTContracts)
import ECO.Replay (fileIdentity)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Verilog

v=mkVId
e=VEVar . v
c w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
assign n x=VMAssign (VLId (v n)) x
program=VProgram [VModule (v "m") []
  [([VAInput (v "a") (range 8),VAInput (v "b") (range 8),VAOutput (v "o") (range 8)],[])]
  [VMDecl (VVDecl VDReg (range 8) [VVar (v "state")]),
   VMStmt True (Vinitial (VAssign (VLId (v "state")) (c 8 0))),assign "o" (e "a")]] [] []
obj=JObject
txt=JString
num=JNumber . show
typed w xs=obj (("type",obj [("bits",num w)]):xs)
leaf w n=typed w [("kind",txt "port"),("id",txt n)]
ref w n=typed w [("kind",txt "definition"),("id",txt n)]
constant w n=typed w [("kind",txt "integer"),("value",txt (show n))]
prim w op xs=typed w [("kind",txt "primitive"),("operator",txt op),("arguments",JArray xs)]
definition w n rhs=typed w [("id",txt n),("expression",rhs)]
obligation a b=obj [("messageKind",txt "test"),("classification",txt "compiler-proved"),
  ("operation",txt "eq"),("left",a),("right",b)]
payload defs obs=obj [("schema",num 1),("availability",txt "captured"),("snapshots",JArray
  [obj [("schema",num 1),("domain",txt "compiler-two-state-IR"),("module",txt "m"),
    ("stage",txt "before-checkproofs"),("definitions",JArray defs),
    ("observedObligations",JArray obs),("pendingObligations",JArray []),
    ("methods",JArray [obj [("callerEnableImpliesReadyProved",JBool False)]]),
    ("representations",JArray [obj [("rawInputValidityProved",JBool False)]]),
    ("scheduleDeclarations",JArray [obj [("classification",txt "user-declaration-not-proof")]])]])]
check name passed=unless passed (error name)
failure action=do result<-try (action >> return ()) :: IO (Either SomeException ()); return (isLeft result)

main=do
  args<-getArgs
  (yosys,dir,native)<-case args of
    [y,d]->return (y,d,[])
    [y,d,p,n]->return (y,d,[p,n])
    _->error "Usage: ASTInvariant.hs YOSYS OUTPUT [PROVED-SNAPSHOT UNPROVED-SNAPSHOT]"
  createDirectoryIfMissing True dir
  ran <- newIORef False
  let deferred = dir </> "export-deferred"
  runOptionalInvariantExport DeferInvariants deferred (writeIORef ran True >> error "deferred export ran")
  deferredStatus <- readFile (deferred </> "status.json")
  didRun <- readIORef ran
  check "explicit deferral skips optional work and records no assumptions"
    (not didRun && "deferred" `isInfixOf` deferredStatus && "\"assumptionsExported\":0" `isInfixOf` deferredStatus)
  let completed = dir </> "export-completed"
  runOptionalInvariantExport ExportInvariants completed $ do
    pending <- readFile (completed </> "status.json")
    length pending `seq` check "export records pending before work" ("pending" `isInfixOf` pending)
    writeFile (completed </> "report.json") "{}\n"
    fileIdentity (completed </> "report.json")
  completedStatus <- readFile (completed </> "status.json")
  check "completed export records exact report identity"
    ("completed" `isInfixOf` completedStatus && "sha256" `isInfixOf` completedStatus)
  let unresolved = dir </> "export-unresolved"
  runOptionalInvariantExport ExportInvariants unresolved (ioError (userError "directed export failure"))
  unresolvedStatus <- readFile (unresolved </> "status.json")
  check "optional export failure is explicit and does not fail RTL result"
    ("unresolved" `isInfixOf` unresolvedStatus && "directed export failure" `isInfixOf` unresolvedStatus)
  let run name bindings defs obs=consumeEvidenceJSON yosys 30 (dir </> name) program [] bindings
                                      (renderEvidenceJSON (payload defs obs))
      count=length . invariantContracts
      one=constant 1 1
      a=leaf 8 "a"
      self=prim 1 "PrimEQ" [a,a]
  positive<-run "expanded" [] [definition 1 "observed" self] [obligation (ref 1 "observed") one]
  check "typed definition expansion independently proves assertion" (count positive==1)
  falseClaim<-run "false-annotation" [] [] [obligation a (leaf 8 "b")]
  check "compiler-proved annotation cannot authorize false final AST fact" (count falseClaim==0)
  initialized<-run "initialized-state" [] [] [obligation (leaf 8 "state") (constant 8 0)]
  check "initial block does not authorize combinational state invariant" (count initialized==0)
  missing<-run "unbound" [] [] [obligation (leaf 8 "missing") a]
  check "unbound input cannot become an implicit net" (count missing==0 && all ((==Nothing).irPredicate) (irRows missing))
  wrongWidth<-run "width" [] [] [obligation (leaf 7 "a") (constant 7 0)]
  check "metadata widths must match actual declarations" (count wrongWidth==0)
  cycle<-run "cycle" [] [definition 1 "loop" (ref 1 "loop")] [obligation (ref 1 "loop") one]
  check "cyclic metadata definition is unresolved" (count cycle==0)
  unsupported<-run "opaque" [] [] [obligation (typed 1 [("kind",txt "opaque")]) one]
  check "opaque compiler IR is not parsed" (count unsupported==0)
  rebound<-run "binding" [("observed_a","a")] [] [obligation (leaf 8 "observed_a") a]
  check "explicit postmatching binding reproves the actual expression" (count rebound==1)
  invalidBinding<-failure (run "duplicate-binding" [("x","a"),("x","b")] [] [])
  check "duplicate external binding source rejected" invalidBinding
  outOfRange<-run "literal-width" [] [] [obligation (constant 1 2) one]
  check "out-of-range metadata literals rejected" (count outOfRange==0)
  badArity<-run "arity" [] [] [obligation (prim 1 "PrimEQ" [a]) one]
  check "primitive arity rejected before proof" (count badArity==0)
  check "duplicate JSON keys rejected" (isLeft (parseEvidenceJSON "{\"schema\":1,\"schema\":2}"))
  check "trailing JSON rejected" (isLeft (parseEvidenceJSON "{} false"))
  let json=payload [] [obligation self one]
  check "metadata JSON roundtrip" (parseEvidenceJSON (renderEvidenceJSON json)==Right json)
  descriptors<-run "descriptors" [] [] []
  check "method/type/user scheduling descriptors remain non-assumptions"
    (count descriptors==0 && "user-declaration-not-proof" `isInfixOf` renderInvariantReport descriptors)
  writeInvariantOutputs (dir </> "export") positive
  decoded<-readASTContracts (dir </> "export/assertions.ast-contracts")
  check "exported assertions preserve native typed contracts" (decoded==invariantContracts positive)
  let printed=renderInvariantAssertions positive
  check "SV include contains assertions only" ("assert (" `isInfixOf` printed && not ("assume (" `isInfixOf` printed))
  let helperProgram helper=case program of
        VProgram [m] dpi comments->VProgram [m {vm_body=vm_body m++
          [VMDecl (VVDecl VDWire (range 8) [VVar (v helper)]),assign helper (e "a"),
           VMDecl (VVDecl VDWire (range 8) [VVar (v "wrong")]),assign "wrong" (e "b")]}] dpi comments
      sourceProgram=helperProgram "observed_q"
      selectedProgram=helperProgram "historical_q"
      relations=[[ ("observed_q","intermediate_q") ],[("intermediate_q","historical_q")]]
      selectedObservation=renderEvidenceJSON (payload [] [obligation (ref 8 "observed_q") a])
  finalBindings<-either error return (composeInvariantBindings sourceProgram relations)
  check "accepted alpha relations compose source to selected signal" (lookup "observed_q" finalBindings==Just "historical_q")
  selectedFacts<-consumeEvidenceJSON yosys 30 (dir </> "selected-renamed") selectedProgram [] finalBindings selectedObservation
  check "renamed selected helper is independently reproved" (count selectedFacts==1)
  wrongFacts<-consumeEvidenceJSON yosys 30 (dir </> "selected-wrong-map") selectedProgram []
    [("observed_q","wrong")] selectedObservation
  check "wrong selected binding cannot trust an earlier proof" (count wrongFacts==0)
  check "ambiguous step relation cannot supply invariant binding"
    (isLeft (composeInvariantBindings sourceProgram [[("observed_q","x"),("wrong","x")]]))
  case native of
    [proved,unproved]->do
      ps<-readVerilogInfo proved
      pr<-consumeASTEvidence yosys 30 (dir </> "native-proved") ps []
      check "actual checked compiler evidence re-proves" (count pr>0 && length (irSources pr)>=5)
      ns<-readVerilogInfo unproved
      nr<-consumeASTEvidence yosys 30 (dir </> "native-unproved") ns []
      check "actual inconclusive compiler evidence remains unresolved" (count nr==0 && not (null (irRows nr)))
      let selectedPath=dir </> "native-selected.v"
      writeFile selectedPath (renderVerilogText ps (viProgram ps))
      selectedIdentity<-fileIdentity selectedPath
      selectedNative<-consumeSelectedASTEvidence yosys 30 (dir </> "native-selected") ps ps
        (viProgram ps) selectedIdentity []
      check "native source evidence binds separately to selected output"
        (count selectedNative>0 && lookup "targetRTL" (irSources selectedNative)==Just selectedIdentity)
      writeFile selectedPath "changed after proof target selection\n"
      changedTarget<-failure (consumeSelectedASTEvidence yosys 30 (dir </> "native-selected-tampered") ps ps
        (viProgram ps) selectedIdentity [])
      check "selected emitted bytes must match final typed AST and identity" changedTarget
      writeFile selectedPath (renderVerilogText ps (viProgram ps))
      let changedChildren=case viProgram ps of
            VProgram [m] dpi comments->VProgram [m {vm_body=VMInst (v "DifferentChild") (v "unboundChild") (Right []) []:vm_body m}] dpi comments
          childPath=dir </> "native-selected-changed-child.v"
      writeFile childPath (renderVerilogText ps changedChildren)
      childIdentity<-fileIdentity childPath
      changedChild<-failure (consumeSelectedASTEvidence yosys 30 (dir </> "native-selected-changed-child") ps ps
        changedChildren childIdentity [])
      check "final AST cannot reuse pin metadata for another child interface" changedChild
      writeInvariantOutputs (dir </> "native-proved") pr
      writeInvariantOutputs (dir </> "native-unproved") nr
      writeInvariantOutputs (dir </> "native-selected") selectedNative
      putStrLn "PASS: 30 invariant consumer checks (including native evidence and selected-output binding)"
    _->putStrLn "PASS: 25 invariant consumer checks"
