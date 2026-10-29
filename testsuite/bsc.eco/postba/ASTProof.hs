module Main where
import Control.Monad (unless)
import Data.Either (isLeft,isRight)
import ECOASTProof
import System.Environment (getArgs)
import System.FilePath ((</>))
import Verilog

v=mkVId
e=VEVar . v
c w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
decl ty w n=VMDecl (VVDecl ty (range w) [VVar (v n)])
assign n x=VMAssign (VLId (v n)) x
op a x b=VEOp (v "") a x b
base body=VProgram [VModule (v "m") [] [([VAInput (v "a") (range 8),VAInput (v "b") (range 8),VAInput (v "sel") Nothing,VAInput (v "clk") Nothing,VAOutput (v "o") (range 8)],[])] body] [] []
comb=base [assign "o" (op (e "a") VAdd (e "b"))]
alias=base [decl VDWire 8 "h",assign "h" (op (e "a") VAdd (e "b")),assign "o" (e "h")]
clone=base [decl VDWire 8 "h",decl VDWire 8 "q",assign "h" (op (e "a") VAdd (e "b")),assign "q" (op (e "a") VAdd (e "b")),assign "o" (e "q")]
proc n rhs=VMStmt False (Valways (VAt (VEEOr (VEE (e "a")) (VEE (e "b"))) (VAssign (VLId (v n)) rhs)))
seqp n rhs=VMStmt False (Valways (VAt (VEEposedge (e "clk")) (VAssignA (VLId (v n)) rhs)))
casep n arms=VMStmt False (Valways (VAt (VEEOr (VEE (e "sel")) (VEEOr (VEE (e "a")) (VEE (e "b")))) (Vcase (e "sel") arms False False)))
as n x=VAssign (VLId (v n)) x
defaultCase=base [decl VDReg 8 "q",casep "q" [VCaseArm [c 1 0] (as "q" (e "a")),VDefault (as "q" (e "b"))],assign "o" (e "q")]
groupedCase=base [decl VDReg 8 "q",casep "q" [VCaseArm [c 1 0] (as "q" (e "a")),VCaseArm [c 1 1] (as "q" (e "b")),VDefault (as "q" (e "b"))],assign "o" (e "q")]
complete n=casep n [VCaseArm [c 1 0] (as n (e "a")),VCaseArm [c 1 1] (as n (e "b"))]
binaryCase=base [decl VDReg 8 "q",complete "q",assign "o" (e "q")]
binaryClone=base [decl VDReg 8 "q",decl VDReg 8 "r",complete "q",complete "r",assign "o" (e "r")]
state n x=base [decl VDReg 8 n,seqp n x,assign "o" (e n)]
child ex=base [VMInst (v "Child") (v "u") (Right []) [(v "I",Just ex)],assign "o" (e "a")]
check label ok=unless ok (error label)
strict a b=isRight (checkAST StrictFourState [] a b)
reject a b=isLeft (checkAST Binary [] a b)

main=do
  args<-getArgs
  let (yosys,dir)=case args of [y,d]->(y,d);_->error "ASTProof YOSYS WORKDIR"
  check "identity" (strict comb comb)
  check "pure alias" (strict comb alias)
  check "pure clone" (strict alias clone)
  check "reorder" (strict alias (base [assign "o" (e "h"),assign "h" (op (e "a") VAdd (e "b")),decl VDWire 8 "h"]))
  check "alias alpha" (isRight (checkAST StrictFourState [("renamed","h")] alias (base [decl VDWire 8 "renamed",assign "renamed" (op (e "a") VAdd (e "b")),assign "o" (e "renamed")])))
  check "port mapping" (isLeft (checkAST Binary [("a","x")] comb comb))
  check "duplicate mapping" (isLeft (checkAST Binary [("h","x"),("h","y")] alias alias))
  check "capture mapping" (isLeft (checkAST Binary [("h","a")] alias alias))
  check "cycle" (reject comb (base [decl VDWire 8 "x",decl VDWire 8 "y",assign "x" (e "y"),assign "y" (e "x"),assign "o" (e "a")]))
  check "unmatched leaf" (reject comb (base [decl VDWire 8 "x",assign "o" (e "x")]))
  check "extra state" (reject comb (state "r" (e "a")))
  check "new ECO state identity" (strict (state "r" (e "a")) (state "r" (e "a")))
  check "state identity rename" (isRight (checkAST StrictFourState [("s","r")] (state "r" (e "a")) (state "s" (e "a"))))
  check "state transition mutation is not structural identity" (not (strict (state "r" (e "a")) (state "r" (e "b"))))
  -- Source ECOs may add state. The proof reference is ordinary-new, not old.
  let addedPipeline body=base ([decl VDReg 8 "stage0",decl VDReg 8 "stage1",
                               seqp "stage0" (e "a"),seqp "stage1" (e "stage0")]++body)
      pipelineNew=addedPipeline [assign "o" (op (e "stage1") VAdd (e "b"))]
      pipelineCandidate=addedPipeline [decl VDWire 8 "historical_result",
                          assign "historical_result" (op (e "stage1") VAdd (e "b")),
                          assign "o" (e "historical_result")]
      resetStage n rhs=VMStmt False (Valways (VAt (VEEposedge (e "clk"))
          (Vifelse (e "sel") (VAssignA (VLId (v n)) (c 8 0))
                              (VAssignA (VLId (v n)) rhs))))
      stagedReset body=base ([decl VDReg 8 "reset0",decl VDReg 8 "reset1",
                             resetStage "reset0" (e "a"),resetStage "reset1" (e "reset0")]++body)
      resetNew=stagedReset [assign "o" (e "reset1")]
      resetCandidate=stagedReset [decl VDWire 8 "reset_alias",assign "reset_alias" (e "reset1"),assign "o" (e "reset_alias")]
  check "inserted pipeline state retained" (strict pipelineNew pipelineCandidate)
  check "old is not the new-state proof reference" (reject (state "stage0" (e "a")) pipelineCandidate)
  check "inserted reset pipeline retained" (strict resetNew resetCandidate)
  check "reset stage bypass rejected" (not (strict resetNew (stagedReset [assign "o" (e "reset0")])))
  check "redundant explicit case label" (strict defaultCase groupedCase)
  let signNot=VEUnOp (v "") VNot (VESelect1 (e "a") (VEConst 0))
      fallback=op (op (e "sel") VNE (c 1 1)) VLOr signNot
      selectorCase extra def=base [decl VDReg 8 "q",casep "q"
        ([VCaseArm [c 1 0] (as "q" (e "a"))]++extra++[VDefault (as "q" def)]),assign "o" (e "q")]
      selectedArm=VCaseArm [c 1 1] (as "q" signNot)
  check "known selector specializes default strictly" (strict (selectorCase [] fallback) (selectorCase [selectedArm] fallback))
  check "wrong specialized arm rejected" (not (strict (selectorCase [] fallback) (selectorCase [VCaseArm [c 1 1] (as "q" (c 8 0))] fallback)))
  check "unknown selector default is preserved" (not (strict (selectorCase [] fallback) (selectorCase [selectedArm] signNot)))
  check "logical zero cannot erase a possible Z" (not (strict (base [assign "o" (op (c 1 0) VLOr (e "sel"))]) (base [assign "o" (e "sel")])))
  check "missing default not strict freedom" (not (strict defaultCase binaryCase))
  check "binary complete clone inventory" (isRight (checkAST Binary [] binaryCase binaryClone))
  check "strict incomplete clone common-history identity" (strict binaryCase binaryClone)
  check "changed child identity" (reject (child (e "a")) (base [VMInst (v "Other") (v "u") (Right []) [(v "I",Just (e "a"))],assign "o" (e "a")]))
  check "changed child lvalue needs direction" (reject (child (e "a")) (child (e "b")))
  let inputPin=[ChildPin "u" "I" ChildInput (Just 8)]
      outputPin=[ChildPin "u" "I" ChildOutput (Just 8)]
      template=VEIf (e "sel") (e "a") (e "a")
  check "native input template strict" (isRight (checkASTWithPins inputPin inputPin StrictFourState [] (child (e "a")) (child template)))
  check "native output template rejected" (isLeft (checkASTWithPins outputPin outputPin Binary [] (child (e "a")) (child template)))
  check "native directions cannot change" (isLeft (checkASTWithPins inputPin outputPin Binary [] (child (e "a")) (child (e "a"))))
  let incomplete x=base [decl VDReg 8 "q",VMStmt False (Valways (VAt (VEE (e "sel")) (Vif (e "sel") (as "q" x)))),assign "o" (e "q")]
  check "incomplete unchanged accepted" (strict (incomplete (e "a")) (incomplete (e "a")))
  check "incomplete changed rejected" (reject (incomplete (e "a")) (incomplete (e "b")))
  let opaqueCase selector event arms=VMStmt False (Valways (VAt
        (VEEOr (VEE event) (VEE (e "a"))) (Vcase selector arms False False)))
      opaqueArms=[VCaseArm [c 1 0] (as "q" (e "a"))]
      opaqueAlias width definition selector event arms=base
        [decl VDReg 8 "q",decl VDWire width "historical_selector",
         assign "historical_selector" definition,opaqueCase selector event arms,assign "o" (e "q")]
      opaqueOriginal=base [decl VDReg 8 "q",opaqueCase (e "sel") (e "sel") opaqueArms,assign "o" (e "q")]
      restoredSelector=opaqueAlias 1 (e "sel") (e "historical_selector") (e "historical_selector") opaqueArms
  check "incomplete case exact-width alias preserves retention" (strict opaqueOriginal restoredSelector)
  check "incomplete case alias truncation rejected" (not (strict opaqueOriginal
    (opaqueAlias 1 (e "a") (e "historical_selector") (e "historical_selector") opaqueArms)))
  check "incomplete case alias extension rejected" (not (strict opaqueOriginal
    (opaqueAlias 2 (e "sel") (e "historical_selector") (e "historical_selector") opaqueArms)))
  check "incomplete case alias cycle rejected" (not (strict opaqueOriginal
    (opaqueAlias 1 (e "historical_selector") (e "historical_selector") (e "historical_selector") opaqueArms)))
  check "incomplete case alias does not hide changed arm" (not (strict opaqueOriginal
    (opaqueAlias 1 (e "sel") (e "historical_selector") (e "historical_selector")
      [VCaseArm [c 1 0] (as "q" (e "b"))])))
  check "incomplete case alias does not invent default" (not (strict opaqueOriginal
    (opaqueAlias 1 (e "sel") (e "historical_selector") (e "historical_selector")
      (opaqueArms++[VDefault (as "q" (e "a"))]))))
  check "incomplete case alias preserves sensitivity" (not (strict opaqueOriginal
    (opaqueAlias 1 (e "sel") (e "historical_selector") (e "clk") opaqueArms)))
  let opaqueWithDriver rhs useAlias=base
        ([decl VDReg 8 "q",decl VDWire 1 "terminal_selector",
          assign "terminal_selector" rhs]++
         (if useAlias then [decl VDWire 1 "historical_selector",
                           assign "historical_selector" (e "terminal_selector")] else [])++
         [opaqueCase (e (if useAlias then "historical_selector" else "terminal_selector"))
                     (e (if useAlias then "historical_selector" else "terminal_selector")) opaqueArms,
          assign "o" (e "q")])
  check "opaque alias terminal cone remains an obligation" (not (strict
    (opaqueWithDriver (op (e "sel") VLOr (c 1 0)) False)
    (opaqueWithDriver (op (e "clk") VLOr (c 1 0)) True)))
  let badSensitivity=base [decl VDReg 8 "q",VMStmt False (Valways (VAt (VEE (e "sel")) (as "q" (e "a")))),assign "o" (e "q")]
  check "sensitivity is functional boundary" (reject (base [decl VDReg 8 "q",proc "q" (e "a"),assign "o" (e "q")]) badSensitivity)
  check "extra effectful helper" (reject comb (base [decl VDWire 8 "random",assign "random" (VEFctCall (v "$random") []),assign "o" (op (e "a") VAdd (e "b"))]))
  check "changed initial" (reject (base [decl VDReg 8 "r",VMStmt False (Vinitial (as "r" (c 8 0))),assign "o" (e "r")]) (base [decl VDReg 8 "r",VMStmt False (Vinitial (as "r" (c 8 1))),assign "o" (e "r")]))
  let commute=base [assign "o" (op (e "b") VAdd (e "a"))]
      wrong=base [assign "o" (op (e "a") VSub (e "b"))]
  binary<-runASTProof yosys 30 (dir </> "binary") Binary [] comb commute
  check "actual RTL SAT commutation" (proofPassed binary)
  no<-runASTProof yosys 30 (dir </> "negative") Binary [] comb wrong
  check "actual RTL SAT mismatch" (not (proofPassed no) && proofStatus no=="rejected")
  completeProof<-runASTProof yosys 30 (dir </> "case") Binary [] binaryCase binaryClone
  check "actual RTL binary complete clone" (proofPassed completeProof)
  let guarded internal=base [decl VDWire 8 "h",assign "h" internal,
                              assign "o" (VEIf (e "sel") (e "a") (e "h"))]
      guardReference=guarded (VEIf (e "sel") (e "b") (e "a"))
      guardCandidate=guarded (e "a")
      stateBoundary x=base [decl VDWire 8 "h",assign "h" x,decl VDReg 8 "r",
                             seqp "r" (e "h"),assign "o" (e "r")]
      childBoundary x=base [decl VDWire 8 "h",assign "h" x,
                             VMInst (v "Child") (v "u") (Right []) [(v "I",Just (e "h"))],assign "o" (e "a")]
      effectBoundary x=base [decl VDWire 8 "h",assign "h" x,
                              VMStmt True (Vinitial (VTask (v "$display") [e "h"])),assign "o" (e "a")]
      observerGate label a b=runASTProof yosys 30 (dir </> label) Binary [] a b
  guardedProof<-observerGate "guarded-internal" guardReference guardCandidate
  check "guard may hide changed internal function" (proofPassed guardedProof)
  guardedWrong<-observerGate "guarded-wrong" guardReference (guarded (e "b"))
  check "guarded output mismatch rejected" (not (proofPassed guardedWrong))
  stateWrong<-observerGate "state-boundary" (stateBoundary (e "a")) (stateBoundary (e "b"))
  check "unchanged sequential text still observes changed cone" (not (proofPassed stateWrong))
  childWrong<-observerGate "child-boundary" (childBoundary (e "a")) (childBoundary (e "b"))
  check "unchanged child connection still observes changed cone" (not (proofPassed childWrong))
  effectWrong<-observerGate "effect-boundary" (effectBoundary (e "a")) (effectBoundary (e "b"))
  check "unchanged effect argument still observes changed cone" (not (proofPassed effectWrong))
  let rhs=op (e "a") VAdd (e "b")
      stateHelpers=base [decl VDWire 8 "h",assign "h" rhs,decl VDReg 8 "r",seqp "r" (e "h"),assign "o" (e "r")]
      stateInline=state "r" rhs
      controlled cond value=base [decl VDReg 8 "r",VMStmt False
        (Valways (VAt (VEEposedge (e "clk")) (Vifelse cond
          (VAssignA (VLId (v "r")) (c 8 0)) (VAssignA (VLId (v "r")) value)))),assign "o" (e "r")]
      secondOutput (VProgram [m] dpi comments)=VProgram [m {vm_ports=vm_ports m++[([VAOutput (v "other") (range 8)],[])],
                      vm_body=vm_body m++[assign "other" (e "h")]}] dpi comments
  check "sequential RHS alias expands strictly" (strict stateHelpers stateInline)
  stateBad<-observerGate "state-rhs-wrong" stateHelpers (state "r" (e "b"))
  check "changed sequential RHS requires actual proof" (not (proofPassed stateBad))
  check "sequential reset test cannot change" (reject (controlled (e "sel") (e "a")) (controlled (VEUnOp (v "") VNot (e "sel")) (e "a")))
  check "sequential RHS context width must match" (reject (state "r" (e "a")) (state "r" (e "sel")))
  check "removed helper cannot leave implicit sequential leaf" (reject stateHelpers (state "r" (e "h")))
  check "child boundary cannot acquire implicit net" (reject (child (e "a")) (child (e "missing")))
  let macroFailure p=case checkAST Binary [] p p of
        Left issues->any ((=="unknown-macro-scope").issueCode) issues
        _->False
      macroEvent macro=base [decl VDReg 8 "r",VMStmt False (Valways
        (VAt (VEEMacro macro (e "sel")) (VAssignA (VLId (v "r")) (e "a")))),assign "o" (e "r")]
      nativeReset=base [decl VDReg 8 "r",VMStmt False (Valways (VAt (VEEposedge (e "clk"))
        (Vifelse (op (e "sel") VEQ (VEMacro "BSV_RESET_VALUE"))
          (VAssignA (VLId (v "r")) (c 8 0)) (VAssignA (VLId (v "r")) (e "a"))))),assign "o" (e "r")]
      originalInstance=VMInst (v "OriginalReg") (v "original") (Right [])
        [(v "D_IN",Just (e "removed_helper")),(v "OTHER",Just (VEMacro "NOT_EMITTED"))]
      attached=case v "r" of VId n ident _->VId n ident (Just originalInstance)
      diagnosticOnly=base [VMDecl (VVDecl VDReg (range 8) [VVar attached]),
        VMStmt False (Vinitial (VAssign (VLId attached) (c 8 0))),seqp "r" (e "a"),assign "o" (e "r")]
      fixedParameter=case child (e "a") of
        VProgram [m] dpi comments->VProgram
          [m {vm_ports=([VAParameter (v "memName") Nothing (VEString "")],[]):vm_ports m,
              vm_body=VMInst (v "Mem") (v "storage") (Right [(v "NAME",Just (e "memName"))]) []:vm_body m}] dpi comments
  check "unknown expression macro has no proof authority" (macroFailure (base [assign "o" (VEMacro "HIDDEN_READS")]))
  check "unknown event macro has no proof authority" (macroFailure (macroEvent "HIDDEN_EVENT"))
  check "printer-defined reset edge remains supported" (strict (macroEvent "BSV_RESET_EDGE") (macroEvent "BSV_RESET_EDGE"))
  check "printer-defined reset value remains supported" (strict nativeReset nativeReset)
  check "identifier provenance is not emitted RTL" (strict diagnosticOnly diagnosticOnly)
  check "fixed child string parameter remains bound" (strict fixedParameter fixedParameter)
  let withDPI name=case fixedParameter of
        VProgram ms _ cs->VProgram ms [VDPI (v name) VDT_void []] cs
      changedParameter=case fixedParameter of
        VProgram [m] ds cs->VProgram [m {vm_ports=([VAParameter (v "memName") Nothing (VEString "changed")],[]):tail (vm_ports m)}] ds cs
  check "program DPI remains opaque and identical" (strict (withDPI "foreign_a") (withDPI "foreign_a"))
  check "changed program DPI identity is rejected" (reject (withDPI "foreign_a") (withDPI "foreign_b"))
  check "changed fixed string parameter is rejected" (reject fixedParameter changedParameter)
  otherWrong<-observerGate "other-output" (secondOutput guardReference) (secondOutput guardCandidate)
  check "every other output remains a proof root" (not (proofPassed otherWrong))
  timed<-runASTProof yosys 0 (dir </> "timeout") Binary [] comb comb
  check "zero budget fails closed" (not (proofPassed timed))
  unavailable<-runASTProof "/nonexistent/solver" 1 (dir </> "unavailable") Binary [] comb commute
  check "unavailable solver fails closed" (not (proofPassed unavailable))
  let dynamic labels def missing=base
        [decl VDReg 8 "q",decl VDWire 1 "label_alias",assign "label_alias" (e "sel"),
         VMStmt False (Valways (VAt
           (if missing then VEEOr (VEE (e "a")) (VEE (e "b")) else
             VEEOr (VEE (e "label_alias")) (VEEOr (VEE (e "a")) (VEEOr (VEE (e "b")) (VEEOr (VEE (e "sel")) (VEE (e "clk"))))))
           (Vcase (c 1 1) ([VCaseArm [x] (as "q" rhs) | (x,rhs)<-labels]++[VDefault (as "q" def)]) False False))),
         assign "o" (e "q")]
      arms=[(e "sel",e "a"),(e "clk",e "b")]
      dyn=dynamic arms (VEUnknown 8 "A") False
      aliasDyn=dynamic [(e "label_alias",e "a"),(e "clk",e "b")] (VEUnknown 8 "A") False
      reversedDyn=dynamic (reverse arms) (VEUnknown 8 "A") False
      arithmetic pol reverseOperands=dynamic
        [(e "sel",if reverseOperands then op (e "b") VAdd (e "a") else op (e "a") VAdd (e "b"))]
        (VEUnknown 8 pol) False
  check "dynamic case exact alias strict" (strict dyn aliasDyn)
  check "dynamic case priority retained" (not (strict dyn reversedDyn))
  check "dynamic case concrete default retained" (not (strict dyn (dynamic arms (VEUnknown 8 "0") False)))
  check "dynamic case label width retained" (not (strict dyn (dynamic [(e "a",e "a"),(e "clk",e "b")] (VEUnknown 8 "A") False)))
  check "dynamic case labels participate in sensitivity" (reject dyn (dynamic arms (VEUnknown 8 "A") True))
  priorityBad<-observerGate "dynamic-priority" dyn reversedDyn
  check "SAT rejects reversed overlapping dynamic labels" (proofStatus priorityBad=="rejected")
  concreteBad<-observerGate "dynamic-default" dyn (dynamic arms (VEUnknown 8 "0") False)
  check "SAT rejects changed deterministic default" (proofStatus concreteBad=="rejected")
  mapM_ (\policy->do
    good<-observerGate ("known-unspecified-"++policy) (arithmetic policy False) (arithmetic policy True)
    check ("concrete unspecified policy "++policy) (proofPassed good)) ["A","0","1"]
  mapM_ (\policy->do
    unknown<-observerGate ("unknown-unspecified-"++policy) (arithmetic policy False) (arithmetic policy True)
    check ("unknown policy not binary freedom "++policy) (not (proofPassed unknown))) ["X","Z"]
  writeFile (dir </> "positive.json") (renderProofResult binary++"\n")
  writeFile (dir </> "negative.json") (renderProofResult no++"\n")
  putStrLn "PASS: 79 native AST proof checks (including actual RTL SAT)"
