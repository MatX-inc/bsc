module Main where
import ECOASTTransform hiding (generateASTCandidates)
import qualified ECOASTTransform as T
import ECOASTLexical
import qualified ECOASTProof as P
import Verilog
import Control.Monad (unless)
import Data.Either (isRight)
import Data.List (find)
import System.Environment (getArgs)
import System.FilePath ((</>))

v=mkVId
e=VEVar . v
lit n x=VEWConst defaultVId n 16 x
range n=if n==1 then Nothing else Just (VEConst (n-1),VEConst 0)
wire n x=VMDecl (VVDecl VDWire (range n) [VVar (v x)])
reg n x=VMDecl (VVDecl VDReg (range n) [VVar (v x)])
assign n x=VMAssign (VLId (v n)) x
op a o b=VEOp defaultVId a o b
program body=VProgram [VModule (v "M") []
  [([VAInput (v "a") (range 8),VAInput (v "b") (range 8),VAInput (v "h") (range 8),
     VAOutput (v "o") (range 8)],[])] body] [] []
items (VProgram [m] _ _)=vm_body m
items _=[]
check label okay=unless okay (error label)
emittable p=isRight (lexicallyEmittable p >> declaredBeforeUse p)
gated cs=if all (emittable . acProgram) cs then cs else error "generated candidate is not emittable"
generateASTCandidates fs n a b=gated (T.generateASTCandidates fs n a b)
get predicate xs=case find predicate xs of Just x->x; _->error "required typed proposal absent"
main=do
  [yosys,out]<-getArgs
  let condition=op (e "b") VUGT (e "h")
      tailValue=VEIf (op (e "a") VUGE (e "h")) (e "h") (e "a")
      oldValue=VEIf (op (e "a") VULT (e "b")) (e "b") (e "tail")
      old=program [wire 8 "tail",assign "tail" tailValue,assign "o" oldValue]
      new=program [wire 8 "tail",wire 8 "lower",assign "tail" tailValue,
                   assign "lower" (VEIf condition (e "h") (e "b")),
                   assign "o" (VEIf (op (e "a") VULT (e "lower")) (e "lower") (e "tail"))]
      proposals=generateASTCandidates ["F11"] 128 old new
      expected=assign "o" (VEIf condition (e "h") oldValue)
      guarded=get (elem expected . items . acProgram) proposals
  positive<-P.runASTProof yosys 15 (out</>"guard-positive") P.Binary [] new (acProgram guarded)
  check "source-neutral guarded old cone proof" (P.proofPassed positive)
  negative<-P.runASTProof yosys 15 (out</>"old-negative") P.Binary [] new old
  check "original old cone is not equivalent" (not (P.proofPassed negative))
  let wrong=program [wire 8 "tail",assign "tail" tailValue,
                    assign "o" (VEIf condition (e "b") oldValue)]
  wrongProof<-P.runASTProof yosys 15 (out</>"wrong-arm-negative") P.Binary [] new wrong
  check "wrong exceptional arm rejected" (not (P.proofPassed wrongProof))
  let guardedOutputs=any (any ((=="guarded-old-cone").fst).acOperations)
      effect=VMStmt False (Vinitial (VTask (v "$display") [VEString "effect"]))
      child=VMInst (v "Child") (v "same") [] [(v "x",Just (e "a"))]
      append x (VProgram [m] ds cs)=VProgram [m {vm_body=vm_body m++[x]}] ds cs
  check "effectful whole cones excluded" (not (guardedOutputs (generateASTCandidates ["F11"] 128 (append effect old) (append effect new))))
  check "child whole cones excluded" (not (guardedOutputs (generateASTCandidates ["F11"] 128 (append child old) (append child new))))
  let sign=VESelect1 (e "a") (VEConst 7)
      wide=VEConcat [sign,e "a"]
      current=program [wire 9 "wide",assign "wide" wide,
                       assign "o" (VEIf (op sign VLAnd (op (e "wide") VULT (lit 9 0x181))) (lit 8 1) (lit 8 0))]
      reduced=assign "o" (VEIf (op (e "a") VEQ (lit 8 128)) (lit 8 1) (lit 8 0))
      rangeCandidate=get (elem reduced.items.acProgram) (generateASTCandidates ["F11"] 128 current current)
  rp<-P.runASTProof yosys 15 (out</>"range-positive") P.Binary [] current (acProgram rangeCandidate)
  check "exact range simplification proof" (P.proofPassed rp)
  let offByOne=program [wire 9 "wide",assign "wide" wide,
                       assign "o" (VEIf (op (e "a") VEQ (lit 8 127)) (lit 8 1) (lit 8 0))]
  rn<-P.runASTProof yosys 15 (out</>"range-negative") P.Binary [] current offByOne
  check "off by one interval rejected" (not (P.proofPassed rn))
  check "range and guarded forms binary only" (acBinaryOnly guarded && acBinaryOnly rangeCandidate)
  let oldShared=program [wire 8 "tail",wire 8 "result",assign "tail" tailValue,
                         assign "result" oldValue,assign "o" (e "result")]
      newShared=program [wire 8 "tail",wire 8 "lower",wire 8 "result",assign "tail" tailValue,
                         assign "lower" (VEIf condition (e "h") (e "b")),
                         assign "result" (VEIf (op (e "a") VULT (e "lower")) (e "lower") (e "tail")),
                         assign "o" (e "result")]
      guardedShared=get (elem (assign "o" (VEIf condition (e "h") (e "result"))) . items . acProgram)
                       (generateASTCandidates ["F11"] 128 oldShared newShared)
  sharedProof<-P.runASTProof yosys 15 (out</>"guard-shared-internal-positive") P.Binary [] newShared (acProgram guardedShared)
  check "guarded internal value may differ when unobservable" (P.proofPassed sharedProof)
  sharedNegative<-P.runASTProof yosys 15 (out</>"guard-shared-internal-old-negative") P.Binary [] newShared oldShared
  check "unguarded old internal remains rejected" (not (P.proofPassed sharedNegative))
  let dcOld=program [assign "o" (e "a")]
      dcNew=program [wire 1 "enable",assign "enable" (lit 1 1),
                    assign "o" (VEIf (e "enable") (e "a") (VEUnknown 8 "X"))]
  -- A tautology proof is a request to prove, never an assumption: a proved
  -- proposition about NEW does not make the unreachable X arm a dont-care.
  tautology<-P.proveTautology [] yosys 15 (out</>"tautology-positive") dcNew (e "enable")
  check "typed proposition over NEW proved as a tautology" (P.proofPassed tautology)
  falsehood<-P.proveTautology [] yosys 15 (out</>"tautology-negative") dcNew (op (e "enable") VEQ (lit 1 0))
  check "false proposition rejected" (P.proofStatus falsehood=="rejected")
  illTyped<-P.proveTautology [] yosys 15 (out</>"tautology-ill-typed") dcNew (e "a")
  check "wide or unbound proposition is not a proposition" (not (P.proofPassed illTyped) && null (P.proofSolverLog illTyped))
  specializedX<-P.runASTProof yosys 15 (out</>"x-arm-not-dont-care") P.Binary [] dcNew dcOld
  check "proved tautology does not waive equivalence over the X arm" (not (P.proofPassed specializedX))
  let stateDecl=VMDecl (VVDecl VDReg (range 8) [VVar (v "q")])
      seqBlock rhs=VMStmt False (Valways (VAt (VEEposedge (e "h"))
                     (Vifelse (op (e "b") VEQ (lit 8 0))
                       (VAssignA (VLId (v "q")) (lit 8 0))
                       (VAssignA (VLId (v "q")) rhs))))
      stateOld=program [stateDecl,seqBlock (e "a"),assign "o" (e "q")]
      stateNew=program [stateDecl,wire 8 "d",assign "d" (op (e "a") VAdd (lit 8 1)),
                       seqBlock (e "d"),assign "o" (e "q")]
  stateOldProof<-P.runASTProof yosys 15 (out</>"state-rhs-old-negative") P.Binary [] stateNew stateOld
  check "old next-state expression remains inequivalent" (not (P.proofPassed stateOldProof))
  let wrongState=program [stateDecl,seqBlock (op (e "a") VAdd (lit 8 2)),assign "o" (e "q")]
  wrongStateProof<-P.runASTProof yosys 15 (out</>"state-rhs-wrong-negative") P.Binary [] stateNew wrongState
  check "wrong next-state constant rejected" (not (P.proofPassed wrongStateProof))
  let historicalSum=program [assign "o" (op (e "a") VAdd (e "b"))]
      extractedSum=program [wire 8 "sum",assign "sum" (op (e "a") VAdd (e "b")),assign "o" (e "sum")]
      recoveredSum=get (elem (assign "o" (op (e "a") VAdd (e "b"))).items.acProgram)
        (generateASTCandidates ["F08"] 128 historicalSum extractedSum)
  sumProof<-P.runASTProof yosys 15 (out</>"historical-helper-recovery-positive") P.StrictFourState [] extractedSum (acProgram recoveredSum)
  check "historical helper recovery independently preserves strict value" (P.proofPassed sumProof)
  let wrongSum=program [assign "o" (op (e "a") VSub (e "b"))]
  wrongSumProof<-P.runASTProof yosys 15 (out</>"historical-helper-wrong-operator-negative") P.Binary [] extractedSum wrongSum
  check "historical helper operator mismatch rejected independently" (not (P.proofPassed wrongSumProof))
  let originalGuard=op (e "b") VEQ (lit 8 0)
      changedShellGuard=op originalGuard VLOr (op (e "h") VEQ (lit 8 3))
      shellOld=program [assign "o" (VEIf originalGuard (lit 8 0) (e "a"))]
      shellNew=program [wire 8 "branch",assign "branch" (VEIf changedShellGuard (lit 8 0) (e "a")),assign "o" (e "branch")]
      shellCandidate=get (elem (assign "o" (VEIf changedShellGuard (lit 8 0) (e "a"))).items.acProgram)
        (generateASTCandidates ["F08"] 128 shellOld shellNew)
  shellProof<-P.runASTProof yosys 15 (out</>"conditional-shell-strict-positive") P.StrictFourState [] shellNew (acProgram shellCandidate)
  check "historical conditional shell preserves new guard strictly" (P.proofPassed shellProof)
  shellWrong<-P.runASTProof yosys 15 (out</>"conditional-shell-old-guard-negative") P.Binary [] shellNew shellOld
  check "conditional shell cannot restore incorrect old guard" (not (P.proofPassed shellWrong))
  let relValue relation=VEConcat [lit 7 0,op (e "a") relation (e "b")]
      relOld=program [assign "o" (relValue VULT)]
      relNew=program [wire 1 "cmp",assign "cmp" (op (e "a") VULE (e "b")),assign "o" (VEConcat [lit 7 0,e "cmp"])]
      relCandidate=get (elem (assign "o" (relValue VULE)).items.acProgram) (generateASTCandidates ["F08"] 128 relOld relNew)
  relProof<-P.runASTProof yosys 15 (out</>"comparison-shell-strict-positive") P.StrictFourState [] relNew (acProgram relCandidate)
  check "comparison shell preserves new relation strictly" (P.proofPassed relProof)
  relWrong<-P.runASTProof yosys 15 (out</>"comparison-shell-old-operator-negative") P.Binary [] relNew relOld
  check "comparison shell cannot overwrite new comparison operator" (not (P.proofPassed relWrong))
  let aliasBody width spelling=[VMDecl (VVDecl VDReg (range width) [VVar (v "pointer")]),
        wire width spelling,assign spelling (e "pointer"),assign "o" (VEConcat [lit (8-width) 0,e spelling])]
      aliasOld=program (aliasBody 2 "oldAlias")
      aliasNew=program (aliasBody 3 "newAlias")
      aliasCandidate=get (const True) (generateASTCandidates ["F01"] 128 aliasOld aliasNew)
  aliasProof<-P.runASTProof yosys 15 (out</>"width-grown-alias-strict-positive") P.StrictFourState [] aliasNew (acProgram aliasCandidate)
  check "width-grown alias strictly preserves new-width state and reads" (P.proofPassed aliasProof)
  let truncatedAlias=program [VMDecl (VVDecl VDReg (range 3) [VVar (v "pointer")]),wire 2 "oldAlias",
        assign "oldAlias" (e "pointer"),assign "o" (VEConcat [lit 6 0,e "oldAlias"])]
  aliasWrong<-P.runASTProof yosys 15 (out</>"width-grown-alias-truncation-negative") P.Binary [] aliasNew truncatedAlias
  check "width alias cannot restore obsolete old declaration width" (not (P.proofPassed aliasWrong))
  let selector=VESelect1 (e "h") (VEConst 0)
      caseBody arms=VMStmt False (Valways (VAt (VEEOr (VEE selector) (VEEOr (VEE (e "a")) (VEE (e "b")))) (Vcase selector arms False False)))
      ca bit rhs=VCaseArm [lit 1 bit] (VAssign (VLId (v "q")) rhs)
      cd rhs=VDefault (VAssign (VLId (v "q")) rhs)
      oldCase=program [stateDecl,caseBody [ca 0 (e "a"),cd (e "b")],assign "o" (e "q")]
      newCase=program [stateDecl,caseBody [ca 0 (e "a"),ca 1 (e "b")],assign "o" (e "q")]
      defaultRecovered=get (any ((=="binary-case-default").fst).acOperations)
                         (generateASTCandidates ["F09"] 128 oldCase newCase)
  defaultProof<-P.runASTProof yosys 15 (out</>"historical-binary-default-positive") P.Binary [] newCase (acProgram defaultRecovered)
  writeFile (out</>"historical-binary-default-positive.json") (P.renderProofResult defaultProof)
  check "historical default complete binary proof" (P.proofPassed defaultProof)
  strictDefault<-P.runASTProof yosys 15 (out</>"historical-default-strict-negative") P.StrictFourState [] newCase (acProgram defaultRecovered)
  check "historical default not silently four-state" (not (P.proofPassed strictDefault))
  let wrongDefault=program [stateDecl,caseBody [ca 0 (e "a"),cd (e "a")],assign "o" (e "q")]
  wrongDefaultProof<-P.runASTProof yosys 15 (out</>"historical-default-value-negative") P.Binary [] newCase wrongDefault
  check "wrong historical default value rejected" (not (P.proofPassed wrongDefaultProof))
  let left=op (e "a") VAdd (lit 8 1)
      right=op (e "b") VAdd (lit 8 1)
      helpers=[wire 8 "left",wire 8 "right",assign "left" left,assign "right" right]
      oldTemplate=program (helpers++[assign "o" (VEIf selector (e "left") (e "right"))])
      newTemplate=program (helpers++[assign "o" right])
      template=get (elem (assign "o" (VEIf selector (e "right") (e "right"))).items.acProgram)
        (generateASTCandidates ["F10"] 128 oldTemplate newTemplate)
  templateProof<-P.runASTProof yosys 15 (out</>"historical-helper-template-positive") P.StrictFourState [] newTemplate (acProgram template)
  check "historical helper reuse preserves complete four-state cones" (P.proofPassed templateProof)
  let wrongHelper=program (helpers++[assign "o" (VEIf selector (e "left") (e "left"))])
  wrongHelperProof<-P.runASTProof yosys 15 (out</>"historical-helper-template-negative") P.Binary [] newTemplate wrongHelper
  check "incorrect historical helper rejected independently" (not (P.proofPassed wrongHelperProof))
  let eqGuard=op (e "a") VEQ (e "b")
      neGuard=op (op (e "a") VULT (e "b")) VLOr (op (e "a") VUGT (e "b"))
      resultA=op (e "a") VXor (e "h")
      resultB=op (e "b") VAdd (e "h")
      conditionalOld=program [assign "o" (VEIf eqGuard resultA resultB)]
      conditionalNew=program [assign "o" (VEIf neGuard resultB resultA)]
      recovered=get (any ((=="historical-conditional").fst).acOperations)
        (generateASTCandidates ["F11"] 128 conditionalOld conditionalNew)
  conditionalProof<-P.runASTProof yosys 15 (out</>"historical-conditional-positive") P.Binary [] conditionalNew (acProgram recovered)
  check "historical comparison complement complete binary proof" (P.proofPassed conditionalProof)
  let changedBranch=program [assign "o" (VEIf neGuard resultB (e "h"))]
  changedProof<-P.runASTProof yosys 15 (out</>"historical-conditional-functional-negative") P.Binary [] changedBranch (acProgram recovered)
  check "historical conditional cannot overwrite real branch change" (not (P.proofPassed changedProof))
  let group label defs=VMComment [label] (VMGroup False [defs])
      declarations=[wire 8 "x",wire 8 "y"]
      first rhs=group "current first" [assign "x" rhs]
      second=group "current second" [assign "y" (e "x")]
      fresh=group "new group" [assign "extra" (e "b")]
      nextState rhs=VMStmt False (Valways (VAt (VEEposedge (e "h")) (VAssignA (VLId (v "q")) rhs)))
      oldGroups=program (declarations++[first (e "a"),second,assign "o" (e "y")])
      changed=op (e "b") VXor (e "h")
      newPrefix=declarations++[wire 8 "extra",stateDecl]
      currentGroups=program (newPrefix++[second,fresh,first changed,assign "o" (op (e "y") VXor (e "q")),nextState (e "a")])
      orderedGroups=get (any ((=="assignment-group-order").fst).acOperations)
        (generateASTCandidates ["F04"] 128 oldGroups currentGroups)
  groupProof<-P.runASTProof yosys 15 (out</>"assignment-group-order-positive") P.StrictFourState [] currentGroups (acProgram orderedGroups)
  check "group ordering proves complete changed dependent cones and added state" (P.proofPassed groupProof)
  let wrongGroups=program (newPrefix++[first (e "a"),fresh,second,assign "o" (op (e "y") VXor (e "q")),nextState (e "a")])
  groupWrong<-P.runASTProof yosys 15 (out</>"assignment-group-old-body-negative") P.Binary [] currentGroups wrongGroups
  check "ordering proof cannot restore old changed function" (not (P.proofPassed groupWrong))
  let wrongStateGroups=program (newPrefix++[first changed,fresh,second,assign "o" (op (e "y") VXor (e "q")),nextState (e "b")])
  groupStateWrong<-P.runASTProof yosys 15 (out</>"assignment-group-new-state-negative") P.Binary [] currentGroups wrongStateGroups
  check "ordering proof preserves newly added state behavior" (not (P.proofPassed groupStateWrong))
  let late n = VMStmt False (Valways (VAt
          (VEEOr (VEE (e "h")) (VEEOr (VEE (e "a")) (VEE (e "b"))))
          (Vcase (VESelect1 (e "h") (VEConst 0)) [VCaseArm [lit 1 0] (VAssign (VLId (v n)) (e "a")),
                          VCaseArm [lit 1 1] (VAssign (VLId (v n)) (e "b"))] False False)))
      oldLate=program [reg 8 "prior",wire 8 "extra",assign "extra" (e "a"),late "prior",assign "o" (op (e "prior") VXor (e "extra"))]
      newLate=program [reg 8 "current",wire 8 "extra",assign "extra" (e "b"),late "current",assign "o" (op (e "current") VXor (e "extra"))]
      namedLate=get ((=="F02").acFeature) (generateASTCandidates ["F02"] 128 oldLate newLate)
  lateProof<-P.runASTProof yosys 15 (out</>"late-process-identity-positive") P.StrictFourState (acStateMap namedLate) newLate (acProgram namedLate)
  check "late process renaming preserves four-state no-default retention and other changed logic" (P.proofPassed lateProof)
  lateWrong<-P.runASTProof yosys 15 (out</>"late-process-other-output-negative") P.Binary (acStateMap namedLate) newLate oldLate
  check "late process correspondence does not authorize restoring other old logic" (not (P.proofPassed lateWrong))
  lateMissing<-P.runASTProof yosys 15 (out</>"late-process-missing-relation-negative") P.StrictFourState [] newLate (acProgram namedLate)
  check "late process retention identity needs explicit relation" (not (P.proofPassed lateMissing))
  let groupState state operand permission=VMRegGroup (v state) "RegUN" []
        (VMGroup False [[reg 8 state],[wire 8 operand,wire 1 permission]])
      stage state operand permission rhs=
        [groupState state operand permission,assign operand rhs,assign permission (lit 1 1),
         VMStmt False (Valways (VAt (VEEposedge (e "h"))
           (Vif (e permission) (VAssignA (VLId (v state)) (e operand))))),
         VMStmt False (Vinitial (VAssign (VLId (v state)) (lit 8 0)))]
      roleOld=program (stage "retained" "operand_before" "permission_before" (e "a")++[assign "o" (e "retained")])
      roleNew=program (stage "retained" "operand_before" "permission_before" (e "a")++
                       stage "inserted" "operand_after" "permission_after" (e "retained")++[assign "o" (e "inserted")])
      role=get (const True) (generateASTCandidates ["F15"] 8 roleOld roleNew)
      replaceOutput (VProgram [m] ds cs)=VProgram [m {vm_body=[case item of
          VMAssign (VLId ident) _ | ident==v "o" -> assign "o" (e "a")
          _ -> item | item<-vm_body m]}] ds cs
  roleProof<-P.runASTProof yosys 15 (out</>"state-role-identity-positive") P.StrictFourState (acStateMap role) roleNew (acProgram role)
  check "state identity preserves added pipeline transitions reset init and controls" (P.proofPassed roleProof)
  roleMissing<-P.runASTProof yosys 15 (out</>"state-role-missing-relation-negative") P.StrictFourState [] roleNew (acProgram role)
  check "state identity requires explicit bijective relation" (not (P.proofPassed roleMissing))
  roleWrong<-P.runASTProof yosys 15 (out</>"state-role-wrong-output-negative") P.StrictFourState (acStateMap role) roleNew (replaceOutput (acProgram role))
  check "state identity cannot remove newly added pipeline behavior" (not (P.proofPassed roleWrong))
  let repValue=op (e "a") VSub (e "b")
      repDecls=[wire 8 "historical",wire 8 "current_root",wire 8 "fanout"]
      repOld=program (repDecls++[assign "historical" (op (e "a") VAdd (e "b")),assign "current_root" (e "historical"),assign "fanout" (e "historical"),assign "o" (e "fanout")])
      repNew=program (repDecls++[assign "historical" (VEIf (VESelect1 (e "h") (VEConst 0)) (e "current_root") (e "current_root")),assign "current_root" repValue,assign "fanout" (e "current_root"),assign "o" (e "fanout")])
      representative=get (any ((=="historical-alias-representative").fst).acOperations) (generateASTCandidates ["F08"] 16 repOld repNew)
      repWrong (VProgram [m] ds cs)=VProgram [m {vm_body=[case x of VMAssign (VLId ident) _ | ident==v "historical" ->assign "historical" (e "a");_->x |x<-vm_body m]}] ds cs
  repProof<-P.runASTProof yosys 15 (out</>"alias-representative-strict-positive") P.StrictFourState [] repNew (acProgram representative)
  check "alias representative preserves current function and unknown conditional behavior" (P.proofPassed repProof)
  repOldProof<-P.runASTProof yosys 15 (out</>"alias-representative-old-function-negative") P.Binary [] repNew repOld
  check "alias representative cannot overwrite real old-to-new function change" (not (P.proofPassed repOldProof))
  repWrongProof<-P.runASTProof yosys 15 (out</>"alias-representative-wrong-function-negative") P.Binary [] repNew (repWrong (acProgram representative))
  check "alias representative mutation rejected by independent gate" (not (P.proofPassed repWrongProof))
  let grouped width names=VMDecl(VVDecl VDWire (range width)[VVar(v n)|n<-names])
      declOld=program[grouped 8["x","y"],assign "x"(e "a"),assign "y"(e "x"),assign "o"(e "y")]
      declNew=program[grouped 8["y","added","x"],assign "x"(e "b"),assign "y"(e "x"),assign "added"(e "a"),assign "o"(op(e "y")VXor(e "added"))]
      declarationCandidate=get(any((=="historical-declaration-partitions").fst).acOperations)(generateASTCandidates["F04"]16 declOld declNew)
  declProof<-P.runASTProof yosys 15(out</>"declaration-partitions-positive")P.StrictFourState[]declNew(acProgram declarationCandidate)
  check "declaration groups prove all current values and new variables" (P.proofPassed declProof)
  let badWidth=program[grouped 7["x"],grouped 8["y","added"],assign "x"(e "b"),assign "y"(e "x"),assign "added"(e "a"),assign "o"(op(e "y")VXor(e "added"))]
  badWidthProof<-P.runASTProof yosys 15(out</>"declaration-width-negative")P.Binary[]declNew badWidth
  check "declaration restoration cannot change current widths" (not(P.proofPassed badWidthProof))
  oldValueProof<-P.runASTProof yosys 15(out</>"declaration-old-value-negative")P.Binary[]declNew declOld
  check "declaration restoration cannot erase added logic" (not(P.proofPassed oldValueProof))
  let declarationRegion=get(any((=="historical-declaration-regions").fst).acOperations)(generateASTCandidates["F04"]16 declOld declNew)
  regionProof<-P.runASTProof yosys 15(out</>"declaration-region-positive")P.StrictFourState[]declNew(acProgram declarationRegion)
  check "declaration region placement proves all current values" (P.proofPassed regionProof)
  putStrLn "PASS native AST transform proof: 47 directed gates"
