module Main where
import Control.Monad (unless)
import ECOASTProof
import ECOASTSearch
import PPrint (pp80)
import System.Directory (createDirectoryIfMissing)
import System.Environment (getArgs)
import System.FilePath ((</>))
import Verilog

v=mkVId
e=VEVar . v
c w n=VEWConst (v "") w 2 n
range w=Just (VEConst (w-1),VEConst 0)
op a x b=VEOp (v "") a x b
assign n x=VMAssign (VLId (v n)) x
base body=VProgram [VModule (v "m") [] [([VAInput (v "a") (range 8),VAInput (v "b") (range 8),VAInput (v "sel") Nothing,VAInput (v "clk") Nothing,VAOutput (v "o") (range 8)],[])] body] [] []
check label ok=unless ok (error label)

main=do
  [yosys,dir]<-getArgs
  createDirectoryIfMissing True dir
  let tautology=op (e "a") VEQ (e "a")
      falsehood=op (e "a") VNE (e "a")
      known=base [assign "o" (e "a")]
      unknown condition arm=base [assign "o" (VEIf condition (e "a") arm)]
      reference=unknown tautology (VEUnknown 8 "X")
      specialization=[SpecializeUnreachableXArm "operation-valid" "o" tautology]
      gate label domain cs ref cand=runASTProofWithContracts [] [] cs yosys 30
          (dir </> label) domain [] ref cand
  asserted<-gate "asserted" Binary [AssertCombinational "identity" tautology] known known
  check "independently prove true assertion" (proofPassed asserted && length (proofObligations asserted)==1)
  unproved<-gate "unproved" Binary [AssertCombinational "input-is-true" (e "sel")] known known
  check "raw input condition is not assumed" (not (proofPassed unproved))
  specialized<-gate "specialized" Binary specialization reference known
  check "unreachable explicit X independently specialized" (proofPassed specialized && length (proofObligations specialized)==2)
  forged<-gate "forged" Binary [SpecializeUnreachableXArm "claimed" "o" (c 1 1)]
            (unknown (e "sel") (VEUnknown 8 "X")) known
  check "true descriptor cannot hide reachable X arm" (not (proofPassed forged))
  strict<-gate "strict" StrictFourState specialization reference known
  check "binary contract never authorizes strict rewrite" (not (proofPassed strict))
  z<-gate "z" Binary specialization (unknown tautology (VETriConst (replicate 8 Vz))) known
  check "Z is not a dont-care arm" (not (proofPassed z))
  context<-gate "context" Binary specialization (unknown tautology (VEUnknown 16 "X")) known
  check "context-changing widths rejected" (not (proofPassed context))
  wrong<-gate "wrong" Binary specialization reference (base [assign "o" (e "b")])
  check "all remaining equations proved" (not (proofPassed wrong) && proofStatus wrong=="rejected")
  otherArm<-gate "other-arm" Binary [SpecializeUnreachableXArm "false-branch" "o" (c 1 1)]
      (base [assign "o" (VEIf falsehood (VETriConst (replicate 8 Vx)) (e "a"))]) known
  check "explicit X in true arm supported" (proofPassed otherArm)
  duplicate<-gate "duplicate" Binary (specialization++specialization) reference known
  check "duplicate IDs rejected" (not (proofPassed duplicate))
  missing<-gate "missing-name" Binary [AssertCombinational "missing" (e "no_such_signal")] known known
  check "missing typed binding rejected" (not (proofPassed missing))
  let decl=VMDecl (VVDecl VDReg (range 8) [VVar (v "state")])
      initState=VMStmt False (Vinitial (VAssign (VLId (v "state")) (c 8 0)))
      stateRef=base [decl,initState,assign "o" (e "state")]
  initialized<-gate "initialized" Binary [AssertCombinational "state-is-zero" (op (e "state") VEQ (c 8 0))] stateRef stateRef
  check "initialization is not a combinational invariant" (not (proofPassed initialized))
  writeASTContracts (dir </> "contracts.bin") specialization
  decoded<-readASTContracts (dir </> "contracts.bin")
  check "typed native artifact roundtrip" (decoded==specialization)
  let mapped=renameASTContracts [("a","previous_a"),("o","previous_o"),("fun","wrong_fun")]
        [SpecializeUnreachableXArm "o" "o" (VEFctCall (v "fun") [e "a"])]
  check "contract rebinding only changes signal/target names"
    (mapped==[SpecializeUnreachableXArm "o" "previous_o" (VEFctCall (v "fun") [e "previous_a"])])
  budget<-runASTProofWithContracts [] [] specialization yosys 0 (dir </> "timeout") Binary [] reference known
  check "obligation timeout cannot authorize rewrite" (not (proofPassed budget))
  let options=defaultSearchOptions {soDomain=Binary,soFeatures=["F12"],soYosys=yosys}
      runner opts path alpha contracts=runASTProofWithContracts [] [] contracts
        (soYosys opts) (soProofSeconds opts) path (soDomain opts) alpha
  searched<-optimizeAST (defaultSearchRequest options (dir </> "contract-search")
    (SearchInput known pp80) (SearchInput reference pp80))
    {searchProof=runner,searchContracts=specialization}
  check "explicit contract improves full AST search" (changedLines (srCost searched)==0 && not (null (planSteps (srPlan searched))))
  persisted<-readASTContracts (dir </> "contract-search" </> stepContractsArtifact (head (planSteps (srPlan searched))))
  check "nonempty obligations stored in saved search plan" (persisted==specialization)
  replayed<-replayASTPlanWith runner options pp80 (dir </> "contract-search/search.plan")
    (dir </> "contract-replay") reference
  check "saved plan re-proves contract-dependent transformation" (replayed==known)
  writeFile (dir </> "positive.json") (renderProofResult specialized++"\n")
  writeFile (dir </> "negative.json") (renderProofResult forged++"\n")
  putStrLn "PASS: 18 typed AST contract checks (independent assertion/selection/equivalence gates and saved-plan replay)"
