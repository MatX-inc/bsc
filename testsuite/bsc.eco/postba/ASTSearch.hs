module Main where
import Control.Exception (IOException,try)
import Control.Monad (unless)
import qualified Data.ByteString as BS
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import Verilog
import PPrint (pp80)
import ECOASTProof (ProofDomain(..))
import ECOASTSearch
import ECOStructurePolicy (StructurePolicy(..), applyStructurePolicy, structurePolicyLabel)

v = mkVId
arg = [([VAInput (v "x") (Just (VEConst 7,VEConst 0)),
         VAOutput (v "y") (Just (VEConst 7,VEConst 0)),
         VAOutput (v "z") (Just (VEConst 7,VEConst 0))],[])]
wire n = VMDecl (VVDecl VDWire (Just (VEConst 7,VEConst 0)) [VVar (v n)])
rhs n = VEOp defaultVId (VEVar (v "x")) VAdd (VEWConst defaultVId 8 10 n)
assign n expr = VMAssign (VLId (v n)) expr
program first second third = VProgram [VModule (v "m") [] arg
    [wire "a",wire "b",wire "c",first,second,third,assign "y" (VEVar (v "a")),assign "z" (VEVar (v "c"))]] [] []
old = program (assign "a" (rhs 1)) (assign "b" (rhs 2)) (assign "c" (rhs 4))
new = program (assign "c" (rhs 4)) (assign "b" (rhs 2)) (assign "a" (rhs 3))
check name ok = unless ok (error name)
fails label action = do
    result <- try (action >> return ()) :: IO (Either IOException ())
    check label (case result of Left _ -> True; _ -> False)

main = do
    [out] <- getArgs
    createDirectoryIfMissing True out
    let opts = defaultSearchOptions {soDomain=StrictFourState,soFeatures=["F04","F05"]}
        request options label a b = defaultSearchRequest options (out </> label)
            (SearchInput a pp80) (SearchInput b pp80)
    result <- optimizeAST (request opts "search" old new)
    check "proved textual improvement" (changedLines (srCost result) < changedLines (srOrdinaryCost result))
    replayed <- replayASTPlan opts pp80 (out </> "search/search.plan") (out </> "replay") new
    check "replay exact AST output" (replayed == srProgram result && pp80 replayed == pp80 (srProgram result))
    let artifact = out </> "search" </> stepArtifact (head (planSteps (srPlan result)))
    originalBytes <- BS.readFile artifact
    BS.appendFile artifact (BS.pack [0])
    fails "reject changed candidate" (replayASTPlan opts pp80 (out </> "search/search.plan") (out </> "tamper") new)
    BS.writeFile artifact originalBytes
    let obligations = out </> "search" </> stepContractsArtifact (head (planSteps (srPlan result)))
    BS.appendFile obligations (BS.pack [0])
    fails "reject changed proof obligations" (replayASTPlan opts pp80 (out </> "search/search.plan") (out </> "contract-tamper") new)
    fails "reject changed proof domain" (replayASTPlan (opts {soDomain=Binary}) pp80 (out </> "search/search.plan") (out </> "domain") new)
    let wrongPorts = case new of VProgram [m] d c -> VProgram [m {vm_ports=[]}] d c
    fails "fixed module ports" (optimizeAST (request opts "ports" old wrongPorts))
    rollback <- optimizeAST (request (opts {soFeatures=["F01"]}) "rollback" old old) {searchRenames=[("a","fresh")]}
    check "worsening name plan rolls back" (srRolledBack rollback && changedLines (srCost rollback)==0 && null (planSteps (srPlan rollback)))
    restored <- replayASTPlan (opts {soFeatures=["F01"]}) pp80 (out </> "rollback/search.plan") (out </> "rollback-replay") old
    check "rollback plan replay" (restored == old)
    fails "nonpositive search budget" (optimizeAST (request (opts {soRounds=0}) "budget" old new))
    let custom=opts {soFeatures=["F01","F09","F10","F11"]}
        ordinary=applyStructurePolicy OrdinaryStructure custom
    check "preserve respects configured exclusions" (applyStructurePolicy PreserveStructure opts==opts)
    check "ordinary excludes only case and equal-arm templates"
      (ordinary==custom {soFeatures=["F01","F11"]} &&
       structurePolicyLabel ordinary=="ordinary-case-and-equal-arm-conditional-structure")
    fails "saved plan cannot cross structure policies"
      (replayASTPlan (opts {soFeatures=soFeatures opts++["F09","F10"]}) pp80
        (out </> "search/search.plan") (out </> "structure-policy") new)
    let grouped label item=VMComment [label] (VMGroup False [[item]])
        groupedOld=program (grouped "first" (assign "a" (rhs 1)))
                           (grouped "second" (assign "b" (rhs 2)))
                           (grouped "third" (assign "c" (rhs 4)))
        groupedNew0=program (grouped "second" (assign "b" (rhs 3)))
                            (grouped "first" (assign "a" (rhs 1)))
                            (grouped "third" (assign "c" (rhs 4)))
        groupedNew=case groupedNew0 of
          VProgram [m] ds cs->VProgram [m {vm_body=
            VMDecl (VVDecl VDReg (Just (VEConst 7,VEConst 0)) [VVar (v "newState")]):
            vm_body m++[VMStmt False (Valways (VAt (VEEposedge (VESelect1 (VEVar (v "x")) (VEConst 0)))
                        (VAssignA (VLId (v "newState")) (VEVar (v "x")))))]}] ds cs
          _->error "group fixture"
        groupOpts=opts {soFeatures=["F04"]}
    groupResult<-optimizeAST (request groupOpts "group-search" groupedOld groupedNew)
    check "partial group ordering accepted with added state"
      (changedLines (srCost groupResult)<changedLines (srOrdinaryCost groupResult) &&
       any (any ((=="assignment-group-order").fst).stepOperations) (planSteps (srPlan groupResult)))
    groupReplayed<-replayASTPlan groupOpts pp80 (out </> "group-search/search.plan")
                    (out </> "group-replay") groupedNew
    check "partial group ordering saved plan reproves" (groupReplayed==srProgram groupResult)
    putStrLn "PASS: 14 AST search/replay checks"
