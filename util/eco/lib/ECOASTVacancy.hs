-- Optional external rule: retain current state semantics under vacant OLD names.
module ECOASTVacancy (vacancyProvider) where
import ECOASTIdentity
import ECOASTSearch (CandidateProvider(..))
import ECOASTTransform (ASTCandidate(..))
import Verilog

vacancyProvider :: CandidateProvider
vacancyProvider = CandidateProvider ["F21"] [] $ \features budget (VProgram olds _ _) (VProgram currents ds cs) ->
  if "F21" `notElem` features then [] else take budget
    [ASTCandidate "F21" "Reclaim a compatible vacant historical state group"
      (VProgram (take i currents ++ [srpModule proposal] ++ drop (i+1) currents) ds cs)
      [("vacant-state-alternative",[("sourceState",srpSourceState proposal),
        ("historicalState",srpHistoricalState proposal),("accounting",show (vacancyAudit budget old current))])]
      False [(o,n) | (n,o) <- srpForwardNames proposal]
    | (i,current) <- zip [0..] currents, old <- olds, vm_name old == vm_name current,
      proposal <- stateRoleAlternatives budget old current]
