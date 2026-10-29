-- This file is deliberately outside the bluehs package module list. It adds a
-- rule using only the public SDK; compiler and SDK artifacts stay frozen.
module Main where
import Control.Exception (IOException,try)
import Control.Monad (unless)
import Data.List (sortOn)
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import Verilog
import PPrint (pp80)
import ECOASTProof (ProofDomain(..),ASTContract(..))
import ECOASTTransform (ASTCandidate(..))
import ECOASTSearch
import ECO.Replay (fileIdentity)

v=mkVId
wire n=VMDecl (VVDecl VDWire (Just (VEConst 7,VEConst 0)) [VVar (v n)])
assign n e=VMAssign (VLId (v n)) e
rhs n=VEOp defaultVId (VEVar (v "x")) VAdd (VEWConst defaultVId 8 10 n)
program items=VProgram [VModule (v "m") []
  [([VAInput (v "x") (Just (VEConst 7,VEConst 0)),
     VAOutput (v "y") (Just (VEConst 7,VEConst 0))],[])]
  ([wire "a",wire "b",wire "c"]++items++[assign "y" (VEVar (v "a"))])] [] []
old=program [assign "a" (rhs 1),assign "b" (rhs 2),assign "c" (rhs 4)]
new=program [assign "c" (rhs 4),assign "b" (rhs 2),assign "a" (rhs 3)]
expected=program [assign "a" (rhs 3),assign "b" (rhs 2),assign "c" (rhs 4)]
feature="local/top-level-assignment-order"

-- A small user rule, intentionally narrower than the bundled ordering rules.
-- Reorder current assignments by OLD destinations while preserving every RHS
-- and all non-assignment slots. No text parsing or direct-edit target is used.
generate _ enabled budget (VProgram olds _ _) current@(VProgram news ds cs)
  | feature `notElem` enabled = []
  | otherwise = take budget [ASTCandidate feature "External assignment order"
      (VProgram (map orderModule news) ds cs) [("external-order",[])] False [] []]
  where
    orderModule m=case [o|o<-olds,vm_name o==vm_name m] of
      [o]->let ranks=[(i,n)|(n,VMAssign (VLId i) _)<-zip [0::Int ..] (vm_body o)]
               rank (VMAssign (VLId i) _)=maybe (length (vm_body o)) id (lookup i ranks)
               rank _=length (vm_body o)
               assignments=[a|a@(VMAssign (VLId _) _)<-vm_body m]
           in m {vm_body=replace (vm_body m) (sortOn rank assignments)}
      _->m
    replace [] _=[]
    replace (VMAssign (VLId _) _:rest) (a:as)=a:replace rest as
    replace (item:rest) as=item:replace rest as

check label ok=unless ok (error label)
fails label action=do
  outcome<-try (action>>return ())::IO(Either IOException ())
  check label (case outcome of Left _->True;_->False)

main=do
  [out,self]<-getArgs
  createDirectoryIfMissing True out
  before<-fileIdentity self
  let provider=CandidateProvider [feature] [self] generate
      options=defaultSearchOptions {soDomain=StrictFourState,soFeatures=[feature]}
      request opts label=defaultSearchRequest opts (out</>label)
        (SearchInput old pp80) (SearchInput new pp80)
      run label p=optimizeAST (request options label) {searchProvider=p}
      proposed label candidate=CandidateProvider [feature] [self]
        (\_ _ budget _ _->take budget [candidate {acFeature=label}])
      candidate=ASTCandidate feature "external proposal" expected [] False [] []
  baseline<-optimizeAST (request (options {soFeatures=[]}) "baseline")
  check "baseline has no rule" (srProgram baseline==new && null (planSteps (srPlan baseline)))
  fails "bundled registry does not know the new rule"
    (optimizeAST (request options "unknown"))
  result<-run "external" provider
  check "separate script improves output through frozen API"
    (srProgram result==expected && changedLines (srCost result)<changedLines (srOrdinaryCost result))
  check "saved plan identifies external feature"
    (map stepFeature (planSteps (srPlan result))==[feature])
  replayed<-replayASTPlan options pp80 (out</>"external/search.plan") (out</>"replay") new
  check "replay needs no provider and reproves artifact" (replayed==expected)
  identities<-readFile (out</>"external/candidate-provider.report")
  check "rule source identity is recorded" (identities==show ([feature],[before])++"\n")
  wrong<-run "wrong-function" (proposed feature candidate {acProgram=old})
  check "smaller but incorrect old function rejected" (srProgram wrong==new && null (planSteps (srPlan wrong)))
  let wrongPorts=case expected of VProgram [m] ds cs->VProgram [m {vm_ports=[]}] ds cs
  ports<-run "wrong-ports" (proposed feature candidate {acProgram=wrongPorts})
  check "external rule cannot change fixed ports" (srProgram ports==new)
  binary<-run "binary-only" (proposed feature candidate {acBinaryOnly=True})
  check "external rule cannot relax proof domain" (srProgram binary==new)
  fails "provider cannot emit unregistered feature" (run "wrong-feature" (proposed "not-enabled" candidate))
  fails "duplicate providers do not silently shadow"
    (run "duplicate" (combineCandidateProviders [provider,provider]))
  combined<-run "combined" (combineCandidateProviders [provider,defaultCandidateProvider])
  check "external provider composes with bundled rules" (srProgram combined==expected)
  let alternativeCertificates=CandidateProvider [feature] [self] $ \_ _ budget _ _->
        take budget [candidate {acStateMap=[("missing-reference","a")]},candidate]
  certified<-run "alternative-certificates" alternativeCertificates
  check "failed certificate cannot hide identical AST with valid certificate"
    (srProgram certified==expected)
  let alternativeObligations=CandidateProvider [feature] [self] $ \_ _ budget _ _->
        take budget [candidate {acContracts=[AssertCombinational "invalid-obligation" (VEWConst defaultVId 1 10 0)]},candidate]
  discharged<-run "alternative-obligations" alternativeObligations
  check "failed obligation cannot hide identical AST without that obligation"
    (srProgram discharged==expected)
  after<-fileIdentity self
  check "rule script unchanged" (before==after)
  putStrLn ("ordinary_changed_lines="++show (changedLines (srOrdinaryCost result)))
  putStrLn ("external_changed_lines="++show (changedLines (srCost result)))
  putStrLn "PASS: 15 external rule extension and proof boundary checks"
