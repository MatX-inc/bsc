module Main where
import Control.Monad(unless)
import System.Environment(getArgs)
import System.Directory(createDirectoryIfMissing)
import System.FilePath((</>))
import PPrint(pp80)
import Verilog
import ECOASTProof(ProofDomain(..))
import ECOASTSearch
import ECOASTTransform(ASTCandidate(..))
import ECOASTRefinements
v=mkVId
r=Just(VEConst 7,VEConst 0)
d n=VMDecl(VVDecl VDWire r[VVar(v n)])
a n e=VMAssign(VLId(v n))e
e=VEVar . v
p body=VProgram[VModule(v "Demo")[][([VAInput(v "x")r,VAInput(v "y")r,VAOutput(v "out")r],[])]([d "first",d "second"]++body)][][]
old=p[a "first"(e "x"),a "second"(e "y"),a "out"(e "first")]
current=p[a "second"(e "y"),a "first"(e "x"),a "out"(e "first")]
check s b=unless b(fail s)>>putStrLn("PASS "++s)
main=do
 [ys,out]<-getArgs
 createDirectoryIfMissing True out
 let flood=CandidateProvider["Flood"][]$ \_ _ budget _ now->replicate budget(ASTCandidate "Flood" "ineligible no-op" now[]False[][])
     providers=[flood,defaultCandidateProvider]
     options=defaultSearchOptions{soDomain=StrictFourState,soFeatures=["Flood","F04"],soCandidates=2,soYosys=ys}
     sequential=combineCandidateProviders providers
     balanced=balancedCandidateProviders providers
     candidates provider=cpGenerate provider[](soFeatures options)(soCandidates options)old current
 check "priority combiner semantics remain unchanged"(all((=="Flood").acFeature)(candidates sequential))
 check "saturated first family leaves a slot for ordinary layout"(map acFeature(candidates balanced)==["Flood","F04"])
 result<-optimizeAST (defaultSearchRequest options (out</>"search")
   (SearchInput old pp80) (SearchInput current pp80)) {searchProvider=balanced}
 check "ordinary layout still improves under saturated earlier family"(changedLines(srCost result)==0&&map stepFeature(planSteps(srPlan result))==["F04"])
 replayed<-replayASTPlan options pp80(out</>"search/search.plan")(out</>"replay")current
 check "balanced search saved replay independently preserves behavior"(replayed==old)
 check "zero budget emits no candidates"(null(cpGenerate balanced[](soFeatures options)0 old current))
 let configured=enableStandardRefinements options{soFeatures=["F04","F22","F22Inactive"]}
 check "standard preset removes inherited unused-helper policy"(all(`notElem`soFeatures configured)["F22","F22Inactive"])
 check "live aliases and helper reuse share standard registration"(all(`elem`soFeatures configured)["LiveAliases","F26LiveHelpers"])
 check "standard registration has no duplicate feature ownership"(length(cpFeatures standardRefinementProvider)==length(unique(cpFeatures standardRefinementProvider)))
 where unique[]= [];unique(x:xs)=x:unique(filter(/=x)xs)
