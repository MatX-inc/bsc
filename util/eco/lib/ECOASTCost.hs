-- External objective policy, separate from candidate emission and equivalence.
module ECOASTCost
  ( LineCost(..),changedLines,lineCost
  , CostProvider(..),wholeFileCostProvider,CostContext,costBaselineIdentity,CostObservation(..)
  , CostEnvelope(..),prepareCost,scoreCost,sealCost,verifyCostEnvelope
  ) where
import Control.Monad(unless,forM,forM_)
import qualified Data.ByteString as B
import Data.List(nub)
import System.Directory
import System.Exit(ExitCode(..))
import System.FilePath
import System.Process(readProcessWithExitCode)
import Text.Read(readMaybe)
import BinData(encode)
import GenABin()
import Verilog(VProgram)
import ECO.Replay(FileIdentity(..),fileIdentity)

data LineCost=LineCost{addedLines::Int,deletedLines::Int}deriving(Eq,Ord,Read,Show)
changedLines :: LineCost -> Int
changedLines c=addedLines c+deletedLines c
need b s=unless b(ioError(userError("ECO objective: "++s)))
lineCost :: FilePath -> FilePath -> IO LineCost
lineCost old new=do
 (code,out,err)<-readProcessWithExitCode"git"
  ["diff","--no-index","--no-ext-diff","--no-textconv","--numstat","--diff-algorithm=myers","--no-indent-heuristic","--",old,new]""
 need(code==ExitSuccess||code==ExitFailure 1)("line metric failed: "++err)
 counts<-mapM row(lines out)
 pure(LineCost(sum(map fst counts))(sum(map snd counts)))
 where
 row line=case words line of
  a:b:_->case(readMaybe a,readMaybe b)of
   (Just x,Just y)|x>=0&&y>=0->pure(x,y)
   _->fail"ECO objective: non-textual line metric"
  _->fail"ECO objective: malformed line metric"

-- Callbacks render scoring witnesses only. They never supply deployed RTL.
-- Sources identify caller-supplied policy code/data; they are provenance, not
-- automatic discovery of every Haskell dependency. Runtime SDK is bound by flow.
data CostProvider=CostProvider
 { costPolicy::String,costSources::[FilePath]
 , costOldText::VProgram->Either String String
 , costNewText::VProgram->Either String String }
wholeFileCostProvider :: (VProgram -> String) -> (VProgram -> String) -> CostProvider
wholeFileCostProvider old new=CostProvider"whole-file-raw-v1"[](Right . old)(Right . new)
data CostObservation=CostObservation
 { costASTHash::String,costWitness::FilePath,costWitnessHash::String
 , costValue::LineCost }deriving(Eq,Read,Show)
data CostEnvelope=CostEnvelope
 { costSchema::Int,costSemanticPlanHash::String,costPolicyIdentity::String
 , costSourceIdentities::[FileIdentity]
 , costOldASTHash::String,costOldWitnessHash::String
 , costObservations::[CostObservation] }deriving(Eq,Read,Show)
data CostContext=CostContext
 { contextDirectory::FilePath,contextSources::[FileIdentity]
 , contextOldASTHash::String,contextOldWitnessHash::String }
-- Branch scores are comparable only when they share this exact OLD witness.
costBaselineIdentity :: CostContext -> (String,String)
costBaselineIdentity context=(contextOldASTHash context,contextOldWitnessHash context)
hashFile=fmap fiSHA256.fileIdentity
writeAST p ast=B.writeFile p(B.pack(encode ast))>>hashFile p
checked=either (ioError.userError) pure
unchanged expected=do
 actual<-fileIdentity(fiPath expected)
 need(actual==expected)("policy input changed: "++fiPath expected)
prepareCost :: CostProvider -> FilePath -> VProgram -> IO CostContext
prepareCost provider directory old=do
 need(not(null(costPolicy provider)))"empty objective policy identity"
 let target=directory</>"objective"
 exists<-doesPathExist target
 need(not exists)"objective output already exists"
 createDirectoryIfMissing True target
 sources<-mapM fileIdentity(nub(costSources provider))
 astHash<-writeAST(target</>"old.ast")old
 text<-checked(costOldText provider old)
 writeFile(target</>"old.score.v")text
 witness<-hashFile(target</>"old.score.v")
 pure(CostContext directory sources astHash witness)
validateContext context=do
 h<-hashFile(contextDirectory context</>"objective/old.score.v")
 a<-hashFile(contextDirectory context</>"objective/old.ast")
 need(h==contextOldWitnessHash context&&a==contextOldASTHash context)"OLD objective baseline artifacts changed"
scoreCost :: CostContext -> CostProvider -> String -> VProgram -> IO CostObservation
scoreCost context provider label ast=do
 validateContext context
 need(not(null label)&&all (`notElem`"/\\")label&&label/="."&&label/="..")"invalid score artifact label"
 let relative="objective"</>label++".score.v"
     path=contextDirectory context</>relative
 exists<-doesPathExist path
 need(not exists)"score witness already exists"
 text<-checked(costNewText provider ast)
 writeFile path text
 witness<-hashFile path
 -- Hash complete candidate AST, not its projected scoring representation.
 astHash<-writeAST(contextDirectory context</>"objective"</>label++".ast")ast
 value<-lineCost(contextDirectory context</>"objective/old.score.v")path
 pure(CostObservation astHash relative witness value)
sealCost :: CostContext -> CostProvider -> FilePath -> [CostObservation] -> IO CostEnvelope
sealCost context provider planPath observations=do
 validateContext context
 need(not(null observations))"missing ordinary objective observation"
 forM_(contextSources context)unchanged
 planHash<-hashFile planPath
 let envelope=CostEnvelope 1 planHash(costPolicy provider)(contextSources context)
      (contextOldASTHash context)(contextOldWitnessHash context)observations
 writeFile(contextDirectory context</>"objective.plan")(show envelope++"\n")
 pure envelope

-- Semantic replay is performed independently by the caller. This checks its
-- exact ordinary/accepted AST sequence, recomputes every score with the caller's
-- expected policy, and rejects stale source, witness, plan or baseline data.
verifyCostEnvelope :: CostProvider -> VProgram -> FilePath -> FilePath -> FilePath -> [VProgram] -> IO [LineCost]
verifyCostEnvelope provider old planPath envelopePath output programs=do
 originalEnvelope<-fileIdentity envelopePath
 text<-readFile envelopePath
 envelope<-maybe(fail"invalid ECO objective envelope")pure(readMaybe text)
 need(costSchema envelope==1)"unsupported objective envelope schema"
 need(costPolicyIdentity envelope==costPolicy provider)"objective policy differs from requested policy"
 sourceIDs<-mapM fileIdentity(nub(costSources provider))
 need(sourceIDs==costSourceIdentities envelope)"objective source identities changed"
 planHash<-hashFile planPath
 need(planHash==costSemanticPlanHash envelope)"objective semantic plan changed"
 need(length programs==length(costObservations envelope)&&not(null programs))"objective AST sequence length differs"
 exists<-doesPathExist output;need(not exists)"objective replay output already exists"
 createDirectory output
 context<-prepareCost provider output old
 need(contextOldASTHash context==costOldASTHash envelope)"objective OLD AST changed"
 need(contextOldWitnessHash context==costOldWitnessHash envelope)"objective OLD scoring renderer changed"
 bundle<-canonicalizePath(takeDirectory envelopePath)
 oldWitness<-canonicalizePath(bundle</>"objective/old.score.v")
 need(take(length(addTrailingPathSeparator bundle))oldWitness==addTrailingPathSeparator bundle)"OLD objective witness escapes bundle"
 oldHash<-hashFile oldWitness
 need(oldHash==costOldWitnessHash envelope)"OLD objective witness changed"
 values<-forM(zip3[0::Int ..]programs(costObservations envelope))$ \(i,program,record)->do
  let relative=costWitness record
  need(isRelative relative&&".."`notElem`splitDirectories relative)"objective witness escapes bundle"
  witness<-canonicalizePath(bundle</>relative)
  need(take(length(addTrailingPathSeparator bundle))witness==addTrailingPathSeparator bundle)"objective witness symlink escapes bundle"
  h<-hashFile witness;need(h==costWitnessHash record)"objective witness changed"
  fresh<-scoreCost context provider("replay-"++show i)program
  need(costASTHash fresh==costASTHash record)"objective candidate AST changed"
  need(costWitnessHash fresh==costWitnessHash record)"objective scoring renderer changed"
  need(costValue fresh==costValue record)"objective score changed"
  pure(costValue fresh)
 need(changedLines(last values)<=changedLines(head values))"selected objective worsens ordinary NEW"
 forM_ sourceIDs unchanged
 unchanged originalEnvelope
 writeFile(output</>"objective-replay.report")(show(originalEnvelope,sourceIDs,values)++"\n")
 pure values
