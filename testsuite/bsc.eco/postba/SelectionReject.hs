module Main where
import ECOASTSelection
import ECO.VerilogInfo
import ECOASTFlow
import ECOASTReplay(rootsFromConfig)
import ECOASTSearch
import ECO.Replay
import ECOMatchTypes
import Verilog
import qualified Data.Generics as G
import BinData(encode)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import Control.Exception
import Control.Monad
import Data.List(isInfixOf)
import System.Directory
import System.FilePath
import System.Environment
readRecord path=read.BC.unpack <$> B.readFile path

fixture source target=do
 createDirectory target
 cfg<-read <$> readFile(source</>"effective.config")
 writeFile(target</>"effective.config")(show(cfg{cfOutput=target})++"\n")
 forM_["adapted-old","adapted-new","kept"] $ \name->do
  yes<-doesDirectoryExist(source</>name)
  when yes(createDirectoryLink(source</>name)(target</>name))
 forM_["ordinary","kept"] $ \branch->do
  let path=source</>(branch++"-search")</>"search.plan"
  yes<-doesFileExist path
  when yes$do
   plan<-read <$> readFile path
   let dest=target</>(branch++"-search")
   createDirectory dest
   copyFile path(dest</>"search.plan")
   forM_(planSteps plan) $ \s->do
    let relative=stepArtifact s
    createDirectoryIfMissing True(takeDirectory(dest</>relative));copyFile(takeDirectory path</>relative)(dest</>relative)
 yes<-doesFileExist(source</>"keep-control-match.eco-plan")
 when yes(copyFile(source</>"keep-control-match.eco-plan")(target</>"keep-control-match.eco-plan"))
 return cfg
main=do
 [source,rootsPath,output]<-getArgs
 createDirectory output
 roots<-rootsFromConfig rootsPath
 -- Mutations target a recorded plan with at least one accepted step.
 stepped<-filterM(\b->do
   let p=source</>(b++"-search")</>"search.plan"
   yes<-doesFileExist p
   if yes then (not.null.planSteps) <$> (readRecord p::IO ASTPlan) else pure False)["ordinary","kept"]
 branch<-case stepped of b:_->pure b;[]->fail"fixture has no recorded accepted step"
 let check label expected mutate includeOriginal=do
      let flow=output</>label;out=flow++"-selection"
      cfg<-fixture source flow
      mutate cfg flow
      result<-try(selectPrefixes roots out(if includeOriginal then[source,flow]else[flow]))::IO(Either SomeException())
      case result of
       Right _->fail(label++" unexpectedly accepted")
       Left err->do
        let message=show err
        writeFile(flow++".rejected.txt")(message++"\n")
        unless(expected`isInfixOf`message)(fail(label++" failed for wrong reason: "++message))
        putStrLn("PASS "++label)
     planPath flow=flow</>(branch++"-search")</>"search.plan"
     editPlan flow f=do p<-readRecord(planPath flow);writeFile(planPath flow)(show(f p)++"\n")
 -- Swapped adapted sides no longer link the root captures; the shared loader
 -- refuses them before the cross-flow baseline comparison.
 check "mixed-authenticated-old" "native adapter source/capture/producer linkage differs" (\_ f->removeDirectoryLink(f</>"adapted-old")>>createDirectoryLink(source</>"adapted-new")(f</>"adapted-old")) True
 check "mixed-authenticated-new" "native adapter source/capture/producer linkage differs" (\_ f->removeDirectoryLink(f</>"adapted-new")>>createDirectoryLink(source</>"adapted-old")(f</>"adapted-new")) True
 check "broken-plan-chain" "broken accepted-step hash chain" (\_ f->editPlan f(\p->p{planSteps=(head(planSteps p)){stepBefore="broken"}:tail(planSteps p)})) False
 check "changed-artifact" "artifact digest changed" (\_ f->do p<-read <$>readFile(planPath f);B.appendFile(f</>(branch++"-search")</>stepArtifact(head(planSteps p)))(B.singleton 0)) False
 check "changed-kept-relation" "fresh ordinary NEW to kept proof failed" (\cfg f->do
   p<-readRecord(f</>"keep-control-match.eco-plan")
   kept<-readVerilogInfo(f</>"kept"</>cfModule cfg<.>"eco-ast")
   let VProgram[m]_ _=viProgram kept
       state=head[getVIdString n|VMRegGroup n _ _ _<-G.listify(const True)m]
       input=head[getVIdString n|(as,_)<-vm_ports m,VAInput n _<-as]
   writeFile(f</>"keep-control-match.eco-plan")(show(p{erpRenames=[(state,input)]}))) False
 check "valid-hash-wrong-function" "replayed proof failed" (\cfg f->do
   old<-readVerilogInfo(f</>"adapted-old"</>"capture"</>cfModule cfg<.>"eco-ast")
   plan<-readRecord(planPath f)
   let original=head(planSteps plan);path=f</>(branch++"-search")</>stepArtifact original
   B.writeFile path(B.pack(encode(viProgram old)))
   ident<-fileIdentity path
   let step=original{stepAfter=fiSHA256 ident,stepArtifactHash=fiSHA256 ident}
   writeFile(planPath f)(show(plan{planSteps=[step],planOutput=fiSHA256 ident})++"\n")) False
 writeFile(output</>"summary.txt")("PASS negativechecks=6 on the "++branch++" plan; mixed baselines, plan/artifact, kept relation, valid-hash wrong-function candidate all refused\n")
