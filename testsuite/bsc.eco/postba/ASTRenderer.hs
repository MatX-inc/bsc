module Main where
import ECO.VerilogInfo
import BinData(encode)
import GenABin()
import System.Directory(createDirectoryIfMissing)
import Verilog
import PPrint
import Control.Exception
import Control.Monad
import Data.List(isInfixOf)
import System.Environment
import System.FilePath
import qualified Data.ByteString as B

assertion label p=unless p(fail label)
vid=mkVId
modu ps body=VModule(vid "test") ["module header"] ps body
stringCommentProgram = VProgram
 [modu [([VAParameter(vid "S")Nothing(VEString "parameter string x") False],
          ["parameter string in a comment stays a comment"])] []] [] []
addComment(VProgram (m:ms) d c)=VProgram(m{vm_body=VMComment["external renderer candidate control"](VMGroup False[]):vm_body m}:ms)d c
addComment p=p
changeParam(VProgram ms d c)=VProgram(map ch ms)d c where
 ch m=m{vm_ports=map (\(ps,cs)->(map arg ps,cs))(vm_ports m)}
 arg(VAParameter i r (VEString _) s)=VAParameter i r(VEString "changed parameter") s
 arg x=x
expectReject label action=do
 result<-try action :: IO(Either SomeException ())
 case result of Left _->pure();Right _->fail(label++" unexpectedly accepted")
main=do
 [context,plainContext,out]<-getArgs
 let str=stringCommentProgram
 assertion "body/comment text not substituted" ("// parameter string in a comment stays a comment" `isInfixOf` pp80 str)
 plain<-readVerilogInfo plainContext
 plainExact<-verilogRenderingIsExact plain;assertion "ordinary rendering exact" plainExact
 snap<-readVerilogInfo context
 exact<-verilogRenderingIsExact snap;assertion "native parameter rendering exact" exact
 _<-verifyVerilogInfo snap(out</>"ordinary")
 let p=addComment(viProgram snap)
 let candidatePath=out</>"candidate"</>aeModule(viEnvelope snap)<.>"v"
 createDirectoryIfMissing True(out</>"candidate")
 B.writeFile(candidatePath++".fixture.ast")(B.pack(encode p))
 emitVerilog snap candidatePath p
 bytes<-readFile(out</>"candidate"</>aeModule(viEnvelope snap)<.>"v")
 assertion "candidate filter exactness" (bytes==renderVerilogText snap p)
 expectReject "changed string parameter" (emitVerilog snap(out</>"bad-parameter.v")(changeParam(viProgram snap)))
 expectReject "candidate-specific filter must preserve rendered bytes" (emitVerilog snap(out</>"missing-sidecar.v")p)
 validateVerilogInfo snap
 putStrLn "PASS native parameter layout, authenticated filter and changed-parameter rejection controls"
