-- A typed fixture filter, not an RTL parser. It renders the independently
-- captured ordinary AST, or an explicit candidate AST sidecar supplied by the
-- test. The candidate emission gate must still compare all actual bytes.
module Main where
import ECOAST
import BinData(decode)
import GenABin()
import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Environment(getArgs)
import System.Directory(doesFileExist)
main=do
 [context,path]<-getArgs
 snapshot<-readVerilogInfo context
 exists<-doesFileExist(path++".fixture.ast")
 program<-if exists then decode <$> B.readFile(path++".fixture.ast") else pure(viProgram snapshot)
 B.writeFile path(TE.encodeUtf8(T.pack(renderVerilogText snapshot program)))
