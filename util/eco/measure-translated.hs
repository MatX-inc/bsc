-- Counting only; this command does not change the candidate or its proof.
module Main where
import ECOASTMetric(MetricInput(..))
import ECOASTTranslatedMetric
import System.Environment(getArgs)
import System.Exit(die)
import Text.Read(readMaybe)
usage="usage: bluehs measure-translated.hs OLD.eco-ast NEW.eco-ast OUT\n       bluehs measure-translated.hs --candidate OLD.eco-ast CONTEXT.eco-ast CANDIDATE.ast ACTUAL.v OUT\n       bluehs measure-translated.hs --inputs 'Snapshot OLD' 'ContextAST CONTEXT AST RTL' OUT"
main=do
 args<-getArgs
 (old,new,out)<-case args of
  [a,b,o]->pure(Snapshot a,Snapshot b,o)
  ["--candidate",a,c,s,v,o]->pure(Snapshot a,ContextAST c s v,o)
  ["--inputs",a,b,o]->case(readMaybe a,readMaybe b)of
   (Just x,Just y)->pure(x,y,o)
   _->die usage
  _->die usage
 measureTranslatedAST out old new >>=putStrLn.renderTranslatedMetricReport
