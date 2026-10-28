-- Native AST reporting only. Candidate inputs require authenticated context
-- and actual matching RTL bytes. This command does not prove equivalence.
module Main where
import ECOASTMetric
import System.Environment (getArgs)
import System.Exit (die)
import Text.Read (readMaybe)

usage = "usage: bluehs measure.hs OLD.eco-ast NEW.eco-ast OUT\n       bluehs measure.hs --candidate OLD.eco-ast CONTEXT.eco-ast CANDIDATE.ast ACTUAL.v OUT\n       bluehs measure.hs --inputs 'Snapshot OLD' 'ContextAST CONTEXT AST RTL' OUT\nInput records use Haskell quoted file paths; compare every output to the same OLD."

main = do
    args <- getArgs
    (old,new,out) <- case args of
      [a,b,o] -> return (Snapshot a,Snapshot b,o)
      ["--candidate",a,context,ast,rtl,o] -> return (Snapshot a,ContextAST context ast rtl,o)
      ["--inputs",a,b,o] -> case (readMaybe a,readMaybe b) of
        (Just x,Just y) -> return (x,y,o)
        _ -> die ("invalid MetricInput record\n"++usage)
      _ -> die usage
    measureAST out old new >>= putStrLn . renderMetricReport
