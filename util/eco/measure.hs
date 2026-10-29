-- Native AST reporting only. Candidate inputs require authenticated context
-- and actual matching RTL bytes. This command does not prove equivalence.
module Main where
import ECOASTMetric
import ECOASTTranslatedMetric
import System.Environment (getArgs)
import System.Exit (die)

usage = "usage: bluehs measure.hs [--translated] OLD.eco-ast NEW.eco-ast OUT\n       bluehs measure.hs [--translated] --candidate OLD.eco-ast CONTEXT.eco-ast CANDIDATE.ast ACTUAL.v OUT\nCompare every output to the same OLD; --translated adds the synthesis-translated projection."

main = do
    args <- getArgs
    let (translated,rest) = case args of
          "--translated":more -> (True,more)
          more -> (False,more)
    (old,new,out) <- case rest of
      [a,b,o] -> return (Snapshot a,Snapshot b,o)
      ["--candidate",a,context,ast,rtl,o] -> return (Snapshot a,ContextAST context ast rtl,o)
      _ -> die usage
    if translated then measureTranslatedAST out old new >>= putStrLn . renderTranslatedMetricReport
                  else measureAST out old new >>= putStrLn . renderMetricReport
