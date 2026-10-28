{-# LANGUAGE CPP #-}
-- Optional scoring policy; full candidate rendering and proof stay unchanged.
module ECOASTTranslatedObjective
 ( TranslatedObjective(..),translatedCostProvider ) where
import ECOAST
import Verilog(VProgram(..))
import System.FilePath ((</>),takeDirectory)
import ECOASTCost
import ECOASTMetric(normalizeHeader,normalizeProgram)
import ECOASTTranslatedMetric(projectTranslatedProgram)

data TranslatedObjective=TranslatedRaw|TranslatedNormalized deriving(Eq,Read,Show)
translatedCostProvider :: TranslatedObjective -> [FilePath] -> VerilogInfo -> VerilogInfo -> CostProvider
translatedCostProvider mode sources old new=CostProvider key (sources++[__FILE__,takeDirectory __FILE__ </> "ECOASTCost.hs",takeDirectory __FILE__ </> "ECOASTTranslatedMetric.hs",takeDirectory __FILE__ </> "ECOASTMetric.hs"]) (render old) (render new)
 where
 key=case mode of TranslatedRaw->"translated-raw-v1";TranslatedNormalized->"translated-normalized-v1"
 render context program=do
  _<-projectTranslatedProgram(VProgram[][][viHeader context])
  (projected,_)<-projectTranslatedProgram program
  pure $ case mode of
   TranslatedRaw->renderVerilogText context projected
   TranslatedNormalized->normalizeHeader(viHeader context)++normalizeProgram projected
