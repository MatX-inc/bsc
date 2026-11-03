-- Optional scoring policy; full candidate rendering and proof stay unchanged.
module ECOASTTranslatedObjective
 ( TranslatedObjective(..),translatedCostProvider ) where
import ECO.VerilogInfo
import Verilog(VProgram(..))
import ECOASTCost
import ECOASTMetric(normalizeHeader,normalizeProgram)
import ECOASTTranslatedMetric(projectTranslatedProgram)

data TranslatedObjective=TranslatedRaw|TranslatedNormalized deriving(Eq,Read,Show)
-- Sources are the caller's policy inputs; the SDK manifest binds this code.
translatedCostProvider :: TranslatedObjective -> [FilePath] -> VerilogInfo -> VerilogInfo -> CostProvider
translatedCostProvider mode sources old new=CostProvider key sources (render old) (render new)
 where
 key=case mode of TranslatedRaw->"translated-raw-v1";TranslatedNormalized->"translated-normalized-v1"
 render context program=do
  _<-projectTranslatedProgram(VProgram[][][viHeader context])
  (projected,_)<-projectTranslatedProgram program
  pure $ case mode of
   TranslatedRaw->renderVerilogText context projected
   TranslatedNormalized->normalizeHeader(viHeader context)++normalizeProgram projected
