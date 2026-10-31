-- Shared graph integrity checks, independent of any matching policy.
module ECO.Validate (validateGraph) where
import ECO.Types
import Control.Monad (unless)
import Data.Maybe (mapMaybe)
import qualified Data.Set as S

validateGraph :: EcoGraph -> Either String ()
validateGraph g = do
    unless (egSchema g == ecoGraphSchema) $ Left "unsupported ECO graph schema"
    let ns = egNodes g
        keys = S.fromList (map enKey ns)
        eligibleNames = mapMaybe enName (filter enEligible ns)
    unless (S.size keys == length ns) $ Left "duplicate ECO node key"
    unless (S.size (S.fromList eligibleNames) == length eligibleNames) $
        Left "duplicate eligible ECO signal name"
    unless (all (all (`S.member` keys) . enOperands) ns) $
        Left "ECO graph contains a dangling operand"
    unless (all (\n -> not (enEligible n) ||
                   (enName n /= Nothing && enBoundary n == Nothing &&
                    not (enOpaque n))) ns) $
        Left "protected or anonymous ECO node marked eligible"
