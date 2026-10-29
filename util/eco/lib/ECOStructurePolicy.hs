-- Optional historical structure policy for external AST search.
module ECOStructurePolicy
  (StructurePolicy(..), applyStructurePolicy, structurePolicyLabel) where
import ECOASTSearch (SearchOptions(..))

-- This choice controls case/equal-arm conditional reconstruction only.
-- Name matching, helper-role recovery and Boolean rewrites remain independent.
-- Preserve respects explicit feature exclusions in a caller's configuration.
data StructurePolicy = PreserveStructure | OrdinaryStructure deriving (Eq,Read,Show)

applyStructurePolicy :: StructurePolicy -> SearchOptions -> SearchOptions
applyStructurePolicy PreserveStructure options = options
applyStructurePolicy OrdinaryStructure options =
    options {soFeatures=filter (`notElem` ["F09","F10"]) (soFeatures options)}

structurePolicyLabel :: SearchOptions -> String
structurePolicyLabel options = case filter (`elem` soFeatures options) ["F09","F10"] of
    [] -> "ordinary-case-and-equal-arm-conditional-structure"
    ["F09","F10"] -> "historical-case-and-equal-arm-conditional-restoration-enabled"
    features -> "custom-historical-control-features:" ++ show features
