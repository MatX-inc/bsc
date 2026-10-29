-- The paired native library owns checked loading, rendering and emission.
-- This facade retains only the external reports' historical policy spelling.
module ECOAST
  ( module Native, ASTRenderingPolicy(..), astRenderingPolicy ) where

import ECO.VerilogInfo as Native

data ASTRenderingPolicy = NativeRendering | ParameterStringMacroRendering
  deriving (Eq, Read, Show)

-- The second constructor is retained solely to read historical reports.
-- The upstream VProgram always uses the native renderer; preview adapters
-- belong to their separately qualified compiler/library combination.
astRenderingPolicy :: VerilogInfo -> ASTRenderingPolicy
astRenderingPolicy _ = NativeRendering
