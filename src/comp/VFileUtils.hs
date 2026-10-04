module VFileUtils ( partitionStaleVerilogMods ) where

import Flags(Flags(..))
import ABin(ABinModInfo(..))
import ASyntax(apkg_name)
import Id(unQualId, getIdString)
import FileNameUtil(genFileName, mkVName, getFullFilePath, getRelativeFilePath,
                    moduleArtifactInputs)
import StaleUtils(allFreshVs)
import VFileName

import Data.Either(partitionEithers)

-- Check whether generated Verilog files are up to date with respect to
-- their module (.bmod/.bsched) files. The Verilog analogue of SimFileUtils,
-- built on the same StaleUtils conventions, with deliberate differences:
--   * no transitive invalidation: a parent's .v refers to child modules
--     by name only, so a stale child never invalidates a fresh parent
--   * no version check: a module pair that loads is current-version by
--     construction (decodeABin rejects other versions when it is read)
--   * no options descriptor: as before, a .v generated under different
--     codegen flags is reused as long as it is newer than its inputs

-- Split modules into (stale: regenerate, fresh: reuse the .v).  A
-- module's .v is stale when it is missing from the location BSC would
-- write it (per -vdir and the prefix) or is older than either artifact.
-- Legacy .ba inputs use the same check against their single file. Fresh
-- modules are returned as (full path for the simulator, relative name
-- for reporting), mirroring writeVerilog's two uses of the file name.
partitionStaleVerilogMods :: Flags -> String
                          -> [(String, ABinModInfo)]
                          -> IO ([(String, ABinModInfo)], [(VFileName, String)])
partitionStaleVerilogMods flags prefix abmis = do
    let checkOne (bafile, abmi) = do
          let modstr = getIdString (unQualId (apkg_name (abmi_apkg abmi)))
          vName_init <- genFileName mkVName (vdir flags) prefix modstr
          let vName = getFullFilePath vName_init
              vNameRel = getRelativeFilePath vName_init
          fresh_inputs <- mapM (\f -> allFreshVs f [vName])
                               (moduleArtifactInputs bafile)
          return $ if and fresh_inputs
                   then Right (VFileName vName, vNameRel)
                   else Left (bafile, abmi)
    results <- mapM checkOne abmis
    return (partitionEithers results)
