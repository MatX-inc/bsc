module SimFileUtils ( analyzeBluesimDependencies
                    , codeGenOptionDescr
                    , bluesimReusePlan
                    ) where

import Id(getIdBaseString)
import Flags(Flags(..))
import SimPackage
import SimPrimitiveModules(isPrimitiveModule)
import ASyntax(AVInst(..))
import VModInfo(VModInfo(..), getVNameString)
import Version(bscVersionStr)
import FileNameUtil
import ErrorUtil(internalError)
import BuildPlan

import System.Posix.Files
import System.Posix.Types(EpochTime)
import System.IO(openFile, hGetContents, hClose, IOMode(..))
import Control.Monad(filterM)
import Control.Exception(bracketOnError)
import Data.List(delete,find,isPrefixOf)
import qualified Data.Map as M

-- import Debug.Trace(traceM)

getModTime :: FilePath -> IO (Maybe EpochTime)
getModTime f =
    do ok <- fileExist f
       if ok
        then do s <- getFileStatus f
                return $ Just (modificationTime s)
        else return Nothing

codeGenOptionDescr :: Flags -> Bool -> String
codeGenOptionDescr flags is_top =
    unwords $ [ "Generation options:" ] ++
              (if (keepFires flags) then ["keep-fires"] else []) ++
              (if (is_top && (genSysC flags)) then ["sysc-top"] else [])

readCodeGenOptionDescr :: FilePath -> IO (Maybe String)
readCodeGenOptionDescr f =
    do ok <- fileExist f
       if ok
        then do bracketOnError (openFile f ReadMode)
                               (\hdl -> do hClose hdl
                                           return Nothing)
                               (\hdl -> do content <- hGetContents hdl
                                           let search_window = take 15 (lines content)
                                               comment = find ("/* Generation options: " `isPrefixOf`) search_window
                                           comment `seq` hClose hdl
                                           return comment)
        else return Nothing

-- The reuse decision is an observation, not code generation. Both interpreters
-- use these exact filenames, timestamps and option comments. The explicit
-- choices preserve rebuild and reuse paths during conservative discovery.
bluesimReusePlan :: Flags -> String -> FilePath -> String -> Bool -> BuildPlan Bool
bluesimReusePlan flags name ba_file version is_top =
    choose ("generated object compiler version for " ++ name)
        (version /= bscVersionStr True) (return False) $ do
            h_file <- observe ("generated header name for " ++ name) $
                genFileName mkHName (cdir flags) "" name
            o_file <- observe ("generated object name for " ++ name) $
                genFileName mkObjName (cdir flags) "" name
            _ <- requireFiles name "generated-object-reuse" "optional"
                [("generated-header", getRelativeFilePath h_file),
                 ("native-object", getRelativeFilePath o_file)]
                ["The .ba compiler version must match; header and object must both exist and have timestamps at least as new as the .ba. The header's Generation options comment must match keep-fires and, for the top, sysc-top. A rebuilt child invalidates its dependent parents."
                ,"These files are inspected even for C++ generation-only stops, because reuse suppresses generation. Preserve timestamps when exercising this behavior; neither file unconditionally substitutes for the .ba."]
            ba_time <- observe ("elaboration timestamp " ++ ba_file) (getModTime ba_file)
            h_time <- observe ("generated header timestamp " ++ h_file) (getModTime h_file)
            obj_time <- observe ("generated object timestamp " ++ o_file) (getModTime o_file)
            let stale_time = case (ba_time, h_time, obj_time) of
                    (Just t1, Just t2, Just t3) -> t2 < t1 || t3 < t1
                    _ -> True
            cg_opt <- observe ("generation options " ++ h_file) (readCodeGenOptionDescr h_file)
            let cg_tgt = Just ("/* " ++ codeGenOptionDescr flags is_top ++ " */")
                reusable = not stale_time && cg_opt == cg_tgt
            choose ("generated object reuse for " ++ name) reusable
                (return True) (return False)

remove_stale :: M.Map String [String] -> [String] -> [String] -> [String]
remove_stale _     []   _      = []
remove_stale _     pkgs []     = pkgs
remove_stale feeds pkgs (x:xs) =
    if (x `elem` pkgs)
    then let invalidated = maybe [] id (M.lookup x feeds)
         in remove_stale feeds (delete x pkgs) (invalidated ++ xs)
    else remove_stale feeds pkgs xs

analyzeBluesimDependencies :: Flags -> SimSystem -> BuildPlan [String]
analyzeBluesimDependencies flags sim_system =
    do let pkgs = M.elems (ssys_packages sim_system)
           ba_map = ssys_filemap sim_system
           influences pkg = let pname = getIdBaseString (sp_name pkg)
                                insts = [ name
                                        | i <- M.elems (sp_state_instances pkg)
                                        , let name = getVNameString (vName (avi_vmi i))
                                        , not (isPrimitiveModule name)
                                        ]
                            in map (\x -> (x,[pname])) insts
           feeds = M.fromListWith (++) (concatMap influences pkgs)
           top_mod = ssys_top sim_system
       stale_pkgs <- filterM (\pkg -> do
           let name = getIdBaseString (sp_name pkg)
               ba_file = case M.lookup name ba_map of
                   Nothing -> internalError $
                       "analyzeBluesimDependencies: unknown package " ++ name
                   Just path -> path
           reusable <- bluesimReusePlan flags name ba_file
               (sp_version pkg) (sp_name pkg == top_mod)
           return (not reusable)) pkgs
       let all_pkg_names = map (getIdBaseString . sp_name) pkgs
           stale_pkg_names = map (getIdBaseString . sp_name) stale_pkgs
           reusable = remove_stale feeds all_pkg_names stale_pkg_names
       return reusable
