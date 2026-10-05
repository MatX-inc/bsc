-- | Artifact orchestration shared by linking and dependency interpretation.
-- Reads expose the hierarchy; execution-only callbacks carry out the expensive
-- work. Stage and file-selection policy lives here, rather than in a scanner.
module DependencyArtifacts
    ( ArtifactActions(..), ArtifactResult(..), ArtifactStage(..), artifactStages, artifactMode
    , artifactPlan
    ) where

import Control.Monad (forM, forM_, when)
import qualified Control.Exception as E
import Data.Char (toLower)
import Data.List (nub)
import System.Directory (findExecutable)
import System.Environment (lookupEnv)
import System.FilePath ((</>), takeExtension)

import ABin
import ABinUtil (ABIHierarchy, getABIHierarchy, readAndCheckABin)
import ASyntax
import Backend (Backend(..))
import BuildPlan
import BuildSystem (getBinFmtType, binFmtToString)
import DependencyReport
import Error (ErrorHandle, EMsgs)
import FileNameUtil (mkCxxName, mkHName, mkObjName, mkVPICName, mkVPIHName,
                     mkNameWithoutSuffix, genFileName, getRelativeFilePath,
                     mangleFileName, dropSuf)
import Flags (Flags(..), DumpFlag(..), verbose)
import ForeignFunctions (ForeignFunction(..))
import Id (getIdString)
import SimCCBlock (primBlocks, sb_name)
import SimFileUtils (bluesimReusePlan)
import TopUtils (dfltCCompile, dfltCxxCompile, dfltMake, dfltBSC_MAKEFLAGS)
import VModInfo (vName, getVNameString)

-- Each stage returns the value required by the next stage. BuildResult keeps
-- those execution results opaque to discovery: no callback communicates
-- through a mutable cell, and no missing result is replaced with a dummy.
data ArtifactActions checked generated compiled linked = ArtifactActions
    { checkArtifactInputs :: IO checked
    , readArtifactInputs :: checked -> [(FilePath, ABin)] -> IO checked
    , generateArtifacts :: checked -> [(FilePath, ABin)] ->
                           Either EMsgs ABIHierarchy -> BuildPlan generated
    , compileArtifacts :: generated -> IO compiled
    , linkArtifacts :: compiled -> IO linked
    , finishArtifacts :: ArtifactResult generated compiled linked -> IO ()
    }

data ArtifactResult generated compiled linked
    = AfterGeneration generated
    | AfterCompilation compiled
    | AfterLink linked

data ArtifactStage = GenerateArtifacts | CompileArtifacts | LinkArtifacts
    deriving (Eq, Show)

artifactStages :: Flags -> Backend -> [ArtifactStage]
artifactStages flags backend' = GenerateArtifacts :
    [CompileArtifacts | compiles] ++ [LinkArtifacts | links]
  where
    stage = artifactStopStage flags backend'
    compiles = stage /= Just DFsimBlocksToC && stage /= Just DFgenSystemC
    links = compiles && stage /= Just DFbluesimcompile &&
            (backend' == Verilog || not (genSysC flags))

artifactStopStage :: Flags -> Backend -> Maybe DumpFlag
artifactStopStage flags backend' = case (backend', kill flags) of
    (Bluesim, Just (stage, Nothing))
        | stage `elem` [DFsimBlocksToC, DFgenSystemC, DFbluesimcompile] -> Just stage
    _ -> Nothing

artifactMode :: Flags -> Backend -> String
artifactMode flags backend'
    | backend' == Verilog = "verilog-link"
    | CompileArtifacts `notElem` stages = "bluesim-cxx"
    | genSysC flags = "systemc-objects"
    | LinkArtifacts `notElem` stages = "bluesim-objects"
    | otherwise = "bluesim-link"
  where stages = artifactStages flags backend'

artifactPlan :: ErrorHandle -> Flags -> Backend -> String ->
    [FilePath] -> [FilePath] -> [FilePath] ->
    ArtifactActions checked generated compiled linked -> BuildPlan ()
artifactPlan errh flags backend' top baFiles vFiles cFiles actions = do
    checked <- performResult (pure (checkArtifactInputs actions))
    note "This is a conservative dependency report, not a prediction of successful linking. Missing, corrupt, backend-incompatible, or schedule-error elaboration files retain the ordinary compiler's failure behavior."
    note "Source packages and package objects (.bs/.bsv/.bo) do not substitute for elaboration files (.ba) in artifact-link mode. Build the .ba in a separate producer action."
    case (kill flags, stopStage) of
        (Nothing, _) -> return ()
        (_, Just stage) -> note ("Requested stop stage: " ++ drop 2 (show stage) ++ ". " ++ stopExplanation stage)
        (Just requested, Nothing) -> incomplete
            ("Dependency discovery does not model this conditional or unsupported stop request: " ++ show requested ++ ". The report conservatively covers the full artifact invocation.")
    forM_ (nub vFiles) $ \path -> required top "explicit-verilog" "verilog" path
        ["The simulator reads this source; an elaboration file cannot replace it."]
    forM_ (nub cFiles) $ \path -> required top "explicit-foreign-input" (cKind path) path
        ["The driver checks the command-line file before loading elaboration metadata, including for a generation-only stop. An object is not automatically rebuilt from a same-named C/C++ source, nor substituted for an explicitly named source."]
    forM_ (nub baFiles) $ \path ->
        required top "explicit-elaboration" "elaboration" path
            ["Explicit .ba inputs are opened and validated before following the hierarchy."]
    -- These stage contracts depend only on invocation inputs. Emit them in
    -- independent scopes before metadata reads, so a rejected .ba cannot hide
    -- an unrelated toolchain, explicit input, or known output.
    independently [requirements | (_, requirements) <- plannedStages]
    decoded <- forM (nub baFiles) $ \path -> do
        result <- observe ("read elaboration " ++ path) $
            tryArtifactRead (readAndCheckABin errh (Just backend') path)
        case result of
            Right abi -> return [abi]
            Left exception -> do
                incomplete ("Cannot discover elaboration dependencies from " ++ path ++
                            ": " ++ E.displayException exception)
                perform (E.throwIO exception)
                return []
    let explicit = concat decoded
    ready <- performResult (readArtifactInputs actions <$> checked <*> pure explicit)
    -- Explicit foreign records also participate in Verilog's fallback when a
    -- complete design hierarchy cannot be loaded.
    forM_ explicit $ \(_, abi) -> case abi of
        ABinForeignFunc f _ -> planFunction (getIdString (ff_name (abffi_foreign_func f)))
        _ -> return ()
    hierarchy <- getABIHierarchy errh (verbose flags) (ifcPath flags) (Just backend')
        (map sb_name primBlocks) top explicit planArtifact
    generated <- planResult
        (generateArtifacts actions <$> ready <*> pure explicit <*> hierarchy)
    result <- if CompileArtifacts `elem` stages
        then do
            compiled <- performResult (compileArtifacts actions <$> generated)
            if LinkArtifacts `elem` stages
                then fmap AfterLink <$> performResult (linkArtifacts actions <$> compiled)
                else return (AfterCompilation <$> compiled)
        else return (AfterGeneration <$> generated)
    withResult result (finishArtifacts actions)
  where
    stages = artifactStages flags backend'
    plannedStages = [(stage, case stage of
        GenerateArtifacts -> generationRequirements
        CompileArtifacts -> compilationRequirements
        LinkArtifacts -> linkingRequirements) | stage <- stages]
    stopStage = artifactStopStage flags backend'
    compilesNative = CompileArtifacts `elem` stages
    linksBluesim = backend' == Bluesim && LinkArtifacts `elem` stages
    writesSystemC = genSysC flags && compilesNative
    usesNativeTools = backend' == Verilog || compilesNative
    linksNative = backend' == Verilog || linksBluesim
    requirement = reportRequirement
    output path = outputs [path]
    required owner role kind path explanation = do
        _ <- requireFiles owner role "required" [(kind, path)] explanation
        return ()
    root owner role path explanation = do
        _ <- requireFiles owner role "toolchain" [("directory-tree", path)] explanation
        return ()
    readEnv variable = observe ("environment " ++ variable) (lookupEnv variable)
    generatedFile mkName name = observe ("generated filename " ++ name) $
        getRelativeFilePath <$> genFileName mkNameWithoutSuffix (cdir flags) "" (mkName Nothing "" name)
    codeOutputs name = do
        cxx <- generatedFile mkCxxName name
        header <- generatedFile mkHName name
        outputs [cxx, header]
        when compilesNative $
            output (mangleFileName (mkObjName Nothing "" (dropSuf cxx)))
    verilogModule name = do
        _ <- requireFiles name "verilog-module" "search"
            [("verilog", path </> (name ++ ".v")) | path <- vPath flags]
            ["Conventional module filename candidates; explicit Verilog files or other files in the search trees may define this module. These alternatives require simulator resolution and do not include .ba."]
        return ()
    planArtifact path name abi = case abi of
        ABinMod m version -> planModule path name version (abmi_apkg m)
        ABinModSchedErr m version -> do
            note ("Elaboration metadata for " ++ name ++ " records a scheduling error; it is not a usable replacement for a successful elaboration artifact.")
            planModule path name version (abmsei_apkg m)
        ABinForeignFunc _ _ -> planFunction name
    planModule path name version pkg
        | backend' == Bluesim = do
            -- Execution checks reuse after the complete hierarchy is loaded.
            -- Inspect that same contract here only during discovery, so a bad
            -- generated header cannot hide a required missing child artifact.
            declareInputs $ bluesimReusePlan flags name path version (name == top) >> return ()
            codeOutputs name
        | otherwise = do
            verilogModule name
            forM_ (apkg_state_instances pkg) $ \inst ->
                when (avi_user_import inst) $
                    verilogModule (getVNameString (vName (avi_vmi inst)))
    planFunction name
        | backend' == Bluesim = generatedFile mkNameWithoutSuffix "imported_BDPI_functions.h" >>= output
        | otherwise = do
            when (not (useDPI flags)) $ do
                root top "vpi-runtime" (bluespecDir flags </> "VPI") []
                _ <- requireFiles name "vpi-wrapper" "one-of"
                    [("c-source", path </> mkVPICName Nothing "" name) | path <- vPath flags]
                    ["The ordinary Verilog linker requires this generated wrapper when using VPI; the foreign-function .ba does not replace its C source."]
                _ <- requireFiles name "vpi-wrapper-header" "search"
                    [("c-header", path </> mkVPIHName Nothing "" name) | path <- vPath flags]
                    ["Wrapper and registration-array compilation resolves headers through wrapper directories and the configured C include roots."]
                return ()
            externalCxx requirement incomplete
    generationRequirements
        | backend' == Bluesim = do
            note "Bluesim linking expands the .ba hierarchy and generates C++ before compiling objects. Existing generated headers and objects are optional reuse candidates, governed by .ba timestamps, generation-option comments, and dependent-module changes. Neither one alone replaces its .ba."
            codeOutputs ("model_" ++ top)
            when (genSysC flags) $
                note "-systemc normally writes and compiles a SystemC wrapper and does not link the ordinary Bluesim executable. A stop at simBlocksToC or genSystemC occurs before the wrapper files are written."
            when writesSystemC $ codeOutputs (top ++ "_systemc")
        | otherwise = return ()
    compilationRequirements = do
        when (backend' == Bluesim) $ do
            root top "bluesim-headers" (bluespecDir flags </> "Bluesim")
                ["Conservative header-search tree used while compiling generated C++; its static libraries are only linked when a Bluesim executable is requested."]
            when writesSystemC $ do
                systemc <- readEnv "SYSTEMC"
                case systemc of
                    Just path | not (null path) -> root top "systemc-headers" (path </> "include") []
                    _ -> incomplete "SYSTEMC is unset; the external C++ compiler's implicit SystemC-header search cannot be enumerated without invoking it."
            externalCxx requirement incomplete
        when (backend' == Verilog && not (null cFiles)) $ externalCxx requirement incomplete
        when usesNativeTools $
            forM_ (nub (cIncPath flags)) $ \path -> root top "c-include-root" path []
        when (usesNativeTools && any ((/= ".o") . takeExtension) cFiles) $
            incomplete "User C/C++ sources may include files outside the configured include trees; dependency discovery does not run the C preprocessor."
    linkingRequirements = do
        if backend' == Bluesim then do
            forM_ ["libbskernel.a", "libbsprim.a"] $ \library ->
                required top "bluesim-runtime-library" "native-library"
                    (bluespecDir flags </> "Bluesim" </> library) []
            required top "bluesim-export-map" "linker-script"
                (bluespecDir flags </> "Bluesim" </>
                 ("bs_" ++ map toLower (binFmtToString getBinFmtType) ++ "_export_map.txt")) []
            outputs [oFile flags, oFile flags ++ ".so"]
            note "The generated Bluesim launcher needs Bluetcl and the installed bluesim.tcl when it is run. Those are execution-time requirements, not substitutes for link inputs."
        else do
            note "Verilog linking passes .v/.sv files and module-search roots to the selected simulator. Regeneration of Verilog from .ba is disabled in the ordinary compiler, even when .ba contains Verilog metadata."
            required top "verilog-harness" "verilog"
                (bluespecDir flags </> "Verilog" </> "main.v") []
            forM_ (nub (vPath flags)) $ \path -> do
                _ <- requireFiles top "verilog-search-root" "search" [("directory-tree", path)]
                    ["Conservative module/include search tree. A module may be defined in a file whose basename differs from the module name."]
                return ()
            verilogModule top
            simulator requirement incomplete
            incomplete "The external Verilog simulator's preprocessing, module resolution, implicit libraries, and arbitrary -Xv options are not interpreted by bsc dependency discovery. Search trees above are conservative known roots, not a complete external-tool closure."
            output (oFile flags)
        when linksNative $
            forM_ (nub (cLibPath flags)) $ \path -> root top "c-library-root" path
                ["The external linker chooses static/shared libraries according to its flags and platform."]
        when (linksNative && not (null (cLibs flags))) $
            note ("External linker library requests: " ++ unwords (map ("-l" ++) (cLibs flags)))
    stopExplanation DFsimBlocksToC =
        "Ordinary C++ and header generation has finished; native compilation, SystemC wrapper writing, and linking do not run."
    stopExplanation DFgenSystemC =
        "Ordinary C++ and header generation has finished. SystemC wrapper text has been constructed when requested, but its files have not been written; native compilation and linking do not run."
    stopExplanation DFbluesimcompile =
        "Native object compilation has finished; the Bluesim shared-library link and launcher generation do not run."
    stopExplanation _ = ""
    cKind path | takeExtension path == ".o" = "native-object"
               | otherwise = "c-source"

    externalCxx :: (DependencyRequirement -> BuildPlan ()) -> (String -> BuildPlan ()) -> BuildPlan ()
    externalCxx requirement incomplete = do
        forM_ [("CXX", dfltCxxCompile), ("CC", dfltCCompile)] $ \(variable, fallback) -> do
            configured <- readEnv variable
            let command = maybe fallback id configured
            executable <- if length (words command) == 1 then observe ("resolve executable " ++ command) (findExecutable command) else return Nothing
            candidates <- case executable of
                Just path -> (:[]) <$> observe path (fileCandidate "executable" path)
                Nothing -> return []
            requirement (Requirement top (map toLower variable ++ "-toolchain") "toolchain" candidates
                [variable ++ "=" ++ command
                ,"Command resolution depends on PATH. Shell command prefixes, implicit compiler headers, linker startup objects, and system libraries are outside this report."])
        envFlags <- forM ["CFLAGS", "BSC_CFLAGS", "CXXFLAGS", "BSC_CXXFLAGS"] $ \variable -> do
            value <- readEnv variable
            return (variable ++ maybe " is unset (compiler default applies)" ("=" ++) value)
        requirement (Requirement top "native-compiler-environment" "toolchain" [] envFlags)
        when (backend' == Bluesim && parallelSimLink flags > 1) $ do
            make <- readEnv "MAKE"
            makeFlags <- readEnv "BSC_MAKEFLAGS"
            requirement (Requirement top "parallel-native-build-tool" "toolchain" []
                ["MAKE=" ++ maybe dfltMake id make
                ,"BSC_MAKEFLAGS=" ++ maybe dfltBSC_MAKEFLAGS id makeFlags
                ,"MAKEFLAGS can also affect the external make process."])
        incomplete ("The external C/C++ toolchain's implicit headers, plugins, and environment flags" ++
            (if linksNative then ", plus linker inputs and system libraries," else "") ++
            " are not enumerated. Pin or separately discover the toolchain when using this report for hermetic builds.")

    simulator :: (DependencyRequirement -> BuildPlan ()) -> (String -> BuildPlan ()) -> BuildPlan ()
    simulator requirement incomplete = do
        env <- readEnv "BSC_VERILOG_SIM"
        let name = maybe (maybe "" id env) id (vsim flags)
        if null name
        then do
            candidate <- observe "simulator support directory" (directoryCandidate "directory-tree" (bluespecDir flags </> "exec"))
            requirement (Requirement top "simulator-auto-detection" "toolchain" [candidate]
                ["The ordinary compiler runs simulator detection scripts. Dependency mode does not execute them; select -vsim to identify the wrapper explicitly."])
            incomplete "Simulator auto-detection was not executed; the selected simulator and its external toolchain are unknown."
        else do
            let wrapper | '/' `elem` name = name
                        | otherwise = bluespecDir flags </> "exec" </> ("bsc_build_vsim_" ++ name)
            candidate <- observe wrapper (fileCandidate "simulator-wrapper" wrapper)
            requirement (Requirement top "simulator-wrapper" "toolchain" [candidate]
                ["The wrapper may invoke additional tools and read environment-dependent files. It is reported without running its detect or link command."])
            execTree <- observe "simulator support directory" (directoryCandidate "directory-tree" (bluespecDir flags </> "exec"))
            requirement (Requirement top "simulator-wrapper-support" "toolchain" [execTree]
                ["Conservative tree for platform and common wrapper helpers."])

-- Preserve the actual exception for execution while allowing discovery to
-- retain the successfully read explicit inputs. Cancellation is never caught.
tryArtifactRead :: IO a -> IO (Either E.SomeException a)
tryArtifactRead action = E.catch (Right <$> action) handler
  where
    handler exception = case E.fromException exception :: Maybe E.SomeAsyncException of
        Just _ -> E.throwIO exception
        Nothing -> return (Left exception)
