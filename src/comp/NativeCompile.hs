-- Native C/C++ compilation and linking used by the compiler's backends.
module NativeCompile
    ( compileBluesimCFile, compileVPICFile, compileUserCFile
    , compileParallelCFiles, cxxLink, missingUserFiles
    ) where

import Control.Monad(when, unless, filterM)
import Data.Char(isSpace, toLower, ord)
import Data.List(partition, isSuffixOf, intercalate)
import Data.Maybe(isNothing)
import Numeric(showOct)
import System.Directory(getCurrentDirectory)
import System.Exit(ExitCode(ExitFailure, ExitSuccess))
import System.Posix.Files(fileMode, unionFileModes, ownerExecuteMode,
                          groupExecuteMode, setFileMode, getFileStatus)
import System.Process(system)
import System.Time(ClockTime(TOD))

import BuildSystem
import Error(ErrorHandle, exitFailWith)
import FileIOUtil(writeFileCatch, readFileMaybe, removeFileCatch)
import FileNameUtil(baseName, hasDotSuf, dropSuf, dirName, mangleFileName,
                    mkSoName, mkObjName, mkMakeName,
                    hSuffix, cSuffix, objSuffix, genFileName,
                    createEncodedFullFilePath, getFullFilePath,
                    getRelativeFilePath)
import Flags(Flags(..), verbose, quiet)
import IOUtil(getEnvDef)
import TopUtils

-- compile a Bluesim generated CXX file
-- returns the name of the object file created
compileBluesimCFile :: ErrorHandle -> Flags -> String -> IO String
compileBluesimCFile errh flags cName = do
    (cmd, oName, msg) <- cmdCompileBluesimCFile flags cName
    execCmd errh flags cmd
    unless (quiet flags) $ putStrLnF msg
    return oName

-- construct the command for compiling a Bluesim CXX file
-- and return the name of the object file
-- and the message to display to the user
-- (this is shared by the serial and parallel compilation paths)
cmdCompileBluesimCFile :: Flags -> String -> IO (String, String, String)
cmdCompileBluesimCFile flags cName = do
    systemc <- getEnvDef "SYSTEMC" ""
    let engine = if (genSysC flags) && ("_systemc" `isSuffixOf` (dropSuf cName))
                 then "SystemC"
                 else "Bluesim"
    let oName = mkObjName Nothing "" (dropSuf cName)
    -- show is used for quoting
    let incflags = ["-I" ++ show (bluespecDir flags) ++ "/Bluesim"] ++
                   (if ((engine == "SystemC") && (systemc /= ""))
                    then ["-I" ++ show systemc ++ "/include"]
                    else [])
        -- Generated C++ code may reference uninitialized variables when it
        -- is known to be safe.
        switches = incflags ++
                   [ "-Wno-uninitialized"
                   , "-fPIC"
                   , "-c"
                   , "-o"
                   , show (mangleFileName oName)
                   ]
        -- show is used for quoting
        opts = map show (cxxFlags flags)
        files = [show (mangleFileName cName)]
    cmd <- cmdCXXCompile flags (opts ++ switches) files
    let cNameRel = getRelativeFilePath cName
    -- we lie here and mention both header and object (un-mangled name)
    let msg = engine ++ " object created: " ++ (dropSuf cNameRel) ++
              ".{" ++ hSuffix ++ "," ++ objSuffix ++ "}"
    return (cmd, oName, msg)

-- returns the name of the object file created
compileVPICFile :: ErrorHandle -> Flags -> [String] -> String -> IO String
compileVPICFile errh flags incdirs cName = do
    let oName = mkObjName Nothing "" (dropSuf cName)
    -- show is used for quoting
    let incflags = map (("-I"++) . show) (cIncPath flags) ++
                   ["-I" ++ show (bluespecDir flags) ++ "/VPI"] ++
                   map (\d -> "-I" ++ d) incdirs
        switches = incflags ++
                   [ "-fPIC"
                   , "-c"
                   , "-o"
                   , show (mangleFileName oName)
                   ]
        files = [show (mangleFileName cName)]
    let compileFn = if hasDotSuf cSuffix cName
                    then cCompile
                    else cxxCompile
    -- show is used for quoting
    let opts = map show $ if hasDotSuf cSuffix cName
                          then cFlags flags
                          else cxxFlags flags
    compileFn errh flags (opts ++ switches) files
    let cNameRel = getRelativeFilePath cName
    let msg = "VPI object created: " ++
              (mkObjName Nothing "" (dropSuf cNameRel))
    unless (quiet flags) $ putStrLnF msg
    return oName

-- returns the name of the object file created
compileUserCFile :: ErrorHandle -> Flags -> Bool -> String -> IO String
compileUserCFile errh flags forVerilog cName = do
    (cmd, oName, msg) <- cmdCompileUserCFile flags forVerilog cName
    execCmd errh flags cmd
    unless (quiet flags) $ putStrLnF msg
    return oName

-- construct the command for compiling a user C or CXX file
-- and return the name of the object file
-- and the message to display to the user
-- (this is shared by the serial and parallel compilation paths)
cmdCompileUserCFile :: Flags -> Bool -> String -> IO (String, String, String)
cmdCompileUserCFile flags forVerilog cName = do
    let oName = mkObjName Nothing "" (dropSuf cName)
    -- show is used for quoting
    let incflags = map (("-I"++) . show) (cIncPath flags)
        switches = incflags ++
                   [ "-fPIC"
                   , "-c"
                   , "-o"
                   , show (mangleFileName oName)
                   ]
        files = [show (mangleFileName cName)]
    let cmdCompileFn = if hasDotSuf cSuffix cName
                       then cmdCCompile
                       else cmdCXXCompile
    -- show is used for quoting
    let opts = map show $ if hasDotSuf cSuffix cName
                          then cFlags flags
                          else cxxFlags flags
    cmd <- cmdCompileFn flags (opts ++ switches) files
    let cNameRel = getRelativeFilePath cName
    let msg = "User object created: " ++
              (mkObjName Nothing "" (dropSuf cNameRel))
    return (cmd, oName, msg)

-- Compile Bluesim and user C/C++ files in parallel, using "make"
compileParallelCFiles :: ErrorHandle -> Flags -> Bool ->
                         String -> [String] -> [String] ->
                         IO ([String], [String])
-- avoid having "make" report "nothing to be done"
compileParallelCFiles errh flags forVerilog toplevel [] [] = return ([], [])
compileParallelCFiles errh flags forVerilog toplevel gen_cNames user_cNames = do
    let mkBluesimRule cName = do
          (cmd, oName, msg) <- cmdCompileBluesimCFile flags cName
          let esc_oName = escMakeTarget oName
              esc_cmd = escMakeRecipe cmd
              esc_msg = escMakeRecipe msg
          -- we use "PHONY" to force recompilation
          -- we use singlequotes on the echos, to avoid interpreting strings
          let rule = ".PHONY: " ++ esc_oName ++ "\n" ++
                     esc_oName ++ ":\n" ++
                     (if (quiet flags) then ""
                      else "\t@echo exec: '" ++ esc_cmd ++ "'\n") ++
                     "\t@" ++ esc_cmd ++ "\n" ++
                     (if (quiet flags) then ""
                      else "\t@echo '" ++ esc_msg ++ "'\n")
          return (oName, rule, esc_oName)
    let mkUserRule cName = do
          (cmd, oName, msg) <- cmdCompileUserCFile flags forVerilog cName
          let esc_oName = escMakeTarget oName
              esc_cmd = escMakeRecipe cmd
              esc_msg = escMakeRecipe msg
          -- we use "PHONY" to force recompilation
          -- we use singlequotes on the echos, to avoid interpreting strings
          let rule = ".PHONY: " ++ esc_oName ++ "\n" ++
                     esc_oName ++ ":\n" ++
                     (if (quiet flags) then ""
                      else "\t@echo exec: '" ++ esc_cmd ++ "'\n") ++
                     "\t@" ++ esc_cmd ++ "\n" ++
                     (if (quiet flags) then ""
                      else "\t@echo '" ++ esc_msg ++ "'\n")
          return (oName, rule, esc_oName)
    (gen_oNames, gen_rules, gen_esc_oNames)
        <- mapM mkBluesimRule gen_cNames >>= return . unzip3
    (user_oNames, user_rules, user_esc_oNames)
        <- mapM mkUserRule user_cNames >>= return . unzip3
    let all_target = "all: " ++
                     intercalate " " (gen_esc_oNames ++ user_esc_oNames)

    -- write the makefile
    pwd <- getCurrentDirectory
    let pwdpath = createEncodedFullFilePath "placeholder" pwd
        prefix = (dirName pwdpath) ++ "/"
        basename = "compile_" ++ toplevel
    fname_init <- genFileName mkMakeName (cdir flags) prefix basename
    let fname = getFullFilePath fname_init
        fcontents = intercalate "\n" $
                        [all_target] ++ [""] ++
                        gen_rules ++ [""] ++
                        user_rules
    writeFileCatch errh fname fcontents

    -- execute make
    let jobs = parallelSimLink flags
        -- show is used for quoting
        switches = ["-f", show fname, "-j", show jobs]
        targets = ["all"]
    cmd <- cmdMake flags switches targets
    execCmd errh flags cmd

    -- delete the makefile unless in debug mode
    when (not (cDebug flags)) $
         removeFileCatch errh fname

    -- return the generated object file names
    return (gen_oNames, user_oNames)

-- Escape file names for use in a Makefile
escMakeTarget :: String -> String
escMakeTarget tname =
    let doEscape '#' = True
        doEscape ',' = True
        doEscape ':' = True
        doEscape ';' = True
        doEscape '=' = True
        doEscape '%' = True
        doEscape '$' = True
        doEscape c = isSpace c
        escChar c accum_str =
            if (doEscape c)
            then "\\0" ++ showOct (ord c) accum_str
            else (c:accum_str)
    in  foldr escChar "" tname

-- Escape commands for use in a Makefile
escMakeRecipe :: String -> String
escMakeRecipe cmd =
    let -- hash in a command is OK
        escChar '$' accum_str = "\044" ++ accum_str
        escChar c accum_str = (c:accum_str)
    in  foldr escChar "" cmd

-- Construct the Make command
cmdMake :: Flags -> [String] -> [String] -> IO String
cmdMake flags sws targets = do
    make <- getEnvDef "MAKE" dfltMake
    -- MAKEFLAGS is a reserved variable that 'make' uses for recursive calls;
    -- it should not be explicitly added to calls to 'make'
    --makeflags <- getEnvDef "MAKEFLAGS" dfltMAKEFLAGS
    let debug_flags = ""
    bsc_makeflags <- getEnvDef "BSC_MAKEFLAGS" dfltBSC_MAKEFLAGS
    let cmd = unwords $ [ make, debug_flags, bsc_makeflags ] ++
                        sws ++ targets
    return cmd

-- Call the C compiler (typically to generate .o from .c)
--   sws = switches (like -c, -o)
--   fs  = filenames
cCompile :: ErrorHandle -> Flags -> [String] -> [String] -> IO ()
cCompile errh flags sws fs = do
    cmd <- cmdCCompile flags sws fs
    execCmd errh flags cmd

-- Construct the C compiler command (typically to generate .o from .c)
--   sws = switches (like -c, -o)
--   fs  = filenames
cmdCCompile :: Flags -> [String] -> [String] -> IO String
cmdCCompile flags sws fs = do
    comp <- getEnvDef "CC" dfltCCompile
    cflags <- getEnvDef "CFLAGS" dfltCFLAGS
    let debug_flags = if (cDebug flags) then "-g" else ""
    bsc_cflags <- getEnvDef "BSC_CFLAGS" dfltBSC_CFLAGS
    let cmd = unwords $ [ comp, cflags, debug_flags, bsc_cflags ] ++ sws ++ fs
    return cmd

-- Call the C++ compiler (typically to generate .o from .cxx)
--   sws = switches (like -c, -o)
--   fs  = filenames
cxxCompile :: ErrorHandle -> Flags -> [String] -> [String] -> IO ()
cxxCompile errh flags sws fs = do
    cmd <- cmdCXXCompile flags sws fs
    execCmd errh flags cmd

-- Construct the C++ compiler command (typically to generate .o from .cxx)
--   sws = switches (like -c, -o)
--   fs  = filenames
cmdCXXCompile :: Flags -> [String] -> [String] -> IO String
cmdCXXCompile flags sws fs = do
    comp <- getEnvDef "CXX" dfltCxxCompile
    cflags <- getEnvDef "CXXFLAGS" dfltCXXFLAGS
    let debug_flags = if (cDebug flags) then "-g" else ""
    bsc_cflags <- getEnvDef "BSC_CXXFLAGS" dfltBSC_CXXFLAGS
    let cmd = unwords $ [ comp, cflags, debug_flags, bsc_cflags ] ++ sws ++ fs
    return cmd

-- Execute the command constructed by one of the above functions
execCmd :: ErrorHandle -> Flags -> String -> IO ()
execCmd errh flags cmd = do
    when (verbose flags) $ putStrLnF ("exec: " ++ cmd)
    rc <- system cmd
    case rc of
        ExitSuccess   -> return ()
        ExitFailure n -> exitFailWith errh n


-- link object files into a shared library
cxxLink :: ErrorHandle -> Flags -> String -> [String] -> TimeInfo -> IO ()
cxxLink errh flags toplevel names creation_time = do
    -- Construct the Bluesim object names
    let bsimLibDir = (bluespecDir flags) ++ "/Bluesim/"
        bsim_names = [ bsimLibDir ++ "lib" ++ name ++ ".a"
                     | name <- ["bskernel", "bsprim"] ]

    -- The schedule.o object should come after libkernel.a
    -- in the link order.  We remove it from names and let
    -- cFiles put it in the correct place.
    let isSchedule n = (baseName n == ("model_" ++ toplevel ++ ".o"))
        (schedule_name, other_names) = partition isSchedule names
        compile_names =
            other_names ++
            schedule_name ++
            bsim_names
    -- link the objects into a .so file
    when (verbose flags) $ putStrLnF "linking"
    let outFile = oFile flags
        soFile = if (dirName outFile) == "."
                 then mkSoName Nothing "" (baseName outFile)
                 else mkSoName (Just (dirName outFile)) "" (baseName outFile)
        -- show is used for quoting
        libdirflags = (map (("-L"++) . show) (cLibPath flags))
        userlibs = map (("-l"++) . show) (cLibs flags)
        exportmap = let binfmt = map toLower (binFmtToString getBinFmtType)
                    in  show $ (bluespecDir flags) ++ "/Bluesim/" ++
                               "bs_" ++ binfmt ++ "_export_map.txt"
        -- this flag doesn't seem to work, so we use a separate call to "strip"
        stripflags = [] -- if (cDebug flags) then [] else ["-Wl,-x"]
        switches =
          case getBinFmtType of
            ELF   -> ["-shared", "-fPIC", "-Wl,-Bsymbolic"] ++ libdirflags ++
                     ["-Wl,--version-script=" ++ exportmap] ++ stripflags ++
                     ["-o", soFile]
            MachO -> ["-dynamiclib", "-fPIC"] ++ libdirflags ++
                     ["-exported_symbols_list", exportmap] ++ stripflags ++
                     ["-o", soFile]
        -- show is used for quoting
        opts = map show $ linkFlags flags
        files = map show compile_names ++ ["-lm"] ++ userlibs
    cxxCompile errh flags (opts ++ switches) files
    when (not (cDebug flags)) $ cleanseSharedLib errh flags soFile
    unless (quiet flags) $ putStrLnF ("Simulation shared library created: " ++ soFile)
    -- Write a script to execute bluesim.tcl with the .so file argument
    let bluesim_cmd = "$BLUESPECDIR/tcllib/bluespec/bluesim.tcl"
        (TimeInfo _ (TOD t _)) = creation_time
        time_flags = if (timeStamps flags)
                     then [ "--creation_time", show t]
                     else []
    writeFileCatch errh outFile $
                   unlines [ "#!/bin/sh"
                           , ""
                           , "BLUESPECDIR=`echo 'puts $env(BLUESPECDIR)' | bluetcl`"
                           , ""
                           , "for arg in $@"
                           , "do"
                           , "  if (test \"$arg\" = \"-h\")"
                           , "  then"
                           , "    " ++ (unwords ["exec", bluesim_cmd, "$0.so", toplevel, "--script_name", "`basename $0`", "-h"])
                           , "  fi"
                           , "done"
                           , unwords $ ["exec", bluesim_cmd, "$0.so", toplevel, "--script_name", "`basename $0`"] ++ time_flags ++ ["\"$@\""]
                           ]
    stat <- getFileStatus outFile
    let mode = fileMode stat
        mode' = foldl1 unionFileModes [mode, ownerExecuteMode, groupExecuteMode]
    setFileMode outFile mode'
    unless (quiet flags) $ putStrLnF ("Simulation executable created: " ++ outFile)

-- strip unwanted symbols from a .so file
cleanseSharedLib :: ErrorHandle -> Flags -> String -> IO ()
cleanseSharedLib errh flags soFile = do
    let switches = case getBinFmtType of
                      ELF    -> ["-x"]
                      MachO  -> ["-u", "-x"]
        cmd = unwords $ ["strip"] ++ switches ++ [soFile]
    when (verbose flags) $ putStrLnF ("exec: " ++ cmd)
    rc <- system cmd
    case rc of
        ExitSuccess   -> return ()
        ExitFailure n -> exitFailWith errh n

missingUserFiles :: Flags -> [String] -> IO [String]
missingUserFiles flags cSrcFiles = filterM cantFind cSrcFiles
  where cantFind f = do x <- readFileMaybe f
                        return $ isNothing x
