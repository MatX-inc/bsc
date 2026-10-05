{-# LANGUAGE BangPatterns #-}
-- | Source-package compilation and its invocation plan.
module SourceCompile (sourceInvocationPlan) where

import Control.Monad (when, foldM)
import qualified Control.Exception as CE
import Data.List (intersect, unzip5, foldl')
import qualified Data.Map as M
import qualified Data.Set as S
import System.Directory (getCurrentDirectory)
import System.IO (hFlush, stdout, hPutStr, stderr)
import System.Time (getClockTime)
import Data.Time.Clock.POSIX (getPOSIXTime)

import ListMap (lookupWithDefault)
import SCC (scc)
import ParseOp (parseOps)
import PFPrint
import Util (headOrErr, fromJustOrErr, fst3)
import FileNameUtil (baseName, hasDotSuf, dropSuf, dirName, bscSrcSuffix,
                     binSuffix, createEncodedFullFilePath, getRelativeFilePath)
import TopUtils
import Flags (Flags(..), DumpFlag(..), hasDump, verbose, extraVerbose, quiet)
import Error (internalError, ErrMsg(..), ErrorHandle, bsWarning, exitFail)
import CVPrint
import Id
import Backend (Backend(..))
import qualified BuildPlan as BP
import Depend (sourceDependencyPlan)
import GenModule (genModule)
import Deriving (derive)
import SymTab (SymTab)
import MakeSymTab (mkSymTab, cConvInst, getPackagesUsedInTypes)
import TypeCheck (cCtxReduceIO, cTypeCheck, mergeCATFCaches)
import PoisonUtils (mkPoisonedCDefn)
import GenSign (genUserSign, genEverythingSign)
import Simplify (simplify)
import ISyntax (IPackage(..), IDef(..), IExpr(..), mergeIATFCaches, fdVars)
import ISyntaxUtil (iMkRealBool, iMkLitSize, iMkString)
import IConv (iConvPackage, iConvDef)
import FixupDefs (fixupDefs, updDef)
import ISyntaxCheck (tCheckIPackage)
import ISimplify (iSimplify)
import BinUtil (BinMap, HashMap, readImports, replaceImportedSignatures)
import GenBin (genBinFile)
import GenWrap (genWrap, WrapInfo(..))
import GenFuncWrap (genFuncWrap, addFuncWrap)
import GenForeign (genForeign)
import IExpandUtils (HeapData)
import VPIWrappers (genVPIWrappers)
import DPIWrappers (genDPIWrappers)
import Version (bscVersionStr, buildnum)
import Classic (SyntaxMode(..), setSyntax)

-- This invocation plan is interpreted either by execution or by conservative
-- dependency discovery. The expensive compiler pipeline is a write effect:
-- no value from it is needed to discover another package's inputs.
sourceInvocationPlan :: ErrorHandle -> Flags -> String -> BP.BuildPlan (BP.BuildResult Bool)
sourceInvocationPlan errh flags name = do
    let verb = updCheck flags && showUpds flags && not (quiet flags)
        flags_depend = flags { updCheck = False, genName = [], showCodeGen = verb }
        flags_this = flags_depend { genName = genName flags }
    t <- BP.observe "source invocation clock" getNow
    BP.perform $ when (updCheck flags) $ do
      when verb $ putStrLnF "checking package dependencies"
      start flags DFdepend
    jobs <- sourceDependencyPlan errh flags name
    BP.performResult $ fmap (\(parserTime, pkgs) -> do
      when (updCheck flags) $ do
        let dumpnames = (Just (baseName (dropSuf name)), Nothing, Nothing)
        _ <- dump errh flags t DFdepend dumpnames (map fst3 pkgs)
        return ()
      let comp (success, binmap0, hashmap0) (fn, pkg, parse_warns) = do
            when verb $ putStrLnF ("compiling " ++ fn)
            when (not $ null parse_warns) $ bsWarning errh parse_warns
            let fl | not (updCheck flags) = flags
                   | fn == name = flags_this
                   | otherwise = flags_depend
            tc <- maybe getNow return parserTime
            (cur_success, binmap, hashmap) <-
              compilePackage errh fl tc binmap0 hashmap0 fn pkg
            return (cur_success && success, binmap, hashmap)
      (ok, _, _) <- foldM comp (True, M.empty, M.empty) pkgs
      when verb $
        if ok then putStrLnF "All packages are up to date."
              else putStrLnF "All packages compiled (some with errors)."
      return ok) jobs

-------------------------------------------------------------------------

compilePackage ::
    ErrorHandle ->
    Flags ->
    TimeInfo ->
    BinMap HeapData ->
    HashMap ->
    String ->
    CPackage ->
    IO (Bool, BinMap HeapData, HashMap)
compilePackage
    errh
    flags                -- user switches
    tStart
    binmap0
    hashmap0
    name_orig -- String --
    min@(CPackage pkgId _ _ _ _ _ _) = do

    -- Set syntax mode for the compilation pipeline (error messages, printing, etc.)
    setSyntax (if hasDotSuf bscSrcSuffix name_orig then CLASSIC else BSV)

    -- Encode the file path for internal use
    pwd <- getCurrentDirectory
    let name = createEncodedFullFilePath name_orig pwd
        dumpnames = (Just (baseName (dropSuf name)), Just (getIdString (unQualId pkgId)), Nothing)

    clkTime <- getClockTime
    epochTime <- getPOSIXTime

    -- Values needed for the Environment module
    let env =
            [("compilerVersion",iMkString $ bscVersionStr True),
             ("date",                iMkString $ show clkTime),
             ("epochTime",      iMkLitSize 32 $ floor epochTime),
             ("buildVersion",   iMkLitSize 32 $ buildnum),
             ("genPackageName", iMkString $ getIdBaseString pkgId),
             ("testAssert",        iMkRealBool $ testAssert flags)
            ]

    start flags DFimports
    -- Read imported signatures
    (mimp@(CPackage _ _ imps impsigs _ _ _), binmap, hashmap)
        <- readImports errh flags binmap0 hashmap0 min
    when (hasDump flags DFimports) $
      let impsigs' = [ppReadable s |  (CImpSign _ _ s) <- impsigs]
      in mapM_ (putStr) impsigs'
         --mapM_ (\ (CImpSign _ _ s) -> putStr (ppReadable s)) impsigs
    t <- dump errh flags tStart DFimports dumpnames mimp

    start flags DFopparse
    mop <- parseOps errh mimp
    t <- dump errh flags t DFopparse dumpnames mop

    -- Generate a global symbol table
    --
    -- Later stages will introduce new symbols that will need to be added
    -- to the symbol table.  Rather than worry about properly inserting
    -- the new symbols, we just build the symbol table fresh each time.
    -- So this is the first of several times that the table is built.
    -- We can't delay the building of the table until after all symbols
    -- are known, because these next stages need a table of the current
    -- symbols.
    --
    start flags DFsyminitial
    symt00 <- mkSymTab errh mop
    t <- dump errh flags t DFsyminitial dumpnames symt00

    -- whether we are doing code generation for modules
    let generating = backend flags /= Nothing

    -- Turn `noinline' into module definitions
    start flags DFgenfuncwrap
    (mfwrp, symt0, funcs) <- genFuncWrap errh flags generating mop symt00
    t <- dump errh flags t DFgenfuncwrap dumpnames mfwrp

    -- Generate wrapper for Verilog interface.
    start flags DFgenwrap
    (mwrp, gens) <- genWrap errh flags (genName flags) generating mfwrp symt0
    t <- dump errh flags t DFgenwrap dumpnames mwrp

    -- Rebuild the symbol table because GenWrap added new types
    -- and typeclass instances for those types
    start flags DFsympostgenwrap
    symt1 <- mkSymTab errh mwrp
    t <- dump errh flags t DFsympostgenwrap dumpnames symt1

    -- Re-add function definitions for `noinline'
    mfawrp <- addFuncWrap errh symt1 funcs mwrp

    -- Turn deriving into instance declarations
    start flags DFderiving
    mder <- derive errh flags symt1 mfawrp
    t <- dump errh flags t DFderiving dumpnames mder

    -- Rebuild the symbol table because Deriving added new instances
    start flags DFsympostderiving
    symt11 <- mkSymTab errh mder
    t <- dump errh flags t DFsympostderiving dumpnames symt11

    -- Extract packages used in type constructors from the parsed package
    -- (before any transformations that might expand synonyms or change types)
    let pkgsUsedInTypes = getPackagesUsedInTypes symt11 mder

    -- Reduce the contexts as far as possible
    start flags DFctxreduce
    (mctx, pkgsUsedInCtxReduce, atfCacheFromCtxReduce) <- cCtxReduceIO errh flags symt11 mder
    t <- dump errh flags t DFctxreduce dumpnames mctx

    -- Rebuild the symbol table because CtxReduce has possibly changed
    -- the types of top-level definitions
    start flags DFsympostctxreduce
    symt <- mkSymTab errh mctx
    t <- dump errh flags t DFsympostctxreduce dumpnames symt

    -- Turn instance declarations into ordinary definitions
    start flags DFconvinst
    let minst = cConvInst errh symt mctx
    t <- dump errh flags t DFconvinst dumpnames minst

    -- Type check and insert dictionaries
    start flags DFtypecheck
    (mod, tcErrors, pkgsUsedInCode, ctypeATFCache) <- cTypeCheck errh flags symt minst
    --putStr (ppReadable mod)
    t <- dump errh flags t DFtypecheck dumpnames mod

    --when (early flags) $ return ()
    let prefix = dirName name ++ "/"

    -- Generate wrapper info for foreign function imports
    -- (this always happens, even when not generating for modules)
    start flags DFgenforeign
    foreign_func_info <- genForeign errh flags prefix mod
    t <- dump errh flags t DFgenforeign dumpnames foreign_func_info

    -- Generate VPI wrappers for foreign function imports
    start flags DFgenVPI
    blurb <- mkGenFileHeader flags
    let ffuncs = map snd foreign_func_info
    vpi_wrappers <- if (backend flags /= Just Verilog)
                    then return []
                    else if (useDPI flags)
                         then genDPIWrappers errh flags prefix blurb ffuncs
                         else genVPIWrappers errh flags prefix blurb ffuncs
    t <- dump errh flags t DFgenVPI dumpnames vpi_wrappers

    -- Simplify a little
    start flags DFsimplified
    let mod' = simplify flags mod
    t <- dump errh flags t DFsimplified dumpnames mod'
    stats flags DFsimplified mod'

    --------------------------------------------
    -- Convert to internal abstract syntax
    --------------------------------------------
    start flags DFinternal
    let combinedATFCache = mergeCATFCaches ctypeATFCache atfCacheFromCtxReduce
    imod <- iConvPackage errh flags symt combinedATFCache mod'
    t <- dump errh flags t DFinternal dumpnames imod
    when (showISyntax flags) (putStrLnF (show imod))
    iPCheck flags symt imod "internal"
    stats flags DFinternal imod

    -- Read binary interface files
    start flags DFbinary
    let (_, _, impsigs', binmods0, pkgsigs) =
            let findFn i = fromJustOrErr "bsc: binmap" $ M.lookup i binmap
                sorted_ps = [ getIdString i
                               | CImpSign _ _ (CSignature i _ _ _) <- impsigs ]
            in  unzip5 $ map findFn sorted_ps

    -- injects the "magic" variables genC and genVerilog
    -- should probably be done via primitives
    -- XXX does this interact with signature matching
    -- or will it be caught by flag-matching?
    let adjEnv ::
            [(String, IExpr a)] ->
            (IPackage a) ->
            (IPackage a)
        adjEnv env (IPackage i lps ps ds atfCache)
                            | getIdString i == "Prelude" =
                    IPackage i lps ps (map adjDef ds) atfCache
            where
                adjDef (IDef i t x p) =
                    case lookup (getIdString (unQualId i)) env of
                        Just e ->  IDef i t e p
                        Nothing -> IDef i t x p
        adjEnv _ p = p

    let
        -- adjust the "raw" packages and then add back their signatures
        -- so they can be put into the current IPackage for linking info
        binmods = zip (map (adjEnv env) binmods0) pkgsigs

    t <- dump errh flags t DFbinary dumpnames binmods

    -- For "genModule" we construct a symbol table that includes all defs,
    -- not just those that are user visible.
    -- XXX This is needed for inserting RWire primitives in AAddSchedAssumps
    -- XXX but is it needed anywhere else?
    start flags DFsympostbinary
    -- XXX The way we construct the symtab is to replace the user-visible
    -- XXX imports with the full imports.
    let mint = replaceImportedSignatures mctx impsigs'
    internalSymt <- mkSymTab errh mint
    t <- dump errh flags t DFsympostbinary dumpnames mint

    start flags DFfixup
    let (imodf, alldefsList) = fixupDefs imod binmods
    let alldefs = M.fromList [(i, e) | IDef i _ e _ <- alldefsList]
    iPCheck flags symt imodf "fixup"
    t <- dump errh flags t DFfixup dumpnames imodf

    start flags DFisimplify
    let imods :: IPackage HeapData
        imods = iSimplify imodf
    iPCheck flags symt imods "isimplify"
    t <- dump errh flags t DFisimplify dumpnames imods
    stats flags DFisimplify imods

    -- The ATF cache used during elaboration: this package's entries unioned
    -- with the entries of every (transitively) loaded import.  This union is
    -- only ever held in memory; each .bo file stores just its own package's
    -- entries.  The union covers the full transitive closure because
    -- "binmods" does: each .bo's ipkg_depends records its writer's entire
    -- loaded closure (see ipkg_sigs in fixupDefs).
    let elabATFCache = foldl' mergeIATFCaches (ipkg_atf_cache imods)
                              [ ipkg_atf_cache m | (m, _) <- binmods ]

    let orderGens :: IPackage HeapData -> [WrapInfo] -> [WrapInfo]
        orderGens (IPackage pid _ _ ds _) gs =
                --trace (ppReadable (gis, g, os)) $
                                              map get os
          where gis = [ qualId pid i
                                | (WrapInfo i _ _ _ _ _) <- gs ]
                tr = [ (qualId pid i_, qualId pid i)
                                | (WrapInfo i _ _ i_ _ _) <- gs ]
                ds' = [ IDef (lookupWithDefault tr i i) t e p
                                | IDef i t e p <- ds, i `notElem` gis ]
                is = [ i | IDef i _ _ _ <- ds' ]
                g  = [ (i, fdVars e `intersect` is) | IDef i _ e _ <- ds' ]
                iis = scc g
                os = concat iis `intersect` gis
                get i = headOrErr "bsc.orderGens: no WrapInfo"
                                  [ x | x@(WrapInfo i' _ _ _ _ _) <- gs,
                                                unQualId i == i' ]
        ordgens :: [WrapInfo]
        ordgens = orderGens imods gens

    when (verbose flags) $
        putStr ("modules: " ++
                    ppReadable [ i | (WrapInfo { mod_nm = i }) <- ordgens ] ++
                    "\n")

    let getDef :: IPackage a -> Id -> IDef a
        getDef (IPackage _ _ _ ds _) i =
           case [ d | d@(IDef i' _ _ _) <- ds, unQualId i == unQualId i' ] of
                [ d ] -> d
                _ -> internalError ("No definition for " ++ pfpString i)

    -- Generate code for all requested modules
    -- TODO this function gen should be defined outside the compilePackage body
    -- Note: This accumulates the new IPackage on each iteration, but it
    --   doesn't update "alldefs"; this is likely OK because it is only used
    --   to build undefined values (in IExpand) and to insert RWires
    --   (in AAddSchedAssumps)
    let gen :: (IPackage HeapData, Bool) -> [WrapInfo] -> IO (IPackage HeapData, Bool)
        gen (im, !success) []  = return (im, success)
        gen (im, !success) (wi@(WrapInfo { mod_nm = i, wrapped_mod = i' }) : xs) = do
            let (mfile, mpkg, _) = dumpnames
                dumpnames' = (mfile, mpkg, Just (getIdString (unQualId i)))
                fwrapper = i `elem` map (\ (i, _, _, _, _) -> i) funcs

            let
                -- in the Maybe monad
                ex_filt ex = do (CE.ErrorCall s) <- (CE.fromException ex)::(Maybe CE.ErrorCall)
                                return s
                def_comp = do
                  def <- genModule errh wi fwrapper flags dumpnames'
                             prefix (getIdBaseString pkgId)
                             internalSymt alldefs elabATFCache (getDef im i')
                  return (def, True)
                ex_comp s = do
                  hFlush stdout >> hPutStr stderr s
                  -- XXX exitFail will do the pre-exit actions again
                  when (not (enablePoisonPills flags)) $ exitFail errh
                  return (mkPoisonedCDefn i (orig_cqt wi), False)

            (def, ok) <- CE.catchJust ex_filt def_comp ex_comp

            -- wrappercomp has substages, so record the overall start time
            tStartWrapper <- getNow

            start flags DFwrappercomp
            when (extraVerbose flags) $
                putStr ("definition of " ++
                        getIdString i ++
                        ":\n" ++
                        ppReadable def ++
                        "\n\n")

            -- "ok2" indicates whether there was a type-checking error
            -- but multiple-error-reporting chose to keep going;
            -- since it will already appear as a user error, no need for
            -- an internal error
            (idef, ok2) <- compileCDefToIDef errh flags dumpnames' symt imods def

            t <- getNow
            start flags DFwrapper_fixup
            -- Replace the pre-synthesis definition for a module with its
            -- post-synthesis definition, and update the package's cyclic
            -- references
            -- XXX Note that alldefs is not updated here.  This works
            -- XXX because the defs we use from it will not have changed.
            let im' = updDef idef im binmods
            t <- dump errh flags t DFwrapper_fixup dumpnames' im'

            t <- dump errh flags tStartWrapper DFwrappercomp dumpnames' idef
            -- recurse for each module in [WrapInfo]
            gen (im', success && ok && ok2) xs


    (imodr, success) <- gen (imods, True) ordgens

    t <- getNow
    -- Finally, generate interface files
    start flags DFwriteBin

    -- Generate the user-visible type signature
    (bi_sig, pkgsUsedInExports) <- genUserSign errh symt mctx
    -- Generate a type signature where everything is visible
    bo_sig <- genEverythingSign errh symt mctx

    -- Check for unused imports by combining packages from all three sources
    let (CPackage _ _ imports _ _ _ _) = mctx
        allUsedPkgs = S.unions [pkgsUsedInTypes, pkgsUsedInCtxReduce, pkgsUsedInCode, pkgsUsedInExports]
        importedPkgs = [i | (CImpId _ i) <- imports]
        unusedPkgs = filter (\pkg -> not (S.member pkg allUsedPkgs)) importedPkgs
        unusedWarns = [(getPosition pkg, WUnusedImport (pfpString pkg)) | pkg <- unusedPkgs]
    when (not (null unusedWarns)) $ bsWarning errh unusedWarns

    -- Generate binary version of the internal tree .bo file
    let bin_filename = putInDir (bdir flags) name binSuffix
    genBinFile errh bin_filename bi_sig bo_sig imodr

    -- Print one message for the two files
    let rel_binname = getRelativeFilePath bin_filename
    when (verbose flags) $
         putStrLnF ("Compiled package file created: " ++ rel_binname)
    t <- dumpStr errh flags t DFwriteBin dumpnames bin_filename

    -- XXX We could add the generated .bo directly to the maps,
    -- XXX but we lack the hash (which is generated when reading in a file)
    -- let binfile = (rel_binname, bi_sig, bo_sig, modr, ...)
    --     binmap' = ...
    --     hashmap' = ...

    return (success && not tcErrors, binmap, hashmap)


compileCDefToIDef :: ErrorHandle -> Flags -> DumpNames -> SymTab ->
                     IPackage a -> CDefn -> IO (IDef a, Bool)
compileCDefToIDef errh flags dumpnames symt ipkg def =
 do
    let pkgid = ipkg_name ipkg
    let cpkg0 = CPackage pkgid (Left []) [] [] [] [def] []
    t <- getNow

    start flags DFwrapper_ctxreduce
    (cpkg_ctx, _, _) <- cCtxReduceIO errh flags symt cpkg0
    t <- dump errh flags t DFwrapper_ctxreduce dumpnames cpkg_ctx

    start flags DFwrapper_typecheck
    (cpkg_chk, tcErrors, _usedPkgs, _wrapperATFCache) <- cTypeCheck errh flags symt cpkg_ctx
    t <- dump errh flags t DFwrapper_typecheck dumpnames cpkg_chk

    start flags DFwrapper_simplified
    let cpkg_simp = simplify flags cpkg_chk
        def' = case cpkg_simp of
                 (CPackage _ _ _ _ _ [d] _) -> d
                 _ -> internalError "compileCDefToIDef: unexpected number of defs"
    t <- dump errh flags t DFwrapper_simplified dumpnames cpkg_simp

    start flags DFwrapper_internal
    let idef = iConvDef errh flags symt ipkg def'
    t <- dump errh flags t DFwrapper_internal dumpnames idef

    return (idef, not tcErrors)

-- ===============

iPCheck :: Flags -> SymTab -> IPackage a -> String -> IO ()
iPCheck flags symt ipkg desc = -- deepseq ipkg $
        if doICheck flags && not (tCheckIPackage flags symt ipkg)
            then internalError (
                "internal typecheck failed (iPCheck after " ++
                desc ++ ")")
            else
                if (verbose flags)
                    then putStrLnF "types OK"
                    else return ()
