{-# LANGUAGE CPP #-}
module Main_dumpbo(main) where

import System.Environment(getArgs)
import System.Exit(exitWith, ExitCode(..))

import Control.Monad(when)

import PPrint
import PFPrint(pfpString)
import GenBin
import ISyntax
import CSyntax
import Id(isDictId)
import Eval(deepseq)
import Error(initErrorHandle)
import FileNameUtil(hasDotSuf, biSuffix, bcheckSuffix)
import System.IO
import qualified Data.ByteString as BS

data Mode = Full | Signature | Stats
          deriving (Eq)

main :: IO ()
main = do
    errh <- initErrorHandle
    as <- getArgs
    (mode, fname) <- case as of
                       ["-bi", mi]             -> return (Signature, mi)
                       ["-stats", mi]          -> return (Stats, mi)
                       [mi@(c:_)] | (c /= '-') -> return (Full, mi)
                       _ -> do putStr ("Usage: dumpbo [-bi|-stats] file\n")
                               exitWith (ExitFailure 1)
    file <- BS.readFile fname
    hSetEncoding stdout utf8
    if (hasDotSuf bcheckSuffix fname)
       then do
        -- a .bcheck holds the dependency names, the typechecked package
        -- after dictionary lifting and its lifted dictionaries; the package
        -- is printed as the compiler's -dliftdicts dump prints it
        (depends, pkg, lifted) <- readBcheckFile errh fname file
        let lifted' = lifted :: [IDef ()]
        case mode of
          Stats -> do
            -- decode and force everything, print counts alone
            (pkg, lifted') `deepseq` return ()
            let CPackage _ _ _ _ _ defs _ = pkg
            putStrLn ("defs: " ++ show (length defs))
            putStrLn ("dictionary bindings: " ++ show (dictBindings pkg))
            putStrLn ("lifted dictionaries: " ++ show (length lifted'))
          _ -> do
            putStrLn ("Depends: " ++ unwords (map pfpString depends))
            putStr (ppReadable pkg)
            when (not (null lifted')) $ do
                putStrLn ("Lifted dictionaries: ")
                putStr (ppReadable lifted')
       else if (hasDotSuf biSuffix fname)
       then do
        -- an interface file holds the dependency names and the user-visible
        -- signature, so "-bi" prints the same for it as for a .bo
        (depends, bi_sig) <- readBiFile errh fname file
        when (mode == Full) $
            putStrLn ("Depends: " ++ unwords (map pfpString depends))
        putStr (ppReadable bi_sig)
       else do
        (bi_sig, bo_sig, ipkg, hash) <- readBinFile errh fname file
        if (mode /= Full)
           then do putStr (ppReadable bi_sig)
           else do putStrLn ("Internal Symbols (export): ")
                   putStr (ppReadable bi_sig)
                   putStrLn ("Internal Symbols (all): ")
                   putStr (ppReadable bo_sig)
                   putStr (ppReadable (ipkg :: IPackage ()))
                   putStrLn ("Hash: " ++ hash)
    exitWith ExitSuccess

-- The dictionary bindings of a typechecked package: the let-bound
-- definitions whose Id the typechecker marked as a dictionary (IdPDict),
-- counted over every definition body.  Expressions are visited where they
-- can hold a binding; patterns, types and literals hold none.
dictBindings :: CPackage -> Int
dictBindings (CPackage _ _ _ _ _ defs _) = sum (map inDefn defs)
  where
    inDefn (CValueSign d) = inDef d
    inDefn (CValue _ cs) = sum (map inClause cs)
    inDefn (Cinstance _ ds) = sum (map inDefl ds)
    inDefn (Cclass _ _ _ _ _ _ fs) = sum (map inField fs)
    inDefn (Cstruct _ _ _ _ fs _) = sum (map inField fs)
    inDefn _ = 0
    inField f = sum (map inClause (cf_default f))
    inDef (CDef _ _ cs) = sum (map inClause cs)
    inDef (CDefT _ _ _ cs) = sum (map inClause cs)
    inClause (CClause _ qs e) = inQuals qs + inExpr e
    inQuals qs = sum (map inQual qs)
    inQual (CQGen _ _ e) = inExpr e
    inQual (CQFilter e) = inExpr e
    inDefl (CLValueSign d qs) = bound (defId d) + inDef d + inQuals qs
    inDefl (CLValue i cs qs) = bound i + sum (map inClause cs) + inQuals qs
    inDefl (CLMatch _ e) = inExpr e
    defId (CDef i _ _) = i
    defId (CDefT i _ _ _) = i
    bound i = if isDictId i then 1 else 0
    inMaybe = maybe 0 inExpr
    inStmt (CSBindT _ me _ _ e) = inMaybe me + inExpr e
    inStmt (CSBind _ me _ e) = inMaybe me + inExpr e
    inStmt (CSletseq ds) = sum (map inDefl ds)
    inStmt (CSletrec ds) = sum (map inDefl ds)
    inStmt (CSExpr me e) = inMaybe me + inExpr e
    inMStmt (CMStmt s) = inStmt s
    inMStmt (CMrules e) = inExpr e
    inMStmt (CMinterface e) = inExpr e
    inMStmt (CMTupleInterface _ es) = inExprs es
    inRule (CRule _ me qs e) = inMaybe me + inQuals qs + inExpr e
    inRule (CRuleNest _ me qs rs) = inMaybe me + inQuals qs + sum (map inRule rs)
    inArm (CCaseArm _ qs e) = inQuals qs + inExpr e
    inOp (CRand e) = inExpr e
    inOp (CRator _ _) = 0
    inExprs es = sum (map inExpr es)
    inFields :: [(a, CExpr)] -> Int
    inFields ies = inExprs (map snd ies)
    inExpr e = case e of
      CLam _ b -> inExpr b
      CLamT _ _ b -> inExpr b
      Cletseq ds b -> sum (map inDefl ds) + inExpr b
      Cletrec ds b -> sum (map inDefl ds) + inExpr b
      CSelect b _ -> inExpr b
      CCon _ es -> inExprs es
      Ccase _ b arms -> inExpr b + sum (map inArm arms)
      CStruct _ _ ies -> inFields ies
      CStructUpd b ies -> inExpr b + inFields ies
      Cwrite _ a b -> inExpr a + inExpr b
      CAny _ _ -> 0
      CVar _ -> 0
      CApply f es -> inExpr f + inExprs es
      CTaskApply f es -> inExpr f + inExprs es
      CTaskApplyT f _ es -> inExpr f + inExprs es
      CLit _ -> 0
      CBinOp a _ b -> inExpr a + inExpr b
      CHasType b _ -> inExpr b
      Cif _ a b c -> inExpr a + inExpr b + inExpr c
      CSub _ a b -> inExpr a + inExpr b
      CSub2 a b c -> inExpr a + inExpr b + inExpr c
      CSubUpdate _ a (b, c) d -> inExpr a + inExpr b + inExpr c + inExpr d
      Cmodule _ ss -> sum (map inMStmt ss)
      Cinterface _ _ ds -> sum (map inDefl ds)
      CmoduleVerilog m _ _ _ args _ _ _ -> inExpr m + inFields args
      CForeignFuncC _ _ -> 0
      Cdo _ ss -> sum (map inStmt ss)
      Caction _ ss -> sum (map inStmt ss)
      Crules _ rs -> sum (map inRule rs)
      COper ops -> sum (map inOp ops)
      CCon1 _ _ b -> inExpr b
      CSelectTT _ b _ -> inExpr b
      CCon0 _ _ -> 0
      CConT _ _ es -> inExprs es
      CStructT _ ies -> inFields ies
      CSelectT _ _ -> 0
      CLitT _ _ -> 0
      CAnyT _ _ _ -> 0
      CmoduleVerilogT _ m _ _ _ args _ _ _ -> inExpr m + inFields args
      CForeignFuncCT _ _ -> 0
      CTApply f _ -> inExpr f
      Cattributes _ -> 0
