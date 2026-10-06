{-# LANGUAGE TemplateHaskell, MagicHash #-}
-- The Template Haskell generator for PrimOp's code tables.  It lives in
-- its own module because of the stage restriction: a function run by a
-- top-level splice must be imported, not defined in the module that
-- splices it.  Prim.hs documents why the tables are generated.
module PrimTH(primOpTables) where

import Data.Bits(xor)
import Data.Char(ord)
import Data.Word(Word64)
import Numeric(showHex)
import GHC.Exts(Int(I#), dataToTag#)
import Language.Haskell.TH

import ErrorUtil(internalError)

-- primOpTables ''PrimOp ''PreElab
--
-- reifies the type and generates, from its constructors in declaration
-- order,
--
--   primOpCode     :: PrimOp p -> Int
--                     the code a file writes for a primitive: its index
--                     in the declaration, which is its constructor tag
--                     (dataToTag#), so the function is a tag read
--   primOpFromCode :: Int -> PrimOp PreElab       the inverse
--   allPrimOps     :: [PrimOp PreElab]            every primitive, in
--                     code (= declaration) order
--   primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
--                     Just for a constructor declared at every phase
--                     (result type `PrimOp p`), Nothing for one whose
--                     result type is refined to some phases
--   primOpTableHash :: String                     16 hex digits: FNV-1a 64
--                     of the table (every code with its name), computed
--                     here at compile time; the .bo and .ba format tags
--                     carry it (GenBin.header, GenABin.header), so a
--                     primitive added, removed, renamed or moved changes
--                     the format identity without a manual bump
--
-- and fails the compile when a constructor is not nullary.  That a
-- constructor's tag is its position in the declaration is GHC's rule
-- (and reify lists the constructors in declaration order); dumpbo
-- -prim-codes reads every code back through primOpFromCode and checks
-- it names the primitive it came from, so the testsuite's pin would
-- catch the two orders disagreeing.
primOpTables :: Name -> Name -> Q [Dec]
primOpTables tyName preElab = do
  info <- reify tyName
  cons <- case info of
            TyConI (DataD _ _ _ _ cs _) -> return cs
            _ -> fail (show tyName ++ " is not a data type")
  conInfos <- mapM conInfo cons
  p <- newName "p"
  q <- newName "q"
  n <- newName "n"
  a <- newName "a"
  let ty = ConT tyName
      intT = ConT ''Int
      codeName = mkName "primOpCode"
      fromName = mkName "primOpFromCode"
      allName = mkName "allPrimOps"
      anyName = mkName "primOpAnyPhase"
      hashName = mkName "primOpTableHash"
      conNames = map fst conInfos
      coded = zip [0 :: Int ..] conNames
      -- primOpCode :: PrimOp p -> Int
      --   primOpCode a = I# (dataToTag# a)
      codeSig = SigD codeName (AppT (AppT ArrowT (AppT ty (VarT p))) intT)
      codeDef = FunD codeName
                  [ Clause [VarP a]
                      (NormalB (AppE (ConE 'I#) (AppE (VarE 'dataToTag#) (VarE a)))) [] ]
      -- primOpFromCode :: Int -> PrimOp PreElab
      fromSig = SigD fromName (AppT (AppT ArrowT intT) (AppT ty (ConT preElab)))
      fromDef = FunD fromName
                  ([ Clause [LitP (IntegerL (toInteger k))] (NormalB (ConE c)) []
                   | (k, c) <- coded ] ++
                   [ Clause [VarP n]
                       (NormalB (AppE (VarE 'internalError)
                                  (InfixE (Just (LitE (StringL (nameBase fromName ++ ": no primitive has the code "))))
                                          (VarE '(++))
                                          (Just (AppE (VarE 'show) (VarE n))))))
                       [] ])
      -- allPrimOps :: [PrimOp PreElab]
      allSig = SigD allName (AppT ListT (AppT ty (ConT preElab)))
      allDef = ValD (VarP allName) (NormalB (ListE [ ConE c | c <- conNames ])) []
      -- primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
      anySig = SigD anyName (AppT (AppT ArrowT (AppT ty (VarT p)))
                                  (AppT (ConT ''Maybe) (AppT ty (VarT q))))
      anyDef = FunD anyName
                 [ Clause [ConP c [] []]
                     (NormalB (if u then AppE (ConE 'Just) (ConE c) else ConE 'Nothing)) []
                 | (c, u) <- conInfos ]
      -- primOpTableHash :: String
      hashSig = SigD hashName (ConT ''String)
      hashDef = ValD (VarP hashName)
                  (NormalB (LitE (StringL (tableHash [ (k, nameBase c) | (k, c) <- coded ])))) []
  return [codeSig, codeDef, fromSig, fromDef, allSig, allDef, anySig, anyDef, hashSig, hashDef]
  where
    -- The hash of the table: FNV-1a 64 over the UTF-8 bytes of one line
    -- per primitive, "<code>\t<name>\n", in code order -- the lines
    -- dumpbo -prim-codes prints, so the value can be recomputed from
    -- that listing.  It changes when a primitive is added, removed,
    -- renamed or moved, and only then; nothing about the compiler that
    -- ran the splice enters it, so every build of one source has the
    -- same hash.
    tableHash :: [(Int, String)] -> String
    tableHash table =
        let line (k, c) = show k ++ "\t" ++ c ++ "\n"
            bytes = concatMap utf8 (concatMap line table)
            fnv :: Word64 -> [Int] -> Word64
            fnv h [] = h
            fnv h (b:bs) = let h' = (h `xor` fromIntegral b) * 0x100000001b3
                           in h' `seq` fnv h' bs
            hex = showHex (fnv 0xcbf29ce484222325 bytes) ""
        in replicate (16 - length hex) '0' ++ hex
    utf8 :: Char -> [Int]
    utf8 ch
      | n < 0x80    = [n]
      | n < 0x800   = [0xC0 + n `div` 0x40, 0x80 + n `mod` 0x40]
      | n < 0x10000 = [0xE0 + n `div` 0x1000, 0x80 + (n `div` 0x40) `mod` 0x40,
                       0x80 + n `mod` 0x40]
      | otherwise   = [0xF0 + n `div` 0x40000, 0x80 + (n `div` 0x1000) `mod` 0x40,
                       0x80 + (n `div` 0x40) `mod` 0x40, 0x80 + n `mod` 0x40]
      where n = ord ch
    -- a nullary constructor and whether its result type is the
    -- unrefined `PrimOp p`
    conInfo :: Con -> Q (Name, Bool)
    conInfo (ForallC _ _ c) = conInfo c
    conInfo (GadtC [c] [] ret) = return (c, unrefined ret)
    conInfo (NormalC c []) = return (c, True)
    conInfo c = fail ("primOpTables: not a nullary constructor: " ++ pprint c)
    unrefined (AppT (ConT t) arg) | t == tyName = isVar arg
    unrefined _ = False
    isVar (VarT _) = True
    isVar (SigT t _) = isVar t
    isVar (ParensT t) = isVar t
    isVar _ = False
