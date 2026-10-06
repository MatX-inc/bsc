{-# LANGUAGE TemplateHaskell #-}
-- The Template Haskell generator for PrimOp's code tables.  It lives in
-- its own module because of the stage restriction: a function run by a
-- top-level splice must be imported, not defined in the module that
-- splices it.  Prim.hs documents why the tables are generated.
module PrimTH(primOpTables) where

import Data.Bits(xor)
import Data.Char(ord)
import Data.List(sort, group)
import qualified Data.Map as M
import Data.Word(Word64)
import Numeric(showHex)
import Language.Haskell.TH

import ErrorUtil(internalError)

-- primOpTables ''PrimOp ''PreElab codes retired
--
--   codes    every constructor's name, in encoding order: a
--            constructor's code is its index in this list
--   retired  (name, binding) for the codes of former constructors that
--            a file format still writes; a binding `binding :: Int` is
--            generated for each
--
-- generates
--   primOpCode     :: PrimOp p -> Int
--   primOpFromCode :: Int -> PrimOp PreElab
--   allPrimOps     :: [PrimOp PreElab]            (in code order)
--   primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
--                     Just for a constructor declared at every phase
--                     (result type `PrimOp p`), Nothing for one whose
--                     result type is refined to some phases
--   <binding>      :: Int                         (one per retired entry)
--   retiredPrimOpCodes :: [(Int, String)]         (code, former name),
--                     in code order, for the listing that pins the table
--   primOpTableHash :: String                     16 hex digits: FNV-1a 64
--                     of the table (every code with its name, retired
--                     ones marked), computed here at compile time; the
--                     .bo and .ba format tags carry it (GenBin.header,
--                     GenABin.header), so a change to the primitive set
--                     changes the format identity without a manual bump
--
-- and fails the compile when the constructor set and the table disagree.
primOpTables :: Name -> Name -> [String] -> [(String, String)] -> Q [Dec]
primOpTables tyName preElab codes retired = do
  info <- reify tyName
  cons <- case info of
            TyConI (DataD _ _ _ _ cs _) -> return cs
            _ -> fail (show tyName ++ " is not a data type")
  conInfos <- mapM conInfo cons
  let conNames = map fst conInfos
      byName = M.fromList [ (nameBase n, (n, u)) | (n, u) <- conInfos ]
      codeOf = M.fromList (zip codes [0 :: Int ..])
      retiredNames = map fst retired
      dupTable = [ c | (c:_:_) <- group (sort codes) ]
      missing = [ nameBase n | n <- conNames, not (M.member (nameBase n) codeOf) ]
      unknown = [ c | c <- codes, not (M.member c byName), c `notElem` retiredNames ]
      stillLive = [ c | c <- retiredNames, M.member c byName ]
      notInTable = [ c | c <- retiredNames, not (M.member c codeOf) ]
      problems =
        [ "duplicated in the code table: " ++ unwords dupTable | not (null dupTable) ] ++
        [ "constructor(s) without a code: " ++ unwords missing ++
          "\n  (a new primitive goes at the END of the table; the .bo and .ba"
          ++ " format tags carry the table's hash and change with it)"
        | not (null missing) ] ++
        [ "code table entries that are not constructors: " ++ unwords unknown ++
          "\n  (a removed primitive keeps its entry, listed as retired)"
        | not (null unknown) ] ++
        [ "retired but still a constructor: " ++ unwords stillLive | not (null stillLive) ] ++
        [ "retired but not in the code table: " ++ unwords notInTable | not (null notInTable) ]
  if null problems
    then return ()
    else fail ("primOpTables " ++ nameBase tyName ++ ":\n" ++ unlines problems)
  p <- newName "p"
  q <- newName "q"
  n <- newName "n"
  let ty = ConT tyName
      intT = ConT ''Int
      codeName = mkName "primOpCode"
      fromName = mkName "primOpFromCode"
      allName = mkName "allPrimOps"
      anyName = mkName "primOpAnyPhase"
      code c = codeOf M.! nameBase c
      inCodeOrder = map snd (M.toAscList (M.fromList [ (code c, c) | c <- conNames ]))
      lit = LitE . IntegerL . toInteger
      -- primOpCode :: PrimOp p -> Int
      codeSig = SigD codeName (AppT (AppT ArrowT (AppT ty (VarT p))) intT)
      codeDef = FunD codeName
                  [ Clause [ConP c [] []] (NormalB (lit (code c))) [] | c <- conNames ]
      -- primOpFromCode :: Int -> PrimOp PreElab
      fromSig = SigD fromName (AppT (AppT ArrowT intT) (AppT ty (ConT preElab)))
      fromDef = FunD fromName
                  ([ Clause [LitP (IntegerL (toInteger (code c)))] (NormalB (ConE c)) []
                   | c <- inCodeOrder ] ++
                   [ Clause [VarP n]
                       (NormalB (AppE (VarE 'internalError)
                                  (InfixE (Just (LitE (StringL (nameBase fromName ++ ": no primitive has the code "))))
                                          (VarE '(++))
                                          (Just (AppE (VarE 'show) (VarE n))))))
                       [] ])
      -- allPrimOps :: [PrimOp PreElab]
      allSig = SigD allName (AppT ListT (AppT ty (ConT preElab)))
      allDef = ValD (VarP allName) (NormalB (ListE [ ConE c | c <- inCodeOrder ])) []
      -- primOpAnyPhase :: PrimOp p -> Maybe (PrimOp q)
      anySig = SigD anyName (AppT (AppT ArrowT (AppT ty (VarT p)))
                                  (AppT (ConT ''Maybe) (AppT ty (VarT q))))
      anyDef = FunD anyName
                 [ Clause [ConP c [] []]
                     (NormalB (if u then AppE (ConE 'Just) (ConE c) else ConE 'Nothing)) []
                 | (c, u) <- conInfos ]
      retiredDecs = concat [ [ SigD (mkName b) intT
                             , ValD (VarP (mkName b)) (NormalB (lit (codeOf M.! c))) [] ]
                           | (c, b) <- retired ]
      -- retiredPrimOpCodes :: [(Int, String)]
      retiredName = mkName "retiredPrimOpCodes"
      retiredSig = SigD retiredName (AppT ListT (AppT (AppT (TupleT 2) intT) (ConT ''String)))
      retiredDef = ValD (VarP retiredName)
                     (NormalB (ListE [ TupE [Just (lit k), Just (LitE (StringL c))]
                                     | (k, c) <- sort [ (codeOf M.! c, c) | (c, _) <- retired ] ])) []
      -- primOpTableHash :: String
      hashName = mkName "primOpTableHash"
      hashSig = SigD hashName (ConT ''String)
      hashDef = ValD (VarP hashName)
                  (NormalB (LitE (StringL (tableHash codes retiredNames)))) []
  return ([codeSig, codeDef, fromSig, fromDef, allSig, allDef, anySig, anyDef]
          ++ retiredDecs ++ [retiredSig, retiredDef, hashSig, hashDef])
  where
    -- The hash of the table: FNV-1a 64 over the UTF-8 bytes of one line
    -- per code, "<code>\t<name>\n" for a live primitive and
    -- "<code>\t<name>\tretired\n" for a retired one, in code order --
    -- the lines dumpbo -prim-codes prints, so the value can be recomputed
    -- from that listing.  It changes when a primitive is added, renamed,
    -- retired or reordered, and only then; nothing about the compiler
    -- that ran the splice enters it, so every build of one source has
    -- the same hash.
    tableHash :: [String] -> [String] -> String
    tableHash cs rs =
        let line (k, c) = show k ++ "\t" ++ c ++
                          (if c `elem` rs then "\tretired" else "") ++ "\n"
            bytes = concatMap utf8 (concatMap line (zip [0 :: Int ..] cs))
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
