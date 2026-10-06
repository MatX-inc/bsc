{-# LANGUAGE TemplateHaskell #-}
-- The Template Haskell generator for PrimOp's code tables.  It lives in
-- its own module because of the stage restriction: a function run by a
-- top-level splice must be imported, not defined in the module that
-- splices it.  Prim.hs documents why the tables are generated.
module PrimTH(primOpTables) where

import Data.List(sort, group)
import qualified Data.Map as M
import Language.Haskell.TH

import ErrorUtil(internalError)

-- primOpTables ''PrimOp ''PreElab codes retired
--
--   codes    every constructor's name, in encoding order: a
--            constructor's code is its index in this list
--   retired  (name, binding) for the codes of former constructors that
--            a file format still writes; a binding `name :: Int` is
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
          "\n  (a new primitive goes at the END of the table, and the .bo and .ba"
          ++ " format tags bump: GenBin.header, GenABin.header)"
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
  return ([codeSig, codeDef, fromSig, fromDef, allSig, allDef, anySig, anyDef] ++ retiredDecs)
  where
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
