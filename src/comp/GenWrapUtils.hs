-- common utils for GenWrap and GenFuncWrap
module GenWrapUtils where

import FStringCompat
import Util(rTake, rDrop)
import ErrorUtil
import Position
import Id
import Pragma
import PreIds
import CSyntax
import CSyntaxUtil(posLiteral, numLiteralAt)
import CFreeVars(getFVD, getFVDl, getFVC, fvSetToFreeVars)
import CType
import Undefined (UndefKind(..))

-- ====================

genSuffix :: String
genSuffix = ['\173']

fsGenSuffix :: FString
fsGenSuffix = mkFString genSuffix


-- Add the Internal Flag to the Id
addInternalProp :: Id -> Id
addInternalProp lid = addIdProp lid IdPGeneratedIfc

isGenId :: Id -> Bool
isGenId i = hasIdProp i IdPGeneratedIfc


-- ====================
-- The BVI parser leaves this ordinary local binding for GenWrap to fill in
-- after it can inspect the imported interface's fields in declaration order.
bviWrapInfoId :: Id -> Id
bviWrapInfoId i = setInternal (mkIdPost i (mkFString "_wrapInfo"))

-- The style binding carries the source interface shape for polymorphic BVI
-- leaves, whose Bits constraint need not imply a Generic representation.
bviWrapStyleId :: Id -> Id
bviWrapStyleId i = setInternal (mkIdPost i (mkFString "_wrapStyle"))


-- ====================
-- Wrapping is ordinary library code, so its signatures must be available
-- before GenWrap and its dependency must be visible to -u and makedepend.
-- BVI imports already contain a qualified wrapping call after parsing;
-- synthesis and noinline definitions acquire one during GenWrap.
idModuleWrap :: Id
idModuleWrap = mkId noPosition (mkFString "ModuleWrap")

moduleWrapImports :: [Id] -> CPackage -> [CImport]
moduleWrapImports genNames (CPackage pkgId _ imps _ _ ds _)
  | pkgId == idModuleWrap = []
  | any importsWrap imps = []
  | any needsWrap ds = [CImpId True idModuleWrap]
  | otherwise = []
 where
  importsWrap (CImpId _ i) = i == idModuleWrap
  isWrapRef i = getIdQual i == getIdBase idModuleWrap
  refsNeedWrap = any isWrapRef . fvSetToFreeVars
  isGenerated i = any ((== unQualId i) . unQualId) genNames
  needsWrap (CPragma (Pproperties _ ps)) = PPverilog `elem` ps
  needsWrap (CPragma (Pnoinline is)) = not (null is)
  needsWrap (CValueSign d@(CDef i _ _)) = isGenerated i || refsNeedWrap (getFVD d)
  needsWrap (CValue i cs) = isGenerated i || any (refsNeedWrap . getFVC) cs
  needsWrap (Cinstance _ dls) = any (refsNeedWrap . getFVDl) dls
  needsWrap (Cstruct _ _ _ _ fs _) = any fieldNeedsWrap fs
  needsWrap (Cclass _ _ _ _ _ _ fs) = any fieldNeedsWrap fs
  needsWrap _ = False
  fieldNeedsWrap = any (refsNeedWrap . getFVC) . cf_default


-- ====================
-- Field and module rename utilities

-- A function to rename the top interface name -- should not rename port of the interface.
-- This needs to generate a unique name for the interface based on the pragmas
-- Therefore Ifc_ is different from Ifc_ {always_ready} etc
ifcIdRename :: [PProp] -> Id -> Id
ifcIdRename pps modid = addInternalProp (mkIdPost nm fsGenSuffix)
 where
  pos = getPosition modid
  nm = flattenUSId ((unQualId modid) : (relevantNames pps))
  relevantNames :: [PProp] -> Longname
  relevantNames []                           = []
  relevantNames ((PPalwaysReady is):ps)      = (pp "AR") : (concat is) ++ (relevantNames ps)
  relevantNames ((PPalwaysEnabled is):ps)    = (pp "AE") : (concat is) ++ (relevantNames ps)
  relevantNames ((PPenabledWhenReady is):ps) = (pp "EWR") : (concat is) ++ (relevantNames ps)
  relevantNames (p:ps)                       = relevantNames ps
  pp s = mkId pos (mkFString s)

-- For now this just adds an underscore
modIdRename :: [PProp] -> Id -> Id
modIdRename pps modid = addInternalProp (mkIdPost (unQualId modid) fsGenSuffix)

dropGenSuffix :: String -> String
dropGenSuffix s =
    let sufLength = length genSuffix
        hasSuf = (length s > length genSuffix) &&
                 (rTake sufLength s == genSuffix)
    in  if (hasSuf)
        then rDrop sufLength s
        else internalError ("dropGenSuffix: " ++ s)

dropGenSuffixId :: Id -> Id
dropGenSuffixId i =
    let base' = dropGenSuffix (getIdBaseString i)
    in  setIdBaseString i base'

-- ====================

getDefArgs :: [CClause] -> CType -> [(Id, CType)]
getDefArgs dcls t =
   let (ts, _) = getArrows t
       vs = take (length ts) tmpVarIds
       -- preserve function/module argument names, if possible
       guessArgName (CPVar li, _) = li
       guessArgName (_      , lv) = lv
       vs' = case dcls of
              [CClause ps _ _] -> let new_vs = map guessArgName (zip ps vs)
                                  -- add the dropped elements of vs to ensure
                                  -- that vs and vs' have the same length
                                  in new_vs ++ (drop (length new_vs) vs)
              _ -> vs
   in  -- check that guessArgName didn't lose any arguments
       if (length vs' /= length ts)
       then internalError ("GenWrapUtils.getDefArgs: " ++
                           (show ((length vs'), (length ts))))
       else zip vs' ts

-- ====================

mkTypeProxyExpr :: CType -> CExpr
mkTypeProxyExpr ty = CHasType (CAny (getPosition ty) UNotUsed) $ CQType [] ty

-- A proxy for the WrapField field-name type argument, evaluating to a
-- raw undefined stamped with the interface field's decl-side position.
-- The Prelude's port-name checks report their errors at getEvalPosition
-- of this proxy, i.e. at the user's interface field.  A plain don't-care
-- (mkTypeProxyExpr) would not do: class-dispatched undefined
-- construction rebuilds the nullary StrArg constructor and the position
-- is lost.
mkStrArgProxyExpr :: Position -> CType -> CExpr
mkStrArgProxyExpr pos ty =
    let e = cVApply idRawUndef [posLiteral pos, numLiteralAt pos (0 :: Integer)]
    in  CHasType e (CQType [] ty)


