{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
module GenWrap(
               genWrap,
               WrapInfo(..)
              ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import Data.Char(isDigit)
import Data.List(nub, (\\), find)
import Control.Monad(when, foldM, filterM, zipWithM)
import Control.Monad.Except(ExceptT, runExceptT, throwError)
import Control.Monad.State(StateT, runStateT, lift, gets, get)
import PFPrint
import Position(Position, noPosition, cmdPosition)
import Error(internalError, EMsg, EMsgs(..), ErrMsg(..), ErrorHandle, bsError)
import ErrorMonad(ErrorMonad, convErrorMonadToIO)
import Flags(Flags)
import FStringCompat
import PreStrings(fsUnderscore, fs_t, fsEmpty, fsEnable, fs_rdy, fsDot)
import Id
import IdPrint
import PreIds
import CSyntax
import CSyntaxUtil
import SymTab(SymTab, TypeInfo(..), FieldInfo(..), findType,
              findField, findFieldInfo, getMethodArgNames)
import MakeSymTab(convCQType)
import Pred hiding (name)
import qualified Pred(name)
import Scheme
import Assump
import CType(cTNum, cTStr, tyConArgs, getArrows, cTVarNum, isIfc, getRes, typeclassId, splitTAp)
import VModInfo(VSchedInfo, VPathInfo(..), VeriPortProp(..), VArgInfo(..),
                VFieldInfo(..), VName(..), VWireInfo(..), VPort)
-- GenWrap defines its own versions that expand synonyms and use qualEq
import Type hiding (isPrimAction, isActionValue, getAVType,
                    isReset, isClock, isInout)
import TypeCheck(cCtxReduceDef)
import TCMisc(normT)
import qualified TIMonad as TI
import Subst(mkSubst, Types(..))
import Classic(isClassic)
import Pragma
import PragmaCheck
import Data.Maybe
import Util
import qualified Data.Map as M
import GenWrapUtils
import Undefined(UndefKind(UNotUsed))

-- ====================

type GWMonad = StateT GenState (ExceptT EMsgs IO)

data GenState = GenState
 {
   errHandle  ::  ErrorHandle,
   symtable   ::  SymTab,
   flags      ::  Flags
 }

runGWMonad :: GWMonad a -> GenState -> IO a
runGWMonad f s = do
  let errh = errHandle s
  result <- runExceptT ((runStateT f) s)
  case result of
    Right (res, _) -> return res
    Left msgs -> bsError errh (errmsgs msgs)

-- don't expect it to fail
runGWMonadNoFail :: GWMonad a -> GenState -> IO a
runGWMonadNoFail f s =
    do (_, res) <- runGWMonadGetNoFail f s
       return res

-- get the state as well,
-- and we don't expect it to fail
runGWMonadGetNoFail :: GWMonad a -> GenState -> IO (GenState, a)
runGWMonadGetNoFail f s =
    do result <- runExceptT ((runStateT f) s)
       case result of
         Right (res, s2) -> return (s2, res)
         Left msgs -> internalError ("runGWMonadGetNoFail: " ++
                                     ppReadable (errmsgs msgs))

-- for converting from ErrorMonad
convEM :: ErrorMonad a -> GWMonad a
convEM m = do
  errh <- gets errHandle
  lift $ lift $ convErrorMonadToIO errh m

getFlags :: GWMonad Flags
getFlags = gets flags

getSymTab :: GWMonad SymTab
getSymTab = do s <- get
               return (symtable s)

bad :: EMsg -> GWMonad a
bad msg = throwError (EMsgs [msg])

bads :: [EMsg] -> GWMonad a
bads msgs = throwError (EMsgs msgs)

-- ====================

-- Field types   can these be nested ?
-- Yes fields can be other interfaces
data FInf = FInf
 {
   name :: Id,
   arg_types :: [CType],
   ret_type :: CType,
   -- a list of any user-provided names (empty if no names)
   arg_names :: [Id]
 }
 deriving (Eq, Show)

instance PPrint FInf where
    pPrint d p finf =
        text "FInf: " <+>
        ppId d (name finf) <+>
        pPrint d p (arg_types finf) <+>
        pPrint d p (ret_type finf) <+>
        pPrint d p (arg_names finf)

instance (HasPosition) FInf where
  getPosition finf = getPosition (name finf)

-- ====================

type DefFun = Bool -> VWireInfo -> VSchedInfo -> VPathInfo ->
              [VPort] -> SymTab -> [VFieldInfo] -> [Id] ->
              IO CDefn

data WrapInfo = WrapInfo
 {
   mod_nm      :: Id,           -- module name for synthesis eg mkTest
   orig_cqt    :: CQType,       -- cqtype of original module
   wrapped_mod :: Id,           -- Id of wrapped module eg.  mkTest_
   wi_prags    :: [PProp],
   wi_field_info :: [(Id, [IfcPragma])], -- elaboration metadata, not serialized
   deffun      :: DefFun
 }

instance PPrint WrapInfo where
    pPrint d p wi =
       text "WrapInfo:" <+> ppId d (mod_nm wi) <> text " :: " <>
       pPrint d 0 (orig_cqt wi) <+> text "wrapped module:" <+>
       ppId d (wrapped_mod wi) $+$
       text " PProps: " <>
       sepList (map (pPrint d p) (wi_prags wi)) (text ",")

-- ====================

-- Source fields and module attributes used to collect boundary metadata.
-- The key distinguishes interfaces with different synthesis attributes;
-- it is not the name of a generated type declaration.

data IfcTRec = IfcTRec
  {
    rec_id        :: Id,        -- key for the boundary metadata
    rec_rootid    :: Id,        -- type constructor of the original type
    rec_pprops    :: [PProp],   -- module properties which apply to the ifc
    rec_finfs     :: [FInf]     -- fields of the original ifc
  }
  deriving (Eq, Show)

instance PPrint IfcTRec where
    pPrint d p r =
        text "IfcTRec:" <+> ppId d (rec_id r) <+> equals <+>
        braces (
                text "rootid:" <> pPrint d p (rec_rootid r) $+$
                text "pprops:" <> pPrint d p (rec_pprops r) $+$
                pPrint d p (rec_finfs r)
               )

-- ====================

-- Flattened field metadata for port validation and synthesis attributes.
-- These fields are not inserted into the package as an interface declaration.

data GeneratedIfc = GeneratedIfc
    {
     genifc_id     :: Id,
     genifc_fields :: [CField],
     genifc_pprops :: [PProp]
    }
  deriving (Show)

instance PPrint GeneratedIfc where
    pPrint d p gifc =
        text "GeneratedIfc: " <+> ppId d (genifc_id gifc) <+> equals <+>
             braces (
                      pPrint d p (genifc_fields gifc)
                      $+$ pPrint d p (genifc_pprops gifc)
                    )

-- ====================

-- Info about modules to be synthesized
-- This data is passed between the stages of "genWrapE"

type ModDefInfo = (CDef, CQType,  -- the new "<mod>_" def and it's type
                   [(Id, CType, ArgInfo)], -- mod arg info with user names
                   [PProp])  -- pprops on the module

-- ====================
-- Top-level entry: genWrap

-- creates wrappers for separately compiled modules: wraps pack/unpack around method
-- arguments and results; surfaces method ready signals
-- Pragma are module specific
genWrap :: ErrorHandle -> Flags -> [String] -> Bool -> CPackage -> SymTab ->
           IO (CPackage, [WrapInfo])
genWrap errh flgs genNames generating cpack symt = do
    runGWMonad (genWrapE generating ppmap cpack) origstate
 where
    origstate = GenState errh symt flgs
    pragmas = (extractPragmas cpack)
    ppmap = makePPropMap genNames pragmas
    --
    extractPragmas :: CPackage -> [Pragma]
    extractPragmas (CPackage _ _ _ _ _ ds _) = [ p | CPragma p <- ds ]

-- ====================

-- Given a list of names to generate from the command line (with -g flag)
-- and the pragmas from the CPackage, generate a mapping from def names
-- to their associated pprops (removing any duplicate pprops)
makePPropMap :: [String] -> [Pragma] -> M.Map Id [PProp]
makePPropMap genNames cpragmas =
    let
        -- We avoid adding cmd-line pragmas that duplicate source pragmas,
        -- but we don't "nub" the source pragmas and we don't use "union"
        -- to construct the map, because we want to preserve the order of
        -- "doc" attributes, which don't have to be unique, either.
        cmd_line_pragmas = makeCmdLineGenPragmas genNames
        all_pragmas = (cmd_line_pragmas \\ cpragmas) ++ cpragmas
        pairs = [ (i, pps) | (Pproperties i pps) <- all_pragmas ]
    in  M.fromListWith (++) pairs

makeCmdLineGenPragmas :: [String] -> [Pragma]
makeCmdLineGenPragmas genNames =
    let
        -- Turn a string into a qualitifed id with cmdPosition
        mId :: String  -> Id
        mId s =
            case span (/= '.') s of
                (ms, '.':is) ->
                    let fms = mkFString ms
                        fis = mkFString is
                    in  mkQId cmdPosition fms fis
                _ ->
                    let fs = mkFString s
                    in  mkId cmdPosition fs
    in  [ Pproperties (mId i)  [PPverilog] | i <- genNames ]

isGenDef :: M.Map Id [PProp] -> Id -> Bool
isGenDef pmap i =
    case (M.lookup i pmap) of
      Just pps -> PPverilog `elem` pps
      Nothing -> False

-- ====================

-- Synthesis boundaries call the wrapping class and retain their declaration
-- metadata for elaboration.  Explicit BVI imports use a separate wrapping style.

genWrapE :: Bool -> M.Map Id [PProp] -> CPackage ->
            GWMonad (CPackage, [WrapInfo])
genWrapE generating ppmap cpack@(CPackage packageId exps imps impsigs fixs ds includes)  =
    do
       -- Fill BVI's style and descriptor before any attribute-driven context
       -- reduction can inspect the parser's provisional raw type annotation.
       (defsWithVector, _) <- foldM fixVerilogIfc ([],[]) ds
       -- information on all modules which are being generated
       -- (this checks the pragmas on all defs which have pragmas, though,
       -- and errors if any do not meet sanity checks)
       -- moduledefs :: [(CDef, CQType, [(Id, CType, ArgInfo)], [PProp])]
       moduledefs <- concatMapM (getDef generating $ reverse defsWithVector) (M.toList ppmap)
       --traceM( "genWrapE:  moduledefs: " ++ ppReadable moduledefs )

       -- build the Interface record
       -- this structure should be used throughout
       -- ifcTypeRecords :: [IfcTRec]
       ifcTypeRecords <- mapM procDef moduledefs
       let trs0 = concat ifcTypeRecords
       let trs = nub trs0

       boundaryInfos <- mapM genTDef trs
       let genIfcMap = M.fromList $ map (\g -> (genifc_id g, g)) boundaryInfos
       newModule_s <- mapM (mkNewModDef genIfcMap) moduledefs

       let fixedDefs = map fixDef defsWithVector

       let finalDefs = reverse fixedDefs ++ newModule_s
       gens <- mapM (genWrapInfo boundaryInfos) moduledefs

       return (
           CPackage packageId exps imps impsigs fixs finalDefs includes,
           gens
          )
    where
       -- removes the [CClause] from the input, and replaces it with singleton list
       fixDef :: CDefn -> CDefn
       fixDef (CValueSign (CDef defid t e)) | generating &&
                                              isGenDef ppmap defid =
           CValueSign (CDef defid t [CClause [] [] (CVar defid)])
       fixDef d = d


-- --------------------
-- genWrapE toplevel: procDef

-- This takes the ModDefInfo (computed by getDef) for each module to be
-- synthesized and collects its interface fields and attributes.
-- It returns a list because it used to support
-- interface arguments, so it would return the provided interface plus
-- any given interfaces.  (Probably only returns one element lists now.)

-- XXX note that it doesn't do a full recursive sweep of the interface

-- Check all definition types and put them in a convenient form
-- XXX the work here is forgotten and repeated many times throughout GenWrap
procDef :: ModDefInfo -> GWMonad [IfcTRec]
procDef (def@(CDef _ t _), _, _, pps) =
    do --traceM( "procdef : def= " ++ ppReadable def ) ;
       ts <- chkType def t
       res <- mapM (makeIfcTRec pps) ts
       return res
procDef _ = internalError "genWrapE::procDef not CDef"

-- check that the type being generated is legal (uses the def to get a position)
chkType :: CDef -> CQType -> GWMonad [CType]
chkType def ty =
 do
   let defId = getDName def
   symt <- getSymTab
   case convCQType symt ty of
     Left msg -> bad msg
     Right ((_:_) :=> _) ->
        let ctx = if isClassic() then "context" else "proviso"
            msg = "It has a " ++ ctx ++ "."
        in  bad (getPosition def, EBadIfcType (Just $ pfpString defId) msg)
     Right ([]    :=> t) ->
      do
        if not (null (tv t))
          then let msg = "Its interface is polymorphic."
               in  bad (getPosition def, EBadIfcType (Just $ pfpString defId) msg)
          else
           do
             --traceM ("chkType: " ++ pfpReadable (t, getArrows t))
             case getArrows t of
               (args, TAp (TCon (TyCon tm _ _)) tr) | tm == idModule -> do
                  pint <- getPolyInterface tr
                  case pint of
                    -- XXX: should do better with the exact field position
                    (f:_) -> let msg = "Its interface has a polymorphic field " ++
                                       quote (pfpString f) ++ "."
                             in  bad (getPosition def,
                                      EBadIfcType (Just $ pfpString defId) msg)
                    [] ->
                     do
                       ----traceM ("chkType 2: " ++ pfpReadable (chkInterface tr))
                       cint <- chkInterface tr
                       case cint of
                         Just _ ->
                          do
                            trs <- getInterfaces args
                            return (tr : trs)
                         Nothing -> let msg = "The type " ++
                                              quote (pfpString t) ++
                                              " is not an interface."
                                    in  bad (getPosition def,
                                             EBadIfcType (Just $ pfpString defId) msg)
               _ -> let msg = "It is not a module."
                    in  bad (getPosition def,
                             EBadIfcType (Just $ pfpString defId) msg)

-- Return a list of names of any fields which are polymorphic,
-- if the given type is an interface; otherwise return an empty list.
getPolyInterface :: Type -> GWMonad [Id]
getPolyInterface t = do
  t' <- expandSynSym t
  case (leftCon t') of
    Nothing -> return []
    Just i -> do
      let ts = tyConArgs t'
      symt <- getSymTab
      case findType symt i of
        Just (TypeInfo (Just ti) _ _ (TIstruct ss fs) _) | isIfc ss -> do
          res <- filterM (isPolyFieldType ti ts) fs
          return res
        _ -> return []


-- Build the Interface record
makeIfcTRec :: [PProp] -> Type -> GWMonad IfcTRec
makeIfcTRec pps t =
    do res <- chkInterface t
       let pps' = filter isRelevantProp pps
       case res of
         Nothing -> internalError ("makeIfcTRec: not interface - " ++ show t)
         Just (_, _, fts) ->
           do -- name based on module attributes not ifc attributes
              flat_t <- flatTypeId pps' t
              raw <- rawTypeId t
              let rootId = addInternalProp raw
              --
              return IfcTRec { rec_id        = flat_t,
                               rec_rootid    = rootId,
                               rec_pprops    = pps',
                               rec_finfs     = fts }

-- pulls out all PProps relevant to GenWrap
isRelevantProp :: PProp -> Bool
isRelevantProp (PPalwaysReady {})      = True
isRelevantProp (PPalwaysEnabled {})    = True
isRelevantProp (PPenabledWhenReady {}) = True
isRelevantProp _                       = False


-- --------------------
-- genWrapE toplevel: getDef

-- For each definition which has PProps attached to it, this checks
-- the properties, and then for those which are to be generated
-- (that the property PPverilog) it creates a record of information:
-- * The def (possibly updated with a new type if it is to be generated,
--   because polymorphic modules get tied to Module)
-- * The original CQType (before tying down the polymorphic module type)
--   for use on the wrapper around the Verilog
-- * Triples of the names of the module arguments and their types,
--   along with a breakdown of the argument (for Vector arguments)
-- * The list of PProps
getDef :: Bool -> [CDefn] -> (Id, [PProp]) -> GWMonad [ModDefInfo]
getDef generating ds (i, pps) =
 do
   flgs <- getFlags
   symt <- getSymTab
   let defs = [ d | d@(CValueSign (CDef i' _ _)) <- ds, unQualId i == unQualId i' ]
   case defs of
     [d] -> do                  -- single element list of the module definition
        let red = cCtxReduceDef flgs symt d
        case red of
          Left msgs -> bads msgs
          Right (CValueSign (CDef defid cqt@(CQType ctx t) e)) ->
            do
              t_ext <- expandSynSym t
              let ext = (CQType ctx t_ext)
              let vts = getDefArgs e t_ext
              vtis <- mapM expandArg vts
              -- attributes for the default clock/reset apply to arguments
              -- designated with the default_clock/default_reset attributes
              let pps' = applyDefaultArgAttrs (renamePProps vtis pps)
              -- check pragmas for every def
              convEM $ checkModuleArgPragmas (getPosition i) pps pps' vtis
              -- only return info for the defs which are generated
              if (generating && (PPverilog `elem` pps'))
                then do newtyp <- fixupPolyModType ext
                        return [(CDef defid newtyp e,
                                 cqt, vtis, pps')]
                else return []
          Right _ -> internalError ("genWrapE::getDef unexpected CtxReduce def" ++ show d )
     _ -> bad (getIdPosition i, EUnboundVar (pfpString i))
 where
   expandArg :: (Id, CType) -> GWMonad (Id, CType, ArgInfo)
   expandArg (v_orig, vt_orig) = do
     let expandInfo :: Id -> CType -> GWMonad ArgInfo
         expandInfo v vt = do
             isVector <- isVectorType vt
             case isVector of
                 Just (n,tVec, isListN) -> expandVecInfo v n tVec isListN
                 _ -> return (Simple v vt)
         expandVecInfo :: Id -> Integer -> Type -> Bool -> GWMonad ArgInfo
         expandVecInfo v sz tVec isListN = do
             -- name the ports p_0, p_1, ...
             let mkSuffix n = concatFString [fsUnderscore, mkFString (itos n)]
                 mkNewName n = mkIdPost v (mkSuffix n)
                 new_vs = map mkNewName [0..(sz-1)]
             -- see if the underlying type is itself a vector
             sub_infos <- mapM (`expandInfo` tVec) new_vs
             return (Vector isListN v tVec sub_infos)
     info <- expandInfo v_orig vt_orig
     return (v_orig, vt_orig, info)


-- force IsModule to be Module when we are generating
fixupPolyModType :: CQType -> GWMonad CQType
fixupPolyModType ty =
 do
   symt <- getSymTab
   case convCQType symt ty of
     Left msg -> return ty -- let other processing handle the error
     -- note that any other provisos will be an error while generating anyway
     Right ([pp] :=> t) ->
       case (removePredPositions pp) of
         (IsIn cls [TVar tv1, TVar tv2]) | (typeclassId (Pred.name cls) == idIsModule)
           -> do
                let modSubst = mkSubst [(tv1, tModule), (tv2, tId)]
                return (CQType [] (apSub modSubst t))
         _ -> return ty
     _ -> return ty


-- --------------------
-- genWrapE toplevel: genWrapInfo

-- This takes the ModDefInfo (computed by getDef) for each module to be
-- synthesized and generates the WrapInfo structure which is returned
-- from GenWrap (info about the module and a DefFun continuation which is
-- used at the end of synthesis to make the final wrapper).
-- It also takes the collected metadata for the interfaces being synthesized.

-- User pragmas are checked that any names match the actual flattened ifc.
-- Pragmas on the ifc type declaration are added to the module's list of
-- pragmas (returned in the WrapInfo).

genWrapInfo :: [GeneratedIfc] -> ModDefInfo -> GWMonad WrapInfo
genWrapInfo genifcs (d@(CDef modName oqt@(CQType _ t) cls), cqt, _, pps) =
 do
   ifcName_ <- ifcNameFromMod pps t
   -- Look up the boundary metadata for this module.
   let mifc = find (qualEq ifcName_ . genifc_id) genifcs
       err = internalError ( "genWrapInfo:: cannot find interface " ++
                             ppReadable ifcName_ ++ ppReadable genifcs )
       ifc = maybe err id mifc
   let iprops = genifc_pprops ifc
   chkUserPragmas pps ifc
   -- XXX the new module name should be already created in ModDefInfo,
   -- XXX so that it does not have to be created both here and in mkNewModDef
   let newModName = modIdRename pps modName
   --traceM ("genWrapInfo" ++ ppReadable ifcName_ ++ ppReadable newModName)
   --traceM ("genWrapInfo" ++ ppReadable genifcs)
   namefun <- mkDef iprops pps d cqt -- pass down the pragma for the wrapper gen state
   return WrapInfo { mod_nm      = modName,
                     orig_cqt    = oqt,
                     wrapped_mod = newModName,
                     wi_prags    = (pps ++ iprops),-- combine pragmas
                     wi_field_info = [(cf_name f, fromMaybe [] (cf_pragmas f))
                                      | f <- genifc_fields ifc],
                     deffun      = namefun }
 where
   --Get interface name from module XXX make disappear
   -- XXX this was already done, but we did not keep the name !
   ifcNameFromMod :: [PProp] -> Type -> GWMonad Id
   ifcNameFromMod localpps localt = flatTypeIdQual localpps tr
     where
       tr = case getArrows localt of
              (_, TAp _ r) -> r
              _ -> internalError "GenWrap.genWrapInfo ifcNameFromMod: tr"
genWrapInfo _ (def,_,_,_) =
    internalError( "genWrapInfo: unexpected def: " ++ ppReadable def )


-- --------------------
-- genWrapE toplevel: fixVerilogIfc

-- The BVI parser inserts the generic unwrap call.  Fill its declaration
-- metadata, style schema, and raw structural type, and retain the existing
-- port-type recording and validation.  Raw `module verilog` is left alone.
-- The matched syntax is marked (*A*) in CVParserImperative.
fixVerilogIfc :: ([CDefn], [IfcTRec]) -> CDefn -> GWMonad ([CDefn], [IfcTRec])
fixVerilogIfc (ds,ts) x@(CValueSign (CDef n (CQType prs tid) [CClause ps qs (Cmodule pos body)])) =
 do
    (newBody, tyrs, prs') <- foldM (fixCModuleVerilog n) ([],[],prs) body
    let d'= (CValueSign (CDef n (CQType prs' tid) [CClause ps qs (Cmodule pos (reverse newBody))]))
    --
    return (d':ds, tyrs++ts)

fixVerilogIfc (ds,ts) d = return ((d:ds),ts)


-- Fill one parser-generated BVI boundary.
fixCModuleVerilog :: Id -> ([CMStmt], [IfcTRec], [CPred]) -> CMStmt -> GWMonad ([CMStmt], [IfcTRec], [CPred])
fixCModuleVerilog n (ss,ts,ps)
       bdy@(CMStmt (CSBindT p@(CPVar mName) mi pprops cq@(CQType qs rawTy)
                            def@(CApply _ [CmoduleVerilog vname _ _ _ aes fs _ _])))
  | (TCon (TyCon wrap _ _), [TCon (TyCon style _ _), ty]) <- splitTAp rawTy,
    wrap == wrapId "WrappedIfc", style == wrapId "BVIWrap" = do
   let mStmtSPT = savePortTypeStmt (CVar id_x)
       mStmtSPTO = savePortTypeOfStmt (CVar id_x)
   let saveArgTypes (Port vp _ _, e) = [mStmtSPTO vp e]
       saveArgTypes (InoutArg vn _ _,
                     CApply (CVar f) [e]) | f == idPrimInoutCast0 =
         let vp = (vn,[])
         in  [mStmtSPTO vp e]
       saveArgTypes _ = []
   let checkArgType fid t = do
         t' <- expandSynSym t
         -- These special types used to survive bitification unchanged (or as
         -- Inout_) and be rejected by the primitive module typechecker.  Reject
         -- them before an unsatisfied Bits requirement can obscure that error
         -- by leaving the associated wrapper type unreduced.
         let special = t' == tClock || t' == tReset ||
                       leftCon t' `elem` map Just
                         [idInout, idActionValue, idActionValue_]
             modName = case vname of
                         CLit (CLiteral _ (LString s)) -> s
                         _ -> pfpString vname
         when special $ bad (getPosition fid,
           EForeignModBadArgType (pfpString t') modName
             (ppReadable (getPosition vname)))
   let saveFieldTypes finf (Method { vf_name = fid,
                                     vf_inputs = inps,
                                     vf_outputs = outs }) = do
         mapM_ (checkArgType fid) (arg_types finf)
         let rt = ret_type finf
         isAV <- isActionValue rt
         output_type <- if isAV then getAVType "fixCModVer" rt else return rt
         let output_stmt = map ((flip mStmtSPT) rt) outs
         -- we let the type-checker error on mismatches
         return (output_stmt ++ (zipWith mStmtSPT (concat inps) (arg_types finf)))
       saveFieldTypes finf (Inout { vf_inout = vn }) = do
         let rt = ret_type finf
             vp = (vn,[])
         return [mStmtSPT vp rt]
       saveFieldTypes _ _ = return []

   cint <- chkInterface ty

   case cint of
    Nothing -> bad (getPosition cq, EBadForeignIfcType (pfpString ty))
    Just (i, k, fts) ->
     do
       -- BVI validation and port reflection use the declaration's flattened names.
       let flattenFInfs prefixes (FInf f as r aIds) = do
               mifc <- chkInterface r
               case (mifc, as) of
                 (Just (_, _, finfs), []) -> do
                     newPrefixes <- extendPrefixes prefixes [] r f
                     meths <- mapM (flattenFInfs newPrefixes) finfs
                     return (concat meths)
                 _ -> -- if Vector of ifcs was supported,
                      -- we'd have to handle it here
                      return [FInf (binId prefixes f) as r aIds]
       flat_fts <- concatMapM (flattenFInfs noPrefixes) fts

       let finf_map = M.fromList [(unQualId (name finf), finf) | finf <- flat_fts]
       let save_f f =
               let fname = vf_name f
               in  -- imports don't have RDY methods
                   if (isRdyId fname)
                   then return []
                   else case (M.lookup fname finf_map) of
                          Just finf -> saveFieldTypes finf f
                          Nothing -> bad (getIdPosition fname,
                                          EForeignModNotField
                                              (pfpString i) (pfpString fname))
       save_f_stmtss <- mapM save_f fs
       let save_a_stmtss = map saveArgTypes aes
       let save_stmts = concat (save_a_stmtss ++ save_f_stmtss)
       let ftypes = nub (concatMap (\ (FInf fid as r aIds) -> (r:as)) flat_fts)
       (_, ps') <- foldM addBVIBitsProviso (0, []) (reverse ftypes)
       let no_rdys = map (\v -> [vf_name v]) (filter (not . (hasReadyFN fs)) fs)
           rdyPPs = [PPalwaysReady no_rdys | not (null no_rdys)]
       descriptor <- mkWrapDescriptor BVIReady rdyPPs [] ty
       schema <- bviWrapSchema ty
       let styleTy = TAp (cTCon $ wrapId "BVIWrap") schema
           wrappedTy = cTApplys (cTCon $ wrapId "WrappedIfc") [styleTy, ty]
           rawStmt = CMStmt $ CSBindT p mi pprops (CQType qs wrappedTy) def
           styleDef = CLValueSign (CDef (bviWrapStyleId mName)
                         (CQType [] styleTy) [CClause [] [] $ wrapStyle "BVIWrap"]) []
           descriptorDef = CLValue (bviWrapInfoId mName)
                             [CClause [] [] descriptor] []
           descriptorStmt = CMStmt $ CSletseq [styleDef, descriptorDef]
           ss'' = save_stmts ++ (saveNameStmt mName id_x : rawStmt : descriptorStmt : ss)
       return (ss'', ts, ps' ++ ps)
fixCModuleVerilog n (ss, ts, ps) s = return (s:ss, ts, ps)

-- Preserve the original declaration-time classification: known interfaces
-- are traversed, and an opaque type variable is an ordinary Bits value.
-- The style parameter records that choice before instantiation, including
-- known arrows and ActionValue results, without requiring Generic evidence
-- for a polymorphic method value.
bviWrapSchema :: CType -> GWMonad CType
bviWrapSchema ty = do
    mi <- chkInterface ty
    case mi of
      Just (_, _, fs) -> do
        children <- mapM fieldSchema fs
        return $ TAp (cTCon $ wrapId "WrapInterfaceSchema") $
                   tMkTuple (getPosition ty) children
      Nothing -> internalError ("bviWrapSchema: " ++ ppReadable ty)
  where
    fieldSchema (FInf _ as r _) = do
      mi <- chkInterface r
      case (mi, as) of
        (Just _, []) -> bviWrapSchema r
        _ -> do
          clock <- isClockType r
          reset <- isResetType r
          inout <- isInoutType r
          action <- isPrimAction r
          actionValue <- isActionValue r
          let result = cTCon $ wrapId $
                if clock || reset || isJust inout then "WrapWireKind"
                else if action then "WrapPrimActionSchema"
                else if actionValue then "WrapActionValueSchema"
                else "WrapValueKind"
          return $ foldr (\_ rest -> TAp (cTCon $ wrapId "WrapFunctionSchema") rest)
                         result as


-- Keep the original BVI module's Bits constraints for polymorphic ports.
-- Wrapping itself resolves the corresponding packed types through the class.
addBVIBitsProviso :: (Int, [CPred]) -> CType -> GWMonad (Int, [CPred])
addBVIBitsProviso (n, ctx) ty = do
    av <- isActionValue ty
    inout <- isInoutType ty
    bitsType <- if av then getAVType "addBVIBitsProviso" ty
                else return $ fromMaybe ty inout
    if av || not (null $ tv bitsType)
      then let size = cTVarNum (enumId "sn" noPosition n)
           in return (n+1, CPred (CTypeclass idBits) [bitsType, size] : ctx)
      else return (n, ctx)


-- --------------------
-- genWrapE toplevel: genTDef

-- Collect flattened field metadata for port validation and pragmas.
-- No nominal interface declaration is generated.
genTDef :: IfcTRec -> GWMonad GeneratedIfc
genTDef trec =
 do
   (fields,newprops) <- genIfc trec
   return GeneratedIfc {
      genifc_id = rec_id trec,
      genifc_fields = fields,
      genifc_pprops = newprops }

-- Traverse the interface declaration for flattened port metadata.
-- Interface properties must still be propagated to the synthesis boundary.
genIfc :: IfcTRec -> GWMonad ([CField], [PProp])
genIfc trec =
    do let fts    = rec_finfs trec
           rootId = rec_rootid trec
       -- lookup interface pragmas from symt and pass them in.
       iprags <- getInterfacePrags rootId
       let prefix = noPrefixes { ifcp_pragmas = iprags }
       fieldprops <- mapM (genIfcField trec rootId prefix) fts
       let (fields,ppropss) = unzip fieldprops
       let newprops = concat ppropss
       --
       return (concat fields, newprops)

-- Generate the ifc fields
-- And any new AR AE pragmas from the methods
-- Note: The FInf are assumed to have unqualified field names.
genIfcField :: IfcTRec -> Id -> IfcPrefixes -> FInf -> GWMonad ([CField], [PProp] )
genIfcField trec ifcIdIn prefixes (FInf fieldIdQ argtypes rettype _) =
    do
       -- get relevant pragmas for this interface and field
       --traceM( "prefixes for: " ++ ppReadable fieldId ++ " = " ++ ppReadable prefixes )
       -- looking up the pragmas requires the qualified name
       ciPrags <- getInterfaceFieldPrags ifcIdIn fieldIdQ
       -- but for the rest of the work, we only need the unqualified name
       let fieldId = unQualId fieldIdQ
       --traceM( "prags for field are: " ++ ppReadable fieldId ++ " = " ++ ppReadable ciPrags)
       --
       mi <- chkInterface rettype
       case mi of
           Just (ifcId, k, fts) | null argtypes ->  -- recursive function this is an ifc of ifc
             do -- create new prefixes before creating new sub-interface fields
                newPrefixes <- extendPrefixes prefixes ciPrags rettype fieldId
                --
                -- if the field is an interface, use its interface Id to
                -- recurse and look up field information
                fieldsprops <- mapM (genIfcField trec ifcId newPrefixes ) fts
                let (fields,props) = unzip fieldsprops
                return ((concat fields), (concat props))
           _ ->               -- leaf function
             do
              isClock <- isClockType rettype
              isReset <- isResetType rettype
              isInout <- isInoutType rettype
              let isIot = isInout/=Nothing
              isPA <- isPrimAction rettype
              isVec <- if null argtypes then isVectorInterfaces rettype
                       else return Nothing
              case (isVec, argtypes) of
                (Just (n, tVec, isListN), []) ->
                    do -- Vectors of interfaces are interfaces
                       --traceM ("tVec:" ++ (show tVec))
                       let nums = map mkNumId [0..(n-1)]
                       newPrefixes <- extendPrefixes prefixes ciPrags rettype fieldId
                       let recurse num = genIfcField trec ifcIdIn newPrefixes  (FInf num [] tVec [])
                       fieldsprops <- mapM recurse nums
                       let (fields,props) = unzip fieldsprops
                       return (concat fields, concat props)
                _ -> do  -- ELSE NOT a Vec
                      let fnt = cTStr (fieldPathName prefixes fieldId) (getIdPosition fieldIdQ)
                      let v = cTVar $ head tmpTyVarIds
                      let ctx = CPred (CTypeclass idWrapField) [fnt, foldr arrow rettype argtypes, v]

                      let fi = binId prefixes fieldId
                      --
                      let (mprops, ifcPragmas) = gen prefixes ciPrags fieldId fi
                          gen | isClock   = genNewClockIfcPragmas
                              | isReset   = genNewResetIfcPragmas
                              | isIot     = genNewInoutIfcPragmas
                              | otherwise = genNewMethodIfcPragmas

                      let ifc_field = CField { cf_name = fi,
                                              cf_pragmas = Just ifcPragmas,
                                              cf_type = CQType [ctx] v,
                                              cf_orig_type = Just (foldr arrow rettype argtypes),
                                              cf_default = []
                                            }
                      --
                      -- the ready field
                      let rdy_field = if (isClock || isReset || isIot) then []
                                      else mkReadyField trec ifcPragmas ifcIdIn fieldId fi
                      --
                      --traceM( "ifc_fields is: " ++ ppReadable ifc_field)
                      return ((ifc_field : rdy_field), mprops )


-- create a RDY field, if requested
mkReadyField :: IfcTRec -> [IfcPragma] -> Id -> Id -> Id -> [CField]
mkReadyField trec ifcprags rootIfcId fieldId pathFieldId = if makingReady then [cfield] else []
    where  pps = rec_pprops trec
           readyId = mkRdyId pathFieldId -- why mkRdyId only to compare it later?
           makingReady = not ((isAlwaysRdy pps readyId) || (isAlwaysReadyIfc ifcprags) )
           --
           renamed = mkRdyId pathFieldId
           cfield = CField { cf_name = renamed,
                             cf_pragmas = Nothing,
                             cf_type = (CQType [] (TAp tBit (cTNum 1 (getPosition fieldId)))),
                             cf_orig_type = Nothing,
                             cf_default = []
                           }

-- --------------------
-- genWrapE toplevel: mkNewModDef

-- For a module def named <modname>, we generate the actual synthesis
-- boundary called "<modname>_" which has bitified arguments and wraps
-- around the actual module definition (lifted with liftM), unpacking
-- the values before applying the module def to them.

mkNewModDef :: M.Map Id GeneratedIfc -> ModDefInfo -> GWMonad CDefn
mkNewModDef genIfcMap (def@(CDef i (CQType _ t) dcls), cqt, vtis, vps) =
 do
   --traceM ("mkNewModDef: " ++ ppReadable def)
   -- XXX This could have been stored in the moduledef info
   -- XXX (note that the first half is the "ts" in "vtis")
   let tr = case getArrows t of
              (_, TAp _ r) -> r
              _ -> internalError "GenWrap.mkNewModDef: tr"
   cint <- chkInterface tr
   (ifcId, _, fts) <- case cint of
                        Just res -> return res
                        Nothing -> internalError "GenWrap.mkNewModDef: cint"

   tyId <- flatTypeId vps tr    -- id of the Ifc_
   let ty = tmod (wrappedIfcType "SynthWrap" tr)

   -- sanity check the port names (clashes, bad identifiers, etc)
   let genifc = case (M.lookup tyId genIfcMap) of
                  Just res -> res
                  Nothing -> internalError ("mkNewModDef: can't find ifc: " ++
                                            ppReadable tyId)
       cfields = genifc_fields genifc
   -- XXX reverse the fields just to match the behvaior of the previous
   -- XXX compiler, which accumulated with fold rather than map
   ftps <- mapM collectIfcInfo (reverse cfields)
   -- get back the arg port to type mapping, for recording
   flgs <- getFlags
   arg_pts <- convEM $ checkModulePortNames flgs (getPosition i) vps vtis ftps
   let arg_infos = thd $ unzip3 vtis
       (vs, ts) = unzip $ concatMap extractVTPairs arg_infos

   -- based on the types of the arguments (and given their names)
   -- generate the expressions that convert from the bitified/blasted ports
   -- to the original arguments (of the original type) of the inner module
   exprs <- mapM convVar arg_infos

   -- make the new def
   let -- the call to the module body (as written by the user) with its
       -- un-bitified and vectorized arguments
       mexp = cApply 10 (CVar i) exprs
       -- statements to record the port-types of module arguments
       -- (for the current module)
       arg_sptStmts = map (uncurry saveTopModPortTypeStmt) arg_pts

   -- interface save-port-type statements
   wrapInfo <- mkWrapDescriptor SynthReady vps [] tr
   let ifc_sptStmts = [CSExpr Nothing $ cVApply idLiftModule
                        [wrapApply "saveWrapIfcPortTypes"
                         [wrapStyle "SynthWrap", mkIfcProxyExpr tr,
                          mkMaybe Nothing, wrapInfo]]]

   let sptStmts = arg_sptStmts ++ ifc_sptStmts
       -- a do-block around the module body, so that we can include the
       -- save-port-type statements
       lexp = if not (null sptStmts)
              then Cdo False (sptStmts ++ [CSExpr Nothing mexp])
              else mexp
       -- liftM of the do-block
       to  = cVApply idLiftM
               [wrapApply "toWrapIfc" [wrapStyle "SynthWrap", wrapInfo], lexp]
       -- let-define the user's module around the expr we just created
       lte = Cletseq [CLValueSign def []] to
       -- the clause for the new def we're creating
       cls = CClause (map CPVar vs) [] lte

   -- produce the context and new base-type for the new def
   let tvs = map cTVarNum (take (length ts) tmpTyVarIds)
   -- reverse because we want to tack on the innermost type first
   -- and we are folding from the left
   (ctx, ty') <- foldM mkCtx ([], ty) (reverse (zip ts tvs))

   --traceM (" mkNewModDef: " ++ ppReadable def )
   --traceM (" mkNewModDef: dcls: " ++ ppReadable dcls)
   --traceM (" mkNewModDef: exprs: " ++ ppReadable exprs)
   --traceM ( "mkNewModDef: cls:" ++  ppReadable cls )
   let result =  (CValueSign (CDef (modIdRename vps i) (CQType ctx ty') [cls]))
   --traceM( "mkNewModDef tyId: " ++ ppReadable tyId )
   --traceM( "mkNewModDef vs: " ++ ppReadable vs )
   --traceM( "mkNewModDef res: " ++ ppReadable result )
   --traceM( "mkNewModDef def" ++ ppReadable def )
   return result
 where

   collectIfcInfo :: CField -> GWMonad  (Id, CType, [IfcPragma])
   collectIfcInfo (CField {cf_name = fid, cf_pragmas = ps,
                           cf_type = qtype@(CQType xpred ftype) }) = do
     -- XXX is this needed?
     ftype' <- expandSynSym ftype
     return (fid, ftype, fromMaybe [] ps)

   convVar :: ArgInfo -> GWMonad CExpr
   convVar (Simple v lt) =
     do
       cint <- chkInterface lt
       case cint of
         Just (ti, _, fts) -> -- interface arguments are not supported
                              bad (getPosition v,
                                   EInterfaceArg (pfpString i))
         Nothing ->
          do
            isInout <- isInoutType lt
            case isInout of
             Just _ -> return (CApply ePrimInoutUncast0 [CVar v])
             _ ->
              do
               isClock <- isClockType lt
               isReset <- isResetType lt
               isParam <- isParamType lt
               if (isClock || isReset || isParam)
                 then return (CVar v)
                 else return (CApply eUnpack [CVar v])
   convVar (Vector isListN _ _ sub_ports) = do
       -- get the expression for each port
       -- (which may split the ports further)
       elem_es <- mapM convVar sub_ports
       return $ cToVector isListN elem_es

   mkCtx :: ([CPred], Type) -> (Type, Type) -> GWMonad ([CPred], Type)
   mkCtx (ctx, rt) (lt, v) =
    do
      -- XXX interface argument remnant
      isgIfc <- isGoodInterface lt
      if isgIfc
        then do
          ft <- flatTypeId [] lt
          return (ctx, cTCon ft `fn` rt)
        else do
          isInout <- isInoutType lt
          case isInout of
            Just tt -> return ((CPred (CTypeclass idBits) [tt, v]):ctx,
                               TAp tInout_ v `fn` rt)
            _ -> do
                  isClock <- isClockType lt
                  isReset <- isResetType lt
                  isParam <- isParamType lt
                  if (isClock || isReset || isParam)
                     then return (ctx, lt `fn` rt)
                     else return ((CPred (CTypeclass idBits) [lt, v]):ctx, TAp tBit v `fn` rt)

mkNewModDef _ (def,_,_,_) =
    internalError ("GenWrap::mkNewModDef unexpected definition " ++ show def )


-- ==============================
-- The wrapper continuation

-- This is the part of "genWrapInfo" which makes the DefFun,
-- a continuation function which does the final wrapper computation.

-- type DefFun = Bool -> VWireInfo -> VSchedInfo -> VPathInfo -> [VPort] -> SymTab -> [VFieldInfo] -> [Id] -> IO CDefn
-- XXX: alwaysEnabled is dropped and broken (not propagated to {inhigh})
mkDef :: [PProp] -> [PProp] -> CDef -> CQType -> GWMonad DefFun
mkDef iprags pps (CDef i (CQType _ qt) _) cqt = do
 st0 <- get
 return (\fmod wire_info sch pathinfo ips symt fields true_ifc_ids -> do
  let
      (ts, tr) = case getArrows qt of
                   (ats, TAp _ r) -> (ats, r)
                   _ -> internalError "GenWrap.mkDef: ts, tr"
      st1 = st0 { symtable = symt }
  let vs = take (length ts) tmpVarIds
  let
      -- return an expression for creating the arg (from the wrapper's args)
      -- and the type of the internal module's arg (for port-type saving)
      genArg :: CExpr -> Type -> GWMonad [(CExpr, CType)]
      genArg vexpr t =
       do
         --traceM( "In genArg: " ++ ppReadable v ++ " " ++ ppReadable t ) ;
         cint <- chkInterface t
         case cint of
           Just x -> -- interface arguments are not supported and should
                     -- already have generated an error
                     internalError ("mkDef: ifc arg: " ++ ppReadable (t,x))
           Nothing -> do
             isInout <- isInoutType t
             case isInout of
              Just _ -> return [(CApply ePrimInoutCast0 [vexpr], t)]
              _ ->
               do
                 isClock <- isClockType t
                 isReset <- isResetType t
                 isParam <- isParamType t
                 if (isClock || isReset || isParam)
                   then return [(vexpr,t)]
                   else do isVector <- isVectorType t
                           case isVector of
                             Just (n,tVec,_) -> genVecArg vexpr n tVec
                             _ -> return [(CApply ePack [vexpr], t)]
      genVecArg :: CExpr -> Integer -> Type -> GWMonad [(CExpr, CType)]
      genVecArg vexpr sz tVec = do
         -- make the expression for each port
         let nums = [0..(sz-1)]
             primselect = idPrimSelectFn noPosition
             lit k = CLit $ num_to_cliteral_at noPosition k
             selector n = cVApply primselect [posLiteral noPosition,
                                              vexpr, lit n]
             elem_sels = map selector nums
         elem_exprs <- mapM (`genArg` tVec) elem_sels
         return (concat elem_exprs)

  (st2, argss) <- runGWMonadGetNoFail (zipWithM genArg (map CVar vs) ts) st1
  let (arg_exprs, arg_ts) = unzip $ concat argss
      -- make the arg port-types, for saving in the module
      arg_pts = mkArgPortTypes wire_info arg_ts
  let
      fields' = filter (not . (isRdyToRemoveField (iprags ++ pps))) fields
      veriFields = (map (fixupVeriField (iprags ++ pps) ips) fields')
      vexp = xWrapperModuleVerilog
             fmod
             pps
             (CLit(CLiteral noPosition(LString( getIdBaseString i) )))
             wire_info
             arg_exprs
             veriFields
             sch
             pathinfo
      vlift = (cVApply idLiftModule [vexp])
  body <- runGWMonadNoFail
              (genStructuralFromBody arg_pts vlift (iprags ++ pps) true_ifc_ids tr)
              st2
  let cls = CClause (map CPVar vs) [] body
  return $ CValueSign (CDef i cqt [cls]))

mkDef _ _ def _ = internalError ("GenWrap::mkDef unexpected " ++ show def )


mkArgPortTypes :: VWireInfo -> [CType] -> [(VPort, CType)]
mkArgPortTypes wire_info arg_ts =
    let
        -- xWrapperModuleVerilog will check that the lists are the same length
        arg_its = zip (wArgs wire_info) arg_ts
        mkPortType (Port vp _ _, t) = [(vp,t)]
        mkPortType (InoutArg vn _ _, t) = [((vn,[]),t)]
        -- XXX fill in info for the rest?
        mkPortType (Param {}, _) = []
        mkPortType (ClockArg {}, _) = [] -- would need the osc and gate
        mkPortType (ResetArg {}, _) = []
    in
        concatMap mkPortType arg_its


-- add port properties to method ports in VModInfo
fixupVeriField :: [PProp] -> [(VName, [VeriPortProp])] -> VFieldInfo -> VFieldInfo
fixupVeriField _ _ f@(Clock { }) = f
fixupVeriField _ _ f@(Reset { }) = f
fixupVeriField _ _ f@(Inout { }) = f
fixupVeriField pps vportprops m@(Method { }) =
        m { vf_inputs = inputs',
            vf_outputs = outputs',
            vf_enable = enable'' }
  where inputs'  = map (map fixup) (vf_inputs m)
        outputs' = map fixup (vf_outputs m)
        enable'  = fmap fixup (vf_enable m)
        fixup    = fixupPort vportprops
        alwaysEnabled = isAlwaysEn pps (vf_name m)
        enable'' = fmap (addInHigh alwaysEnabled) enable'

-- tack on extra port properties at the port level
fixupPort :: [(VName, [VeriPortProp])] -> VPort -> VPort
fixupPort vportprops vport@(vname, base_props) =
  case (lookup vname vportprops) of
    Nothing -> vport
    Just props -> (vname, base_props ++ props)

-- add inhigh prop to alwaysEnabled port
addInHigh :: Bool -> VPort -> VPort
addInHigh False p = p
addInHigh True (n, props)  = (n, VPinhigh:props)

-- ====================
-- Interface checking and associated type analysis utilities

-- Checks if a given type is an interface and, if so, it returns:
--  * interface id
--  * kind
--  * _qualified_ field info
-- (The qualified info is needed when we generate struct selectors
--  under the covers, because these may not be available to the user
--  but are available to genwrap by their qualified name.)
-- XXX this is called too many times to gather the same information.
chkInterface :: Type -> GWMonad (Maybe (Id, Kind, [FInf]))
chkInterface t = do
  --traceM("chkInterface: before: " ++ ppReadable(t))
  symt <- getSymTab
  t' <- expandSynSym t
  --traceM("chkInterface: after: " ++ ppReadable(t'))
  case (leftCon t') of
   Nothing -> return Nothing
   Just i -> do
    let ts = tyConArgs t'
    case findType symt i of
     Just (TypeInfo (Just ti) k vs (TIstruct ss fs) _) | isIfc ss -> do
       anyPoly <- isAnyPolyFieldType ti ts fs
       if anyPoly
         then return Nothing
         else do
           finfs <- mapM (getFInf ti ts) fs
           return (Just (ti, k, finfs))
     _ -> return Nothing
 where
   getFInf ti ts f =
    do
      ft <- getFieldType ti ts f
      --traceM("getFInf: before: " ++ ppReadable(ft))
      ft_ext <- expandSynSym ft
      --traceM("getFInf: after: " ++ show(ft_ext))
      let (as, r) = case getArrows ft_ext of
                      (_:ats, rt) -> (ats, rt)
                      _ -> internalError "GenWrap.chkInterface: as, r"
      -- get any user-declared names for the arguments
      symt <- getSymTab
      let aIds = getMethodArgNames symt ti f
      return (FInf f as r aIds)

getFieldType :: Id -> [Type] -> Id -> GWMonad CType
getFieldType ifcType ts fieldId =
 do
   symt <- getSymTab
   case findField symt fieldId of
     Just xs -> do
       let finfos = [sc | (FieldInfo { fi_id = ti',
                                       fi_arity = n,
                                       fi_assump = (_ :>: sc) }) <- xs,
                          ifcType == ti']
       case finfos of
         [Forall ks t] -> do
           case inst ts t of
             [] :=> lt -> return lt
             p -> internalError ("getFieldType: 1 " ++ pfpReadable p)
         ys -> internalError ("getFieldType: 2 " ++ ppString (xs, (ifcType, fieldId), ys))
     Nothing -> internalError ("getFieldType: 3 " ++ ppString (ifcType, fieldId))

isAnyPolyFieldType :: Id -> [Type] -> [Id] -> GWMonad Bool
isAnyPolyFieldType ti ts [] = return False
isAnyPolyFieldType ti ts (c:cs) =
 do
   res <- isPolyFieldType ti ts c
   anyPoly <- isAnyPolyFieldType ti ts cs
   return (res || anyPoly)

isPolyFieldType :: Id -> [Type] -> Id -> GWMonad Bool
isPolyFieldType ti ts c =
 do
   symt <- getSymTab
   case findField symt c of
     Just xs -> do
       let finfos = [ sc | (FieldInfo { fi_id = ti',
                                        fi_arity = n,
                                        fi_assump = (_ :>: sc) }) <- xs,
                           ti == ti']
       case finfos of
         [Forall ks t] -> return (length ks /= length ts)
         ys -> internalError ("isPolyFieldType: 2 " ++ ppString (xs, (ti, c), ys))
     Nothing -> internalError ("isPolyFieldType: 3 " ++ ppString (ti, c))

isVectorInterfaces :: Type -> GWMonad (Maybe (Integer, Type, Bool))
isVectorInterfaces t =
 do
   t' <- expandSynSym t
   case vectorTypeParts t' of
     Just (sz, tVec, _) -> do
       res <- chkInterfaceVectorElementType tVec
       if res then do
         vec <- isVectorType t'
         case vec of
           Just _ -> return vec
           Nothing -> bad (getPosition sz, EBadIfcType Nothing $
             "The interface vector length could not be determined: " ++
             pfpString sz)
       else return Nothing
     _ -> return Nothing

-- is the supplied type a valid component of a vector of interfaces?
-- this includes Clocks, Resets, interfaces and vectors of such types
chkInterfaceVectorElementType :: Type -> GWMonad (Bool)
chkInterfaceVectorElementType t =
 do
   t' <- expandSynSym t
   case vectorTypeParts t' of
     Just (_,tVec,_) -> chkInterfaceVectorElementType tVec
     Nothing -> do
         isClock <- isClockType t
         isReset <- isResetType t
         if (isClock || isReset) then return True
          else do
            inout <- isInoutType t
            if (isJust inout) then return True
             else do ifc <- chkInterface t
                     return (isJust ifc)

-- --------------------

-- XXX use of "qualEq" is wrong, but "genFuncWrap" generates unqualified names

-- XXX get rid of "isPrimAction" because there should not be raw PrimActions
isPrimAction :: Type -> GWMonad Bool
isPrimAction t =
 do
   et <- expandSynSym t
   case et of
     TCon (TyCon i _ _) -> return (qualEq i idPrimAction)
     _ -> return False

isActionValue :: Type -> GWMonad Bool
isActionValue t =
 do
   et <- expandSynSym t
   case et of
        (TAp (TCon (TyCon i _ _)) _) -> return (qualEq i idActionValue)
        _ -> return False

getAVType :: String -> Type -> GWMonad (Type)
getAVType tag xt =
 do
   et <- expandSynSym xt
   case et of
     TAp (TCon (TyCon i _ _)) t | qualEq i idActionValue -> return t
     _ -> internalError (tag ++ " getAVType " ++ ppReadable xt)


-- is the type a valid Verilog parameter (but not a wire or bitifiable port)?
isParamType :: Type -> GWMonad Bool
isParamType t =
  do
    t' <- expandSynSym t
    return (t' == tReal || t' == tString || t' == tInteger)

isClockType :: Type -> GWMonad Bool
isClockType t =
 do
   t' <- expandSynSym t
   return   (t' == tClock)

isResetType :: Type -> GWMonad Bool
isResetType t =
 do
   t' <- expandSynSym t
   return   (t' == tReset)

isInoutType :: Type -> GWMonad (Maybe Type)
isInoutType t =
 do
   t' <- expandSynSym t
   case  t' of
            (TAp tt ta) | (tt == tInout) -> return (Just ta)
            _          -> return Nothing

isVectorType :: Type -> GWMonad (Maybe (Integer, Type, Bool))
isVectorType t =
 do
   t' <- expandSynSym t
   case vectorTypeParts t' of
     Just (sz, tVec, isListN) -> do
         -- Generic wrapping also traverses vectors whose concrete length is a
         -- type function.  Resolve it before constructing field metadata, so
         -- both traversals agree on the number of leaves (for example SizeOf).
         sz' <- case sz of
           TCon (TyNum {}) -> return sz
           _ -> do
             flgs <- getFlags
             symt <- getSymTab
             case TI.tiResult $ TI.runTI flgs False symt $ do
                    TI.addBoundTVs (tv sz)
                    normT sz
               of Left msgs -> bads msgs
                  Right res -> return res
         case sz' of
           TCon (TyNum n _) -> return $ Just (n,tVec,isListN)
           _ -> return Nothing
     _ -> return $ Nothing

-- Classification depends on the element type, even before the length can be
-- reduced.  An unresolved interface vector must not become a method leaf in
-- the descriptor while the wrapping library later traverses its elements.
vectorTypeParts :: Type -> Maybe (Type, Type, Bool)
vectorTypeParts t =
   case (leftCon t, tyConArgs t) of
     (Just con, [sz, elemTy])
       | qualEq con idListN || qualEq con idVector ->
           Just (sz, elemTy, qualEq con idListN)
     _ -> Nothing

-- --------------------

expandSynSym :: Type -> GWMonad Type
expandSynSym xt =
 do
   symt <- getSymTab
   return (expandSyn (updTypes symt xt))
 where
   updTypes r o@(TCon (TyCon i _ TIabstract)) =
     case findType r i of
       Just (TypeInfo (Just i') k _ ti _) -> TCon (TyCon i' (Just k) ti)
       Just (TypeInfo Nothing _ _ _ _) ->
         internalError ("genWrap: expandSynSym: unexpected numeric type:" ++
                        ppReadable i)
       Nothing -> o
   updTypes r (TAp f a) = TAp (updTypes r f) (updTypes r a)
   updTypes r t = t

-- --------------------

-- Create a qualified ID, based on the specific (interface) type generated
-- the first part of the name is based on the type of the interface, while the second is
-- based on the specific pragmas, AR/AE from the PProps
flatTypeIdQual :: [PProp] -> CType -> GWMonad Id
flatTypeIdQual pps sty =
 do
   esty <- expandSynSym sty
   let qual = case esty of
          TCon (TyCon i _ _) -> fst (getIdFStrings i);
          _                  -> fsEmpty
   let flat_fs = concatFString (map (getIdFString . unQualId) (flattenType esty))
   let flat_id = mkQId noPosition qual flat_fs
   return (ifcIdRename pps flat_id)
   --
 where
   flattenType :: CType -> [Id]
   flattenType (TVar (TyVar i _ _)) = [i] -- xxx aren't any at top level
   flattenType (TCon (TyCon i _ _)) = [i]
   flattenType (TCon (TyNum i _)) = [mkNumId i]
   flattenType (TAp t1 t2) = flattenType t1 ++ flattenType t2
   flattenType t = internalError ("GenWrap::flattenType: " ++ pfpReadable t)

flatTypeId :: [PProp] -> CType -> GWMonad Id
flatTypeId pps ty =
 do
   res <- flatTypeIdQual pps ty
   return (unQualId res)

-- extract the name ID  from the type
rawTypeId :: CType -> GWMonad Id
rawTypeId sty =
 do
   symt <- getSymTab
   esty <- expandSynSym sty
   let raw = extractId esty
   -- keep the qualified name
   return raw
 where
   extractId ltype = case ltype of
         TCon (TyCon i _ _) -> i
         TAp r _            -> extractId r
         _                  -> internalError ("GenWrap :: rawTypeId: " ++ show ltype )


-- ====================
-- Prefix utilities (for flattening interfaces)

-- structure to hold names, prefix strings, and pragmas passed from parent interfaces
-- down to specific fields.
data  IfcPrefixes = IfcPrefixes
    {
     ifcp_pathIdString   :: [FString], -- list of paths to root.  all methods will be renamed via this
     ifcp_renamePrefixes :: String,
     ifcp_pragmas        :: [IfcPragma]
    }

instance PPrint IfcPrefixes where
    pPrint d p ip =
        text "IfcPrefix:" <+> pPrint d p (ifcp_pathIdString ip) <+>
        doubleQuotes (text (ifcp_renamePrefixes ip)) <+> pPrint d p (ifcp_pragmas ip)

noPrefixes :: IfcPrefixes
noPrefixes = IfcPrefixes {
                                         ifcp_pathIdString   = [],
                                         ifcp_renamePrefixes = "",
                                         ifcp_pragmas        = [] }

-- builds a qualified path for the interface field name
-- FString is the hierarchy path
binId :: IfcPrefixes -> Id -> Id
-- XXX horrible hack when there isn't selection required at the end
binId ifcp i | i == idEmpty =  mkId noPosition (concatFString (init (ifcp_pathIdString ifcp)))
binId ifcp i = (mkIdPre (concatFString (ifcp_pathIdString ifcp)) (unQualId i))

fieldPathName :: IfcPrefixes -> Id -> FString
-- XXX horrible hack when there isn't selection required at the end
fieldPathName ifcp i | i == idEmpty = concatFString $ init $ map fixupPathElement $ ifcp_pathIdString ifcp
fieldPathName ifcp i = concatFString $ map fixupPathElement (ifcp_pathIdString ifcp) ++ [getIdBase i]

fsIndexPlaceholder :: FString
fsIndexPlaceholder = mkFString "[_]"

-- This has two transformations we want to do on path elements when constructing
-- the String type passed to WrapField for the field-wrapping error message:
-- 1. Replace the "_" separators in the path with "." to match the field selection that
-- would appear in the source.
-- 2. Replace the concrete index numbers in the path with "[_]". This is so that we do
-- not create multiple independent WrapField contexts when wrapping each element of a
-- vector-blasted field. If we use concrete field numbers each WrapField context needs to
-- be resolved individually, leading to repeated construction of the field-wrapping
-- dictionaries. If we use the placeholder all of the WrapField contexts for each
-- vector-blasted field will match so they will be joined into a single context
-- (see joinNeededCtxs in TCMisc.hs) before being resolved, so the field-wrapping
-- dictionaries will be constructed only once.
fixupPathElement :: FString -> FString
fixupPathElement fs
  | fs == fsUnderscore = fsDot
  | all isDigit (getFString fs) = fsIndexPlaceholder
  | otherwise = fs

-- Extend the prefixes
-- Take the current set of prefix information, add to that information
-- from the the pragma of the current field Id, and add it to the current set of
-- information
-- Generate a new set of prefixes since the field Id is an interface and we have to traverse down it.
extendPrefixes :: IfcPrefixes -> [IfcPragma] -> CType -> Id -> GWMonad IfcPrefixes
-- XXX idEmpty is a horrible way to know we're not terminating on an interface..
extendPrefixes ifcp _ _ fid | fid == idEmpty = return ifcp
extendPrefixes ifcp iprags ifct fid =
    do  localiprags <- getInterfacePrags (getResultIdFromType ifct)
        return IfcPrefixes {
                            ifcp_pathIdString = newPP,
                            ifcp_renamePrefixes = renames',
                            ifcp_pragmas = aear ++ localiprags
                           }
    where pathPrefix = ifcp_pathIdString ifcp
          renames = ifcp_renamePrefixes ifcp
          -- the new prefix is the just the path extended with the current path.
          newPP = pathPrefix ++ [getIdBase fid, fsUnderscore]
          -- The new prefix string come from the user speced prefix of the interface name
          mpre = fromMaybe (getIdBaseString fid) (lookupPrefixIfcPragma iprags )
          renames' = joinStrings_  renames mpre
          -- consider only the AE AR pragma here, the prefix is accounted for above,
          -- and it is not needed for levels below.
          aear = getARAEPragmas (iprags ++ (ifcp_pragmas ifcp) )


getResultIdFromType :: CType -> Id
getResultIdFromType ftype = fromMaybe err (leftCon (getRes ftype))
    where
    err = internalError( "getResultIdFromType: " ++ show ftype)


-- ====================
-- Pragma utilities

-- We want to fill out the pragma list.   Adding prefixes to all the names, NO NO NO this is called at the top
-- we want to extract all th prefixes, here and generate a list of Ifc Pragmas to add
-- the only thing itneresting is AR, AE.
getInterfaceFieldPrags ::  Id -> Id -> GWMonad [IfcPragma]
getInterfaceFieldPrags  ifcId fieldId  =
    do
       symt <- getSymTab
       let fprags :: [IfcPragma]
           fprags = maybe [] fi_pragmas (findFieldInfo symt ifcId fieldId)
       return fprags

getInterfacePrags :: Id -> GWMonad [IfcPragma]
getInterfacePrags ifcId =
    do symt <- getSymTab
       let r = case (findType symt ifcId) of
                   Nothing -> []
                   Just (TypeInfo mi knd vs tis _) ->
                       case tis of
                           TIstruct (SInterface prags) _ -> prags
                           _                             -> []
       return r


-- generate a full set of pragmas for the new method which is being created.
-- these pragmas are then loaded in the symbol table, and picked out during eval for name creation.
genNewMethodIfcPragmas :: IfcPrefixes -> [IfcPragma] -> Id -> Id -> ([PProp], [IfcPragma])
genNewMethodIfcPragmas ifcp pragmas fieldId newFieldId  =
    (par ++ pae ,( prefix:resName:rdyName:enName:(args ++ ar ++ ae)))
    where methodStr = getIdString fieldId
          currentPre  = ifcp_renamePrefixes ifcp -- the current rename prefix
          --
          localPrefix1 = fromMaybe (getIdString fieldId) (lookupPrefixIfcPragma pragmas)
          localPrefix = joinStrings_  currentPre localPrefix1
          --
          prefix = (PIPrefixStr localPrefix)
          -- arg names are unchanged.
          args    = [a | a@(PIArgNames {} ) <- pragmas]
          -- find AR/AE signals  look for these in parents as well
          joinedPrags = pragmas ++ ifcp_pragmas ifcp
          ar = if (isAlwaysReadyIfc joinedPrags)   then [PIAlwaysRdy     ] else []
          ae = if (isAlwaysEnabledIfc joinedPrags) then [PIAlwaysEnabled ] else []
          -- The result names used the prefix plus the given of generated name
          localResult1 = fromMaybe (getIdString fieldId) (lookupResultIfcPragma pragmas)
          resultName = joinStrings_  currentPre localResult1
          --
          resName = (PIResultName resultName)
          -- The ready name
          mRdyStr = lookupRdySignalIfcPragma pragmas
          readyName =  case mRdyStr of
                       Just str -> joinStrings_ currentPre  str
                       Nothing  -> (getFString fs_rdy) ++ (joinStrings_ currentPre methodStr)
          rdyName = (PIRdySignalName  readyName)
          -- The Enable name
          mEnStr = lookupEnSignalIfcPragma pragmas
          enableName =  case mEnStr of
                        Just str -> joinStrings_ currentPre  str
                        Nothing  -> (getFString fsEnable) ++ (joinStrings_ currentPre methodStr)
          enName = (PIEnSignalName  enableName)
          -- Now the module properties
          -- we need to look the local as well as the parents pragmas
          par = if (isAlwaysReadyIfc   joinedPrags) then [PPalwaysReady   [[newFieldId]]] else []
          pae = if (isAlwaysEnabledIfc joinedPrags) then [PPalwaysEnabled [[newFieldId]]] else []


-- generate a full set of pragmas for the new clock field which is being created.
-- these pragmas are then loaded in the symbol table, and picked out during eval for name creation.
genNewClockIfcPragmas :: IfcPrefixes -> [IfcPragma] -> Id -> Id -> ([PProp], [IfcPragma])
genNewClockIfcPragmas ifcp pragmas fieldId newFieldId  =
    ([] , [prefix])
    where currentPre  = ifcp_renamePrefixes ifcp -- the current rename prefix
          --
          localPrefix1 = fromMaybe (getIdString fieldId) (lookupPrefixIfcPragma pragmas)
          localPrefix = joinStrings_  currentPre localPrefix1
          --
          prefix = (PIPrefixStr localPrefix)


-- generate a full set of pragmas for the new reset field which is being created.
-- these pragmas are then loaded in the symbol table, and picked out during eval for name creation.
genNewResetIfcPragmas :: IfcPrefixes -> [IfcPragma] -> Id -> Id -> ([PProp], [IfcPragma])
genNewResetIfcPragmas ifcp pragmas fieldId newFieldId  =
    ([] , [prefix])
    where currentPre  = ifcp_renamePrefixes ifcp -- the current rename prefix
          --
          localPrefix1 = fromMaybe (getIdString fieldId) (lookupPrefixIfcPragma pragmas)
          localPrefix = joinStrings_  currentPre localPrefix1
          --
          prefix = (PIPrefixStr localPrefix)


-- generate a full set of pragmas for the new inout field which is being created.
-- these pragmas are then loaded in the symbol table, and picked out during eval for name creation.
genNewInoutIfcPragmas :: IfcPrefixes -> [IfcPragma] -> Id -> Id -> ([PProp], [IfcPragma])
genNewInoutIfcPragmas ifcp pragmas fieldId newFieldId  =
    ([] , [prefix])
    where currentPre  = ifcp_renamePrefixes ifcp -- the current rename prefix
          --
          localPrefix1 = fromMaybe (getIdString fieldId) (lookupPrefixIfcPragma pragmas)
          localPrefix = joinStrings_  currentPre localPrefix1
          --
          prefix = (PIPrefixStr localPrefix)


-- Join string together with an underscore if either is not empty.
joinStrings_ :: String -> String -> String
joinStrings_  "" s2 = s2
joinStrings_  s1 "" = s1
joinStrings_  s1 s2 = s1 ++ "_" ++ s2

-- --------------------

-- Check that the names exist
-- XXX also check that always_enabled isn't applied to a value method?
chkUserPragmas :: [PProp] -> GeneratedIfc -> GWMonad ()
chkUserPragmas pps ifc = do
    -- find any names mentioned by the user
    let getMethIds (PPalwaysReady is) = is
        getMethIds (PPalwaysEnabled is) = is
        getMethIds (PPenabledWhenReady is) = is
        getMethIds _ = []
    let pp_ids = map flattenUSId $ -- turn Longname into Id
                 concatMap getMethIds pps
    when (null pp_ids) $ return ()

    -- find the fields of the flattened ifc
    let fields = genifc_fields ifc

    -- identify which fields are methods
    let dropQ (CQType _ t) = t
    let isMethType t = do
          let tt = dropQ t
          isClock <- isClockType tt
          isReset <- isResetType tt
          isInout <- isInoutType tt
          return (not isClock && not isReset && isInout==Nothing)
    meth_fields <- filterM (isMethType . cf_type) fields
    -- and get their flattened ids
    let meth_ids = map cf_name meth_fields

    let bad_pp_ids = filter (\i -> not (elem i meth_ids)) pp_ids
        mkErr i = (getPosition i, EAttributeIdNotAMethod (getIdString i))
    if (null bad_pp_ids)
        then return ()
        else bads $ map mkErr bad_pp_ids


-- ====================
-- Saving name/type information

-- liftModule $ primSavePortType (Valid v) s t
savePortTypeStmt :: CExpr -> (VName, b) -> CType -> CMStmt
savePortTypeStmt v (VName s, _) t =
  CMStmt $ CSExpr Nothing $
    cVApply idLiftModule $
      [cVApply idSavePortType
        [mkMaybe (Just v), stringLiteralAt noPosition s, typeLiteral t]]

-- liftModule $ primSavePortType (Valid v) s (typeOf e)
savePortTypeOfStmt :: CExpr -> (VName, b) -> CExpr -> CMStmt
savePortTypeOfStmt v (VName s, _) e =
  CMStmt $ CSExpr Nothing $
    cVApply idLiftModule $
      [cVApply idSavePortType
        [mkMaybe (Just v), stringLiteralAt noPosition s, cVApply idTypeOf [e]]]

-- primSavePortType Invalid i t
saveTopModPortTypeStmt :: Id -> CType -> CStmt
saveTopModPortTypeStmt i t =
  let s = getIdBaseString i
  in  CSExpr Nothing $
        cVApply idSavePortType
          [mkMaybe Nothing, stringLiteralAt noPosition s, typeLiteral t]

saveNameStmt :: Id -> Id -> CMStmt
saveNameStmt svName resultVar = CMStmt (CSletseq [(CLValue resultVar [CClause [] [] nameExpr]) []])
  where nameExpr = cVApply idGetModuleName [cVApply idAsIfc [CVar svName]]


-- ====================
-- CSyntax utilities

-- The wrapping implementation is ordinary library code.  GenWrap supplies
-- declaration metadata and calls it; neither style needs a generated type.
wrapId :: String -> Id
wrapId = mkQId noPosition (mkFString "ModuleWrap") . mkFString

wrapApply :: String -> [CExpr] -> CExpr
wrapApply name = cVApply (wrapId name)

wrapStyle :: String -> CExpr
wrapStyle name = CCon (wrapId name) []

wrappedIfcType :: String -> CType -> CType
wrappedIfcType style ty =
    cTApplys (cTCon $ wrapId "WrappedIfc") [cTCon $ wrapId style, ty]

-- asIfc suppresses implicit reads on variables.  Applying it to a type
-- annotation would still allow the annotation's internal binding to read
-- _read, so keep the typed binding and its use explicit here.
mkIfcProxyExpr :: CType -> CExpr
mkIfcProxyExpr ty =
    let pos = getPosition ty
        proxy = id_t pos
        def = CLValueSign (CDef proxy (CQType [] ty)
                [CClause [] [] $ CAny pos UNotUsed]) []
    in Cletseq [def] $ cVApply idAsIfc [CVar proxy]

-- Descriptor order agrees with Generic's declaration-order traversal.  Vector
-- elements share the same wrapping dictionary; their names remain term data.
data WrapReadyPolicy = SynthReady | BVIReady

mkWrapDescriptor :: WrapReadyPolicy -> [PProp] -> [Id] -> CType -> GWMonad CExpr
mkWrapDescriptor readyPolicy pps trueFields ty = do
    mi <- chkInterface ty
    case mi of
      Nothing -> internalError ("mkWrapDescriptor: " ++ ppReadable ty)
      Just (ti, _, fs) -> do
        iprags <- getInterfacePrags ti
        entries <- concatMapM (field (noPrefixes { ifcp_pragmas = iprags }) ti) fs
        return $ mkList (getPosition ty) entries
  where
    field prefixes ti (FInf f as r argIds) = do
      prags <- getInterfaceFieldPrags ti f
      mi <- chkInterface r
      case (mi, as) of
        (Just (subti, _, fs), []) -> do
          prefixes' <- extendPrefixes prefixes prags r f
          concatMapM (field prefixes' subti) fs
        _ -> do
          -- BVI vectors remain whole-Bits method values, even if their element
          -- type is an interface with a user Bits instance.  Synthesized
          -- argument-taking methods also use the ordinary method policy.
          mv <- case (readyPolicy, as) of
                  (SynthReady, []) -> isVectorInterfaces r
                  _ -> return Nothing
          case (mv, as) of
            (Just (n, elemTy, _), []) -> do
              prefixes' <- extendPrefixes prefixes prags r f
              concatMapM (\k -> field prefixes' ti (FInf (mkNumId k) [] elemTy [])) [0..n-1]
            _ -> do
              clock <- isClockType r
              reset <- isResetType r
              inout <- isInoutType r
              let pos = getPosition f
                  logical = binId prefixes f
                  current = ifcp_renamePrefixes prefixes
                  prefix = joinStrings_ current $
                             fromMaybe (getIdBaseString f) (lookupPrefixIfcPragma prags)
                  result = joinStrings_ current $
                             fromMaybe (getIdBaseString f) (lookupResultIfcPragma prags)
                  -- Interface always_ready attributes constrain synthesis.
                  -- An import must instead honor its explicit BVI ready port,
                  -- even when the source interface has such an attribute.
                  ifcReady = case readyPolicy of
                    SynthReady -> isAlwaysReadyIfc (prags ++ ifcp_pragmas prefixes)
                    BVIReady -> False
                  ready = clock || reset || isJust inout ||
                          mkRdyId logical `elem` trueFields ||
                          isAlwaysRdy pps (mkRdyId logical) ||
                          ifcReady
                  str = stringLiteralAt pos
                  record = CStruct (Just True) (wrapId "WrapFieldInfo")
                    [(wrapId "wrapName", str $ getIdBaseString logical),
                     (wrapId "wrapPrefix", str prefix),
                     (wrapId "wrapArgNames", mkList pos
                        [stringLiteralAt (getPosition a) (getIdBaseString a) | a <- argIds]),
                     (wrapId "wrapResult", str result),
                     (wrapId "wrapReady", CCon (if ready then idTrue else idFalse) []),
                     (wrapId "wrapPosition", posLiteral pos)]
              return [record]

-- DefFun still constructs and typechecks this definition after synthesis.
-- Only its reconstruction body changes: one library call replaces generated
-- interface methods and subinterface constructors.
genStructuralFromBody :: [(VPort, CType)] -> CExpr -> [PProp] -> [Id] ->
                         CType -> GWMonad CExpr
genStructuralFromBody argTypes mk pps trueFields ty = do
    descriptor <- mkWrapDescriptor SynthReady pps trueFields ty
    let pos = getPosition ty
        raw = id_t pos
        rawType = wrappedIfcType "SynthWrap" ty
        saveArgs = map (uncurry (savePortTypeStmt $ CVar id_x)) argTypes
        saveIfc = CMStmt $ CSExpr Nothing $ cVApply idLiftModule
          [wrapApply "saveWrapIfcPortTypes"
            [wrapStyle "SynthWrap", mkIfcProxyExpr ty,
             mkMaybe (Just $ CVar id_x), descriptor]]
        result = wrapApply "fromWrapIfc" [wrapStyle "SynthWrap", descriptor, CVar raw]
    return $ Cmodule pos $
      [CMStmt $ CSBindT (CPVar raw) Nothing [] (CQType [] rawType) mk,
       saveNameStmt raw id_x] ++ saveArgs ++
      [saveIfc, CMStmt $ CSExpr Nothing $ cVApply (idReturn pos) [result]]

cToVector :: Bool -> [CExpr] -> CExpr
cToVector isListN es = cVApply f [vcons es]
  where vcons [] = CCon (idNil noPosition) []
        vcons (x:xs) = CCon (idCons noPosition) [x, vcons xs]
        f = if isListN then idToListN else idToVector

eUnpack :: CExpr
eUnpack = CVar idUnpack
ePack :: CExpr
ePack = CVar idPack
ePrimInoutCast0 :: CExpr
ePrimInoutCast0 = CVar idPrimInoutCast0
ePrimInoutUncast0 :: CExpr
ePrimInoutUncast0 = CVar idPrimInoutUncast0

-- converts type (ifc) to TAp Module -> ifc
tmod :: CType -> CType
tmod t = TAp (cTCon idModule) t

id_t :: Position -> Id
id_t pos = mkId pos fs_t

-- ====================
-- Ready method utilities

isRdyToRemoveField :: [PProp] -> VFieldInfo -> Bool
isRdyToRemoveField pps f = isAlwaysRdy pps (vf_name f)


hasReadyFN :: [VFieldInfo] -> VFieldInfo -> Bool
hasReadyFN vfs xvf = or (map (isReadyFor xvf) vfs)
 where
   isReadyFor vf@(Method {}) vf_rdy = (mkRdyId (vf_name vf)) == (vf_name vf_rdy)
   isReadyFor _ _ = False


-- ====================
-- XXX Interface argument remnants

getInterfaces :: [Type] -> GWMonad [Type]
getInterfaces ts =
 do
   filterM isGoodInterface ts

isGoodInterface :: Type -> GWMonad Bool
isGoodInterface t =
 do
   res <- chkInterface t
   case res of
     Just _ -> return True
     Nothing -> return False


-- ====================
