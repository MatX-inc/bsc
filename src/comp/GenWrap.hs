{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fwarn-name-shadowing #-}
module GenWrap(
               genWrap,
               WrapInfo(..)
              ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import Data.List(nub, (\\))
import Control.Monad(when, unless, foldM, filterM, zipWithM)
import Control.Monad.Reader(ReaderT, runReaderT, lift, asks, ask)
import PFPrint
import Position(Position, noPosition, cmdPosition)
import Error(internalError, EMsg, ErrMsg(..), ErrorHandle, bsError, bsWarning)
import ErrorMonad(ErrorMonad(..))
import Flags(Flags)
import FStringCompat
import PreStrings(fsUnderscore, fs_t, fsEnable, fs_rdy)
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
import CType(cTNum, tyConArgs, getArrows, cTVarNum, isIfc, getRes, typeclassId, splitTAp)
import VModInfo(VSchedInfo, VPathInfo(..), VeriPortProp(..), VArgInfo(..),
                VFieldInfo(..), VName(..), VWireInfo(..), VPort)
-- GenWrap defines its own versions that expand synonyms and use qualEq
import Type hiding (isPrimAction, isActionValue, getAVType,
                    isReset, isClock, isInout)
import TypeCheck(cCtxReduceDef)
import TCMisc(mkQualType, normT)
import qualified TIMonad as TI
import Subst(mkSubst, Types(..))
import Classic(isClassic)
import Pragma
import PragmaCheck
import Data.Maybe
import Util
import qualified Data.Map as M
import qualified Data.Set as S
import GenWrapUtils
import Undefined(UndefKind(UNotUsed))

-- ====================

-- All type queries share the post-deriving environment and retain the
-- package dependencies and associated-type resolutions they discover.
type GWMonad = ReaderT GenEnv TI.TI

data GenEnv = GenEnv
 {
   errHandle  ::  ErrorHandle,
   symtable   ::  SymTab,
   flags      ::  Flags
 }

runGWMonad :: GWMonad a -> GenEnv -> TI.TIResult a
runGWMonad f env =
    TI.runTI (flags env) False (symtable env) (runReaderT f env)

-- don't expect it to fail
runGWMonadNoFail :: GWMonad a -> GenEnv -> IO a
runGWMonadNoFail f env = do
    let result = runGWMonad f env
    when (not (null (TI.tiWarnings result))) $
        bsWarning (errHandle env) (TI.tiWarnings result)
    case TI.tiResult result of
      Right res -> return res
      Left msgs -> internalError ("runGWMonadNoFail: " ++ ppReadable msgs)

-- for converting from ErrorMonad
convEM :: ErrorMonad a -> GWMonad a
convEM (EMError msgs) = bads msgs
convEM (EMWarning msgs x) = lift (mapM_ TI.twarn msgs) >> return x
convEM (EMResult x) = return x

getFlags :: GWMonad Flags
getFlags = asks flags

getSymTab :: GWMonad SymTab
getSymTab = asks symtable

bad :: EMsg -> GWMonad a
bad = lift . TI.err

bads :: [EMsg] -> GWMonad a
bads = lift . TI.errs "GenWrap"

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

-- One flattened source field supplies port checks, pragmas and the library's
-- descriptor.  There is no synthetic interface declaration or name to track.
data BoundaryField = BoundaryField
  { boundarySource :: FInf
  , boundaryPragmas :: [IfcPragma]
  , boundaryProperties :: [PProp]
  , boundaryPrefix :: String
  , boundaryResult :: String
  , boundaryReady :: Bool
  , boundaryWire :: Bool
  }

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
           IO (CPackage, [WrapInfo], S.Set Id, TI.CATFCache)
genWrap errh flgs genNames generating cpack symt = do
    let result = runGWMonad (genWrapE generating ppmap cpack) env
    when (not (null (TI.tiWarnings result))) $
        bsWarning errh (TI.tiWarnings result)
    case TI.tiResult result of
      Left msgs -> bsError errh msgs
      Right (cpack', infos) ->
          return (cpack', infos, TI.tiUsedPackages result, TI.tiATFCache result)
 where
    env = GenEnv errh symt flgs
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
genWrapE generating ppmap (CPackage packageId exps imps impsigs fixs ds includes) = do
    defs <- mapM fixVerilogIfc ds
    modules <- concatMapM (getDef generating defs) (M.toList ppmap)
    wrapped <- mapM wrapModule modules
    let (newDefs, infos) = unzip wrapped
        fixDef (CValueSign (CDef i t _)) | generating && isGenDef ppmap i =
            CValueSign (CDef i t [CClause [] [] (CVar i)])
        fixDef d = d
    return (CPackage packageId exps imps impsigs fixs
                     (map fixDef defs ++ newDefs) includes, infos)
  where
    wrapModule info@(def@(CDef _ ty _), _, _, _) = do
        ifc <- chkType def ty
        fields <- collectBoundaryFields SynthReady ifc
        wrapped <- mkNewModDef fields info
        continuation <- genWrapInfo fields info
        return (wrapped, continuation)
    wrapModule (def, _, _, _) =
        internalError ("GenWrap.wrapModule: " ++ ppReadable def)

-- check that the type being generated is legal (uses the def to get a position)
chkType :: CDef -> CQType -> GWMonad CType
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
                         Just _ -> return tr
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

genWrapInfo :: [BoundaryField] -> ModDefInfo -> GWMonad WrapInfo
genWrapInfo fields (d@(CDef modName oqt _), cqt, _, pps) = do
    let iprops = concatMap boundaryProperties fields
        ports = boundaryPorts pps fields
    chkUserPragmas pps fields
    namefun <- mkDef fields iprops pps d cqt
    return WrapInfo { mod_nm = modName
                    , orig_cqt = oqt
                    , wrapped_mod = modIdRename pps modName
                    , wi_prags = pps ++ iprops
                    , wi_field_info = [(i, ps) | (i, _, ps) <- ports]
                    , deffun = namefun }
genWrapInfo _ (def, _, _, _) =
    internalError ("genWrapInfo: unexpected def: " ++ ppReadable def)

-- --------------------
-- genWrapE toplevel: fixVerilogIfc

-- The BVI parser inserts the generic unwrap call.  Fill its declaration
-- metadata, style schema, and raw structural type, and retain the existing
-- port-type recording and validation.  Raw `module verilog` is left alone.
-- The matched syntax is marked (*A*) in CVParserImperative.
fixVerilogIfc :: CDefn -> GWMonad CDefn
fixVerilogIfc (CValueSign (CDef n (CQType prs tid)
                          [CClause ps qs (Cmodule pos body)])) = do
    (newBody, prs') <- foldM (fixCModuleVerilog (CQType prs tid)) ([], prs) body
    return $ CValueSign $ CDef n (CQType prs' tid)
        [CClause ps qs (Cmodule pos (reverse newBody))]
fixVerilogIfc d = return d

-- Fill one parser-generated BVI boundary.
fixCModuleVerilog :: CQType -> ([CMStmt], [CPred]) -> CMStmt -> GWMonad ([CMStmt], [CPred])
fixCModuleVerilog moduleType (ss, ps)
       bdy@(CMStmt (CSBindT p@(CPVar mName) mi pprops cq@(CQType qs rawTy)
                            def@(CApply _ [CmoduleVerilog vname _ _ _ aes fs _ _])))
  | (TCon (TyCon wrap _ _), [TCon (TyCon style _ _), ty]) <- splitTAp rawTy,
    wrap == wrapId "WrappedIfc", style == wrapId "BVIWrap" =
   withModuleContext moduleType $ do
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
       -- Share declaration-order metadata with the synthesis path, retaining
       -- BVI's packed-value policy for vectors and abstract method results.
       boundary <- collectBoundaryFields BVIReady ty
       let flat_fts = map boundarySource boundary
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
       let descriptor = mkWrapDescriptor rdyPPs [] boundary
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
       return (ss'', ps' ++ ps)
fixCModuleVerilog _ (ss, ps) s = return (s:ss, ps)

-- BVI shape is determined at the declaration, including its explicit
-- provisos.  A given may identify an associated type as a subinterface.
withModuleContext :: CQType -> GWMonad a -> GWMonad a
withModuleContext (CQType [] _) query = query
withModuleContext cqt query = do
    qt@(ps :=> _) <- lift (mkQualType cqt)
    givens <- lift $ mapM (TI.mkEPred . removePredPositions) ps
    env <- ask
    lift $ TI.withLocalTypeContext (tv qt) givens (runReaderT query env)

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


-- Flatten declaration metadata once, after deriving has made type functions
-- available.  Generic handles the value traversal; this walk only preserves
-- source names, positions and pragmas which are not part of Generic.
data WrapReadyPolicy = SynthReady | BVIReady

collectBoundaryFields :: WrapReadyPolicy -> CType -> GWMonad [BoundaryField]
collectBoundaryFields policy ty = do
    mi <- chkInterface ty
    case mi of
      Nothing -> internalError ("collectBoundaryFields: " ++ ppReadable ty)
      Just (ti, _, fs) -> do
        prags <- getInterfacePrags ti
        concatMapM (field (noPrefixes { ifcp_pragmas = prags }) ti) fs
  where
    field prefixes ti (FInf f as r argIds) = do
      prags <- getInterfaceFieldPrags ti f
      mi <- chkInterface r
      case (mi, as) of
        (Just (subti, _, fs), []) -> do
          prefixes' <- extendPrefixes prefixes prags r f
          concatMapM (field prefixes' subti) fs
        _ -> do
          -- BVI vectors and argument-taking methods are packed values.
          mv <- case (policy, as) of
                  (SynthReady, []) -> isVectorInterfaces r
                  _ -> return Nothing
          case mv of
            Just (n, elemTy, _) -> do
              prefixes' <- extendPrefixes prefixes prags r f
              concatMapM (\k -> field prefixes' ti (FInf (mkNumId k) [] elemTy [])) [0..n-1]
            Nothing -> do
              clock <- isClockType r
              reset <- isResetType r
              inout <- isInoutType r
              let logical = binId prefixes f
                  sourceName = unQualId f
                  current = ifcp_renamePrefixes prefixes
                  prefix = joinStrings_ current $
                    fromMaybe (getIdBaseString f) (lookupPrefixIfcPragma prags)
                  result = joinStrings_ current $
                    fromMaybe (getIdBaseString f) (lookupResultIfcPragma prags)
                  gen | wire = genNewWireIfcPragmas
                      | otherwise = genNewMethodIfcPragmas
                  (props, fieldPrags) = gen prefixes prags sourceName logical
                  wire = clock || reset || isJust inout
                  ifcReady = case policy of
                    SynthReady -> isAlwaysReadyIfc (prags ++ ifcp_pragmas prefixes)
                    BVIReady -> False
              return [BoundaryField (FInf logical as r argIds) fieldPrags props
                                    prefix result (wire || ifcReady) wire]

-- Ready signals are port metadata, not additional source fields.
boundaryPorts :: [PProp] -> [BoundaryField] -> [(Id, CType, [IfcPragma])]
boundaryPorts pps = concatMap ports
  where
    ports f =
      let src = boundarySource f
          i = name src
          t = foldr arrow (ret_type src) (arg_types src)
          ready = mkRdyId i
          readyPort = [(ready, TAp tBit (cTNum 1 (getPosition i)), []) |
                        not (boundaryReady f || isAlwaysRdy pps ready)]
      in (i, t, boundaryPragmas f) : readyPort

-- --------------------
-- genWrapE toplevel: mkNewModDef

-- For a module def named <modname>, we generate the actual synthesis
-- boundary called "<modname>_" which has bitified arguments and wraps
-- around the actual module definition (lifted with liftM), unpacking
-- the values before applying the module def to them.

mkNewModDef :: [BoundaryField] -> ModDefInfo -> GWMonad CDefn
mkNewModDef fields (def@(CDef i (CQType _ t) _), _, vtis, vps) = do
   let tr = case getArrows t of
              (_, TAp _ r) -> r
              _ -> internalError "GenWrap.mkNewModDef: tr"
       ty = tmod (wrappedIfcType "SynthWrap" tr)
       -- Preserve diagnostic ordering from the previous port checker.
       ftps = reverse (boundaryPorts vps fields)
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
   let wrapInfo = mkWrapDescriptor vps [] fields
   let ifc_sptStmts = [CSExpr Nothing $ cVApply idLiftModule
                        [wrapApply "saveWrapIfcPortTypes"
                         [wrapStyle "SynthWrap", mkIfcProxyExpr tr,
                          mkMaybe Nothing, wrapInfo]]]

   let sptStmts = arg_sptStmts ++ ifc_sptStmts
       -- a do-block around the module body, so that we can include the
       -- save-port-type statements
       lexp = Cdo False (sptStmts ++ [CSExpr Nothing mexp])
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
      isInout <- isInoutType lt
      case isInout of
        Just tt -> return (CPred (CTypeclass idBits) [tt, v] : ctx,
                           TAp tInout_ v `fn` rt)
        Nothing -> do
          isClock <- isClockType lt
          isReset <- isResetType lt
          isParam <- isParamType lt
          if isClock || isReset || isParam
            then return (ctx, lt `fn` rt)
            else return (CPred (CTypeclass idBits) [lt, v] : ctx,
                         TAp tBit v `fn` rt)

mkNewModDef _ (def,_,_,_) =
    internalError ("GenWrap::mkNewModDef unexpected definition " ++ show def )


-- ==============================
-- The wrapper continuation

-- This is the part of "genWrapInfo" which makes the DefFun,
-- a continuation function which does the final wrapper computation.

-- type DefFun = Bool -> VWireInfo -> VSchedInfo -> VPathInfo -> [VPort] -> SymTab -> [VFieldInfo] -> [Id] -> IO CDefn
-- XXX: alwaysEnabled is dropped and broken (not propagated to {inhigh})
mkDef :: [BoundaryField] -> [PProp] -> [PProp] -> CDef -> CQType -> GWMonad DefFun
mkDef boundary iprags pps (CDef i (CQType _ qt) _) cqt = do
 env <- ask
 return (\fmod wire_info sch pathinfo ips symt fields true_ifc_ids -> do
  let
      (ts, tr) = case getArrows qt of
                   (ats, TAp _ r) -> (ats, r)
                   _ -> internalError "GenWrap.mkDef: ts, tr"
      env' = env { symtable = symt }
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

  argss <- runGWMonadNoFail (zipWithM genArg (map CVar vs) ts) env'
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
  let body = genStructuralFromBody boundary arg_pts vlift
                 (iprags ++ pps) true_ifc_ids tr
      cls = CClause (map CPVar vs) [] body
  return $ CValueSign (CDef i cqt [cls]))

mkDef _ _ _ def _ = internalError ("GenWrap::mkDef unexpected " ++ show def )


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
     Just (TCon (TyNum n _), tVec, isListN) ->
         return $ Just (n, tVec, isListN)
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
   -- The parsed BVI type still has kind variables.  Reuse the kinds inferred
   -- for its declaration before matching typeclass instances.
   bound <- lift TI.getBoundTVs
   let scoped = apSub (mkSubst [(v, TVar v) | v <- bound]) xt
       t = updTypes symt scoped
   lift $ do
     TI.addBoundTVs (tv t)
     t' <- normT t
     TI.popBoundTVs
     return t'
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


-- Clock, reset and inout fields only carry their physical name prefix.
genNewWireIfcPragmas :: IfcPrefixes -> [IfcPragma] -> Id -> Id -> ([PProp], [IfcPragma])
genNewWireIfcPragmas ifcp pragmas fieldId _ =
    ([], [PIPrefixStr prefix])
  where
    localPrefix = fromMaybe (getIdString fieldId) (lookupPrefixIfcPragma pragmas)
    prefix = joinStrings_ (ifcp_renamePrefixes ifcp) localPrefix


-- Join string together with an underscore if either is not empty.
joinStrings_ :: String -> String -> String
joinStrings_  "" s2 = s2
joinStrings_  s1 "" = s1
joinStrings_  s1 s2 = s1 ++ "_" ++ s2

-- --------------------

-- Check that the names exist
-- XXX also check that always_enabled isn't applied to a value method?
chkUserPragmas :: [PProp] -> [BoundaryField] -> GWMonad ()
chkUserPragmas pps fields = do
    let getMethIds (PPalwaysReady is) = is
        getMethIds (PPalwaysEnabled is) = is
        getMethIds (PPenabledWhenReady is) = is
        getMethIds _ = []
        ids = map flattenUSId $ concatMap getMethIds pps
        methods = [name (boundarySource f) | f <- fields, not (boundaryWire f)]
        invalid = filter (`notElem` methods) ids
    unless (null invalid) $ bads
      [(getPosition i, EAttributeIdNotAMethod (getIdString i)) | i <- invalid]


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
wrapId = mkQId noPosition (mkFString "WrapModule") . mkFString

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

-- Descriptor order agrees with Generic's declaration-order traversal.
-- Readiness learned during synthesis only changes these values; the source
-- interface does not need to be traversed again by the continuation.
mkWrapDescriptor :: [PProp] -> [Id] -> [BoundaryField] -> CExpr
mkWrapDescriptor pps trueFields fields = mkList noPosition (map record fields)
  where
    record f =
      let src = boundarySource f
          i = name src
          pos = getPosition i
          str = stringLiteralAt pos
          ready = boundaryReady f || mkRdyId i `elem` trueFields ||
                  isAlwaysRdy pps (mkRdyId i)
      in CStruct (Just True) (wrapId "WrapFieldInfo")
           [(wrapId "wrapName", str $ getIdBaseString i),
            (wrapId "wrapPrefix", str $ boundaryPrefix f),
            (wrapId "wrapArgNames", mkList pos
               [stringLiteralAt (getPosition a) (getIdBaseString a) | a <- arg_names src]),
            (wrapId "wrapResult", str $ boundaryResult f),
            (wrapId "wrapReady", CCon (if ready then idTrue else idFalse) []),
            (wrapId "wrapPosition", posLiteral pos)]

-- DefFun still constructs and typechecks this definition after synthesis.
-- Only its reconstruction body changes: one library call replaces generated
-- interface methods and subinterface constructors.
genStructuralFromBody :: [BoundaryField] -> [(VPort, CType)] -> CExpr ->
                         [PProp] -> [Id] -> CType -> CExpr
genStructuralFromBody fields argTypes mk pps trueFields ty =
    let descriptor = mkWrapDescriptor pps trueFields fields
        pos = getPosition ty
        raw = id_t pos
        rawType = wrappedIfcType "SynthWrap" ty
        saveArgs = map (uncurry (savePortTypeStmt $ CVar id_x)) argTypes
        saveIfc = CMStmt $ CSExpr Nothing $ cVApply idLiftModule
          [wrapApply "saveWrapIfcPortTypes"
            [wrapStyle "SynthWrap", mkIfcProxyExpr ty,
             mkMaybe (Just $ CVar id_x), descriptor]]
        result = wrapApply "fromWrapIfc" [wrapStyle "SynthWrap", descriptor, CVar raw]
    in Cmodule pos $
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
