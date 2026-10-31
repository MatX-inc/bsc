-- The native-version adapter.  Construct this value lazily and serialize
-- it only after ordinary output has been forced (and intern state captured).
-- All identifier conversion is read-only: never manufacture Ids/FStrings here.
module ECO.Graph (extractFinalGraph, extractPreOptGraph) where

import ASyntax
import ECO.Types
import Id (getIdString)
import IntLit (IntLit(..))
import VModInfo (VArgInfo(..), VName(..), VFieldInfo(..), VPort,
                 VeriPortProp(..), vName, vFields)
import Data.Char (isAlphaNum)
import Data.List (sort)
import Data.Maybe (mapMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

extractFinalGraph :: APackage -> ASPackage -> ASPackage -> EcoGraph
extractFinalGraph scheduled state final = EcoGraph ecoGraphSchema
    (name (aspkg_name final)) "final-typed-definitions" (M.elems allNodes)
    (sort (S.toList reserved))
  where
    name = getIdString
    defs = aspkg_values final ++ aspkg_inout_values final
    defNames = S.fromList (map (name . adef_objid) defs)
    outputs = S.fromList (map (name . fst) (aspkg_outputs final))
    inouts = S.fromList (map (name . fst) (aspkg_inouts final) ++
                        map (name . adef_objid) (aspkg_inout_values final))
    foreignWrites = S.fromList [name i | (_,cs) <- aspkg_foreign_calls final,
                                       c <- cs, i <- afc_writes c]
    -- Constructor arguments are in getInstArgs; method arguments/enables are
    -- final definitions named instance$formal.  Both are consumer anchors.
    -- Build text directly, rather than invoking backend naming helpers that
    -- allocate fresh interned strings while observing the compilation.
    methodInputs = M.fromList
        [(name (avi_vname inst) ++ "$" ++ port,
          "instance-method-input:" ++ name (avi_vname inst) ++ ":" ++ port) |
            inst <- aspkg_state_instances final,
            meth@Method {} <- vFields (avi_vmi inst),
            base <- map portName (concat (vf_inputs meth) ++ maybe [] (:[]) (vf_enable meth)),
            port <- if vf_mult meth <= 1 then [base]
                    else [base ++ "_" ++ show i | i <- [1..vf_mult meth]]]
    sources = [("input",p) | p <- aspkg_inputs final] ++
              [("parameter",p) | p <- aspkg_parameters final] ++
              [("inout",p) | p <- aspkg_inouts final] ++
              [("state-output",p) | p <- aspkg_state_outputs final] ++
              [("output",p) | p <- aspkg_outputs final,
                             S.notMember (name (fst p)) defNames]
    tupleFields = concatMap emittedFields defs
    emittedFields d = case (adef_type d,adef_expr d) of
        (ATTuple ts,ATuple _ es) | length ts == length es ->
            [(d,ix,t,Just e) | (ix,(t,e)) <- zip [1 :: Integer ..] (zip ts es)]
        -- Instance-driven noinline result fields also exist in emitted RTL.
        -- Preserve their identity/width, but never classify them as pure defs.
        (ATTuple ts,ANoInlineFunCall {}) ->
            [(d,ix,t,Nothing) | (ix,t) <- zip [1 :: Integer ..] ts]
        _ -> []
    fieldName d ix = name (adef_objid d) ++ "_" ++ show ix
    fieldKey d ix = "field:" ++ name (adef_objid d) ++ ":" ++ show ix
    symbols = M.fromList $ [(name i,"wire:" ++ name i) | (_,(i,_)) <- sources] ++
                              [(name (adef_objid d),"def:" ++ name (adef_objid d)) | d <- defs] ++
                              [(tupleSymbol (adef_objid d) ix,fieldKey d ix) |
                                  (d,ix,_,_) <- tupleFields]
    sourceNodes = [EcoNode (symbols M.! name i) (Just (name i)) (typeKey t)
                     "boundary" [] (Just (kind ++ ":" ++ name i)) False [] False
                  | (kind,(i,t)) <- sources, S.notMember (name i) defNames]
    baNames = S.fromList (map (name . adef_objid) (apkg_local_defs scheduled))
    signalInfo = aspkg_signal_info state
    roles = M.fromListWith (++) $
        [(name i,["rule:" ++ name r ++ ":fire:" ++ show ix]) |
            (r,is) <- aspsi_rule_sched signalInfo, (ix,i) <- zip [0 :: Int ..] is] ++
        [(name i,[kind ++ ":" ++ name i]) | (kind,is) <-
            [("astate-mux-selector",aspsi_mux_selectors signalInfo),
             ("astate-mux-value",aspsi_mux_values signalInfo),
             ("astate-enable",aspsi_submod_enables signalInfo)], i <- is]
    context n = ["scheduled-definition:" ++ n | S.member n baNames] ++
                M.findWithDefault [] n roles
    defNodes d =
        let n = name (adef_objid d)
            key = symbols M.! n
            (ek,es) = exprNodes symbols (key ++ ":rhs") (adef_expr d)
            opaque = unsupported (adef_expr d) || S.member n foreignWrites
            boundary | S.member n outputs = Just ("output:" ++ n)
                     | S.member n inouts = Just ("inout:" ++ n)
                     | S.member n foreignWrites = Just ("foreign-result:" ++ n)
                     | Just role <- M.lookup n methodInputs = Just role
                     | otherwise = Nothing
        in EcoNode key (Just n) (typeKey (adef_type d)) "definition" [ek]
                   boundary (boundary == Nothing && not opaque && scalar (adef_type d)) (context n) opaque : es
    fieldNodes (d,ix,t,Nothing) =
        let parent = name (adef_objid d)
        in [EcoNode (fieldKey d ix) (Just (fieldName d ix)) (typeKey t)
             "boundary" [] (Just ("noinline-result:" ++ fieldName d ix)) False
             (context parent ++ ["tuple-parent:" ++ parent,"tuple-field-index:" ++ show ix]) True]
    fieldNodes (d,ix,t,Just e) =
        let n = fieldName d ix
            key = fieldKey d ix
            parent = name (adef_objid d)
            (ek,es) = exprNodes symbols (key ++ ":rhs") e
            opaque = unsupported e || S.member parent foreignWrites
            boundary | S.member n outputs = Just ("output:" ++ n)
                     | S.member n inouts = Just ("inout:" ++ n)
                     | S.member parent foreignWrites = Just ("foreign-result:" ++ n)
                     | otherwise = Nothing
        in EcoNode key (Just n) (typeKey t) "definition" [ek] boundary
             (scalar t && boundary == Nothing && not opaque)
             (context parent ++ ["tuple-parent:" ++ parent,"tuple-field-index:" ++ show ix]) opaque : es
    instanceNodes inst = concat
        [let formal = argName arg
             key = "instance:" ++ name (avi_vname inst) ++ ":argument:" ++ formal
             (ek,es) = exprNodes symbols (key ++ ":rhs") expr
         in EcoNode key Nothing (typeKey (ae_type expr))
                ("instance-argument:" ++ getModuleName inst) [ek]
                (Just key) False [] False : es
         | (arg,expr) <- getInstArgs inst]
    getModuleName = (\(VName s) -> s) . vName . avi_vmi
    foreignNodes = concat
        [let key = "foreign:" ++ name (afc_name c) ++ ":" ++ show ix
             children = [exprNodes symbols (key ++ ":arg:" ++ show n) e |
                            (n,e) <- zip [0 :: Int ..] (afc_args c ++ afc_resets c)]
         in EcoNode key Nothing "Action" ("foreign:" ++ afc_fun c)
                (map fst children) (Just key) False [] True : concatMap snd children
         | (ix,c) <- zip [0 :: Int ..] (concatMap snd (aspkg_foreign_calls final))]
    allNodes = M.fromList [(enKey n,n) | n <- sourceNodes ++ concatMap defNodes defs ++ concatMap fieldNodes tupleFields ++
                     concatMap instanceNodes (aspkg_state_instances final) ++ foreignNodes]
    reserved = S.fromList $ mapMaybe enName (M.elems allNodes) ++
        [name (avi_vname i) | i <- aspkg_state_instances final] ++
        S.toList foreignWrites

-- Same neutral typed DAG at the exact keep boundary. Its eligibility denotes
-- pure scalar helper candidates; final emission applies its independent AST
-- declaration/port/state checks before allowing any rename.
extractPreOptGraph :: APackage -> ASPackage -> ASPackage -> EcoGraph
extractPreOptGraph scheduled state pre =
    (extractFinalGraph scheduled state pre) { egStage = "pre-aopt-typed-definitions" }

scalar :: AType -> Bool
scalar (ATBit w) = w > 0
scalar _ = False

tupleSymbol :: AId -> Integer -> String
tupleSymbol i ix = getIdString i ++ "\0tuple-field:" ++ show ix

argName :: VArgInfo -> String
argName (Param (VName s)) = "parameter:" ++ s
argName (Port (VName s,_) _ _) = "port:" ++ s
argName (ClockArg i) = "clock:" ++ getIdString i
argName (ResetArg i) = "reset:" ++ getIdString i
argName (InoutArg (VName s) _ _) = "inout:" ++ s

-- Keep this textual projection in sync with the port spelling in
-- BackendNamingConventions.getFStringForVerilogPair.
portName :: VPort -> String
portName (VName str,props) =
    filter (\c -> isAlphaNum c || c == '_') (map replace str) ++
    if VPinhigh `elem` props then "_AlwaysEnabled" else ""
  where replace ' ' = '_'
        replace '?' = 'X'
        replace c = c

typeKey :: AType -> String
typeKey (ATBit w) = "Bit:" ++ show w
typeKey (ATString w) = "String:" ++ show w
typeKey ATReal = "Real"
typeKey (ATArray n t) = "Array:" ++ show (n,typeKey t)
typeKey (ATTuple ts) = "Tuple:" ++ show (map typeKey ts)
typeKey (ATAbstract i ns) = "Abstract:" ++ show (getIdString i,ns)

-- Inline expression occurrences stay separate.  References to definitions
-- point to their one DAG node instead of expanding their right-hand sides.
exprNodes :: M.Map String EcoKey -> EcoKey -> AExpr -> (EcoKey,[EcoNode])
exprNodes symbols key expr = case expr of
    ATupleSel _ (ASDef _ i) ix | Just k <- M.lookup (tupleSymbol i ix) symbols -> (k,[])
    ASDef _ i -> reference "unresolved-definition" i
    ASPort _ i -> reference "port" i
    ASParam _ i -> reference "parameter" i
    _ -> let (op,args,opaque) = expressionShape expr
             children = [exprNodes symbols (key ++ ":" ++ show ix) arg |
                            (ix,arg) <- zip [0 :: Int ..] args]
             node = EcoNode key Nothing (typeKey (ae_type expr)) op
                        (map fst children) Nothing False [] opaque
         in (key,node : concatMap snd children)
  where
    reference kind i =
        let n = getIdString i
        in case M.lookup n symbols of
            Just k -> (k,[])
            Nothing -> let k = "external:" ++ kind ++ ":" ++ n
                       in (k,[EcoNode k (Just n) (typeKey (ae_type expr))
                                "boundary" [] (Just (kind ++ ":" ++ n))
                                False [] (kind == "unresolved-definition")])

expressionShape :: AExpr -> (String,[AExpr],Bool)
expressionShape expr = case expr of
    APrim _ _ op args -> ("primitive:" ++ show op,args,False)
    ASInt _ _ lit -> ("integer:" ++ show (ilWidth lit,ilBase lit,ilValue lit),[],False)
    ASReal _ _ val -> ("real:" ++ show val,[],False)
    ASStr _ _ val -> ("string:" ++ show val,[],False)
    ATuple _ args -> ("tuple",args,False)
    ATupleSel _ arg ix -> ("tuple-select:" ++ show ix,[arg],False)
    -- Record the existing don't-care and its optional chosen value exactly.
    -- Naming never completes it or replaces it with an old expression.
    ASAny _ val -> ("any",maybe [] (:[]) val,False)
    AMethCall _ obj meth args ->
        ("method:" ++ show (getIdString obj,getIdString meth),args,True)
    AMethValue _ obj meth ->
        ("method-value:" ++ show (getIdString obj,getIdString meth),[],True)
    ANoInlineFunCall _ _ fun args -> ("noinline:" ++ show fun,args,True)
    AFunCall _ _ fun isC args -> ("function:" ++ show (fun,isC),args,True)
    ATaskValue _ _ fun isC cookie -> ("task:" ++ show (fun,isC,cookie),[],True)
    ASClock _ (AClock osc gate) -> ("abstract-clock",[osc,gate],True)
    ASReset _ (AReset arg) -> ("abstract-reset",[arg],True)
    ASInout _ (AInout arg) -> ("abstract-inout",[arg],True)
    AMGate _ obj clk -> ("clock-gate:" ++ show (getIdString obj,getIdString clk),[],True)
    ASDef {} -> ("reference",[],False)
    ASPort {} -> ("reference",[],False)
    ASParam {} -> ("reference",[],False)

unsupported :: AExpr -> Bool
unsupported expr = let (_,args,opaque) = expressionShape expr
                   in opaque || any unsupported args
