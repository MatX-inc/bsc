-- Independent final-AST scalar classification; no matching or rewriting policy.
module ECO.Namespace (ecoNamespace, ecoEligible) where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Verilog

-- Traverse the typed Verilog constructors, never compiler Id internals or
-- generic Data representations. Keep VId's optional original-register item:
-- its names were reserved by the original observer, even when not emitted.
-- Metadata items contribute expression/statement protection, but are not
-- module-level declarations or instances in their own right.
data Scan = Scan
    { scanNames :: S.Set String
    , scanProtected :: S.Set String
    , scanWrites :: S.Set String
    }

emptyScan :: Scan
emptyScan = Scan S.empty S.empty S.empty

scans :: [Scan] -> Scan
scans xs = Scan (S.unions (map scanNames xs)) (S.unions (map scanProtected xs))
                (S.unions (map scanWrites xs))

protect :: S.Set String -> Scan -> Scan
protect ns s = s { scanProtected = ns `S.union` scanProtected s }

protectAll :: Scan -> Scan
protectAll s = protect (scanNames s) s

ecoNamespace :: VProgram -> [String]
ecoNamespace = S.toAscList . scanNames . programScan

-- A final typed ADef is necessary but not sufficient for eligibility. Only
-- module-level scalar declarations survive this independent Verilog check.
-- Functions, state, arrays, external identities and hierarchy are protected.
ecoEligible :: [String] -> VProgram -> [String]
ecoEligible requested p@(VProgram [m] _ _) = S.toAscList $
    S.fromList requested `S.intersection` declared (vm_body m)
                         `S.difference` scanProtected (programScan p)
ecoEligible _ _ = []

declared :: [VMItem] -> S.Set String
declared = S.unions . map go
  where
    go (VMDecl (VVDecl ty _ vs)) | ty == VDWire || ty == VDReg =
        S.fromList [getVIdString v | VVar v <- vs]
    go (VMDecl (VVDWire _ (VVar v) _)) = S.singleton (getVIdString v)
    go (VMComment _ x) = go x
    go (VMGroup _ xs) = declared (concat xs)
    go _ = S.empty

programScan :: VProgram -> Scan
programScan p@(VProgram ms dpis _) = scans (map moduleScan ms ++ map dpiScan dpis)
  where
    ws = signalWidths p
    moduleScan m = scans [protectAll (idScan ws (vm_name m)),
                         protectAll (scans (map (argScan ws) (concatMap fst (vm_ports m)))),
                         scans (map (itemScan ws True) (vm_body m))]
    dpiScan (VDPI f _ args) = protectAll $ scans
        (idScan ws f : [idScan ws i | (i,_,_) <- args])

idScan :: SignalWidths -> VId -> Scan
idScan ws (VId n _ metadata) = protect hierarchy $
    scans [Scan (S.singleton n) S.empty S.empty, maybe emptyScan (itemScan ws False) metadata]
  where
    hierarchy | '.' `elem` n = S.fromList [n,takeWhile (/= '.') n]
              | otherwise = S.empty

rangeScan :: SignalWidths -> Maybe VRange -> Scan
rangeScan ws = maybe emptyScan (\(hi,lo) -> scans [exprScan ws hi,exprScan ws lo])

argScan :: SignalWidths -> VArg -> Scan
argScan ws arg = case arg of
    VAInput i r -> scans [idScan ws i,rangeScan ws r]
    VAOutput i r -> scans [idScan ws i,rangeScan ws r]
    VAInout i alias r -> scans [idScan ws i,maybe emptyScan (idScan ws) alias,
                               maybe emptyScan (rangeScan ws) r]
    VAParameter i r e -> scans [idScan ws i,rangeScan ws r,exprScan ws e]

varScan :: SignalWidths -> VVar -> Scan
varScan ws (VVar i) = idScan ws i
varScan ws (VArray r i) = scans [rangeScan ws (Just r),idScan ws i]

declScan :: SignalWidths -> VVDecl -> Scan
declScan ws (VVDecl _ r vs) = scans (rangeScan ws r : map (varScan ws) vs)
declScan ws (VVDWire r v e) = scans [rangeScan ws r,varScan ws v,exprScan ws e]

itemScan :: SignalWidths -> Bool -> VMItem -> Scan
itemScan ws atModule item = case item of
    VMDecl d -> protect (if atModule then protectedDecl d else S.empty) (declScan ws d)
    VMInst mn inst params ports ->
        let named ps = scans [scans [idScan ws i,maybe emptyScan (exprScan ws) e] | (i,e) <- ps]
            formalIds = mn : inst : map fst ports ++ either (const []) (map fst) params
            allParts = scans [idScan ws mn,idScan ws inst,named ports,
                         either (scans . map (exprScan ws . snd)) named params]
        in protect (if atModule then scanNames (scans (map (idScan ws) formalIds)) else S.empty) allParts
    VMAssign lv e -> scans [lvalueScan ws lv,exprScan ws e]
    VMStmt _ s -> statementScan (stmtScan ws s)
    VMComment _ x -> itemScan ws atModule x
    VMRegGroup i _ _ x -> markAll (scans [idScan ws i,itemScan ws False x])
    VMGroup _ xs -> scans (map (itemScan ws atModule) (concat xs))
    VMFunction (VFunction f r ds s) -> markAll $
        scans [idScan ws f,rangeScan ws r,scans (map (declScan ws) ds),statementScan (stmtScan ws s)]
  where
    markAll s | atModule = protectAll s
              | otherwise = s
    protectedDecl (VVDecl ty _ vs) = S.fromList
        [getVIdString (vvName v) | v <- vs, ty `elem` [VDInput,VDInout,VDOutput] || isArray v]
    protectedDecl (VVDWire _ v _) | isArray v = scanNames (varScan ws v)
    protectedDecl _ = S.empty
    isArray (VArray _ _) = True
    isArray _ = False

exprScan :: SignalWidths -> VExpr -> Scan
exprScan ws expr = case expr of
    VEConst _ -> emptyScan
    VEReal _ -> emptyScan
    VEWConst i _ _ _ -> idScan ws i
    VEUnknown _ _ -> emptyScan
    VEString _ -> emptyScan
    VETriConst _ -> emptyScan
    VEUnOp i _ e -> scans [idScan ws i,exprScan ws e]
    VEOp i a _ b -> scans [idScan ws i,exprScan ws a,exprScan ws b]
    VEVar i -> idScan ws i
    VEConcat es -> scans (map (exprScan ws) es)
    VEIndex i e -> scans [idScan ws i,exprScan ws e]
    VESelect e hi lo -> scans (map (exprScan ws) [e,hi,lo])
    VESelect1 e ix -> scans (map (exprScan ws) [e,ix])
    VERepeat count e -> scans (map (exprScan ws) [count,e])
    VEIf c a b -> scans (map (exprScan ws) [c,a,b])
    VEFctCall f es -> scans (protectAll (idScan ws f) : map (exprScan ws) es)
    VEMacro _ -> emptyScan

lvalueScan :: SignalWidths -> VLValue -> Scan
lvalueScan ws (VLId i) = idScan ws i
lvalueScan ws (VLConcat lvs) = scans (map (lvalueScan ws) lvs)
lvalueScan ws (VLSub lv e) = scans [lvalueScan ws lv,exprScan ws e]

eventScan :: SignalWidths -> VEventExpr -> Scan
eventScan ws (VEEOr a b) = scans [eventScan ws a,eventScan ws b]
eventScan ws (VEEposedge e) = exprScan ws e
eventScan ws (VEEnegedge e) = exprScan ws e
eventScan ws (VEEMacro _ e) = exprScan ws e
eventScan ws (VEE e) = exprScan ws e

edgeEvent :: VEventExpr -> Bool
edgeEvent (VEEOr a b) = edgeEvent a || edgeEvent b
edgeEvent (VEE _) = False
edgeEvent _ = True

-- Each statement body is summarized once. Parents reuse the names, writes,
-- and definite-assignment result instead of rescanning every nested subtree.
data StatementScan = StatementScan
    { statementScan :: Scan
    , statementDefinite :: Maybe (S.Set String)
    }

statementWrites :: StatementScan -> S.Set String
statementWrites = scanWrites . statementScan

stmtScan :: SignalWidths -> VStmt -> StatementScan
stmtScan ws stmt = case stmt of
    VAt ev body ->
        let b = stmtScan ws body
            scan = scans [eventScan ws ev,statementScan b]
        in StatementScan (if edgeEvent ev then protect (scanNames (statementScan b)) scan else scan)
                         Nothing
    Valways (VAt ev body) ->
        let b = stmtScan ws body
            scan = scans [eventScan ws ev,statementScan b]
            blocked | edgeEvent ev = scanNames (statementScan b) `S.union` scanWrites scan
                    | otherwise = statementWrites b `S.difference` maybe S.empty id (statementDefinite b)
        in StatementScan (protect blocked scan) Nothing
    Valways body ->
        let b = stmtScan ws body
        in b { statementScan = protect (statementWrites b) (statementScan b), statementDefinite = Nothing }
    Vinitial body ->
        let b = stmtScan ws body
        in b { statementScan = protectAll (statementScan b), statementDefinite = Nothing }
    VSeq ss -> combine emptyScan (map (stmtScan ws) ss) (fmap S.unions . mapM statementDefinite)
    Vcase e arms _ _ -> caseScan e arms (binaryComplete ws e arms)
    Vcasex e arms _ _ -> caseScan e arms False
    VAssign lv e -> assignment lv e False
    VAssignA lv e -> assignment lv e True
    Vif e body -> combine (exprScan ws e) [stmtScan ws body] $ \bs ->
        mapM statementDefinite bs >> Just S.empty
    Vifelse e a b -> combine (exprScan ws e) (map (stmtScan ws) [a,b]) $ \bs ->
        case bs of [x,y] -> S.intersection <$> statementDefinite x <*> statementDefinite y
                   _ -> Nothing
    Vdumpvars _ ids -> leaf (scans (map (idScan ws) ids)) (Just S.empty)
    VTask f es -> leaf (scans (protectAll (idScan ws f) : map (exprScan ws) es)) (Just S.empty)
    VAssert ev es -> leaf (scans (eventScan ws ev : map (exprScan ws) es)) (Just S.empty)
    VZeroDelay -> leaf emptyScan Nothing
  where
    leaf s definite = StatementScan s definite
    assignment lv e nonblocking =
        let lhs = lvalueScan ws lv
            ns = scanNames lhs
            children = scans [lhs,exprScan ws e]
            scan = children { scanWrites = ns `S.union` scanWrites children }
            definite = case (nonblocking,lv) of
                (False,VLId i) -> Just (S.singleton (getVIdString i))
                _ -> Nothing
        in StatementScan (if nonblocking then protect ns scan else scan) definite
    combine extra bs definite = StatementScan
        (scans (extra : map statementScan bs)) (definite bs)
    caseScan e arms complete =
        let branch (VCaseArm es s) = (scans (map (exprScan ws) es),stmtScan ws s)
            branch (VDefault s) = (emptyScan,stmtScan ws s)
            branches = map branch arms
            isDefault (VDefault _) = True
            isDefault _ = False
            definite bs = do
                sets <- mapM statementDefinite bs
                return $ if (complete || any isDefault arms) && not (null sets)
                         then foldr1 S.intersection sets else S.empty
        in combine (scans (exprScan ws e : map fst branches)) (map snd branches) definite

-- BSC's PrimCase lowering omits a default only for exhaustive binary
-- coverage (AVerilogUtil.vDefMpd). X/Z selector behavior is a don't-care in
-- that combinational contract. Recognize this structurally rather than
-- treating the missing default as state or trusting a full_case annotation.
-- Emission still preserves the original case and every arm exactly.
type SignalWidths = M.Map String (Integer, Bool)

signalWidths :: VProgram -> SignalWidths
signalWidths (VProgram ms _ _) = M.mapMaybe id $ M.fromListWith agree $
    concatMap moduleWidths ms
  where
    agree a b | a == b = a
              | otherwise = Nothing
    moduleWidths m = concatMap argWidth (concatMap fst (vm_ports m)) ++
                     concatMap itemWidths (vm_body m)
    argWidth (VAInput v r) = [(getVIdString v, rangeWidth r)]
    argWidth (VAOutput v r) = [(getVIdString v, rangeWidth r)]
    argWidth (VAInout v _ (Just r)) = [(getVIdString v, rangeWidth r)]
    argWidth _ = []
    itemWidths (VMDecl (VVDecl _ r vs)) =
        [(getVIdString v, rangeWidth r) | VVar v <- vs]
    itemWidths (VMDecl (VVDWire r (VVar v) _)) = [(getVIdString v, rangeWidth r)]
    itemWidths (VMComment _ x) = itemWidths x
    itemWidths (VMRegGroup _ _ _ x) = itemWidths x
    itemWidths (VMGroup _ xs) = concatMap itemWidths (concat xs)
    itemWidths _ = []
    rangeWidth Nothing = Just (1, True)
    rangeWidth (Just (hi,lo)) = do
        h <- constantInteger hi
        l <- constantInteger lo
        return (abs (h-l) + 1, h >= l && l == 0)

constantInteger :: VExpr -> Maybe Integer
constantInteger (VEConst n) = Just n
constantInteger (VEWConst _ _ _ n) = Just n
constantInteger _ = Nothing

binaryComplete :: SignalWidths -> VExpr -> [VCaseArm] -> Bool
binaryComplete ws e arms = case (width e, mapM literal labels) of
    (Just w, Just values) ->
        -- Bound the exponent before constructing it; never enumerate a domain.
        w > 0 && w <= toInteger (length values) &&
        all (\(n,v) -> n == w && v >= 0 && v < 2^w) values &&
        S.size (S.fromList (map snd values)) == length values &&
        toInteger (length values) == 2^w
    _ -> False
  where
    labels = concat [es | VCaseArm es _ <- arms]
    width (VEVar v) = fmap fst (M.lookup (getVIdString v) ws)
    width (VEWConst _ w _ _) = Just w
    width (VESelect (VEVar v) hi lo) = do
        w <- indexedWidth v
        h <- constantInteger hi
        l <- constantInteger lo
        if 0 <= l && l <= h && h < w then Just (h-l+1) else Nothing
    width (VESelect1 (VEVar v) index) = do
        w <- indexedWidth v
        i <- constantInteger index
        if 0 <= i && i < w then Just 1 else Nothing
    width _ = Nothing
    indexedWidth v = case M.lookup (getVIdString v) ws of
        Just (w,True) -> Just w
        _ -> Nothing
    literal (VEWConst _ w base n) | base `elem` [2,8,10,16] = Just (w,n)
    literal _ = Nothing
