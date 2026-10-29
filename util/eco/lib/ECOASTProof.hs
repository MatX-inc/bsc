-- Independent complete-inventory checker over the compiler's captured Verilog
-- AST.  This module neither parses RTL nor imports candidate transformations.
module ECOASTProof
  ( ProofDomain(..), ProofIssue(..), ProofBundle(..), ProofResult(..)
  , ChildDirection(..), ChildPin(..), checkASTWithPins, runASTProofWithPins
  , checkAST, runASTProof, renderProofResult, expressionWidth
  , ASTContract(..), runASTProofWithContracts, readASTContracts, writeASTContracts
  , renameASTContracts
  ) where

import Prelude hiding (reads)
import Control.Exception (IOException, try)
import Control.Monad (foldM, unless)
import qualified Data.Generics as G
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.List (intercalate, isInfixOf, sort, sortOn)
import Data.Maybe (catMaybes)
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(..))
import System.FilePath ((</>))
import System.Process (proc, readCreateProcessWithExitCode, cwd)
import System.Timeout (timeout)
import PPrint (ppReadable)
import Verilog
import BinData (encode,decode)
import GenABin ()

data ProofDomain = StrictFourState | Binary deriving (Eq, Ord, Read, Show)
data ChildDirection = ChildInput | ChildOutput | ChildInout deriving (Eq,Ord,Read,Show)
data ChildPin = ChildPin { pinInstance :: String, pinFormal :: String,
  pinDirection :: ChildDirection, pinWidth :: Maybe Integer }
  deriving (Eq,Ord,Read,Show)
data ProofIssue = ProofIssue { issueCode :: String, issueDetail :: String }
  deriving (Eq, Read, Show)
data ProofBundle = ProofBundle
  { proofDomain :: ProofDomain
  , proofInventory :: [(String,Int)]
  , proofStructuralRoots :: [String]
  , proofResidualRoots :: [String]
  , proofMiter :: Maybe VProgram
  } deriving (Show)
data ProofResult = ProofResult
  { proofPassed :: Bool, proofStatus :: String, proofIssues :: [ProofIssue]
  , proofChecked :: Maybe ProofBundle, proofSolverLog :: Maybe FilePath
  , proofObligations :: [(String,ProofResult)]
  } deriving (Show)

-- These are requests to prove propositions, never assumptions. A type-validity
-- annotation or a reset/environment promise does not discharge a descriptor.
data ASTContract
  = AssertCombinational String VExpr
  | SpecializeUnreachableXArm String String VExpr
  deriving (Eq,Show)

-- Rebinding is not evidence: all propositions are discharged again after this
-- signal-only traversal. Callable names and descriptor IDs remain unchanged.
renameASTContracts :: [(String,String)] -> [ASTContract] -> [ASTContract]
renameASTContracts relation = map rename
  where
    mapping=M.fromList relation
    rename (AssertCombinational ident predicate)=AssertCombinational ident (renameExpr mapping predicate)
    rename (SpecializeUnreachableXArm ident target predicate)=
      SpecializeUnreachableXArm ident (M.findWithDefault target target mapping) (renameExpr mapping predicate)

-- Existing native AST codec, not expression text or an RTL parser. The outer
-- replay plan binds this artifact's bytes and re-runs every obligation.
writeASTContracts :: FilePath -> [ASTContract] -> IO ()
writeASTContracts path cs = BS.writeFile path (BS.pack (encode (1::Int,map row cs)))
  where
    row (AssertCombinational ident predicate)=(0::Int,ident,"",predicate)
    row (SpecializeUnreachableXArm ident target predicate)=(1::Int,ident,target,predicate)
readASTContracts :: FilePath -> IO [ASTContract]
readASTContracts path = do
  bytes<-BS.readFile path
  let (version,rows)=decode bytes :: (Int,[(Int,String,String,VExpr)])
  if version/=1 then ioError (userError "Unsupported AST contract schema") else mapM row rows
  where
    row (0,ident,"",predicate)=return (AssertCombinational ident predicate)
    row (1,ident,target,predicate)=return (SpecializeUnreachableXArm ident target predicate)
    row _=ioError (userError "Unsupported AST contract descriptor")

type Check a = Either [ProofIssue] a
type Name = String
type Widths = M.Map Name Integer
data Decl = Decl VDType (Maybe VRange) VVar deriving (Eq, Show)
data Driver = Continuous VExpr | Process VEventExpr VStmt [Name]
  deriving (Eq, Show)
data Inventory = Inventory
  { invModule :: VModule, invWidths :: Widths, invDecls :: M.Map Name Decl
  , invDrivers :: M.Map Name Driver, invOpaque :: [VMItem]
  , invChildren :: M.Map Name VMItem, invAllNames :: S.Set Name
  } deriving (Show)

bad :: String -> String -> Check a
bad k s = Left [ProofIssue k s]
ensure :: Bool -> String -> String -> Check ()
ensure b k s = unless b (bad k s)
name :: VId -> Name
name = getVIdString

-- VId stores original inlined-instance provenance as Maybe VMItem. That is
-- not emitted RTL and may refer to helpers no longer present. Semantic walks
-- must stop at the identifier rather than recursively inventing dependencies
-- from its diagnostic attachment.
semanticValues :: (G.Data a,G.Typeable b) => a -> [b]
semanticValues x = maybe [] (:[]) (G.cast x) ++
  if identifier x then [] else concat (G.gmapQ semanticValues x)
  where
    identifier value=case G.cast value :: Maybe VId of Just _->True;Nothing->False
integer :: VExpr -> Maybe Integer
integer (VEConst n) = Just n
integer (VEWConst _ _ _ n) = Just n
integer _ = Nothing
rangeWidth :: Maybe VRange -> Maybe Integer
rangeWidth Nothing = Just 1
rangeWidth (Just (h,l)) = do
  hi <- integer h; lo <- integer l
  if lo == 0 && hi >= lo then Just (hi-lo+1) else Nothing

-- Only expression identifiers count as signal references. Operator source IDs,
-- module names, formal pin labels and function names are never alpha-renamed.
refs :: VExpr -> S.Set Name
refs e = case e of
  VEVar v -> S.singleton (name v)
  VEIndex v x -> S.insert (name v) (refs x)
  VEUnOp _ _ x -> refs x
  VEOp _ a _ b -> refs a `S.union` refs b
  VEConcat xs -> S.unions (map refs xs)
  VESelect x h l -> S.unions (map refs [x,h,l])
  VESelect1 x i -> refs x `S.union` refs i
  VERepeat n x -> refs n `S.union` refs x
  VEIf c t f -> S.unions (map refs [c,t,f])
  VEFctCall _ xs -> S.unions (map refs xs)
  _ -> S.empty
lvNames :: VLValue -> S.Set Name
lvNames (VLId v) = S.singleton (name v)
lvNames (VLConcat xs) = S.unions (map lvNames xs)
lvNames (VLSub x _) = lvNames x
writes :: VStmt -> S.Set Name
writes s = case s of
  VAssign l _ -> lvNames l
  VAssignA l _ -> lvNames l
  VSeq xs -> S.unions (map writes xs)
  Vif _ x -> writes x
  Vifelse _ x y -> writes x `S.union` writes y
  Vcase _ as _ _ -> S.unions (map (writes . armBody) as)
  Vcasex _ as _ _ -> S.unions (map (writes . armBody) as)
  VAt _ x -> writes x
  Valways x -> writes x
  Vinitial x -> writes x
  _ -> S.empty
reads :: VStmt -> S.Set Name
reads s = case s of
  VAssign _ x -> refs x
  VAssignA _ x -> refs x
  VSeq xs -> S.unions (map reads xs)
  Vif c x -> refs c `S.union` reads x
  Vifelse c x y -> S.unions [refs c,reads x,reads y]
  Vcase c as _ _ -> refs c `S.union` S.unions (map armReads as)
  Vcasex c as _ _ -> refs c `S.union` S.unions (map armReads as)
  VAt _ x -> reads x
  Valways x -> reads x
  Vinitial x -> reads x
  _ -> S.empty
armReads :: VCaseArm -> S.Set Name
armReads (VCaseArm labels body) = S.unions (reads body : map refs labels)
armReads (VDefault body) = reads body

armBody :: VCaseArm -> VStmt
armBody (VCaseArm _ x) = x
armBody (VDefault x) = x
edge :: VEventExpr -> Bool
edge (VEE _) = False
edge (VEEOr a b) = edge a || edge b
edge _ = True
eventRefs :: VEventExpr -> S.Set Name
eventRefs (VEEOr a b)=eventRefs a `S.union` eventRefs b
eventRefs (VEE e)=refs e
eventRefs _=S.empty

expressionWidth :: Widths -> VExpr -> Maybe Integer
expressionWidth ws e = case e of
  VEVar v -> M.lookup (name v) ws
  VEConst n | n >= 0 && n < 2^(31::Int) -> Just 32
  VEWConst _ w _ _ | w > 0 -> Just w
  VEUnknown w _ | w > 0 -> Just w
  VETriConst xs -> Just (toInteger (length xs))
  VEConcat xs -> sum <$> mapM (expressionWidth ws) xs
  VESelect x h l -> do
    w <- expressionWidth ws x; hi <- integer h; lo <- integer l
    if lo >= 0 && hi >= lo && hi < w then Just (hi-lo+1) else Nothing
  VESelect1 x i -> do
    w <- expressionWidth ws x; n <- integer i
    if n >= 0 && n < w then Just 1 else Nothing
  VERepeat n x -> do
    k <- integer n; w <- expressionWidth ws x
    if k > 0 && k <= 1000000 then Just (k*w) else Nothing
  VEIf _ t f -> max <$> expressionWidth ws t <*> expressionWidth ws f
  VEUnOp _ VNot _ -> Just 1
  VEUnOp _ _ x -> expressionWidth ws x
  VEOp _ a op b
    | op `elem` [VULT,VULE,VUGT,VUGE,VEQ,VNE,VEQ3,VNE3,VLAnd,VLOr] -> Just 1
    | op `elem` [VShL,VShR,VShLA,VShRA] -> expressionWidth ws a
    | otherwise -> max <$> expressionWidth ws a <*> expressionWidth ws b
  _ -> Nothing

binaryComplete :: Widths -> VExpr -> [VCaseArm] -> Bool
binaryComplete ws e as = case (expressionWidth ws e,mapM label labels) of
  (Just w,Just ns) -> w > 0 && w <= toInteger (length ns) &&
    toInteger (length ns) == 2^w && S.size (S.fromList ns) == length ns &&
    all (\(k,n) -> k == w && n >= 0 && n < 2^w) ns
  _ -> False
  where
    labels = concat [es | VCaseArm es _ <- as]
    label (VEWConst _ w _ n) = Just (w,n)
    label _ = Nothing

-- Definite assignment is checked without full_case/parallel_case promises.
definite :: ProofDomain -> Widths -> VStmt -> Maybe (S.Set Name)
definite _ _ (VAssign (VLId v) _) = Just (S.singleton (name v))
definite d ws (VSeq xs) = S.unions <$> mapM (definite d ws) xs
definite d ws (Vif _ x) = definite d ws x >> Just S.empty
definite d ws (Vifelse _ a b) = S.intersection <$> definite d ws a <*> definite d ws b
definite d ws (Vcase e as _ _) = do
  ds <- mapM (definite d ws . armBody) as
  if not (null ds) && (any isDefault as || d == Binary && binaryComplete ws e as)
    then Just (foldr1 S.intersection ds) else Just S.empty
  where isDefault (VDefault _) = True; isDefault _ = False
definite _ _ _ = Nothing

pureStatement :: VStmt -> Bool
pureStatement (VAssign (VLId _) _) = True
pureStatement (VSeq xs) = all pureStatement xs
pureStatement (Vif _ x) = pureStatement x
pureStatement (Vifelse _ x y) = pureStatement x && pureStatement y
pureStatement (Vcase _ as _ _) = all (pureStatement . armBody) as
pureStatement _ = False

flatten :: VMItem -> [VMItem]
flatten (VMComment _ x) = flatten x
flatten (VMRegGroup _ _ _ x) = flatten x
flatten (VMGroup False xs) = concatMap flatten (concat xs)
flatten x = [x]

inventory :: ProofDomain -> VModule -> Check Inventory
inventory domain m = do
  -- These two macros have definitions in Verilog's own VProgram printer.
  -- Arbitrary macro bodies have unobservable scope/effects and are unsupported.
  ensure (null [s | VEMacro s<-semanticValues m,s/="BSV_RESET_VALUE"] &&
          null [s | VEEMacro s _<-semanticValues m,s/="BSV_RESET_EDGE"])
    "unknown-macro-scope" "Only the printer-defined reset value/edge macros have a supported dependency inventory"
  decls <- foldM addDecl M.empty (portDecls ++ bodyDecls)
  let ws = M.mapMaybe (\(Decl _ r v) -> case v of VVar _ -> rangeWidth r; _ -> Nothing) decls
      base = Inventory m ws decls M.empty [] M.empty (M.keysSet decls)
  inv <- foldM addItem base items
  let drivers = invDrivers inv
  mapM_ (\(n,d) -> do
    ensure (M.member n decls) "undeclared-destination" n
    ensure (M.member n ws) "unsupported-width" n
    ensure (all (`M.member` decls) (S.toList (driverReads d))) "undeclared-dependency" n
    ensure (null [() | VEFctCall _ _ <- semanticValues (driverItem n d)]) "effectful-expression" n) (M.toList drivers)
  _ <- foldM (visit drivers S.empty) S.empty (M.keys drivers)
  let opaqueWrites=S.unions [writes s | VMStmt _ s<-invOpaque inv]
      boundaryReads=S.unions [refs e | x<-invOpaque inv++M.elems (invChildren inv),
                                      not (functionScope x),
                                      e<-semanticValues x]
      parameters=S.fromList [name v | VAParameter v _ _<-concatMap fst (vm_ports m)]
  ensure (S.null (opaqueWrites `S.intersection` M.keysSet drivers)) "multiple-driver" "Pure and opaque processes write the same destination"
  ensure (boundaryReads `S.isSubsetOf` (M.keysSet decls `S.union` parameters)) "undeclared-boundary-dependency"
    (intercalate ", " (S.toAscList (boundaryReads `S.difference` (M.keysSet decls `S.union` parameters))))
  mapM_ (\n -> ensure (isReg inv n) "state-declaration" n) (S.toList opaqueWrites)
  return inv
  where
    items = concatMap flatten (vm_body m)
    portDecls = concatMap argDecl (concatMap fst (vm_ports m))
    argDecl (VAInput v r) = [(name v,Decl VDInput r (VVar v))]
    argDecl (VAOutput v r) = [(name v,Decl VDOutput r (VVar v))]
    argDecl (VAInout v _ (Just r)) = [(name v,Decl VDInout r (VVar v))]
    argDecl (VAParameter _ _ _) = []
    argDecl _ = []
    bodyDecls = concatMap declsOf items
    declsOf (VMDecl (VVDecl t r vs)) = [(name (vvName v),Decl t r v) | v <- vs]
    declsOf (VMDecl (VVDWire r v _)) = [(name (vvName v),Decl VDWire r v)]
    declsOf _ = []
    addDecl ds (n,d) = case M.lookup n ds of
      Nothing -> Right (M.insert n d ds)
      Just (Decl old r v) -> case d of
        Decl new r' v' | r == r' && v == v' && old == VDOutput && new `elem` [VDReg,VDWire] -> Right (M.insert n d ds)
        _ -> bad "duplicate-declaration" n
    addDriver inv n d = do
      ensure (M.notMember n (invDrivers inv)) "multiple-driver" n
      return inv { invDrivers=M.insert n d (invDrivers inv) }
    addItem inv x = case x of
      VMDecl (VVDWire _ (VVar v) e) -> addDriver inv (name v) (Continuous e)
      VMDecl (VVDWire _ _ _) -> bad "array-initializer" "Array initializer requires dedicated proof"
      VMDecl _ -> Right inv
      VMAssign (VLId v) e -> addDriver inv (name v) (Continuous e)
      VMAssign _ _ -> bad "partial-continuous-write" "Only whole scalar continuous destinations are supported"
      VMStmt False (Valways (VAt ev s))
        | not (edge ev), let ns=writes s, not (S.null ns)
        , Just ds <- definite domain (invWidths inv) s, ns `S.isSubsetOf` ds
        , S.null (reads s `S.intersection` ns)
        , reads s `S.isSubsetOf` eventRefs ev -> do
            ensure (all (isReg inv) (S.toList ns)) "procedural-declaration" (show (S.toList ns))
            foldM (\a n -> addDriver a n (Process ev s (S.toAscList ns))) inv (S.toAscList ns)
      VMInst _ v _ _ -> do
        ensure (M.notMember (name v) (invChildren inv)) "duplicate-instance" (name v)
        return inv { invChildren=M.insert (name v) x (invChildren inv) }
      _ -> Right inv { invOpaque=invOpaque inv ++ [x] }
    isReg inv n = case M.lookup n (invDecls inv) of Just (Decl VDReg _ _) -> True; _ -> False
    functionScope VMFunction {}=True
    functionScope _=False
    visit ds active done n
      | n `S.member` active = bad "combinational-cycle" n
      | n `S.member` done = Right done
      | otherwise = case M.lookup n ds of
          Nothing -> Right done
          Just d -> S.insert n <$> foldM (visit ds (S.insert n active)) done (S.toList (driverReads d))

driverReads :: Driver -> S.Set Name
driverReads (Continuous e) = refs e
driverReads (Process ev s _) = reads s `S.union` eventRefs ev

driverItem :: Name -> Driver -> VMItem
driverItem n (Continuous e) = VMAssign (VLId (mkVId n)) e
driverItem _ (Process ev s _) = VMStmt False (Valways (VAt ev s))

-- A no-default process retains its previous result for unhandled values. It
-- must not be converted into a total combinational function. We can still
-- compare its unchanged syntax through exact-width, whole-vector continuous
-- aliases: these preserve every bit, including X/Z. Normalize read positions
-- and level-sensitive events only; destinations, edge/initial/effect blocks,
-- case structure and the presence/absence of a default remain untouched.
-- Inventory has already rejected cycles and multiple drivers. Replacing the
-- opaque inventory also makes its terminal alias dependencies proof roots,
-- rather than leaving an unproved cut at an added/removed helper spelling.
normalizeOpaqueAliases :: Inventory -> Inventory
normalizeOpaqueAliases inv = inv {invOpaque=map item (invOpaque inv)}
  where
    direct=M.mapMaybeWithKey alias (invDrivers inv)
    alias target (Continuous (VEVar source))
      | Just w<-M.lookup target (invWidths inv)
      , M.lookup (name source) (invWidths inv)==Just w = Just (name source)
    alias _ _=Nothing
    terminal seen n
      | n `S.member` seen=n
      | Just next<-M.lookup n direct=terminal (S.insert n seen) next
      | otherwise=n
    mapping=M.map (terminal S.empty) direct
    expr=renameExpr mapping
    item (VMStmt False (Valways (VAt ev s)))
      | not (edge ev),pureStatement s=
          VMStmt False (Valways (VAt (renameEvent mapping ev) (statement s)))
    item x=x
    statement (VAssign l e)=VAssign l (expr e)
    statement (VSeq xs)=VSeq (map statement xs)
    statement (Vif c s)=Vif (expr c) (statement s)
    statement (Vifelse c s t)=Vifelse (expr c) (statement s) (statement t)
    statement (Vcase c arms p f)=Vcase (expr c) (map arm arms) p f
    statement s=s
    arm (VCaseArm labels s)=VCaseArm (map expr labels) (statement s)
    arm (VDefault s)=VDefault (statement s)

-- Duplicate incomplete combinational processes are not total functions.  A
-- fresh, uninitialized scalar Verilog reg starts at all-X.  Two such regs with
-- identical level events and blocking-assignment bodies therefore remain equal:
-- each event assigns identical bits, or both retain their equal prior values.
-- This is a common-Verilog-start/history argument for settled observations,
-- not equality of arbitrarily independent retained-state cuts.  Keep one
-- actual process as the history boundary and use checker-only aliases for its
-- duplicates.  The incoming complete cones remain ordinary proof obligations.
--
-- Deliberately exclude extra writers/initializers, ports, effects, functions,
-- child connections, incomplete sensitivity, and feedback through any retained
-- combinational process.  Case/default syntax and exact event syntax are part
-- of the key; no unspecified/default value is turned into proof freedom.
collapseDuplicateHistories :: Inventory -> Inventory -> (Inventory,Inventory,[Name])
collapseDuplicateHistories a b = (collapse a pairsA,collapse b pairsB,evidence)
  where
    ga=groups a; gb=groups b
    matches=[(minimum common,as,bs) | (key,as)<-M.toAscList ga,
      Just bs<-[M.lookup key gb],
      let common=S.toAscList (S.fromList (map fst as) `S.intersection` S.fromList (map fst bs)),
      not (null common),length as>1 || length bs>1]
    pairsA=[(n,anchor,i) | (anchor,as,_)<-matches,(n,i)<-as,n/=anchor]
    pairsB=[(n,anchor,i) | (anchor,_,bs)<-matches,(n,i)<-bs,n/=anchor]
    evidence=["history-alias:"++side++":"++n++"="++anchor |
      (side,ps)<-[("reference",pairsA),("candidate",pairsB)],(n,anchor,_)<-ps]
    collapse inv pairs=inv
      { invOpaque=[x | (i,x)<-zip [0::Int ..] (invOpaque inv),i `notElem` [j | (_,_,j)<-pairs]]
      , invDrivers=foldr (\(n,anchor,_) -> M.insert n (Continuous (VEVar (mkVId anchor))))
          (invDrivers inv) pairs }
    groups inv=M.fromListWith (++)
      [(key,[(target,index)]) | (index,x)<-zip [0::Int ..] (invOpaque inv),
        Just (target,key)<-[eligible inv index x]]
    eligible inv index (VMStmt False (Valways (VAt ev stmt))) = do
      [target]<-Just (S.toList (writes stmt))
      Decl VDReg range (VVar _)<-M.lookup target (invDecls inv)
      width<-rangeWidth range
      let ds=invDrivers inv
          ports=S.fromList [name (vargName x) | x<-concatMap fst (vm_ports (invModule inv))]
          retained=S.unions [writes s | VMStmt False (Valways (VAt event s))<-invOpaque inv,not (edge event)]
          dependencies=closure ds (reads stmt `S.union` eventRefs ev)
          other=[x | (j,x)<-zip [0::Int ..] (invOpaque inv),j/=index]++M.elems (invChildren inv)
          otherNames=S.unions [closure ds (S.fromList [name v | v<-semanticValues x]) | x<-other]
          childNames=S.fromList [name v | x<-M.elems (invChildren inv),v<-semanticValues x]
          functionNames=S.fromList [name v | x@(VMFunction _)<-invOpaque inv,v<-semanticValues x]
          parameterNames=S.fromList [name v | VAParameter v _ _<-concatMap fst (vm_ports (invModule inv))]
          effects=[() | x<-invOpaque inv,statement<-semanticValues x,unsupportedEffect statement]
          functions=[() | x<-invOpaque inv,VMFunction _<-semanticValues x]
          calls=[() | x<-invOpaque inv,VEFctCall _ _<-semanticValues x]
          valid=null effects && null functions && null calls &&
            not (edge ev) && simpleEvent ev && pureStatement stmt &&
            not (S.null (eventRefs ev)) && reads stmt `S.isSubsetOf` eventRefs ev &&
            target `S.notMember` ports && M.notMember target ds &&
            target `S.notMember` otherNames && S.null (dependencies `S.intersection` retained) &&
            S.null (dependencies `S.intersection` (childNames `S.union` functionNames `S.union` parameterNames)) &&
            all (`M.member` invWidths inv) (S.toList dependencies) &&
            exactAssignments inv target width stmt && all semanticExpr (semanticValues stmt)
      if valid then Just (target,encode (width,canonical ev,
                         canonical (renameStmt (M.singleton target "__eco_history_destination") stmt)))
        else Nothing
    eligible _ _ _=Nothing
    unsupportedEffect VTask {}=True
    unsupportedEffect VAssert {}=True
    unsupportedEffect Vdumpvars {}=True
    unsupportedEffect VZeroDelay=True
    unsupportedEffect _=False
    simpleEvent (VEE (VEVar _))=True
    simpleEvent (VEEOr x y)=simpleEvent x && simpleEvent y
    simpleEvent _=False
    closure ds names=go S.empty (S.toList names)
      where
        go seen []=seen
        go seen (n:ns)
          | n `S.member` seen=go seen ns
          | otherwise=go (S.insert n seen)
              (maybe [] (S.toList . driverReads) (M.lookup n ds)++ns)
    exactAssignments inv target width s=case s of
      VAssign (VLId v) rhs->name v==target && expressionWidth (invWidths inv) rhs==Just width
      VSeq xs->all recur xs
      Vif _ x->recur x
      Vifelse _ x y->recur x && recur y
      Vcase _ arms _ _->all (recur . armBody) arms
      _->False
      where recur=exactAssignments inv target width
    -- Source IDs and inlined-instance attachments are not emitted semantics.
    -- Stop at VId, so diagnostic metadata cannot introduce a spurious driver.
    canonical :: G.Data t => t -> t
    canonical x=case G.cast x :: Maybe VId of
      Just v->case G.cast (mkVId (name v)) of Just y->y;Nothing->x
      Nothing->G.gmapT canonical x

-- Local alpha traversal deliberately independent of the transformation layer.
renameExpr :: M.Map Name Name -> VExpr -> VExpr
renameExpr ren e = case e of
  VEVar v -> VEVar (rv v)
  VEIndex v x -> VEIndex (rv v) (go x)
  VEUnOp v op x -> VEUnOp v op (go x)
  VEOp v a op b -> VEOp v (go a) op (go b)
  VEConcat xs -> VEConcat (map go xs)
  VESelect x h l -> VESelect (go x) (go h) (go l)
  VESelect1 x i -> VESelect1 (go x) (go i)
  VERepeat n x -> VERepeat (go n) (go x)
  VEIf c t f -> VEIf (go c) (go t) (go f)
  VEFctCall f xs -> VEFctCall f (map go xs)
  _ -> e
  where
    rv v = maybe v mkVId (M.lookup (name v) ren)
    go = renameExpr ren
renameLValue :: M.Map Name Name -> VLValue -> VLValue
renameLValue r (VLId v) = VLId (maybe v mkVId (M.lookup (name v) r))
renameLValue r (VLConcat xs) = VLConcat (map (renameLValue r) xs)
renameLValue r (VLSub x e) = VLSub (renameLValue r x) (renameExpr r e)
renameEvent :: M.Map Name Name -> VEventExpr -> VEventExpr
renameEvent r e = case e of
  VEEOr a b -> VEEOr (go a) (go b)
  VEEposedge x -> VEEposedge (re x)
  VEEnegedge x -> VEEnegedge (re x)
  VEE x -> VEE (re x)
  VEEMacro s x -> VEEMacro s (re x)
  where go=renameEvent r; re=renameExpr r
renameStmt :: M.Map Name Name -> VStmt -> VStmt
renameStmt r s = case s of
  VAt e x -> VAt (renameEvent r e) (go x)
  Valways x -> Valways (go x)
  Vinitial x -> Vinitial (go x)
  VSeq xs -> VSeq (map go xs)
  Vcase e as p f -> Vcase (re e) (map arm as) p f
  Vcasex e as p f -> Vcasex (re e) (map arm as) p f
  VAssign l e -> VAssign (renameLValue r l) (re e)
  VAssignA l e -> VAssignA (renameLValue r l) (re e)
  Vif e x -> Vif (re e) (go x)
  Vifelse e x y -> Vifelse (re e) (go x) (go y)
  VTask f xs -> VTask f (map re xs)
  VAssert ev xs -> VAssert (renameEvent r ev) (map re xs)
  Vdumpvars d xs -> Vdumpvars d (map (\v -> maybe v mkVId (M.lookup (name v) r)) xs)
  _ -> s
  where
    go=renameStmt r; re=renameExpr r
    arm (VCaseArm es x) = VCaseArm (map re es) (go x)
    arm (VDefault x) = VDefault (go x)
renameItem :: M.Map Name Name -> VMItem -> VMItem
renameItem r x = case x of
  VMDecl (VVDecl t mr vs) -> VMDecl (VVDecl t (fmap rr mr) (map var vs))
  VMDecl (VVDWire mr v e) -> VMDecl (VVDWire (fmap rr mr) (var v) (re e))
  VMAssign l e -> VMAssign (renameLValue r l) (re e)
  VMStmt t s -> VMStmt t (renameStmt r s)
  VMInst mn i ps cs -> VMInst mn i (case ps of {Left xs -> Left [(p,re e) | (p,e)<-xs]; Right xs -> Right [(p,fmap re e) | (p,e)<-xs]}) [(p,fmap re e) | (p,e)<-cs]
  VMComment c y -> VMComment c (go y)
  VMRegGroup v s c y -> VMRegGroup v s c (go y)
  VMGroup t ys -> VMGroup t (map (map go) ys)
  _ -> x -- functions/DPI remain exact opaque items; maps into these are rejected
  where
    go=renameItem r; re=renameExpr r; rr (h,l)=(re h,re l)
    rv v=maybe v mkVId (M.lookup (name v) r)
    var (VVar v)=VVar (rv v)
    var (VArray z v)=VArray (rr z) (rv v)

-- Hash-consed typed normal forms avoid exponential expansion of shared DAGs.
-- No Boolean two-state identities are used in this four-state normalization.
data Node = Node Integer String [Integer] [Int] deriving (Eq,Ord,Show)
data Norm = Norm (M.Map Node Int) (M.Map (Int,Name) Int) (M.Map Int Node) deriving Show
emptyNorm :: Norm
emptyNorm = Norm M.empty M.empty M.empty
intern :: Node -> Norm -> (Int,Norm)
intern original st@(Norm ns memo byId) = case simplify original of
  Left i -> (i,st)
  Right simplified -> let x=logicalAC simplified in case M.lookup x ns of
    Just i -> (i,st)
    Nothing -> let i=M.size ns in (i,Norm (M.insert x i ns) memo (M.insert i x byId))
  where
    -- Logical conjunction/disjunction are associative and commutative over
    -- Verilog truth values (0, 1, X); a raw Z is coerced to X by the operator.
    -- Keep the operator, every typed child, and duplicate occurrences. Counts
    -- avoid exponential expansion of shared h[n] = h[n-1] && h[n-1] DAGs.
    -- Only fresh width-one, parameter-free logical nodes enter this form;
    -- no bitwise operator, resize, singleton elimination or distribution does.
    logicalAC original@(Node 1 k [] ids)
      | k `elem` ["VLAnd","VLOr"],length ids==2 =
          let occurrences=M.unionsWith (+) (map collect ids)
              ordered=M.toAscList occurrences
          in Node 1 k (map snd ordered) (map fst ordered)
      where
        collect i=case M.lookup i byId of
          Just (Node 1 childKind counts children)
            | childKind==k,not(null counts),length counts==length children,
              all (>0) counts -> M.fromListWith (+) (zip children counts)
          _ -> M.singleton i 1
    logicalAC original=original
    constant i=case M.lookup i byId of Just (Node _ "constant" [n] [])->Just n;_->Nothing
    truth n=n/=0
    boolean n=Node 1 "constant" [if n then 1 else 0] []
    -- A one-bit wire may carry Z; 0 || wire is then X, not Z. Only reuse
    -- expressions whose Verilog operators cannot produce a Z result.
    truthValue i=case M.lookup i byId of
      Just (Node 1 k _ _) -> k `elem` ["constant","VNot","VLAnd","VLOr","VULT","VULE","VUGT","VUGE","VEQ","VNE","VEQ3","VNE3"]
      _ -> False
    -- Selecting one complete component of an exactly sized concatenation
    -- copies its bits, including X and Z. Do not look through resize nodes:
    -- truncation/extension and partial/cross-component slices stay obligations.
    concatField w hi lo i=do
      Node total "concat" [] parts<-M.lookup i byId
      widths<-mapM (\p->case M.lookup p byId of
        Just (Node n _ _ _) | n>0->Just n
        _->Nothing) parts
      if null parts || sum widths/=total || lo<0 || hi>=total ||
         hi<lo || w/=hi-lo+1 then Nothing else
        case [p | (p,(n,offset))<-zip parts (zip widths (tail (scanr (+) 0 widths))),
                  lo==offset,hi==offset+n-1] of
          [p]->Just p
          _->Nothing
    -- A comparison result is one bit and never Z. Complement only this
    -- already-typed node: retain the exact context-sized operand IDs, rather
    -- than re-normalizing its source expressions in a new sizing context.
    -- Relational comparisons propagate X/Z. Ordinary equality can also
    -- resolve a known mismatching bit, but == and != still give complementary
    -- known results or both X, and !X is X. Keep case equality distinct.
    -- Unsupported signed/unsized forms
    -- still fail in normExpr; this rule adds no interpretation for them.
    relationalComplement i=do
      Node 1 op [] [a,b]<-M.lookup i byId
      opposite<-lookup op [("VULT","VUGE"),("VULE","VUGT"),
                           ("VUGT","VULE"),("VUGE","VULT"),
                           ("VEQ","VNE"),("VNE","VEQ")]
      Node aw _ _ _<-M.lookup a byId
      Node bw _ _ _<-M.lookup b byId
      if aw>0 && aw==bw then Just (Node 1 opposite [] [a,b]) else Nothing
    simplify x@(Node w k params ids)=case (k,ids) of
      ("select",[i]) | [hi,lo]<-params,Just p<-concatField w hi lo i -> Left p
      ("resize",[i]) | Just n<-constant i -> Right (Node w "constant" [n `mod` 2^w] [])
      ("truncate",[i]) | Just n<-constant i -> Right (Node w "constant" [n `mod` 2^w] [])
      ("VNot",[i]) | Just n<-constant i -> Right (boolean (not (truth n)))
      ("VNot",[i]) | w==1,null params,Just opposite<-relationalComplement i -> Right opposite
      ("VLOr",[a,b])
        | Just n<-constant a,truth n -> Right (boolean True)
        | Just n<-constant b,truth n -> Right (boolean True)
        | Just 0<-constant a,truthValue b -> Left b
        | Just 0<-constant b,truthValue a -> Left a
      ("VLAnd",[a,b])
        | Just 0<-constant a -> Right (boolean False)
        | Just 0<-constant b -> Right (boolean False)
        | Just n<-constant a,truth n,truthValue b -> Left b
        | Just n<-constant b,truth n,truthValue a -> Left a
      (_, [a,b]) | Just av<-constant a,Just bv<-constant b -> case k of
        "VEQ"->Right (boolean (av==bv)); "VEQ3"->Right (boolean (av==bv))
        "VNE"->Right (boolean (av/=bv)); "VNE3"->Right (boolean (av/=bv))
        "VULT"->Right (boolean (av<bv)); "VULE"->Right (boolean (av<=bv))
        "VUGT"->Right (boolean (av>bv)); "VUGE"->Right (boolean (av>=bv))
        _->Right x
      _->Right x
normDefinition :: Int -> Inventory -> Name -> Norm -> Maybe (Int,Norm)
normDefinition side inv n st@(Norm _ memo _) = case M.lookup (side,n) memo of
  Just i -> Just (i,st)
  Nothing -> do
    w <- M.lookup n (invWidths inv)
    (i,Norm ns' memo' byId') <- case M.lookup n (invDrivers inv) of
      Nothing -> Just (intern (Node w ("leaf:"++n) [] []) st)
      Just (Continuous e) -> normExpr side inv w e st
      Just (Process _ s _) -> normStmt side inv n w s st
    return (i,Norm ns' (M.insert (side,n) i memo') byId')
normExpr :: Int -> Inventory -> Integer -> VExpr -> Norm -> Maybe (Int,Norm)
normExpr side inv expected e st = do
  natural <- expressionWidth (invWidths inv) e
  let width=max natural expected
      add w k is es z=Just (intern (Node w k is es) z)
      resize w (i,z) | w==natural = Just (i,z)
                     | otherwise = add w "resize" [natural] [i] z
      n w x z=normExpr side inv w x z
  case e of
    VEVar v -> normDefinition side inv (name v) st >>= resize expected
    VEWConst _ w _ value -> add expected "constant" [value `mod` 2^(min w expected)] [] st
    -- Identical literal unknowns remain identical in a structural proof. This
    -- never treats X/Z as a free value, and SAT still rejects unknown cones.
    VEUnknown _ _ -> add expected ("literal:"++show e) [] [] st
    VETriConst _ -> add expected ("literal:"++show e) [] [] st
    VEConst _ -> Nothing -- signed context sizing is left to actual RTL SAT
    VEConcat xs -> do
      (ids,z)<-foldM (\(is,a) x -> do w<-expressionWidth (invWidths inv) x; (i,b)<-n w x a; return (is++[i],b)) ([],st) xs
      resize expected (intern (Node natural "concat" [] ids) z)
    VESelect x hi lo -> do
      h<-integer hi; l<-integer lo; w<-expressionWidth (invWidths inv) x
      (i,z)<-n w x st
      resize expected (intern (Node natural "select" [h,l] [i]) z)
    VESelect1 x ix -> normExpr side inv expected (VESelect x ix ix) st
    VERepeat count x -> do
      k<-integer count; w<-expressionWidth (invWidths inv) x; (i,z)<-n w x st
      resize expected (intern (Node natural "repeat" [k] [i]) z)
    VEIf c t f -> do
      cw<-expressionWidth (invWidths inv) c
      (ci,z)<-n cw c st; (ti,z')<-n width t z; (fi,z'')<-n width f z'
      let result=if ti==fi then (ti,z'') else intern (Node width "conditional" [] [ci,ti,fi]) z''
      if expected==width then Just result else add expected "truncate" [width] [fst result] (snd result)
    VEUnOp _ op x -> do
      xw<-expressionWidth (invWidths inv) x
      (i,z)<-n (if op==VNot then xw else width) x st
      let result=intern (Node (if op==VNot then 1 else width) (show op) [] [i]) z
      if op==VNot then resize expected result
        else if expected==width then Just result else add expected "truncate" [width] [fst result] (snd result)
    VEOp _ a op b -> do
      aw<-expressionWidth (invWidths inv) a; bw<-expressionWidth (invWidths inv) b
      let logical=op `elem` [VLAnd,VLOr]
          relation=op `elem` [VULT,VULE,VUGT,VUGE,VEQ,VNE,VEQ3,VNE3]
          shift=op `elem` [VShL,VShR,VShLA,VShRA]
          wa=if logical then aw else if relation then max aw bw else width
          wb=if logical || shift then bw else wa
      (ai,z)<-n wa a st; (bi,z')<-n wb b z
      let out=if logical || relation then 1 else width
          result=intern (Node out (show op) [] [ai,bi]) z'
      if expected==out then Just result else add expected "resize" [out] [fst result] (snd result)
    _ -> Nothing
normStmt :: Int -> Inventory -> Name -> Integer -> VStmt -> Norm -> Maybe (Int,Norm)
normStmt side inv target width stmt st = case stmt of
  VAssign (VLId v) e | name v==target -> normExpr side inv width e st
  VSeq xs -> case filter (S.member target . writes) xs of [x]->go x st; _->Nothing
  Vifelse c a b -> do
    cw<-expressionWidth (invWidths inv) c
    (ci,z)<-normExpr side inv cw c st; (ai,z')<-go a z; (bi,z'')<-go b z'
    return (if ai==bi then (ai,z'') else intern (Node width "procedural-if" [] [ci,ai,bi]) z'')
  Vcase c arms _ _ -> do
    cw<-expressionWidth (invWidths inv) c
    let labels=concat [es | VCaseArm es _<-arms]
        valid (VEWConst _ w _ n)=w==cw && n>=0 && n<2^w
        valid _=False
    if all valid labels && S.size (S.fromList (map integer labels))==length labels
      then do
        (ci,z)<-normExpr side inv cw c st
        (pairs,defaults,z')<-foldM step ([],[],z) arms
        case defaults of
          [d] -> do
            -- A known case label establishes the selector's value in that arm.
            -- Keep the original default for every unlisted value, including X/Z.
            (conditioned,z'')<-foldM (conditionedArm c arms d) ([],z') pairs
            let nondefault=sort conditioned
            Just (intern (Node width "case" (map fst nondefault) (ci:d:map snd nondefault)) z'')
          _ -> Nothing
      else do
        -- Dynamic case labels are ordered four-state comparisons. Preserve
        -- first-match priority, every label, and the explicit default exactly;
        -- do not infer mutual exclusion or use full_case/parallel_case promises.
        [def]<-Just [body | VDefault body<-arms]
        (ci,z)<-normExpr side inv cw c st
        (di,z')<-go def z
        (sizes,values,z'')<-foldM (orderedArm cw) ([],[],z')
                             [(es,body) | VCaseArm es body<-arms]
        Just (intern (Node width "ordered-case" sizes (ci:di:values)) z'')
  _ -> Nothing
  where
    go=normStmt side inv target width
    step (ps,ds,z) arm = do
      (i,z')<-go (armBody arm) z
      case arm of
        VDefault _ -> return (ps,ds++[i],z')
        VCaseArm es _ -> do
          vs<-mapM integer es
          return (ps++[(v,i)|v<-vs],ds,z')
    orderedArm cw (sizes,values,z) (labels,body) = do
      if null labels || any ((/=Just cw) . expressionWidth (invWidths inv)) labels
        then Nothing else Just ()
      (ids,z')<-foldM (\(out,state) label -> do
        (i,next)<-normExpr side inv cw label state
        return (out++[i],next)) ([],z) labels
      (bi,z'')<-go body z'
      return (sizes++[toInteger (length labels)],values++ids++[bi],z'')
    conditionedArm selector arms defId (out,z) (value,armId)
      | armId==defId = Just (out,z)
      | VEVar variable<-selector
      , Just sw<-expressionWidth (invWidths inv) selector
      , [defStmt]<-[body | VDefault body<-arms]
      , [body]<-[body | VCaseArm labels body<-arms,Just value `elem` map integer labels] = do
          let local=inv {invDrivers=M.insert (name variable)
                           (Continuous (VEWConst (mkVId "") sw 2 value)) (invDrivers inv)}
              -- Facts are local to one label: retain shared node interning but
              -- never reuse or export memoized definitions from this context.
              under statement (Norm nodes memo indexed)=do
                (i,Norm nodes' _ indexed')<-normStmt side local target width statement
                                             (Norm nodes M.empty indexed)
                return (i,Norm nodes' memo indexed')
          (ai,z')<-under body z
          (di,z'')<-under defStmt z'
          return (if ai==di then out else out++[(value,ai)],z'')
      | otherwise=Just (out++[(value,armId)],z)

checkAST :: ProofDomain -> [(String,String)] -> VProgram -> VProgram -> Check ProofBundle
checkAST = checkASTWithPins [] []

checkASTWithPins :: [ChildPin] -> [ChildPin] -> ProofDomain -> [(String,String)] -> VProgram -> VProgram -> Check ProofBundle
checkASTWithPins refPins candPins domain alpha (VProgram [a] adpi _) (VProgram [b0] bdpi _) = do
  ensure (vm_name a==vm_name b0) "module-identity" "Module name changed"
  ensure (concatMap fst (vm_ports a)==concatMap fst (vm_ports b0)) "fixed-ports" "Module ports, order, widths or parameters changed"
  ensure (adpi==bdpi) "DPI-identity" "Program DPI declarations changed"
  raw<-inventory domain b0
  let sources=map fst alpha; targets=map snd alpha; sourceSet=S.fromList sources
      ports=S.fromList (map (name . vargName) (concatMap fst (vm_ports b0)))
  ensure (length sources==S.size sourceSet && length targets==S.size (S.fromList targets)) "alpha-bijection" "Duplicate source or target"
  ensure (sourceSet `S.isSubsetOf` invAllNames raw && S.null (sourceSet `S.intersection` ports)) "alpha-scope" "Only internal declared names may be mapped"
  ensure (S.null (S.fromList targets `S.intersection` (invAllNames raw `S.difference` sourceSet))) "alpha-collision" "Rename captures an unmoved name"
  ensure (all vIsValidIdent targets) "alpha-identifier" "Invalid target identifier"
  let protected=S.fromList ([name v | x<-concatMap flatten (vm_body b0), isFunction x,
                                    v<-semanticValues x] ++
                           [name v | v<-semanticValues bdpi])
  ensure (S.null ((sourceSet `S.union` S.fromList targets) `S.intersection` protected)) "alpha-function-scope" "Function/DPI identifiers require scope-specific correspondence"
  let b=b0 {vm_body=map (renameItem (M.fromList alpha)) (vm_body b0)}
  originalA<-normalizeOpaqueAliases <$> inventory domain a
  originalB<-normalizeOpaqueAliases <$> inventory domain b
  -- Foreign declarations may conceal stateful effects outside the local
  -- process inventory. They cannot justify a new common-history relation.
  let (ia,ib,historyAliases)=if domain==StrictFourState && null adpi && null bdpi
        then collapseDuplicateHistories originalA originalB else (originalA,originalB,[])
  sequential<-checkOpaque ia ib
  let da=invDecls ia; db=invDecls ib; ca=invDrivers ia; cb=invDrivers ib
      common=M.keysSet da `S.intersection` M.keysSet db
      extraA=M.keysSet da `S.difference` M.keysSet db
      extraB=M.keysSet db `S.difference` M.keysSet da
  mapM_ (\n -> ensure (sameDecl (n `M.member` ca && n `M.member` cb) (da M.! n) (db M.! n)) "declaration-type" n) (S.toList common)
  ensure (extraA `S.isSubsetOf` M.keysSet ca && extraB `S.isSubsetOf` M.keysSet cb) "unmatched-boundary" "Added or removed state/input/child leaf lacks correspondence"
  ensure (S.null (((M.keysSet ca `S.difference` M.keysSet cb) `S.union` (M.keysSet cb `S.difference` M.keysSet ca)) `S.intersection` common)) "driver-boundary-change" "Common symbol changed between combinational driver and boundary"
  children<-checkChildren refPins candPins ia ib
  let virtual=sequential++children
  -- Internal spellings are not observable functions. A historical cone may
  -- differ under a guard which prevents that value reaching any consumer.
  -- Prove complete cones at every observable boundary, including unchanged
  -- boundary expressions whose transitive definitions have changed.
  let observed=observableNames ia `S.union` observableNames ib
      roots=S.toAscList (observed `S.intersection` M.keysSet ca `S.intersection` M.keysSet cb)
      classify (yes,no,z) n = case do
        (x,z')<-normDefinition 0 ia n z
        (y,z'')<-normDefinition 1 ib n z'
        return (x==y,z'') of
          Just (True,z')->(yes++[n],no,z')
          Just (_,z')->(yes,no++[n],z')
          Nothing->(yes,no++[n],z)
      (structural,residual,normState) = foldl classify ([],[],emptyNorm) roots
      classifyPin (yes,no,z) entry@(n,x,y,w) = case do
        (xi,z')<-normExpr 0 ia w x z
        (yi,z'')<-normExpr 1 ib w y z'
        return (xi==yi,z'') of
          Just (True,z')->(yes++[n],no,z')
          Just (_,z')->(yes,no++[entry],z')
          Nothing->(yes,no++[entry],z)
      (structuralPins,residualPins,_) = foldl classifyPin ([],[],normState) virtual
      counts=[("referenceDeclarations",M.size da),("candidateDeclarations",M.size db),
              ("referenceCombinationalDestinations",M.size ca),("candidateCombinationalDestinations",M.size cb),
              ("opaqueItems",length (invOpaque ia)),("childInstances",M.size (invChildren ia)),
              ("observableRoots",length roots),("structuralRoots",length structural),
              ("residualRoots",length residual),("childConnectionRoots",length children),
              ("sequentialRHSRoots",length sequential),
              ("commonInitialHistoryAliases",length historyAliases)]
  miter<-if null residual && null residualPins then Right Nothing
    else if domain==StrictFourState then bad "four-state-residual" (intercalate ", " (residual++map (\(n,_,_,_)->n) residualPins))
    else Just <$> makeMiter ia ib residual residualPins
  return (ProofBundle domain counts (structural++structuralPins++historyAliases) (residual++map (\(n,_,_,_)->n) residualPins) miter)
  where
    isFunction (VMFunction _) = True
    isFunction _ = False
    sameDecl pureNet (Decl ta ra va) (Decl tb rb vb)=ra==rb && va==vb &&
      (ta==tb || pureNet && ta `elem` [VDReg,VDWire] && tb `elem` [VDReg,VDWire])
checkASTWithPins _ _ _ _ _ _ = bad "module-count" "Exactly one captured module is required"

-- Sequential scheduling, controls, reset branches, events and destinations
-- stay exactly fixed. Only whole-scalar nonblocking RHS values may differ;
-- they become typed, complete-cone proof obligations. Initial/effect blocks
-- remain literal identities, and no reset/initial fact constrains the proof.
checkOpaque :: Inventory -> Inventory -> Check [(Name,VExpr,VExpr,Integer)]
checkOpaque a b=do
  let xa=invOpaque a; xb=invOpaque b
      fixedA=filter (not.movable) xa; fixedB=filter (not.movable) xb
  ensure (length fixedA==length fixedB &&
          sortOn show (filter movable xa)==sortOn show (filter movable xb))
    "state-reset-init-effects" "Opaque item inventory or incomplete combinational process changed"
  concat <$> mapM item (zip [0::Int ..] (zip fixedA fixedB))
  where
    movable (VMStmt False (Valways (VAt ev s)))=not (edge ev) && pureStatement s
    movable _=False
    item (_, (x,y)) | x==y=Right []
    item (i,(VMStmt False (Valways (VAt ev x)),VMStmt False (Valways (VAt ev' y))))
      | ev==ev',edge ev=statement ("state:"++show i) x y
    item _=bad "state-reset-init-effects" "State/event/initialization/effect/function structure changed"
    statement _ x y | x==y=Right []
    statement path (VAssignA (VLId v) x) (VAssignA (VLId v') y) | v==v'=do
      w<-maybe (bad "state-rhs-width" (name v)) Right (M.lookup (name v) (invWidths a))
      ensure (M.lookup (name v) (invWidths b)==Just w &&
              expressionWidth (invWidths a) x==Just w &&
              expressionWidth (invWidths b) y==Just w && semanticExpr x && semanticExpr y)
        "state-rhs-width" (name v++": exact-width pure RHS required")
      return [(path++":"++name v,x,y,w)]
    statement path (VSeq xs) (VSeq ys) | length xs==length ys=
      concat <$> mapM (\(i,(x,y))->statement (path++":"++show i) x y)
        (zip [0::Int ..] (zip xs ys))
    statement path (Vif c x) (Vif d y) | c==d=statement (path++":if") x y
    statement path (Vifelse c x y) (Vifelse d x' y') | c==d=
      (++) <$> statement (path++":then") x x' <*> statement (path++":else") y y'
    statement _ _ _=bad "state-reset-init-effects" "Sequential control/destination skeleton changed or unsupported RHS position"

-- Opaque items are required to remain exactly equal above. All their typed
-- identifiers are conservative boundaries: RHS values, array indices, event
-- controls, reset tests, assertions/tasks and dump arguments are included.
-- Function/DPI scope and arbitrary macro text cannot establish a precise read
-- set here, so retain the stronger all-internal-functions obligation for those
-- items. No unchanged internal combinational name becomes a shared SAT cut.
observableNames :: Inventory -> S.Set Name
observableNames inv = S.unions [ports,opaque,children,fallback]
  where
    ports=S.fromList [name (vargName p) | p<-concatMap fst (vm_ports (invModule inv)),output p]
    output VAOutput {}=True
    output VAInout {}=True
    output _=False
    opaque=S.fromList [name v | x<-invOpaque inv,v<-semanticValues x]
    children=S.unions [refs e | x<-M.elems (invChildren inv),
                               e<-semanticValues x]
    fallback=if any opaqueScope (invOpaque inv) then M.keysSet (invDrivers inv) else S.empty
    opaqueScope VMFunction {}=True
    opaqueScope _=False -- unknown macros were rejected; reset macros are scoped by the printer

checkChildren :: [ChildPin] -> [ChildPin] -> Inventory -> Inventory -> Check [(Name,VExpr,VExpr,Integer)]
checkChildren apins bpins a b = do
  ensure (sort apins==sort bpins) "child-metadata" "Native child formal direction/width descriptors changed"
  ensure (length apins==M.size pins) "duplicate-child-metadata" "Conflicting or duplicate child pin descriptors"
  ensure (M.keys (invChildren a)==M.keys (invChildren b)) "child-identity" "Child instance set changed"
  fmap concat $ mapM each (M.keys (invChildren a))
  where
    pins=M.fromList [((pinInstance p,pinFormal p),p) | p<-apins]
    each n = case (invChildren a M.! n,invChildren b M.! n) of
      (VMInst am _ ap ac,VMInst bm _ bp bc) -> do
        ensure (am==bm && ap==bp && map fst (sortOn fst ac)==map fst (sortOn fst bc)) "child-interface" n
        fmap catMaybes $ mapM pin (zip (sortOn fst ac) (sortOn fst bc))
        where
          pin ((p,Just x),(_,Just y)) | x/=y = do
            let meta=M.lookup (n,name p) pins
            ensure (maybe (not (isLValue x) && not (isLValue y)) ((==ChildInput) . pinDirection) meta) "child-direction-unavailable" (n++"."++name p)
            w<-maybe (bad "child-width" (n++"."++name p)) Right (case meta of
              Just info | Just width<-pinWidth info -> Just width
              _ -> if expressionWidth (invWidths a) x==expressionWidth (invWidths b) y then expressionWidth (invWidths a) x else Nothing)
            return (Just ("child:"++n++":"++name p,x,y,w))
          pin ((p,x),(_,y)) = do
            ensure (x==y) "child-connection" (n++"."++name p)
            case (M.lookup (n,name p) pins,x) of
              (Just info,Just value) | pinDirection info/=ChildInput ->
                ensure (S.null (refs value `S.intersection` (M.keysSet (invDrivers a) `S.union` M.keysSet (invDrivers b)))) "child-multiple-driver" (n++"."++name p)
              _ -> Right ()
            return Nothing
      _ -> bad "child-inventory" n
    isLValue (VEVar _) = True
    isLValue (VEIndex _ _) = True
    isLValue (VESelect x _ _) = isLValue x
    isLValue (VESelect1 x _) = isLValue x
    isLValue (VEConcat xs) = all isLValue xs
    isLValue _ = False

-- Residual cones use complete combinational closure. Only already-corresponding
-- primary/state/opaque-child leaves become shared SAT inputs.
makeMiter :: Inventory -> Inventory -> [Name] -> [(Name,VExpr,VExpr,Integer)] -> Check VProgram
makeMiter a b roots virtual = do
  (an,al)<-closure a (roots++concatMap (S.toList . refs . left) virtual)
  (bn,bl)<-closure b (roots++concatMap (S.toList . refs . right) virtual)
  let leaves=sort (S.toList (al `S.union` bl))
  mapM_ (\n -> ensure (M.lookup n (invWidths a)==M.lookup n (invWidths b) && M.member n (invWidths a) && M.notMember n (invDrivers a) && M.notMember n (invDrivers b)) "unproved-cut" n) leaves
  widths<-mapM (\n -> maybe (bad "root-width" n) Right (M.lookup n (invWidths a))) roots
  let bits=sum widths+sum [w|(_,_,_,w)<-virtual]
      bs=M.fromList [(n,"b"++show i)|(i,n)<-zip [0::Int ..] leaves]
      args=[VAInput (mkVId (bs M.! n)) (rng (invWidths a M.! n))|n<-leaves]
  am<-cut a (0::Int) an bs args bits roots [(left v,w)|v@(_,_,_,w)<-virtual]
  bm<-cut b (1::Int) bn bs args bits roots [(right v,w)|v@(_,_,_,w)<-virtual]
  let result k=mkVId ("r"++show k)
      wire k=VMDecl (VVDecl VDWire (rng bits) [VVar (result k)])
      inst k=VMInst (mkVId ("cut"++show k)) (mkVId ("u"++show k)) (Right [])
        ([(mkVId v,Just (VEVar (mkVId v)))|v<-M.elems bs]++[(mkVId "result",Just (VEVar (result k)))])
      mismatch=VEOp (mkVId "") (VEVar (result (0::Int))) VNE (VEVar (result (1::Int)))
      top=VModule (mkVId "miter") [] [(args++[VAOutput (mkVId "mismatch") Nothing],[])]
        [wire (0::Int),wire (1::Int),inst (0::Int),inst (1::Int),VMAssign (VLId (mkVId "mismatch")) mismatch]
  return (VProgram [am,bm,top] [] [])
  where
    left (_,x,_,_)=x; right (_,_,y,_)=y
    rng w=Just (VEConst (w-1),VEConst 0)
    closure inv seeds=foldM (walk inv S.empty) (S.empty,S.empty) seeds
    walk inv active (included,leaves) n
      | n `S.member` included = Right (included,leaves)
      | n `S.member` active = bad "cone-cycle" n
      | otherwise = case M.lookup n (invDrivers inv) of
          Nothing -> ensure (M.member n (invWidths inv)) "untyped-cut" n >> Right (included,S.insert n leaves)
          Just d -> do
            ensure (semanticDriver d) "unsupported-binary-expression" n
            (ds,ls)<-foldM (walk inv (S.insert n active)) (included,leaves) (S.toList (driverReads d))
            let outputs=case d of Continuous _->[n];Process _ _ ns->ns
            return (S.union ds (S.fromList outputs),ls)
    cut inv side included boundary args bits outputs exprs = do
      let ns=S.toAscList included
          ren=M.union (M.fromList [(n,"n"++show i)|(i,n)<-zip [0::Int ..] ns]) boundary
          drv=invDrivers inv
          decl n=VMDecl (VVDecl (case drv M.! n of Continuous _->VDWire;_->VDReg) (rng (invWidths inv M.! n)) [VVar (mkVId (ren M.! n))])
          item n=case drv M.! n of
            Continuous e->[VMAssign (VLId (mkVId (ren M.! n))) (renameExpr ren e)]
            Process ev s names | n==minimum names->[VMStmt False (Valways (VAt (renameEvent ren ev) (simulationStmt (renameStmt ren s))))]
                               | otherwise->[]
          virtualIds=[mkVId ("v"++show i)|(i,_)<-zip [0::Int ..] exprs]
          virtualItems=concat [[VMDecl (VVDecl VDWire (rng w) [VVar v]),VMAssign (VLId v) (renameExpr ren e)]|(v,(e,w))<-zip virtualIds exprs]
          out=VEConcat (map (VEVar . mkVId . (ren M.!)) outputs++map VEVar virtualIds)
      return (VModule (mkVId ("cut"++show side)) [] [(args++[VAOutput (mkVId "result") (rng bits)],[])]
        (map decl ns++concatMap item ns++virtualItems++[VMAssign (VLId (mkVId "result")) out]))

semanticExpr :: VExpr -> Bool
semanticExpr e=case e of
  VEConst n->n>=0 && n<2^(31::Int)
  VEWConst _ w _ _->w>0
  -- These native unspecified-value policies print concrete bit patterns.
  -- X/Z remain unsupported in the binary miter.
  VEUnknown w policy->w>0 && policy `elem` ["A","0","1"]
  VEVar _->True
  VEUnOp _ _ x->go x
  VEOp _ a _ b->go a && go b
  VEConcat xs->all go xs
  VESelect x h l->all go [x,h,l]
  VESelect1 x i->go x && go i
  VERepeat n x->go n && go x
  VEIf c t f->all go [c,t,f]
  _->False
  where go=semanticExpr
semanticDriver :: Driver -> Bool
semanticDriver (Continuous e)=semanticExpr e
semanticDriver (Process _ s _)=go s
  where
    go (VAssign (VLId _) e)=semanticExpr e
    go (VSeq xs)=all go xs
    go (Vif e s')=semanticExpr e && go s'
    go (Vifelse e a b)=semanticExpr e && go a && go b
    go (Vcase e as _ _)=semanticExpr e && all arm as
    go _=False
    arm (VDefault x)=go x
    arm (VCaseArm es x)=all semanticExpr es && go x

-- full_case/parallel_case are optimization promises, not proof assumptions.
-- Keep every actual arm/default and remove only these comments from the miter.
simulationStmt :: VStmt -> VStmt
simulationStmt s=case s of
  VSeq xs->VSeq (map simulationStmt xs)
  Vif e x->Vif e (simulationStmt x)
  Vifelse e a b->Vifelse e (simulationStmt a) (simulationStmt b)
  Vcase e as _ _->Vcase e (map arm as) False False
  _->s
  where arm (VCaseArm es x)=VCaseArm es (simulationStmt x)
        arm (VDefault x)=VDefault (simulationStmt x)

runASTProof :: FilePath -> Int -> FilePath -> ProofDomain -> [(String,String)] -> VProgram -> VProgram -> IO ProofResult
runASTProof = runASTProofWithPins [] []

runASTProofWithPins :: [ChildPin] -> [ChildPin] -> FilePath -> Int -> FilePath -> ProofDomain -> [(String,String)] -> VProgram -> VProgram -> IO ProofResult
runASTProofWithPins refPins candPins yosys seconds directory domain alpha reference candidate
  | seconds<=0 = return (ProofResult False "unresolved" [ProofIssue "timeout" "Proof budget exhausted"] Nothing Nothing [])
  | otherwise = case checkASTWithPins refPins candPins domain alpha reference candidate of
      Left issues -> return (ProofResult False "unresolved" issues Nothing Nothing [])
      Right bundle | Nothing <- proofMiter bundle -> return (ProofResult True "passed" [] (Just bundle) Nothing [])
      Right bundle -> do
        createDirectoryIfMissing True directory
        let Just miter=proofMiter bundle
            logfile=directory </> "proof.log"
        writeFile (directory </> "miter.v") (ppReadable miter)
        writeFile (directory </> "prove.ys") (unlines
          ["read_verilog -sv miter.v","hierarchy -check -top miter","proc","flatten","opt","check -assert"
          ,"select -assert-none t:$dlatch t:$dlatchsr t:$dff t:$adff t:$sdff"
          ,"sat -timeout "++show seconds++" -prove mismatch 0 -show-inputs -show-outputs"])
        outcome<-timeout (seconds*1000000) (try (readCreateProcessWithExitCode ((proc yosys ["-Q","-T","-s","prove.ys"]) {cwd=Just directory}) "") :: IO (Either IOException (ExitCode,String,String)))
        case outcome of
          Nothing->return (ProofResult False "unresolved" [ProofIssue "timeout" "Solver time budget exhausted"] (Just bundle) (Just logfile) [])
          Just (Left e)->return (ProofResult False "unresolved" [ProofIssue "solver-exception" (show e)] (Just bundle) (Just logfile) [])
          Just (Right (exit,out,err))->do
            let logtext=out++err
                passed=exit==ExitSuccess && "SAT proof finished - no model found: SUCCESS!" `isInfixOf` logtext
                rejected="SAT proof finished - model found: FAIL!" `isInfixOf` logtext
            writeFile logfile logtext
            return (ProofResult passed (if passed then "passed" else if rejected then "rejected" else "unresolved")
              (if passed then [] else [ProofIssue (if rejected then "inequivalent" else "solver-failure") logfile]) (Just bundle) (Just logfile) [])

runASTProofWithContracts :: [ChildPin] -> [ChildPin] -> [ASTContract] -> FilePath -> Int -> FilePath -> ProofDomain -> [(String,String)] -> VProgram -> VProgram -> IO ProofResult
runASTProofWithContracts refPins candPins contracts yosys seconds directory domain alpha reference candidate
  | null contracts = runASTProofWithPins refPins candPins yosys seconds directory domain alpha reference candidate
  | domain/=Binary = return (failure "contract-domain" "Contract specialization currently requires the explicit binary domain")
  | otherwise = case prepare of
      Left issues -> return (ProofResult False "unresolved" issues Nothing Nothing [])
      Right (obligations,replacements) -> do
        -- Every proposition is checked against the ORIGINAL reference before
        -- any specialization. A descriptor's label/provenance is not evidence.
        results<-mapM proveObligation (zip [0::Int ..] obligations)
        case [r | (_,r)<-results,not (proofPassed r)] of
          failed:_ -> return failed {proofObligations=results,
                         proofIssues=ProofIssue "contract-not-proved" "At least one predicate or defined-arm selection was not independently proved":proofIssues failed}
          [] -> do
            final<-runASTProofWithPins refPins candPins yosys seconds (directory </> "equivalence")
                     domain alpha (replaceProgram replacements reference) candidate
            return final {proofObligations=results}
  where
    failure code message=ProofResult False "unresolved" [ProofIssue code message] Nothing Nothing []
    ident (AssertCombinational i _)=i
    ident (SpecializeUnreachableXArm i _ _)=i
    predicate (AssertCombinational _ p)=p
    predicate (SpecializeUnreachableXArm _ _ p)=p
    one=VEWConst (mkVId "") 1 2 1
    notE=VEUnOp (mkVId "") VNot
    bool e=notE (notE e)
    explicitX (VEUnknown w "X")=w>0
    explicitX (VETriConst bits)=not (null bits) && all (==Vx) bits
    explicitX _=False
    prepare = case reference of
      VProgram [m] _ _ -> do
        inv<-inventory Binary m
        let ids=map ident contracts
            targets=[n|SpecializeUnreachableXArm _ n _<-contracts]
        ensure (all (not . null) ids && length ids==S.size (S.fromList ids)) "contract-identity" "Contract IDs must be nonempty and unique"
        ensure (length targets==S.size (S.fromList targets)) "contract-target" "Only one specialization per destination is supported"
        rows<-mapM (prepareOne inv) contracts
        return (concatMap fst rows,concatMap snd rows)
      _ -> bad "contract-module-count" "Exactly one captured module is required"
    prepareOne inv contract = do
      let p=predicate contract; i=ident contract
      ensure (expressionWidth (invWidths inv) p==Just 1 && semanticExpr p &&
              refs p `S.isSubsetOf` M.keysSet (invWidths inv)) "contract-predicate" (i++": expected a typed, pure, one-bit combinational proposition")
      case contract of
        AssertCombinational _ _ -> return ([(i++":assertion",p)],[])
        SpecializeUnreachableXArm _ target _ -> do
          (guard,defined,selected,x)<-case M.lookup target (invDrivers inv) of
            Just (Continuous (VEIf c t f))
              | explicitX f && not (explicitX t) -> Right (c,t,bool c,f)
              | explicitX t && not (explicitX f) -> Right (c,f,notE c,t)
            _ -> bad "contract-target" (i++": target must be a continuous conditional with exactly one explicit literal-X arm; Z and missing defaults are not freedom")
          w<-maybe (bad "contract-width" target) Right (M.lookup target (invWidths inv))
          ensure (expressionWidth (invWidths inv) defined==Just w &&
                  expressionWidth (invWidths inv) x==Just w &&
                  semanticExpr defined && semanticExpr guard) "contract-width" (i++": both arms must have the destination width and the defined arm/guard must be pure; context-changing specialization is unsupported")
          let implication=VEOp (mkVId "") (notE p) VLOr selected
          return ([(i++":assertion",p),(i++":defined-arm",implication)],[(target,defined)])
    proveObligation (index,(label,p)) = case assertionPrograms p of
      Left issues -> return (label,ProofResult False "unresolved" issues Nothing Nothing [])
      Right (gold,trial) -> do
        result<-runASTProofWithPins refPins refPins yosys seconds
                   (directory </> "obligation-"++show index) Binary [] gold trial
        return (label,result)
    assertionPrograms p = case reference of
      VProgram [m] dpi comments -> do
        inv<-inventory Binary m
        let occupied=invAllNames inv `S.union` M.keysSet (invChildren inv)
            fresh=head ["eco_assertion_"++show n|n<-[0::Int ..],("eco_assertion_"++show n) `S.notMember` occupied]
            with value=VProgram [m {vm_ports=vm_ports m++[([VAOutput (mkVId fresh) Nothing],[])],
                                    vm_body=vm_body m++[VMAssign (VLId (mkVId fresh)) value]}] dpi comments
        return (with one,with p)
      _ -> bad "contract-module-count" "Exactly one captured module is required"
    replaceProgram changes (VProgram ms dpi comments)=VProgram
      [m {vm_body=map (replaceItem (M.fromList changes)) (vm_body m)}|m<-ms] dpi comments
    replaceItem replacements item=case item of
      VMAssign (VLId v) e -> VMAssign (VLId v) (M.findWithDefault e (name v) replacements)
      VMDecl (VVDWire range var@(VVar v) e) -> VMDecl (VVDWire range var (M.findWithDefault e (name v) replacements))
      VMComment comments x -> VMComment comments (replaceItem replacements x)
      VMRegGroup v s comments x -> VMRegGroup v s comments (replaceItem replacements x)
      VMGroup off groups -> VMGroup off (map (map (replaceItem replacements)) groups)
      _ -> item

renderProofResult :: ProofResult -> String
renderProofResult r = "{\"schema\":1,\"checker\":\"independent-native-Verilog-AST\",\"passed\":"++bool (proofPassed r)++
  ",\"status\":"++show (proofStatus r)++",\"issues\":["++intercalate "," ["{\"code\":"++show k++",\"detail\":"++show d++"}"|ProofIssue k d<-proofIssues r]++"]"++
  maybe "" (\b -> ",\"domain\":"++show (show (proofDomain b))++",\"inventory\":{"++intercalate "," [show k++":"++show n|(k,n)<-proofInventory b]++"},\"structuralRoots\":"++strings (proofStructuralRoots b)++",\"residualRoots\":"++strings (proofResidualRoots b)) (proofChecked r)++
  ",\"obligations\":["++intercalate "," ["{\"id\":"++show ident++",\"proof\":"++renderProofResult result++"}"|(ident,result)<-proofObligations r]++"]"++
  ",\"historyBasis\":"++show historyBasis++",\"scope\":"++show scope++"}"
  where
    historyUsed=maybe False ((>0) . maybe 0 id . lookup "commonInitialHistoryAliases" . proofInventory) (proofChecked r)
    historyBasis=if historyUsed then "common-Verilog-initialization-and-history" else "not-used"
    scope="Complete settled composition with corresponding ordinary-new state and fixed children; "++
      (if historyUsed then "reported duplicate retention relations use local common-Verilog-start/history induction, without reset or software assumptions; " else "")++
      "no general temporal initialization, reachability, delta-cycle, timing or QoR proof"
    bool True="true";bool False="false";strings xs="["++intercalate "," (map show xs)++"]"
