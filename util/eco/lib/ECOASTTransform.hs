-- Pure, bounded proposals on the compiler's retained final Verilog AST.
-- No RTL parser, rendered-text substitution, source-case identifiers, or
-- direct-edit target participates in candidate generation. Every proposal
-- requires the independent ECOASTProof gate against ordinary NEW output.
module ECOASTTransform
  ( ASTCandidate(..), generateASTCandidates, generateASTCandidatesWithContracts, expressionWidth
  ) where

import Verilog
import VLocalNames (mapRenameableVIds, collectRenameableVIds)
import ECOASTProof (ASTContract(..))
import ECOASTIdentity (StateRolePermutation(..), stateRolePermutations)
import ECOASTAliases (Representative(..), representativeProposals)
import ECOASTDeclarations (declarationProposals)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.List (sortOn, nub, intercalate, isInfixOf)
import Data.Char (toLower)
import Control.Monad (foldM)
import Data.Maybe (mapMaybe, fromMaybe)

data ASTCandidate = ASTCandidate
  { acFeature :: String
  , acDescription :: String
  , acProgram :: VProgram
  , acOperations :: [(String, [(String,String)])]
  , acBinaryOnly :: Bool
  , acStateMap :: [(String,String)]
  , acContracts :: [ASTContract]
  } deriving (Eq, Show)

type Symbols = M.Map String (VDType, Maybe VRange)
type Definitions = M.Map String VExpr

name :: VId -> String
name = getVIdString

leaves :: [VMItem] -> [VMItem]
leaves = concatMap go
 where
  go (VMComment _ x) = go x
  go (VMRegGroup _ _ _ x) = go x
  go (VMGroup _ xss) = leaves (concat xss)
  go x = [x]

-- Traverse only semantic expression positions, never VId provenance fields.
mapExpr :: (VExpr -> VExpr) -> VExpr -> VExpr
mapExpr f = go
 where
  go e = f $ case e of
    VEUnOp i o a -> VEUnOp i o (go a)
    VEOp i a o b -> VEOp i (go a) o (go b)
    VEConcat xs -> VEConcat (map go xs)
    VEIndex i a -> VEIndex i (go a)
    VESelect a h l -> VESelect (go a) (go h) (go l)
    VESelect1 a i -> VESelect1 (go a) (go i)
    VERepeat n a -> VERepeat (go n) (go a)
    VEIf c a b -> VEIf (go c) (go a) (go b)
    VEFctCall i xs -> VEFctCall i (map go xs)
    _ -> e

canon :: VExpr -> VExpr
canon = mapExpr clean
 where
  clean (VEUnOp _ o a) = VEUnOp defaultVId o a
  clean (VEOp _ a o b) = VEOp defaultVId a o b
  clean (VEWConst _ w b v) = VEWConst defaultVId w b v
  clean e = e

sameExpr :: VExpr -> VExpr -> Bool
sameExpr a b = canon a == canon b

literal :: VExpr -> Maybe Integer
literal (VEConst n) = Just n
literal (VEWConst _ _ _ n) = Just n
literal _ = Nothing

rangeWidth :: Maybe VRange -> Maybe Integer
rangeWidth Nothing = Just 1
rangeWidth (Just (h,l)) = do
  a <- literal h; b <- literal l
  if b == 0 && a >= b then Just (a-b+1) else Nothing

declarations :: VModule -> Symbols
declarations m = M.fromList (ports ++ concatMap decl (leaves (vm_body m)))
 where
  ports = concatMap (concatMap port . fst) (vm_ports m)
  port (VAInput i r) = [(name i,(VDInput,r))]
  port (VAOutput i r) = [(name i,(VDOutput,r))]
  port (VAInout i _ (Just r)) = [(name i,(VDInout,r))]
  port _ = []
  decl (VMDecl (VVDecl t r vs)) = [(name i,(t,r)) | VVar i <- vs]
  decl (VMDecl (VVDWire r (VVar i) _)) = [(name i,(VDWire,r))]
  decl _ = []

definitions :: VModule -> Definitions
definitions = M.fromList . mapMaybe def . leaves . vm_body
 where
  def (VMAssign (VLId i) e) = Just (name i,e)
  def (VMDecl (VVDWire _ (VVar i) e)) = Just (name i,e)
  def _ = Nothing

portNames :: VModule -> S.Set String
portNames = S.fromList . map (name . vargName) . concatMap fst . vm_ports

-- Conservative unsigned widths. Context-sensitive unsized arithmetic, calls,
-- macros, arrays and dynamic slices are not extraction evidence.
expressionWidth :: M.Map String (VDType,Maybe VRange) -> VExpr -> Maybe Integer
expressionWidth sy = go
 where
  go (VEVar i) = M.lookup (name i) sy >>= rangeWidth . snd
  go (VEWConst _ w _ _) | w > 0 = Just w
  go (VEUnknown w _) | w > 0 = Just w
  go (VETriConst xs) | not (null xs) = Just (toInteger (length xs))
  go (VESelect a h l) = do
    w <- go a; hi <- literal h; lo <- literal l
    if lo >= 0 && hi >= lo && hi < w then Just (hi-lo+1) else Nothing
  go (VESelect1 a i) = do
    w <- go a; n <- literal i
    if n >= 0 && n < w then Just 1 else Nothing
  go (VEConcat xs) = if null xs then Nothing else sum <$> mapM go xs
  go (VERepeat n a) = do
    count <- literal n; w <- go a
    if count > 0 then Just (count*w) else Nothing
  go (VEUnOp _ VNot a) = go a >> return 1
  go (VEUnOp _ _ a) = go a
  go (VEOp _ a o b) = do
    x <- go a; y <- go b
    return $ if o `elem` relations ++ [VLAnd,VLOr] then 1
             else if o `elem` [VShL,VShR,VShLA,VShRA] then x else max x y
  go (VEIf c a b) = do _ <- go c; x <- go a; y <- go b; return (max x y)
  go _ = Nothing

relations :: [VOp]
relations = [VULT,VULE,VUGT,VUGE,VEQ,VNE,VEQ3,VNE3]

pureExpr :: VExpr -> Bool
pureExpr (VEFctCall _ _) = False
pureExpr (VEMacro _) = False
pureExpr (VEIndex _ _) = False
pureExpr e = all pureExpr (children e)

children :: VExpr -> [VExpr]
children (VEUnOp _ _ a) = [a]
children (VEOp _ a _ b) = [a,b]
children (VEConcat xs) = xs
children (VEIndex _ a) = [a]
children (VESelect a h l) = [a,h,l]
children (VESelect1 a i) = [a,i]
children (VERepeat n a) = [n,a]
children (VEIf c a b) = [c,a,b]
children (VEFctCall _ xs) = xs
children _ = []

exprNames :: VExpr -> S.Set String
exprNames e = S.unions (own : map exprNames (children e))
 where own = case e of VEVar i -> S.singleton (name i); VEIndex i _ -> S.singleton (name i); _ -> S.empty

lvalues :: VLValue -> [String]
lvalues (VLId i) = [name i]
lvalues (VLConcat xs) = concatMap lvalues xs
lvalues (VLSub x _) = lvalues x

stmtOutputs :: VStmt -> S.Set String
stmtOutputs (VAssign v _) = S.fromList (lvalues v)
stmtOutputs (VAssignA v _) = S.fromList (lvalues v)
stmtOutputs (VAt _ s) = stmtOutputs s
stmtOutputs (Valways s) = stmtOutputs s
stmtOutputs (Vinitial s) = stmtOutputs s
stmtOutputs (VSeq xs) = S.unions (map stmtOutputs xs)
stmtOutputs (Vif _ s) = stmtOutputs s
stmtOutputs (Vifelse _ a b) = S.union (stmtOutputs a) (stmtOutputs b)
stmtOutputs (Vcase _ as _ _) = S.unions (map armOutputs as)
stmtOutputs (Vcasex _ as _ _) = S.unions (map armOutputs as)
stmtOutputs _ = S.empty

armOutputs :: VCaseArm -> S.Set String
armOutputs (VCaseArm _ s) = stmtOutputs s
armOutputs (VDefault s) = stmtOutputs s

combEvent :: VEventExpr -> Bool
combEvent (VEE e) = pureExpr e
combEvent (VEEOr a b) = combEvent a && combEvent b
combEvent _ = False

pureStmt :: VStmt -> Bool
pureStmt (Valways s) = pureStmt s
pureStmt (VAt e s) = combEvent e && pureStmt s
pureStmt (VSeq xs) = all pureStmt xs
pureStmt (VAssign (VLId _) e) = pureExpr e
pureStmt (Vif e s) = pureExpr e && pureStmt s
pureStmt (Vifelse e a b) = pureExpr e && pureStmt a && pureStmt b
pureStmt (Vcase e as _ _) = pureExpr e && all arm as
 where arm (VCaseArm es s) = all pureExpr es && pureStmt s
       arm (VDefault s) = pureStmt s
pureStmt _ = False

itemKey :: VMItem -> Maybe String
itemKey (VMAssign (VLId i) e) | pureExpr e = Just ("assign:"++name i)
itemKey (VMStmt False s) | pureStmt s = Just ("comb:"++intercalate "," (S.toAscList (stmtOutputs s)))
itemKey x@(VMInst {}) = Just ("instance:"++name (vi_inst_name x))
itemKey _ = Nothing

mapItems :: (VMItem -> VMItem) -> [VMItem] -> [VMItem]
mapItems f = map go
 where
  go (VMComment c x) = VMComment c (go x)
  go (VMRegGroup i s c x) = VMRegGroup i s c (go x)
  go (VMGroup t xs) = VMGroup t (map (map go) xs)
  go x = f x

-- Reuse the historical wrapper/group skeleton only when every semantic leaf
-- has a unique current counterpart. Protected leaves must be literally equal;
-- ports and program/header comments remain NEW. New state is not compared to
-- old state as a functional requirement: unsupported layout simply declines.
-- The translate-off context is a semantic boundary, not formatting. Array
-- declarations and directive-bearing comments are never historical templates.
contextLeaves :: [VMItem] -> [(Bool,VMItem)]
contextLeaves = concatMap (go False)
 where
  go t (VMComment _ x) = go t x
  go t (VMRegGroup _ _ _ x) = go t x
  go t (VMGroup off xss) = concatMap (go (t || off)) (concat xss)
  go t x = [(t,x)]

scalarDeclaration :: VMItem -> Bool
scalarDeclaration (VMDecl (VVDecl _ _ vs)) = not (null vs) && all scalar vs
 where scalar (VVar _) = True
       scalar _ = False
scalarDeclaration _ = False

safeComments :: [VMItem] -> Bool
safeComments = all go
 where
  okay cs = not $ any (`isInfixOf` map toLower (unlines cs))
    ["synthesis", "synopsys", "translate_", "verilator", "pragma",
     "`ifdef", "`ifndef", "`define", "`include", "`timescale", "(*", "*)"]
  go (VMComment c x) = okay c && go x
  go (VMRegGroup _ _ c x) = okay c && go x
  go (VMGroup _ xs) = all safeComments xs
  go _ = True

historicalLayout :: VModule -> VModule -> Maybe VModule
historicalLayout old new = do
  let os = declarations old; ns = declarations new
      oc = contextLeaves (vm_body old); nc = contextLeaves (vm_body new)
      ol = map snd oc; nl = map snd nc
      movable = M.fromListWith (++) [(k,[x]) | x <- nl, Just k <- [itemKey x]]
      keys xs = sortOn id [(k,t) | (t,x) <- xs, Just k <- [itemKey x]]
      protected xs = [(t,x) | (t,x) <- xs, itemKey x==Nothing, not (scalarDeclaration x)]
      declarationContexts xs = M.fromList
        [(name i,t) | (t,VMDecl (VVDecl _ _ vs)) <- xs,VVar i <- vs]
  if os /= ns || keys oc /= keys nc || protected oc /= protected nc
     || declarationContexts oc /= declarationContexts nc
     || not (safeComments (vm_body old) && safeComments (vm_body new))
    then Nothing else do
      body <- mapM (rebuild movable) (vm_body old)
      return new { vm_body=body }
 where
  rebuild env (VMComment c x) = VMComment c <$> rebuild env x
  rebuild env (VMRegGroup i s c x) = VMRegGroup i s c <$> rebuild env x
  rebuild env (VMGroup t xs) = VMGroup t <$> mapM (mapM (rebuild env)) xs
  rebuild _ x | scalarDeclaration x = Just x
  rebuild env x = case itemKey x of
    Just k -> case M.lookup k env of Just [y] -> Just y; _ -> Nothing
    Nothing -> Just x

-- Reorder complete statement objects only within their current sibling
-- container. Translate-off wrappers, non-process anchors and current comments
-- stay attached to their original AST objects.
processOrder :: VModule -> VModule -> VModule
processOrder old new=new {vm_body=level (vm_body new)}
 where
  ranks=M.fromList [(k,i) | (i,x)<-zip [0::Int ..] (leaves (vm_body old)),
                           Just k<-[itemKey x],isProcess x]
  isProcess (VMStmt False s)=pureStmt s
  isProcess _=False
  key x=case leaves [x] of
    [y] | isProcess y && safeComments [x]->itemKey y
    _->Nothing
  rank x=fromMaybe maxBound (key x >>= (`M.lookup` ranks))
  recurse (VMComment c x)=VMComment c (recurse x)
  recurse (VMRegGroup i s c x)=VMRegGroup i s c (recurse x)
  recurse (VMGroup t xs)=VMGroup t (map level xs)
  recurse x=x
  level xs=let current=map recurse xs
               ordered=sortOn rank [x | x<-current,key x/=Nothing]
               fill [] _=[]
               fill (x:rest) ys | key x/=Nothing=case ys of
                  z:zs->z:fill rest zs
                  []->x:fill rest []
               fill (x:rest) ys=x:fill rest ys
           in fill current ordered

-- Match complete continuous-assignment groups by their populated signal IDs.
-- Unlike historicalLayout this does not require equal module inventories: new
-- registers and unmatched groups stay in the current AST. Only matched slots
-- within one uninterrupted pure sibling segment are permuted. Current bodies,
-- comments and unmatched-group order are preserved; effects, declarations,
-- instances, directives and translate-off containers are barriers.
assignmentGroupOrder :: VModule -> VModule -> VModule
assignmentGroupOrder old new = new {vm_body=level (vm_body new)}
 where
  writers m=M.fromListWith (+)
    [(name i,1::Int) | VMAssign (VLId i) _<-leaves (vm_body m)]
  oldWriters=writers old
  newWriters=writers new
  single n=M.lookup n oldWriters==Just 1 && M.lookup n newWriters==Just 1
  populated (VMAssign (VLId i) e) | pureExpr e=Just [name i]
  populated x@(VMComment _ body) | safeComments [x]=populated body
  populated (VMGroup False xss)=do
    names<-concat <$> mapM populated (concat xss)
    if null names then Nothing else Just names
  populated _=Nothing
  key item=do
    names<-populated item
    if all single names && length names==S.size (S.fromList names)
      then Just (sortOn id names) else Nothing
  -- A transparent wrapper and its single child may describe the same group;
  -- the underlying writer-count check distinguishes this from duplicate defs.
  historical xs=concatMap visit xs
  visit item=maybe [] (:[]) (key item) ++ case item of
    x@(VMComment _ body) | safeComments [x]->visit body
    VMGroup False xss->concatMap historical xss
    _->[]
  ranks=M.fromListWith min [(k,n) | (n,k)<-zip [0::Int ..] (historical (vm_body old))]
  matched item=maybe False (`M.member` ranks) (key item)
  rank item=fromMaybe maxBound (key item >>= (`M.lookup` ranks))
  safe item=case populated item of Just _->True; _->False
  descend x@(VMComment comment body) | safeComments [x]=VMComment comment (descend body)
  descend (VMGroup False xss)=VMGroup False (map level xss)
  descend item=item
  level=arrange . map descend
  arrange []=[]
  arrange (item:rest)
    | safe item=
        let (segment,remaining)=span safe (item:rest)
            ordered=sortOn rank (filter matched segment)
        in fill segment ordered ++ arrange remaining
    | otherwise=item:arrange rest
  fill [] _=[]
  fill (item:rest) choices
    | matched item=case choices of
        next:remaining->next:fill rest remaining
        []->item:fill rest []
    | otherwise=item:fill rest choices

-- Expressions stored in assignments and pure procedures are transformation
-- sites. Child input/output direction is absent in VMInst, so no general
-- expression replacement enters child connections here.
mapStmtExprs :: (VExpr -> VExpr) -> VStmt -> VStmt
mapStmtExprs f = go
 where
  go (Valways s) = Valways (go s)
  go (VAt e s) = VAt e (go s)
  go (VSeq xs) = VSeq (map go xs)
  go (VAssign v e) = VAssign v (f e)
  go (Vif c s) = Vif (f c) (go s)
  go (Vifelse c a b) = Vifelse (f c) (go a) (go b)
  go (Vcase e as p q) = Vcase (f e) (map arm as) p q
  go s = s
  arm (VCaseArm es s) = VCaseArm es (go s)
  arm (VDefault s) = VDefault (go s)

transformExpressions :: (VExpr -> VExpr) -> VModule -> VModule
transformExpressions f m = m { vm_body=mapItems change (vm_body m) }
 where
  change (VMAssign v e) = VMAssign v (f e)
  change (VMStmt False s) | pureStmt s = VMStmt False (mapStmtExprs f s)
  change x = x

complement :: VOp -> Maybe VOp
complement VULT = Just VUGE
complement VULE = Just VUGT
complement VUGT = Just VULE
complement VUGE = Just VULT
complement VEQ = Just VNE
complement VNE = Just VEQ
complement _ = Nothing

relationalForms :: VModule -> VModule
relationalForms = transformExpressions (mapExpr rewrite)
 where
  rewrite (VEUnOp i VNot (VEOp j a o b)) = case complement o of
    Just op -> VEOp j a op b
    Nothing -> VEUnOp i VNot (VEOp j a o b)
  rewrite e = e

type Claims = [(String,VExpr)]

-- Historical variable occurrences are holes, not historical functions. A
-- hole is filled by the corresponding NEW expression, preserving its type.
roleExpr :: Symbols -> Symbols -> S.Set String -> Bool -> VExpr -> VExpr
         -> Maybe (VExpr,Claims)
roleExpr oldSy newSy missing self = go self
 where
  go context (VEVar i) current | name i `S.member` missing = do
    (_,r) <- M.lookup (name i) oldSy
    w <- rangeWidth r; actual <- expressionWidth newSy current
    let intrinsic = case current of
          VEVar _ -> True
          VESelect _ _ _ -> True
          VESelect1 _ _ -> True
          VEOp _ _ o _ -> o `elem` relations++[VLAnd,VLOr]
          VEUnOp _ VNot _ -> True
          _ -> False
    if w==actual && pureExpr current && (context || intrinsic)
       then Just (VEVar i,[(name i,current)]) else Nothing
  go _ (VEUnOp _ a x) (VEUnOp i b y) | a==b = do
    (z,cs)<-go False x y; return (VEUnOp i b z,cs)
  go _ (VEOp _ a o b) (VEOp i x p y) | o==p = do
    (l,ls)<-go False a x; (r,rs)<-go False b y; return (VEOp i l p r,ls++rs)
  go _ (VEConcat as) (VEConcat bs) | length as==length bs = do
    xs<-sequence (zipWith (go True) as bs); return (VEConcat (map fst xs),concatMap snd xs)
  go _ (VEIf a b c) (VEIf x y z) = do
    (c',cc)<-go False a x; (a',ac)<-go False b y; (b',bc)<-go False c z
    return (VEIf c' a' b',cc++ac++bc)
  go _ (VESelect a h l) (VESelect b h' l') | sameExpr h h' && sameExpr l l' = do
    (x,cs)<-go False a b; return (VESelect x h' l',cs)
  go _ (VESelect1 a i) (VESelect1 b j) | sameExpr i j = do
    (x,cs)<-go False a b; return (VESelect1 x j,cs)
  go _ (VERepeat n a) (VERepeat m b) | sameExpr n m = do
    (x,cs)<-go True a b; return (VERepeat m x,cs)
  go _ a b | sameExpr a b = Just (b,[])
  go _ _ _ = Nothing

mapEvent :: (VExpr -> VExpr) -> VEventExpr -> VEventExpr
mapEvent f (VEE e) = VEE (f e)
mapEvent f (VEEOr a b) = VEEOr (mapEvent f a) (mapEvent f b)
mapEvent _ x = x

roleStmt :: Symbols -> Symbols -> S.Set String -> VStmt -> VStmt
         -> Maybe (VStmt,Claims)
roleStmt os ns missing = go
 where
  re = roleExpr os ns missing False
  go (Valways a) (Valways b) = do (s,c)<-go a b; return (Valways s,c)
  go (VAt e a) (VAt f b) | combEvent e && combEvent f = do
    (s,cs)<-go a b
    let replace current = case [(n,x) | (n,x)<-cs, sameExpr x current] of
          (n,_):_ -> VEVar (mkVId n)
          [] -> current
    return (VAt (mapEvent replace f) s,cs)
  go (VSeq as) (VSeq bs) | length as==length bs = do
    xs<-sequence (zipWith go as bs); return (VSeq (map fst xs),concatMap snd xs)
  go (VAssign a x) (VAssign b y) | a==b = do
    (z,cs)<-re x y; return (VAssign b z,cs)
  go (Vif c a) (Vif d b) = do
    (e,ec)<-re c d; (s,sc)<-go a b; return (Vif e s,ec++sc)
  go (Vifelse c a b) (Vifelse d x y) = do
    (e,ec)<-re c d; (a',ac)<-go a x; (b',bc)<-go b y
    return (Vifelse e a' b',ec++ac++bc)
  go (Vcase e as p q) (Vcase f bs r s)
    | p==r && q==s && length as==length bs = do
      (e',ec)<-re e f; arms<-sequence (zipWith arm as bs)
      return (Vcase e' (map fst arms) r s,ec++concatMap snd arms)
  go a b | a==b && pureStmt b = Just (b,[])
  go _ _ = Nothing
  arm (VCaseArm es a) (VCaseArm fs b)
    | length es==length fs && and (zipWith sameExpr es fs) = do
      (s,c)<-go a b; return (VCaseArm fs s,c)
  arm (VDefault a) (VDefault b) = do (s,c)<-go a b; return (VDefault s,c)
  arm _ _ = Nothing

restoreRoles :: VModule -> VModule -> Maybe (VModule,[String])
restoreRoles old new = do
  let os=declarations old; ns=declarations new
      od=definitions old
      missing=S.fromList [n | (n,(VDWire,r))<-M.toAscList os,
                              M.notMember n ns, M.member n od,
                              n `S.notMember` portNames old, rangeWidth r/=Nothing]
      oldMap=M.fromList [(k,x) | x<-leaves (vm_body old),Just k<-[itemKey x]]
      align allowed current = do
        k<-itemKey current; prior<-M.lookup k oldMap
        case (prior,current) of
          (VMAssign (VLId i) a,VMAssign v b) -> do
            let full=expressionWidth ns b==(M.lookup (name i) ns >>= rangeWidth.snd)
            (e,cs)<-roleExpr os ns allowed full a b
            return (VMAssign v e,cs)
          (VMStmt False a,VMStmt False b) | pureStmt a && pureStmt b -> do
            (s,cs)<-roleStmt os ns allowed a b; return (VMStmt False s,cs)
          _ -> Nothing
      initialClaims=concat [cs | x<-leaves (vm_body new),Just (_,cs)<-[align missing x]]
      grouped=M.fromListWith (++) [(n,[e]) | (n,e)<-initialClaims]
      valid=M.map head $ M.filter (\xs -> not (null xs) && all (sameExpr (head xs)) xs) grouped
      allowed=M.keysSet valid
      mapped=mapItems (\x -> maybe x fst (align allowed x)) (vm_body new)
      actual=S.fromList [n | x<-leaves (vm_body new),Just (_,cs)<-[align allowed x],(n,_)<-cs]
      additions=[(n,e,r) | (n,e)<-M.toAscList valid,n `S.member` actual,Just (_,r)<-[M.lookup n os]]
      decls=[VMDecl (VVDecl VDWire r [VVar (mkVId n)]) | (n,_,r)<-additions]
      defs=[VMAssign (VLId (mkVId n)) e | (n,e,_)<-additions]
  if null additions then Nothing else
    return (new {vm_body=mapped++decls++defs},[n | (n,_,_)<-additions])

definite :: Bool -> Symbols -> VStmt -> S.Set String
definite binary sy = go
 where
  go (Valways s)=go s
  go (VAt e s) | combEvent e=go s
  go (VSeq xs)=S.unions (map go xs)
  go (VAssign (VLId i) e) | pureExpr e=S.singleton (name i)
  go (Vifelse _ a b)=S.intersection (go a) (go b)
  go (Vcase e arms _ _) | any isDefault arms || binary && covers e arms =
    case map (go.armBody) arms of []->S.empty; x:xs->foldl S.intersection x xs
  go _=S.empty
  isDefault (VDefault _)=True
  isDefault _=False
  armBody (VDefault s)=s
  armBody (VCaseArm _ s)=s
  covers e arms=case expressionWidth sy e of
    Just w | w>0 && w<=20 ->
      let labels=concat [es | VCaseArm es _<-arms]
          valid (VEWConst _ width _ n) | width==w && n>=0 && n<2^w=Just n
          valid _=Nothing
      in case mapM valid labels of
           Just values->toInteger (length values)==2^w && S.size (S.fromList values)==length values
           _->False
    _->False

stmtReadNames :: VStmt -> S.Set String
stmtReadNames (Valways s) = stmtReadNames s
stmtReadNames (VAt _ s) = stmtReadNames s
stmtReadNames (VSeq xs) = S.unions (map stmtReadNames xs)
stmtReadNames (VAssign _ e) = exprNames e
stmtReadNames (Vif c s) = S.union (exprNames c) (stmtReadNames s)
stmtReadNames (Vifelse c a b) = S.unions [exprNames c,stmtReadNames a,stmtReadNames b]
stmtReadNames (Vcase e arms _ _) = S.unions (exprNames e : map arm arms)
 where arm (VCaseArm es s) = S.unions (stmtReadNames s:map exprNames es)
       arm (VDefault s) = stmtReadNames s
stmtReadNames _ = S.empty

renameDestination :: String -> VId -> VStmt -> VStmt
renameDestination old new = go
 where
  go (Valways s)=Valways (go s)
  go (VAt e s)=VAt e (go s)
  go (VSeq xs)=VSeq (map go xs)
  go (VAssign (VLId i) e) | name i==old=VAssign (VLId new) e
  go (Vif c s)=Vif c (go s)
  go (Vifelse c a b)=Vifelse c (go a) (go b)
  go (Vcase e as p q)=Vcase e (map arm as) p q
  go s=s
  arm (VCaseArm es s)=VCaseArm es (go s)
  arm (VDefault s)=VDefault (go s)

cloneProcesses :: VModule -> VModule -> Maybe (VModule,[String],Bool)
cloneProcesses old new = do
  let os=declarations old; ns=declarations new
      processes m=M.fromList [(n,s) | VMStmt False s<-leaves (vm_body m),pureStmt s,
                                    [n]<-[S.toList (stmtOutputs s)]]
      op=processes old; np=processes new
      missing=M.keysSet op `S.difference` M.keysSet ns
      od=definitions old
      claims=concat [cs | VMAssign (VLId i) e<-leaves (vm_body new),Just prior<-[M.lookup (name i) od],
                         Just (_,cs)<-[roleExpr os ns missing True prior e]]
      grouped=M.fromListWith (++) [(n,[e]) | (n,e)<-claims]
      choices=[(n,target,s,r,not (S.member target (definite False ns s)))
              | (n,es)<-M.toAscList grouped,VEVar v:_<-[es],all (sameExpr (VEVar v)) es,
                let target=name v,Just s<-[M.lookup target np],
                target `S.notMember` stmtReadNames s,
                S.member target (definite True ns s),Just (VDReg,r)<-[M.lookup n os],
                M.lookup target ns==Just (VDReg,r)]
      allowed=S.fromList [n | (n,_,_,_,_)<-choices]
      rewrite current@(VMAssign (VLId i) e)=case M.lookup (name i) od of
        Just prior->case roleExpr os ns allowed True prior e of Just (value,_)->VMAssign (VLId i) value; _->current
        _->current
      rewrite x=x
      decls=[VMDecl (VVDecl VDReg r [VVar (mkVId n)]) | (n,_,_,r,_)<-choices]
      bodies=[VMStmt False (renameDestination target (mkVId n) s) | (n,target,s,_,_)<-choices]
  if null choices then Nothing else
    return (new {vm_body=mapItems rewrite (vm_body new)++decls++bodies},
            [n | (n,_,_,_,_)<-choices],or [b | (_,_,_,_,b)<-choices])

-- Recover explicit historical case-arm labels only when the current default
-- exists. Missing defaults never become don't-care contracts.
caseSkeleton :: VModule -> VModule -> VModule
caseSkeleton old new = new {vm_body=mapItems change (vm_body new)}
 where
  oldMap=M.fromList [(k,x) | x<-leaves (vm_body old),Just k<-[itemKey x]]
  change current@(VMStmt False s) | pureStmt s = case itemKey current >>= (`M.lookup` oldMap) of
    Just (VMStmt False prior) -> VMStmt False (go prior s)
    _ -> current
  change x=x
  go (Valways a) (Valways b)=Valways (go a b)
  go (VAt _ a) (VAt e b)=VAt e (go a b)
  go (VSeq as) (VSeq bs) | length as==length bs=VSeq (zipWith go as bs)
  go (Vcase e as p q) (Vcase f bs r s)
    | sameExpr e f && any isDefault bs =
      let present=concat [es | VCaseArm es _<-bs]
          add arm@(VCaseArm es body)=case [labels | VCaseArm labels oldBody<-as,mapStmtExprs canon oldBody==mapStmtExprs canon body] of
            [labels] -> VCaseArm (es++[l | l<-labels,not (any (sameExpr l) present)]) body
            _ -> arm
          add arm=arm
      in Vcase f (map add bs) r s
  go _ b=b
  isDefault (VDefault _)=True
  isDefault _=False

-- A complete binary case may recover a historical default for an explicitly
-- enumerated value. This is not an X contract: the candidate is binary-only,
-- every sized label is checked, and incomplete/duplicate cases are declined.
binaryCaseLabels :: Symbols -> VExpr -> [VCaseArm] -> Maybe [Integer]
binaryCaseLabels sy selector arms=do
  width<-expressionWidth sy selector
  if width<=0 || width>20 || any isDefault arms then Nothing else do
    values<-mapM (value width) (concat [xs | VCaseArm xs _<-arms])
    if toInteger (length values)==2^width && S.size (S.fromList values)==length values
      then Just values else Nothing
 where
  isDefault (VDefault _)=True
  isDefault _=False
  value width (VEWConst _ w _ n) | w==width && n>=0 && n<2^width=Just n
  value _ _=Nothing

binaryCaseDefaults :: VModule -> VModule -> VModule
binaryCaseDefaults old new=new {vm_body=mapItems change (vm_body new)}
 where
  sy=declarations new
  prior=M.fromList [(key,s) | item@(VMStmt False s)<-leaves (vm_body old),
                            pureStmt s,Just key<-[itemKey item]]
  change item@(VMStmt False s) | pureStmt s=case itemKey item >>= (`M.lookup` prior) of
    Just oldS->VMStmt False (go oldS s)
    _->item
  change item=item
  go (Valways a) (Valways b)=Valways (go a b)
  go (VAt _ a) (VAt event b)=VAt event (go a b)
  go (VSeq as) (VSeq bs) | length as==length bs=VSeq (zipWith go as bs)
  go (Vcase a oldArms _ _) current@(Vcase b arms full parallel)
    | sameExpr a b,Just _<-binaryCaseLabels sy b arms,
      [defaultBody]<-[s | VDefault s<-oldArms] =
      let historicalLabels=concat [xs | VCaseArm xs _<-oldArms]
          removable labels body=mapStmtExprs canon body==mapStmtExprs canon defaultBody &&
                                all (\x->not (any (sameExpr x) historicalLabels)) labels
          removed=[body | VCaseArm labels body<-arms,removable labels body]
          retained=[arm | arm@(VCaseArm labels body)<-arms,not (removable labels body)]
      in case removed of
        body:_ -> Vcase b (retained++[VDefault body]) full parallel
        [] -> current
  go _ current=current

-- Recover a historical continuous conditional role from a small complete
-- current case. Original case arm values are used verbatim. Equality/ternary
-- X behavior differs from case dispatch, so independent binary proof is
-- mandatory even when the current case has an explicit default.
caseToConditional :: VModule -> VModule -> Maybe (VModule,[String])
caseToConditional old new=do
  let sy=declarations new; oldDefs=definitions old
      strip (Valways s)=strip s
      strip (VAt event s) | combEvent event=strip s
      strip (VSeq [s])=strip s
      strip s=s
      extract body=case strip body of
        Vcase selector arms _ _ | pureStmt body,length arms<=16 -> do
          width<-expressionWidth sy selector
          rows<-mapM (arm width) arms
          let outputs=nub [n | (_,n,_)<-rows]
              labels=concat [xs | (Just xs,_,_)<-rows]
          output<-case outputs of [n]->Just n; _->Nothing
          prior<-M.lookup output oldDefs
          case prior of VEIf {}->Just (); _->Nothing
          destination<-M.lookup output sy >>= rangeWidth . snd
          if any (\(_,_,rhs)->expressionWidth sy rhs/=Just destination) rows ||
             length labels/=S.size (S.fromList (map canon labels)) then Nothing else do
            (choices,fallback)<-case [rhs | (Nothing,_,rhs)<-rows] of
              [rhs]->Just ([(xs,value) | (Just xs,_,value)<-rows],rhs)
              [] | Just _<-binaryCaseLabels sy selector arms ->case reverse rows of
                (_,_,rhs):rest->Just ([(xs,value) | (Just xs,_,value)<-reverse rest],rhs)
                _->Nothing
              _->Nothing
            let test labels=foldr1 (VEOp defaultVId `flip` VLOr)
                                [VEOp defaultVId selector VEQ label | label<-labels]
                result=foldr (\(xs,rhs) rest->VEIf (test xs) rhs rest) fallback choices
            if output `S.member` exprNames result then Nothing else Just (output,result)
        _->Nothing
      arm width (VCaseArm labels (VAssign (VLId output) rhs))
        | not (null labels),all (valid width) labels,pureExpr rhs=Just (Just labels,name output,rhs)
      arm _ (VDefault (VAssign (VLId output) rhs)) | pureExpr rhs=Just (Nothing,name output,rhs)
      arm _ _=Nothing
      valid width (VEWConst _ w _ n)=w==width && n>=0 && n<2^width
      valid _ _=False
      replacements=M.fromList [(n,rhs) | VMStmt False body<-leaves (vm_body new),Just (n,rhs)<-[extract body]]
      change (VMStmt False body)=case extract body of
        Just (n,rhs)->VMAssign (VLId (mkVId n)) rhs
        _->VMStmt False body
      change (VMDecl (VVDecl VDReg range variables))=
        -- A mixed declaration is left untouched; splitting declarations is a
        -- separate layout proposal rather than a hidden port/type rewrite.
        if all ((`M.member` replacements).name.vvName) variables
          then VMDecl (VVDecl VDWire range variables) else VMDecl (VVDecl VDReg range variables)
      change item=item
  if M.null replacements then Nothing else Just (new {vm_body=mapItems change (vm_body new)},M.keys replacements)

equalArmTemplates :: VModule -> VModule -> VModule
equalArmTemplates old new = new {vm_body=mapItems change (vm_body new)}
 where
  os=declarations old; ns=declarations new
  ds=definitions new
  oldMap=M.fromList [(k,x) | x<-leaves (vm_body old),Just k<-[itemKey x]]
  -- Recover only helper roles already present in the historical conditional.
  -- A lost use may have been expanded in the new backend even though another
  -- consumer retained the same helper. Reusing that existing, exact-width cone
  -- avoids duplicating the expanded expression when restoring the template.
  -- This does not compact new register or new-only consumer scaffolding.
  expanded e=fmap (canon . fst) (expand 256 S.empty e)
  expand budget active e
    | budget<=0 || not (pureExpr e)=Nothing
    | otherwise=case e of
      VEVar i | Just rhs<-M.lookup (name i) ds ->do
        if name i `S.member` active || expressionWidth ns e/=expressionWidth ns rhs ||
           expressionWidth ns e==Nothing then Nothing
        else expand (budget-1) (S.insert (name i) active) rhs
      _->do
        (parts,left)<-foldM (\(parts,left) child->do
            (value,remaining)<-expand left active child
            return (value:parts,remaining)) ([],budget-1) (children e)
        rebuilt<-rebuildExpr e (reverse parts)
        return (rebuilt,left)
  historicalHelpers e=case e of
    VEVar i->[i]
    _->concatMap historicalHelpers (children e)
  reuse a b current=case
      [VEVar i | i<-nub (historicalHelpers a++historicalHelpers b),
       expressionWidth ns (VEVar i)==expressionWidth ns current,
       equivalentHelper i current] of
    x:_->x
    _->current
  equivalentHelper i current=case M.lookup (name i) ds of
    Just rhs | sameExpr rhs current -> True
    Just _ ->case expanded current of
      Just actual -> expanded (VEVar i)==Just actual
      _ -> False
    _ -> False
  expression (VEIf c _ _) current@(VEIf {})=current
  expression prior@(VEIf c a b) current
    | pureExpr c && exprNames c `S.isSubsetOf` M.keysSet ns
      && expressionWidth os prior==expressionWidth ns current
      && expressionWidth ns current/=Nothing =
        let arm=reuse a b current in VEIf c arm arm
  expression _ current=current
  change current@(VMAssign v e)=case itemKey current >>= (`M.lookup` oldMap) of
    Just (VMAssign _ prior) -> VMAssign v (expression prior e)
    _ -> current
  change current@(VMInst {})=case itemKey current >>= (`M.lookup` oldMap) of
    Just prior@(VMInst {}) | vi_module_name prior==vi_module_name current ->
      let ports=M.fromList (vi_inst_ports prior)
          port (i,Just e)=case M.lookup i ports of
            Just (Just oldE) -> (i,Just (expression oldE e))
            _ -> (i,Just e)
          port x=x
      in current {vi_inst_ports=map port (vi_inst_ports current)}
    _ -> current
  change x=x

rebuildExpr :: VExpr -> [VExpr] -> Maybe VExpr
rebuildExpr (VEUnOp i o _) [a]=Just (VEUnOp i o a)
rebuildExpr (VEOp i _ o _) [a,b]=Just (VEOp i a o b)
rebuildExpr (VEConcat _) xs=Just (VEConcat xs)
rebuildExpr (VESelect _ _ _) [a,h,l]=Just (VESelect a h l)
rebuildExpr (VESelect1 _ _) [a,i]=Just (VESelect1 a i)
rebuildExpr (VERepeat _ _) [n,a]=Just (VERepeat n a)
rebuildExpr (VEIf _ _ _) [c,a,b]=Just (VEIf c a b)
rebuildExpr x []=Just x
rebuildExpr _ _=Nothing

atPath :: [Int] -> VExpr -> Maybe VExpr
atPath [] e=Just e
atPath (i:is) e=if i<length (children e) then atPath is (children e!!i) else Nothing

replacePath :: [Int] -> VExpr -> VExpr -> Maybe VExpr
replacePath [] replacement _=Just replacement
replacePath (i:is) replacement e=do
  let xs=children e
  if i>=length xs then Nothing else do
    child<-replacePath is replacement (xs!!i)
    rebuildExpr e (take i xs++[child]++drop (i+1) xs)

shape :: Int -> VExpr -> String
shape depth e=tag e++if depth<=0 then "" else "("++intercalate "," (map (shape (depth-1)) (children e))++")"
 where
  tag (VEOp _ _ o _)="op"++show o
  tag (VEUnOp _ o _)="un"++show o
  tag (VEIf {})="if"
  tag (VEConcat _)="concat"
  tag (VERepeat _ _)="repeat"
  tag (VESelect _ _ _)="slice"
  tag (VESelect1 _ _)="bit"
  tag (VEVar _)="variable"
  tag (VEWConst {})="literal"
  tag (VEConst _)="literal"
  tag _="opaque"

paired :: [Int] -> VExpr -> VExpr -> [([Int],VExpr,VExpr)]
paired path a b
  | sameExpr a b=[]
  | otherwise=(path,a,b):if shape 0 a==shape 0 b && length (children a)==length (children b)
      then concat [paired (path++[i]) x y | (i,(x,y))<-zip [0..] (zip (children a) (children b))]
      else []

booleanForms :: VModule -> VModule -> [(String,VModule,Int)]
booleanForms old new = concatMap forms (M.toAscList groups)
 where
  os=declarations old; ns=declarations new; od=definitions old
  candidates=[((length path,path,shape 2 a,shape 2 b),(n,path,a))
    | (n,e)<-M.toAscList (definitions new),Just prior<-[M.lookup n od]
    , (path,a@(VEOp _ l op r),b)<-paired [] prior e
    , op `elem` [VLAnd,VLOr], expressionWidth os a==Just 1,expressionWidth ns b==Just 1
    , expressionWidth os l==Just 1,expressionWidth os r==Just 1
    , exprNames a `S.isSubsetOf` M.keysSet ns]
  groups=M.fromListWith (++) [(k,[v]) | (k,v)<-candidates]
  forms (_,vs)=mapMaybe (make vs) [("swap",True,False,False),("negate-left",False,True,False),
                    ("swap-negate-left",True,True,False),("negate-right",False,False,True),
                    ("swap-negate-right",True,False,True),("swap-negate-both",True,True,True)]
  make vs (label,sw,nl,nr)=
    let changes=M.fromList [(n,(path,variant sw nl nr a)) | (n,path,a)<-vs]
        change item@(VMAssign (VLId i) e)=case M.lookup (name i) changes of
          Just (path,replacement)->maybe item (VMAssign (VLId i)) (replacePath path replacement e)
          _->item
        change x=x
        m=new {vm_body=mapItems change (vm_body new)}
    in if m==new then Nothing else Just (label,m,length vs)
  variant sw nl nr (VEOp i l o r)=VEOp i (neg nl l) (if sw then if o==VLAnd then VLOr else VLAnd else o) (neg nr r)
  variant _ _ _ e=e
  neg True e=VEUnOp defaultVId VNot e
  neg False e=e

-- Recover an existing compiler-emitted conditional only when its guard is a
-- bounded Boolean formula over one typed unsigned comparison pair and the NEW
-- guard is its binary complement with exactly swapped current branches. This
-- creates one grouped proposal, not a module-wide search over arbitrary OLD
-- definitions. The independent full-cone proof still checks the proposal.
historicalConditionals :: VModule -> VModule -> Maybe (VModule,Int)
historicalConditionals old new =
  let oldDefinitions=definitions old
      changes=M.fromList [(n,prior) | (n,current)<-M.toAscList (definitions new),
        Just prior<-[M.lookup n oldDefinitions], eligible n prior current]
      change item@(VMAssign (VLId i) _)=
        maybe item (VMAssign (VLId i)) (M.lookup (name i) changes)
      change item@(VMDecl (VVDWire r (VVar i) _))=
        maybe item (VMDecl . VVDWire r (VVar i)) (M.lookup (name i) changes)
      change item=item
  in if M.null changes then Nothing else
       Just (new {vm_body=mapItems change (vm_body new)},M.size changes)
 where
  os=declarations old; ns=declarations new
  eligible n prior@(VEIf oc oa ob) current@(VEIf nc na nb)=
    pureExpr prior && pureExpr current && sameExpr oa nb && sameExpr ob na &&
    exprNames prior `S.isSubsetOf` M.keysSet ns &&
    case (M.lookup n ns >>= rangeWidth . snd,
          expressionWidth os prior,expressionWidth ns prior,expressionWidth ns current,
          comparisonForm os oc,comparisonForm ns nc) of
      (Just w,Just a,Just b,Just c,Just (ol,orr,ow,ov),Just (nl,nr,nw,nv))->
        w==a && a==b && b==c && ow==nw && sameExpr ol nl && sameExpr orr nr && ov==map not nv
      _->False
  eligible _ _ _=False
  comparisonForm sy e
    | length (take 65 (nodes e))>64=Nothing
    | otherwise=go e
   where
    nodes x=x:concatMap nodes (children x)
    go (VEUnOp _ VNot a)=do
      (l,r,w,v)<-go a;return (l,r,w,map not v)
    go (VEOp _ a o b) | o `elem` [VLAnd,VLOr]=do
      (l,r,w,av)<-go a;(ll,rr,ww,bv)<-go b
      if sameExpr l ll && sameExpr r rr && w==ww
        then return (l,r,w,zipWith (if o==VLAnd then (&&) else (||)) av bv)
        else Nothing
    go (VEOp _ a o b) | o `elem` [VULT,VULE,VUGT,VUGE,VEQ,VNE]=do
      aw<-expressionWidth sy a;bw<-expressionWidth sy b
      if aw==bw && aw>0 && pureExpr a && pureExpr b then
        let values=case o of
              VULT->[True,False,False]; VULE->[True,True,False]
              VUGT->[False,False,True]; VUGE->[False,True,True]
              VEQ->[False,True,False]; _->[True,False,True]
        in Just (a,b,aw,values)
      else Nothing
    go _=Nothing

-- References in opaque/effectful members are never rewritten by helper
-- inlining. This is a syntactic safety filter; the proof gate remains required.
allStmtNames :: VStmt -> S.Set String
allStmtNames (VAt e s)=S.union (eventNames e) (allStmtNames s)
allStmtNames (Valways s)=allStmtNames s
allStmtNames (Vinitial s)=allStmtNames s
allStmtNames (VSeq xs)=S.unions (map allStmtNames xs)
allStmtNames (VAssign l e)=S.union (S.fromList (lvalues l)) (exprNames e)
allStmtNames (VAssignA l e)=S.union (S.fromList (lvalues l)) (exprNames e)
allStmtNames (Vif c s)=S.union (exprNames c) (allStmtNames s)
allStmtNames (Vifelse c a b)=S.unions [exprNames c,allStmtNames a,allStmtNames b]
allStmtNames (Vcase e arms _ _)=S.unions (exprNames e:map allArmNames arms)
allStmtNames (Vcasex e arms _ _)=S.unions (exprNames e:map allArmNames arms)
allStmtNames (Vdumpvars _ ns)=S.fromList (map name ns)
allStmtNames (VTask _ es)=S.unions (map exprNames es)
allStmtNames (VAssert e es)=S.unions (eventNames e:map exprNames es)
allStmtNames VZeroDelay=S.empty

allArmNames :: VCaseArm -> S.Set String
allArmNames (VCaseArm es s)=S.unions (allStmtNames s:map exprNames es)
allArmNames (VDefault s)=allStmtNames s

eventNames :: VEventExpr -> S.Set String
eventNames (VEE e)=exprNames e
eventNames (VEEOr a b)=S.union (eventNames a) (eventNames b)
eventNames (VEEposedge e)=exprNames e
eventNames (VEEnegedge e)=exprNames e
eventNames (VEEMacro _ e)=exprNames e

opaqueNames :: VMItem -> S.Set String
opaqueNames (VMInst _ _ ps xs)=S.unions (map exprNames (either (map snd) (mapMaybe snd) ps ++ mapMaybe snd xs))
opaqueNames (VMStmt False s) | pureStmt s=S.empty
opaqueNames (VMStmt _ s)=allStmtNames s
opaqueNames (VMFunction (VFunction _ _ _ s))=allStmtNames s
opaqueNames (VMAssign l e) | not (pureExpr e)=S.union (S.fromList (lvalues l)) (exprNames e)
opaqueNames _=S.empty

-- Selectively inline new-only, exact-width pure helpers. Operators retain
-- their original AST and assignment width is checked before substitution.
-- A bundled relational form can cross a neutral textual intermediate, while
-- the independent binary proof rejects any unsound alternative.
inlineNewHelpers :: VModule -> VModule -> Maybe (VModule,[String])
inlineNewHelpers = inlineNewHelpersWithState False

-- State identity, event expressions, branch tests and assignment operators are
-- fixed. Only exact-width pure RHS helpers of nonblocking updates are sites.
-- Initial/effectful blocks and any helper also used by a control stay opaque.
stateRhsBlock :: VStmt -> Bool
stateRhsBlock (Valways (VAt event body))=edge event && bodyOK body
 where
  edge (VEEposedge _) = True
  edge (VEEnegedge _) = True
  edge (VEEOr a b)=edge a || edge b
  edge _=False
  bodyOK (VAssignA (VLId _) rhs)=pureExpr rhs
  bodyOK (VSeq xs)=not (null xs) && all bodyOK xs
  bodyOK (Vif c s)=controlOK c && bodyOK s
  bodyOK (Vifelse c a b)=controlOK c && bodyOK a && bodyOK b
  bodyOK _=False
  -- Reset-value macros occur in immutable branch tests in ordinary BSC RTL.
  -- Keeping that syntax is safe; it is never expanded or substituted here.
  controlOK (VEFctCall _ _)=False
  controlOK (VEIndex _ _)=False
  controlOK x=all controlOK (children x)
stateRhsBlock _=False

stateControls :: VStmt -> S.Set String
stateControls (Valways s)=stateControls s
stateControls (VAt event s)=S.union (eventNames event) (stateControls s)
stateControls (VSeq xs)=S.unions (map stateControls xs)
stateControls (VAssignA lhs _)=S.fromList (lvalues lhs)
stateControls (Vif c s)=S.union (exprNames c) (stateControls s)
stateControls (Vifelse c a b)=S.unions [exprNames c,stateControls a,stateControls b]
stateControls s=allStmtNames s

mapStateRhs :: (VExpr -> VExpr) -> VStmt -> VStmt
mapStateRhs f (Valways s)=Valways (mapStateRhs f s)
mapStateRhs f (VAt event s)=VAt event (mapStateRhs f s)
mapStateRhs f (VSeq xs)=VSeq (map (mapStateRhs f) xs)
mapStateRhs f (Vif c s)=Vif c (mapStateRhs f s)
mapStateRhs f (Vifelse c a b)=Vifelse c (mapStateRhs f a) (mapStateRhs f b)
mapStateRhs f (VAssignA lhs rhs)=VAssignA lhs (f rhs)
mapStateRhs _ s=s

inlineNewHelpersWithState :: Bool -> VModule -> VModule -> Maybe (VModule,[String])
inlineNewHelpersWithState allowState old new = do
  let os=declarations old; ns=declarations new
      defs=definitions new
      protect (VMStmt False s) | allowState && stateRhsBlock s=stateControls s
      protect x=opaqueNames x
      protected=S.unions (map protect (leaves (vm_body new)))
      eligible=M.fromList [(n,e) | (n,e)<-M.toAscList defs,
        M.notMember n os, n `S.notMember` portNames new, n `S.notMember` protected,
        Just (VDWire,r)<-[M.lookup n ns],
        rangeWidth r==expressionWidth ns e, rangeWidth r/=Nothing, pureExpr e]
      -- A historical destination alone is not evidence for inlining: its new
      -- RHS may be novel state/control scaffolding. Require bounded expansion
      -- to restore the actual old expression operators and aligned leaves.
      -- Every terminal consumer must qualify; otherwise retain the helper and
      -- its dependencies rather than compacting an unrelated novel consumer.
      oldRoles=M.fromList [(key,item) | item<-leaves (vm_body old),Just key<-[itemKey item]]
      reads (VMAssign _ rhs)=exprNames rhs
      reads (VMDecl (VVDWire _ _ rhs))=exprNames rhs
      reads (VMStmt False body) | pureStmt body=stmtReadNames body
      reads _=S.empty
      reach names=let next=S.union names (S.unions [exprNames rhs | n<-S.toList names,
                                                       Just rhs<-[M.lookup n eligible]])
                 in if next==names then names else reach next
      expand table fuel _ _ | fuel<=0=Nothing
      expand table fuel seen e@(VEVar i)=case M.lookup (name i) table of
        Nothing->Just (e,fuel-1)
        Just rhs | name i `S.member` seen->Nothing
                 | otherwise->expand table (fuel-1) (S.insert (name i) seen) rhs
      expand table fuel seen e=do
        (xs,left)<-foldM (\(done,remaining) child->do
          (value,next)<-expand table remaining seen child
          return (done++[value],next)) ([],fuel-1) (children e)
        value<-rebuildExpr e xs
        return (value,left)
      expandedShape prior current=case expand eligible (256::Int) S.empty current of
        Just (value,_) -> sameHistoricalExpr prior value
        Nothing -> False
      -- This existing, bounded F11 normalization permits the bundled
      -- !(a <= b) -> a > b recovery. It is not an arbitrary operator hole.
      relational=mapExpr (\e->case e of
        VEUnOp i VNot (VEOp j a o b)->case complement o of
          Just p->VEOp j a p b
          Nothing->VEUnOp i VNot (VEOp j a o b)
        _->e)
      sameHistoricalExpr a b=match (relational a) (relational b)
      -- A changed predicate can still occupy an existing conditional shell.
      -- Keep both historical data arms exact, and reject any new boundary
      -- leaf (including new state hidden behind an old named equation).
      historicalGuard value=expressionWidth ns value==Just 1 && pureExpr value
        && case guardWalk (256::Int) S.empty value of Just _->True; Nothing->False
      guardWalk fuel seen _ | fuel<=0=Nothing
      guardWalk fuel seen (VEVar i)=
        let n=name i
        in if n `S.member` seen || M.notMember n os
           || expressionWidth os (VEVar i)/=expressionWidth ns (VEVar i)
           then Nothing else case M.lookup n defs of
                Nothing->Just (fuel-1)
                Just rhs | pureExpr rhs->guardWalk (fuel-1) (S.insert n seen) rhs
                _->Nothing
      guardWalk fuel seen value=foldM (\remaining child->guardWalk remaining seen child)
                                      (fuel-1) (children value)
      exactOperand a b
        | expressionWidth os a/=expressionWidth ns b=False
        | otherwise=case (a,b) of
            (VEVar i,VEVar j)->name i==name j && M.member (name i) os
            (VEConst x,VEConst y)->x==y
            (VEWConst _ w r x,VEWConst _ z s y)->w==z && r==s && x==y
            _->not (null (children a)) && shape 0 a==shape 0 b
               && length (children a)==length (children b)
               && and (zipWith exactOperand (children a) (children b))
      relation op=op `elem` [VULT,VULE,VUGT,VUGE]
      match a b
        | expressionWidth os a/=expressionWidth ns b=False
        | otherwise=case (a,b) of
            (VEVar i,VEVar j)->name i==name j && M.member (name i) os
            (VEConst x,VEConst y)->x==y
            (VEWConst _ w r x,VEWConst _ z s y)->w==z && r==s && x==y
            (VEIf c x y,VEIf d u v)->match x u && match y v
              && (match c d || historicalGuard d)
            (VEOp _ x p y,VEOp _ u q v) | relation p && relation q ->
              exactOperand x u && exactOperand y v
            _->not (null (children a)) && shape 0 a==shape 0 b
               && length (children a)==length (children b)
               && and (zipWith match (children a) (children b))
      historicalItem (VMAssign l a) (VMAssign k b)=l==k && expandedShape a b
      historicalItem (VMStmt False a) (VMStmt False b)=historicalStmt a b
      historicalItem _ _=False
      historicalStmt (Valways a) (Valways b)=historicalStmt a b
      historicalStmt (VAt a x) (VAt b y)=combEvent a && combEvent b && historicalStmt x y
      historicalStmt (VSeq xs) (VSeq ys)=length xs==length ys && and (zipWith historicalStmt xs ys)
      historicalStmt (VAssign l a) (VAssign k b)=l==k && expandedShape a b
      historicalStmt (Vif a x) (Vif b y)=expandedShape a b && historicalStmt x y
      historicalStmt (Vifelse a x y) (Vifelse b u v)=expandedShape a b && historicalStmt x u && historicalStmt y v
      historicalStmt (Vcase a xs p q) (Vcase b ys r s)=p==r && q==s && expandedShape a b
        && length xs==length ys && and (zipWith historicalArm xs ys)
      historicalStmt _ _=False
      historicalArm (VCaseArm as a) (VCaseArm bs b)=map canon as==map canon bs && historicalStmt a b
      historicalArm (VDefault a) (VDefault b)=historicalStmt a b
      historicalArm _ _=False
      isHelper (VMAssign (VLId i) _)=name i `M.member` eligible
      isHelper (VMDecl (VVDWire _ (VVar i) _))=name i `M.member` eligible
      isHelper _=False
      allReads=S.unions [reads item | item<-leaves (vm_body new)]
      consumers=[(reach (reads item),maybe False (`historicalItem` item)
                    (itemKey item >>= (`M.lookup` oldRoles)))
                | item<-leaves (vm_body new),not (isHelper item)]
                ++[(reach (S.singleton n),False) | n<-M.keys eligible,n `S.notMember` allReads]
      justified=S.difference (S.unions [xs | (xs,True)<-consumers])
                             (S.unions [xs | (xs,False)<-consumers])
      allowed=if allowState then eligible else M.filterWithKey (\n _->n `S.member` justified) eligible
      expanded=M.mapMaybeWithKey (\n e->fst <$> expand allowed (256::Int) (S.singleton n) e) allowed
      closed=M.filter (\e->S.null (exprNames e `S.intersection` M.keysSet allowed)) expanded
      replace e@(VEVar i)=fromMaybe e (M.lookup (name i) closed)
      replace e=e
      rw=mapExpr replace
      eventStmt (Valways s)=Valways (eventStmt s)
      eventStmt (VAt e s)=VAt (mapEvent rw e) (eventStmt s)
      eventStmt s=s
      item (VMAssign l e)=VMAssign l (rw e)
      item (VMDecl (VVDWire r v e))=VMDecl (VVDWire r v (rw e))
      item (VMStmt False s) | pureStmt s=VMStmt False (eventStmt (mapStmtExprs rw s))
      item (VMStmt False s) | allowState && stateRhsBlock s=VMStmt False (mapStateRhs rw s)
      item x=x
      remove (VMComment c x)=map (VMComment c) (remove x)
      remove (VMRegGroup i s c x)=map (VMRegGroup i s c) (remove x)
      remove (VMGroup t xs)=[VMGroup t (map (concatMap remove) xs)]
      remove (VMDecl (VVDecl t r vs))=
        let kept=[v | v<-vs,name (vvName v) `M.notMember` closed]
        in [VMDecl (VVDecl t r kept) | not (null kept)]
      remove (VMAssign (VLId i) _) | name i `M.member` closed=[]
      remove (VMDecl (VVDWire _ (VVar i) _)) | name i `M.member` closed=[]
      remove x=[item x]
  if M.null closed then Nothing else
    Just (new {vm_body=concatMap remove (vm_body new)},M.keys closed)

-- Final lowering can create scalar process destinations absent from the
-- earlier graph. Match their complete typed process objects, including event,
-- case priority and default absence. This proposes only a simultaneous rename;
-- the independent gate receives the explicit candidate-to-current relation.
lateProcessIdentities :: VModule -> VModule -> Maybe (VModule,[(String,String)])
lateProcessIdentities old new =
  let oldProcesses=processes old; newProcesses=processes new
      namespace=S.fromList (map name (collectRenameableVIds (vm_body new)))
                  `S.union` portNames new
      matches width statement=filter (\(_,w,s)->w==width && s==statement)
      pairs=[(current,historical) | (historical,width,statement)<-oldProcesses,
        historical `S.notMember` namespace,
        [(current,_,_)]<-[matches width statement newProcesses],
        length (matches width statement oldProcesses)==1]
      mapping=M.fromList pairs
      rename ident@(VId spelling source info)=case M.lookup spelling mapping of
        Just target->VId target source info
        Nothing->ident
  in if null pairs || length pairs/=S.size (S.fromList (map fst pairs))
                   || length pairs/=S.size (S.fromList (map snd pairs))
       then Nothing else Just (new {vm_body=mapRenameableVIds rename (vm_body new)},pairs)
 where
  writers m=M.fromListWith (+) [(n,1::Int) | item<-leaves (vm_body m),n<-written item]
  written (VMStmt _ s)=S.toList (stmtOutputs s)
  written (VMAssign l _) = lvalues l
  written (VMDecl (VVDWire _ v _))=[name (vvName v)]
  written _=[]
  processes m =
    let sy=declarations m; counts=writers m
    in [(output,width,renameDestination output (mkVId "__eco_destination__") (normalize statement))
       | item<-eligible (vm_body m),VMStmt False statement@(Valways (VAt event body))<-[item],
         pureStmt statement,[output]<-[S.toList (stmtOutputs body)],
         output `S.notMember` portNames m,
         output `S.notMember` (stmtReadNames body `S.union` eventNames event),
         M.lookup output counts==Just 1,
         Just (VDReg,range)<-[M.lookup output sy],Just width<-[rangeWidth range]]
  eligible=concatMap visit
  visit x@(VMComment _ body) | safeComments [x]=visit body
  visit x@(VMRegGroup _ _ _ body) | safeComments [x]=visit body
  visit (VMGroup False xss)=eligible (concat xss)
  visit item@(VMStmt False _ )=[item]
  visit _=[]
  normalize (Valways body)=Valways (normalize body)
  normalize (VAt event body)=VAt (mapEvent canon event) (normalize body)
  normalize (VSeq bodies)=VSeq (map normalize bodies)
  normalize (VAssign l rhs)=VAssign l (canon rhs)
  normalize (Vif condition body)=Vif (canon condition) (normalize body)
  normalize (Vifelse condition a b)=Vifelse (canon condition) (normalize a) (normalize b)
  normalize (Vcase selector arms parallel full)=Vcase (canon selector) (map arm arms) parallel full
  normalize statement=statement
  arm (VCaseArm labels body)=VCaseArm (map canon labels) (normalize body)
  arm (VDefault body)=VDefault (normalize body)

-- Match only unique whole-register read aliases across a declared width
-- increase. The register identity must already exist on both sides; the NEW
-- wire width, RHS and every consumer expression remain intact. This recovers
-- a historical spelling without forcing unlike-width graph nodes to match.
widthGrowthAliases :: VModule -> VModule -> Maybe (VModule,[(String,String)])
widthGrowthAliases old new =
  let os=declarations old; ns=declarations new
      oldNames=S.fromList (map name (collectRenameableVIds (vm_body old))) `S.union` portNames old
      newNames=S.fromList (map name (collectRenameableVIds (vm_body new))) `S.union` portNames new
      aliases m sy other=M.fromListWith (++)
        [(name root,[(n,width)]) | (n,VEVar root)<-M.toAscList (definitions m),
          n `S.notMember` other,n `S.notMember` portNames m,
          Just (VDWire,r)<-[M.lookup n sy],Just width<-[rangeWidth r],
          Just (VDReg,rr)<-[M.lookup (name root) sy],rangeWidth rr==Just width]
      oa=aliases old os newNames; na=aliases new ns oldNames
      pairs=[(newName,oldName) | (root,[(oldName,oldWidth)])<-M.toAscList oa,
        Just [(newName,newWidth)]<-[M.lookup root na],newWidth>oldWidth]
      mapping=M.fromList pairs
      rename ident@(VId spelling source info)=case M.lookup spelling mapping of
        Just target->VId target source info
        Nothing->ident
  in if null pairs then Nothing else
       Just (new {vm_body=mapRenameableVIds rename (vm_body new)},pairs)

-- Bounded exceptional-region reuse is restricted to modules consisting only
-- of pure continuous equations. The new guard and possible replacement values
-- come from actual NEW muxes over fixed inputs; OLD contributes only a complete
-- proposed cone. Every such proposal requires full binary equivalence.
guardedOldCones :: VModule -> VModule -> [(String,VModule)]
guardedOldCones old new
  | not (continuous old && continuous new) || vm_ports old/=vm_ports new=[]
  | otherwise=take 24
      [("guard-"++show index++"-"++polarity,
        new {vm_body=mapItems (replace output replacement) (vm_body old)})
      | (index,(condition,a,b))<-zip [0::Int ..] muxes
      , (output,prior)<-M.toAscList (definitions old),output `S.member` outputs
      , replacementValue<-[a,b],Just alternative<-[broadcast prior replacementValue]
      , (polarity,replacement)<-[("true",VEIf condition alternative prior),
                                 ("false",VEIf condition prior alternative)]]
 where
  os=declarations old; ns=declarations new
  inputs=S.fromList [name i | (args,_)<-vm_ports new,VAInput i _<-args]
  outputs=S.fromList [name i | (args,_)<-vm_ports new,VAOutput i _<-args]
  continuous m=safeComments (vm_body m) && all safe (contextLeaves (vm_body m))
  safe (False,VMDecl (VVDecl t _ vs))=t `elem` [VDWire,VDInput,VDOutput]
      && all (\v->case v of VVar _->True; _->False) vs
  safe (False,VMDecl (VVDWire _ (VVar _) e))=pureExpr e
  safe (False,VMAssign (VLId _) e)=pureExpr e
  safe _=False
  expressions e=e:concatMap expressions (children e)
  muxes=nub [(canon c,canon a,canon b)
    | e<-M.elems (definitions new),VEIf c a b<-expressions e,
      pureExpr c,expressionWidth ns c==Just 1,
      S.unions (map exprNames [c,a,b]) `S.isSubsetOf` inputs,
      expressionWidth ns a==expressionWidth ns b,expressionWidth ns a/=Nothing,
      not (sameExpr a b)]
  broadcast prior value=do
    width<-expressionWidth ns value; total<-expressionWidth os prior
    if width==total then Just value else case prior of
      VEConcat parts | not (null parts),all ((==Just width).expressionWidth os) parts ->
        Just (VERepeat (VEConst (toInteger (length parts))) value)
      _->Nothing
  replace output value (VMAssign (VLId i) _) | name i==output=VMAssign (VLId i) value
  replace _ _ x=x

-- Exact unsigned interval reasoning under an explicit sign-bit guard. Alias
-- expansion is width checked; this never assumes a signed Verilog operation,
-- an initialization invariant or a known value of an arbitrary input bit.
rangeForms :: VModule -> VModule
rangeForms m=transformExpressions (mapExpr simplify) m
 where
  sy=declarations m; defs=definitions m
  expand depth e | depth<=0=e
  expand depth e@(VEVar i)=case M.lookup (name i) defs of
    Just rhs | pureExpr rhs,expressionWidth sy rhs==expressionWidth sy e -> expand (depth-1) rhs
    _->e
  expand _ e=e
  base e=case expand (16::Int) e of
    VEVar i->do width<-expressionWidth sy (VEVar i);return (i,width)
    _->Nothing
  sign e=case expand 16 e of
    VESelect1 x n->do (i,width)<-base x;k<-literal n;if k==width-1 then Just (i,width) else Nothing
    VESelect x h l | sameExpr h l->sign (VESelect1 x h)
    _->Nothing
  extension e=case expand 16 e of
    VEConcat [s,x]->do
      (i,width)<-base x
      case s of
        VERepeat count z->do (j,w)<-sign z;n<-literal count
                             if i==j && width==w && n>0 then Just (i,width,width+n) else Nothing
        _->do (j,w)<-sign s
              if i==j && width==w then Just (i,width,width+1) else Nothing
    _->Nothing
  guard e=case e of
    VEUnOp _ VNot x->do (i,w)<-sign x;return (i,w,False)
    _->do (i,w)<-sign e;return (i,w,True)
  simplify current@(VEOp _ g VLAnd cmp)=fromMaybe current (reduce g cmp)
  simplify e=e
  reduce g cmp=do
    (i,width,negative)<-guard g
    (wide,relation,k)<-case cmp of
      VEOp _ wide relation (VEWConst _ cw _ k)->do
        (_,_,ew)<-extension wide
        if cw==ew && k>=0 && k<2^cw && relation `elem` [VULT,VULE,VUGT,VUGE,VEQ,VNE]
          then Just (wide,relation,k) else Nothing
      _->Nothing
    (j,w,total)<-extension wide
    if i/=j || w/=width || width<=0 || width>256 || total>512 then Nothing else do
      let lo=if negative then 2^(width-1) else 0
          hi=if negative then 2^width-1 else 2^(width-1)-1
          offset=if negative then 2^total-2^width else 0
          threshold=k-offset
          (lower,upper)=case relation of
            VULT->(lo,min hi (threshold-1))
            VULE->(lo,min hi threshold)
            VUGT->(max lo (threshold+1),hi)
            VUGE->(max lo threshold,hi)
            VEQ->(max lo threshold,min hi threshold)
            _->(lo,hi)
      if relation==VNE then Nothing
      else if lower>upper then Just (VEWConst defaultVId 1 2 0)
      else if lower==upper then Just (VEOp defaultVId (VEVar i) VEQ (VEWConst defaultVId width 16 lower))
      else if lower==lo && upper==hi then Just g
      else Nothing

-- Each returned program is merely a proposal. Bounded enumeration is explicit
-- and deterministic; absence of a proposal never claims impossibility.
generateASTCandidates :: [String] -> Int -> VProgram -> VProgram -> [ASTCandidate]
generateASTCandidates = generateASTCandidatesWithContracts []

-- Explicit descriptors are proof requests. No missing default, environment
-- annotation, reset promise or source type is inferred to be a contract.
generateASTCandidatesWithContracts :: [ASTContract] -> [String] -> Int -> VProgram -> VProgram -> [ASTCandidate]
generateASTCandidatesWithContracts contracts features budget (VProgram [old] _ _) newP@(VProgram [new] dpis comments)
  | budget<=0 || vm_name old/=vm_name new || vm_ports old/=vm_ports new = []
  | otherwise = take budget . unique $ concat
      [ case widthGrowthAliases old new of
          Nothing->[]
          Just (m,pairs)->proposal "F01" "Historical unique register-read alias identity across width growth" False
            [("width-growth-read-alias",[("from",a),("to",b),("newWidthAndConsumers","preserved")]) | (a,b)<-pairs] (Just m)
      , case lateProcessIdentities old new of
          Nothing->[]
          Just (m,pairs)->
            [candidate {acStateMap=[(target,source) | (source,target)<-pairs]}
            | candidate<-proposal "F02" "Unique complete late process identities" False
                [("late-process-identity",[("from",source),("to",target),
                   ("eventBodyPriorityAndDefault","unchanged")]) | (source,target)<-pairs] (Just m)]
      , [ candidate {acStateMap=[(target,source) | (source,target)<-srpForwardNames permutation]}
        | "F15" `elem` features
        , permutation<-stateRolePermutations budget old new
        , candidate<-proposal "F15" "Historical state identity from unchanged typed output roles" False
            [("state-role-permutation",[("sourceState",srpSourceState permutation),
                ("historicalState",srpHistoricalState permutation),
                ("supportingOutputs",intercalate "," (srpOutputConsumers permutation)),
                ("stateTransitionsAndScaffolding","preserved bijectively")])]
            (Just (srpModule permutation))]
      , proposal "F03" "Historical process order within current AST containers" False
          [("process-order",[("nonProcessAnchors","preserved"),("currentComments","preserved")])]
          (Just (processOrder old new))
      , proposal "F04" "Historical typed declaration/member/group layout" False
          [("historical-layout",[("semanticBodies","new"),("headerComments","new")])]
          (historicalLayout old new)
      , proposal "F04" "Historical populated-signal order for current assignment groups" False
          [("assignment-group-order",[("identity","unique populated signal IDs"),
             ("currentBodiesAndComments","preserved"),("unmatchedGroups","fixed slots"),
             ("scopeAndEffectBarriers","preserved")])]
          (Just (assignmentGroupOrder old new))
      , [ candidate
        | "F04" `elem` features
        , (operation,proposed)<-declarationProposals safeComments old new
        , candidate<-proposal "F04" "Historical declaration partitions with current types and new state" False
            [(operation,[("historicalEvidence","unique declaration member identities"),
                ("currentTypesAndVariables","preserved"),("newDeclarations","separate"),
                ("semanticBarriers","preserved")])] (Just proposed)]
      , proposal "F11" "Typed direct relational forms for negated comparisons" True
          [("complement-relations",[("obligation","ordinary-new binary equivalence")])]
          (Just (relationalForms new))
      , proposal "F11" "Exact typed range simplification under explicit bit guard" True
          [("guarded-range",[("domain","binary"),("boundaryAssumptions","none")])]
          (Just (rangeForms new))
      , case historicalConditionals old new of
          Nothing->[]
          Just (m,count)->proposal "F11" "Historical comparison complement with swapped conditional branches" True
            [("historical-conditional",[("sites",show count),("guardNodeBudget","64"),
              ("candidateScope","one typed comparison pair and exact swapped branches")])] (Just m)
      , [ candidate
        | "F08" `elem` features
        , representative<-representativeProposals expressionWidth safeComments budget old new
        , candidate<-proposal "F08" "Historical alias representative computing the current value" False
            [("historical-alias-representative",[("source",repSource representative),
                ("target",repTarget representative),("consumers",intercalate "," (repConsumers representative)),
                ("valueNodeBudget","1024"),("dependencyNodeBudget","256"),
                ("declarationsAndStateScaffolding","unchanged")])]
            (Just (repModule representative))]
      , roleCandidates
      , cloneCandidates
      , inlineCandidates
      , stateInlineCandidates
      , proposal "F09" "Historical explicit case-arm skeleton with current default" False
          [("case-skeleton",[("missingDefaultIsDontCare","false")])]
          (Just (caseSkeleton old new))
      , proposal "F09" "Historical default recovered from explicitly exhaustive binary case" True
          [("binary-case-default",[("missingDefaultIsDontCare","false"),("selectorCoverage","independently exhaustive")])]
          (Just (binaryCaseDefaults old new))
      , caseConditionalCandidates
      , proposal "F10" "Historical equal-arm conditional templates" False
          [("equal-arm-template",[("childDirections","independent proof obligation")])]
          (Just (equalArmTemplates old new))
      , concat [proposal "F11" ("Guarded historical pure cone: "++label) True
          [("guarded-old-cone",[("template",label),("guards","new fixed-input muxes"),("candidateBudget","24")])] (Just m)
          | (label,m)<-guardedOldCones old new]
      , concat [proposal "F11" ("Grouped historical Boolean form: "++label) True
          [("bounded-boolean-form",[("variant",label),("sites",show count)])] (Just m)
          | (label,m,count)<-booleanForms old new]
      , contractCandidates
      ]
 where
  proposal feature description binary ops mm = case mm of
    Just m | feature `elem` features && m/=new ->
      [ASTCandidate feature description (VProgram [m] dpis comments) ops binary [] []]
    _ -> []
  roleCandidates=case restoreRoles old new of
    Nothing->[]
    Just (m,restored)->
      let ops=[("restore-current-value-at-historical-role",[("name",n)]) | n<-restored]
          simple n=case M.lookup n (definitions m) of Just (VEVar _)->True; _->False
          feature=if all simple restored then "F07" else "F08"
          base=proposal feature "Historical typed roles computing current values" False ops (Just m)
          laid=proposal feature "Historical typed roles and member layout" False ops (historicalLayout old m)
          boolean=concat [proposal "F08" ("Restore roles with checked Boolean form: "++label) True
                         (ops++[("bounded-boolean-form",[("variant",label),("sites",show count)])])
                         (Just (fromMaybe value (historicalLayout old value)))
                         | (label,value,count)<-booleanForms old m,"F11" `elem` features]
      in base++laid++boolean
  inlineCandidates=case inlineNewHelpers old new of
    Nothing->[]
    Just (m,removed)->
      let ops=[("inline-new-only-pure-helper",[("name",n),("width","preserved"),("expansionNodeBudget","256")]) | n<-removed]
          plain=proposal "F08" "Inline typed new-only helpers at current consumer roles" False ops (Just m)
          laid=proposal "F08" "Inline new-only helpers with historical layout" False ops (historicalLayout old m)
          rel=relationalForms m
          bundled=proposal "F08" "Inline helpers with checked direct relational forms" True
                    (ops++[("complement-relations",[("obligation","binary equivalence")])])
                    (Just (fromMaybe rel (historicalLayout old rel)))
      in plain++laid++[c | c<-bundled,"F11" `elem` features]
  cloneCandidates=case cloneProcesses old new of
    Nothing->[]
    Just (m,restored,binary)->
      let ops=[("selective-pure-process-clone",[("name",n),("binaryOnly",show binary)]) | n<-restored]
      in proposal "F07" "Selectively clone current complete pure processes" binary ops (Just m)
         ++proposal "F07" "Selectively clone current processes in historical layout" binary ops (historicalLayout old m)
  -- Retained only as an explicit diagnostic, never in F01..F20/default search.
  -- Smaller novel register plumbing has no established downstream ECO benefit.
  stateInlineCandidates
    | "F08StateRHSDiagnostic" `notElem` features=[]
    | otherwise=case inlineNewHelpersWithState True old new of
    Nothing->[]
    Just (m,removed)->proposal "F08StateRHSDiagnostic" "Diagnostic: inline sequential RHS helpers beyond compiler normal form" False
      [("inline-sequential-rhs",[("name",n),("eventAndControlSkeleton","fixed"),
                                  ("initialAndEffects","unchanged"),("expansionNodeBudget","256")]) | n<-removed]
      (Just m)
  caseConditionalCandidates
    | "F09ConditionalDiagnostic" `notElem` features=[]
    | otherwise=case caseToConditional old new of
    Nothing->[]
    Just (m,outputs)->proposal "F09ConditionalDiagnostic" "Diagnostic: complete current case in historical continuous conditional role" True
      [("case-to-conditional",[("output",n),("armBudget","16"),("domain","binary")]) | n<-outputs] (Just m)
  contractCandidates=concatMap requested contracts
  requested contract@(SpecializeUnreachableXArm ident output _)=
    case M.lookup output (definitions new) of
      Just (VEIf condition a b) | literalX a /= literalX b ->
        let defined=if literalX a then b else a
            replacements=defined : maybe [] (:[]) (M.lookup output (definitions old))
        in concat [contractProposal "F12" ident contract output rhs
                  | rhs<-nub replacements, sameWidth output rhs]
      _->[]
  requested contract@(AssertCombinational ident _)=
    concat [contractProposal "F13" ident contract output rhs
           | (output,rhs)<-M.toAscList (definitions old),
             M.member output (definitions new),sameWidth output rhs]
  literalX (VEUnknown width spelling)=width>0 && not (null spelling) && all (`elem` "xX?") spelling
  literalX (VETriConst bits)=not (null bits) && all (==Vx) bits
  literalX _=False
  sameWidth output rhs=pureExpr rhs && exprNames rhs `S.isSubsetOf` M.keysSet (declarations new)
    && expressionWidth (declarations new) rhs/=Nothing
    && expressionWidth (declarations new) rhs==(M.lookup output (declarations new) >>= rangeWidth . snd)
  contractProposal feature ident contract output rhs=
    [c {acContracts=[contract]} | c<-proposal feature "Historical expression with independently discharged explicit contract" True
       [("explicit-contract",[("id",ident),("output",output),("assumed","false")])]
       (Just (new {vm_body=mapItems replace (vm_body new)}))]
    where
      replace (VMAssign (VLId i) _) | name i==output=VMAssign (VLId i) rhs
      replace (VMDecl (VVDWire range (VVar i) _)) | name i==output=VMDecl (VVDWire range (VVar i) rhs)
      replace item=item
  unique = go []
  go _ []=[]
  go seen (c:cs) | acProgram c==newP || acProgram c `elem` seen = go seen cs
                 | otherwise = c:go (acProgram c:seen) cs
generateASTCandidatesWithContracts _ _ _ _ _ = []
