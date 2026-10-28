-- Experimental, complete typed name assignment.  Evidence ranks legal name
-- reuse; it is never a claim that old and new logic are equivalent.
module ECOAggressive
    ( aggressiveAssign, AssignmentAudit(..), TypeGroup(..), Disposition(..)
    , AssignmentScore(..), auditJSON
    ) where

import ECO.Types
import ECOMatchTypes
import Data.Char (ord)
import Data.List (foldl', intercalate, sort, sortOn)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric (showHex)

type Nodes = M.Map EcoKey EcoNode
type Assignment = M.Map EcoKey EcoKey -- new -> old, eligible named nodes only
data LocalShape = Ref String String | Expr String String (Maybe String) [Int] (Maybe String)
    deriving (Eq,Ord)
data AssignmentScore = AssignmentScore
    { scoreWeighted :: Int, scoreLocal :: Int, scoreContent :: Int
    , scoreIdentity :: Int } deriving (Eq,Ord,Read,Show)
data TypeGroup = TypeGroup
    { groupType :: String, groupOld :: [EcoKey], groupNew :: [EcoKey]
    , groupMatched :: Int, groupUnmatchedOld :: [EcoKey], groupUnmatchedNew :: [EcoKey]
    } deriving (Eq,Read,Show)
data Disposition = Disposition
    { dispositionKey :: EcoKey, dispositionName :: String, dispositionType :: String
    , dispositionStatus :: String, dispositionCounterpart :: Maybe EcoKey
    , dispositionEvidence :: String
    } deriving (Eq,Read,Show)
data AssignmentAudit = AssignmentAudit
    { auditGroups :: [TypeGroup], auditOld :: [Disposition], auditNew :: [Disposition]
    , auditInitial :: AssignmentScore, auditFinal :: AssignmentScore
    , auditIterations :: Int, auditStop :: String
    } deriving (Eq,Read,Show)

-- Named operands terminate local expansion. Anonymous subgraphs are memoized;
-- no expression tree is duplicated by a fanout expansion.
localShapes :: String -> Nodes -> [EcoKey] -> Assignment -> M.Map LocalShape Int ->
               (M.Map EcoKey Int, M.Map LocalShape Int)
localShapes side nodes roots refs table0 = foldl' visit (M.empty,table0) roots
  where
    intern shape table = let i=M.findWithDefault (M.size table) shape table
                         in (i,M.insert shape i table)
    visit state@(memo,table) k
        | M.member k memo = state
        | otherwise =
            let n=nodes M.! k
                (ids,memo',table')=foldl' child ([],memo,table) (enOperands n)
                opaque=if enOpaque n then Just (side++k) else Nothing
                (i,t)=intern (Expr (enType n) (enOp n) (enBoundary n) (reverse ids) opaque) table'
            in (M.insert k i memo',t)
    child (ids,memo,table) k
        | isJust (enName n) =
            let identity=if side=="old:" then "old:"++k
                         else maybe (side++k) ("old:"++) (M.lookup k refs)
                (i,t)=intern (Ref (enType n) identity) table
            in (i:ids,memo,t)
        | otherwise=let (m,t)=visit (memo,table) k in (m M.! k:ids,m,t)
      where n=nodes M.! k

-- The reverse index contains exactly those roots whose local shape can change
-- when a named operand changes correspondence. Expansion stops at every named
-- operand; anonymous DAG sharing is memoized independently of the assignment.
referenceUsers :: Nodes -> [EcoKey] -> M.Map EcoKey (S.Set EcoKey)
referenceUsers nodes roots = M.fromListWith S.union
    [(ref,S.singleton root)|(root,refs)<-M.toList rootRefs,ref<-S.toList refs]
  where
    (rootRefs,_) = foldl' visitRoot (M.empty,M.empty) roots
    visitRoot (found,memo) root =
        let (refs,memo')=expand memo root in (M.insert root refs found,memo')
    expand memo key = case M.lookup key memo of
        Just refs -> (refs,memo)
        Nothing -> let (refs,memo')=foldl' child (S.empty,memo) (enOperands (nodes M.! key))
                   in (refs,M.insert key refs memo')
    child (refs,memo) key
        | isJust (enName (nodes M.! key)) = (S.insert key refs,memo)
        | otherwise = let (more,memo')=expand memo key in (S.union refs more,memo')

-- Direct paired comparison is equivalent to localShapes, but permits scoring
-- just the affected roots of a proposed chain/cycle. The roots expand even when
-- named; only operands use the active name correspondence. Memoization bounds
-- traversal by distinct anonymous node pairs, including shared fanout.
localEqual :: Nodes -> Nodes -> Assignment -> EcoKey -> EcoKey -> Bool
localEqual os ns refs oldKey newKey = fst (expand M.empty oldKey newKey)
  where
    expand memo a b = case M.lookup (a,b) memo of
        Just equal -> (equal,memo)
        Nothing ->
            let o=os M.! a; n=ns M.! b
                compatible=not (enOpaque o || enOpaque n) &&
                    (enType o,enOp o,enBoundary o,length (enOperands o)) ==
                    (enType n,enOp n,enBoundary n,length (enOperands n))
                (equal,memo')=if compatible
                    then children memo (zip (enOperands o) (enOperands n))
                    else (False,memo)
            in (equal,M.insert (a,b) equal memo')
    children memo []=(True,memo)
    children memo ((a,b):rest)=
        let o=os M.! a; n=ns M.! b
            (equal,memo')=case (enName o,enName n) of
                (Just _,Just _) -> (enType o==enType n && M.lookup b refs==Just a,memo)
                (Nothing,Nothing) -> expand memo a b
                _ -> (False,memo)
        in if equal then children memo' rest else (False,memo')

zeroScore :: AssignmentScore
zeroScore=AssignmentScore 0 0 0 0
addScore :: AssignmentScore -> AssignmentScore -> AssignmentScore
addScore (AssignmentScore a b c d) (AssignmentScore e f g h)=AssignmentScore (a+e) (b+f) (c+g) (d+h)
subtractScore :: AssignmentScore -> AssignmentScore -> AssignmentScore
subtractScore (AssignmentScore a b c d) (AssignmentScore e f g h)=AssignmentScore (a-e) (b-f) (c-g) (d-h)

-- Complete a partial injection. Same-name ties are preferred, followed by the
-- previous assignment, then sorted keys. Each type is a complete bipartite
-- candidate graph, so this reaches its exact maximum cardinality.
complete :: Nodes -> Nodes -> [EcoKey] -> [EcoKey] -> Assignment -> Assignment -> Assignment
complete os ns oldKeys newKeys previous fixed =
    let freeOld=S.fromList oldKeys S.\\ S.fromList (M.elems fixed)
        freeNew=S.fromList newKeys S.\\ M.keysSet fixed
        sameNames=M.fromList [(fromMaybe "" (enName (os M.! a)),a)|a<-S.toList freeOld]
        identities=[(b,a)|b<-S.toList freeNew,Just a<-[M.lookup (fromMaybe "" (enName (ns M.! b))) sameNames],
                           enType (os M.! a)==enType (ns M.! b)]
        choose (assignment,occupied) (b,a)
            | M.member b assignment || S.member a occupied = (assignment,occupied)
            | otherwise=(M.insert b a assignment,S.insert a occupied)
        (first,used)=foldl' choose (fixed,S.fromList (M.elems fixed)) identities
        (second,used')=foldl' choose (first,used)
            [(b,a)|(b,a)<-M.toList previous,S.member b freeNew,S.member a freeOld]
        oldGroups=M.fromListWith (++) [(enType (os M.! a),[a])|a<-oldKeys,S.notMember a used']
        newGroups=M.fromListWith (++) [(enType (ns M.! b),[b])|b<-newKeys,M.notMember b second]
        rest=[(b,a)|(as,bs)<-M.elems (M.intersectionWith (,) oldGroups newGroups),
                    (a,b)<-zip (sort as) (sort bs)]
    in M.union second (M.fromList rest)

-- Pick maximum-size evidence buckets without expanding their Cartesian
-- products. Equal-evidence choices preserve the active assignment where
-- possible, then prefer identical spellings and sorted keys.
requests :: Ord s => Nodes -> Nodes -> [EcoKey] -> [EcoKey] ->
            M.Map EcoKey s -> M.Map EcoKey s -> Assignment -> Assignment
requests os ns olds news oldSig newSig current = M.unions
    [bucket as bs | (as,bs)<-M.elems (M.intersectionWith (,) (groups olds oldSig) (groups news newSig))]
  where
    groups ks sig=M.fromListWith (++) [(sig M.! k,[k])|k<-ks]
    bucket as bs=let aset=S.fromList as; bset=S.fromList bs
                     retained=M.filterWithKey (\b a->S.member b bset && S.member a aset) current
                 in complete os ns (sort as) (sort bs) M.empty retained

-- Backward evidence complements local RHS requests: an already assigned
-- consumer can identify the role of an operand even when that operand's source
-- changed. Follow equal typed/operator/ordered anonymous structure and stop at
-- named operands. Each node pair is visited once per proposal family, so shared
-- DAGs do not expand into expression trees. Votes rank requests only; the same
-- independently checked active objective must improve before any move is kept.
consumerRequests :: Nodes -> Nodes -> S.Set EcoKey -> S.Set EcoKey ->
                    Assignment -> Assignment -> Assignment
consumerRequests os ns oldSet newSet roots current = fst $ foldl' choose (M.empty,S.empty) ranked
  where
    (_,votes)=foldl' (\state (b,a)->visit state a b) (S.empty,M.empty) (M.toAscList roots)
    visit state@(seen,counts) a b
        | S.member (a,b) seen=state
        | otherwise=
            let o=os M.! a; n=ns M.! b
                seen'=S.insert (a,b) seen
                compatible=not (enOpaque o || enOpaque n) &&
                    (enType o,enOp o,enBoundary o,length (enOperands o)) ==
                    (enType n,enOp n,enBoundary n,length (enOperands n))
            in if compatible then foldl' child (seen',counts) (zip (enOperands o) (enOperands n))
               else (seen',counts)
    child state@(seen,counts) (a,b)=
        let o=os M.! a; n=ns M.! b in case (enName o,enName n) of
          (Just _,Just _) | S.member a oldSet && S.member b newSet && enType o==enType n ->
              (seen,M.insertWith (+) (b,a) (1::Int) counts)
          (Nothing,Nothing) -> visit state a b
          _ -> state
    ranked=sortOn (\((b,a),count)->
        (negate count,M.lookup b current/=Just a,enName (os M.! a)/=enName (ns M.! b),b,a)) (M.toList votes)
    choose state@(chosen,used) ((b,a),_)
        | M.member b chosen || S.member a used=state
        | otherwise=(M.insert b a chosen,S.insert a used)

-- A simultaneous chain/cycle move retains requests, then fills the displaced
-- endpoints within each connected component. This avoids unrelated fallback
-- names leaking into a short reassignment chain.
components :: Assignment -> Assignment -> [([EcoKey],[EcoKey],Assignment)]
components current wanted = go S.empty (M.keys wanted)
  where
    edges=[((False,a),(True,b))|(b,a)<-M.toList current++M.toList wanted]
    adjacency=M.fromListWith S.union $ concat [[(a,S.singleton b),(b,S.singleton a)]|(a,b)<-edges]
    flood seen []=seen
    flood seen (x:xs) | S.member x seen=flood seen xs
                     | otherwise=flood (S.insert x seen) (S.toList (M.findWithDefault S.empty x adjacency)++xs)
    go _ []=[]
    go seen (b:bs)
        | S.member (True,b) seen=go seen bs
        | otherwise=let found=flood S.empty [(True,b)]
                        olds=[a|(False,a)<-S.toList found]
                        news=[n|(True,n)<-S.toList found]
                        requested=M.filterWithKey (\n _->S.member (True,n) found) wanted
                    in (olds,news,requested):go (S.union seen found) bs

applyComponents :: Nodes -> Nodes -> Assignment -> [([EcoKey],[EcoKey],Assignment)] -> Assignment
applyComponents os ns = foldl' apply
  where
    apply current (as,bs,wanted)=
        let bset=S.fromList bs
            previous=M.filterWithKey (\b _->S.member b bset) current
            inside=complete os ns as bs previous wanted
        in M.union inside (M.filterWithKey (\b _->S.notMember b bset) current)

aggressiveAssign :: M.Map EcoKey Int -> M.Map EcoKey Int -> EcoGraph -> EcoGraph ->
                    EcoMatchResult -> (EcoMatchResult,AssignmentAudit)
aggressiveAssign ofp nfp old new prior = (result,audit)
  where
    os=M.fromList [(enKey n,n)|n<-egNodes old]
    ns=M.fromList [(enKey n,n)|n<-egNodes new]
    oldEligible=[k|(k,n)<-M.toList os,enEligible n]
    news=[k|(k,n)<-M.toList ns,enEligible n]
    newNames=S.fromList [fromMaybe "" (enName (ns M.! k))|k<-news]
    protected=S.fromList (egReserved new) S.\\ newNames
    olds=[k|k<-oldEligible,maybe False (`S.notMember` protected) (enName (os M.! k))]
    oldSet=S.fromList olds; newSet=S.fromList news
    previous=M.fromList [(ecNew c,ecOld c)|c<-emCorrespondences prior,
             S.member (ecOld c) oldSet,S.member (ecNew c) newSet,
             enType (os M.! ecOld c)==enType (ns M.! ecNew c)]
    initial=complete os ns olds news previous previous
    externalRefs=M.fromList [(ecNew c,ecOld c)|c<-emCorrespondences prior,
                 S.notMember (ecNew c) newSet,isJust (enName (ns M.! ecNew c)),
                 -- Protected names are not rewritten by emission. A prior
                 -- structural correspondence with a different spelling cannot
                 -- stand in for a name substitution that will never occur.
                 enName (ns M.! ecNew c)==enName (os M.! ecOld c)]
    (oldLocal,localTable)=localShapes "old:" os olds M.empty M.empty
    local assignment=fst $ localShapes "new:" ns news (M.union assignment externalRefs) localTable
    score assignment=
        let nl=local assignment
            exact=length [()|(b,a)<-M.toList assignment,ofp M.! a==nfp M.! b]
            rhs=length [()|(b,a)<-M.toList assignment,oldLocal M.! a==nl M.! b]
            identity=length [()|(b,a)<-M.toList assignment,enName (os M.! a)==enName (ns M.! b)]
        in AssignmentScore (10*exact+5*rhs+identity) rhs exact identity
    users=referenceUsers ns news
    contributions assignment keys = M.fromSet (contribution assignment (M.union assignment externalRefs)) keys
    contribution assignment refs b = case M.lookup b assignment of
        Nothing -> zeroScore
        Just a ->
            let exact=fromEnum (ofp M.! a==nfp M.! b)
                rhs=fromEnum (localEqual os ns refs a b)
                identity=fromEnum (enName (os M.! a)==enName (ns M.! b))
            in AssignmentScore (10*exact+5*rhs+identity) rhs exact identity
    initialParts=contributions initial newSet
    initialScore=foldl' addScore zeroScore (M.elems initialParts)
    -- This independent full hash-consed score check runs after every accepted
    -- proposal, including initialization. Incremental accounting must never
    -- silently change the objective used to choose a move.
    checked assignment activeScore value
        | score assignment==activeScore=value
        | otherwise=error "ECO aggressive incremental score disagrees with full score"
    -- Quality search is deliberately finite and reported. Cardinality is
    -- complete independently of this heuristic objective and its stop point.
    improve roundNo assignment activeParts activeScore
        | roundNo>=32=(assignment,roundNo,"quality-round-limit")
        | otherwise=
            let nl=local assignment
                localWanted=requests os ns olds news oldLocal nl assignment
                contentWanted=requests os ns olds news ofp nfp assignment
                consumerWanted=consumerRequests os ns oldSet newSet (M.union assignment externalRefs) assignment
                changed=M.filterWithKey (\b a->M.lookup b assignment/=Just a)
                proposals=map (components assignment . changed) [localWanted,contentWanted,consumerWanted]
                evaluate componentSet=
                    let candidate=applyComponents os ns assignment componentSet
                        touched=S.fromList (concat [bs|(_,bs,_)<-componentSet])
                        reassigned=S.filter (\b->M.lookup b assignment/=M.lookup b candidate) touched
                        affected=S.unions (reassigned:[M.findWithDefault S.empty b users|b<-S.toList reassigned])
                        replacements=contributions candidate affected
                        before=foldl' addScore zeroScore [activeParts M.! b|b<-S.toList affected]
                        after=foldl' addScore zeroScore (M.elems replacements)
                        candidateScore=addScore (subtractScore activeScore before) after
                    in (candidateScore,candidate,M.union replacements activeParts)
                improving=[proposal|cs'<-proposals,not (null cs'),let proposal@(s,_,_)=evaluate cs',s>activeScore]
                best=case improving of
                    [] -> firstImproving (concat proposals)
                    xs -> Just $ head (sortOn (\(s,a,_)->
                           (negate (scoreWeighted s),negate (scoreLocal s),
                            negate (scoreContent s),negate (scoreIdentity s),M.toAscList a)) xs)
                firstImproving []=Nothing
                firstImproving (component:rest)=
                    let candidate@(candidateScore,_,_)=evaluate [component]
                    in if candidateScore>activeScore then Just candidate else firstImproving rest
            in case best of
                 Nothing -> (assignment,roundNo,"no-improving-generated-assignment")
                 Just (s,a,parts) -> checked a s (improve (roundNo+1) a parts s)
    (final,rounds,stop)=checked initial initialScore (improve 0 initial initialParts initialScore)
    finalLocal=local final
    evidence b a
        | ofp M.! a==nfp M.! b = "typed-content"
        | oldLocal M.! a==finalLocal M.! b = "typed-local-rhs-active-operands"
        | enName (os M.! a)==enName (ns M.! b) = "same-name-typed-fallback"
        | M.lookup b previous==Just a = "prior-role-typed-fallback"
        | otherwise = "deterministic-typed-fallback"
    retained=[c|c<-emCorrespondences prior,not (enEligible (os M.! ecOld c)),
                 not (enEligible (ns M.! ecNew c))]
    added=[EcoCorrespondence a b (if ofp M.! a==nfp M.! b then ExactContent else ChangedRole)
               ("aggressive:"++evidence b a)|(b,a)<-M.toList final]
    cs=sortOn ecNew (retained++added)
    matchedOld=S.fromList (map ecOld cs); matchedNew=S.fromList (map ecNew cs)
    result=EcoMatchResult cs (filter (`S.notMember` matchedOld) (M.keys os))
              (filter (`S.notMember` matchedNew) (M.keys ns)) (emAmbiguousBuckets prior)
    reverseFinal=M.fromList [(a,b)|(b,a)<-M.toList final]
    types=S.toList (S.fromList (map (enType . (os M.!)) olds++map (enType . (ns M.!)) news))
    groups=[let as=filter ((==ty).enType.(os M.!)) olds
                bs=filter ((==ty).enType.(ns M.!)) news
            in TypeGroup ty as bs (length [b|b<-bs,M.member b final])
                 (filter (`M.notMember` reverseFinal) as) (filter (`M.notMember` final) bs)|ty<-types]
    oldDisposition a=let n=os M.! a; counterpart=M.lookup a reverseFinal
                         unavailable=S.notMember a oldSet
                     in Disposition a (fromMaybe "" (enName n)) (enType n)
                        (if unavailable then "protected-target" else maybe "type-surplus" (const "matched") counterpart)
                        counterpart (maybe (if unavailable then "old name is protected in new namespace" else "complete typed graph cardinality surplus; identity chosen deterministically")
                                           (\b->evidence b a) counterpart)
    newDisposition b=let n=ns M.! b; counterpart=M.lookup b final
                     in Disposition b (fromMaybe "" (enName n)) (enType n)
                        (maybe "type-surplus" (const "matched") counterpart) counterpart
                        (maybe "complete typed graph cardinality surplus; identity chosen deterministically" (evidence b) counterpart)
    audit=AssignmentAudit groups (map oldDisposition oldEligible) (map newDisposition news)
              (score initial) (score final) rounds stop

-- This is the actual allocation, after the independent namespace allocator.
-- The candidate graph is compactly and completely described by type groups.
auditJSON :: [(String,String)] -> AssignmentAudit -> String
auditJSON renames audit=obj
    [("schema","1"),("policy",q "aggressive"),
     ("candidate_scope",q "complete same-type eligible named pairs excluding protected old target names; surplus identity is a deterministic choice, not per-node impossibility"),
     ("unresolved_eligible","0"),("type_groups",arr (map group (auditGroups audit))),
     ("old",arr (map (node False) (auditOld audit))),("new",arr (map (node True) (auditNew audit))),
     ("optimization",obj [("initial",scoreJSON (auditInitial audit)),("final",scoreJSON (auditFinal audit)),
       ("iterations",show (auditIterations audit)),("stop_reason",q (auditStop audit)),
       ("global_weighted_optimum","false"),("weights",obj [("typed_content","10"),("typed_local_rhs","5"),("identity","1")]),
       ("tie_break",q "retain active evidence assignment, then identical spelling, then sorted node keys; deterministic equal-score assignments are not claimed unique"),
       ("quality_scope",q "strictly improving local-content, global-content and backward-consumer chain/cycle assignments; active operand identities recomputed after each proposal; 32-round quality limit does not limit typed cardinality")])]
  where
    mapping=M.fromList renames
    node isNew d=let output=if isNew then Just (M.findWithDefault (dispositionName d) (dispositionName d) mapping) else Nothing
                     allocation=case output of
                       Nothing->"not-applicable"
                       Just s | s==dispositionName d->"kept"
                              | dispositionStatus d=="matched"->"recovered"
                              | otherwise->"freshened"
                 in obj [("key",q (dispositionKey d)),("name",q (dispositionName d)),("type",q (dispositionType d)),
                         ("status",q (dispositionStatus d)),("counterpart",maybe "null" q (dispositionCounterpart d)),
                         ("evidence",q (dispositionEvidence d)),("output_name",maybe "null" q output),("allocation",q allocation)]
    group g=obj [("type",q (groupType g)),("old",arr (map q (groupOld g))),("new",arr (map q (groupNew g))),
       ("maximum_cardinality",show (min (length (groupOld g)) (length (groupNew g)))),("matched",show (groupMatched g)),
       ("unmatched_old",arr (map q (groupUnmatchedOld g))),("unmatched_new",arr (map q (groupUnmatchedNew g)))]
    scoreJSON s=obj [("weighted",show (scoreWeighted s)),("local_rhs",show (scoreLocal s)),
                     ("content",show (scoreContent s)),("identity",show (scoreIdentity s))]

q :: String -> String
q s='"':concatMap escape s++"\""
  where escape '"'="\\\""
        escape '\\'="\\\\"
        escape c | ord c<32=let h=showHex (ord c) "" in "\\u"++replicate (4-length h) '0'++h
                 | otherwise=[c]
obj :: [(String,String)] -> String
obj xs="{"++intercalate "," [q k++":"++v|(k,v)<-xs]++"}"
arr :: [String] -> String
arr xs="["++intercalate "," xs++"]"
