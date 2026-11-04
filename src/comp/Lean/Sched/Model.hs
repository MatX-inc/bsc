-- | The module's model (model.py): kinds and slots, steps as site lists, opaque operators over read
-- fan-in, readiness facts, user assertions, task statements and recorded exclusions.
module Lean.Sched.Model
    ( DView(..), DMeth(..), DStep(..), mkDView, readsOfE
    , MX(..), mxRepr, MPort, mportRepr, MSite(..), MStep(..), MFact(..), MCheck(..), MCf(..), CfC(..), InvRef(..), Model(..), PyErr(..)
    , buildModel, buildModelWith, modelPort, ent, users, exclSrc, portMeth, pyStrErr, showModel, bitLength, muxPairs, stepFanIn
    ) where

import ASyntax (AExpr)
import Control.Monad (forM, forM_, when, unless, foldM)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.State.Strict (gets, modify', MonadIO(liftIO), MonadTrans(lift), StateT(runStateT))
import Data.Array (listArray, (!))
import Data.Char (isAlphaNum, isDigit)
import Data.List (elemIndex, intercalate, isPrefixOf, sort, sortOn)
import Data.Maybe (fromMaybe, isJust, listToMaybe)
import PPrint (ppString)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, (!), elems, empty, findWithDefault, keys, keysSet, lookup, member,
    toList, union, insert, fromList, fromListWith, insertWith, map, singleton)
import qualified Data.Map.Lazy as ML (Map, findWithDefault, map)
import qualified Data.Set as S (Set, member, empty, fromList, insert, intersection, isSubsetOf, null,
    singleton, size, toAscList, toList, union, unions)
import qualified Data.Sequence as Sq (Seq, adjust', empty, fromList, index, length, null, (|>))

import Lean.Sched.Export (Export(exModule), SynthKid(..), exMethAR, isSynthKind, synthKind, viewKind)
import Lean.Sched.Expr (aexprE)
import Lean.Sched.Kinds (cfgTypes, genericKind, kindOf, kindOrder, makeKinds, noValueTasks, pmRepr, tasksKind,
    KindDef(kdHasArg, kdPorts, kdPrec, kdShare), PM(..), Sched)
import Lean.Sched.MECheck (mePairs, meParse, samePairs)
import Lean.Sched.Py (endsWith, findSub, isPySpace, omElems, omEmpty, omFromList, omInsert, omLookup, omMember,
    omSetDefault, omSize, omToList, pyReprStr, startsWith, strip, OMap)
import Lean.Sched.Sites (Sites(..), SiteRec(..), RecKind(..), RdySV(..))
import Lean.Sched.Tree (eKids, reprE, APkg(apRules, apMethArgs, apDefs, apInsts, apIfcVals, apGuarded),
    Act(ATask, ACallA), E(..))
import Lean.Sched.Types (portLabel, Call(clArgs, clPort, clCond), Child, Child(chOrder, chSched, chStmts, chModule,
    chMethods), Design(dSteps, dChildOrder, dDisjoint, dBlocks, dUserME, dExportedCF, dExportedC, dExportedSB,
    dChildren), Meth(mtCopies, mtName, mtKind), MethKind(ActionValueMethod, ValueMethod, ActionMethod),
    Port(ptCopy, ptArgs), Step(stChecks, stName, stDescr, stReads, stCalls))

-- ---------------------------------------------------------------- the design as design.Design reads it

data DMeth = DMeth { dmKind :: String, dmHasArg :: Bool, dmCopies :: [String] }

-- | A step: name, kind word (rule, method, value), reads (inst, meth),
-- calls (inst, meth, condition text, argument text) and check lines.
data DStep = DStep { dsName :: String, dsKind :: String, dsReads :: [(String, String)]
                   , dsCalls :: [(String, String, String, String)], dsCallConds :: [AExpr], dsChecks :: [(String, AExpr)] }

data DView = DView
    { dvKids :: M.Map String (String, String, Sched)   -- ^ Verilog module, schedule text, statements
    , dvKidOrder :: [String]
    , dvPorts :: M.Map String (OMap String DMeth)
    , dvSteps :: [DStep]
    , dvDisj, dvUrg, dvMe, dvCF, dvC :: [(String, String)]
    , dvSBSBR :: [(String, String, String)]
    , dvA :: APkg
    , dvSynth, dvBvi :: S.Set String
    , dvReads :: ML.Map String (S.Set (String, String))
    , dvChildren :: M.Map String Child         -- ^ the children as the parent's .ba shows them
    , dvExport :: Maybe Export                 -- ^ written as a synthesized child: its exported interface
    , dvSynthK :: M.Map String SynthKid        -- ^ its synthesized children with their exports (Lean.Sched.Hier)
    }

-- | The design as the prototype read it from the dump and the package; @ifc@ is each instance's
-- interface type name, @pkgs@ bsc's library packages.
mkDView :: Design -> APkg -> [(String, String)] -> S.Set String -> DView
mkDView d ap ifc pkgs = dv
  where
    dv = DView kids [ k | k <- dChildOrder d, M.member k kids ] ports steps (dDisjoint d) (S.toList (dBlocks d)) (dUserME d)
               (dExportedCF d) (dExportedC d) (dExportedSB d) ap synth bvi rd (dChildren d) Nothing M.empty
    kidOf c = let s = unwords (words (chSched c))
              in case s of
                   ('[' : r) | not (null r), last r == ']' -> Just (chModule c, init r, chStmts c)
                   _ -> Nothing
    kids = M.fromList [ (k, x) | (k, c) <- M.toList (dChildren d), Just x <- [kidOf c], noSpace k, noSpace (chModule c) ]
    noSpace = all (not . isPySpace)
    ports = M.fromList [ (k, omFromList [ (mtName mt, meth mt) | mt <- map (chMethods c M.!) (chOrder c) ])
                       | (k, c) <- M.toList (dChildren d), M.member k kids ]
    meth mt = DMeth (case mtKind mt of { ValueMethod -> "value"; ActionMethod -> "action"; ActionValueMethod -> "actionvalue" })
                    (any (not . null . ptArgs) (mtCopies mt)) [ maybe "-" show (ptCopy p) | p <- mtCopies mt ]
    steps = [ DStep (stName st) (takeWhile (\ch -> isAlphaNum ch || ch == '_') (stDescr st))
                    [ splitLabel (portLabel p) | (p, _) <- stReads st ]
                    [ callOf c | c <- stCalls st ] (map clCond (stCalls st)) [ (ppString c, c) | c <- stChecks st ]
            | st <- dSteps d ]
    splitLabel l = let (a, b) = break (== '.') l in (a, drop 1 b)
    -- a call's condition and arguments as the dump prints them ("calls p when c with args")
    callOf c = let (i, m) = splitLabel (portLabel (clPort c))
               in (i, m, ppString (clCond c), strip (intercalate ", " (map ppString (clArgs c))))
    synth = S.fromList [ i | (i, t) <- ifc, endsWith t "\x00ad" ]
    bvi = S.fromList [ i | (i, t) <- ifc, not (endsWith t "\x00ad"), not (S.member (takeWhile (/= '.') t) pkgs) ]
    defs = M.fromList (omToList (apDefs ap))
    rd = ML.map readsE defs
    readsE e = readsOfWith rd e

readsOfWith :: ML.Map String (S.Set (String, String)) -> E -> S.Set (String, String)
readsOfWith rd = go
  where go e = case e of
            EId n -> ML.findWithDefault S.empty n rd
            ELit _ -> S.empty
            ECall i m as -> S.insert (i, m) (S.unions (map go as))
            _ -> S.unions (map go (eKids e))

-- | design.reads_of: the child ports an expression reads, through definitions.
readsOfE :: DView -> E -> S.Set (String, String)
readsOfE dv = readsOfWith (dvReads dv)

faninName :: DView -> String -> S.Set (String, String)
faninName dv n = if omMember n (apDefs (dvA dv)) then readsOfE dv (EId n) else S.empty

-- | 'sub[2]' -> ('sub', Just 2).
portMeth :: String -> (String, Maybe Int)
portMeth m = case break (== '[') m of
    (a@(_ : _), '[' : r) | all (\c -> isAlphaNum c || c == '_') a, (ds@(_ : _), "]") <- span isDigit r -> (a, Just (read ds))
    _ -> (m, Nothing)

-- ---------------------------------------------------------------- the model

-- | The prototype's term tuples: z, k, x, f, g, gk, split, true, false, lit, check, chk, tup
-- (each constructor below one tuple tag); cf: a conflict_free check's line, a task statement's hardware.
data MX = MZ | MK Int | MXarg | MF Int [Int] | MG Int [Int] | MGK Int | MSplit [Int] MX MX
        | MTrue | MFalse | MLit Int | MChkI Int | MChk | MTup | MCfT Int
    deriving (Eq, Ord)

mxRepr :: MX -> String
mxRepr x = case x of
    MZ -> "('z',)"
    MK n -> "('k', " ++ show n ++ ")"
    MXarg -> "('x',)"
    MF n is -> "('f', " ++ show n ++ ", " ++ lst is ++ ")"
    MG n is -> "('g', " ++ show n ++ ", " ++ lst is ++ ")"
    MGK n -> "('gk', " ++ show n ++ ")"
    MSplit is e a -> "('split', " ++ lst is ++ ", " ++ mxRepr e ++ ", " ++ mxRepr a ++ ")"
    MTrue -> "('true',)"
    MFalse -> "('false',)"
    MLit i -> "('lit', " ++ show i ++ ")"
    MChkI c -> "('check', " ++ show c ++ ")"
    MChk -> "('chk',)"
    MTup -> "('tup',)"
    MCfT c -> "('cf', " ++ show c ++ ")"
  where lst is = "[" ++ intercalate ", " (map show is) ++ "]"

type MPort = (String, Int, PM)

mportRepr :: MPort -> String
mportRepr (k, i, m) = "(" ++ pyReprStr k ++ ", " ++ show i ++ ", " ++ pmRepr m ++ ")"

data InvRef = InvNone | InvSelf | InvFact Int
    deriving (Eq)

data MSite = MSite
    { sKind :: String, sPort :: MPort, sLabel :: Maybe String, sArg :: MX, sCond :: MX
    , sPortName :: Maybe (String, String), sR :: Maybe MX, sCKind :: Maybe String, sRdyE :: Maybe SiteRec
    , sYsv :: Maybe [Int], sFact :: Maybe Int, sConfirmed :: Maybe Bool, sInv :: Maybe InvRef
    , sHw :: Maybe MX, sArgs :: Maybe [MX] }

site0 :: String -> MPort -> Maybe String -> MX -> MX -> MSite
site0 k p l a c = MSite k p l a c Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing

data MStep = MStep
    { tName :: String, tMeth :: Maybe String, tSites :: Sq.Seq MSite, tReadPos :: OMap (String, String) Int
    , tChecks :: [(String, AExpr)], tGuard :: Maybe MX, tTasks :: [Int], tRes :: MX, tFallback :: Bool }

data MFact = MFact { fKind :: String, fSite :: Int, fLit :: Maybe Int, fOk :: Bool, fStep :: String, fLabel :: String
                   , fY :: [Int], fLate :: Int }

data MCheck = MCheck { ckRule :: Int, ckHead :: Int, ckLst :: [Int], ckLine :: String, ckFull :: Bool
                     , ckHeads :: [Int], ckAll :: Bool }   -- ^ the line's first side; every rule of the line a step

-- | A conflict_free check as a file states it (design.md §5, trust point 3): rule R's line, a conjunction
-- of WILL_FIRE signals and conditions over one step's reads (an instrumentation wire as its writer's argument).
data MCf = MCf { cfRule :: Int, cfLine :: String, cfE :: E, cfConj :: [CfC] }
data CfC = CfWF Int | CfX Int MX

data Model = Model
    { mDV :: DView
    , mRules :: [MStep]                 -- ^ every step: rules (and value methods), then methods
    , mNR, mNM, mNS, mNT :: Int
    , mRI, mMI :: M.Map String Int
    , mK :: M.Map String KindDef
    , mInst :: OMap String (String, Int)
    , mByk :: M.Map String [String]
    , mGen :: OMap (String, String) String
    , mGcls :: M.Map String String
    , mKinds :: [String]
    , mOff :: M.Map String Int
    , mNSlots, mD :: Int
    , mLits, mOpsF, mOpsG :: OMap String Int
    , mFacts :: [MFact]
    , mLaterDrops, mFanOutside :: Int
    , mChecks :: [MCheck]
    , mBlk :: M.Map Int [Int]
    , mDisj :: [(Int, Int)]
    , mDFind :: M.Map (Int, Int) (Int, Bool)
    , mMEFind :: M.Map (Int, Int) (Int, Int, Bool)
    , mCfChecks :: [(String, (String, AExpr))]
    , mCfS :: [MCf]                     -- ^ the conflict_free checks a file states, in mCfChecks' order
    , mUnguarded :: [String]
    , mMTasks :: [(String, Int)]
    , mTStart :: [Int]
    , mStepFallback :: [String]
    , mLenient :: Bool                  -- ^ the gate's run used a leniency the emitter's run stops on
    }

-- | A Python exception: its type name and message.
data PyErr = PyErr String String

pyStrErr :: PyErr -> String
pyStrErr (PyErr t m) = t ++ ": " ++ m

data MS = MS { msLits, msOpsF, msOpsG :: !(OMap String Int), msFacts :: !(Sq.Seq MFact), msLater :: !Int
             , msFanOut :: !Int, msFallback :: !(Sq.Seq String)
             , msFanMemo :: !(M.Map (String, String, Bool) MX) }   -- a step's term for a definition

type MM = ExceptT PyErr (StateT MS IO)

throwPy :: String -> String -> MM a
throwPy t m = throwError (PyErr t m)

numberIn :: (MS -> OMap String Int) -> (OMap String Int -> MS -> MS) -> String -> MM Int
numberIn get' set' n = do
    m <- lift (gets get')
    case omLookup n m of
        Just i -> return i
        Nothing -> do
            let i = omSize m
            lift (modify' (set' (omInsert n i m)))
            return i

litM, fidM, gidM :: String -> MM Int
litM = numberIn msLits (\m s -> s { msLits = m })
fidM = numberIn msOpsF (\m s -> s { msOpsF = m })
gidM = numberIn msOpsG (\m s -> s { msOpsG = m })

catchError' :: MM a -> (PyErr -> MM a) -> MM a
catchError' = catchError

-- | Static context of model building.
data Ctx = Ctx { cxDV :: DView, cxInst :: OMap String (String, Int), cxK :: M.Map String KindDef, cxCS :: M.Map String (M.Map String SiteRec)
               , cxSites :: Sites, cxGate :: Bool }

modelPort :: OMap String (String, Int) -> M.Map String KindDef -> String -> String -> Either PyErr MPort
modelPort inst k child meth = case omLookup child inst of
    Nothing -> Left (PyErr "KeyError" (pyReprStr child))
    Just (kd, i)
        | M.member kd k && (startsWith kd "g" || isSynthKind kd) -> Right (kd, i, PS meth)
        | otherwise -> let (m, c) = portMeth meth
                       in Right (kd, i, PS (if kd == "rf" && m == "sub" then 's' : show (fromMaybe 0 c) else m))

portM :: Ctx -> String -> String -> MM MPort
portM cx c m = either throwError return (modelPort (cxInst cx) (cxK cx) c m)

-- | A step under construction: its read positions and sites so far.
data SB = SB { sbName :: String, sbRP :: OMap (String, String) Int, sbSites :: Sq.Seq MSite }

fanM :: SB -> S.Set (String, String) -> String -> MM [Int]
fanM sb ports what = do
    idx <- forM (S.toAscList ports) $ \p@(pc, pm) -> case omLookup p (sbRP sb) of
        Just i -> return [i]
        Nothing -> do
            let cp = [ i | ((c, m), i) <- omToList (sbRP sb), c == pc, fst (portMeth m) == pm ]
            when (null cp) $ throwPy "AssertionError" (what ++ " reads " ++ pc ++ "." ++ pm ++ ", not a read site of " ++ sbName sb)
            return cp
    return (S.toAscList (S.fromList (concat idx)))

wrapM :: SB -> MX -> Bool -> String -> MM MX
wrapM sb e boolean name = case e of
    MF _ is -> go is
    MG _ is -> go is
    _ -> return e
  where
    go is = do
        let cr = [ i | i <- is, isJust (sR (Sq.index (sbSites sb) i)) ]
        if null cr then return e else do
            let rest = [ i | i <- is, i `notElem` cr ]
            alt <- if not (null rest)
                   then if boolean then (\n -> MG n rest) <$> gidM (name ++ "#nx") else (\n -> MF n rest) <$> fidM (name ++ "#nx")
                   else if boolean then MGK <$> gidM (name ++ "#nx") else MK <$> litM (name ++ "#nx")
            return (MSplit (S.toAscList (S.fromList cr)) e alt)

exprName :: Ctx -> SB -> String -> String -> Bool -> MM MX
exprName cx sb name what boolean = do
    -- (a step's read positions and read conditions are fixed before any definition is named)
    memo <- lift (gets msFanMemo)
    case M.lookup (sbName sb, name, boolean) memo of
        Just x -> return x
        Nothing -> do
            x <- exprName' cx sb name what boolean
            lift (modify' (\st -> st { msFanMemo = M.insert (sbName sb, name, boolean) x (msFanMemo st) }))
            return x

exprName' :: Ctx -> SB -> String -> String -> Bool -> MM MX
exprName' cx sb name what boolean = do
    let fan = faninName (cxDV cx) name
    if S.null fan then (if boolean then MGK <$> gidM name else MK <$> litM name) else do
        n <- if boolean then gidM name else fidM name
        f <- fanM sb fan what
        wrapM sb (if boolean then MG n f else MF n f) boolean name

exprAst :: Ctx -> SB -> E -> String -> String -> Bool -> MM MX
exprAst cx sb x name what boolean = case x of
    ELit s -> if boolean then MGK <$> gidM name else MK <$> litM s
    _ -> do
        let fan = readsOfE (cxDV cx) x
        if S.null fan then (if boolean then MGK <$> gidM name else MK <$> litM (reprE x)) else do
            n <- if boolean then gidM name else fidM name
            f <- fanM sb fan what
            wrapM sb (if boolean then MG n f else MF n f) boolean name

isLitText :: String -> Bool
isLitText s = case span isDigit s of
    (_ : _, '\'' : b : r) -> b `elem` "dbh" && not (null r) && all (`elem` "0123456789abcdefABCDEF") r
    _ -> False

argExpr :: Ctx -> SB -> String -> Maybe String -> MM MX
argExpr cx sb a meth
    | null a = return MZ
    | Just m <- meth, a `elem` fromMaybe [] (omLookup m (apMethArgs ap)) = return MXarg
    | isLitText a = MK <$> litM a
    | omMember a (apDefs ap) = exprName cx sb a "argument" False
    | otherwise = MK <$> litM a
  where ap = dvA (cxDV cx)

-- | model._sub_calls: the RegFile sub calls (instance, address) the expressions reach.
subCalls :: Ctx -> [E] -> [(String, E)]
subCalls cx es0 = go es0 S.empty []
  where
    ap = dvA (cxDV cx)
    isRF i = case omLookup i (cxInst cx) of { Just ("rf", _) -> True; _ -> False }
    go [] _ acc = reverse acc
    go (x : st) seen acc = case x of
        EId n | not (S.member n seen), Just de <- omLookup n (apDefs ap) -> go (de : st) (S.insert n seen) acc
              | otherwise -> go st seen acc
        ECall i m as -> go (as ++ st) seen (if m == "sub" && isRF i then (i, case as of { (a : _) -> a; [] -> ELit "0" }) : acc else acc)
        _ -> go (eKids x ++ st) seen acc

ruleExprs :: APkg -> String -> [E]
ruleExprs ap name = case omLookup name (apRules ap) of
    Nothing -> []
    Just (p, acts) -> p : concat [ case a of
                                     ACallA _ _ c as -> maybe [] pure c ++ as
                                     ATask _ c as -> maybe [] pure c ++ as
                                     _ -> []
                                 | a <- acts ]

setSite :: Int -> (MSite -> MSite) -> SB -> SB
setSite i f sb = sb { sbSites = Sq.adjust' f i (sbSites sb) }

-- | model._step: one step as a site list.
stepM :: Ctx -> DStep -> Maybe String -> MM MStep
stepM cx s meth = do
    let dv = cxDV cx
        ap = dvA dv
        name = dsName s
        cs = M.findWithDefault M.empty name (cxCS cx)
        reads = dsReads s
    rpl <- forM reads $ \(c, m) -> (\p -> ((c, m), p)) <$> portM cx c m
    let rp = M.fromList rpl
        litsFirst = S.fromList [ x | e <- M.elems cs, RRdy { rRdy = x } <- [srKind e], M.member x rp ]
        subA = M.fromListWith S.union [ (i, readsOfE dv a) | (i, a) <- subCalls cx (ruleExprs ap name) ]
        dep = M.fromList
            [ ((c, m), S.intersection d (M.keysSet rp))
            | (c, m) <- reads
            , let d1 = if fst (portMeth m) == "sub" then S.fromList [ x | (c2, m2) <- maybe [] S.toList (M.lookup c subA)
                                                                        , x <- M.keys rp, fst x == c2, fst (portMeth (snd x)) == m2 ]
                       else S.empty
                  e = M.lookup (c ++ "." ++ m) cs
                  d2 = case fmap srKind e of
                      Just (RReach { rTrue = False, rFan = fan }) -> S.fromList fan
                      Just (RRdy { rRdy = x }) | M.member x rp -> S.singleton x
                      _ -> S.empty
                  d = S.unions [d1, d2] ]
        firstIdx = M.fromListWith (\_ old -> old) (zip reads [0 :: Int ..])
        pend0 = sortOn (\x -> (if S.member x litsFirst then 0 :: Int else 1, firstIdx M.! x)) reads
        place [] _ acc = Right (reverse acc)
        place pend placed acc = case break (\x -> (dep M.! x) `S.isSubsetOf` placed) pend of
            (pre, x : post) -> place (pre ++ post) (S.insert x placed) (x : acc)
            (_, []) -> Left ()
    order <- case place pend0 S.empty [] of
        Right o -> return o
        Left () -> throwPy "ValueError" ("cyclic read conditions in " ++ name)
    let sb0 = SB name (omFromList (zip order [0 ..]))
                 (Sq.fromList [ (site0 "read" (rp M.! x) (Just (c ++ "." ++ m)) MZ MTrue) { sPortName = Just x } | x@(c, m) <- order ])
    -- conditions of conditional reads
    sb1 <- foldM (\sb i -> do
        let site = Sq.index (sbSites sb) i
        case M.lookup (fromMaybe "" (sLabel site)) cs of
            Nothing -> return sb
            Just e -> case srKind e of
                RReach { rTrue = t, rFan = fan }
                    | t -> return sb
                    | otherwise -> do
                        g <- gidM ("reach_" ++ name ++ "_" ++ fromMaybe "" (sLabel site))
                        f <- fanM sb (S.fromList fan) "reach"
                        let c = MG g f
                        return (setSite i (\x -> x { sCond = c, sR = Just c, sCKind = Just "reach" }) sb)
                RRdy { rRdy = lp0, rRFan = rfan } -> do
                    let lp = [ j | Just j <- [omLookup lp0 (sbRP sb)] ]
                    when (null lp) $ throwPy "AssertionError" ("no ready literal read for " ++ fromMaybe "" (sLabel site))
                    g <- gidM ("rseq_" ++ name ++ "_" ++ fromMaybe "" (sLabel site))
                    let fan = [ x | x <- rfan, omMember x (sbRP sb) ]
                        r = MG g (sort [ fromMaybe 0 (omLookup x (sbRP sb)) | x <- fan ])
                    lift (modify' (\st -> st { msFanOut = msFanOut st + length [ x | x <- rfan, not (omMember x (sbRP sb)) ] }))
                    return (setSite i (\x -> x { sCond = MLit (orDie "no ready literal" (listToMaybe lp)), sCKind = Just "rdy", sR = Just r, sRdyE = Just e }) sb))
        sb0 [0 .. Sq.length (sbSites sb0) - 1]
    forM_ (F.toList (sbSites sb1)) $ \site -> case (sCKind site, sR site) of
        (Just "reach", Just (MG _ js)) | any (\j -> isJust (sR (Sq.index (sbSites sb1) j))) js ->
            throwPy "AssertionError" ("an (a) condition over a conditional read in " ++ name)
        _ -> return ()
    -- RegFile sub copies carry an address: the step's sub calls paired with its copies, in order
    let exprs = ruleExprs ap name ++ [ v | Just m <- [meth], Just v <- [omLookup m (apIfcVals ap)] ]
        subargs0 = foldl' (\acc (i, a) -> M.insertWith (\_ old -> if a `elem` old then old else old ++ [a]) i [a] acc) M.empty (subCalls cx exprs)
        subargs = M.map (sortOn reprE) subargs0
        rfIdx = [ i | (i, st) <- zip [0 ..] (F.toList (sbSites sb1)), let (kd, _, _) = sPort st, kd == "rf" ]
    sb2 <- foldM (\sb i -> do
        let site = Sq.index (sbSites sb) i
            (_, ii, pm) = sPort site
            m = case pm of { PS x -> x; PT _ -> error "schedcheck: a RegFile copy on a task port" }
            inst = fromMaybe "" (lookupNth ii [ k | (k, ("rf", _)) <- omToList (cxInst cx) ])
            copies = sort [ read (drop 1 m2) :: Int | st <- F.toList (sbSites sb), (kd2, i2, PS m2) <- [sPort st], kd2 == "rf", i2 == ii ]
            args = M.findWithDefault [] inst subargs
            c = read (drop 1 m) :: Int
            ci = fromMaybe 0 (elemIndex c copies)
            a | ci < length args = Just (args !! ci)
              | not (null args) = Just (last args)
              | otherwise = Nothing
        arg <- case a of
            Nothing -> MK <$> litM ("rfaddr_" ++ name ++ "_" ++ fromMaybe "" (sLabel site))
            Just x -> exprAst cx sb x (case x of { EId n -> n; _ -> "rfaddr_" ++ reprE x }) "address" False
        return (setSite i (\st -> st { sArg = arg }) sb)) sb1 rfIdx
    -- calls
    sb3 <- foldM (\sb ((c, m, cond, a), ce) -> do
        p <- portM cx c m
        cc <- if cond == "1'd1" then return MTrue else do
            cc0 <- if omMember cond (apDefs ap) then exprName cx sb cond "condition" True
                   else case aexprE (apInsts ap) ce of
                       Left err -> throwPy "ValueError" err
                       Right x -> exprAst cx sb x ("cc_" ++ name ++ "_" ++ show (Sq.length (sbSites sb))) "condition" True
            return (case cc0 of { MGK _ | cond `elem` ["1'b1", "1'd1"] -> MTrue; _ -> cc0 })
        arg <- argExpr cx sb a meth
        return sb { sbSites = sbSites sb Sq.|> site0 "call" p (Just (c ++ "." ++ m)) arg cc }) sb2 (zip (dsCalls s) (dsCallConds s))
    sb4 <- rdyTerms cx (orderCalls cx (Sq.length (sbSites sb2)) sb3)
    let alwaysRdy = case (meth, dvExport dv) of
            (Just m, Just ex) -> exMethAR ex m
            _ -> False
    guard <- if alwaysRdy then return (Just MTrue)
             else if null (dsChecks s) then do
                 g <- exprName cx sb4 ("CAN_FIRE_" ++ name) "guard" True
                 return (Just (case g of { MGK n -> MG n []; _ -> g }))
             else return Nothing
    res <- case meth of
        Just m | Just v <- omLookup m (apIfcVals ap), not (startsWith m "RDY_") -> exprAst cx sb4 v ("res_" ++ m) "result" False
        _ -> return MZ
    return (MStep name meth (sbSites sb4) (sbRP sb4) (dsChecks s) guard [] res False)

-- | A rule's calls of one child in an order its annotation allows (bsc composes a rule's actions in
-- parallel): a call moves before the first earlier one it must precede, only where that order is wrong.
orderCalls :: Ctx -> Int -> SB -> SB
orderCalls cx n0 sb
    | Sq.length ss - n0 < 2 || any refsCall (F.toList ss) = sb
    | otherwise = sb { sbSites = Sq.fromList (take n0 (F.toList ss) ++ go Sq.empty S.empty (drop n0 (F.toList ss))) }
  where
    ss = sbSites sb
    refsCall st = any (>= n0) (concatMap mxIdx (sCond st : sArg st : fromMaybe [] (sArgs st)))
    mxIdx e = case e of
        MF _ js -> js
        MG _ js -> js
        MLit j -> [j]
        MSplit js a b -> js ++ mxIdx a ++ mxIdx b
        _ -> []
    before x y = let (kx, ix, px) = sPort x
                     (ky, iy, py) = sPort y
                 in kx == ky && ix == iy && maybe False (\kd -> kdPrec kd px py && not (kdPrec kd py px)) (M.lookup kx (cxK cx))
    -- a call of an instance with no call yet goes last (no call before it can relate to it)
    instOf x = let (k, i, _) = sPort x in (k, i)
    go acc _ [] = F.toList acc
    go acc seen (c : rest)
        | not (S.member (instOf c) seen) = go (acc Sq.|> c) (S.insert (instOf c) seen) rest
        | otherwise = case break (before c) (F.toList acc) of
            (pre, post@(_ : _)) | not (any (`before` c) post) -> go (Sq.fromList (pre ++ [c] ++ post)) seen rest
            _ -> go (acc Sq.|> c) seen rest

orDie :: String -> Maybe a -> a
orDie msg = fromMaybe (error ("schedcheck: " ++ msg))

lookupNth :: Int -> [a] -> Maybe a
lookupNth i xs = case drop i xs of { (x : _) | i >= 0 -> Just x; _ -> Nothing }

-- | model._rdy_terms: readiness R terms in site-value form, later-site literals dropped.
rdyTerms :: Ctx -> SB -> MM SB
rdyTerms cx sb0 = foldM one sb0 [ i | (i, s) <- zip [0 ..] (F.toList (sbSites sb0)), sCKind s == Just "rdy" ]
  where
    n = Sq.length (sbSites sb0)
    splits e = case e of
        MSplit is a b -> is ++ splits a ++ splits b
        _ -> []
    firstUse = M.fromListWith min [ (i, k) | (k, s) <- zip [0 ..] (F.toList (sbSites sb0))
                                         , i <- splits (sCond s) ++ splits (sArg s) ++ concatMap splits (fromMaybe [] (sArgs s)) ]
    one sb i = do
        let site = Sq.index (sbSites sb) i
            e = orDie "a readiness site without its record" (sRdyE site)
            (g, fan0) = case sR site of { Just (MG g' f') -> (g', f'); _ -> error "schedcheck: a readiness site without its R" }
            lim = M.findWithDefault n i firstUse
            late = [ j | j <- fan0, j >= lim ]
            sv0 = case srKind e of { RRdy { rSV = x } -> x; _ -> error "schedcheck: a readiness record of reach kind" }
        (sv, fan) <- if null late then return (sv0, fan0) else do
            lift (modify' (\st -> st { msLater = msLater st + length late }))
            let drop' = S.fromList [ fromMaybe ("", "") (sPortName (Sq.index (sbSites sb) j)) | j <- late ]
            r <- liftIO (sxRecheck (cxSites cx) (srStep e) (srLabel e) drop' (sxRSb (cxSites cx)))
            case r of
                Just (sv', _) -> return (sv', [ j | j <- fan0, j < lim ])
                Nothing -> throwPy "KeyError" (pyReprStr (srLabel e))
        let sb1 = if null late then sb else setSite i (\x -> x { sR = Just (MG g fan) }) sb
            ys = [ j | j <- fan, isJust (sR (Sq.index (sbSites sb1) j)) ]
            yset = S.fromList [ fromMaybe ("", "") (sPortName (Sq.index (sbSites sb1) j)) | j <- ys ]
        unless (yset == S.fromList (svY sv)) $
            throwPy "AssertionError" ("site-value reads differ in " ++ sbName sb ++ " / " ++ fromMaybe "" (sLabel site))
        nf <- lift (gets (Sq.length . msFacts))
        let litI = case sCond site of { MLit x -> x; _ -> error "schedcheck: a readiness site without its literal" }
            fact = MFact "rdy" i (Just litI) (svRdy sv) (sbName sb) (fromMaybe "" (sLabel site)) ys (length late)
            others = [ j | j <- ys, j /= i ]
        lift (modify' (\st -> st { msFacts = msFacts st Sq.|> fact }))
        inv <- if not (null others) then do
                   lift (modify' (\st -> st { msFacts = msFacts st Sq.|> MFact "inv" i Nothing (svInv sv == Just True) (sbName sb)
                                                                                  (fromMaybe "" (sLabel site)) ys (length late) }))
                   return (InvFact (nf + 1))
               else return (if null ys then InvNone else InvSelf)
        return (setSite i (\x -> x { sYsv = Just ys, sFact = Just nf, sConfirmed = Just (svRdy sv), sInv = Just inv }) sb1)

-- | model._gstep: in the gate's run, a step the builder cannot build gets its footprint only.
gstepM :: Ctx -> DStep -> Maybe String -> MM MStep
gstepM cx s meth
    | not (cxGate cx) = stepM cx s meth
    | otherwise = catchError' (stepM cx s meth) $ \err@(PyErr t msg) ->
        if t `notElem` ["AssertionError", "ValueError", "KeyError"] then throwError err else do
            lift (modify' (\st -> st { msFallback = msFallback st Sq.|> (dsName s ++ ": " ++ take 80 msg) }))
            rs <- forM (dsReads s) $ \(c, m) -> do
                p <- portM cx c m
                return ((c, m), (site0 "read" p (Just (c ++ "." ++ m)) MZ MTrue) { sPortName = Just (c, m) })
            cl <- forM (dsCalls s) $ \(c, m, _, _) -> do
                p <- portM cx c m
                return (site0 "call" p (Just (c ++ "." ++ m)) MZ MTrue)
            let rp = foldl' (\om (k, (x, _)) -> omInsert x k om) omEmpty (zip [0 ..] rs)
            return (MStep (dsName s) meth (Sq.fromList (map snd rs ++ cl)) rp (dsChecks s) (Just MTrue) [] MZ True)

-- | Build the model in the gate's run (@gate@) or the emitter's.
buildModel :: Bool -> DView -> Sites -> IO (Either PyErr Model)
buildModel gate = buildModelWith gate True Nothing

-- | With task statements on or off and, for measurement, only the first rules (model.py's NSTEPS).
buildModelWith :: Bool -> Bool -> Maybe Int -> DView -> Sites -> IO (Either PyErr Model)
buildModelWith gate tasksOn nsteps dv sites = do
    let st0 = MS omEmpty omEmpty omEmpty Sq.empty 0 0 Sq.empty M.empty
    (r, st) <- runStateT (runExceptT (build gate tasksOn nsteps dv sites)) st0
    return $ case r of
        Left e -> Left e
        Right m -> Right m { mLits = msLits st, mOpsF = msOpsF st, mOpsG = msOpsG st, mFacts = F.toList (msFacts st)
                           , mLaterDrops = msLater st, mFanOutside = msFanOut st, mStepFallback = F.toList (msFallback st)
                           , mLenient = mLenient m || not (Sq.null (msFallback st)) }

meLine :: (String, AExpr) -> MM Bool
meLine (_, c) = case meParse c of
    Left err -> throwPy "ValueError" err
    Right r -> return (isJust r)

build :: Bool -> Bool -> Maybe Int -> DView -> Sites -> MM Model
build gate tasksOn nsteps dv sites = do
    let steps = dvSteps dv
        -- a synthesized child's value methods are methods of its interface (design.md §3.1)
        isMeth s = dsKind s == "method" || (isJust (dvExport dv) && dsKind s == "value")
        rulesA = [ s | s <- steps, not (isMeth s) ]
        methsA = [ s | s <- steps, isMeth s ]
        -- NSTEPS: the first n rules without checks, and the check steps naming only kept rules
        (rules0, meths) = case nsteps of
            Nothing -> (rulesA, methsA)
            Just n -> let keep = S.fromList (take n [ dsName s | s <- rulesA, null (dsChecks s) ])
                          kept s = S.member (dsName s) keep || case dsChecks s of
                              ((t, _) : _) | (x : xs) <- wfNames t -> S.member x keep && any (`S.member` keep) xs
                              _ -> False
                      in ([ s | s <- rulesA, kept s ], [])
    -- trust point 3: the check lines, read by structure, assert exactly bsc's mutually_exclusive pairs
    melines <- fmap concat $ forM [ c | s <- steps, c <- dsChecks s ] $ \c -> do
        b <- meLine c
        return [ c | b ]
    let parsed = [ x | (_, c) <- melines, Right (Just x) <- [meParse c] ]
    unless (samePairs parsed (dvMe dv)) $
        throwPy "AssertionError" ("mutually_exclusive check lines (" ++ show (S.size (pairSet (concatMap mePairs parsed)))
                                  ++ " pairs) differ from the dumper's list (" ++ show (S.size (pairSet (dvMe dv))) ++ " pairs)")
    -- user assertions: mutually_exclusive lines stay check lines, the others are conflict_free checks
    rulesCf <- forM rules0 $ \s -> if null (dsChecks s) then return (s, []) else do
        flags <- mapM meLine (dsChecks s)
        let me = [ c | (c, True) <- zip (dsChecks s) flags ]
            cf = [ (dsName s, c) | (c, False) <- zip (dsChecks s) flags ]
        return (if length me /= length (dsChecks s) then s { dsChecks = me } else s, cf)
    let rules = map fst rulesCf
        cfChecks = concatMap snd rulesCf
        nR = max 1 (length rules)
        nM = length meths
        ri = M.fromList (zip (map dsName rules) [0 ..])
        mi = M.fromList (zip (map dsName meths) [0 ..])
        kidsOf = [ (k, dvKids dv M.! k) | k <- dvKidOrder dv ]
        fifoS = case [ sl | (_, ("FIFOL1", _, sl)) <- kidsOf ] of { (x : _) -> x; [] -> [] }
        cfgS = case [ sl | (_, (t, _, sl)) <- kidsOf, t `elem` cfgTypes ] of { (x : _) -> x; [] -> [] }
        k0 = makeKinds fifoS cfgS
        used = S.fromList ([ c | s <- rules ++ meths, (c, _) <- dsReads s ] ++ [ c | s <- rules ++ meths, (c, _, _, _) <- dsCalls s ])
        -- each (type, schedule text) group by number, so ports are keyed without comparing schedule texts
        grpNo = M.fromListWith (\_ old -> old) (zip [ (typ, ss) | (_, (typ, ss, _)) <- M.toList (dvKids dv) ] [0 :: Int ..])
        kidGrp = M.map (\(typ, ss, _) -> grpNo M.! (typ, ss)) (dvKids dv)
        gp1 = foldl' (\om (key, p, v) -> omSetDefault key omEmpty (\o -> if omMember p o then o else omInsert p v o) om) omEmpty
            ([ (kidGrp M.! k, p, (m, dmKind pm, dmHasArg pm))
             | (k, (typ, ss, _)) <- kidsOf, S.member k used, (m, pm) <- maybe [] omToList (M.lookup k (dvPorts dv))
             , let cps = [ c | c <- dmCopies pm, c /= "-" ]
             , p <- if null cps then [m] else [ m ++ "[" ++ c ++ "]" | c <- cps ] ]
             ++ [ (g, m, (fst (portMeth m), "value", maybe False dmHasArg (M.lookup k (dvPorts dv) >>= omLookup (fst (portMeth m)))))
                | s <- rules ++ meths, (k, m) <- dsReads s, Just g <- [M.lookup k kidGrp] ])
    -- instances: a library model only for a bsc library primitive; the gate's generic kinds otherwise
    (inst, byk, gen, gcls, kmap, ung) <- foldM (\(inst, byk, gen, gcls, kmap, ung) (k, (typ, ss, sl)) -> do
          case M.lookup k (dvSynthK dv) of
           Just sk | S.member k (dvSynth dv) -> do
            -- a synthesized child with its export: on path A its file's interface, else an assumed leaf
            let kd = skKind sk
                kdef = maybe (synthKind kd (skExport sk)) (const (viewKind kd (dvChildren dv M.! k))) (skOff sk)
                n = length (M.findWithDefault [] kd byk)
            return (omInsert k (kd, n) inst, M.insertWith (\new old -> old ++ new) kd [k] byk, gen, gcls, M.insert kd kdef kmap, ung)
           _ -> do
            let lib = if S.member k (dvSynth dv) || S.member k (dvBvi dv)
                      then Left ("child " ++ k ++ " (" ++ typ ++ ") is " ++ (if S.member k (dvSynth dv) then "synthesized" else "a user BVI import"))
                      else kindOf typ ss
            (kd, gen', gcls', kmap') <- case lib of
                Right kd -> return (kd, gen, gcls, kmap)
                Left err | not gate -> throwPy "ValueError" err
                         | otherwise -> do
                    let (gen1, gcls1, kmap1) = case omLookup (typ, ss) gen of
                            Just _ -> (gen, gcls, kmap)
                            Nothing ->
                                let gk = 'g' : show (omSize gen)
                                    cls = if S.member k (dvSynth dv) then "synth" else if S.member k (dvBvi dv) then "bvi" else "lib"
                                    ms = [ (p, b, kn, h) | (p, (b, kn, h)) <- maybe [] omToList (omLookup (kidGrp M.! k) gp1) ]
                                in (omInsert (typ, ss) gk gen, M.insert gk cls gcls, M.insert gk (genericKind gk typ sl ms (cls == "synth") (cls == "lib")) kmap)
                        gkd = orDie "a generic kind not made" (omLookup (typ, ss) gen1)
                    unless ((gcls1 M.! gkd == "synth") == S.member k (dvSynth dv)) $
                        throwPy "AssertionError" ("a type both synthesized and not: " ++ typ)
                    return (gkd, gen1, gcls1, kmap1)
            when (kd == "fifo" && sl /= fifoS) $ throwPy "AssertionError" "FIFOL1 schedules differ"
            when (kd == "cfg" && sl /= cfgS) $ throwPy "AssertionError" "ConfigRegN schedules differ"
            ung' <- if kd `elem` ["f2", "sf"] && omLookup k (apGuarded (dvA dv)) == Just False
                    then if gate then return (ung ++ [k]) else throwPy "AssertionError" ("unguarded FIFO " ++ k)
                    else return ung
            let n = length (M.findWithDefault [] kd byk)
            return (omInsert k (kd, n) inst, M.insertWith (\new old -> old ++ new) kd [k] byk, gen', gcls', kmap', ung'))
        (omEmpty, M.empty, omEmpty, M.empty, k0, []) [ x | x@(k, _) <- kidsOf, S.member k used ]
    csm <- return (M.fromListWith (\new old -> M.union new old) [ (srStep e, M.singleton (srLabel e) e) | e <- sxRecs sites ])
    let cx = Ctx dv inst kmap csm sites gate
    -- the synthesized children the parent cannot state (methods not fired by its enables), and reads of a
    -- synthesized child's value method with arguments (not modelled yet); in the gate's run, leniencies
    let synthStops = [ "child " ++ c ++ " (" ++ exModule (skExport sk) ++ "): " ++ why
                     | (c, sk) <- M.toList (dvSynthK dv), S.member c used, Just why <- [skStop sk] ]
                     ++ [ sn ++ " reads " ++ c ++ "." ++ m ++ ", a value method with arguments of a synthesized child (its argument is not modelled yet)"
                        | s <- rules ++ meths, let sn = dsName s, (c, m) <- dsReads s, M.member c (dvSynthK dv)
                        , Just (kd, _) <- [omLookup c inst], Just kdef <- [M.lookup kd kmap]
                        , kdShare kdef (PS m) && kdHasArg kdef (PS m) ]
    unless gate $ forM_ (take 1 synthStops) (throwPy "ValueError")
    rsteps0 <- forM rules $ \s -> gstepM cx s Nothing
    -- a module without rules (a child of methods only): one rule that never fires stands in, since a
    -- file indexes rules by Fin nR with nR at least 1 (the contract's rules are existential)
    let rsteps = if null rsteps0 then [MStep "__never" Nothing Sq.empty omEmpty [] (Just MFalse) [] MZ False] else rsteps0
    msteps <- forM meths $ \s -> gstepM cx s (Just (dsName s))
    let allSteps = rsteps ++ msteps
    -- user assertions as checks; urgency
    (allSteps1, checks) <- checksM gate ri nR allSteps
    cfS <- cfStated cx ri allSteps1 cfChecks
    let blk = M.fromListWith (\new old -> old ++ new) [ (ri M.! a, [ri M.! b]) | (a, b) <- dvUrg dv, M.member a ri, M.member b ri ]
        -- the check forms only the gate accepts (checksM)
        checksLenient = or [ length (tChecks r) /= 1 | r <- allSteps, not (null (tChecks r)) ]
    -- task statements
    (allSteps2, nT, tstart, mtasks) <- if tasksOn then tasksM cx cfS allSteps1
                                       else return (allSteps1, 0, replicate (length allSteps1 + 1) 0, [])
    let kmap2 = if nT > 0 then M.insert "tasks" (tasksKind nT) kmap else kmap
        byk2 = if nT > 0 then M.insert "tasks" ["__tasks"] byk else byk
        inst2 = if nT > 0 then omInsert "__tasks" ("tasks", 0) inst else inst
        gens = sortOn (\g -> read (drop 1 g) :: Int) (omElems gen)
        synths = sortOn (\g -> read (drop 3 g) :: Int) [ k | k <- M.keys byk2, isSynthKind k ]
        kinds = [ k | k <- init kindOrder ++ gens ++ synths ++ ["tasks"], not (null (M.findWithDefault [] k byk2)) ]
        offs = scanl (\o k -> o + (if k == "tasks" then 1 else length (byk2 M.! k) * length (kdPorts (kmap2 M.! k)))) 0 kinds
        nslots = last offs
        disj = dedup [ (ri M.! a, ri M.! b) | (a, b) <- dvDisj dv, M.member a ri, M.member b ri ]
        dfind = foldl' (\m (d, (x, y)) -> M.insertWith (\_ old -> old) (y, x) (d, True) (M.insertWith (\_ old -> old) (x, y) (d, False) m))
                       M.empty (zip [0 ..] disj)
        mefind = foldl' (\m (c, p, h, y) -> M.insertWith (\_ old -> old) (y, h) (c, p, True) (M.insertWith (\_ old -> old) (h, y) (c, p, False) m))
                        M.empty [ (c, p, ckHead ch, y) | (c, ch) <- zip [0 ..] checks, (p, y) <- zip [0 ..] (ckLst ch) ]
    return Model
        { mDV = dv, mRules = allSteps2, mNR = nR, mNM = nM, mNS = length allSteps2, mNT = nT, mRI = ri, mMI = mi
        , mK = kmap2, mInst = inst2, mByk = byk2, mGen = gen, mGcls = gcls, mKinds = kinds
        , mOff = M.fromList (zip kinds offs), mNSlots = nslots, mD = max 1 (bitLength (nslots - 1))
        , mLits = omEmpty, mOpsF = omEmpty, mOpsG = omEmpty, mFacts = [], mLaterDrops = 0, mFanOutside = 0
        , mChecks = checks, mBlk = blk, mDisj = disj, mDFind = dfind, mMEFind = mefind, mCfChecks = cfChecks, mCfS = cfS
        , mUnguarded = ung, mMTasks = mtasks, mTStart = tstart, mStepFallback = []
        , mLenient = omSize gen > 0 || not (null ung) || checksLenient || not (null synthStops) }
  where
    pairSet ps = S.fromList [ S.fromList [a, b] | (a, b) <- ps ]
    dedup = go S.empty
      where go _ [] = []
            go seen (x : xs) | S.member x seen = go seen xs
                             | otherwise = x : go (S.insert x seen) xs

-- | The conflict_free checks as a file states them: each conjunct a WILL_FIRE or a condition over one step's
-- reads, a wire as its one unconditional writer's argument (trust point 3); else a gate leniency, left out.
cfStated :: Ctx -> M.Map String Int -> [MStep] -> [(String, (String, AExpr))] -> MM [MCf]
cfStated cx ri steps cfs = fmap concat $ forM (zip [0 :: Int ..] cfs) $ \(c, (rn, (line, ae))) ->
    catchError' (pure <$> one c rn line ae) $ \err@(PyErr t msg) ->
        if not (cxGate cx) || t `notElem` ["AssertionError", "ValueError"] then throwError err else do
            lift (modify' (\st -> st { msFallback = msFallback st Sq.|> (rn ++ " conflict_free check " ++ show c ++ ": " ++ take 80 msg) }))
            return []
  where
    ap = dvA (cxDV cx)
    stA = listArray (0, length steps - 1) steps
    writers = M.fromListWith (flip (++)) [ (i, [(wn, wc, as)]) | (wn, (_, acts)) <- omToList (apRules ap), ACallA i "wset" wc as <- acts ]
    true wc = case wc of { Nothing -> True; Just (ELit x) -> x `elem` ["1'd1", "1'b1"]; _ -> False }
    conjs e = case e of { EBin "&&" a b -> conjs a ++ conjs b; _ -> [e] }
    wfOf x = if "WILL_FIRE_" `isPrefixOf` x then M.lookup (drop 10 x) ri else Nothing
    -- a firing signal or an instrumentation wire below the top level, through definitions
    hidden x0 = go S.empty [x0]
      where go _ [] = False
            go seen (x : xs) = case x of
                EId n | "WILL_FIRE_" `isPrefixOf` n || "CAN_FIRE_" `isPrefixOf` n -> True
                      | S.member n seen -> go seen xs
                      | Just d <- omLookup n (apDefs ap) -> go (S.insert n seen) (d : xs)
                      | otherwise -> go seen xs
                ECall _ m [] | m `elem` ["wget", "whas"] -> True
                _ -> go seen (eKids x ++ xs)
    unstated line = throwPy "AssertionError" ("a conflict_free check in a form a file cannot state: " ++ take 80 line)
    cond k x nm = let st = stA ! k
                      sb = SB (tName st) (tReadPos st) (tSites st)
                  in case x of
                       ELit l | l `elem` ["1'd1", "1'b1"] -> return MTrue
                              | l `elem` ["1'd0", "1'b0"] -> return MFalse
                       EId n | omMember n (apDefs ap) -> exprName cx sb n "conflict_free condition" True
                       _ -> exprAst cx sb x nm "conflict_free condition" True
    one c rn line ae = do
        e <- either (throwPy "ValueError") return (aexprE (apInsts ap) ae)
        k <- maybe (unstated line) return (M.lookup rn ri)
        cs <- forM (zip [0 :: Int ..] (conjs e)) $ \(q, x) -> do
            let nm = "cf_" ++ rn ++ "_" ++ show c ++ "_" ++ show q
            case x of
                EId n | Just j <- wfOf n -> return (CfWF j)
                ECall i mt [] | mt `elem` ["wget", "whas"] -> case M.findWithDefault [] i writers of
                    [(wn, wc, a : _)] | true wc, Just j <- M.lookup wn ri ->
                        if mt == "whas" then return (CfWF j) else if hidden a then unstated line else CfX j <$> cond j a nm
                    _ -> unstated line
                _ | hidden x -> unstated line
                  | otherwise -> CfX k <$> cond k x nm
        return (MCf k line e cs)

-- | re.findall(r'WILL_FIRE_(\S+?)(?=[\s)&|]|$)', t): the rule names a check line reads.
wfNames :: String -> [String]
wfNames t = case findSub "WILL_FIRE_" t of
    Nothing -> []
    Just i -> let r = drop (i + 10) t
                  stop c = isPySpace c || c `elem` ")&|"
                  -- non-greedy \S+? up to the first lookahead match (at least one char)
                  nm = case r of
                      (c : rest) | not (isPySpace c) -> c : takeWhile (not . stop) rest
                      _ -> ""
              in if null nm then wfNames (drop (i + 10) t) else nm : wfNames (drop (i + 10 + length nm) t)

bitLength :: Int -> Int
bitLength n | n <= 0 = 0
            | otherwise = 1 + bitLength (n `div` 2)

-- | model._checks: each mutually_exclusive line as checks (head, rest) of its rule.
checksM :: Bool -> M.Map String Int -> Int -> [MStep] -> MM ([MStep], [MCheck])
checksM gate ri _ steps0 = do
    (steps, cs) <- foldM one ([], []) (zip [0 ..] steps0)
    return (reverse steps, cs)
  where
    one (acc, cs) (k, r)
        | null (tChecks r) = return (r : acc, cs)
        | otherwise = do
            unless (gate || length (tChecks r) == 1) $ throwPy "AssertionError" ("a check step with " ++ show (length (tChecks r)) ++ " check lines")
            (r', cs', _) <- foldM (\(r1, cs1, first) (line, c) -> do
                (as, bs) <- case meParse c of
                    Right (Just x) -> return x
                    Right Nothing -> throwPy "TypeError" "cannot unpack non-iterable NoneType object"
                    Left err -> throwPy "ValueError" err
                -- a line with several rules on its first side: one check per rule of it (together, bsc's line)
                let heads = [ ri M.! a | a <- as, M.member a ri ]
                    allIn = all (`M.member` ri) (as ++ bs)
                foldM (\(r2, cs2, fst2) hd -> case M.lookup hd ri of
                        Nothing -> return (if fst2 then r2 { tGuard = Just MFalse } else r2, cs2, fst2)
                        Just h -> do
                            let rest = [ x | x <- bs, M.member x ri ]
                                r3 = if fst2 then r2 { tGuard = Just (MChkI (length cs2)) } else r2
                            return (r3, cs2 ++ [MCheck k h (map (ri M.!) rest) line (length as == 1 && length rest == length bs) heads allIn], False))
                      (r1, cs1, first) as)
                (r, cs, True) (tChecks r)
            return (r' : acc, cs')

-- | model._tasks: task statements numbered in rank order, and within a rule in action order.
tasksM :: Ctx -> [MCf] -> [MStep] -> MM ([MStep], Int, [Int], [(String, Int)])
tasksM cx cfS steps0 = do
    (steps, nT, ts, mt) <- foldM one ([], 0, [], []) (zip [0 ..] steps0)
    return (reverse steps, nT, reverse (nT : ts), mt)
  where
    ap = dvA (cxDV cx)
    actsOf n = maybe [] snd (omLookup n (apRules ap))
    cfAt = M.fromListWith (flip (++)) [ (cfRule cf, [(i, cfE cf)]) | (i, cf) <- zip [0 :: Int ..] cfS ]
    one (acc, nT, ts, mt) (k, r) = case tMeth r of
        Just m -> do
            let tsk = [ a | a@(ATask {}) <- actsOf (tName r) ]
            forM_ [ nm | ATask nm _ _ <- tsk ] $ \nm -> unless (S.member (lastDot nm) noValueTasks) $
                throwPy "ValueError" ("method " ++ m ++ ": task call " ++ nm ++ " may return a value (not modelled once left out)")
            return (r : acc, nT, nT : ts, mt ++ [ (m, length tsk) | not (null tsk) ])
        Nothing -> do
            let sb0 = SB (tName r) (tReadPos r) (tSites r)
            (sb, n', tl) <- foldM (\(sb, t, tl) a -> case a of
                ATask _ c args -> do
                    let cfHit = c >>= \ce -> listToMaybe [ i | (i, e) <- M.findWithDefault [] (k :: Int) cfAt, e == usesNamed ce ]
                        attempt = do
                            -- a conflict_free check's report: its step never makes it (the file's asm), the
                            -- hardware's enable is the check's line
                            (cond, hw) <- if isJust cfHit then return (MFalse, MCfT (fromMaybe 0 cfHit))
                                          else if not (null (tChecks r)) then return (MTrue, MChk)
                                          else case c of
                                              Nothing -> return (MTrue, MTrue)
                                              Just ce -> do
                                                  x <- exprAst cx sb ce ("tcond_" ++ show t) "task condition" True
                                                  let x' = case x of { MGK g -> MG g []; _ -> x }
                                                  return (x', x')
                            as <- forM (zip [0 :: Int ..] args) $ \(q, x) -> exprAst cx sb x ("targ_" ++ show t ++ "_" ++ show q) "task argument" False
                            return (cond, hw, as)
                    (cond, hw, as) <- catchError' attempt $ \err@(PyErr ty msg) ->
                        if ty /= "AssertionError" || not (cxGate cx) then throwError err else do
                            lift (modify' (\st -> st { msFallback = msFallback st Sq.|> (tName r ++ " task " ++ show t ++ ": " ++ take 80 msg) }))
                            return (MTrue, MTrue, [])
                    let site = (site0 "task" ("tasks", 0, PT t) Nothing MTup cond) { sHw = Just hw, sArgs = Just as }
                    return (sb { sbSites = sbSites sb Sq.|> site }, t + 1, tl ++ [t])
                _ -> return (sb, t, tl)) (sb0, nT, []) (actsOf (tName r))
            return (r { tSites = sbSites sb, tTasks = tl } : acc, n', nT : ts, mt)
    lastDot s = reverse (takeWhile (/= '.') (reverse s))
    -- the package's scheduler uses (__dusesK) by the names the extracted check lines read (Expr.mkSD's)
    usesNamed e = case e of
        EId n | "__duses" `isPrefixOf` n -> EId ("bsc_uses_" ++ drop 7 n)
        ECall i m as -> ECall i m (map usesNamed as)
        EIf a b c -> EIf (usesNamed a) (usesNamed b) (usesNamed c)
        ECase i arms d -> ECase (usesNamed i) [ (usesNamed l, usesNamed v) | (l, v) <- arms ] (fmap usesNamed d)
        ESel a i -> ESel (usesNamed a) (usesNamed i)
        EApp h as -> EApp (usesNamed h) (map usesNamed as)
        EBin o a b -> EBin o (usesNamed a) (usesNamed b)
        EUn o a -> EUn o (usesNamed a)
        ETup xs -> ETup (map usesNamed xs)
        _ -> e

-- | model.ent: the slot base, kind number and port code.
ent :: Model -> MPort -> Either PyErr (Int, Int, Int)
ent m (kd, i, p)
    | kd == "tasks" = Right (mOff m M.! kd, kn, 0)
    | otherwise = case elemIndex p (kdPorts k) of
        Just c -> Right (mOff m M.! kd + i * length (kdPorts k), kn, c)
        Nothing -> Left (PyErr "ValueError" (pmRepr p ++ " is not in list"))
  where k = mK m M.! kd
        kn = fromMaybe 0 (elemIndex kd (mKinds m))

users :: Model -> M.Map MPort [(Int, Int)]
users m = M.fromListWith (\new old -> old ++ new) [ (sPort s, [(k, i)]) | (k, r) <- zip [0 ..] (mRules m), (i, s) <- zip [0 ..] (F.toList (tSites r)) ]

-- | model.excl_src: a recorded reason rules x and y never both fire ('blk',
-- 'd' or 'me'), or Nothing.
exclSrc :: Model -> Int -> Int -> Maybe String
exclSrc m x y
    | y `elem` M.findWithDefault [] x (mBlk m) = Just "blk"
    | x `elem` M.findWithDefault [] y (mBlk m) = Just "blk"
    | M.member (x, y) (mDFind m) = Just "d"
    | M.member (x, y) (mMEFind m) = Just "me"
    | otherwise = Nothing

-- ---------------------------------------------------------------- the view

pyB :: Bool -> String
pyB b = if b then "True" else "False"

pyList :: [String] -> String
pyList xs = "[" ++ intercalate ", " xs ++ "]"

-- | --show-model=model (with the scan lines added by the caller).
showModel :: Model -> [String]
showModel m =
    [ "kinds " ++ unwords (mKinds m) ]
    ++ [ "gen " ++ g ++ " " ++ (mGcls m M.! g) ++ " " ++ t ++ " | " ++ ss | ((t, ss), g) <- omToList (mGen m) ]
    ++ [ "inst " ++ k ++ " " ++ kd ++ " " ++ show i | (k, (kd, i)) <- omToList (mInst m) ]
    ++ [ "nR " ++ show (mNR m) ++ " nM " ++ show (mNM m) ++ " nS " ++ show (mNS m) ++ " nT " ++ show (mNT m)
         ++ " nslots " ++ show (mNSlots m) ++ " d " ++ show (mD m) ]
    ++ concat [ [ "step " ++ show k ++ " " ++ tName r ++ " meth=" ++ fromMaybe "None" (tMeth r) ++ " fallback=" ++ pyB (tFallback r)
                , "  guard " ++ maybe "None" mxRepr (tGuard r)
                , "  res " ++ mxRepr (tRes r)
                , "  tasks " ++ pyList (map show (tTasks r))
                , "  readpos " ++ unwords [ c ++ "." ++ mm ++ "=" ++ show i | ((c, mm), i) <- omToList (tReadPos r) ] ]
                ++ [ "  site " ++ show i ++ " " ++ sKind s ++ " " ++ mportRepr (sPort s) ++ " " ++ fromMaybe "None" (sLabel s)
                     ++ " cond=" ++ mxRepr (sCond s) ++ " arg=" ++ mxRepr (sArg s) ++ extras s
                   | (i, s) <- zip [0 :: Int ..] (F.toList (tSites r)) ]
              | (k, r) <- zip [0 :: Int ..] (mRules m) ]
    ++ [ "lit " ++ show i ++ " " ++ pyReprStr n | (n, i) <- omToList (mLits m) ]
    ++ [ "opf " ++ show i ++ " " ++ pyReprStr n | (n, i) <- omToList (mOpsF m) ]
    ++ [ "opg " ++ show i ++ " " ++ pyReprStr n | (n, i) <- omToList (mOpsG m) ]
    ++ [ "fact " ++ show i ++ " " ++ fKind f ++ " site=" ++ show (fSite f) ++ " lit=" ++ maybe "None" show (fLit f)
         ++ " ok=" ++ pyB (fOk f) ++ " step=" ++ fStep f ++ " label=" ++ fLabel f ++ " Y=" ++ pyList (map show (fY f))
         ++ " late=" ++ show (fLate f)
       | (i, f) <- zip [0 :: Int ..] (mFacts m) ]
    ++ [ "later_drops " ++ show (mLaterDrops m) ++ " fan_outside " ++ show (mFanOutside m) ]
    ++ [ "check " ++ show c ++ " rule=" ++ show (ckRule ch) ++ " head=" ++ show (ckHead ch) ++ " lst=" ++ pyList (map show (ckLst ch))
         ++ " full=" ++ pyB (ckFull ch) | (c, ch) <- zip [0 :: Int ..] (mChecks m) ]
    ++ [ "blk " ++ show k ++ " " ++ pyList (map show v) | (k, v) <- M.toList (mBlk m) ]
    ++ [ "disj " ++ show x ++ " " ++ show y | (x, y) <- mDisj m ]
    ++ [ "cf_check " ++ rn ++ " " ++ ln | (rn, (ln, _)) <- mCfChecks m ]
    ++ [ "mtasks " ++ pyList [ "(" ++ pyReprStr a ++ ", " ++ show b ++ ")" | (a, b) <- mMTasks m ]
       , "tstart " ++ pyList (map show (mTStart m)) ]
    ++ [ "step_fallback " ++ s | s <- mStepFallback m ]
    ++ [ "unguarded " ++ pyList (map pyReprStr (mUnguarded m)) ]
    ++ [ "users " ++ mportRepr p ++ " " ++ pyList [ "(" ++ show k ++ ", " ++ show i ++ ")" | (k, i) <- us ] | (p, us) <- M.toList (users m) ]
  where
    extras s = concat
        [ maybe "" (\x -> " R=" ++ mxRepr x) (sR s)
        , maybe "" (\x -> " ckind=" ++ pyReprStr x) (sCKind s)
        , maybe "" (\x -> " Ysv=" ++ pyList (map show x)) (sYsv s)
        , maybe "" (\x -> " fact=" ++ show x) (sFact s)
        , maybe "" (\x -> " inv=" ++ (case x of { InvNone -> "None"; InvSelf -> "'self'"; InvFact k -> show k })) (sInv s)
        , maybe "" (\x -> " confirmed=" ++ pyB x) (sConfirmed s)
        , maybe "" (\x -> " hw=" ++ mxRepr x) (sHw s)
        , maybe "" (\x -> " args=" ++ pyList (map mxRepr x)) (sArgs s) ]

-- | The largest step fan-in: its sites' conditions' and arguments' operator inputs (each site states its own).
stepFanIn :: Model -> Int
stepFanIn m = maximum (0 : [ sum [ fan (sCond s) + fan (sArg s) | s <- F.toList (tSites r) ] | r <- mRules m ])
  where
    fan x = case x of
        MF _ js -> length js
        MG _ js -> length js
        MLit _ -> 1
        MSplit js a b -> length js + fan a + fan b
        _ -> 0

-- | Pairs of users of a port with arguments that differ, outside one check group: the
-- members a file's mux proves exclusive one by one, at most.
muxPairs :: Model -> Int
muxPairs m = sum [ length [ () | (a, x) <- zip [0 :: Int ..] ks, y <- drop (a + 1) ks, exclSrc m x y /= Just "me" ]
                 | ((kd, _, pm), us) <- M.toList (users m), kd /= "tasks", kdHasArg (mK m M.! kd) pm
                 , let ks = S.toAscList (S.fromList (map fst us)), length ks >= 2
                 , S.size (S.fromList [ sArg (Sq.index (tSites (rules ! k)) si) | (k, si) <- us ]) >= 2 ]
  where rules = listArray (0, length (mRules m) - 1) (mRules m)
