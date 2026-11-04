-- | The modular obligation file of one module (mke2e.py, emit/conventions.md):
-- the sections in order, the facts' group paths, parts and the summary.
module Lean.Sched.Emit
    ( emitFile, Emitted(..), Proves(..), emOff, provesText, MutantFile(..), emitModule
    ) where

import Control.Exception (ErrorCall(..), SomeAsyncException, displayException, evaluate, fromException, throwIO, try)
import Data.Array ((!), listArray)
import Data.Bits (Bits(popCount))
import Data.Char (ord, isAlphaNum)
import Data.List (isPrefixOf, intercalate)
import Data.Maybe (fromMaybe, isJust, isNothing)
import System.FilePath (takeBaseName)
import qualified Data.Foldable as F (Foldable(toList))
import qualified Data.Map.Strict as M (Map, lookup)

import Lean.Print (runW, w, WS(wsL))
import Lean.Sched.Bounds (Bounds(bnGatePart))
import Lean.Sched.Emit.Acyc (acycPlan, assumesOf, emitAcyc, emitTop, AcycPlan(apAgNeed, apNoAcyc, apNN))
import Lean.Sched.Emit.Arms (emitArms, emitHyps)
import Lean.Sched.Emit.Cert (emitCert, emitEAQ, emitLC)
import Lean.Sched.Emit.Ctx (isUncond, kd, step, EC(ecESC, ecNS, ecName, ecM, ecEFacts, ecNR, ecNM, ecNT,
    ecMuxes, ecNeed, ecEdit, ecDomNote, ecHasUnconf, ecCFacts))
import Lean.Sched.Emit.Hdr (emitGuards, emitHeader, emitSites, emitUses)
import Lean.Sched.Emit.Plan (mkPlan)
import Lean.Sched.Domain (domainNote)
import Lean.Sched.Emit.Proofs (emitChecks, emitHQ, emitRY, emitSiteProofs, emitValues)
import Lean.Sched.Emit.ScanTabs (emitChains, emitScan, Counts(cnUncov, cnChkPairs))
import Lean.Sched.Facts (newFactShare)
import Lean.Sched.Kinds (KindDef(kdAlwaysRdy))
import Lean.Sched.Model (buildModelWith, pyStrErr, MFact(fOk), MSite(sPort), MStep(tSites), Model(mLaterDrops,
    mMTasks, mRules, mInst, mFacts))
import Lean.Sched.Mutate (candidates, choose, renumberTasks, Change(..), Mutant(..))
import Lean.Sched.Py (omSize)
import Lean.Sched.Run (prepare, Input(inBounds, inTimer, inDesign), Prep(pBL, pSolver, pDV, pSites, pDomain, pDomainWhy))
import Lean.Sched.Time (phase)

-- | The file's parts (path stem suffix and text), the lines the prototype printed, what it proves and
-- what it assumes (Acyc.assumesOf).
data Emitted = Emitted { emParts :: [(Maybe Int, String)], emLog :: [String], emProves :: Proves, emAssume :: [String] }

-- | What a file proves: the facts it states as hypotheses that the solver did not confirm (path B: escape
-- and caller pairs, then readiness and invariance facts), and why it has no Acyc/top (Nothing: it has).
data Proves = Proves { pvUnconf :: (Int, Int), pvNoTop :: Maybe String }

-- | What a file proves when it is not top from Hyps alone (Nothing: it is), for the gate row and --check.
provesText :: Proves -> Maybe String
provesText (Proves (a, b) noTop) = case (a + b, noTop) of
    (0, Nothing) -> Nothing
    (0, Just why) -> Just ("obligation only, no Acyc/top: " ++ why)
    (_, Nothing) -> Just ("path B: top under " ++ unc)
    (_, Just why) -> Just ("path B: obligation only, under " ++ unc ++ "; no Acyc/top: " ++ why)
  where unc = show (a + b) ++ " unconfirmed fact" ++ (if a + b == 1 then "" else "s")
              ++ " (escape or caller pairs " ++ show a ++ ", readiness or invariance " ++ show b ++ ")"

-- | Why a parent cannot build on the file (no top, or unconfirmed facts); Nothing on path A.
emOff :: Emitted -> Maybe String
emOff em = case emProves em of
    Proves _ (Just _) -> Just "no Acyc/top"
    Proves (a, b) Nothing | a + b > 0 -> Just "unconfirmed facts"
    _ -> Nothing

-- | A hardware mutant (--mutants): its kind, what it changes, and its file at a given path.
data MutantFile = MutantFile { mfKind :: String, mfWhat :: String, mfEmit :: FilePath -> IO (Either String Emitted) }

-- | The default run: sites, the model, its plan, the file's parts in memory (Left: the stop message)
-- and the first @nmut@ mutants; @tasks@ and @firstRules@ are measurement options.
emitModule :: Input -> Bool -> Maybe Int -> FilePath -> Int -> IO (Either String (Emitted, [MutantFile]))
emitModule inp tasks firstRules file nmut = stops (emitModule' inp tasks firstRules file nmut)

-- | Any exception (an error call, a failed pattern) is a stop; interrupts and overflows propagate.
stops :: IO (Either String a) -> IO (Either String a)
stops act = do
    r <- try act
    case r of
        Left e | Just a <- fromException e -> throwIO (a :: SomeAsyncException)
               | otherwise -> return (Left (dropPrefix "schedcheck: " (maybe (displayException e) (\(ErrorCall m) -> m) (fromException e))))
        Right x -> return x
  where dropPrefix pre s = if take (length pre) s == pre then drop (length pre) s else s

forced :: Emitted -> IO Emitted
forced em = evaluate (sum (map (length . snd) (emParts em)) + length (concat (emLog em))) >> return em

emitModule' :: Input -> Bool -> Maybe Int -> FilePath -> Int -> IO (Either String (Emitted, [MutantFile]))
emitModule' inp tasks firstRules file nmut = do
    let t = inTimer inp
    r <- prepare inp
    case r of
        Left (Left err) -> return (Left ("sites: " ++ err))
        Left (Right err) -> return (Left err)
        Right p | (why : _) <- pDomainWhy p -> return (Left why)
        Right p -> do
            me <- phase t "model" (buildModelWith False tasks firstRules (pDV p) (pSites p))
            case me of
                Left err -> return (Left (pyStrErr err))
                Right m -> do
                    let plan m' = newFactShare (pSolver p) (pSites p) >>= \share ->
                            fmap (fmap (\ec -> ec { ecDomNote = domainNote (pDomain p) }))
                                 (mkPlan (pSolver p) share (pDV p) (pSites p) (pBL p) (inBounds inp) (takeBaseName file) m')
                    pe <- phase t "facts" (plan m)
                    case pe of
                        Left err -> return (Left err)
                        Right ec -> phase t "emit" $ do
                            -- forced here, so the emitter's own stops are caught by emitModule
                            em <- forced (emitFile ec (inBounds inp) file)
                            let mutant mu = MutantFile (muKind mu) (muWhat mu) $ \out -> stops $ case muChange mu of
                                    Hw e -> Right <$> forced (emitFile ec { ecEdit = e } (inBounds inp) out)
                                    Renumber k -> plan (renumberTasks k m) >>= either (return . Left)
                                                      (\ec' -> Right <$> forced (emitFile ec' (inBounds inp) out))
                                muts = if nmut <= 0 then [] else map mutant (choose nmut (candidates (inDesign inp) ec))
                            return (Right (em, muts))

-- | UTF-8 length of a string, as os.path.getsize counts it.
utf8Len :: String -> Int
utf8Len = sum . map (\c -> let o = ord c in if o < 0x80 then 1 else if o < 0x800 then 2 else if o < 0x10000 then 3 else 4)

emitFile :: EC -> Bounds -> FilePath -> Emitted
emitFile ec bnd out = Emitted files (notes ++ partsMsg ++ [segMsg, wroteMsg] ++ [ "file " ++ out ++ ": " ++ t | Just t <- [provesText proves] ])
                               proves (assumesOf ec)
  where
    unconfE = length [ () | (_, _, _, _, Nothing, _) <- ecEFacts ec ] + length [ () | (_, _, _, _, _, Nothing, _) <- ecCFacts ec ]
    unconfF = length (filter (not . fOk) (mFacts m))
    proves = Proves (if ecHasUnconf ec then (unconfE, unconfF) else (0, 0)) (apNoAcyc ap)
    nS = ecNS ec
    needuc = or [ not (isUncond s) && not (kdAlwaysRdy (kd ec k) p) | k <- [0 .. nS - 1], s <- F.toList (tSites (step ec k)), let (k, _, p) = sPort s ]
    ap = acycPlan ec
    prog = do
        emitHeader ec
        emitUses ec
        emitSites ec
        emitGuards ec
        emitArms ec
        hy@(_, modF, nHy, nUn) <- emitHyps ec
        emitSiteProofs ec
        emitValues ec
        emitHQ ec
        emitRY ec needuc
        emitChecks ec
        cn <- emitChains ec
        (nseg, segUses) <- emitScan ec bnd (modF, nHy, nUn, 0)
        hvr <- emitLC ec
        emitEAQ ec
        emitCert ec hvr
        w "/-! ## Acyclicity: read lists as filters of allOuts -/\n"
        if isJust (apNoAcyc ap)
            then w ("end\nend " ++ ecName ec ++ "\n\n#print axioms " ++ ecName ec ++ ".obl\n")
            else emitAcyc ec ap >> emitTop ec
        let (fp, _, _, _) = hy
        return (fp, cn, nseg, segUses)
    ((fpath, cn, nseg, segUses), ws) = runW prog
    lsRaw = F.toList (wsL ws)
    ls = map (subFacts fpath) lsRaw
    name = ecName ec
    -- parts: cut between top-level declarations when the file is above the part bound
    total = sum (map ((+ 1) . length) ls)
    partBytes = if total > bnGatePart bnd then bnGatePart bnd `div` 2 else 0
    hdrEnd = length (takeWhile (\l -> not ("\nnoncomputable section\n" `isInfix` ("\n" ++ l ++ "\n"))) ls)
    lv = listArray (0, length ls - 1) ls
    depths = scanl (\d (i, l) -> if i > hdrEnd then d + sum [ delta ln | ln <- lines' l ] else d) 0 (zip [0 :: Int ..] ls)
    delta ln | "section" `isPrefixOf` ln || "noncomputable section" `isPrefixOf` ln = if wordStart ln then 1 else 0
             | "end" `isPrefixOf` ln && endWord ln = -1
             | otherwise = 0 :: Int
    wordStart ln = let r = if "noncomputable " `isPrefixOf` ln then drop 14 ln else ln
                   in "section" `isPrefixOf` r && (length r == 7 || not (isWordC (r !! 7)))
    endWord ln = length ln == 3 || not (isWordC (ln !! 3))
    isWordC c = isAlphaNum c || c == '_'
    depA = listArray (0, length depths - 1) depths
    dep i = depA ! i
    parts | partBytes == 0 || isJust (apNoAcyc ap) = [(0, length ls)]
          | otherwise = go 0 0 0
      where go start acc i
              | i >= length ls = [(start, length ls)]
              | i > hdrEnd && acc + length (lv ! i) + 1 > partBytes && acc > 0 && i < length ls - 1 && dep i `elem` [0, 1]
                && dep (i + 1) >= 0 && not ("variable" `isPrefixOf` (lv ! i)) = (start, i) : go i (length (lv ! i) + 1) (i + 1)
              | otherwise = go start (if i > hdrEnd then acc + length (lv ! i) + 1 else acc) (i + 1)
    stem = reverse (takeWhile (/= '/') (reverse (dropExt out)))
    dropExt p = case break (== '.') (reverse p) of
        (e, '.' : r) | '/' `notElem` e -> reverse r
        _ -> p
    hdr = let h = intercalate "\n" (take (hdrEnd + 1) ls)
              ix = fromMaybe (error "schedcheck: the header has no noncomputable section") (findIx "\nnoncomputable section\n" h)
          in take (ix + length "\nnoncomputable section\n") h
    np = length parts
    body pi (a, b)
        | pi == 0 = slice a b
        | otherwise = [replaceImport (pi - 1) hdr] ++ [ "section\nvariable {V : Type}" | dep a /= 0 ] ++ slice a b
    slice a b = take (b - a) (drop a ls)
    replaceImport p h = case findIx "\nimport Scan\n" ("\n" ++ h ++ "\n") of
        _ -> subImport p h
    subImport p h = unlines' [ if l `elem` ["import Scan", "import LastCaller"] && firstImp l h then "import " ++ stem ++ "_p" ++ show p else l | l <- lines' h ]
    firstImp _ _ = True
    texts = [ if pi < np - 1
              then (Just pi, intercalate "\n" (body pi pr) ++ (if dep (snd pr) /= 0 then "\nend" else "") ++ "\nend\nend " ++ name ++ "\n")
              else (Nothing, intercalate "\n" (body pi pr) ++ "\n")
            | (pi, pr) <- zip [0 ..] parts ]
    sizes = map (utf8Len . snd) texts
    largest = maximum sizes
    files = [ (p, t ++ (if isNothing p then "\n/-! Gate: " ++ show np ++ " part(s) (one Lean file each), the largest " ++ show largest
                                            ++ " bytes, at most " ++ show (bnGatePart bnd) ++ ". -/\n" else ""))
            | (p, t) <- texts ]
    m = ecM ec
    notes = [ "note: " ++ name ++ ": method " ++ a ++ ": " ++ show n ++ " task call(s) left out of task agreement (bsc G0020)" | (a, n) <- mMTasks m ]
    partsMsg = if np > 1 then ["parts " ++ show np ++ ": " ++ intercalate ", " [ dropExt out ++ "_p" ++ show i ++ ".lean" | i <- [0 .. np - 2] ] ++ ", then " ++ out] else []
    segMsg = "scan segments " ++ show nseg ++ " (at most " ++ show segUses ++ " uses each)"
    nsites = sum [ length (tSites r) | r <- mRules m ]
    efs = ecEFacts ec
    wroteMsg = "wrote " ++ out ++ ": rules " ++ show (ecNR ec) ++ ", methods " ++ show (ecNM ec) ++ ", children " ++ show (omSize (mInst m))
               ++ ", sites " ++ show nsites ++ ", task statements " ++ show (ecNT ec) ++ ", muxes " ++ show (length (ecMuxes ec))
               ++ " (member pairs: " ++ show (cnChkPairs cn) ++ " by checks, " ++ show (cnUncov cn) ++ " one by one), exR bits "
               ++ show (sum (map popCount (ecNeed ec))) ++ ", escapes " ++ show (length (ecESC ec)) ++ " (site pairs " ++ show (length efs)
               ++ ", unconfirmed " ++ show (length [ () | (_, _, _, _, Nothing, _) <- efs ]) ++ "), readiness and invariance facts "
               ++ show (length (mFacts m)) ++ " (unconfirmed " ++ show (length (filter (not . fOk) (mFacts m))) ++ "), later-site literals dropped "
               ++ show (mLaterDrops m) ++ ", ranks N=" ++ show (apNN ap) ++ ", ag lemmas " ++ show (length (F.toList (apAgNeed ap)))

isInfix :: String -> String -> Bool
isInfix pat s = any (pat `isPrefixOf`) (tailsS s)

tailsS :: String -> [String]
tailsS [] = [[]]
tailsS s@(_ : r) = s : tailsS r

findIx :: String -> String -> Maybe Int
findIx pat s = case [ i | (i, t) <- zip [0 ..] (tailsS s), pat `isPrefixOf` t ] of
    (i : _) -> Just i
    [] -> Nothing

lines' :: String -> [String]
lines' s = case break (== '\n') s of
    (a, '\n' : r) -> a : lines' r
    (a, _) -> [a]

unlines' :: [String] -> String
unlines' = intercalate "\n"

-- | A fact's projection through its group: hy.esc_17 -> hy.g0.esc_17.
subFacts :: M.Map (String, String) String -> String -> String
subFacts fp = go ' '
  where
    go _ [] = []
    go prev s@(c : r)
        | not (isW prev), Just (src, rest) <- pfx s, (nm, rest') <- factName rest, not (null nm), endOk rest'
        = case M.lookup (src, nm) fp of
            Just g -> src ++ "." ++ g ++ "." ++ nm ++ go (lastC nm) rest'
            Nothing -> error ("schedcheck: KeyError: " ++ nm)
        | otherwise = c : go c r
    pfx s = case s of
        ('h' : 'y' : '.' : r) -> Just ("hy", r)
        ('h' : 'r' : '.' : r) -> Just ("hr", r)
        _ -> Nothing
    factName r = case [ p | p <- ["rdy_", "inv_", "esc_", "cx_"], p `isPrefixOf` r ] of
        (p : _) -> let ds = takeWhile (`elem` ['0' .. '9']) (drop (length p) r)
                   in if null ds then ("", r) else (p ++ ds, drop (length p + length ds) r)
        [] -> ("", r)
    endOk rest = case rest of { (c : _) -> not (isW c); [] -> True }
    isW c = isAlphaNum c || c == '_'
    lastC nm = last nm
