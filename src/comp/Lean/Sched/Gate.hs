-- | The gate (design.md §11): does the modular proof apply to a module in its bsc mode?
-- Items from the model's record (Facts), readiness (Sites), shadow classes and the counts here.
module Lean.Sched.Gate
    ( Counts(..), gateCounts, countRow, GateIn(..), gateReasons, gateRow, rdyLeft, modeName, gateReport
    ) where

import ABin (ABinModInfo(..))
import Control.Exception (SomeException, catch, evaluate)
import Data.List (intercalate, isPrefixOf, sortBy)
import Flags (Flags(..))
import PPrint (ppString)
import Text.Printf (printf)
import qualified Data.Map.Strict as M ((!), elems, findWithDefault, fromList, fromListWith)
import qualified Data.Set as S (member, empty, fromList, singleton, size, union)

import Lean.Sched.Bounds (Bounds(bnGateMuxPairs, bnGateEsc, bnGateEscFacts, bnGateSeg, bnGateModFacts,
    bnGateSelmux, bnGateSlots, bnGateCodes, bnGateLcUsers, bnGateStep, bnGatePortUsers, bnGateGroup))
import Lean.Sched.Emit (emitFile, Emitted(..))
import Lean.Sched.Emit.Ctx (ecHasUnconf)
import Lean.Sched.Emit.Plan (mkPlan)
import Lean.Sched.Expr (memAPkg)
import Lean.Sched.Facts (J(..), GateFacts(..), jsonDumps)
import Lean.Sched.Model (Model(..), muxPairs)
import Lean.Sched.Py (omLookup)
import Lean.Sched.Run (gateModel, prepare, GateModel(gmModel, gmShare, gmFacts), Input(inFlags, inTimer,
    inDesign, inBounds, inName, inABMI), Prep(pSites, pAP, pPrims, pSolver, pDV, pBL))
import Lean.Sched.Shadow (ShadowOut(..), shadowReport)
import Lean.Sched.Sites (Sites(..), SiteRec(..), RecKind(..), RdySV(..))
import Lean.Sched.Time (phase)
import Lean.Sched.Tree (APkg(..), Act(..))
import Lean.Sched.Types (leanNamespace, portLabel, Call(clCond, clPort), Design(dUnrealized, dSteps),
    Step(stChecks, stDescr, stReads, stCalls, stName))

-- | The bsc mode, from the flags the module was compiled with.
modeName :: Flags -> String
modeName fl = case (aggImpConds fl, schedConds fl) of
    (True, True) -> "default"
    (False, True) -> "noagg"
    (True, False) -> "nosched"
    (False, False) -> "both"

-- | Steps' sites (reads, calls, a rule's task statements), the largest port's users, the
-- largest group of run-time checks, and the most callers of a port proved pair by pair.
data Counts = Counts { cnSteps, cnSites, cnTasks, cnStep1, cnStep2, cnStep3, cnSegs, cnUsers, cnGroup, cnPairs :: Int }

gateCounts :: Bounds -> Design -> APkg -> Counts
gateCounts bnd d ap = Counts (length rows) (sum rows) (sum rows - sum (map rc steps)) t1 t2 t3 (segments rows)
                             (maximum (0 : map S.size (M.elems ports))) (largestGroup checks)
                             (maximum (0 : [ S.size us | (us, c) <- M.elems allp, hasm || c ]))
  where
    steps = dSteps d
    kind st = case words (stDescr st) of { (k : _) -> k; _ -> "" }
    rc st = length (stReads st) + length (stCalls st)
    nt st | kind st == "rule" = length [ () | Just (_, acts) <- [omLookup (stName st) (apRules ap)], ATask {} <- acts ]
          | otherwise = 0
    rows = [ rc st + nt st | st <- steps ]
    (t1, t2, t3) = case take 3 (sortBy (flip compare) rows ++ [0, 0, 0]) of { [a, b, c] -> (a, b, c); _ -> (0, 0, 0) }
    segments = go 1 0
      where go n _ [] = n :: Int
            go n acc (u : us) | acc + u > bnGateSeg bnd && acc > 0 = go (n + 1) u us
                              | otherwise = go n (acc + u) us
    hasm = any ((== "method") . kind) steps
    ports = M.fromListWith S.union [ (portLabel (clPort c), S.singleton (stName st)) | st <- steps, c <- stCalls st ]
    allp = M.fromListWith (\(a, x) (b, y) -> (S.union a b, x || y))
               [ (portLabel (clPort c), (S.singleton (stName st), trim (ppString (clCond c)) `notElem` ["1'b1", "1'd1"]))
               | st <- steps, c <- stCalls st ]
    trim = reverse . dropWhile (== ' ') . reverse . dropWhile (== ' ')
    checks = [ (h, t) | st <- steps, c <- stChecks st, (h : t) <- [wfNames (ppString c)] ]

-- | The rules a check line names, as the regex WILL_FIRE_(\S+?)(?=[\s)&|]|$) finds them.
wfNames :: String -> [String]
wfNames s = case s of
    [] -> []
    _ | "WILL_FIRE_" `isPrefixOf` s, (c : r) <- drop 10 s, c `notElem` " \t\n\r\f\v" ->
          let (n, rest) = break (`elem` " \t\n\r\f\v)&|") r
          in (c : n) : wfNames rest
    (_ : r) -> wfNames r

-- | The longest chain of one-tail check lines, each list the suffix of the next.
largestGroup :: [(String, [String])] -> Int
largestGroup checks = go S.empty 0 (zip [0 :: Int ..] checks)
  where
    arr = M.fromList (zip [0 :: Int ..] checks)
    bylst = M.fromListWith (flip (++)) [ (t, [c]) | (c, (_, t)) <- zip [0 :: Int ..] checks ]
    go _ best [] = best
    go done best ((c0, (h0, t0)) : rest)
        | length t0 /= 1 || S.member c0 done = go done best rest
        | otherwise =
            let chase sq chain = case [ c | c <- M.findWithDefault [] sq bylst, not (S.member c done), c `notElem` chain ] of
                    [] -> (sq, chain)
                    (c : _) -> chase (fst (arr M.! c) : sq) (chain ++ [c])
                (sq, ch) = chase (h0 : t0) [c0]
            in go (S.union done (S.fromList ch)) (max best (length sq)) rest

countRow :: String -> Counts -> String
countRow name c = printf "%-24s %5d %6d %5d %6d %6d %6d %6d %5d %5d %5d" name (cnSteps c) (cnSites c) (cnTasks c)
                         (cnStep1 c) (cnStep2 c) (cnStep3 c) (cnSegs c) (cnUsers c) (cnGroup c) (cnPairs c)

-- | Readiness sites whose site-value facts are refuted: (step, label).
rdyLeft :: Sites -> [(String, String)]
rdyLeft st = [ (srStep r, srLabel r) | r <- sxRecs st, RRdy { rSV = sv } <- [srKind r], not (svOk sv) ]

data GateIn = GateIn
    { giRecord :: [(String, J)]
    , giRdy :: Maybe [(String, String)]
    , giShadow :: Maybe ShadowOut
    , giCounts :: Counts
    , giUnrealized :: Int
    , giUnconf :: Bool          -- ^ the emitter's file would take unconfirmed facts as hypotheses
    , giMuxPairs :: Int         -- ^ mux member pairs outside a check group (an upper bound on the file's)
    }

-- | The failing gate items, in the prototype's order and words; none means path A.
gateReasons :: Bounds -> GateIn -> [String]
gateReasons bnd gi = why0 ++ [ "the file would state unconfirmed facts as hypotheses" | null why0, giUnconf gi ]
  where
    why0 = unreal ++ scanWhy ++ rdyWhy ++ shWhy ++ ctWhy
    g = giRecord gi
    look k = lookup k g
    int k = case look k of { Just (JI n) -> Just n; _ -> Nothing }
    num k = maybe 0 id (int k)
    gt k b = maybe False (> fromIntegral b) (int k)
    how = case look "esc_facts_how" of { Just (JO kv) -> kv; _ -> [] }
    howN k = case lookup k how of { Just (JI n) -> n; _ -> 0 }
    unreal = [ "decided flat (" ++ show (giUnrealized gi) ++ " exported CF pairs bsc records as unrealizable)" | giUnrealized gi > 0 ]
    scanWhy = case look "error" of
        Just e -> ["emitter scan not computed (" ++ pyStr e ++ ")"]
        Nothing -> first ++ rest
    first
        | look "scan" /= Nothing && pyStr' (look "scan") /= "ok" = ["emitter scan fails: " ++ pyStr' (look "scan")]
        | gt "escapes" (bnGateEsc bnd) = [show (num "escapes") ++ " escaped pairs"]
        | gt "esc_facts_max_per_pair" (bnGateEscFacts bnd) = [show (num "esc_facts_max_per_pair") ++ " dynamic facts in one escaped pair"]
        | num "escapes" /= 0 && (int "esc_facts" == Nothing || not (isNull (look "esc_facts_err"))) =
            ["escape facts not computed (" ++ pyStr' (look "esc_facts_err") ++ ")"]
        | otherwise =
            [ show (howN "null") ++ " escape facts unconfirmed (" ++ intercalate "; " (take 3 unconf) ++ ")" | howN "null" /= 0 ]
            ++ [ show (num "esc_facts_fallback") ++ " escape facts on a footprint-only step (not checked as a file would)"
               | num "esc_facts_fallback" /= 0 ]
    unconf = case look "esc_facts_unconfirmed" of { Just (JL xs) -> [ s | JS s <- xs ]; _ -> [] }
    rest = [ "a step of " ++ show (num "seg_uses") ++ " uses in one scan segment" | gt "seg_uses" (bnGateSeg bnd) ]
        ++ [ show (num "mod_facts") ++ " facts in the module" | gt "mod_facts" (bnGateModFacts bnd) ]
        ++ [ "pairwise port of " ++ show (num "pairwise") ++ " users" | gt "pairwise" (bnGateSelmux bnd) ]
        ++ [ show (num "slots") ++ " slots" | gt "slots" (bnGateSlots bnd) ]
        ++ [ show (num "codes") ++ " rule codes" | gt "codes" (bnGateCodes bnd) ]
        ++ [ "last-caller port with " ++ show lcg ++ " callers" | lcg > fromIntegral (bnGateLcUsers bnd) ]
    lcg = maximum (0 : [ n | Just (JL ps) <- [look "lc"], JL [_, JI n] <- ps ])
    rdyWhy = case giRdy gi of
        Nothing -> ["sites: not measured"]
        Just [] -> []
        Just ls -> ["readiness refuted: " ++ intercalate "; " [ a ++ " / " ++ b | (a, b) <- ls ]]
    shWhy = case giShadow gi of
        Just sh | shOk sh ->
            [ "shadowing: " ++ intercalate "; " (shBlocked sh) | not (null (shBlocked sh)) ]
            ++ [ "last-caller port with " ++ show (shLcMax sh) ++ " callers" | shLcMax sh > bnGateLcUsers bnd ]
        _ -> ["shadow: not measured"]
    c = giCounts gi
    ctWhy = [ "step of " ++ show (cnStep1 c) ++ " sites" | cnStep1 c > bnGateStep bnd ]
         ++ [ "port of " ++ show (cnUsers c) ++ " users" | cnUsers c > bnGatePortUsers bnd ]
         ++ [ "group of " ++ show (cnGroup c) | cnGroup c > bnGateGroup bnd ]
         ++ [ show (giMuxPairs gi) ++ " mux member pairs proved one by one" | giMuxPairs gi > bnGateMuxPairs bnd ]
    isNull v = case v of { Nothing -> True; Just JNull -> True; _ -> False }
    pyStr' = maybe "None" pyStr

-- | A JSON value as Python's '%s' prints it after json.loads.
pyStr :: J -> String
pyStr v = case v of
    JS s -> s
    JNull -> "None"
    JB b -> if b then "True" else "False"
    JI n -> show n
    JL xs -> "[" ++ intercalate ", " (map repr xs) ++ "]"
    JO kv -> "{" ++ intercalate ", " [ "'" ++ k ++ "': " ++ repr x | (k, x) <- kv ] ++ "}"
  where repr x = case x of { JS s -> "'" ++ s ++ "'"; _ -> pyStr x }

-- | The summary row: design, mode, verdict, the emitter's dry run, shadow
-- classes, the record's counts, assertions relied on, leaves, reasons, largest step.
gateRow :: String -> String -> Maybe Bool -> GateIn -> [String] -> String
gateRow name mode emits gi why =
    printf "%-24s %-8s %-5s%s shadow ports [%s] lc callers max %d%s%s" name mode (if null why then "A" else "flat" :: String)
           em (if null cls then "-" else cls) lcmax gx (if null why then "" else " | " ++ intercalate " | " why)
    ++ " step " ++ show (cnStep1 (giCounts gi))
  where
    g = giRecord gi
    look k = lookup k g
    sh = maybe Nothing (\s -> if shOk s then Just s else Nothing) (giShadow gi)
    cls = maybe "?" (\s -> intercalate ", " [ k ++ " " ++ show n | (k, n) <- shClasses s ]) sh
    lcmax = maybe 0 shLcMax sh
    val k = maybe "None" pyStr (look k)
    how = case look "esc_facts_how" of { Just (JO kv) -> kv; _ -> [] }
    howS = intercalate ", " [ k ++ " " ++ show n | (k, JI n) <- how, n /= 0 ]
    hasErr = look "error" /= Nothing
    gx | hasErr = ""
       | otherwise = " slots " ++ val "slots" ++ " codes " ++ val "codes" ++ " esc " ++ val "escapes" ++ " facts " ++ val "esc_facts"
                     ++ " (" ++ (if null howS then "-" else howS) ++ ") pairwise " ++ val "pairwise"
                     ++ " seg_uses " ++ val "seg_uses" ++ " mod_facts " ++ val "mod_facts" ++ relied ++ leaves
    ra = ( if hasErr then 0 else intOf "me_needed_pairs", if hasErr then 0 else maybe 0 id (lookup "assert" [ (k, n) | (k, JI n) <- how ])
         , maybe 0 (fst . shRely) sh, maybe 0 (snd . shRely) sh )
    intOf k = case look k of { Just (JI n) -> n; _ -> 0 }
    relied = let (a, b, x, y) = ra in
             if (a, b, x, y) == (0, 0, 0, 0) then ""
             else printf " assertions relied on: scan %d me, escape facts %d cf, shadow %d me %d cf" a b x y
    leaves = case look "generic_class" of
        Just (JO kv@(_ : _)) -> " leaves " ++ intercalate ", " [ k ++ ":" ++ pyStr v | (k, v) <- sortBy (\a b -> compare (fst a) (fst b)) kv ]
        _ -> ""
    em = case emits of { Nothing -> ""; Just e -> " emitter " ++ (if e then "yes" else "no ") }

instance Eq J where
    a == b = case (a, b) of
        (JS x, JS y) -> x == y
        (JI x, JI y) -> x == y
        (JNull, JNull) -> True
        (JB x, JB y) -> x == y
        (JL x, JL y) -> x == y
        (JO x, JO y) -> x == y
        _ -> False

-- | --gate: items 1-7 from the model, the shadow classes and the counts, and whether the
-- emitter writes the file; the GATE record, count row, shadow lines and summary row.
gateReport :: Input -> IO (Either String [String])
gateReport inp = do
    let t = inTimer inp
        d = inDesign inp
        bnd = inBounds inp
        name = inName inp
        unreal = length (dUnrealized d)
    r <- prepare inp
    let counts = gateCounts bnd d (either (const (memAPkg (abmi_apkg (inABMI inp)))) pAP r)
    _ <- phase t "counts" (evaluate (cnStep1 counts + cnGroup counts + cnPairs counts + unreal))
    res <- case r of
        Left (Right err) -> return (Left err)
        Left (Left err) -> return (Right ([("name", JS name), ("error", JS ("sites: " ++ take 200 err))], Nothing, Nothing, (False, False), 0))
        Right p -> do
            gm <- gateModel inp p
            sh <- phase t "shadow" $ shadowReport (pSolver p) d (pDV p) (pSites p) (pPrims p) name
                      >>= \x -> evaluate (length (concat (shLines x))) >> return x
            -- the emitter's run builds this model unless the gate's run was lenient, so it is reused
            ok <- phase t "emit-dry" $ case gmModel gm of
                Right m | not (mLenient m) -> do
                    let run = do
                            pe <- mkPlan (pSolver p) (gmShare gm) (pDV p) (pSites p) (pBL p) bnd (leanNamespace d) m
                            case pe of
                                Left _ -> return (False, False)
                                Right ec -> do
                                    let em = emitFile ec bnd (leanNamespace d ++ ".lean")
                                    _ <- evaluate (sum (map (length . snd) (emParts em)) + length (concat (emLog em)))
                                    return (True, ecHasUnconf ec)
                    run `catch` (\e -> const (return (False, False)) (e :: SomeException))
                _ -> return (False, False)
            return (Right (gfRecord (gmFacts gm), Just (rdyLeft (pSites p)), Just sh, ok, either (const 0) muxPairs (gmModel gm)))
    return $ case res of
        Left err -> Left err
        Right (rec, rdy, sh, (emits, unconf), mp) ->
            let gi = GateIn rec rdy sh counts unreal unconf mp
            in Right ([ "GATE " ++ jsonDumps (JO rec), "counts " ++ countRow name counts, "unrealized " ++ show unreal ]
                      ++ map ("shadow " ++) (maybe [] shLines sh)
                      ++ [gateRow name (modeName (inFlags inp)) (Just emits) gi (gateReasons bnd gi)])
