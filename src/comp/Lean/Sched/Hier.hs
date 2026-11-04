-- | Synthesized children (design.md §3.1): the instance DAG (from each .ba's instances), children first,
-- each child's place in a parent; a module is read in full only at its turn, one at a time.
module Lean.Sched.Hier
    ( Hier, synthInsts, loadHier, hierGate, hierEmit, HierFile(..)
    ) where

import ABin (ABin(..), ABinModInfo(..))
import ASyntax (APackage(apkg_state_instances), AVInst(avi_vname, avi_vmi))
import AState (aState)
import Control.DeepSeq (force)
import Control.Exception (evaluate)
import Data.List (intercalate, isPrefixOf)
import Error (ErrorHandle)
import GenABin (readABinFile)
import Id (getIdString)
import System.Directory (doesFileExist)
import System.FilePath ((</>), (<.>), takeBaseName, takeDirectory)
import VModInfo (vName, getVNameString)
import qualified Data.ByteString as BS (readFile)
import qualified Data.Map.Strict as M (Map, (!), empty, findWithDefault, fromList, fromListWith, insert, lookup, member)
import qualified Data.Set as S (Set, empty, fromList, insert, member, toList)

import Lean.Sched.Bounds (Bounds)
import Lean.Sched.Emit (Emitted(..), MutantFile, emitModule)
import Lean.Sched.Gate (gateReportS)
import Lean.Sched.BscFacts (disjointCandidates, confirmDisjointnessMore)
import Lean.Sched.Domain (domainOf, multiDomain, multiDomainVerdict)
import Lean.Sched.Export (Export(..), SynthKid(..), enableStop, exportOf, forceExport, mkExport, viewCheck, viewInHigh)
import Lean.Sched.Extract (extractDesign, renameSchedulerUses)
import Lean.Sched.Kinds (ifcTypes)
import Lean.Sched.Run (HierIn(..), Input(..), noHier)
import Lean.Sched.Time (Timer, phase)
import Lean.Sched.Types (Child(chMethods), Design(dChildren, dSteps, dModule), Port(ptInst, ptMeth),
    Step(stReads, stReadSites, stTaskReads))

-- | A module of the run: its .ba, why it is not read (Nothing: it is), and its synthesized instances
-- (instance, module) in instance order.
data HNode = HNode { hnPath :: FilePath, hnUnread :: Maybe String, hnKids :: [(String, String)] }

-- | The DAG: the top (read), nodes by module, the order children first (each module once, the top
-- last), and how a module is read in full.
data Hier = Hier { hTop :: String, hTopIn :: Input, hNodes :: M.Map String HNode, hOrder :: [String]
                 , hLoad :: FilePath -> IO (Either String Input) }

-- | A module's synthesized instances (bsc's soft-hyphen interface type), with their Verilog modules.
synthInsts :: APackage -> [(String, String)]
synthInsts apkg = [ (getIdString (avi_vname v), getVNameString (vName (avi_vmi v)))
                  | v <- apkg_state_instances apkg, S.member (getIdString (avi_vname v)) syn ]
  where syn = S.fromList [ i | (i, t) <- ifcTypes apkg, not (null t), last t == '\x00ad' ]

-- | A .ba's synthesized instances, from its package alone (Left: why the module is not read).
peekBa :: ErrorHandle -> FilePath -> IO (Either String [(String, String)])
peekBa errh path = do
    bytes <- BS.readFile path
    case fst (readABinFile errh path bytes) of
        ABinMod abmi _ -> case multiDomain (domainOf (abmi_apkg abmi)) of
            Just why -> return (Left (multiDomainVerdict ++ " (" ++ why ++ ")"))
            Nothing -> Right <$> evaluate (force (synthInsts (abmi_apkg abmi)))
        _ -> return (Left (path ++ " is not a module's .ba"))

-- | One .ba as the driver reads it: extraction, bsc's facts; a module of several clock domains is
-- not read (its own verdict).
loadBa :: ErrorHandle -> Timer -> Bounds -> FilePath -> IO (Either String Input)
loadBa errh timer bounds path = do
    bytes <- BS.readFile path
    let (abin, _) = readABinFile errh path bytes
    case abin of
        ABinMod abmi ver -> do
            _ <- evaluate (force (abmi_apkg abmi, abmi_aschedinfo abmi))
            let flags = abmi_flags abmi
                apkg0 = abmi_apkg abmi
            case multiDomain (domainOf apkg0) of
                Just why -> return (Left (multiDomainVerdict ++ " (" ++ why ++ ")"))
                Nothing -> do
                    aspkg <- aState errh flags (abmi_pps abmi) (abmi_aschedinfo abmi) apkg0 >>= evaluate . force
                    let abmi' = abmi { abmi_apkg = renameSchedulerUses apkg0 }
                    case extractDesign False path ver abmi' aspkg of
                        Left e -> return (Left e)
                        Right d0 -> do
                            d <- evaluate (force d0)
                            let cands = force (disjointCandidates d)
                            (facts, more, sv) <- confirmDisjointnessMore errh flags (abmi_apkg abmi') d cands
                            _ <- evaluate (force (cands, facts))
                            return (Right (Input d cands facts more sv abmi bounds (takeBaseName path) flags errh timer noHier))
        _ -> return (Left (path ++ " is not a module's .ba"))

-- | The DAG below a loaded top: each child's .ba beside its parent's, else in the search path.
loadHier :: ErrorHandle -> Timer -> Bounds -> [FilePath] -> FilePath -> Input -> IO Hier
loadHier errh timer bounds dirs topPath top = do
    let topName = dModule (inDesign top)
        topNode = HNode topPath Nothing (synthInsts (abmi_apkg (inABMI top)))
    (nodes, _, order) <- go (M.insert topName topNode M.empty, S.empty, []) S.empty topName
    return (Hier topName top nodes (reverse order) (\p -> phase timer "hier-read" (loadBa errh timer bounds p)))
  where
    -- depth-first, children in instance order, post-order (kept reversed); a module is read once
    go st@(nodes, done, order) onPath mn
        | S.member mn done || S.member mn onPath = return st
        | otherwise = do
            let node = nodes M.! mn
                kidMods = distinct (map snd (hnKids node))
            nodes' <- foldlM' (\ns k -> if M.member k ns then return ns else do
                        let here = takeDirectory (hnPath node)
                            cands = [ dir </> k <.> "ba" | dir <- here : dirs ]
                        found <- firstM doesFileExist cands
                        n <- case found of
                            Nothing -> return (HNode (here </> k <.> "ba") (Just (k ++ ".ba not found")) [])
                            Just p -> either (\e -> HNode p (Just e) []) (HNode p Nothing) <$> peekBa errh p
                        return (M.insert k n ns)) nodes kidMods
            (nodes'', done', order') <- foldlM' (\s k -> go s (S.insert mn onPath) k) (nodes', done, order) kidMods
            return (nodes'', S.insert mn done', mn : order')

-- | Distinct elements, in first-occurrence order.
distinct :: Ord a => [a] -> [a]
distinct = go S.empty
  where go _ [] = []
        go seen (x : xs) | S.member x seen = go seen xs
                         | otherwise = x : go (S.insert x seen) xs

foldlM' :: Monad m => (b -> a -> m b) -> b -> [a] -> m b
foldlM' _ z [] = return z
foldlM' f z (x : xs) = f z x >>= \z' -> foldlM' f z' xs

firstM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
firstM _ [] = return Nothing
firstM p (x : xs) = p x >>= \b -> if b then return (Just x) else firstM p xs

-- | What a parent knows of a child module: its export (or why none), why its file is no base for a
-- parent (Nothing: path A), why a parent cannot state it at all, and what its file assumes.
data KidStatus = KidStatus { ksExport :: Either String Export, ksOff :: Maybe String, ksStop :: Maybe String
                           , ksAssume :: [String] }

-- | A status recorded for the parents, evaluated so that it holds nothing of the module's input or file.
record :: String -> KidStatus -> M.Map String KidStatus -> IO (M.Map String KidStatus)
record mn ks st = do
    let str = maybe 0 length
    _ <- evaluate (either length (\x -> forceExport x `seq` 0) (ksExport ks) + str (ksOff ks) + str (ksStop ks)
                   + length (concat (ksAssume ks)))
    return (M.insert mn ks st)

-- | A module's place in the run: its export when it is not the top, and each synthesized instance's
-- kind (numbered by first instance) with its child's status.
kidsIn :: [(String, String)] -> Input -> M.Map String KidStatus -> Maybe Export -> HierIn
kidsIn kids inp status ex = HierIn ex (M.fromList [ (i, kid i k) | (i, k) <- kids ])
  where
    kindNo = M.fromList (zip (distinct (map snd kids)) [0 :: Int ..])
    insts = M.fromListWith (flip (++)) [ (k, [i]) | (i, k) <- kids ]
    rdyByInst = M.fromListWith (flip (++)) [ (j, [m]) | (j, m) <- S.toList (readyReads inp) ]
    none _ _ = False
    kid i k =
        let ks = M.findWithDefault (KidStatus (Left (k ++ ": not processed")) (Just "not processed") Nothing []) k status
            ex' = either (const (mkExport k [] none none none)) id (ksExport ks)
            child = M.lookup i (dChildren (inDesign inp))
            why = case ksExport ks of
                Left e -> Just e
                Right x -> case ksOff ks of
                    Just o -> Just ("its file: " ++ o)
                    Nothing -> case child >>= viewCheck x of
                        Just v -> Just v
                        Nothing -> case concat [ M.findWithDefault [] j rdyByInst | j <- M.findWithDefault [] k insts ] of
                            (m : _) -> Just ("the parent reads its ready port " ++ m)
                            [] -> Nothing
            stop = case (ksStop ks, maybe [] viewInHigh child) of
                (Just s, _) -> Just s
                (_, m : _) -> Just ("method " ++ m ++ " is always_enabled: it does not fire on the parent's enable"
                                    ++ " (the parent's alw obligation is not emitted yet)")
                _ -> Nothing
        in SynthKid ("syn" ++ show (kindNo M.! k)) ex' why stop (if why == Nothing then ksAssume ks else [])

-- | The child ports a module's steps read whose method is a child method's RDY.
readyReads :: Input -> S.Set (String, String)
readyReads inp = S.fromList [ (ptInst p, ptMeth p) | st <- dSteps d, p <- stReadSites st ++ map fst (stReads st) ++ concat [ ps | (_, _, ps) <- stTaskReads st ]
                                                    , "RDY_" `isPrefixOf` ptMeth p
                                                    , Just c <- [M.lookup (ptInst p) (dChildren d)], M.member (drop 4 (ptMeth p)) (chMethods c) ]
  where d = inDesign inp

-- | A child module's file, in the top file's directory.
childFile :: FilePath -> Export -> FilePath
childFile topFile ex = case takeDirectory topFile of
    "." -> exNS ex <.> "lean"
    d -> d </> exNS ex <.> "lean"

-- | The hierarchy row: each module, and each child instance's kind and place.
hierRow :: Hier -> [[String]] -> String
hierRow h kidRows = "hierarchy " ++ hTop h ++ ": modules " ++ show (length (hOrder h)) ++ " (" ++ intercalate ", " (hOrder h) ++ ")"
    ++ concatMap ("; " ++) (concat kidRows)

-- | A module's entries in the hierarchy row.
kidRow :: String -> [(String, String)] -> HierIn -> [String]
kidRow mn kids hi = [ mn ++ "." ++ i ++ " " ++ skKind sk ++ " " ++ k ++ " " ++ maybe ("path A" ++ assumes sk) ("assumed leaf: " ++) (skOff sk)
                    | (i, k) <- kids, Just sk <- [M.lookup i (hiKids hi)] ]
  where assumes sk = if null (skAssume sk) then "" else " (its file assumes " ++ intercalate ", " (skAssume sk) ++ ")"

-- | A module at its turn, read (the top already is): its input, its export when it is a child (Left:
-- why it has none), and why a parent cannot state it.
turn :: Hier -> String -> IO (Either String (Input, Either String (Maybe Export), Maybe String))
turn h mn
    | mn == hTop h = return (Right (hTopIn h, Right Nothing, Nothing))
    | Just why <- hnUnread node = return (Left why)
    | otherwise = fmap (fmap info) (hLoad h (hnPath node))
  where
    node = hNodes h M.! mn
    info inp = let (pps, d, apkg) = (abmi_pps (inABMI inp), inDesign inp, abmi_apkg (inABMI inp))
               in (inp, Just <$> exportOf pps d apkg, enableStop pps d apkg)

-- | --gate over the hierarchy: each module's record and rows, children first, then the hierarchy row.
hierGate :: Hier -> IO (Either String [String])
hierGate h = go M.empty [] [] (hOrder h)
  where
    kidsOf mn = hnKids (hNodes h M.! mn)
    go _ rows acc [] = return (Right (concat (reverse acc) ++ [hierRow h (reverse rows)]))
    go st rows acc (mn : rest) = do
        t <- turn h mn
        case t of
            Left why -> record mn (KidStatus (Left why) (Just why) Nothing []) st >>= \st' -> go st' rows acc rest
            Right (inp, ex, stop) -> do
                let hi = kidsIn (kidsOf mn) inp st (either (const Nothing) id ex)
                    row = kidRow mn (kidsOf mn) hi
                _ <- evaluate (sum (map length row))
                r <- gateReportS inp { inHier = hi }
                case r of
                    Left err | mn == hTop h -> return (Left err)
                             | otherwise -> record mn (KidStatus (Left err) (Just err) stop []) st >>= \st' ->
                                                go st' (row : rows) ([mn ++ ": " ++ err] : acc) rest
                    Right (ls, summ, asm) -> do
                        let s' = case ex of
                                Left why -> KidStatus (Left why) (Just why) stop []
                                Right mex -> KidStatus (maybe (Left "the top module") Right mex) summ stop asm
                        _ <- evaluate (sum (map length ls))
                        st' <- record mn s' st
                        go st' (row : rows) (ls : acc) rest

-- | A module's file in a hierarchical run.
data HierFile = HierFile { hfFile :: FilePath, hfEmitted :: Emitted, hfMutants :: [MutantFile], hfTop :: Bool }

-- | The hierarchy's files, children first, each handed to @onFile@ (which writes it) as it is emitted; the
-- notes (a child without a file: an assumed leaf) with the hierarchy row, and the top's stop.
hierEmit :: Hier -> FilePath -> Bool -> Maybe Int -> Int -> (HierFile -> IO ()) -> IO ([String], Maybe String)
hierEmit h topFile tasks firstRules nmut onFile = go M.empty S.empty [] [] (hOrder h)
  where
    kidsOf mn = hnKids (hNodes h M.! mn)
    finish rows notes stop = return (reverse notes ++ [hierRow h (reverse rows)], stop)
    go _ _ rows notes [] = finish rows notes Nothing
    go st files rows notes (mn : rest) = do
        t <- turn h mn
        case t of
            Left why -> record mn (KidStatus (Left why) (Just why) Nothing []) st >>= \st' -> go st' files rows
                           (("child " ++ mn ++ ": not read (" ++ why ++ "); an assumed leaf in its parents") : notes) rest
            Right (_, Left why, stop) -> record mn (KidStatus (Left why) (Just why) stop []) st >>= \st' -> go st' files rows
                           (("child " ++ mn ++ ": no file (" ++ why ++ "); an assumed leaf in its parents") : notes) rest
            Right (inp, Right mex, stop) -> do
                let isTop = mn == hTop h
                    file = maybe topFile (childFile topFile) mex
                    hi = kidsIn (kidsOf mn) inp st mex
                    row = kidRow mn (kidsOf mn) hi
                    status off = KidStatus (maybe (Left "top") Right mex) off stop
                _ <- evaluate (sum (map length row))
                r <- if not isTop && (file == topFile || S.member file files)
                     then return (Left (file ++ " is another module's file"))
                     else emitModule inp { inHier = hi } tasks firstRules file nmut
                case r of
                    Left msg | isTop -> finish (row : rows) notes (Just msg)
                             | otherwise -> do
                                 let note = "child " ++ mn ++ ": no file (the emitter stops: " ++ takeWhile (/= '\n') msg
                                            ++ "); an assumed leaf in its parents"
                                 _ <- evaluate (length note)
                                 st' <- record mn (status (Just "the emitter stops") []) st
                                 go st' files (row : rows) (note : notes) rest
                    Right (em, muts) -> do
                        onFile (HierFile file em muts isTop)
                        st' <- record mn (status (emOff em) (emAssume em)) st
                        go st' (S.insert file files) (row : rows) notes rest
