-- | Child kinds as the modular file models them (model.py): the library
-- primitives with Lean models, the assumed leaves, and the gate's generic kinds.
module Lean.Sched.Kinds
    ( PM(..), pmRepr, KindDef(..), kdCode, kdFx, Sched, schedMap, schedRel, ofSched, makeKinds, kindOrder, kindOf, regTypes, cfgTypes
    , genericKind, codedKind, schedCodes, ctrSched, noValueTasks, tasksKind, parseSched, schedNames, libSets, ifcTypes
    ) where

import ASyntax (APackage(..), AVInst(..), AType(..))
import Control.Exception (catch, IOException)
import Data.Array (Array)
import qualified Data.Array as A (listArray)
import Data.Array.Unboxed (UArray, (!), accumArray, listArray)
import Data.List (elemIndex, isInfixOf)
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Flags (Flags(..))
import Id (getIdString)
import PPrint (ppString)
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath ((</>))
import qualified Data.Map.Strict as M (Map, (!), findWithDefault, fromList, fromListWith, lookup, size)
import qualified Data.Set as S (Set, fromList, toList)

import Lean.Sched.Py (isPySpace, pyReprStr, pySplit, strip)

-- | A port of a kind: a method name, or a task statement's number.
data PM = PS String | PT Int
    deriving (Eq, Ord, Show)

pmRepr :: PM -> String
pmRepr (PS s) = pyReprStr s
pmRepr (PT k) = show k

-- | model.KindDef: ports in code order and the annotation, as the Lean interfaces.
data KindDef = KindDef
    { kdName :: String, kdIface :: Maybe String, kdMType :: Maybe String, kdPorts :: [PM], kdLPorts :: Maybe [String]
    , kdPrec, kdSbr, kdConf :: PM -> PM -> Bool
    , kdShare, kdHasEn, kdAlwaysRdy, kdHasArg :: PM -> Bool
    , kdPaths :: [(PM, PM)], kdBuilder, kdUsesc :: Maybe String, kdLib :: Bool
    , kdCodeRel :: Maybe (Int -> Int -> Bool, Int -> Int -> Bool, Int -> Int -> Bool)
      -- ^ prec, sbr and conf over port codes, where a kind is large (generic and synthesized kinds)
    , kdIx :: Maybe (M.Map PM Int)  -- ^ each port's code, where a kind is large
    , kdLPortA :: Maybe (Array Int String)  -- ^ kdLPorts by code, where a kind is large
    }

kdCode :: KindDef -> PM -> Int
kdCode k p = fromMaybe (error ("schedlean: no port " ++ show p ++ " in kind " ++ kdName k))
    (maybe (elemIndex p (kdPorts k)) (M.lookup p) (kdIx k))

-- | An argument-less value port: every call carries z.
kdFx :: KindDef -> PM -> Bool
kdFx k p = kdShare k p && not (kdHasArg k p)

-- | bsc's schedule statements, as design.parse_sched reads the printed annotation.
type Sched = [([String], String, [String])]

-- | Each related pair's relation, from the first statement naming it (built once per kind).
schedMap :: Sched -> M.Map (String, String) String
schedMap sl = M.fromListWith (\_ old -> old) [ ((p, q), r) | (ps, r, qs) <- sl, p <- ps, q <- qs ]

schedRel :: M.Map (String, String) String -> String -> String -> Maybe String
schedRel sm p q = M.lookup (p, q) sm

ofSched :: Sched -> (PM -> PM -> Bool, PM -> PM -> Bool, PM -> PM -> Bool)
ofSched sl = (prec, sbr, conf)
  where
    sm = schedMap sl
    rel (PS p) (PS q) = schedRel sm p q
    rel _ _ = Nothing
    prec p q = p /= q && rel p q `elem` [Just "SB", Just "SBR"]
    sbr p q = rel p q == Just "SBR"
    conf p q = let a = rel p q; b = rel q p in a == Just "C" || b == Just "C" || (a == Nothing && b == Nothing)

-- | design.parse_sched on the text inside the child line's brackets.
parseSched :: String -> Sched
parseSched s = [ x | item <- splitTop s, Just x <- [one item] ]
  where
    splitTop str = [ strip x | x <- go (0 :: Int) "" str, not (null (strip x)) ]
      where go _ cur [] = [reverse cur]
            go d cur (c : r)
                | c == ',' && d == 0 = reverse cur : go d "" r
                | otherwise = go d' (c : cur) r
              where d' | c `elem` "[(" = d + 1
                       | c `elem` "])" = d - 1
                       | otherwise = d
    one item = do
        (g1, r1) <- grp item
        r2 <- spaces r1
        let (rel, r3) = break isPySpace r2
        if rel `elem` ["CF", "SB", "SBR", "C"] then Just () else Nothing
        r4 <- spaces r3
        (g3, r5) <- grp r4
        if null r5 then Just (schedNames g1, rel, schedNames g3) else Nothing
    spaces str = let (a, b) = span isPySpace str in if null a then Nothing else Just b
    grp str = case str of
        ('[' : r) | (a, ']' : b) <- break (== ']') r -> Just ('[' : a ++ "]", b)
        _ -> let (a, b) = break isPySpace str in if null a then Nothing else Just (a, b)

schedNames :: String -> [String]
schedNames t0 = case strip t0 of
    ('[' : r) -> [ strip x | x <- pySplit "," (init r), not (null (strip x)) ]
    t -> [t]

flPorts, f2Ports :: [String]
flPorts = ["enq", "deq", "first", "notFull", "i_notFull", "notEmpty", "i_notEmpty", "clear"]
f2Ports = ["enq", "deq", "first", "notEmpty", "notFull", "i_notEmpty", "i_notFull", "clear"]

rfCopies :: Int
rfCopies = 5

mkKind :: String -> Maybe String -> Maybe String -> [PM] -> Maybe [String] -> (PM -> PM -> Bool) -> (PM -> PM -> Bool)
       -> (PM -> PM -> Bool) -> (PM -> Bool) -> (PM -> Bool) -> (PM -> Bool) -> [(PM, PM)] -> Maybe String -> Maybe String
       -> Bool -> Maybe (PM -> Bool) -> KindDef
mkKind n i mt ps lps prec sbr conf share en rdy paths b u lib ha =
    KindDef n i mt ps lps prec sbr conf share en rdy (fromMaybe dflt ha) paths b u lib Nothing Nothing Nothing
  where dflt p = en p && p `notElem` [PS "deq", PS "clear"]

inS :: [String] -> PM -> Bool
inS xs (PS p) = p `elem` xs
inS _ _ = False

pairIn :: [(String, String)] -> PM -> PM -> Bool
pairIn xs (PS p) (PS q) = (p, q) `elem` xs
pairIn _ _ _ = False

-- | model.make_kinds, with the design's FIFOL1 and ConfigRegN schedules.
makeKinds :: Sched -> Sched -> M.Map String KindDef
makeKinds fifoSched cfgSched = M.fromList [ (kdName k, k) | k <- ks ]
  where
    isW = (== PS "write")
    sps = map PS
    pre q = Just . map (q ++)
    (pc, sc, cc) = ofSched cfgSched
    (pf, sf, cf) = ofSched fifoSched
    f2prec = pairIn [("first", "deq"), ("first", "clear"), ("notEmpty", "deq"), ("notEmpty", "enq"), ("notEmpty", "clear")
                    , ("notFull", "deq"), ("notFull", "enq"), ("notFull", "clear"), ("deq", "clear"), ("enq", "clear")]
    f2sbr = pairIn [("deq", "clear"), ("enq", "clear"), ("clear", "clear")]
    f2conf = pairIn [("enq", "enq"), ("deq", "deq")]
    f2share = not . inS ["enq", "deq", "clear"]
    f2en = inS ["enq", "deq", "clear"]
    f2rdy = not . inS ["enq", "deq", "first"]
    rfp = "upd" : [ 's' : show c | c <- [0 .. rfCopies - 1] ]
    isS = (/= PS "upd")
    w3 = ["wset", "wget", "whas"]
    rw n sbrRead multi =
        mkKind n (Just ("RWire.X " ++ pyBool sbrRead ++ " " ++ pyBool multi)) (Just "WireM") (sps w3) (pre "WireM." w3)
               (\p q -> p == PS "wset" && q /= PS "wset")
               (\p q -> (sbrRead && ((p == PS "wset" && q /= PS "wset") || (q == PS "wset" && p /= PS "wset")))
                        || (multi && p == PS "wset" && q == PS "wset"))
               (\p q -> not multi && p == PS "wset" && q == PS "wset")
               (/= PS "wset") (== PS "wset") (const True) [(PS "wset", PS "wget"), (PS "wset", PS "whas")]
               (Just "wireW") (Just "wireU") True Nothing
    w0 = ["wset", "whas"]
    rw0 n sbrRead multi =
        mkKind n (Just ("RWire0.X " ++ pyBool sbrRead ++ " " ++ pyBool multi)) (Just "Wire0M") (sps w0) (pre "Wire0M." w0)
               (\p q -> p == PS "wset" && q == PS "whas")
               (\p q -> (sbrRead && p /= q) || (multi && p == PS "wset" && q == PS "wset"))
               (\p q -> not multi && p == PS "wset" && q == PS "wset")
               (== PS "whas") (== PS "wset") (const True) [(PS "wset", PS "whas")]
               (Just "wire0W") (Just "wire0U") True (Just (const False))
    pyBool b = if b then "true" else "false"
    ks =
        [ mkKind "reg" (Just "Reg.X") (Just "CRegM 1") (sps ["read", "write"]) (Just ["Reg.read", "Reg.write"])
                 (\p q -> not (isW p) && isW q) (\p q -> isW p && isW q) (\_ _ -> False)
                 (not . isW) isW (const True) [] (Just "regW") (Just "regU") True Nothing
        , mkKind "cfg" (Just "Cfg.X") (Just "CfgM") (sps ["read", "write"]) (Just ["CfgM.read", "CfgM.write"]) pc sc cc
                 (== PS "read") (== PS "write") (const True) [] (Just "cfgW") (Just "cfgU") False Nothing
        , mkKind "fifo" (Just "FL.X") (Just "FL") (sps flPorts) (pre "FL." flPorts) pf sf cf
                 (not . inS ["enq", "deq", "clear"]) (inS ["enq", "deq", "clear"]) (not . inS ["enq", "deq", "first"])
                 [ (PS a, PS b) | (a, b) <- [("deq", "enq"), ("deq", "notFull"), ("deq", "i_notFull"), ("deq", "clear"), ("enq", "clear")] ]
                 (Just "fifoW") (Just "fifoU") False Nothing
        , mkKind "f2" (Just "FIFO2.X") (Just "FifoM") (sps f2Ports) (pre "FifoM." f2Ports) f2prec f2sbr f2conf
                 f2share f2en f2rdy [] (Just "f2W") (Just "f2U") True Nothing
        , mkKind "sf" (Just "FIFO2.X") (Just "FifoM") (sps f2Ports) (pre "FifoM." f2Ports) f2prec f2sbr f2conf
                 f2share f2en f2rdy [] (Just "f2W") (Just "f2U") False Nothing
        , mkKind "rf" (Just "RF.X") (Just "RFM") (sps rfp) (pre "RFM." rfp)
                 (\p q -> isS p && q == PS "upd") (\_ _ -> False) (\p q -> p == PS "upd" && q == PS "upd")
                 isS (== PS "upd") (const True) [ (PS ('s' : show c), PS ('s' : show c)) | c <- [0 .. rfCopies - 1] ]
                 (Just "rfW") (Just "rfU") False (Just (const True))
        , rw "rwS" True True, rw "rwC" True False, rw0 "rw0S" True True, rw0 "rw0C" True False ]

kindOrder :: [String]
kindOrder = ["reg", "cfg", "fifo", "f2", "sf", "rf", "rwS", "rwC", "rw0S", "rw0C", "tasks"]

-- | ConfigReg's Verilog modules: one Lean model (outside reset ConfigRegA behaves as ConfigRegN).
-- | bsc's register and ConfigReg primitives (Verilog modules), the kinds reg and cfg.
regTypes, cfgTypes :: [String]
regTypes = ["RegN", "RegUN", "RegA"]
cfgTypes = ["ConfigRegN", "ConfigRegUN", "ConfigRegA"]

-- | model.kind_of: the library primitive a child's Verilog module is (RegA, ConfigRegA: their
-- synchronous models, outside reset; FIFOs select async reset by a Verilog define, not by name).
kindOf :: String -> String -> Either String String
kindOf typ ss
    | typ `elem` regTypes = Right "reg"
    | typ `elem` cfgTypes = Right "cfg"
    | typ == "FIFOL1" = Right "fifo"
    | typ == "FIFO2" = Right "f2"
    | typ == "SizedFIFO" = Right "sf"
    | typ == "RegFile" = Right "rf"
    | typ == "RWire" = Right (if "wset SBR [wget, whas, wset]" `isInfixOf` ss then "rwS" else "rwC")
    | typ == "RWire0" = Right (if "wset SBR [whas, wset]" `isInfixOf` ss then "rw0S" else "rw0C")
    | otherwise = Left ("unsupported child type " ++ typ)

-- | LastCaller.ctrAnn, Counter's corrected schedule (Ravi's decision).
ctrSched :: Sched
ctrSched = [ (["value"], "SB", ["addA", "addB", "setC", "setF"]), (["setC"], "SB", ["addA", "addB"])
           , (["addA", "addB", "setC", "setF"], "SBR", ["setF"]), (["addA"], "C", ["addA"]), (["addB"], "C", ["addB"])
           , (["value"], "CF", ["value"]), (["addA"], "CF", ["addB"]), (["addB"], "CF", ["addA"]), (["setC"], "CF", ["setC"]) ]

-- | model.generic_kind (the gate only): every method and copy a port, from the
-- declared schedule; a synthesized child's action methods C with themselves.
genericKind :: String -> String -> Sched -> [(String, String, String, Bool)] -> Bool -> Bool -> KindDef
genericKind name typ sl0 meths synth lib =
    (mkKind name Nothing Nothing [ PS p | p <- ports ] Nothing prec sbr conf (not . act) act (const True) [] Nothing Nothing False
            (Just hasArg)) { kdCodeRel = Just (precC, sbrC, confC), kdIx = Just (M.fromList (zip (map PS ports) [0 ..])) }
  where
    ports = [ p | (p, _, _, _) <- meths ]
    nP = length ports
    sl = if typ == "Counter" && lib && not synth then ctrSched else sl0
    -- ports and their methods by number, and the relation of each method pair (first statement wins)
    portIx = M.fromList (zip ports [0 :: Int ..])
    baseIx = M.fromList (zip (S.toList (S.fromList [ b | (_, b, _, _) <- meths ])) [0 :: Int ..])
    nB = M.size baseIx
    bOf = listArray (0, nP - 1) [ baseIx M.! b | (_, b, _, _) <- meths ] :: UArray Int Int
    actA = listArray (0, nP - 1) [ k /= "value" | (_, _, k, _) <- meths ] :: UArray Int Bool
    hargA = listArray (0, nP - 1) [ h | (_, _, _, h) <- meths ] :: UArray Int Bool
    relA = accumArray (\old new -> if old == 0 then new else old) 0 (0, nB * nB - 1)
               [ (a * nB + b, relCode r) | (ps, r, qs) <- sl, Just a <- map (`M.lookup` baseIx) ps
                                         , Just b <- map (`M.lookup` baseIx) qs ] :: UArray Int Word8
    rel p q = relA ! (bOf ! p * nB + bOf ! q)
    precC p q = p /= q && bOf ! p /= bOf ! q && rel p q `elem` [2, 3]
    sbrC p q
        | bOf ! p == bOf ! q && (p /= q || (synth && actA ! p)) = p == q && not synth && rel p q == 3
        | otherwise = rel p q == 3
    confC p q
        | bOf ! p == bOf ! q && p /= q = False
        | p == q && synth && actA ! p = True
        | otherwise = let a = rel p q; b = rel q p in a == 4 || b == 4 || (a == 0 && b == 0)
    ix p = M.findWithDefault (error ("schedlean: no port " ++ p ++ " in kind " ++ name)) p portIx
    onPS f (PS p) (PS q) = f (ix p) (ix q)
    onPS _ _ _ = False
    prec = onPS precC
    sbr = onPS sbrC
    conf = onPS confC
    act (PS p) = actA ! ix p
    act _ = False
    hasArg (PS p) = hargA ! ix p
    hasArg _ = False

-- | A kind over its ports' codes (0 to n - 1): prec, sbr and conf by code pair; share, hasEn,
-- alwaysRdy and hasArg by code; the queries by port through one index (a port not in it: False).
codedKind :: String -> Maybe String -> Maybe String -> [PM] -> Maybe [String] -> (Int -> Int -> Bool, Int -> Int -> Bool, Int -> Int -> Bool)
          -> (Int -> Bool, Int -> Bool, Int -> Bool, Int -> Bool) -> KindDef
codedKind name iface mty ports lps rels@(precC, sbrC, confC) (shareC, enC, rdyC, argC) = KindDef
    { kdName = name, kdIface = iface, kdMType = mty, kdPorts = ports, kdLPorts = lps
    , kdPrec = on precC, kdSbr = on sbrC, kdConf = on confC
    , kdShare = one shareA, kdHasEn = one enA, kdAlwaysRdy = one rdyA, kdHasArg = one argA
    , kdPaths = [], kdBuilder = Nothing, kdUsesc = Nothing, kdLib = False, kdCodeRel = Just rels, kdIx = Just ix
    , kdLPortA = fmap (A.listArray (0, n - 1)) lps }
  where
    n = length ports
    ix = M.fromList (zip ports [0 ..])
    arrOf f = listArray (0, n - 1) (map f [0 .. n - 1]) :: UArray Int Bool
    (shareA, enA, rdyA, argA) = (arrOf shareC, arrOf enC, arrOf rdyC, arrOf argC)
    on f p q = case (M.lookup p ix, M.lookup q ix) of
        (Just a, Just b) -> f a b
        _ -> False
    one a p = maybe False (a !) (M.lookup p ix)

-- | bsc's schedule statements as relation codes by port pair (row-major over @ports@; 0 none, 1 CF,
-- 2 SB, 3 SBR, 4 C, 5 other), the first statement naming a pair winning (as 'schedMap').
schedCodes :: [String] -> Sched -> UArray Int Word8
schedCodes ports sl = accumArray (\old new -> if old == 0 then new else old) 0 (0, n * n - 1)
    [ (a * n + b, relCode r) | (ps, r, qs) <- sl, Just a <- map (`M.lookup` ix) ps, Just b <- map (`M.lookup` ix) qs ]
  where
    n = length ports
    ix = M.fromList (zip ports [0 :: Int ..])

relCode :: String -> Word8
relCode r = case r of { "CF" -> 1; "SB" -> 2; "SBR" -> 3; "C" -> 4; _ -> 5 }

-- | The task statements' kind: one port per statement, in rank order.
tasksKind :: Int -> KindDef
tasksKind nT = mkKind "tasks" (Just "Tasks.X nT") (Just "Fin nT") [ PT t | t <- [0 .. nT - 1] ] Nothing
                      lt (\_ _ -> False) (==) (const False) (const True) (const True) [] Nothing Nothing True (Just (const True))
  where lt (PT a) (PT b) = a < b
        lt _ _ = False

-- | System tasks that return nothing: the only task calls a method may make.
noValueTasks :: S.Set String
noValueTasks = S.fromList ([ '$' : t ++ sfx | t <- ["display", "write", "fdisplay", "fwrite"], sfx <- ["", "b", "h", "o"] ]
    ++ [ "$finish", "$stop", "$fflush", "$fclose", "$dumpvars", "$dumpon", "$dumpoff", "$dumpfile", "$dumpall"
       , "$dumpflush", "$error", "$warning", "$info", "$fatal", "_finish_", "_stop_" ])

-- | Each instance's interface type name (the APackage printout's word after ABSTRACT:).
ifcTypes :: APackage -> [(String, String)]
ifcTypes apkg = [ (getIdString (avi_vname v), ppString t) | v <- apkg_state_instances apkg, ATAbstract t _ <- [avi_type v] ]

-- | bsc's library Verilog modules and packages, from $BLUESPECDIR (else the .ba's).
libSets :: Flags -> IO (Either String (S.Set String, S.Set String))
libSets flags = do
    env <- lookupEnv "BLUESPECDIR"
    let dir = fromMaybe (bluespecDir flags) env
        ls d = listDirectory d `catch` (\e -> const (return []) (e :: IOException))
    vs <- ls (dir </> "Verilog")
    bos <- ls (dir </> "Libraries")
    return $ if null vs || null bos
        then Left ("no bsc library in " ++ dir ++ " (set BLUESPECDIR to bsc's lib directory)")
        else Right ( S.fromList [ take (length f - 2) f | f <- vs, endsWith f ".v" ]
                   , S.fromList [ take (length f - 3) f | f <- bos, endsWith f ".bo" ] )
  where endsWith f sfx = reverse sfx == take (length sfx) (reverse f)
