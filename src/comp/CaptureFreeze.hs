-- Freezing the live capture (CaptureLive) into the .ba's record (Capture).
--
-- Runs inside the .ba encode, as the last field, after the APackage it
-- keys against has been written: every Id it turns into an ordinal or a
-- spelling is one of that package's (written above), or a name some stage
-- held, which that stage's dump forced.  It spells forced FStrings only
-- (getFString reads the intern table and adds nothing), remaps file names
-- with Position.remapPositionPath on Strings, and never calls mkFString,
-- mkId or legalizeNameInfo: derived spellings are the reader's.
module CaptureFreeze(freezeCapture) where

import qualified Data.Map.Strict as M
import Control.Monad.State.Strict(State, runState, state, get, put)
import Data.Maybe(mapMaybe)
import Data.Char(isDigit)
import Data.List(foldl')

import FStringCompat(getFString, fstringId)
import Position(Position(..))
import Id(Id, getIdQual, getIdBase, getIdPosition, getIdString)
import IType(IType(..), tCode)
import ASyntax
import Capture
import CaptureLive
import ErrorUtil(internalError)

-- String table, file table and site table under construction
data B = B { b_strs  :: !(M.Map String Int), b_nstr :: !Int, b_strl :: [String]
           , b_files :: !(M.Map (Int, Bool) Int), b_nfile :: !Int, b_filel :: [CapFile]
           , b_sites :: !(M.Map (Int, Int, Int) Int), b_nsite :: !Int
           , b_sitel :: [(Int, Int, Int)]
           , b_types :: !(M.Map (Int, Integer) Int), b_ntype :: !Int, b_typel :: [CapType] }

type F = State B

-- mapM in strict State keeps a stack frame per element until the list
-- ends, and cap_inner reaches 169k entries; this runs in constant stack,
-- with the same effects in the same order.
{-# INLINE mapF #-}
mapF :: (a -> F b) -> [a] -> F [b]
mapF f xs0 = state (go [] xs0)
  where go acc [] s = (reverse acc, s)
        go acc (x : xs) s = case runState (f x) s of
                              (y, s') -> go (y : acc) xs s'

{-# INLINE forF #-}
forF :: [a] -> (a -> F b) -> F [b]
forF = flip mapF

str :: String -> F Int
str s = do
  b <- get
  case M.lookup s (b_strs b) of
    Just k -> return k
    Nothing -> do
      let k = b_nstr b + 1
      put b { b_strs = M.insert s k (b_strs b), b_nstr = k, b_strl = s : b_strl b }
      return k

-- the file as Bin Position would store it
file :: (Bool -> String -> String) -> Position -> F Int
file remap (Position f _ _ lib) = do
  b <- get
  let key = (fstringId f, lib)
  case M.lookup key (b_files b) of
    Just k -> return k
    Nothing -> do
      p <- str (remap lib (getFString f))
      b' <- get
      let k = b_nfile b' + 1
      put b' { b_files = M.insert key k (b_files b'), b_nfile = k,
               b_filel = CapFile p lib : b_filel b' }
      return k

site :: (Bool -> String -> String) -> Position -> F Int
site remap pos@(Position _ l c _)
  | l < 0 && c < 0 = return 0
  | otherwise = do
      f <- file remap pos
      b <- get
      let key = (f, l, c)
      case M.lookup key (b_sites b) of
        Just k -> return k
        Nothing -> do
          let k = b_nsite b + 1
          put b { b_sites = M.insert key k (b_sites b), b_nsite = k,
                  b_sitel = key : b_sitel b }
          return k

-- A type (an observation's: fully built), post-order, each node once.
-- Keyed by tCode, which reads strict fields (a large number by value);
-- spelled with getFString, which adds nothing to the intern table.
typeIx :: IType -> F Int
typeIx t = do
  let key = case tCode t of
              c | c >= 0 -> (c, 0)
              _ -> case t of
                     ITNum n -> (-1, n)
                     _ -> (-2, 0)
  b <- get
  case M.lookup key (b_types b) of
    Just k -> return k
    Nothing -> do
      node <- case t of
                ITAp f a -> do fi <- typeIx f
                               ai <- typeIx a
                               return (CTAp fi ai)
                ITCon i _ _ -> uncurry CTCon <$> idQB i
                ITVar i -> CTVar <$> str (getFString (getIdBase i))
                ITNum n -> return (CTNum n)
                ITStr fs -> CTStr <$> str (getFString fs)
                ITForAll i _ body -> do v <- str (getFString (getIdBase i))
                                        bi <- typeIx body
                                        return (CTForAll v bi)
      b' <- get
      let k = b_ntype b' + 1
      put b' { b_types = M.insert key k (b_types b'), b_ntype = k,
               b_typel = node : b_typel b' }
      return k

obsRec :: ObsL -> F ObsRec
obsRec (ObsL k tier ty ty2 lo w src l n) = do
  a <- capTy ty
  b <- capTy ty2
  return (ObsRec k tier a b lo w src l n)
 where capTy NoTy = return 0
       capTy (Ty t) = typeIx t

-- a record's facts: its names and its observations
splitFacts :: [NameFact] -> ([NameFact], [ObsL])
splitFacts fs = ([ f | f@(NameFact {}) <- fs ], [ o | ObsFact o <- fs ])

idQB :: Id -> F (Int, Int)
idQB i = do
  q <- str (getFString (getIdQual i))
  n <- str (getFString (getIdBase i))
  return (q, n)

-- An unqualified "__h<p>" (an unnamed cell's def, IExpand.pDef) or
-- "__i<n>"/"__j<n>" (an ITransform CSE def) is stored in the form the
-- reader spells back to the same string, so these unique names cost no
-- string-table entry.
idName :: Id -> F NameRef
idName i =
  case (getFString (getIdQual i), getFString (getIdBase i)) of
    ("", '_' : '_' : c : ds)
      | c == 'h', decimal ds -> return (NHeap (number ds) Nothing)
      | (c == 'i' || c == 'j'), decimal ds -> return (NGen (fromEnum c) (number ds))
    _ -> uncurry NId <$> idQB i
  where -- as show prints a non-negative Int
        decimal ds = not (null ds) && length ds < 19 && all isDigit ds &&
                     (ds == "0" || head ds /= '0')
        number = foldl' (\n d -> n * 10 + (fromEnum d - fromEnum '0')) 0

freezeCapture :: Int -> (Bool -> String -> String) -> [(Id, String)] ->
                 APackage -> Maybe NetLive -> Capture
freezeCapture mode remap imports apkg mnet =
  let b0 = B M.empty 0 [] M.empty 0 [] M.empty 0 [] M.empty 0 []
      (c, b) = runState (build mode remap imports apkg mnet) b0
      (fs, ls, cs) = unzip3 (reverse (b_sitel b))
  in  c { cap_strings = reverse (b_strl b),
          cap_files = reverse (b_filel b),
          cap_sites = CapSites fs ls cs,
          cap_types = reverse (b_typel b) }

build :: Int -> (Bool -> String -> String) -> [(Id, String)] -> APackage ->
         Maybe NetLive -> F Capture
build mode remap imports apkg mnet = do
  let -- the stored package's items, by Id
      defIx = M.fromList [ (i, DRDef k) | (k, ADef i _ _ _) <- zip [0..] (apkg_local_defs apkg) ]
      inputIx = M.fromList (concat [ inputIds k a | (k, a) <- zip [0..] (apkg_inputs apkg) ])
      inputIds k (AAI_Port (i, _)) = [(i, DRInput k)]
      inputIds k (AAI_Clock o mg) = (o, DRInput k) : [ (g, DRInput k) | Just g <- [mg] ]
      inputIds k (AAI_Reset r) = [(r, DRInput k)]
      inputIds k (AAI_Inout r _) = [(r, DRInput k)]
      instIx = M.fromList [ (avi_vname v, DRInst k (-1))
                          | (k, v) <- zip [0..] (apkg_state_instances apkg) ]
      ifcIx = M.fromList (concat [ (aif_name f, DRIfc k 0) :
                                   [ (i, DRIfc k j)
                                   | (j, (i, _)) <- zip [1..] (concat (ifcInputs f)) ]
                                 | (k, f) <- zip [0..] (apkg_interface apkg) ])
      ifcInputs (AIDef { aif_inputs = is }) = is
      ifcInputs (AIAction { aif_inputs = is }) = is
      ifcInputs (AIActionValue { aif_inputs = is }) = is
      ifcInputs _ = []
      ruleIx = M.fromList [ (arule_id r, DRRule k) | (k, r) <- zip [0..] (apkg_rules apkg) ]
      instOrd i = case M.lookup i instIx of
                    Just (DRInst k _) -> k
                    _ -> -1
      -- the earlier tables win: a def named like an input is the def
      resolve i = case M.lookup i defIx of
                    Just r -> Just r
                    Nothing -> case M.lookup i inputIx of
                      Just r -> Just r
                      Nothing -> case M.lookup i instIx of
                        Just r -> Just r
                        Nothing -> case M.lookup i ifcIx of
                          Just r -> Just r
                          Nothing -> M.lookup i ruleIx
      tbl = capOf (apkg_capture apkg)
  pkgs <- forF imports $ \(i, h) -> do
            n <- str (getIdString i)
            hs <- str h
            return (CapPackage n hs)
  -- apkg space
  let attached = [ (r, ns) | (i, ns) <- capRecs tbl, Just r <- [resolve i] ]
      unattached = [ (i, ns) | (i, ns) <- capRecs tbl, Nothing <- [resolve i] ]
  defs <- forF (M.toList (M.fromListWith (flip (++)) attached)) $ \(r, fs) -> do
            let (ns, os) = splitFacts fs
            ns' <- mapF (nameRec remap) ns
            os' <- mapF obsRec os
            return (DefRec r ns' os')
  let gone = reverse (ct_gone tbl)
      goneIx = M.fromListWith (\_ old -> old)
                 [ (i, k) | (k, Gone { g_names = (NameFact (DefName i) _ _ : _) })
                              <- zip [1..] gone ]
      owner = ownerOf tbl gone
      hostOf (HLId h) = hostId (3 :: Int) h
      hostOf (HLInst h a) = case M.lookup h instIx of
                              Just (DRInst k _) -> return (HDef (DRInst k a))
                              _ -> HName <$> idName h
      -- a host merged into another def later is that def's
      hostId n h = case resolve h of
                     Just r -> return (HDef r)
                     Nothing -> case M.lookup h goneIx of
                       Just k -> return (HInner k)
                       Nothing -> case M.lookup h owner of
                         Just (Left k) | n > 0 && k /= h -> hostId (n - 1) k
                         Just (Right k) -> return (HInner k)
                         _ -> HName <$> idName h
  inners <- forF gone $ \g -> goneRec remap hostOf g
  orphans <- forF unattached $ \(i, fs) -> do
               let (ns, os) = splitFacts fs
               self <- nameRec remap (NameFact (DefName i) viaSelf 0)
               ns' <- mapF (nameRec remap) ns
               os' <- mapF obsRec os
               return (InnerRec innerKindUnattached viaSelf 0 (self : ns') [] os')
  net <- case mnet of
           Nothing -> return Nothing
           Just nl -> Just <$> freezeNet remap resolve instOrd nl
  let stats = M.toList (ct_stats tbl)
      stats' = if perturbing ptHeader then zip (map fst stats) (rotate (map snd stats))
               else stats
  return Capture { cap_version = capVersion, cap_mode = mode,
                   cap_strings = [], cap_files = [], cap_packages = pkgs,
                   cap_sites = CapSites [] [] [], cap_types = [],
                   cap_defs = defs, cap_inner = inners ++ orphans,
                   cap_stats = stats', cap_net = net }

nameRec :: (Bool -> String -> String) -> NameFact -> F NameRec
nameRec _ (ObsFact _) = internalError "CaptureFreeze.nameRec: an observation"
nameRec _ (LinkFact _) = internalError "CaptureFreeze.nameRec: a link"
nameRec remap (NameFact ref via stage) = do
  (n, s) <- case ref of
              DefName i -> do n <- idName i
                              s <- site remap (getIdPosition i)
                              return (n, s)
              HeapName p Nothing -> return (NHeap p Nothing, 0)
              HeapName p (Just i) -> do qb <- idQB i
                                        s <- site remap (getIdPosition i)
                                        return (NHeap p (Just qb), s)
              CseNo c k -> return (NGen (fromEnum c) k, 0)
  return (NameRec n via stage s)

-- For each def merged into another's record: that record's owner (Left),
-- or the inner record it left with (Right, 1-based in gone order).  Copy
-- and creation facts name defs that were not merged.
ownerOf :: CapTable -> [Gone] -> M.Map Id (Either Id Int)
ownerOf tbl gone =
    M.fromListWith (\_ old -> old)
      ([ (d, Left k) | (k, fs) <- capRecs tbl, d <- merged fs ] ++
       [ (d, Right g) | (g, Gone { g_names = (_ : fs) }) <- zip [1..] gone, d <- merged fs ])
  where merged fs = [ d | NameFact (DefName d) via _ <- fs,
                          via /= viaCopy, via /= viaCreated ]

goneRec :: (Bool -> String -> String) -> (HostL -> F Host) -> Gone -> F InnerRec
goneRec remap hostOf (Gone kind via stage fs hs) = do
  let (ns, os) = splitFacts fs
  ns' <- mapF (nameRec remap) ns
  hs' <- mapF hostOf hs
  os' <- mapF obsRec os
  return (InnerRec kind via stage ns' hs' os')

-- Net space: the ledger's names, each with the apkg items and later names
-- it stands for, the net-space records, and the boundary links on both.
freezeNet :: (Bool -> String -> String) -> (Id -> Maybe DefRef) -> (Id -> Int) ->
             NetLive -> F NetCap
freezeNet remap resolve instOrd (NetLive names tbl) = do
  let netIx = M.fromListWith (\_ old -> old)
                [ (i, k) | (k, nl) <- zip [1..] names, i <- nnl_ids nl ]
      origin (NameFact (DefName d) via _) =
        case resolve d of
          Just r -> return (OApkg r via)
          Nothing -> do n <- idName d
                        return (OName n via)
      origin f = do NameRec n _ _ _ <- nameRec remap f
                    return (OName n (nf_via f))
  ns <- forF names $ \(NetNameL v ids froms) -> do
          vs <- str v
          own <- forF ids $ \i ->
                   case resolve i of
                     Just r -> return [OApkg r viaSelf]
                     Nothing -> return []
          merged <- mapF origin (filter isNameFact (concat [ capNamesOf i tbl | i <- ids ]))
          fs <- forF froms $ \f -> case f of
                  NFInout s -> OInout <$> str s
                  NFSvStd s -> OSvStd <$> str s
                  NFDollar i -> ODollar <$> str (getIdString i)
          return (NetName vs (concat own ++ merged ++ fs))
  let gone = reverse (ct_gone tbl)
      goneIx = M.fromListWith (\_ old -> old)
                 [ (i, k) | (k, Gone { g_names = (NameFact (DefName i) _ _ : _) })
                              <- zip [1..] gone ]
      owner = ownerOf tbl gone
      hostOf (HLId h) = hostId (3 :: Int) h
      hostOf (HLInst h _) = HName <$> idName h
      -- a host merged into another def later is that def's
      hostId n h = case M.lookup h netIx of
                     Just k -> return (HNet k)
                     Nothing -> case M.lookup h goneIx of
                       Just k -> return (HInner k)
                       Nothing -> case M.lookup h owner of
                         Just (Left k) | n > 0 && k /= h -> hostId (n - 1) k
                         Just (Right k) -> return (HInner k)
                         _ -> case resolve h of
                                Just r -> return (HDef r)
                                Nothing -> HName <$> idName h
  inners <- forF gone $ \g -> goneRec remap hostOf g
  -- records of names no identifier stands for
  let unattached = [ (i, fs) | (i, fs) <- capRecs tbl, not (M.member i netIx) ]
  orphans <- forF unattached $ \(i, fs) -> do
               let (ns, os) = splitFacts fs
               self <- nameRec remap (NameFact (DefName i) viaSelf 0)
               ns' <- mapF (nameRec remap) ns
               os' <- mapF obsRec os
               return (InnerRec innerKindUnattached viaSelf 0 (self : ns') [] os')
  let dropped = mapMaybe (\g -> case g of
                            Gone { g_kind = k, g_stage = s,
                                   g_names = (NameFact (DefName d) _ _ : _) }
                              | k == kindDead -> fmap (\r -> (r, s)) (resolve d)
                            _ -> Nothing) gone
      -- a net's links, then those of the records in nc_inner (minus their
      -- entry), in that order
      linked = [ (k, l) | (k, nl) <- zip [1 :: Int ..] names, i <- nnl_ids nl,
                          LinkFact l <- capNamesOf i tbl ] ++
               [ (negate k, l) | (k, fs) <- zip [1 :: Int ..]
                                              (map g_names gone ++ map snd unattached),
                                 LinkFact l <- fs ]
  links <- forF linked $ \(d, LinkL r n o mm c a p) -> do
             m <- case mm of
                    Nothing -> return 0
                    Just x -> str (getFString (getIdBase x))
             return (NetLink d (instOrd o) m a p r n c)
  return (NetCap ns (inners ++ orphans) dropped links)
