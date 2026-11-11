{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenTci(TciFile(..), TciInput(..), TciStats(..),
              genTciFile, readTciFile, readTciLibraries) where

-- The .tci file: the typecheck input that -signature-only hands to
-- -check-only.  It holds the package after context reduction (mctx), its
-- imported MatX signatures pruned to what a lookup from the package can
-- reach, a reference to each library signature, and the values that the
-- check cannot compute without the steps before context reduction.
-- -check-only X.tci reads nothing else but the referenced library .bi
-- files of the installation (see bsc.hs, checkTci).  It carries no hashes
-- of the MatX import closure, so it changes only when what the check
-- reads changes.  Library signatures are whole (the check inserts
-- references to library names) and never change under a MatX edit.
--
-- The file is the tag and three format 2 containers, each but the last
-- preceded by its length (u64, little-endian):
--   1. strings in the writer's intern order: every string of the other
--      two and of the referenced library .bi files, and every string the
--      writer had interned when it reached the handoff except those only
--      the pruned entries hold (the compiler's own strings, PreStrings,
--      are always kept).  The reader interns a container's string table
--      eagerly and in table order, so these keep their relative order,
--      including the ones the check makes again before typecheck
--      (instance names, the built-in classes, kind variables), and
--      reading a library .bi afterwards interns nothing new;
--   2. TciLocal: mctx (imported signatures as references), the hash of
--      each library's .bi, and the rest;
--   3. the imported MatX signatures, pruned, in closure order.
-- Imported positions are stored with only their file (see genTciFile),
-- except in imported class and field default bodies, which the check
-- typechecks in the package's scope.

import Control.Monad(when, zipWithM, foldM)
import Control.Exception(evaluate)
import Data.Maybe(isNothing, fromMaybe)
import Data.Bits(shiftL)
import Data.Char(isDigit)
import Data.List(sort, foldl')
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Builder as BB
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word(Word64)

import FStringCompat(FString, fstringId, headFString, allFString,
                     foldrFString, isSuffixOfFString)
import Position(Position(..), noPosition)
import Id
import CType
import CSyntax
import Pragma(Pragma(..))
import PFPrint(pfpString)
import Error(ErrMsg(..), ErrorHandle, bsError)
import ErrorUtil(internalError)
import BinData
import GenBin(CPackageBin(..))
import GenBi(BiFile(..), readBiFile, readBiFileMaybe, readBiStrings)
import FileNameUtil(biSuffix)
import FileIOUtil(writeFileBuilderCatch, existsFilePath, readBinFoundFile)
import Util(hashInit, nextHashByte, showHash)
import SpeedyString(internCount)
import PreStrings(allPreStrings, fsPolyWrapField, fs_read, fs_write, fs_if,
                  fsPrimSelectFn, fsPrimUpdateFn, fsPrimWriteFn)

-- .tci file tag -- change this whenever the .tci format changes
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-tci-20261110-2"

headerBS :: B.ByteString
headerBS = B.pack header

-- The package and the values that cross the handoff, as read back
data TciFile = TciFile
    { tci_source       :: String   -- the source file, as the writer encoded it
    , tci_flags        :: String   -- fingerprint of the semantic flags
      -- mctx, with the pruned signatures (file name "", no import list);
      -- a library's has no entries until readTciLibraries reads them
    , tci_package      :: CPackage
      -- per signature: Just the hash of the library .bi it refers to
    , tci_libs         :: [Maybe String]
    , tci_used_types   :: S.Set Id -- packages used in types (before ctxreduce)
    , tci_used_ctx     :: S.Set Id -- packages used by context reduction
    , tci_used_exports :: S.Set Id -- packages used by the export list
    , tci_atf          :: M.Map (Id, [Type]) Type  -- ctxreduce's ATF cache
    }

-- What the writer is given
data TciInput = TciInput
    { ti_source    :: String
    , ti_flags     :: String
    , ti_package   :: CPackage  -- mctx, with the full imported signatures
    , ti_bi_hashes :: M.Map String String  -- closure package -> hash of its .bi
    , ti_used      :: (S.Set Id, S.Set Id, S.Set Id)  -- types, ctxreduce, exports
    , ti_atf       :: M.Map (Id, [Type]) Type
      -- every string interned when the package reached the handoff (after
      -- the table that follows context reduction)
    , ti_handoff_strings :: [FString]
    , ti_full      :: Bool  -- keep every imported signature whole
    , ti_keep_pos  :: Bool  -- keep imported positions
    }

data TciStats = TciStats
    { ts_entries    :: (Int, Int)  -- imported non-library entries: kept, all
    , ts_lib        :: (Int, Int)  -- library signatures, their entries
    , ts_names      :: Int         -- names in the keep set
    , ts_strings    :: Int
    , ts_bytes      :: (Int, Int, Int)  -- strings, local part, signatures
    , ts_stripped   :: Int         -- files whose positions were reduced
    }

-- The local part of the file
data TciLocal = TciLocal String String CPackageBin [Maybe String]
                         [Id] [Id] [Id] [((Id, [Type]), Type)]

instance Bin TciLocal where
    writeBytes (TciLocal src fl pkg libs ut uc ue atf) = section "TciLocal" $ do
        toBin src; toBin fl; toBin pkg; toBin libs
        toBin ut; toBin uc; toBin ue; toBin atf
    readBytes = do src <- fromBin; fl <- fromBin; pkg <- fromBin; libs <- fromBin
                   ut <- fromBin; uc <- fromBin; ue <- fromBin; atf <- fromBin
                   return (TciLocal src fl pkg libs ut uc ue atf)

-- An imported signature, written as Bin CSignature writes it, except
-- that default bodies keep their positions
newtype TciSig = TciSig CSignature

instance Bin TciSig where
    writeBytes (TciSig (CSignature n is fs ds)) =
        do toBin n; toBin is; toBin fs; toBin (map TciDefn ds)
    readBytes = TciSig <$> fromBin

newtype TciDefn = TciDefn CDefn

instance Bin TciDefn where
    writeBytes (TciDefn (Cstruct vis st ik is fs _)) =
        do putI 2; toBin vis; toBin st; toBin ik; toBin is; toBin (map TciField fs)
    writeBytes (TciDefn (Cclass incoh ps ik is deps ats fs)) =
        do putI 3; toBin incoh; toBin ps; toBin ik; toBin is; toBin deps; toBin ats
           toBin (map TciField fs)
    writeBytes (TciDefn d) = writeBytes d
    readBytes = TciDefn <$> fromBin

newtype TciField = TciField CField

instance Bin TciField where
    writeBytes (TciField (CField n ps cqt def_cs mot)) =
        do toBin n; toBin ps; toBin cqt; keepPositions (toBin def_cs); toBin mot
    readBytes = TciField <$> fromBin

-- -------------------------
-- Writing

genTciFile :: ErrorHandle -> String -> TciInput -> IO (TciStats, String)
genTciFile errh fn ti = do
    let CPackage pid exps imps sigs fixs ds incs = ti_package ti
        isLib = [ isLibrary sig | CImpSign _ _ sig <- sigs ]
        biHash n = case M.lookup (getIdString n) (ti_bi_hashes ti) of
                     Just h -> h
                     Nothing -> internalError ("genTciFile: no .bi hash: " ++ getIdString n)
        libs = [ if l then Just (biHash n) else Nothing
               | (l, CImpSign _ _ (CSignature n _ _ _)) <- zip isLib sigs ]
        -- the files of the closure's non-library packages: a position in
        -- one of them is an imported definition's (a package's own
        -- positions, including its include files, are kept)
        impFiles = IS.fromList [ fstringId (pos_file (getIdPosition n))
                               | (False, CImpSign _ _ (CSignature n _ _ _)) <- zip isLib sigs ]
        sel p = not (pos_is_stdlib p) && pos_line p >= 0 &&
                IS.member (fstringId (pos_file p)) impFiles
        msel = if ti_keep_pos ti then Nothing else Just sel
        (ut, uc, ue) = ti_used ti
        local = TciLocal (ti_source ti) (ti_flags ti)
                         (CPackageBin (CPackage pid exps imps sigs fixs ds incs))
                         libs (sortIds ut) (sortIds uc) (sortIds ue)
                         (M.toList (ti_atf ti))
    (bsL, strsL) <- encodeV2Strip msel local
    -- every string of the .bi files the signatures were read from (Sig has
    -- read them, so these are interned already): the check interns a
    -- library's when it reads it; a MatX file's that no kept entry holds
    -- are those only the pruned entries hold (its package name is kept)
    let foldBi lib k z = foldM (\a f -> do strs <- readBiStrings errh f
                                           return $! foldl' k a strs)
                               z [ f | (l, CImpSign f _ _) <- zip isLib sigs, l == lib ]
    strsLib <- foldBi True addNew IM.empty
    let exportAll = S.fromList [ getIdString p | Left es <- [exps], CExpPkg p <- es
                                               , p /= pid ]
    (sigs', stats) <-
        if ti_full ti
        then return ([ wholeSig s | (False, s) <- zip isLib sigs ], keepAllStats isLib sigs)
        else pruneSigs strsL exportAll (zip isLib sigs)
    (bsS, strsS) <- encodeV2Strip msel [ TciSig s | s <- sigs' ]
    let kept = foldl' (foldl' addNew) strsLib [strsL, strsS]
        -- a check makes the compiler's own strings itself (NumEq for a
        -- numeric proviso, say), and spellings of the shapes it generates;
        -- dropping one would move it after every string of the file
        -- instead of keeping Sig's rank
        builtins = IS.fromList (map fstringId allPreStrings)
        dropCand d x = let i = fstringId x
                       in  if IM.member i kept || IS.member i builtins || IS.member i d
                           then d else IS.insert i d
    -- the strings of the MatX files that are neither kept nor the compiler's
    cands <- if ti_full ti then return IS.empty else foldBi False dropCand IS.empty
    let onlyDropped x = IS.member (fstringId x) cands && not (generatedShape x)
        handoff = [ x | x <- ti_handoff_strings ti, not (onlyDropped x) ]
        strs = IM.elems (foldl' addNew kept handoff)
    bsT <- encodeV2Strings strs
    let lenLE b = BB.toLazyByteString (BB.word64LE (fromIntegral (BL.length b)))
        body = [lenLE bsT, bsT, lenLE bsL, bsL, bsS]
        hash = showHash (foldl' (BL.foldl' nextHashByte) hashInit body)
    writeFileBuilderCatch errh fn (BB.byteString headerBS <> foldMap BB.lazyByteString body)
    return (stats { ts_strings = length strs
                  , ts_bytes = (bl bsT, bl bsL, bl bsS)
                  , ts_stripped = if ti_keep_pos ti then 0 else IS.size impFiles },
            hash)
  where wholeSig (CImpSign _ _ (CSignature n _ fs ds)) = CSignature n [] fs ds
        bl = fromIntegral . BL.length
        -- an IntMap by id is the strings in Ord FString order
        addNew m x = let i = fstringId x
                     in  if IM.member i m then m else IM.insert i x m

-- The spellings of the names that the check generates (_tcdictN, x__,
-- x_AV for $swrite into x (TCheck.createIdNew), instance names with
-- dots, x~y, numbers), by their shape
generatedShape :: FString -> Bool
generatedShape fs =
    headFString fs == Just '_' || isSuffixOfFString "__" fs || isSuffixOfFString "_AV" fs ||
    (headFString fs /= Nothing && allFString isDigit fs) ||
    foldrFString (\c r -> c `elem` ".~" || r) False fs

-- A library's .bi is compiled with -stdlib-names, which marks every
-- position in it, the package name's included (the file it was read from
-- is no guide: a search path can reach the install by another spelling)
isLibrary :: CSignature -> Bool
isLibrary (CSignature n _ _ _) = pos_is_stdlib (getIdPosition n)

sortIds :: S.Set Id -> [Id]
sortIds s = map snd (sort [ (getIdString i, i) | i <- S.toList s ])

keepAllStats :: [Bool] -> [CImportedSignature] -> TciStats
keepAllStats libs sigs =
    let n lib = sum [ length ds | (l, CImpSign _ _ (CSignature _ _ _ ds)) <- zip libs sigs
                                , l == lib ]
    in  TciStats (n False, n False) (length (filter id libs), n True) 0 0 (0, 0, 0) 0

-- -------------------------
-- The keep rule
--
-- Deliberately loose (resolving names exactly needs the typechecker).  N
-- is a set of base names, seeded with every string of the local part
-- (every Id of mctx, generated names included, and the used sets and ATF
-- cache); with the Prelude names the typechecker refers to unqualified
-- (val, the Generic poly-wrap field; _read and _write, for Cwrite; _if,
-- primSelectFn, primWriteFn and primUpdateFn, for its desugarings in
-- TCheck), which resolve against every unqualified import; with every
-- name a library binds in the type namespace (foldTypeBinders), which
-- TIMonad.findFields tests by bare name; and with every name that a kept
-- entry binds or mentions.  Library signatures are referred to whole and
-- never walked, so library names reach N only by these seeds.  Kept:
-- every instance; every entry of a package re-exported whole (export
-- Q::*); every entry binding a ready name (an RDY_ Id, Id.isRdyId); and
-- every entry that binds a name in N, in any package and namespace.
-- Fixities and pragmas are kept when their name is in N.  Each closure
-- package keeps its signature, in order, even when nothing in it is kept.
-- Returns the non-library signatures, pruned.

pruneSigs :: [FString] -> S.Set String -> [(Bool, CImportedSignature)] ->
             IO ([CSignature], TciStats)
pruneSigs seeds exportAll lsigs = do
    let -- the non-library signatures' entries, numbered in order, with
        -- whether their package is re-exported whole
        pkgs = [ (getIdString n `S.member` exportAll, ds)
               | (False, CImpSign _ _ (CSignature n _ _ ds)) <- lsigs ]
        offsets = scanl (+) 0 (map (length . snd) pkgs)
        nEntries = last offsets
        libDefs = [ d | (True, CImpSign _ _ (CSignature _ _ _ ds)) <- lsigs, d <- ds ]
    -- every string of the entries' default bodies (rare in signatures)
    bodies <- fmap IM.fromList $ sequence
                [ do (_, strs) <- encodeV2Strip Nothing defs
                     return (e, strs)
                | (e0, (_, ds)) <- zip offsets pkgs, (e, d) <- zip [e0 ..] ds
                , let defs = defaultBodies d, not (null defs) ]
    let -- N is the least fixpoint of passes that walk every entry kept by N
        -- so far, adding the names it binds and mentions.  A pass takes the
        -- packages and their entries last first, since an entry mostly
        -- mentions what its imports and earlier entries bind, and skips an
        -- entry kept by N as the previous pass started (walked by then).
        revPkgs = reverse [ (e0 + length ds - 1, whole, reverse ds)
                          | (e0, (whole, ds)) <- zip offsets pkgs ]
        pass prev ns0 = foldl' (\ns (eLast, whole, rds) -> passPkg prev whole eLast ns rds) ns0 revPkgs
        passPkg _ !_ !_ !ns [] = ns
        passPkg prev whole e ns (d : ds) =
            let walked = maybe False (\p -> keptBy p whole d) prev
            in  passPkg prev whole (e - 1)
                        (if keptBy ns whole d && not walked then walk e d ns else ns) ds
        walk e d ns = foldl' add (foldDefnNames add ns d) (IM.findWithDefault [] e bodies)
        add ns x = let i = fstringId x
                   in  if IS.member i ns then ns else IS.insert i ns
        -- every instance, every entry of a package re-exported whole, every
        -- entry binding a ready name, and every entry binding a name in N
        keptBy ns whole d =
            whole || case firstBinder d of
                       Nothing -> isInstance d
                       Just _ -> foldDefnBinders (\r b -> r || isRdyId b || IS.member (nameId b) ns) False d
        fixpoint prev ns = let ns' = pass prev ns
                           in  if IS.size ns' == IS.size ns then ns else fixpoint (Just ns) ns'
        nFinal = fixpoint Nothing (foldl' (foldTypeBinders (\s i -> add s (getIdBase i)))
                                          (foldl' add IS.empty (prelude ++ seeds)) libDefs)
        prelude = [fsPolyWrapField, fs_read, fs_write, fs_if, fsPrimSelectFn, fsPrimUpdateFn, fsPrimWriteFn]
        inN i = IS.member (nameId i) nFinal
        -- an entry kept by binding a name has every name it binds in N
        keepEntry whole d = case firstBinder d of
                              Just b -> inN b
                              Nothing -> whole || isInstance d
        keepFix (CInfix _ i) = inN i
        keepFix (CInfixl _ i) = inN i
        keepFix (CInfixr _ i) = inN i
        keepPragma (CPragma (Pproperties i _)) = inN i
        keepPragma (CPragma (Pnoinline is)) = any inN is
        keepPragma _ = False
        prune (CImpSign _ _ (CSignature n _ fs ds)) =
            let whole = getIdString n `S.member` exportAll
            in  CSignature n [] (filter keepFix fs)
                    [ d | d <- ds, keepEntry whole d || (isPragma d && keepPragma d) ]
        isPragma (CPragma _) = True
        isPragma _ = False
        sigs' = [ prune s | (False, s) <- lsigs ]
        nKept = length [ () | (whole, ds) <- pkgs, d <- ds, keepEntry whole d ]
    return (sigs',
            TciStats (nKept, nEntries) (length (filter fst lsigs), length libDefs)
                     (IS.size nFinal) 0 (0, 0, 0) 0)
  where isInstance (CIinstance {}) = True
        isInstance _ = False

nameId :: Id -> Int
nameId = fstringId . getIdBase

-- The default bodies of a struct's or class's fields
defaultBodies :: CDefn -> [CClause]
defaultBodies (Cstruct _ _ _ _ fs _) = concatMap cf_default fs
defaultBodies (Cclass _ _ _ _ _ _ fs) = concatMap cf_default fs
defaultBodies _ = []

-- A strict left fold over the Ids an entry binds, in order (F1 of the keep
-- rule matches them by base name)
{-# INLINE foldDefnBinders #-}
foldDefnBinders :: (a -> Id -> a) -> a -> CDefn -> a
foldDefnBinders f z d = case d of
    Ctype ik _ _ -> f z (iKName ik)
    CItype ik _ _ -> f z (iKName ik)
    CprimType ik -> f z (iKName ik)
    Cdata { cd_name = ik, cd_original_summands = ocs, cd_internal_summands = ics } ->
        let z1 = foldl' (\a c -> fs a (cos_names c)) (f z (iKName ik)) ocs
            z2 = foldl' (\a c -> maybe a (fs a) (cos_field_names c)) z1 ocs
        in  foldl' (\a c -> fs a (cis_names c)) z2 ics
    Cstruct _ _ ik _ flds _ -> foldl' (\a fl -> f a (cf_name fl)) (f z (iKName ik)) flds
    Cclass _ _ ik _ _ ats flds ->
        foldl' (\a fl -> f a (cf_name fl)) (foldl' (\a at -> f a (ca_name at)) (f z (iKName ik)) ats) flds
    CIclass _ _ ik _ _ ats ms _ -> fs (foldl' (\a at -> f a (ca_name at)) (f z (iKName ik)) ats) ms
    CIValueSign i _ -> f z i
    Cforeign { cforg_name = i } -> f z i
    Cprimitive i _ -> f z i
    _ -> z
  where fs = foldl' f

-- A strict left fold over the Ids an entry binds in the type namespace:
-- a type, synonym or class, and a class's associated type families.
-- TIMonad.findFields takes a struct type as imported when its bare name
-- finds any type in scope, so a MatX type with a library type's name stays
-- even when nothing the package reaches names it.
{-# INLINE foldTypeBinders #-}
foldTypeBinders :: (a -> Id -> a) -> a -> CDefn -> a
foldTypeBinders f z d = case d of
    Ctype ik _ _ -> f z (iKName ik)
    CItype ik _ _ -> f z (iKName ik)
    CprimType ik -> f z (iKName ik)
    Cdata { cd_name = ik } -> f z (iKName ik)
    Cstruct _ _ ik _ _ _ -> f z (iKName ik)
    Cclass _ _ ik _ _ ats _ -> foldl' (\a at -> f a (ca_name at)) (f z (iKName ik)) ats
    CIclass _ _ ik _ _ ats _ _ -> foldl' (\a at -> f a (ca_name at)) (f z (iKName ik)) ats
    _ -> z

-- The first Id that foldDefnBinders gives
{-# INLINE firstBinder #-}
firstBinder :: CDefn -> Maybe Id
firstBinder d = case d of
    Ctype ik _ _ -> Just (iKName ik)
    CItype ik _ _ -> Just (iKName ik)
    CprimType ik -> Just (iKName ik)
    Cdata { cd_name = ik } -> Just (iKName ik)
    Cstruct _ _ ik _ _ _ -> Just (iKName ik)
    Cclass _ _ ik _ _ _ _ -> Just (iKName ik)
    CIclass _ _ ik _ _ _ _ _ -> Just (iKName ik)
    CIValueSign i _ -> Just i
    Cforeign { cforg_name = i } -> Just i
    Cprimitive i _ -> Just i
    _ -> Nothing

-- A strict left fold over the base names an entry binds or mentions,
-- except in default bodies (see pruneSigs).  Type variables and the sorts
-- carried by TyCons are left out: a tyvar binds nothing, and a TyCon's
-- sort comes from the entry that binds its name, which its name keeps.
-- Inlined so that f is known at each use: otherwise every name is passed
-- as a thunk and every step reboxes the accumulator.
{-# INLINE foldDefnNames #-}
foldDefnNames :: (a -> FString -> a) -> a -> CDefn -> a
foldDefnNames f z0 d = defn (foldDefnBinders (\z i -> f z (getIdBase i)) z0 d)
  where
    defn z = case d of
      Ctype _ _ t -> ty z t
      CItype _ _ _ -> z
      CprimType _ -> z
      Cdata { cd_original_summands = ocs, cd_internal_summands = ics, cd_derivings = ders } ->
          foldl' der (foldl' (\z' -> ty z' . cis_arg_type) (foldl' (foldl' qt) z (map cos_arg_types ocs)) ics) ders
      Cstruct _ st _ _ fs ders -> foldl' der (foldl' field (sub z st) fs) ders
      Cclass _ ps _ _ _ _ fs -> foldl' field (foldl' prd z ps) fs
      CIclass _ ps _ _ _ _ _ _ -> foldl' prd z ps
      CIValueSign _ t -> qt z t
      Cforeign { cforg_type = t } -> qt z t
      Cprimitive _ t -> qt z t
      CIinstance i t -> qt (idf z i) t
      CPragma (Pproperties i _) -> idf z i
      CPragma (Pnoinline is) -> foldl' idf z is
      Cinstance t _ -> qt z t
      CValue i _ -> idf z i
      CValueSign (CDef i t _) -> qt (idf z i) t
      CValueSign (CDefT i _ t _) -> qt (idf z i) t
    idf !z i = f z (getIdBase i)
    der z (CStock tcs) = foldl' idf z (map typeclassId tcs)
    der z (CVia tc i) = idf (idf z (typeclassId tc)) i
    sub z (SDataCon i _) = idf z i
    sub z (SPolyWrap i mc g) = maybe id (flip idf) mc (idf (idf z i) g)
    sub z _ = z
    field z (CField n _ t _ mot) = maybe id (flip ty) mot (qt (idf z n) t)
    qt z (CQType ps t) = ty (foldl' prd z ps) t
    prd z (CPred tc ts) = foldl' ty (idf z (typeclassId tc)) ts
    ty z (TCon (TyCon i _ _)) = idf z i
    ty z (TCon _) = z
    ty z (TAp a b) = ty (ty z a) b
    ty z (TVar _) = z
    ty z (TGen _ _) = z
    ty z (TDefMonad _) = z

-- -------------------------
-- Reading

-- The strings are interned first, then afterStrings runs, then the rest
-- is decoded; also returns the hash of every byte after the tag.  Library
-- signatures are left as references (see readTciLibraries).
readTciFile :: ErrorHandle -> String -> B.ByteString -> IO () ->
               IO (TciFile, String)
readTciFile errh nm s afterStrings = do
    let hlen = B.length headerBS
        bad = bsError errh [(noPosition, EBinFileVerMismatch nm)]
    when (B.take hlen s /= headerBS) bad
    let body = B.drop hlen s
        (bsT, rest1) = splitLen body
        (bsL, bsS) = splitLen rest1
        hash = showHash (B.foldl' nextHashByte hashInit body)
    when (B.length body < 8) bad
    -- the container's string table is interned when it is decoded
    _ <- evaluate (length (decode bsT :: [FString]))
    afterStrings
    let TciLocal src fl (CPackageBin pkg) libs ut uc ue atf = decode bsL
        sigs = decode bsS :: [CSignature]
        CPackage pid exps imps refs fixs ds incs = pkg
        merge (CImpSign _ q sig : rs) (Just _ : ls) ss = CImpSign "" q sig : merge rs ls ss
        merge (CImpSign _ q _ : rs) (Nothing : ls) (sig : ss) = CImpSign "" q sig : merge rs ls ss
        merge [] [] [] = []
        merge _ _ _ = internalError "readTciFile: signatures"
    when (length libs /= length refs ||
          length sigs /= length (filter isNothing libs)) $ bad
    return (TciFile { tci_source = src
                    , tci_flags = fl
                    , tci_package = CPackage pid exps imps (merge refs libs sigs) fixs ds incs
                    , tci_libs = libs
                    , tci_used_types = S.fromList ut
                    , tci_used_ctx = S.fromList uc
                    , tci_used_exports = S.fromList ue
                    , tci_atf = M.fromList atf },
            hash)
  where splitLen b = let n = fromIntegral (readW64 (B.take 8 b))
                     in  B.splitAt n (B.drop 8 b)
        readW64 :: B.ByteString -> Word64
        readW64 b = foldr (\x acc -> (acc `shiftL` 8) + fromIntegral x) 0 (B.unpack b)

-- Fill in each library signature of a .tci that readTciFile read, from
-- the library's .bi on the search path (verbose: each copy read is
-- reported), which must be the file -signature-only read: the first
-- copy with the recorded hash, so other copies are no reason for a
-- warning.  Its strings are among the .tci's, which readTciFile has
-- interned, so this interns nothing; the .bi is decoded as lazily as an
-- import is, and a copy passed over is not decoded.
readTciLibraries :: ErrorHandle -> Bool -> [String] -> TciFile -> IO TciFile
readTciLibraries errh verb path tci = do
    let CPackage pid exps imps sigs fixs ds incs = tci_package tci
        fill Nothing s = return s
        fill (Just h) (CImpSign f q (CSignature n _ _ _)) = do
            let biname = getIdString n ++ "." ++ biSuffix
                search first [] = return (Left first)
                search first (c:cs) = do
                    mfile <- readBinFoundFile errh noPosition verb c
                    case mfile of
                      Nothing -> search first cs
                      Just file
                        | Just (bi, hash) <- readBiFileMaybe file, hash == h
                        -> return (Right (bi, c))
                        | otherwise -> search (Just (fromMaybe (file, c) first)) cs
            found <- existsFilePath biname path >>= search Nothing
            (bi, name) <- case found of
                Right bc -> return bc
                Left Nothing ->
                    bsError errh [(noPosition, EMissingIfcFile biname (pfpString n))]
                -- the first copy read is reported: not a .bi, or not the one
                Left (Just (file, name)) -> do
                    _ <- readBiFile errh name file
                    bsError errh [(noPosition, EBinFileSignatureMismatch biname (pfpString pid))]
            when (bi_name bi /= n) $
                bsError errh [(noPosition, EBinFilePkgNameMismatch name
                                               (pfpString n) (pfpString (bi_name bi)))]
            let CSignature n' _ fs es = bi_sig bi
            return (CImpSign f q (CSignature n' [] fs es))
    before <- internCount
    sigs' <- zipWithM fill (tci_libs tci) sigs
    after <- internCount
    when (after /= before) $
        internalError ("readTciLibraries: a library .bi holds " ++ show (after - before) ++
                       " strings that the .tci does not")
    return (tci { tci_package = CPackage pid exps imps sigs' fixs ds incs })
