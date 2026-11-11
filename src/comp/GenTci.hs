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

import Control.Monad(when, zipWithM)
import Control.Exception(evaluate)
import Data.Maybe(isNothing, fromMaybe)
import Data.Bits(shiftL)
import Data.Char(isDigit)
import Data.List(isPrefixOf, isSuffixOf, sort)
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

import FStringCompat(FString, getFString, fstringId, mkFString)
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
import FileIOUtil(writeFileBuilderCatch, readBinaryFileCatch, existsFilePath, readBinFoundFile)
import Util(hashInit, nextHashByte, showHash)
import SpeedyString(internCount)
import PreStrings(allPreStrings)

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
    (bsL, strsL) <- strict <$> encodeV2Strip msel local
    -- every string of the .bi files the signatures were read from (Sig has
    -- read them, so these are interned already): the check interns a
    -- library's when it reads it; a MatX file's that no kept entry holds
    -- are those only the pruned entries hold (its package name is kept)
    let biStrings lib = concat <$> sequence
                          [ readBinaryFileCatch errh noPosition f >>= readBiStrings errh f
                          | (l, CImpSign f _ _) <- zip isLib sigs, l == lib ]
    strsLib <- biStrings True
    let exportAll = S.fromList [ getIdString p | Left es <- [exps], CExpPkg p <- es
                                               , p /= pid ]
    (sigs', stats) <-
        if ti_full ti
        then return ([ wholeSig s | (False, s) <- zip isLib sigs ], keepAllStats isLib sigs)
        else pruneSigs (IS.fromList (map fstringId strsL)) exportAll (zip isLib sigs)
    (bsS, strsS) <- strict <$> encodeV2Strip msel [ TciSig s | s <- sigs' ]
    strsD <- if ti_full ti then return [] else biStrings False
    let kept = IS.fromList (map fstringId (strsL ++ strsS ++ strsLib))
        -- a check makes the compiler's own strings itself (NumEq for a
        -- numeric proviso, say), and spellings of the shapes it generates;
        -- dropping one would move it after every string of the file
        -- instead of keeping Sig's rank
        builtins = IS.fromList (map fstringId allPreStrings)
        onlyDropped = IS.difference (IS.fromList [ fstringId x | x <- strsD
                                                             , not (generatedShape x) ])
                                    (IS.union kept builtins)
        handoff = [ x | x <- ti_handoff_strings ti
                      , not (IS.member (fstringId x) onlyDropped) ]
        strs = S.toAscList (S.fromList (strsL ++ strsS ++ strsLib ++ handoff))
    (bsT, _) <- strict <$> encodeV2Strip Nothing strs
    let body = BB.word64LE (fromIntegral (B.length bsT)) <> BB.byteString bsT <>
               BB.word64LE (fromIntegral (B.length bsL)) <> BB.byteString bsL <>
               BB.byteString bsS
        bodyBS = BL.toStrict (BB.toLazyByteString body)
        hash = showHash (B.foldl' nextHashByte hashInit bodyBS)
    writeFileBuilderCatch errh fn (BB.byteString headerBS <> BB.byteString bodyBS)
    return (stats { ts_strings = length strs
                  , ts_bytes = (B.length bsT, B.length bsL, B.length bsS)
                  , ts_stripped = if ti_keep_pos ti then 0 else IS.size impFiles },
            hash)
  where wholeSig (CImpSign _ _ (CSignature n _ fs ds)) = CSignature n [] fs ds
        strict (bs, ss) = (BL.toStrict bs, ss)

-- The spellings of the names that the check generates (_tcdictN, x__,
-- x_AV for $swrite into x (TCheck.createIdNew), instance names with
-- dots, x~y, numbers), by their shape
generatedShape :: FString -> Bool
generatedShape fs =
    let s = getFString fs
    in  take 1 s == "_" || "__" `isSuffixOf` s || "_AV" `isSuffixOf` s ||
        (not (null s) && all isDigit s) || any (`elem` ".~") s

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
-- cache), with "val" (the Generic poly-wrap field), and with every name
-- that a kept entry binds or mentions.  Kept (library signatures are
-- referred to whole, and every name they bind or mention is in N): every
-- instance; every entry of a package re-exported whole
-- (export Q::*); every entry binding an RDY_ name; and every entry that
-- binds a name in N, in any package and namespace.  Fixities and pragmas
-- are kept when their name is in N.  Each closure package keeps its
-- signature, in order, even when nothing in it is kept.  Returns the
-- non-library signatures, pruned.

pruneSigs :: IS.IntSet -> S.Set String -> [(Bool, CImportedSignature)] ->
             IO ([CSignature], TciStats)
pruneSigs seeds exportAll lsigs = do
    let -- (signature, entry) -> entry, for non-library signatures
        entries = [ ((si, di), (n, d))
                  | (si, (False, CImpSign _ _ (CSignature n _ _ ds))) <- zip [0 :: Int ..] lsigs
                  , (di, d) <- zip [0 :: Int ..] ds ]
    -- every string of the entries' default bodies (rare in signatures)
    bodies <- fmap M.fromList $ sequence
                [ do (_, strs) <- encodeV2Strip Nothing defs
                     return (k, strs)
                | (k, (_, d)) <- entries, let defs = defaultBodies d, not (null defs) ]
    let entryMap = M.fromList entries
        namesOf k d = defnNames d ++ M.findWithDefault [] k bodies
        byName = IM.fromListWith (++)
                   [ (fstringId b, [k]) | (k, (_, d)) <- entries, b <- defnBinders d ]
        libNames = concat [ concatMap defnNames ds
                          | (True, CImpSign _ _ (CSignature _ _ _ ds)) <- lsigs ]
        isRdy b = "RDY_" `isPrefixOf` getFString b
        k0 = [ k | (k, (n, d)) <- entries
                 , isInstance d || getIdString n `S.member` exportAll
                   || any isRdy (defnBinders d) ]
        n0 = IS.unions [ seeds
                       , IS.fromList (map fstringId (mkFString "val" : libNames)) ]
        -- keep the entries k, then everything the names reach
        go :: IS.IntSet -> S.Set (Int, Int) -> [Int] -> [(Int, Int)] ->
              (IS.IntSet, S.Set (Int, Int))
        go ns kept [] [] = (ns, kept)
        go ns kept names (k : ks)
            | k `S.member` kept = go ns kept names ks
            | otherwise =
                let d = snd (entryMap M.! k)
                    new = IS.toList (IS.fromList [ x | x <- map fstringId (namesOf k d)
                                                     , not (IS.member x ns) ])
                    ns' = foldr IS.insert ns new
                in  go ns' (S.insert k kept) (new ++ names) ks
        go ns kept (x : xs) [] = go ns kept xs (IM.findWithDefault [] x byName)
        (nFinal, keptSet) = go n0 S.empty (IS.toList n0) k0
        inN i = IS.member (fstringId (getIdBase i)) nFinal
        keepFix (CInfix _ i) = inN i
        keepFix (CInfixl _ i) = inN i
        keepFix (CInfixr _ i) = inN i
        keepPragma (CPragma (Pproperties i _)) = inN i
        keepPragma (CPragma (Pnoinline is)) = any inN is
        keepPragma _ = False
        prune si (CImpSign _ _ (CSignature n _ fs ds)) =
            CSignature n [] (filter keepFix fs)
                [ d | (di, d) <- zip [0 ..] ds
                    , S.member (si, di) keptSet || (isPragma d && keepPragma d) ]
        isPragma (CPragma _) = True
        isPragma _ = False
        nLib = sum [ length ds | (True, CImpSign _ _ (CSignature _ _ _ ds)) <- lsigs ]
        sigs' = [ prune si s | (si, (False, s)) <- zip [0 ..] lsigs ]
    return (sigs',
            TciStats (S.size keptSet, length entries) (length (filter fst lsigs), nLib)
                     (IS.size nFinal) 0 (0, 0, 0) 0)
  where isInstance (CIinstance {}) = True
        isInstance _ = False

-- The default bodies of a struct's or class's fields
defaultBodies :: CDefn -> [CClause]
defaultBodies (Cstruct _ _ _ _ fs _) = concatMap cf_default fs
defaultBodies (Cclass _ _ _ _ _ _ fs) = concatMap cf_default fs
defaultBodies _ = []

-- The base names an entry binds (as F1 of the keep rule matches them)
defnBinders :: CDefn -> [FString]
defnBinders d = map getIdBase $ case d of
    Ctype ik _ _ -> [iKName ik]
    CItype ik _ _ -> [iKName ik]
    CprimType ik -> [iKName ik]
    Cdata { cd_name = ik, cd_original_summands = ocs, cd_internal_summands = ics } ->
        iKName ik : concatMap cos_names ocs ++ concat [ fs | Just fs <- map cos_field_names ocs ]
                  ++ concatMap cis_names ics
    Cstruct _ _ ik _ fs _ -> iKName ik : map cf_name fs
    Cclass _ _ ik _ _ ats fs -> iKName ik : map ca_name ats ++ map cf_name fs
    CIclass _ _ ik _ _ ats ms _ -> iKName ik : map ca_name ats ++ ms
    CIValueSign i _ -> [i]
    Cforeign { cforg_name = i } -> [i]
    Cprimitive i _ -> [i]
    _ -> []

-- The base names an entry binds or mentions, except in default bodies
-- (see pruneSigs).  Type variables and the sorts carried by TyCons are
-- left out: a tyvar binds nothing, and a TyCon's sort comes from the
-- entry that binds its name, which its name keeps.
defnNames :: CDefn -> [FString]
defnNames d = defnBinders d ++ map getIdBase (defnIds d)

defnIds :: CDefn -> [Id]
defnIds d = case d of
    Ctype _ _ t -> tyIds t
    CItype _ _ _ -> []
    CprimType _ -> []
    Cdata { cd_original_summands = ocs, cd_internal_summands = ics, cd_derivings = ders } ->
        concatMap (concatMap qtIds . cos_arg_types) ocs ++
        concatMap (tyIds . cis_arg_type) ics ++ concatMap derIds ders
    Cstruct _ st _ _ fs ders -> stIds st ++ concatMap fieldIds fs ++ concatMap derIds ders
    Cclass _ ps _ _ _ _ fs -> concatMap predIds ps ++ concatMap fieldIds fs
    CIclass _ ps _ _ _ _ _ _ -> concatMap predIds ps
    CIValueSign _ qt -> qtIds qt
    Cforeign { cforg_type = qt } -> qtIds qt
    Cprimitive _ qt -> qtIds qt
    CIinstance i qt -> i : qtIds qt
    CPragma (Pproperties i _) -> [i]
    CPragma (Pnoinline is) -> is
    Cinstance qt _ -> qtIds qt
    CValue i _ -> [i]
    CValueSign (CDef i qt _) -> i : qtIds qt
    CValueSign (CDefT i _ qt _) -> i : qtIds qt
  where
    derIds (CStock tcs) = map typeclassId tcs
    derIds (CVia tc i) = [typeclassId tc, i]
    stIds (SDataCon i _) = [i]
    stIds (SPolyWrap i mc f) = i : f : maybe [] (: []) mc
    stIds _ = []
    fieldIds (CField n _ qt _ mot) = n : qtIds qt ++ maybe [] tyIds mot

qtIds :: CQType -> [Id]
qtIds (CQType ps t) = concatMap predIds ps ++ tyIds t

predIds :: CPred -> [Id]
predIds (CPred tc ts) = typeclassId tc : concatMap tyIds ts

tyIds :: CType -> [Id]
tyIds (TCon (TyCon i _ _)) = [i]
tyIds (TCon _) = []
tyIds (TAp a b) = tyIds a ++ tyIds b
tyIds (TVar _) = []
tyIds (TGen _ _) = []
tyIds (TDefMonad _) = []

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
