{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Werror -fwarn-incomplete-patterns #-}
module GenBXref(BXrefFile(..), BXrefStage(..), bxrefStageName,
                genBXrefFile, readBXrefFile) where

import Error(internalError, ErrMsg(..), ErrorHandle, bsErrorUnsafe)
import Position(Position, noPosition)
import Version(bscVersionStr)

import BinData
import IExpandMeta(ModuleMeta(..), SiteTable(..), SiteKey(..), SiteKind,
                   SiteRow(..), RecKey(..), ContextTable(..), TypeTable(..),
                   TypeKey(..), RepackKind(..), RepackFact(..),
                   DefRecord(..), MetaEvent(..), PassTag, siteIdsFromRows,
                   contextIdsFromRows, typeIdsFromRows)
import FileIOUtil(writeBinaryFileCatch)

import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString as B

-- ===============
-- The .bxref file
--
-- <module>.bxref, written beside the module's .ba under -bxref, is the
-- module's cross-reference: the evaluator's metadata table
-- (IExpandMeta.ModuleMeta: the sites, the contexts, the type windows,
-- the roots, one record per lifted def and the repacks) and the
-- ledger of what the passes since
-- elaboration did to the def and rule ids, as the ledger stood when
-- the module's code was generated.  It is an artifact of its own: the
-- .ba never carries the table or the ledger, and nothing in a compile
-- reads a .bxref; its readers are the tools (dumpbxref).  The file is
-- the format tag below, then one BinData container holding a
-- BXrefFile, written with the -remap-path-prefix remap the .ba's
-- positions get.

-- .bxref file tag -- change this whenever the .bxref format changes
-- See also GenABin.header and GenBin.header
header :: [Byte]
header = B.unpack $ TE.encodeUtf8 $ T.pack "bsc-bxref-20261103-1"

headerBS :: B.ByteString
headerBS = B.pack header

-- The stage the ledger had reached when the file was written: the end
-- of the Verilog backend (the final ASPackage, after finalCleanup); the
-- final APackage, which the Bluesim backend takes; or the package
-- handed to the scheduler, when scheduling failed.
data BXrefStage = BXVerilog | BXBluesim | BXSchedErr
        deriving (Eq, Show, Enum, Bounded)

-- the stage's name in the file's text renderings
bxrefStageName :: BXrefStage -> String
bxrefStageName BXVerilog = "verilog"
bxrefStageName BXBluesim = "bluesim"
bxrefStageName BXSchedErr = "schederr"

data BXrefFile = BXrefFile {
        -- the compiler that wrote the file (bscVersionStr True, as a
        -- .ba's ab_version); the reader accepts no other
        bx_version :: String,
        -- the module's name
        bx_module :: String,
        bx_stage :: BXrefStage,
        -- the table and the ledger
        bx_meta :: ModuleMeta
    }

genBXrefFile :: ErrorHandle -> (Position -> Position) -> String -> BXrefFile
             -> IO ()
genBXrefFile errh remapP fn bxfile =
    writeBinaryFileCatch errh fn (header ++ encodeWith remapP bxfile)

-- Decoded strictly, as a .ba is (GenABin.readABinFile): the tools read
-- the whole file.  A file with another tag, or written by another
-- compiler build, is rejected as a .ba is (ABinUtil.decodeABin): the
-- metadata is produced and consumed by one compiler.
readBXrefFile :: ErrorHandle -> String -> B.ByteString -> BXrefFile
readBXrefFile errh nm s =
    let hlen = B.length headerBS
        bxfile = decodeStrict (B.drop hlen s)
    in  if B.take hlen s /= headerBS || bx_version bxfile /= bscVersionStr True
        then bsErrorUnsafe errh [(noPosition, EBinFileVerMismatch nm)]
        else bxfile

-- ----------
-- Bin instances of the table and the ledger.  They live here rather
-- than beside their types because BinData imports ISyntax, which
-- carries the ledger.  An IntMap or IntSet is written as its ascending
-- list and an enumeration as its index, checked against its bounds
-- when read, so the bytes are a function of the content.  The
-- inverse maps of the tables (st_ids, ct_ids, tt_ids) are not
-- written: all are rebuilt from the rows on read
-- (IExpandMeta.siteIdsFromRows, contextIdsFromRows, typeIdsFromRows),
-- and SiteKey and TypeKey order by intern id (the identifier's, the
-- type's), so writing st_ids or tt_ids would make the bytes depend on
-- the live intern table.  A type key's type is written as the .bo
-- writes an IType.

instance Bin BXrefFile where
    writeBytes (BXrefFile v m s mm) =
        section "BXrefFile" $
        do toBin v
           toBin m
           toBin s
           toBin mm
    readBytes = do v <- fromBin
                   m <- fromBin
                   s <- fromBin
                   mm <- fromBin
                   return (BXrefFile v m s mm)

instance Bin BXrefStage where
    writeBytes s = putI (fromEnum s)
    readBytes = fmap toEnumOrErr getI

instance Bin ModuleMeta where
    writeBytes (ModuleMeta s c ty r d rp e) =
        do toBin s
           toBin c
           toBin ty
           toBin r
           toBin (IM.toAscList d)
           toBin rp
           toBin e
    readBytes = do s <- fromBin
                   c <- fromBin
                   ty <- fromBin
                   r <- fromBin
                   d <- fromBin
                   rp <- fromBin
                   e <- fromBin
                   return (ModuleMeta s c ty r (IM.fromDistinctAscList d) rp e)

instance Bin SiteTable where
    writeBytes (SiteTable n _ rows recs) =
        do toBin n
           toBin (IM.toAscList rows)
           toBin (IM.toAscList recs)
    readBytes = do n <- fromBin
                   rows <- fromBin
                   recs <- fromBin
                   let rowMap = IM.fromDistinctAscList rows
                   return (SiteTable n (siteIdsFromRows rowMap) rowMap
                                     (IM.fromDistinctAscList recs))

instance Bin SiteKey where
    writeBytes (IdKey b q p k) = do putI 0; toBin b; toBin q; toBin p; toBin k
    writeBytes (PrimKey op) = do putI 1; toBin op
    writeBytes (RootKey n k) = do putI 2; toBin n; toBin k
    writeBytes BoundKey = putI 3
    writeBytes (FieldKey n) = do putI 4; toBin n
    readBytes = do i <- getI
                   case i of
                     0 -> do b <- fromBin; q <- fromBin; p <- fromBin; k <- fromBin
                             return (IdKey b q p k)
                     1 -> fmap PrimKey fromBin
                     2 -> do n <- fromBin; k <- fromBin
                             return (RootKey n k)
                     3 -> return BoundKey
                     4 -> fmap FieldKey fromBin
                     n -> internalError ("GenBXref.Bin(SiteKey).readBytes: " ++ show n)

instance Bin SiteKind where
    writeBytes k = putI (fromEnum k)
    readBytes = fmap toEnumOrErr getI

instance Bin SiteRow where
    writeBytes (SiteRow k u r) = do toBin k; toBin u; toBin r
    readBytes = do k <- fromBin; u <- fromBin; r <- fromBin
                   return (SiteRow k u r)

instance Bin RecKey where
    writeBytes (RecKey k u) = do toBin k; toBin u
    readBytes = do k <- fromBin; u <- fromBin
                   return (RecKey k u)

instance Bin ContextTable where
    writeBytes (ContextTable n _ rows ops) =
        do toBin n
           toBin (IM.toAscList rows)
           toBin ops
    readBytes = do n <- fromBin
                   rows <- fromBin
                   ops <- fromBin
                   let rowMap = IM.fromDistinctAscList rows
                   return (ContextTable n (contextIdsFromRows rowMap) rowMap ops)

instance Bin DefRecord where
    writeBytes (DefRecord p h s c cs m f par sh b a vd rps us) =
        do toBin p
           toBin h
           toBin s
           toBin c
           toBin (IS.toAscList cs)
           toBin m
           toBin f
           toBin par
           toBin sh
           toBin b
           toBin a
           toBin vd
           toBin rps
           toBin us
    readBytes = do p <- fromBin
                   h <- fromBin
                   s <- fromBin
                   c <- fromBin
                   cs <- fromBin
                   m <- fromBin
                   f <- fromBin
                   par <- fromBin
                   sh <- fromBin
                   b <- fromBin
                   a <- fromBin
                   vd <- fromBin
                   rps <- fromBin
                   us <- fromBin
                   return (DefRecord p h s c (IS.fromDistinctAscList cs) m f par sh b a
                                     vd rps us)

instance Bin TypeTable where
    writeBytes (TypeTable n _ rows) =
        do toBin n
           toBin (IM.toAscList rows)
    readBytes = do n <- fromBin
                   rows <- fromBin
                   let rowMap = IM.fromDistinctAscList rows
                   return (TypeTable n (typeIdsFromRows rowMap) rowMap)

instance Bin TypeKey where
    writeBytes (TypeKey t o w) = do toBin t; toBin o; toBin w
    readBytes = do t <- fromBin; o <- fromBin; w <- fromBin
                   return (TypeKey t o w)

instance Bin RepackKind where
    writeBytes RKUnpack = putI 0
    writeBytes RKChr = putI 1
    readBytes = do i <- getI
                   case i of
                     0 -> return RKUnpack
                     1 -> return RKChr
                     n -> internalError ("GenBXref.Bin(RepackKind).readBytes: " ++ show n)

instance Bin RepackFact where
    writeBytes (RepackFact k c f t cx) =
        do toBin k; toBin c; toBin f; toBin t; toBin cx
    readBytes = do k <- fromBin; c <- fromBin; f <- fromBin; t <- fromBin
                   cx <- fromBin
                   return (RepackFact k c f t cx)

instance Bin PassTag where
    writeBytes t = putI (fromEnum t)
    readBytes = fmap toEnumOrErr getI

instance Bin MetaEvent where
    writeBytes (Renamed t a b) = do putI 0; toBin t; toBin a; toBin b
    writeBytes (Merged t as b) = do putI 1; toBin t; toBin as; toBin b
    writeBytes (Inlined t a bs) = do putI 2; toBin t; toBin a; toBin bs
    writeBytes (Created t a bs) = do putI 3; toBin t; toBin a; toBin bs
    writeBytes (Dropped t a) = do putI 4; toBin t; toBin a
    readBytes = do i <- getI
                   case i of
                     0 -> do t <- fromBin; a <- fromBin; b <- fromBin
                             return (Renamed t a b)
                     1 -> do t <- fromBin; as <- fromBin; b <- fromBin
                             return (Merged t as b)
                     2 -> do t <- fromBin; a <- fromBin; bs <- fromBin
                             return (Inlined t a bs)
                     3 -> do t <- fromBin; a <- fromBin; bs <- fromBin
                             return (Created t a bs)
                     4 -> do t <- fromBin; a <- fromBin
                             return (Dropped t a)
                     n -> internalError ("GenBXref.Bin(MetaEvent).readBytes: " ++ show n)

-- an enumeration's index back to the value, within its bounds
toEnumOrErr :: (Enum a, Bounded a, Show a) => Int -> a
toEnumOrErr i =
    let r = toEnum i
        lo = minBound `asTypeOf` r
        hi = maxBound `asTypeOf` r
    in  if i >= fromEnum lo && i <= fromEnum hi
        then r
        else internalError ("GenBXref.toEnumOrErr: " ++ show i ++ " outside " ++
                            show lo ++ ".." ++ show hi)
