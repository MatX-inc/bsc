{-# LANGUAGE OverloadedStrings #-}

-- | BIR export: serialize the post-scheduling simulation system for the
-- TRS backend (src/trs).
--
-- The format is specified in src/trs/BIR.md and defined operationally
-- by the Rust types in src/trs/crates/trs-ir; @trs ir dump@
-- round-trips the output.  The input is the same 'SimSystem' that
-- 'simMakeCBlocks' consumes today (post 'simExpand' / 'simPackageOpt'),
-- so schedule merging and package optimization stay in this compiler.
--
-- Wire conventions match ciborium's serde defaults on the Rust side:
-- structs are CBOR maps keyed by field name, tuples and Vecs are arrays,
-- @Option@ is null-or-value, unit enum variants are strings and payload
-- variants are single-entry maps.
--
-- STATUS (P0, in progress): exports the design skeleton — schema version,
-- interned string table, module inventory, instance map, defaults.
-- Module bodies (defs, rules, methods, instances), segmented schedules,
-- and compositions are the remainder of P0 and encode as empty sections
-- meanwhile, which the Rust verifier accepts.
module SimExportIR
    ( birVersion
    , simSystemToBir
    , writeBirFile
    ) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Text as T
import Data.Word (Word32)

import qualified Codec.CBOR.Encoding as C
import qualified Codec.CBOR.Write as CW

import Id (getIdBaseString)
import SimPackage

-- | Bumped on any change to the encoded shape; must equal BIR_VERSION in
-- trs-ir/src/lib.rs.
birVersion :: Word32
birVersion = 1

-- ===============
-- String interning
--
-- All identifiers in BIR are indices into one design-wide string table.

data StrTable = StrTable !(M.Map String Word32) ![String] !Word32

emptyStrTable :: StrTable
emptyStrTable = StrTable M.empty [] 0

intern :: String -> StrTable -> (Word32, StrTable)
intern s tbl@(StrTable m rev n) =
    case M.lookup s m of
      Just i  -> (i, tbl)
      Nothing -> (n, StrTable (M.insert s n m) (s : rev) (n + 1))

interns :: [String] -> StrTable -> ([Word32], StrTable)
interns [] tbl = ([], tbl)
interns (s : rest) tbl =
    let (i, tbl') = intern s tbl
        (is, tbl'') = interns rest tbl'
    in  (i : is, tbl'')

tableStrings :: StrTable -> [String]
tableStrings (StrTable _ rev _) = reverse rev

-- ===============
-- Encoding helpers (ciborium/serde conventions)

-- A struct is a map keyed by field name.
encStruct :: [(String, C.Encoding)] -> C.Encoding
encStruct fields =
    C.encodeMapLen (fromIntegral (length fields))
    <> mconcat [ encStr k <> v | (k, v) <- fields ]

-- Vec<T> and tuples are arrays.
encList :: [C.Encoding] -> C.Encoding
encList xs = C.encodeListLen (fromIntegral (length xs)) <> mconcat xs

encPair :: C.Encoding -> C.Encoding -> C.Encoding
encPair a b = C.encodeListLen 2 <> a <> b

-- Option<T> is null or the value.
encMaybe :: (a -> C.Encoding) -> Maybe a -> C.Encoding
encMaybe _ Nothing  = C.encodeNull
encMaybe f (Just x) = f x

encW32 :: Word32 -> C.Encoding
encW32 = C.encodeWord32

encStr :: String -> C.Encoding
encStr = C.encodeString . T.pack

-- ===============
-- SimSystem -> BIR

-- | Encode a 'SimSystem' as a BIR design document.
simSystemToBir :: SimSystem -> L.ByteString
simSystemToBir ssys = CW.toLazyByteString (encDesign ssys)

-- | Write the design's .bir file.
writeBirFile :: FilePath -> SimSystem -> IO ()
writeBirFile path ssys = L.writeFile path (simSystemToBir ssys)

encDesign :: SimSystem -> C.Encoding
encDesign ssys =
    let pkgs = M.elems (ssys_packages ssys)
        instmap = M.toList (ssys_instmap ssys)

        -- interning pass: order matters only for determinism
        (topId, t1) = intern (getIdBaseString (ssys_top ssys)) emptyStrTable
        (modIds, t2) = interns (map (getIdBaseString . sp_name) pkgs) t1
        (instPathIds, t3) = interns (map fst instmap) t2
        (instModIds, t4) = interns (map snd instmap) t3
        (clkId, t5) = internMaybe (ssys_default_clk ssys) t4
        (rstId, t6) = internMaybe (ssys_default_rst ssys) t5

        internMaybe Nothing  t = (Nothing, t)
        internMaybe (Just s) t = let (i, t') = intern s t in (Just i, t')
    in
    encStruct
      [ ("version", encW32 birVersion)
      , ("strings", encList (map encStr (tableStrings t6)))
      , ("top", encW32 topId)
      , ("modules", encList (zipWith encModule modIds pkgs))
      , ("instance_map",
          encList (zipWith (\p m -> encPair (encW32 p) (encW32 m))
                           instPathIds instModIds))
      , ("compositions", encList [])   -- P0 TODO: per-(clock,edge) interleaving
      , ("foreign_funcs", encList [])  -- P0 TODO: from ssys_ffuncmap
      , ("default_clock", encMaybe encW32 clkId)
      , ("default_reset", encMaybe encW32 rstId)
      ]

-- P0 TODO: bodies.  Every section below the name is exported empty until
-- the corresponding encoder lands; the Rust verifier treats empty sections
-- as "module not yet populated" during bring-up.
encModule :: Word32 -> SimPackage -> C.Encoding
encModule nameId _pkg =
    encStruct
      [ ("name", encW32 nameId)
      , ("content_hash", encList (replicate 32 (C.encodeWord8 0)))
      , ("clock_domains", encList [])
      , ("resets", encList [])
      , ("inputs", encList [])
      , ("instances", encList [])
      , ("defs", encList [])
      , ("rules", encList [])
      , ("methods", encList [])
      , ("schedule", encSchedule)
      ]

encSchedule :: C.Encoding
encSchedule =
    encStruct
      [ ("domains", encList [])
      , ("conflicts", encList [])
      , ("disjoint", encList [])
      ]
