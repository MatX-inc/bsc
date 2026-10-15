{-# LANGUAGE CPP #-}
-- The object format of the host: GHC defines the host OS macro.
module BuildSystem
  ( BinFmtType(..),
    binFmtToString,
    getBinFmtType,
  )
where

data BinFmtType = ELF | MachO

binFmtToString :: BinFmtType -> String
binFmtToString ELF   = "ELF"
binFmtToString MachO = "Mach-O"

getBinFmtType :: BinFmtType
#if defined(darwin_HOST_OS)
getBinFmtType = MachO
#else
getBinFmtType = ELF
#endif
