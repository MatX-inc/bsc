-- profiling-only: tiny leaf functions called at extreme frequency;
-- an SCC here blocks their inlining (distorting -fprof-auto
-- profiles) and their time belongs to callers
{-# OPTIONS_GHC -fno-prof-auto #-}
module IOMutVar(MutableVar, newVar, readVar, writeVar) where

import Data.IORef

type MutableVar a = IORef a

newVar :: a -> IO (IORef a)
newVar = newIORef

readVar :: IORef a -> IO a
readVar = readIORef

writeVar :: IORef a -> a -> IO ()
writeVar = writeIORef
