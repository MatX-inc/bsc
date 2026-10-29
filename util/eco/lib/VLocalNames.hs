module VLocalNames (mapRenameableVIds, collectRenameableVIds) where

import Verilog
import qualified Data.Generics as Generic

-- Apply a VId transformation to every renameable identifier position in a
-- module body: everything except submodule instantiation port/parameter
-- names and submodule module names, which belong to the instantiated
-- module.  (Keep this in sync with collectRenameableVIds below.)
mapRenameableVIds :: (VId -> VId) -> [VMItem] -> [VMItem]
mapRenameableVIds f items = map go items
  where
    go inst@(VMInst { vi_inst_name = inm, vi_inst_params = iparams,
                      vi_inst_ports = iports }) =
        inst { vi_inst_name = f inm,
               vi_inst_params = [(pn, fmap gen me) | (pn,me) <- iparams],
               vi_inst_ports = [ (pn, fmap gen me) | (pn, me) <- iports ] }
    go (VMComment c it)      = VMComment c (go it)
    go (VMRegGroup a b c it) = VMRegGroup (f a) b c (go it)
    go (VMGroup t bss)       = VMGroup t (map (map go) bss)
    go item                  = gen item
    gen :: Generic.Data a => a -> a
    gen = Generic.everywhere (Generic.mkT f)

-- The identifiers that mapRenameableVIds would transform.
-- (Keep this in sync with mapRenameableVIds above.)
collectRenameableVIds :: [VMItem] -> [VId]
collectRenameableVIds items = concatMap go items
  where
    go (VMInst { vi_inst_name = inm, vi_inst_params = iparams,
                 vi_inst_ports = iports }) =
        inm : concatMap (maybe [] vids . snd) iparams
            ++ concatMap (maybe [] vids . snd) iports
    go (VMComment _ it)      = go it
    go (VMRegGroup a _ _ it) = a : go it
    go (VMGroup _ bss)       = concatMap (concatMap go) bss
    go item                  = vids item
    vids :: Generic.Data a => a -> [VId]
    vids = Generic.listify vidAny
      where vidAny :: VId -> Bool
            vidAny _ = True
