-- | The structural part of Bluesim's module-argument validation.
-- Scheduling checks the resulting arguments; artifact readers only replay
-- the inlining on an already validated module.
module ASimParams (aInlineSimParams) where

import qualified Data.Map as M
import ASyntax
import ASyntaxUtil (aSubst)
import VModInfo (VArgInfo(..), vArgs)

aInlineSimParams :: APackage -> APackage
aInlineSimParams apkg =
    let
        defs = apkg_local_defs apkg
        dmap = M.fromList [ (i, aSubst dmap e) | ADef i _ e _ <- defs ]

        inlineArg (Param {}, expr) = aSubst dmap expr
        inlineArg (Port {},  expr) = aSubst dmap expr
        inlineArg (_,        expr) = expr

        inlineArgs avi =
            let varginfo = vArgs (avi_vmi avi)
                es = avi_iargs avi
                es' = map inlineArg (zip varginfo es)
            in  avi { avi_iargs = es' }

        insts = apkg_state_instances apkg
        insts' = map inlineArgs insts
    in apkg { apkg_state_instances = insts' }
