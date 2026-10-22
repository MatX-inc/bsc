-- Checks on a module's SVA assertions before code generation: labels are
-- Verilog identifiers and unique, the property fits the chosen emission
-- style, and the backend can emit them at all.
module ASVA(checkSVA, svaKindOf, svaCallArgs) where

import Data.Char(isAlpha, isAlphaNum)
import Data.Maybe(isJust)
import Control.Monad(unless)

import Error(ErrorHandle, ErrMsg(..), bsError)
import ErrorUtil(internalError)
import PPrint(ppReadable)
import Position(getPosition)
import Id(getIdBaseString)
import Flags(Flags, backend, svaImmediate)
import Backend(Backend(..))
import Prim
import ASyntax
import Verilog(VAssertKind(..), vKeywords)

-- the foreign-function names of the Prelude's $SVA family
svaKindOf :: String -> Maybe VAssertKind
svaKindOf "$sva_assert" = Just VAssertKind
svaKindOf "$sva_assume" = Just VAssumeKind
svaKindOf "$sva_cover"  = Just VCoverKind
svaKindOf _ = Nothing

-- condition, label and property of a $SVA call's arguments
svaCallArgs :: [AExpr] -> (AExpr, String, AExpr)
svaCallArgs [c, ASStr _ _ label, prop] = (c, label, prop)
svaCallArgs es = internalError ("svaCallArgs: " ++ ppReadable es)

checkSVA :: ErrorHandle -> Flags -> APackage -> IO ()
checkSVA errh flags apkg = unless (null errs) (bsError errh errs)
  where
    modName = getIdBaseString (apkg_name apkg)
    calls = [ (getPosition (arule_id r), label, prop)
            | r <- apkg_rules apkg
            , AFCall { afcall_fun = f, aact_args = args } <- arule_actions r
            , isJust (svaKindOf f)
            , let (_, label, prop) = svaCallArgs args ]
    errs = [ (pos, ESVABadLabel modName l)
           | (pos, l, _) <- calls, not (validLabel l) ]
        ++ [ (pos, ESVADuplicateLabel modName l)
           | (i, (pos, l, _)) <- zip [(0::Int)..] calls
           , l `elem` [ l' | (_, l', _) <- take i calls ] ]
        ++ [ (pos, ESVAImmediateUnsupported modName l)
           | svaImmediate flags, (pos, l, p) <- calls, not (booleanOnly p) ]
        ++ [ (pos, EBluesimNoSVA modName l)
           | backend flags == Just Bluesim, (pos, l, _) <- calls ]

validLabel :: String -> Bool
validLabel [] = False
validLabel l@(c:cs) = (isAlpha c || c == '_') && all identChar cs && l `notElem` vKeywords
  where identChar x = isAlphaNum x || x == '_' || x == '$'

-- a bare design expression, which the immediate style can print
booleanOnly :: AExpr -> Bool
booleanOnly (APrim _ _ PrimSvaBits [_]) = True
booleanOnly _ = False
