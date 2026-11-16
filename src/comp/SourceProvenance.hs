{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
-- Evaluator-derived source provenance carried as side-table metadata from
-- IExpand through IModule, APackage (.ba), and ASPackage/AOpt.
--
-- Like SourceTypeInfo, provenance entries are pure annotations and never act
-- as hardware liveness or use-count roots.  Keeping carrier expressions
-- parameterized over the live IR expression type (IExpr a -> AExpr) lets
-- existing substitution, inlining, state-port conversion, and CSE passes
-- rewrite base expressions alongside the module's definitions.
module SourceProvenance(
        NamedLitInfo(..),
        ProvStep(..),
        ProvFact(..),
        SourceProvenance,
        mapProvFact,
        mapMProvFact,
        mapProvenance,
        mapMProvenance,
        dedupFacts,
        dedupFactsBy
    ) where

#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 804)
import Prelude hiding ((<>))
#endif

import Eval
import PFPrint

-- Semantic identity of a literal created by the evaluator for an enum
-- constructor (Y con / Y ord), a tagged-union constructor test (Y tag), or a
-- dynamic array-update slot comparison (Y slot), matched to consuming nodes
-- via StableName (W records).  Retaining nli_val guards against algebraic
-- constant rewrites changing a literal's numeric value while keeping its slot.
data NamedLitInfo
  = NLitEnum
      { nli_type   :: !String
      , nli_qual   :: !String
      , nli_member :: !String
      , nli_conNo  :: !Int
      , nli_numCon :: !Int
      , nli_width  :: !Int
      , nli_val    :: !Integer
      }
  | NLitTag
      { nli_type   :: !String
      , nli_qual   :: !String
      , nli_con    :: !String
      , nli_conNo  :: !Int
      , nli_numCon :: !Int
      , nli_width  :: !Int
      , nli_val    :: !Integer
      , nli_isEnum :: !Bool
      }
  | NLitSlot
      { nli_slot   :: !Int
      , nli_len    :: !Int
      , nli_width  :: !Int
      , nli_val    :: !Integer
      }
  deriving (Eq, Ord, Show)

-- One typed projection or test step from an enclosing value to a component
-- (Y sel / sel*, Y elem / arr, Y out / out*, Y is / is*, or tag read).
-- Steps in PFactAccess are ordered outermost-to-innermost from the hardware
-- Bit carrier expression to the resulting expression.
data ProvStep
  = PStepField
      { ps_type     :: !String
      , ps_qual     :: !String
      , ps_field    :: !String
      , ps_fieldIdx :: !Int
      , ps_numField :: !Int
      , ps_isTuple  :: !Bool
      }
  | PStepElem
      { ps_elemIdx  :: !Int
      , ps_arrayLen :: !Int
      }
  | PStepVariantPayload
      { ps_type     :: !String
      , ps_qual     :: !String
      , ps_con      :: !String
      , ps_conIdx   :: !Int
      , ps_numCon   :: !Int
      }
  | PStepVariantTest
      { ps_type     :: !String
      , ps_qual     :: !String
      , ps_con      :: !String
      , ps_conIdx   :: !Int
      , ps_numCon   :: !Int
      , ps_isEnum   :: !Bool
      }
  | PStepVariantTag
      { ps_type     :: !String
      , ps_qual     :: !String
      , ps_numCon   :: !Int
      }
  deriving (Eq, Ord, Show)

-- Evaluator fact attached to an expression.  Multiple facts on one canonical
-- expression record genuine CSE merges (e.g. two tagged-union constructors
-- whose payloads occupy the same slice), preserved in first-sighting order.
data ProvFact expr
  = PFactAccess expr [ProvStep]
  | PFactConcat [expr]
  | PFactLit !NamedLitInfo
  | PFactArgLits [(Int, NamedLitInfo)]
  deriving (Eq, Ord, Show, Functor)

type SourceProvenance expr = [(expr, [ProvFact expr])]

mapProvFact :: (a -> b) -> ProvFact a -> ProvFact b
mapProvFact = fmap

mapMProvFact :: Monad m => (a -> m b) -> ProvFact a -> m (ProvFact b)
mapMProvFact f (PFactAccess base steps) = do
    base' <- f base
    return (PFactAccess base' steps)
mapMProvFact f (PFactConcat parts) = do
    parts' <- mapM f parts
    return (PFactConcat parts')
mapMProvFact _ (PFactLit lit) = return (PFactLit lit)
mapMProvFact _ (PFactArgLits kvs) = return (PFactArgLits kvs)

mapProvenance :: (a -> b) -> SourceProvenance a -> SourceProvenance b
mapProvenance f = map (\(e, fs) -> (f e, map (mapProvFact f) fs))

mapMProvenance :: Monad m => (a -> m b) -> SourceProvenance a -> m (SourceProvenance b)
mapMProvenance f = mapM $ \(e, fs) -> do
    e' <- f e
    fs' <- mapM (mapMProvFact f) fs
    return (e', fs')

-- First-sighting deduplication without requiring Ord on expressions.
dedupFacts :: Eq a => [ProvFact a] -> [ProvFact a]
dedupFacts = dedupFactsBy id

dedupFactsBy :: Eq k => (a -> k) -> [ProvFact a] -> [ProvFact a]
dedupFactsBy proj = go []
  where
    go _ [] = []
    go seen (x : xs) =
        let kx = fmap proj x
        in if kx `elem` seen
           then go seen xs
           else x : go (kx : seen) xs

instance NFData NamedLitInfo where
    rnf (NLitEnum a b c d e f g) = rnf7 a b c d e f g
    rnf (NLitTag a b c d e f g h) = rnf7 a b c d e f g `seq` rnf h
    rnf (NLitSlot a b c d) = rnf4 a b c d

instance NFData ProvStep where
    rnf (PStepField a b c d e f) = rnf6 a b c d e f
    rnf (PStepElem a b) = rnf2 a b
    rnf (PStepVariantPayload a b c d e) = rnf5 a b c d e
    rnf (PStepVariantTest a b c d e f) = rnf6 a b c d e f
    rnf (PStepVariantTag a b c) = rnf3 a b c

instance NFData a => NFData (ProvFact a) where
    rnf (PFactAccess base steps) = rnf2 base steps
    rnf (PFactConcat parts) = rnf parts
    rnf (PFactLit lit) = rnf lit
    rnf (PFactArgLits kvs) = rnf kvs

instance PPrint NamedLitInfo where
    pPrint _ _ (NLitEnum ty _ mem _ _ w v) =
        text (ty ++ "." ++ mem ++ "#" ++ show w ++ "'" ++ show v)
    pPrint _ _ (NLitTag ty _ con _ _ w v _) =
        text ("tag:" ++ ty ++ "." ++ con ++ "#" ++ show w ++ "'" ++ show v)
    pPrint _ _ (NLitSlot slot len w v) =
        text ("slot:" ++ show slot ++ "/" ++ show len ++ "#" ++ show w ++ "'" ++ show v)

instance PPrint ProvStep where
    pPrint _ _ (PStepField ty _ fld idx n isTpl)
      | isTpl = text ("." ++ fld ++ "(" ++ show idx ++ "/" ++ show n ++ ")")
      | otherwise = text ("." ++ ty ++ "." ++ fld ++ "(" ++ show idx ++ "/" ++ show n ++ ")")
    pPrint _ _ (PStepElem idx n) =
        text ("[" ++ show idx ++ "/" ++ show n ++ "]")
    pPrint _ _ (PStepVariantPayload ty _ con idx n) =
        text (".out:" ++ ty ++ "." ++ con ++ "(" ++ show idx ++ "/" ++ show n ++ ")")
    pPrint _ _ (PStepVariantTest ty _ con idx n _) =
        text (".is:" ++ ty ++ "." ++ con ++ "(" ++ show idx ++ "/" ++ show n ++ ")")
    pPrint _ _ (PStepVariantTag ty _ n) =
        text (".tag:" ++ ty ++ "(/" ++ show n ++ ")")

instance PPrint a => PPrint (ProvFact a) where
    pPrint d p (PFactAccess base steps) =
        pPrint d p base <> hcat (map (pPrint d 0) steps)
    pPrint d p (PFactConcat parts) =
        text "concat" <> parens (sepList (map (pPrint d p) parts) (text ","))
    pPrint d p (PFactLit lit) = pPrint d p lit
    pPrint d p (PFactArgLits kvs) =
        text "argLits" <> pPrint d p kvs
