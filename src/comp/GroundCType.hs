module GroundCType(groundCTypeEnabled, internGroundCType,
                   groundCTypeStats) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.IORef(IORef, newIORef, readIORef, modifyIORef',
                  atomicModifyIORef')
import Data.List(genericLength, genericSplitAt)
import Control.Monad(foldM, when)
import System.IO.Unsafe(unsafePerformIO)
import System.Mem.StableName(StableName, makeStableName, hashStableName,
                             eqStableName)

import TypeShareFlags(shareTypesBoundary)
import Id(Id, getIdBase, getIdQual)
import CType(Type(..), TyCon(..), TISort(..), splitTAp)
import TypeOps(opNumT, opStrT, isPrimTFunName)
import Pred(Instantiate(..))
import Position(noPosition)
import FStringCompat(FString)
import PreStrings(fsEmpty)

-- =====
-- Ground-CType interning (the -hack-ground-ctype lever)
--
-- A side hash-cons table for GROUND types: one bottom-up walk converts
-- a type to a small node id AND the canonical, physically shared CType
-- node for its normal form, with O(1)-per-node table keys (child node
-- ids plus normalized leaf names).  Consumers that repeatedly need the
-- identity of large ground types -- the ground-dictionary pool's
-- probes, the CType-to-IType conversion memo -- pay one O(size) walk
-- instead of a synonym-expanding copy plus deep structural comparisons
-- per consultation; and consumers that RETAIN the type (the emitted
-- _lifted_dict definitions, the pool-hit alias bindings) store the one
-- canonical node instead of their own duplicate tree.
--
-- The canonical nodes are ordinary CTypes built from the same
-- constructors -- callers pattern match them like any other type; no
-- representation change is involved anywhere.  Sharing them is safe
-- precisely because they are ground: no substitution or zonking can
-- ever need to rewrite one.
--
-- A pointer-keyed fast path (StableName-indexed) makes re-interning an
-- already-canonical tree -- or any tree object interned before --
-- O(1) at the node where it is recognized, without descending; as
-- canonical nodes circulate through the boundaries above, repeated
-- consultations amortize toward O(1), and a canonical node fed to the
-- conversion memo costs one pointer probe, no walk.
--
-- The walk subsumes three jobs: the groundness check (a type variable
-- anywhere yields Nothing), synonym normalization (TItype applications
-- are expanded exactly as Pred.expandSyn expands them, and primitive
-- type functions over literals are evaluated exactly as its apTFun
-- does), and the key construction itself.  Equal node ids therefore
-- mean: both types are ground and reduce to the same synonym-free,
-- type-function-free normal form -- which is also the structure of the
-- canonical node.
--
-- The tables are process-global (the same unsafePerformIO/NOINLINE
-- discipline as IType's intern table): ground terms are immutable
-- under zonking and context-free, so they need no invalidation and no
-- scope tracking -- which is exactly why ground-only interning avoids
-- the complexity of full CType interning (a separate, deferred
-- design).  Node ids are arrival-order identifiers and must never
-- influence anything observable; they may only be used for identity.
--
-- Position policy: table keys carry no positions anywhere -- interior
-- nodes are keyed on child node ids, TCon leaves on (qualifier, base)
-- exactly like Id's own equality (idEq compares only the name
-- strings, excluding positions and IdProps), and TNum/TStr leaves on
-- their Integer/FString value alone (their Position fields excluded;
-- literal leaves are the highest-hit-rate entries, and position-
-- sensitive keys would silently zero their sharing).  Canonicalization
-- therefore conflates exactly what bsc's type equality already
-- conflates.  Canonical nodes keep their first-encounter positions
-- (inside TCon/TyNum/TyStr; a ground type contains no TVar or TGen,
-- so there are no other position carriers) -- the same conflation
-- .bo-imported types already exhibit; a literal produced by
-- type-function evaluation gets noPosition, as it has no single
-- source occurrence.  Predicate-level error positions (the position
-- lists of VPred/PredWithPositions) live outside the type and are
-- unaffected by the put-back; typecheck error anchors come from
-- those, and types print without positions.
--
-- A type is refused (Nothing) when interning cannot both establish
-- identity cheaply and guarantee a context-free normal form:
--   - it contains a type variable (not ground) or a TGen/TDefMonad;
--   - a synonym is recursive or partially applied (the normal paths
--     report those; the walk just declines);
--   - a primitive type function does not evaluate to a literal;
--   - it contains an associated type function (TIatf): those resolve
--     against the CURRENT symbol table's instances, so their normal
--     form is not context-free -- and precisely because reduction is
--     deferred to instance resolution, an unresolved ATF application
--     never names a stable ground type;
--   - a type constructor is unqualified: identity-by-name needs the
--     one-tycon-per-qualified-name invariant (cf. IType.mkITCon).

{-# NOINLINE groundCTypeEnabled #-}
groundCTypeEnabled :: Bool
groundCTypeEnabled = shareTypesBoundary

-- measurement counters for the -trace-ctype-stats dump: consultation
-- count, pointer-fast-path hits (root and in-walk), and nodes walked
-- (the "walk bytes" the lever-3 decision needs)
{-# NOINLINE cnGCIntern #-}
cnGCIntern :: IORef Int
cnGCIntern = unsafePerformIO $ newIORef 0

{-# NOINLINE cnGCRootPtrHit #-}
cnGCRootPtrHit :: IORef Int
cnGCRootPtrHit = unsafePerformIO $ newIORef 0

{-# NOINLINE cnGCWalkPtrHit #-}
cnGCWalkPtrHit :: IORef Int
cnGCWalkPtrHit = unsafePerformIO $ newIORef 0

{-# NOINLINE cnGCWalkNodes #-}
cnGCWalkNodes :: IORef Int
cnGCWalkNodes = unsafePerformIO $ newIORef 0

gcBump :: IORef Int -> IO ()
gcBump r = modifyIORef' r (+1)

-- | Counter/table snapshot for the -trace-ctype-stats dump.
groundCTypeStats :: IO [(String, Int)]
groundCTypeStats = do
    n1 <- readIORef cnGCIntern
    n2 <- readIORef cnGCRootPtrHit
    n3 <- readIORef cnGCWalkPtrHit
    n4 <- readIORef cnGCWalkNodes
    GCTable _ n <- readIORef gcTable
    return [ ("gctype.intern_calls", n1)
           , ("gctype.root_ptr_hit", n2)
           , ("gctype.walk_ptr_hit", n3)
           , ("gctype.walk_nodes", n4)
           , ("gctype.table_nodes", n)
           ]

-- The identity of a child inside a table key: interior applications
-- by their node ids, leaves by normalized name or value (positions,
-- kinds and sorts excluded: a qualified name determines its tycon
-- under the one-tycon-per-qualified-name invariant, and leaf ids
-- share one namespace with interior ids).
data GCKey
        = GCAp {-# UNPACK #-} !Int {-# UNPACK #-} !Int
        | GCCon !FString !FString   -- qualifier, base
        | GCNum !Integer
        | GCStr !FString
        deriving (Eq, Ord)

-- key -> (node id, canonical node); the canonical node is the
-- first-arrival structure, its children already canonical
data GCTable = GCTable !(M.Map GCKey (Int, Type)) {-# UNPACK #-} !Int

{-# NOINLINE gcTable #-}
gcTable :: IORef GCTable
gcTable = unsafePerformIO $ newIORef (GCTable M.empty 0)

-- The pointer fast path: heap objects known to intern to an entry --
-- every canonical interior node, and every root a caller has interned
-- -- keyed by StableName (bucketed on its hash; eqStableName resolves
-- within the bucket).  Objects are named only after forcing to WHNF:
-- a thunk and its value may name differently, so naming unforced
-- objects could only cause misses, never false hits -- force anyway
-- and keep the path deterministic.
data PtrTable = PtrTable !(IM.IntMap [(StableName Type, (Int, Type))])

{-# NOINLINE ptrTable #-}
ptrTable :: IORef PtrTable
ptrTable = unsafePerformIO $ newIORef (PtrTable IM.empty)

ptrLookup :: Type -> IO (Maybe (Int, Type))
ptrLookup t = t `seq` do
    sn <- makeStableName t
    PtrTable m <- readIORef ptrTable
    case IM.lookup (hashStableName sn) m of
      Nothing -> return Nothing
      Just bucket -> return (go sn bucket)
  where go sn ((sn', e) : rest) | eqStableName sn sn' = Just e
                                | otherwise = go sn rest
        go _ [] = Nothing

ptrInsert :: Type -> (Int, Type) -> IO ()
ptrInsert t e = t `seq` do
    sn <- makeStableName t
    atomicModifyIORef' ptrTable
        (\ (PtrTable m) ->
             (PtrTable (IM.insertWith (++) (hashStableName sn) [(sn, e)] m),
              ()))

-- the intern-table probe: the canonical candidate (its children
-- already canonical) is only forced if the key is new
nodeEntry :: GCKey -> Type -> IO (Int, Type)
nodeEntry key cand = do
    GCTable m0 _ <- readIORef gcTable
    case M.lookup key m0 of
      Just e  -> return e
      Nothing -> do
        (e@(_, canon), isNew) <- atomicModifyIORef' gcTable go
        -- a new interior node joins the pointer fast path, so walks
        -- over circulating canonical trees stop at it without
        -- descending
        when (isNew && isAp) $ ptrInsert canon e
        return e
  where
    isAp = case key of GCAp _ _ -> True
                       _        -> False
    go st@(GCTable m n) =
        case M.lookup key m of
          Just e  -> (st, (e, False))
          Nothing -> (GCTable (M.insert key (n, cand) m) (n+1),
                      ((n, cand), True))

-- the value view of an evaluated node, for primitive-type-function
-- evaluation; derivable from the canonical node, which is normalized
-- (a literal is literally a literal leaf)
data GCView = GVNum !Integer | GVStr !FString | GVOther

viewOf :: Type -> GCView
viewOf (TCon (TyNum n _)) = GVNum n
viewOf (TCon (TyStr s _)) = GVStr s
viewOf _ = GVOther

-- | Intern a ground type, returning its node id and the canonical
-- physically-shared node of its normal form: equal ids mean equal
-- synonym-expanded normal forms, and the canonical node IS that
-- normal form, safe to store in place of the argument.  Nothing when
-- the type is not ground or not safely internable (see the module
-- note).
{-# NOINLINE internGroundCType #-}
internGroundCType :: Type -> Maybe (Int, Type)
internGroundCType t = unsafePerformIO $ do
    gcBump cnGCIntern
    hit <- ptrLookup t
    case hit of
      Just e -> do gcBump cnGCRootPtrHit
                   return (Just e)
      Nothing -> do
        r <- walk [] t
        case r of
          Just e -> do ptrInsert t e   -- re-interning this object: O(1)
                       return (Just e)
          Nothing -> return Nothing

walk :: [Id] -> Type -> IO (Maybe (Int, Type))
walk syns t0 = do
    hit <- ptrLookup t0
    case hit of
      Just e -> do gcBump cnGCWalkPtrHit
                   return (Just e)
      Nothing -> do
        r <- walk' syns t0
        -- memoize every successfully walked OBJECT, not only interned
        -- roots: a duplicating synonym body puts the same raw node in
        -- both slots, and without this the walk re-expands it once
        -- per path (exponential on synonym towers).  Refusals are not
        -- cached: a refusal under a non-empty synonym stack (rec
        -- detection) does not transfer to other contexts.
        case r of
          Just e  -> ptrInsert t0 e
          Nothing -> return ()
        return r

walk' :: [Id] -> Type -> IO (Maybe (Int, Type))
walk' syns t0 = gcBump cnGCWalkNodes >>
    let (f, as) = splitTAp t0
    in  case f of
          TCon (TyCon i _ (TItype n body))
            | i `elem` syns -> return Nothing      -- recursive synonym
            | genericLength as < n -> return Nothing  -- partial application
            | otherwise ->
                -- expand like Pred.expandSyn: substitute the first n
                -- arguments into the body, keep the rest applied
                let (as1, as2) = genericSplitAt n as
                in  walk (i:syns) (foldl TAp (inst as1 body) as2)
          TCon (TyCon _ _ (TIatf {})) -> return Nothing
          TCon tc@(TyCon i _ _)
            | isPrimTFunName i -> do
                mks <- walkArgs syns as
                case mks of
                  Nothing -> return Nothing
                  Just ks ->
                    -- evaluate like Pred.apTFun; anything it would
                    -- leave structural is refused instead
                    case tfunVal i (map (viewOf . snd) ks) of
                      Just (Left n)  ->
                          Just <$> nodeEntry (GCNum n)
                                             (TCon (TyNum n noPosition))
                      Just (Right s) ->
                          Just <$> nodeEntry (GCStr s)
                                             (TCon (TyStr s noPosition))
                      Nothing        -> return Nothing
            | getIdQual i == fsEmpty -> return Nothing
            | otherwise -> do
                mks <- walkArgs syns as
                case mks of
                  Nothing -> return Nothing
                  Just ks -> do
                      e0 <- nodeEntry (GCCon (getIdQual i) (getIdBase i))
                                      (TCon tc)
                      e  <- foldM app e0 ks
                      return (Just e)
          TCon tc@(TyNum n _) | null as -> Just <$> nodeEntry (GCNum n) (TCon tc)
          TCon tc@(TyStr s _) | null as -> Just <$> nodeEntry (GCStr s) (TCon tc)
          _ -> return Nothing   -- TVar/TGen/TDefMonad/ill-kinded
  where
    app (k1, c1) (k2, c2) = nodeEntry (GCAp k1 k2) (TAp c1 c2)
    walkArgs sy ts = do
        mks <- mapM (walk sy) ts
        return (sequence mks)

tfunVal :: Id -> [GCView] -> Maybe (Either Integer FString)
tfunVal i [GVNum x, GVNum y] = Left <$> opNumT i [x, y]
tfunVal i [GVNum x]          = Left <$> opNumT i [x]
tfunVal i [GVStr x, GVStr y] = Right <$> opStrT i [x, y]
tfunVal _ _                  = Nothing
