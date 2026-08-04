module GroundCType(groundCTypeEnabled, internGroundCType,
                   isGroundNodeId) where

import qualified Data.Map.Strict as M
import qualified Data.IntMap.Strict as IM
import Data.IORef(IORef, newIORef, readIORef, atomicModifyIORef')
import Data.List(genericLength, genericSplitAt)
import Control.Monad(foldM, when)
import System.IO.Unsafe(unsafePerformIO)
import System.Mem.StableName(StableName, makeStableName, hashStableName,
                             eqStableName)

import Id(Id, getIdBase, getIdQual)
import CType(Type(..), TyCon(..), TISort(..), splitTAp)
import TypeOps(opNumT, opStrT, isPrimTFunName)
import Pred(Instantiate(..))
import Position(noPosition)
import FStringCompat(FString)
import PreStrings(fsEmpty)

-- =====
-- Ground-CType interning
--
-- A side hash-cons table for GROUND types: one bottom-up walk converts
-- a type to a small node id AND the canonical, physically shared CType
-- node for its normal form, with O(1)-per-node table keys (child node
-- ids plus normalized leaf names).  Consumers that repeatedly need the
-- identity of large ground types (e.g. a pool or memo keyed on ground
-- dictionary types, or a CType-to-IType conversion memo) pay one
-- O(size) walk instead of a synonym-expanding copy plus deep
-- structural comparisons per consultation; and consumers that RETAIN
-- the type can store the one canonical node instead of their own
-- duplicate tree.
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
-- design).  Node ids pack an arrival-order identifier with a
-- groundness bit (gcMkId); the arrival component must never influence
-- anything observable -- ids may only be used for identity (equality)
-- and for the groundness certificate (isGroundNodeId).
--
-- Canonical and ground are two different claims.  A node id certifies
-- identity -- this is the unique node for its structure -- and its
-- low bit certifies groundness; only isGroundNodeId may stand in for
-- "has no variables inside".  Every node interned today happens to be
-- ground (the walk refuses every variable-bearing type outright), so
-- the two coincide and every id is even; the separation exists so
-- that interning a variable-bearing node cannot silently widen what a
-- groundness-relying consumer skips.
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

-- The enablement point for consumers of the interner: always on.
-- (Interning is a pure side table -- a consumer that never calls
-- internGroundCType pays nothing -- so no command-line toggle is
-- needed; a future consumer wanting its own lever can gate its calls
-- here.)
groundCTypeEnabled :: Bool
groundCTypeEnabled = True

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

-- Ids pack (arrival order, groundness) so the two properties a
-- canonical node can certify stay separable.  They were fused while
-- only ground nodes were ever interned; keeping them fused is what
-- makes interning anything variable-bearing unsafe, because a single
-- test then answers two different questions.
gcMkId :: Int -> Bool -> Int
gcMkId n grnd = 2 * n + (if grnd then 0 else 1)

-- | Canonical AND ground: no TVar/TGen/TDefMonad anywhere inside the
-- node this id names, so substitution, instantiation, zonking and
-- free-variable collection are all the identity on it.  This is the
-- certificate a ground guard must rely on, and the ONLY test that may
-- stand in for "has no variables inside".
--
-- Today every interned node is ground, so this holds of every id the
-- interner hands out; the distinction exists so that interning a
-- variable-bearing node cannot silently widen what a ground guard
-- skips.
isGroundNodeId :: Int -> Bool
isGroundNodeId i = i >= 0 && even i

-- ground only if both children are: one variable-bearing child makes
-- the whole spine non-ground.  A leaf that interns at all is ground
-- (the walk refuses every variable-bearing leaf).
keyIsGround :: GCKey -> Bool
keyIsGround (GCAp k1 k2) = isGroundNodeId k1 && isGroundNodeId k2
keyIsGround _            = True

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
          Nothing -> let e = (gcMkId n (keyIsGround key), cand)
                     in  (GCTable (M.insert key e m) (n+1), (e, True))

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
    hit <- ptrLookup t
    case hit of
      Just e -> return (Just e)
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
      Just e -> return (Just e)
      Nothing -> walk' syns t0

walk' :: [Id] -> Type -> IO (Maybe (Int, Type))
walk' syns t0 =
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
