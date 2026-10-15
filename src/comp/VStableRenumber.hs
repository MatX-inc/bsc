module VStableRenumber(stableRenumberVProgram) where

import Data.Bits(shiftR)
import Data.Char(isDigit, ord)
import Data.List(dropWhileEnd, sort, sortBy)
import Data.Maybe(listToMaybe, mapMaybe)
import Data.Word(Word64)
import qualified Data.Map as M
import qualified Data.Set as S

import Id(setIdBaseString)
import Util(Hash, hashInit, nextHashByte, hashValue)
import Verilog

-- Under -stable-verilog, renumber the compiler-generated name families
--
--     <stem>___dN   (backend def temporaries)
--     <stem>__hN    (elaboration heap ids, minted into the .ba)
--     <stem>__qN    (Verilog-quirks temporaries)
--     <stem>__fN    (ANoInline instance-connection wires)
--     _dmN          (AOpt-minted, whole name, no stem)
--     _dsN          (Synthesize-minted, whole name, no stem)
--     _dfooN        (ACleanup-minted, whole name, no stem)
--
-- per module, by the CONTENT of what each name is driven by, so that N
-- is a pure function of the member's own logic rather than of mint
-- order or of its position among the module's other members.
--
-- The content hash of a member covers its declared width and every
-- piece of the module that assigns it, in body order: continuous
-- assignments (whole, as an element of a concatenation, or to a part
-- select), each procedural assignment to it together with the control
-- path that leads there (the block kind and event, the enclosing if
-- branches and case arms), a foreign task that writes it through its
-- first argument, or the instance output port that drives it.  A reg
-- assigned in a block therefore depends on its own assignments and
-- their conditions, not on the rest of the block.  Inside
-- those, a reference to another family member contributes that
-- member's content hash, never its name; every other identifier (a
-- port, a state element, a submodule port, a rule or method signal,
-- a foreign function) contributes its name, which is where the hash
-- stops.  So N depends, ultimately, on the ports, the state elements
-- and the operations between them, and an unchanged definition keeps
-- its name whatever is added, removed or reordered around it.
--
-- N is the hash reduced to twelve decimal digits, so the family shape
-- <stem><marker><digits> is unchanged for everything that matches it.
-- Two members with the same stem and the same content (possible when
-- join-defs is off) or a twelve-digit collision are told apart by
-- rehashing in first-occurrence order; a target that would collide
-- with a non-renameable name is rehashed the same way.  Definitions
-- with no driver in the module hash to their stem alone, and a
-- reference that closes a cycle (none are expected: the backend's
-- definitions are acyclic) contributes the referenced stem, so the
-- pass always terminates and stays deterministic.
--
-- After the rename, the groups that AVerilog had sorted by name (the
-- merged declarations and the "remaining internal signals") are sorted
-- again, by the new names: a list sorted under the old names is
-- otherwise in old-name order, which is mint order.  The VMItem
-- ordering treats procedural items, instances and groups as equal, and
-- the sort is stable, so only declarations and continuous assignments
-- (order-independent in Verilog) ever move.
--
-- Soundness:
--  * Renaming is a per-module simultaneous substitution.  A target
--    name <stem><marker><k> can only collide with another family
--    member -- any name matching the pattern is itself in the map --
--    and family members all move at once.  Targets are made distinct
--    from one another and from non-renameable names before use.
--  * Names in non-renameable positions are never collected and never
--    produced as targets: the module's own name, everything in the
--    port list, instantiated module names, instance names, and the
--    formal port/parameter keys of instantiations (they belong to the
--    child module).  If a family-shaped name also occurs in such a
--    position (a user port named x__h5), the whole name is left
--    untouched to avoid capture.
--  * This runs before removeDollarsFromVerilog; generated family
--    names never contain '$', so the passes are disjoint.
--
-- The traversals below are written out per constructor rather than
-- derived generically.  Constructors are matched without a wildcard so
-- that adding one to Verilog.hs is a compile error here rather than a
-- name that escapes the walk.

stableRenumberVProgram :: VProgram -> VProgram
stableRenumberVProgram (VProgram ms dpis cs) =
    VProgram (map renumberModule ms) dpis cs

data Family = FamD | FamH | FamQ | FamF | FamDM | FamDS | FamDFoo
    deriving (Eq, Ord)

-- in the order matchFamily tries them; the first match wins
families :: [Family]
families = [FamD, FamH, FamQ, FamF, FamDM, FamDS, FamDFoo]

markerText :: Family -> String
markerText FamD  = "___d"
markerText FamH  = "__h"
markerText FamQ  = "__q"
markerText FamF  = "__f"   -- ANoInline instance-connection wires
markerText FamDM = "_dm"   -- AOpt-minted names (whole name, no stem)
markerText FamDS = "_ds"   -- Synthesize-minted names (whole name, no stem)
markerText FamDFoo = "_dfoo" -- ACleanup-minted names (whole name, no stem)

-- whether the family's names are the bare marker plus number
-- (no stem before the marker)
stemlessOK :: Family -> Bool
stemlessOK FamDM = True
stemlessOK FamDS = True
stemlessOK FamDFoo = True
stemlessOK _     = False

-- split a name into (stem, family, part) at the rightmost
-- marker-followed-by-digits suffix.  The stem is built only for the
-- family that matches.  A tuple-typed def is printed as one wire per
-- element, <name>_k (AVerilogUtil.tupleElemVId): the part suffix _k is
-- kept and the number before it is the one that is renumbered.
matchFamily :: String -> Maybe (String, Family, String)
matchFamily s =
    case matchBare s of
      Just (stem, fam) -> Just (stem, fam, "")
      Nothing ->
          let digits = reverse (takeWhile isDigit (reverse s))
              rest   = dropWhileEnd isDigit s
          in  if null digits || null rest || last rest /= '_'
              then Nothing
              else case matchBare (init rest) of
                     Just (stem, fam) -> Just (stem, fam, '_' : digits)
                     Nothing          -> Nothing

matchBare :: String -> Maybe (String, Family)
matchBare s
    | null s || not (isDigit (last s)) = Nothing
    | otherwise = listToMaybe (mapMaybe try families)
  where
    rest = dropWhileEnd isDigit s
    try fam =
        let m = markerText fam
            n = length rest - length m
        in  if n >= 0 && (if stemlessOK fam then n == 0 else n > 0)
               && drop n rest == m
            then Just (take n rest, fam)
            else Nothing

-- The sets and the rename map below are used for membership and lookup only,
-- so the injective transformation from String -> (Hash,String) keys is sound.
-- The hash provides fast inequality detection for long strings with common
-- prefixes, and the string comparison cost is only paid when there is a hash
-- collision.
data NameKey = NameKey !Word64 String
    deriving (Eq, Ord)

nameKey :: String -> NameKey
nameKey s = NameKey (hashName s) s

hashName :: String -> Word64
hashName = hashValue . feedString hashInit

-- ------------------------------------------------------------ hash feeding

-- A string's bytes followed by a terminator, so that consecutive fields
-- cannot run together.
feedString :: Hash -> String -> Hash
feedString h s = nextHashByte (foldl' (\a c -> nextHashByte a (fromIntegral (ord c))) h s) 0

feedWord :: Hash -> Word64 -> Hash
feedWord h w = foldl' (\a i -> nextHashByte a (fromIntegral (w `shiftR` (8 * i)))) h [0 .. 7]

feedShow :: Show a => Hash -> a -> Hash
feedShow h x = feedString h (show x)

-- One walk covers all three of the following using the Hit data type:
-- 1. Collect every identifier occurrence.
-- 2. Collect every foreign function name.
-- 3. Collect every system task name.
data Hit = HId VId | HForeign String

-- ---------------------------------------------------------------- collection
--
-- Accumulator-passing, with the accumulator named in every equation so that
-- every call is saturated.  Written point-free with (.) instead, each node
-- allocates a partial application per child plus a closure per composition,
-- which costs more than the intermediate lists it was meant to avoid.
-- The accumulator is never forced, so the walk still streams and costs stack
-- proportional to the depth of the tree rather than the length of the list.

hitsItems :: [VMItem] -> [Hit] -> [Hit]
hitsItems its acc = foldr hitsItem acc its

hitsItem :: VMItem -> [Hit] -> [Hit]
hitsItem (VMDecl d) acc            = hitsDecl d acc
hitsItem (VMInst m i pas pos) acc  =
    hitsId m (hitsId i (foldr hitsAssoc (foldr hitsAssoc acc pos) pas))
hitsItem (VMAssign l e) acc        = hitsLV l (hitsExpr e acc)
hitsItem (VMStmt _ s) acc          = hitsStmt s acc
hitsItem (VMComment _ it) acc      = hitsItem it acc
hitsItem (VMRegGroup i _ _ it) acc = hitsId i (hitsItem it acc)
hitsItem (VMGroup _ itss) acc      = foldr hitsItems acc itss
hitsItem (VMFunction f) acc        = hitsFunction f acc
hitsItem (VMDPI d) acc             = hitsDPI d acc

hitsAssoc :: (VId, Maybe VExpr) -> [Hit] -> [Hit]
hitsAssoc (v, me) acc = hitsId v (hitsMExpr me acc)

hitsMExpr :: Maybe VExpr -> [Hit] -> [Hit]
hitsMExpr Nothing acc  = acc
hitsMExpr (Just e) acc = hitsExpr e acc

-- A VId carries an optional inlined-register item, which the walk enters
-- after the identifier itself.
hitsId :: VId -> [Hit] -> [Hit]
hitsId v@(VId _ _ Nothing) acc   = HId v : acc
hitsId v@(VId _ _ (Just it)) acc = HId v : hitsItem it acc

hitsFunction :: VFunction -> [Hit] -> [Hit]
hitsFunction (VFunction n mr ds s) acc =
    hitsId n (hitsMRange mr (foldr hitsDecl (hitsStmt s acc) ds))

hitsDPI :: VDPI -> [Hit] -> [Hit]
hitsDPI (VDPI n _ _ _ args) acc =
    hitsId n (foldr (\(a, _, _) r -> hitsId a r) acc args)

hitsDecl :: VVDecl -> [Hit] -> [Hit]
hitsDecl (VVDecl _ mr vs) acc = hitsMRange mr (foldr hitsVar acc vs)
hitsDecl (VVDWire mr v e) acc = hitsMRange mr (hitsVar v (hitsExpr e acc))

hitsVar :: VVar -> [Hit] -> [Hit]
hitsVar (VVar v) acc     = hitsId v acc
hitsVar (VArray r v) acc = hitsRange r (hitsId v acc)

hitsMRange :: Maybe VRange -> [Hit] -> [Hit]
hitsMRange Nothing acc  = acc
hitsMRange (Just r) acc = hitsRange r acc

hitsRange :: VRange -> [Hit] -> [Hit]
hitsRange (e1, e2) acc = hitsExpr e1 (hitsExpr e2 acc)

hitsLV :: VLValue -> [Hit] -> [Hit]
hitsLV (VLId v) acc      = hitsId v acc
hitsLV (VLConcat ls) acc = foldr hitsLV acc ls
hitsLV (VLSub l e) acc   = hitsLV l (hitsExpr e acc)

hitsStmt :: VStmt -> [Hit] -> [Hit]
hitsStmt (VAt ee s) acc         = hitsEvent ee (hitsStmt s acc)
hitsStmt (Valways s) acc        = hitsStmt s acc
hitsStmt (Vinitial s) acc       = hitsStmt s acc
hitsStmt (VSeq ss) acc          = foldr hitsStmt acc ss
hitsStmt (Vcasex e as _ _) acc  = hitsExpr e (foldr hitsArm acc as)
hitsStmt (Vcase e as _ _) acc   = hitsExpr e (foldr hitsArm acc as)
hitsStmt (VAssign l e) acc      = hitsLV l (hitsExpr e acc)
hitsStmt (VAssignA l e) acc     = hitsLV l (hitsExpr e acc)
hitsStmt (Vif e s) acc          = hitsExpr e (hitsStmt s acc)
hitsStmt (Vifelse e s1 s2) acc  = hitsExpr e (hitsStmt s1 (hitsStmt s2 acc))
hitsStmt (Vdumpvars _ vs) acc   = foldr hitsId acc vs
hitsStmt (VTask t es) acc       =
    HForeign (getVIdString t) : hitsId t (foldr hitsExpr acc es)
hitsStmt (VAssert ee es) acc    = hitsEvent ee (foldr hitsExpr acc es)
hitsStmt VZeroDelay acc         = acc

hitsArm :: VCaseArm -> [Hit] -> [Hit]
hitsArm (VCaseArm es s) acc = foldr hitsExpr (hitsStmt s acc) es
hitsArm (VDefault s) acc    = hitsStmt s acc

hitsEvent :: VEventExpr -> [Hit] -> [Hit]
hitsEvent (VEEOr e1 e2) acc  = hitsEvent e1 (hitsEvent e2 acc)
hitsEvent (VEEposedge e) acc = hitsExpr e acc
hitsEvent (VEEnegedge e) acc = hitsExpr e acc
hitsEvent (VEE e) acc        = hitsExpr e acc
hitsEvent (VEEMacro _ e) acc = hitsExpr e acc

hitsExpr :: VExpr -> [Hit] -> [Hit]
hitsExpr (VEConst _) acc         = acc
hitsExpr (VEReal _) acc          = acc
hitsExpr (VEWConst v _ _ _) acc  = hitsId v acc
hitsExpr (VEUnknown _ _) acc     = acc
hitsExpr (VEString _) acc        = acc
hitsExpr (VETriConst _) acc      = acc
hitsExpr (VEUnOp v _ e) acc      = hitsId v (hitsExpr e acc)
hitsExpr (VEOp v e1 _ e2) acc    = hitsId v (hitsExpr e1 (hitsExpr e2 acc))
hitsExpr (VEVar v) acc           = hitsId v acc
hitsExpr (VEConcat es) acc       = foldr hitsExpr acc es
hitsExpr (VEIndex v e) acc       = hitsId v (hitsExpr e acc)
hitsExpr (VESelect e1 e2 e3) acc = hitsExpr e1 (hitsExpr e2 (hitsExpr e3 acc))
hitsExpr (VESelect1 e1 e2) acc   = hitsExpr e1 (hitsExpr e2 acc)
hitsExpr (VERepeat e1 e2) acc    = hitsExpr e1 (hitsExpr e2 acc)
hitsExpr (VEIf e1 e2 e3) acc     = hitsExpr e1 (hitsExpr e2 (hitsExpr e3 acc))
hitsExpr (VEFctCall f es) acc    =
    HForeign (getVIdString f) : hitsId f (foldr hitsExpr acc es)
hitsExpr (VEMacro _) acc         = acc

-- port-list identifiers, which are excluded from renaming
portIds :: [([VArg], VComment)] -> [VId]
portIds vps = [ i | (args, _) <- vps, a <- args, i <- argIds a ]
  where
    argIds (VAInput i mr)        = i : rangeIds' mr
    argIds (VAInout i mi mmr)    = i : maybe [] (: []) mi
                                     ++ maybe [] rangeIds' mmr
    argIds (VAOutput i mr)       = i : rangeIds' mr
    argIds (VAParameter i mr e _) = i : rangeIds' mr ++ exprIds e
    rangeIds' = maybe [] (\(e1, e2) -> exprIds e1 ++ exprIds e2)
    exprIds e = [ v | HId v <- hitsExpr e [] ]

-- ------------------------------------------------------------------- rewrite

mapItems :: (VId -> VId) -> [VMItem] -> [VMItem]
mapItems f = map (mapItem f)

mapItem :: (VId -> VId) -> VMItem -> VMItem
mapItem f (VMDecl d)            = VMDecl (mapDecl f d)
mapItem f (VMInst m i pas pos)  = VMInst (mapId f m) (mapId f i)
                                         (map (mapAssoc f) pas)
                                         (map (mapAssoc f) pos)
mapItem f (VMAssign l e)        = VMAssign (mapLV f l) (mapExpr f e)
mapItem f (VMStmt t s)          = VMStmt t (mapStmt f s)
mapItem f (VMComment c it)      = VMComment c (mapItem f it)
mapItem f (VMRegGroup i s c it) = VMRegGroup (mapId f i) s c (mapItem f it)
mapItem f (VMGroup t itss)      = VMGroup t (map (mapItems f) itss)
mapItem f (VMFunction fn)       = VMFunction (mapFunction f fn)
mapItem f (VMDPI d)             = VMDPI (mapDPI f d)

mapAssoc :: (VId -> VId) -> (VId, Maybe VExpr) -> (VId, Maybe VExpr)
mapAssoc f (v, me) = (mapId f v, fmap (mapExpr f) me)

mapId :: (VId -> VId) -> VId -> VId
mapId f v =
    case f v of
      VId s i mitem -> VId s i (fmap (mapItem f) mitem)

mapFunction :: (VId -> VId) -> VFunction -> VFunction
mapFunction f (VFunction n mr ds s) =
    VFunction (mapId f n) (mapMRange f mr) (map (mapDecl f) ds) (mapStmt f s)

mapDPI :: (VId -> VId) -> VDPI -> VDPI
mapDPI f (VDPI n a b t args) =
    VDPI (mapId f n) a b t [ (mapId f i, x, y) | (i, x, y) <- args ]

mapDecl :: (VId -> VId) -> VVDecl -> VVDecl
mapDecl f (VVDecl t mr vs) = VVDecl t (mapMRange f mr) (map (mapVar f) vs)
mapDecl f (VVDWire mr v e) = VVDWire (mapMRange f mr) (mapVar f v) (mapExpr f e)

mapVar :: (VId -> VId) -> VVar -> VVar
mapVar f (VVar v)     = VVar (mapId f v)
mapVar f (VArray r v) = VArray (mapRange f r) (mapId f v)

mapMRange :: (VId -> VId) -> Maybe VRange -> Maybe VRange
mapMRange f = fmap (mapRange f)

mapRange :: (VId -> VId) -> VRange -> VRange
mapRange f (e1, e2) = (mapExpr f e1, mapExpr f e2)

mapLV :: (VId -> VId) -> VLValue -> VLValue
mapLV f (VLId v)      = VLId (mapId f v)
mapLV f (VLConcat ls) = VLConcat (map (mapLV f) ls)
mapLV f (VLSub l e)   = VLSub (mapLV f l) (mapExpr f e)

mapStmt :: (VId -> VId) -> VStmt -> VStmt
mapStmt f (VAt ee s)         = VAt (mapEvent f ee) (mapStmt f s)
mapStmt f (Valways s)        = Valways (mapStmt f s)
mapStmt f (Vinitial s)       = Vinitial (mapStmt f s)
mapStmt f (VSeq ss)          = VSeq (map (mapStmt f) ss)
mapStmt f (Vcasex e as p l)  = Vcasex (mapExpr f e) (map (mapArm f) as) p l
mapStmt f (Vcase e as p l)   = Vcase (mapExpr f e) (map (mapArm f) as) p l
mapStmt f (VAssign l e)      = VAssign (mapLV f l) (mapExpr f e)
mapStmt f (VAssignA l e)     = VAssignA (mapLV f l) (mapExpr f e)
mapStmt f (Vif e s)          = Vif (mapExpr f e) (mapStmt f s)
mapStmt f (Vifelse e s1 s2)  = Vifelse (mapExpr f e) (mapStmt f s1)
                                       (mapStmt f s2)
mapStmt f (Vdumpvars n vs)   = Vdumpvars n (map (mapId f) vs)
mapStmt f (VTask t es)       = VTask (mapId f t) (map (mapExpr f) es)
mapStmt f (VAssert ee es)    = VAssert (mapEvent f ee) (map (mapExpr f) es)
mapStmt _ VZeroDelay         = VZeroDelay

mapArm :: (VId -> VId) -> VCaseArm -> VCaseArm
mapArm f (VCaseArm es s) = VCaseArm (map (mapExpr f) es) (mapStmt f s)
mapArm f (VDefault s)    = VDefault (mapStmt f s)

mapEvent :: (VId -> VId) -> VEventExpr -> VEventExpr
mapEvent f (VEEOr e1 e2)  = VEEOr (mapEvent f e1) (mapEvent f e2)
mapEvent f (VEEposedge e) = VEEposedge (mapExpr f e)
mapEvent f (VEEnegedge e) = VEEnegedge (mapExpr f e)
mapEvent f (VEE e)        = VEE (mapExpr f e)
mapEvent f (VEEMacro s e) = VEEMacro s (mapExpr f e)

mapExpr :: (VId -> VId) -> VExpr -> VExpr
mapExpr _ e@(VEConst _)        = e
mapExpr _ e@(VEReal _)         = e
mapExpr f (VEWConst v a b c)   = VEWConst (mapId f v) a b c
mapExpr _ e@(VEUnknown _ _)    = e
mapExpr _ e@(VEString _)       = e
mapExpr _ e@(VETriConst _)     = e
mapExpr f (VEUnOp v o e)       = VEUnOp (mapId f v) o (mapExpr f e)
mapExpr f (VEOp v e1 o e2)     = VEOp (mapId f v) (mapExpr f e1) o
                                      (mapExpr f e2)
mapExpr f (VEVar v)            = VEVar (mapId f v)
mapExpr f (VEConcat es)        = VEConcat (map (mapExpr f) es)
mapExpr f (VEIndex v e)        = VEIndex (mapId f v) (mapExpr f e)
mapExpr f (VESelect e1 e2 e3)  = VESelect (mapExpr f e1) (mapExpr f e2)
                                          (mapExpr f e3)
mapExpr f (VESelect1 e1 e2)    = VESelect1 (mapExpr f e1) (mapExpr f e2)
mapExpr f (VERepeat e1 e2)     = VERepeat (mapExpr f e1) (mapExpr f e2)
mapExpr f (VEIf e1 e2 e3)      = VEIf (mapExpr f e1) (mapExpr f e2)
                                      (mapExpr f e3)
mapExpr f (VEFctCall fn es)    = VEFctCall (mapId f fn) (map (mapExpr f) es)
mapExpr _ e@(VEMacro _)        = e

-- ---------------------------------------------------------------- drivers

-- What drives a family member: every piece of the module that assigns it,
-- in body order.
data Frag
    = FAssign VExpr                      -- assign v = e;  wire v = e;
    | FConcat Int Int VLValue VExpr      -- assign {.. v ..} = e; the k-th of n elements
    | FSub VLValue VExpr                 -- assign v[i] = e;
    | FProc [Guard] Proc                 -- a procedural assignment under its control path
    | FInst String String String         -- driven by an instance port: module, instance, port

-- the control path to a procedural assignment, outermost first
data Guard
    = GEvent Int VEventExpr              -- always @(...), numbered per item: hashed once, not once per assignment
    | GBlock String                      -- always / initial
    | GIf VExpr Bool                     -- the then (True) or else branch
    | GCase String VExpr [VExpr]         -- a case/casex arm with these labels
    | GDefault String VExpr              -- the default arm

data Proc
    = PAssign Bool VLValue VExpr         -- non-blocking (True) or blocking assignment
    | PTask String [VExpr]               -- a foreign task writing the target through its first argument

data Drivers = Drivers {
    drFrags :: M.Map NameKey [Frag],              -- reversed; FInst never here
    drInsts :: M.Map NameKey Frag,                -- first instance port driving the name
    drDecls :: M.Map NameKey (Maybe VRange, Maybe VRange),  -- declared range, array range
    drNext  :: Int                                -- the next procedural item number
  }

noDrivers :: Drivers
noDrivers = Drivers M.empty M.empty M.empty 0

addFrag :: NameKey -> Frag -> Drivers -> Drivers
addFrag k f d = d { drFrags = M.insertWith (++) k [f] (drFrags d) }

addInst :: NameKey -> Frag -> Drivers -> Drivers
addInst k f d = d { drInsts = M.insertWith (\_ old -> old) k f (drInsts d) }

addDecl :: NameKey -> (Maybe VRange, Maybe VRange) -> Drivers -> Drivers
addDecl k r d = d { drDecls = M.insertWith (\_ old -> old) k r (drDecls d) }

-- the base identifier of an lvalue, if it has a single one
lvBase :: VLValue -> Maybe VId
lvBase (VLId v)      = Just v
lvBase (VLSub l _)   = lvBase l
lvBase (VLConcat _)  = Nothing

collectItems :: (String -> Bool) -> [VMItem] -> Drivers -> Drivers
collectItems isM its d = foldl' (flip (collectItem isM)) d its

collectItem :: (String -> Bool) -> VMItem -> Drivers -> Drivers
collectItem isM (VMDecl dcl) d            = collectDecl isM dcl d
collectItem isM (VMInst m i _ pos) d      = foldl' port d pos
  where
    port acc (p, Just (VEVar v))
        | isM (getVIdString v) =
            addInst (nameKey (getVIdString v))
                    (FInst (getVIdString m) (getVIdString i) (getVIdString p)) acc
    port acc _ = acc
collectItem isM (VMAssign l e) d          = collectAssign isM l e d
collectItem isM (VMStmt _ s) d            = collectStmt isM s d
collectItem isM (VMComment _ it) d        = collectItem isM it d
collectItem isM (VMRegGroup _ _ _ it) d   = collectItem isM it d
collectItem isM (VMGroup _ itss) d        = foldl' (\acc its -> collectItems isM its acc) d itss
collectItem _   (VMFunction _) d          = d
collectItem _   (VMDPI _) d               = d

collectDecl :: (String -> Bool) -> VVDecl -> Drivers -> Drivers
collectDecl isM (VVDecl _ mr vs) d = foldl' (declVar isM mr) d vs
collectDecl isM (VVDWire mr v e) d =
    let d' = declVar isM mr d v
        i  = case v of
               VVar x     -> x
               VArray _ x -> x
    in  if isM (getVIdString i)
        then addFrag (nameKey (getVIdString i)) (FAssign e) d'
        else d'

declVar :: (String -> Bool) -> Maybe VRange -> Drivers -> VVar -> Drivers
declVar isM mr d (VVar i)
    | isM (getVIdString i) = addDecl (nameKey (getVIdString i)) (mr, Nothing) d
declVar isM mr d (VArray r i)
    | isM (getVIdString i) = addDecl (nameKey (getVIdString i)) (mr, Just r) d
declVar _ _ d _ = d

collectAssign :: (String -> Bool) -> VLValue -> VExpr -> Drivers -> Drivers
collectAssign isM l e d =
    case l of
      VLId v | isM (getVIdString v) ->
          addFrag (nameKey (getVIdString v)) (FAssign e) d
      VLConcat ls ->
          let n = length ls
              elt acc (k, lv) =
                  case lvBase lv of
                    Just v | isM (getVIdString v) ->
                        addFrag (nameKey (getVIdString v)) (FConcat k n lv e) acc
                    _ -> acc
          in  foldl' elt d (zip [1 ..] ls)
      VLSub _ _ ->
          case lvBase l of
            Just v | isM (getVIdString v) ->
                addFrag (nameKey (getVIdString v)) (FSub l e) d
            _ -> d
      _ -> d

collectStmt :: (String -> Bool) -> VStmt -> Drivers -> Drivers
collectStmt isM s d =
    let n = drNext d
    in  foldl' (\acc (v, f) -> if isM (getVIdString v)
                               then addFrag (nameKey (getVIdString v)) f acc
                               else acc)
               d { drNext = n + 1 } (procs n [] s [])

-- the procedural assignments of a statement with their control paths
-- (innermost guard first while walking; stored outermost first)
procs :: Int -> [Guard] -> VStmt -> [(VId, Frag)] -> [(VId, Frag)]
procs n gs (VAt ee s) acc         = procs n (GEvent n ee : gs) s acc
procs n gs (Valways s) acc        = procs n (GBlock "always" : gs) s acc
procs n gs (Vinitial s) acc       = procs n (GBlock "initial" : gs) s acc
procs n gs (VSeq ss) acc          = foldr (procs n gs) acc ss
procs n gs (Vcasex e as _ _) acc  = foldr (procArm n gs "casex" e) acc as
procs n gs (Vcase e as _ _) acc   = foldr (procArm n gs "case" e) acc as
procs _ gs (VAssign l e) acc      = procLV gs False l e acc
procs _ gs (VAssignA l e) acc     = procLV gs True l e acc
procs n gs (Vif e s) acc          = procs n (GIf e True : gs) s acc
procs n gs (Vifelse e s1 s2) acc  = procs n (GIf e True : gs) s1 (procs n (GIf e False : gs) s2 acc)
procs _ _  (Vdumpvars _ _) acc    = acc
procs _ gs (VTask t (VEVar v : es)) acc
    | isForeignTask (getVIdString t) =
        (v, FProc (reverse gs) (PTask (getVIdString t) es)) : acc
procs _ _  (VTask _ _) acc        = acc
procs _ _  (VAssert _ _) acc      = acc
procs _ _  VZeroDelay acc         = acc

procArm :: Int -> [Guard] -> String -> VExpr -> VCaseArm -> [(VId, Frag)] -> [(VId, Frag)]
procArm n gs kind e (VCaseArm ls s) acc = procs n (GCase kind e ls : gs) s acc
procArm n gs kind e (VDefault s) acc    = procs n (GDefault kind e : gs) s acc

-- the one reg a procedural item assigns, if it assigns exactly one
singleTarget :: VMItem -> Maybe String
singleTarget (VMComment _ it)      = singleTarget it
singleTarget (VMRegGroup _ _ _ it) = singleTarget it
singleTarget (VMStmt _ s) =
    case S.toList (S.fromList [ getVIdString v | (v, FProc _ (PAssign _ _ _)) <- procs 0 [] s [] ]) of
      [t] -> Just t
      _   -> Nothing
singleTarget _ = Nothing

procLV :: [Guard] -> Bool -> VLValue -> VExpr -> [(VId, Frag)] -> [(VId, Frag)]
procLV gs nb l e acc =
    let frag = FProc (reverse gs) (PAssign nb l e)
        elems = case l of
                  VLConcat ls -> ls
                  _           -> [l]
    in  foldr (\lv a -> case lvBase lv of
                          Just v  -> (v, frag) : a
                          Nothing -> a) acc elems

-- A system task ($display and the like) assigns nothing.  A foreign
-- function called as a task (AVerilogUtil: $imported_<f>, or the DPI
-- name, which has no '$') writes its result through its first argument.
-- A void foreign task takes an input there instead; such a name has a
-- driver of its own, which fragsOf prefers.
isForeignTask :: String -> Bool
isForeignTask n = take 10 n == "$imported_" || take 1 n /= "$"

-- the identifiers a driver mentions (for the dependency order)
fragHits :: Frag -> [Hit]
fragHits (FAssign e)         = hitsExpr e []
fragHits (FConcat _ _ l e)   = hitsLV l (hitsExpr e [])
fragHits (FSub l e)          = hitsLV l (hitsExpr e [])
fragHits (FProc gs p)        = foldr guardHits (procHits p []) gs
fragHits (FInst _ _ _)       = []

guardHits :: Guard -> [Hit] -> [Hit]
guardHits (GEvent _ ee) acc   = hitsEvent ee acc
guardHits (GBlock _) acc      = acc
guardHits (GIf e _) acc       = hitsExpr e acc
guardHits (GCase _ e ls) acc  = hitsExpr e (foldr hitsExpr acc ls)
guardHits (GDefault _ e) acc  = hitsExpr e acc

procHits :: Proc -> [Hit] -> [Hit]
procHits (PAssign _ l e) acc  = hitsLV l (hitsExpr e acc)
procHits (PTask _ es) acc     = foldr hitsExpr acc es

-- ---------------------------------------------------------------- hashing

data Ctx = Ctx {
    cxEnv  :: M.Map NameKey Word64,  -- content hashes computed so far
    cxEv   :: M.Map Int Word64,      -- event-list hashes, by procedural item number
    cxIsM  :: String -> Bool,        -- a renameable family member?
    cxLhs  :: M.Map NameKey Int      -- the current driver's own targets, by position
  }

positions :: [NameKey] -> M.Map NameKey Int
positions ks = M.fromList (zip ks [1 ..])

-- a reference: the member's content, a target's position, or the name
hRef :: Ctx -> VId -> Hash -> Hash
hRef cx v h =
    let s = getVIdString v
        k = nameKey s
    in  case M.lookup k (cxLhs cx) of
          Just i -> feedShow (feedString h "L") i
          Nothing
            | cxIsM cx s ->
                case M.lookup k (cxEnv cx) of
                  Just w  -> feedWord (feedString h "R") w
                  Nothing -> -- closes a cycle: the stem stands in
                      feedString (feedString h "Y")
                                 (maybe s (\(stem, _, _) -> stem) (matchFamily s))
            | otherwise -> feedString (feedString h "N") s

hExpr :: Ctx -> VExpr -> Hash -> Hash
hExpr _  (VEConst i) h           = feedShow (feedString h "c") i
hExpr _  (VEReal r) h            = feedShow (feedString h "r") r
hExpr _  (VEWConst _ w b v) h    = feedShow (feedString h "C") (w, b, v)
hExpr _  (VEUnknown w s) h       = feedString (feedShow (feedString h "u") w) s
hExpr _  (VEString s) h          = feedString (feedString h "s") s
hExpr _  (VETriConst ts) h       = feedShow (feedString h "t") ts
hExpr cx (VEUnOp _ o e) h        = hExpr cx e (feedShow (feedString h "U") o)
hExpr cx (VEOp _ e1 o e2) h      = hExpr cx e2 (hExpr cx e1 (feedShow (feedString h "O") o))
hExpr cx (VEVar v) h             = hRef cx v (feedString h "V")
hExpr cx (VEConcat es) h         = feedString (foldl' (flip (hExpr cx)) (feedString h "{") es) "}"
hExpr cx (VEIndex v e) h         = hExpr cx e (hRef cx v (feedString h "X"))
hExpr cx (VESelect e1 e2 e3) h   = hExpr cx e3 (hExpr cx e2 (hExpr cx e1 (feedString h "P")))
hExpr cx (VESelect1 e1 e2) h     = hExpr cx e2 (hExpr cx e1 (feedString h "Q"))
hExpr cx (VERepeat e1 e2) h      = hExpr cx e2 (hExpr cx e1 (feedString h "*"))
hExpr cx (VEIf e1 e2 e3) h       = hExpr cx e3 (hExpr cx e2 (hExpr cx e1 (feedString h "?")))
hExpr cx (VEFctCall f es) h      =
    feedString (foldl' (flip (hExpr cx)) (feedString (feedString h "F") (getVIdString f)) es) ")"
hExpr _  (VEMacro s) h           = feedString (feedString h "M") s

hRange :: Ctx -> VRange -> Hash -> Hash
hRange cx (e1, e2) h = hExpr cx e2 (hExpr cx e1 (feedString h ":"))

hMRange :: Ctx -> Maybe VRange -> Hash -> Hash
hMRange _  Nothing h  = feedString h "-"
hMRange cx (Just r) h = hRange cx r h

hLV :: Ctx -> VLValue -> Hash -> Hash
hLV cx (VLId v) h      = hRef cx v (feedString h "l")
hLV cx (VLConcat ls) h = feedString (foldl' (flip (hLV cx)) (feedString h "{") ls) "}"
hLV cx (VLSub l e) h   = hExpr cx e (hLV cx l (feedString h "["))

hEvent :: Ctx -> VEventExpr -> Hash -> Hash
hEvent cx (VEEOr e1 e2) h  = hEvent cx e2 (hEvent cx e1 (feedString h "or"))
hEvent cx (VEEposedge e) h = hExpr cx e (feedString h "posedge")
hEvent cx (VEEnegedge e) h = hExpr cx e (feedString h "negedge")
hEvent cx (VEE e) h        = hExpr cx e (feedString h "ev")
hEvent cx (VEEMacro s e) h = hExpr cx e (feedString (feedString h "evmacro") s)

hGuard :: Ctx -> Guard -> Hash -> Hash
hGuard cx (GEvent n _) h      = feedWord (feedString h "@") (M.findWithDefault 0 n (cxEv cx))
hGuard _  (GBlock s) h        = feedString h s
hGuard cx (GIf e b) h         = feedShow (hExpr cx e (feedString h "if")) b
hGuard cx (GCase kind e ls) h = feedString (foldl' (flip (hExpr cx)) (hExpr cx e (feedString h kind)) ls) ":"
hGuard cx (GDefault kind e) h = hExpr cx e (feedString h (kind ++ "default"))

hProc :: Ctx -> Proc -> Hash -> Hash
hProc cx (PAssign nb l e) h = hExpr cx e (hLV cx l (feedString h (if nb then "<=" else "=")))
hProc cx (PTask n es) h     =
    feedString (foldl' (flip (hExpr cx)) (feedString (feedString h "task") n) es) ")"

hFrag :: Ctx -> Frag -> Hash -> Hash
hFrag cx (FAssign e) h        = hExpr cx e (feedString h "A")
hFrag cx (FConcat k n l e) h  = hExpr cx e (hLV cx l (feedShow (feedString h "K") (k, n)))
hFrag cx (FSub l e) h         = hExpr cx e (hLV cx l (feedString h "S"))
hFrag cx (FProc gs p) h       = hProc cx p (foldl' (flip (hGuard cx)) (feedString h "P") gs)
hFrag _  (FInst m i p) h      = feedString (feedString (feedString (feedString h "I") m) i) p

-- the content hash of one member, given the hashes of what it references
contentHash :: Ctx -> NameKey -> [Frag] -> Maybe (Maybe VRange, Maybe VRange) -> Word64
contentHash cx self frags decl =
    let cx' = cx { cxLhs = positions [self] }
        h0  = case decl of
                Nothing       -> feedString hashInit "undeclared"
                Just (mr, ar) -> hMRange cx' ar (hMRange cx' mr (feedString hashInit "W"))
        h1  = case frags of
                [] -> feedString (feedString h0 "undriven")
                                 (maybe "" (\(stem, _, _) -> stem) (matchFamily (keyString self)))
                _  -> foldl' (\h f -> hFrag cx' f h) h0 frags
    in  hashValue h1

keyString :: NameKey -> String
keyString (NameKey _ s) = s

-- Hash every member, references first.  Members are visited in
-- first-occurrence order; a reference back into the visiting stack is a
-- cycle, which hRef resolves to the stem.
-- An item's event list (a case mux's sensitivity list has one entry
-- per arm) is hashed once, when the first assignment under it is
-- reached: by then every member it mentions is hashed.
hashMembers :: (String -> Bool) -> [NameKey] -> (NameKey -> [Frag])
            -> (NameKey -> Maybe (Maybe VRange, Maybe VRange))
            -> M.Map NameKey Word64
hashMembers isM keys fragsOf declOf = fst (foldl' (visit S.empty) (M.empty, M.empty) keys)
  where
    refsOf k = [ r | HId v <- concatMap fragHits (fragsOf k)
                   , let s = getVIdString v, isM s
                   , let r = nameKey s, r /= k ]
    visit stack st@(env, ev) k
        | M.member k env       = st
        | k `S.member` stack   = st
        | otherwise =
            let stack'     = S.insert k stack
                (env', ev') = foldl' (visit stack') st (refsOf k)
                frags      = fragsOf k
                cx0        = Ctx { cxEnv = env', cxEv = ev', cxIsM = isM, cxLhs = M.empty }
                ev''       = foldl' (ensureEvent cx0) ev' frags
                w          = contentHash cx0 { cxEv = ev'' } k frags (declOf k)
            in  (M.insert k w env', ev'')
    ensureEvent cx ev (FProc gs _) = foldl' one ev gs
      where one m (GEvent n ee)
                | not (M.member n m) = M.insert n (hashValue (hEvent cx ee hashInit)) m
            one m _ = m
    ensureEvent _ ev _ = ev

-- twelve decimal digits of a hash, rehashed for the n-th retry
digitsOf :: Word64 -> Int -> String
digitsOf w 0 = show (w `mod` 1000000000000)
digitsOf w n = show (hashValue (feedShow (feedWord hashInit w) n) `mod` 1000000000000)

-- targets in first-occurrence order, each distinct from the ones before
-- it and from the non-renameable names
assignTargets :: S.Set NameKey -> [(String, String, Family, String, Word64)] -> [(String, String)]
assignTargets _ [] = []
assignTargets taken ((nm, stem, fam, part, w) : rest) =
    let pick n = let t = stem ++ markerText fam ++ digitsOf w n ++ part
                 in  if nameKey t `S.member` taken then pick (n + 1) else t
        target = pick (0 :: Int)
    in  (nm, target) : assignTargets (S.insert (nameKey target) taken) rest

-- --------------------------------------------------------------- re-sort

-- Sort again, by the new names, every group list that was sorted under
-- the old ones (the original and renamed bodies have the same shape).
-- Declaration variable lists are always sorted.
resortItems :: [VMItem] -> [VMItem] -> [VMItem]
resortItems = zipWith resortItem

resortItem :: VMItem -> VMItem -> VMItem
resortItem (VMGroup _ itss0) (VMGroup t itss)        = VMGroup t (zipWith resortList itss0 itss)
resortItem (VMComment _ it0) (VMComment c it)        = VMComment c (resortItem it0 it)
resortItem (VMRegGroup _ _ _ it0) (VMRegGroup i s c it) = VMRegGroup i s c (resortItem it0 it)
resortItem _ (VMDecl (VVDecl t mr vs))               = VMDecl (VVDecl t mr (sort vs))
resortItem _ it                                      = it

resortList :: [VMItem] -> [VMItem] -> [VMItem]
resortList its0 its =
    let its' = resortItems its0 its
    in  if and (zipWith (<=) its0 (drop 1 its0)) then sortBy cmpItem its' else its'

-- the VMItem order, except that procedural items assigning exactly one
-- reg (the case muxes) are ordered by that reg's new name; the VMItem
-- order has them all equal, which would leave them in mint order
cmpItem :: VMItem -> VMItem -> Ordering
cmpItem x y =
    case compare x y of
      EQ -> compare (singleTarget x) (singleTarget y)
      o  -> o

-- ------------------------------------------------------------------ the pass

renumberModule :: VModule -> VModule
renumberModule vm =
    let
        body = vm_body vm

        body_hits :: [Hit]
        body_hits = hitsItems body []

        -- names that must not change (or be created by renaming).
        instFixed :: VMItem -> [String]
        instFixed (VMInst m i pas pos) =
            [getVIdString m, getVIdString i] ++
            map (getVIdString . fst) pas ++
            map (getVIdString . fst) pos
        instFixed (VMComment _ it)      = instFixed it
        instFixed (VMRegGroup _ _ _ it) = instFixed it
        instFixed (VMGroup _ itss)      = concatMap (concatMap instFixed) itss
        instFixed _                     = []

        -- foreign linkage names must never change: the function/task
        -- names of foreign calls (DPI declarations live at file scope,
        -- outside this module walk)
        -- (a user's imported function may itself be family-shaped,
        -- e.g. import "BDPI" f__h1)
        excluded :: S.Set NameKey
        excluded = S.fromList (map nameKey (
            getVIdString (vm_name vm) :
            map getVIdString (portIds (vm_ports vm)) ++
            [ n | HForeign n <- body_hits ] ++
            concatMap instFixed body))

        -- family members in order of first occurrence.
        occurrences :: [String]
        occurrences = [ getVIdString v | HId v <- body_hits ]

        -- the seen set only needs to record names that match a family shape
        firstSeen :: S.Set NameKey -> [String] -> [(String, String, Family, String)]
        firstSeen _ [] = []
        firstSeen seen (s : ss) =
            case matchFamily s of
              Nothing -> firstSeen seen ss
              Just (stem, fam, part) ->
                  let k = nameKey s
                  in  if k `S.member` seen
                      then firstSeen seen ss
                      else if k `S.member` excluded
                           then firstSeen (S.insert k seen) ss
                           else (s, stem, fam, part) : firstSeen (S.insert k seen) ss

        members :: [(String, String, Family, String)]  -- (name, stem, family, part suffix)
        members = firstSeen S.empty occurrences

        memberKeys :: S.Set NameKey
        memberKeys = S.fromList [ nameKey nm | (nm, _, _, _) <- members ]

        isM :: String -> Bool
        isM s = nameKey s `S.member` memberKeys

        drivers = collectItems isM body noDrivers

        -- a task argument counts as a driver only for a name that has
        -- no other: a void task's input has its own assignment or port
        fragsOf :: NameKey -> [Frag]
        fragsOf k =
            let inst = maybe [] (: []) (M.lookup k (drInsts drivers))
            in  case M.lookup k (drFrags drivers) of
                  Nothing -> inst
                  Just fs ->
                      let own = [ f | f <- reverse fs, not (isTask f) ]
                      in  if not (null own) then own
                          else if not (null inst) then inst
                          else reverse fs
        isTask (FProc _ (PTask _ _)) = True
        isTask _                     = False

        hashes :: M.Map NameKey Word64
        hashes = hashMembers isM [ nameKey nm | (nm, _, _, _) <- members ]
                             fragsOf (\k -> M.lookup k (drDecls drivers))

        targets :: [(String, String)]
        targets = assignTargets excluded
                      [ (nm, stem, fam, part, M.findWithDefault 0 (nameKey nm) hashes)
                      | (nm, stem, fam, part) <- members ]

        renames :: M.Map NameKey String
        renames = M.fromList [ (nameKey nm, tgt) | (nm, tgt) <- targets, nm /= tgt ]

        subst :: VId -> VId
        subst v@(VId s i info) =
            case M.lookup (nameKey s) renames of
              Just s' -> VId s' (setIdBaseString i s') info
              Nothing -> v
    in
        if M.null renames
        then vm
        else vm { vm_body = resortItems body (mapItems subst body) }
