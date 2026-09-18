-- Debug information for the waveforms a Bluesim model dumps: which
-- dumped signal is which source-level entity, and how the Bluespec
-- types of those signals lay out in bits.  Written as JSON for
-- waveform viewers (and their plugins) to read alongside the dump.
--
-- The document has two parts.
--
-- "signals" lists the design's own signals: the state elements
-- (registers, FIFOs, wires, ... -- one entry per primitive instance),
-- the synthesized submodule instances, each rule's CAN_FIRE and
-- WILL_FIRE, and each method's ports.  For each, "synthpath" is the
-- signal's path in the dump (scope names then the signal name, as
-- Bluesim emits them) and "bsvpath" is its path in the source: the
-- instance names down through the inlined modules, ending in the
-- entity's own name.  "type" names the signal's Bluespec type, in the
-- spelling the dump records; "file", "line" and "column" locate the
-- entity in the source.
--
-- "types" describes every type named by a signal, and the types those
-- reach: kind, width in bits, and for structs, unions and enums where
-- each member lies in the packed value, as the type's Bits instance
-- computes it (WaveLayout reduces the instance's own code).  A member
-- whose bits are a plain slice of the value carries their range
-- (half-open, [lo, hi), counted from the least significant bit); a
-- union arm or enum constant told apart by a fixed range of tag bits
-- carries the tag value(s) selecting it, and the type carries the
-- range; anything else carries the expression that computes it, for
-- the reader to evaluate.  "layout" says whether the result is the
-- derived one (struct fields concatenated, first field most
-- significant; a union's tag above the widest payload, each payload
-- right-aligned), "custom", or "unknown" when the instance could not
-- be reduced.  A vector's element 0 is lowest.
module WaveDebugInfo (waveDebugInfo) where

import Data.List(sortBy, nub)
import Data.Maybe(mapMaybe, isJust)
import Data.Bits(shiftL, testBit)
import Data.Char(isDigit)
import qualified Data.Map as M
import qualified Data.Set as S

import Error(ErrorHandle)
import Flags(Flags, tclShowHidden)
import Position(getPositionFile, getPositionLine,
                getPositionColumn, noPosition)
import TclUtils(isRealPosition)
import Id
import IType(IType, iToCT)
import ISyntaxUtil(itBool)
import CSyntax
import Undefined(UndefKind(..))
import PreIds(idPack, idUnpack, idTrue, idFalse, idPrimUnit)
import FStringCompat(mkFString)
import WaveLayout
import Pred(qualToType)
import PVPrint(pvpString)
import SymTab(SymTab, ConInfo(..), findCon)
import ConTagInfo(ConTagInfo(..))
import TypeAnalysis(TypeAnalysis(..), analyzeType, getWidth)
import VModInfo
import ASyntax
import ABin(ABinEitherModInfo, abemi_apkg)
import ABinUtil(HierMap)
import InstNodes(InstNode(..), InstTree, isHiddenAll, isHiddenKP, nodeChildren)

-- ---------------
-- JSON

data JValue = JObj [(String, JValue)]
            | JArr [JValue]
            | JStr String
            | JNum Integer
            | JBool Bool
            | JNull

render :: JValue -> String
render v = go 0 v ++ "\n"
  where
    go :: Int -> JValue -> String
    go _ (JStr s)  = quote s
    go _ (JNum n)  = show n
    go _ (JBool b) = if b then "true" else "false"
    go _ JNull     = "null"
    go _ (JArr []) = "[]"
    go _ (JObj []) = "{}"
    go ind (JArr vs)
      | all scalar vs = "[" ++ commas (map (go ind) vs) ++ "]"
      | otherwise = "[\n" ++ lines' (ind + 2) (map (go (ind + 2)) vs) ++
                    "\n" ++ pad ind ++ "]"
    go ind (JObj kvs) =
      "{\n" ++ lines' (ind + 2) [ quote k ++ ": " ++ go (ind + 2) x
                                | (k, x) <- kvs ] ++
      "\n" ++ pad ind ++ "}"
    scalar (JArr _) = False
    scalar (JObj _) = False
    scalar _        = True
    commas = foldr1' (\a b -> a ++ ", " ++ b)
    lines' ind xs = foldr1' (\a b -> a ++ ",\n" ++ b) (map (pad ind ++) xs)
    foldr1' _ [] = ""
    foldr1' f xs = foldr1 f xs
    pad n = replicate n ' '
    quote s = "\"" ++ concatMap esc s ++ "\""
    esc '"'  = "\\\""
    esc '\\' = "\\\\"
    esc '\n' = "\\n"
    esc '\t' = "\\t"
    esc c | c < ' ' = "\\u" ++ hex4 (fromEnum c)
          | otherwise = [c]
    hex4 n = let h = "0123456789abcdef"
             in  [ h !! ((n `div` d) `mod` 16) | d <- [4096, 256, 16, 1] ]

-- ---------------
-- Signals

data Signal = Signal { sig_synth :: [String]   -- path in the dump
                     , sig_bsv :: [String]     -- path in the source
                     , sig_kind :: String
                     , sig_detail :: [(String, JValue)]
                     , sig_type :: Maybe CType
                     , sig_pos :: Maybe Id     -- the entity whose position locates it
                     }

-- Types are named in the spelling the dump records (BSV syntax, so
-- every consumer sees one spelling); rendered from the CType so that
-- the names of member types, which only exist as CTypes, match.
typeName :: CType -> String
typeName = pvpString

signalJson :: Signal -> JValue
signalJson s =
    JObj $ [ ("synthpath", JArr (map JStr (sig_synth s)))
           , ("bsvpath", JArr (map JStr (sig_bsv s)))
           , ("kind", JStr (sig_kind s)) ]
           ++ sig_detail s
           ++ maybe [] (\t -> [("type", JStr (typeName t))]) (sig_type s)
           ++ maybe [] posJson (sig_pos s)
  where posJson i =
          let p = getPosition i
          in  if isRealPosition p
              then [ ("file", JStr (getPositionFile p))
                   , ("line", JNum (toInteger (getPositionLine p)))
                   , ("column", JNum (toInteger (getPositionColumn p))) ]
              else []

-- The design hierarchy: the module of each synthesized instance, and
-- the elaborated packages by module name
data Design = Design { d_hier :: HierMap
                     , d_mods :: M.Map String ABinEitherModInfo
                     , d_hide :: Bool  -- leave out {-# hide #-}'d instances
                     }

-- The source name an instance node displays: its display name when
-- the elaborator recorded one (e.g. for loop bodies), else its own
localName :: Id -> String
localName = getIdBaseString . addIdDisplayName

-- The signals of one synthesized module and, recursively, of the
-- synthesized modules it instantiates.  `scope` is the module's scope
-- path in the dump, `bsv` its instance path in the source.
moduleSignals :: Design -> [String] -> [String] -> String -> [Signal]
moduleSignals d scope bsv modname =
    case M.lookup modname (d_mods d) of
      Nothing -> []
      Just abmi ->
        let apkg = abemi_apkg abmi
            submods = M.fromList (fst (M.findWithDefault ([], []) modname (d_hier d)))
            prims = M.fromList [ (getIdBaseString (avi_vname avi), avi)
                               | avi <- apkg_state_instances apkg ]
            fromTree = concatMap (treeSignals d scope bsv submods prims)
                                 (treeChildren (apkg_inst_tree apkg))
            ports = concatMap (portSignals apkg scope bsv) (apkg_interface apkg)
        in  ports ++ fromTree

treeChildren :: InstTree -> [InstNode]
treeChildren t = sortBy cmpNode (M.elems t)
  where cmpNode a b = cmpIdByName (node_name a) (node_name b)

-- A state element or rule sits under a Loc node carrying its source
-- name, whose single child carries the name the dump uses (a library
-- module's hidden wrapper instance in between, such as mkReg's, is
-- looked through).  Any other Loc is an inlined module instance (or a
-- loop body), one more level of the source path.
treeSignals :: Design -> [String] -> [String] -> M.Map String String
            -> M.Map String AVInst -> InstNode -> [Signal]
treeSignals d scope bsv submods prims node =
    case node of
      Loc {}
        | d_hide d && isHiddenAll node -> []
        | otherwise ->
            case nodeChildren (d_hide d) node of
              [StateVar { node_name = flat }] ->
                  stateSignals d scope bsv submods prims (node_name node) flat
              [Rule { node_name = rule }] ->
                  ruleSignals scope bsv (node_name node) rule
              children
                | node_ignore node || (d_hide d && isHiddenKP node) ->
                    concatMap recurse (sortBy cmpNode children)
                | otherwise ->
                    concatMap (treeSignals d scope (bsv ++ [localName (node_name node)]) submods prims)
                              (sortBy cmpNode children)
      StateVar { node_name = flat } ->
          stateSignals d scope bsv submods prims flat flat
      Rule { node_name = rule } ->
          ruleSignals scope bsv rule rule
  where recurse = treeSignals d scope bsv submods prims
        cmpNode a b = cmpIdByName (node_name a) (node_name b)

-- A rule's fire signals are dumped at the module's scope under the
-- rule's elaborated name (RL_...); the source knows the rule by its
-- own name.
ruleSignals :: [String] -> [String] -> Id -> Id -> [Signal]
ruleSignals scope bsv name rule =
    [ Signal (scope ++ [getIdBaseString (mkIdWillFire rule)])
             (bsv ++ [local, "WILL_FIRE"]) "fire"
             [("rule", JStr local)] (Just tBool) (Just name)
    , Signal (scope ++ [getIdBaseString (mkIdCanFire rule)])
             (bsv ++ [local, "CAN_FIRE"]) "fire"
             [("rule", JStr local)] (Just tBool) (Just name)
    ]
  where local = localName name

tBool :: CType
tBool = iToCT itBool

-- A state element is a synthesized submodule instance (a scope of its
-- own, whose contents follow) or a primitive instance.  The dump names
-- a primitive by its flattened instance name, either as a signal (a
-- register's contents) or as a scope holding its ports (a FIFO); the
-- entry records which primitive, and the type of its contents.
stateSignals :: Design -> [String] -> [String] -> M.Map String String
             -> M.Map String AVInst -> Id -> Id -> [Signal]
stateSignals d scope bsv submods prims name flat =
    let flatname = getIdBaseString flat
        local = localName name
        synth = scope ++ [flatname]
    in  case M.lookup flatname submods of
          Just submod | M.member submod (d_mods d) ->
              Signal synth (bsv ++ [local]) "module"
                     [("module", JStr submod)] Nothing (Just name)
              : moduleSignals d synth (bsv ++ [local]) submod
          _ ->
              case M.lookup flatname prims of
                Just avi ->
                    let vmi = avi_vmi avi
                        prim = getVNameString (vName vmi)
                        ty = fmap iToCT (primDataType avi)
                    in  [ Signal synth (bsv ++ [local]) "state"
                                 [("prim", JStr prim)] ty (Just name) ]
                Nothing ->
                    [ Signal synth (bsv ++ [local]) "state" [] Nothing (Just name) ]

-- The type of a primitive's contents: the recorded type of its data
-- port.  Every data port of a register, FIFO, wire or counter carries
-- the same type; a memory's ports differ, and its output port's type
-- is the one that counts as its contents.
primDataType :: AVInst -> Maybe IType
primDataType avi =
    let pts = avi_port_types avi
        firstOf names = case mapMaybe (\n -> M.lookup (VName n) pts) names of
                          (t:_) -> Just t
                          []    -> Nothing
    in  case firstOf ["Q_OUT", "D_OUT", "DO", "DOA", "WGET", "IN", "WHAS"] of
          Just t  -> Just t
          Nothing -> case M.elems pts of
                       (t:_) -> Just t
                       []    -> Nothing

-- A method's ports, at the module's scope.  Argument, result and
-- enable ports are named by the interface; the ready signal is a
-- method of its own (RDY_<method>) in the elaborated interface.
portSignals :: APackage -> [String] -> [String] -> AIFace -> [Signal]
portSignals apkg scope bsv aif =
    case aif_fieldinfo aif of
      Method { vf_inputs = ins, vf_outputs = outs, vf_enable = en }
        | isRdyId name ->
            [ (port (getFromReady name) "RDY" "ready" (fst vp)) { sig_type = Just tBool }
            | vp <- outs ]
        | otherwise ->
            [ port meth (argName i vn) "arg" vn
            | (i, vps) <- zip [(1 :: Int) ..] ins, (vn, _) <- vps ] ++
            [ Signal (scope ++ [getVNameString vn]) (bsv ++ [meth]) "port"
                     [("role", JStr "result"), ("method", JStr meth)]
                     (fmap iToCT (M.lookup vn types)) (Just name)
            | (vn, _) <- outs ] ++
            [ (port meth "EN" "enable" vn) { sig_type = Just tBool } | Just (vn, _) <- [en] ]
      _ -> []
  where
    name = aif_name aif
    meth = getIdBaseString name
    types = apkg_external_wire_types apkg
    port m leaf role vn =
        Signal (scope ++ [getVNameString vn]) (bsv ++ [m, leaf]) "port"
               [("role", JStr role), ("method", JStr m)]
               (fmap iToCT (M.lookup vn types)) (Just name)
    -- an argument port is named <method>_<arg>; the source knows the arg
    -- by its name, or by position when the interface gave it none
    argName :: Int -> VName -> String
    argName i vn =
        let full = getVNameString vn
            pre = meth ++ "_"
            rest = drop (length pre) full
        in  if take (length pre) full == pre && not (null rest) && not (all isDigit rest)
            then rest
            else "arg" ++ show i

-- ---------------
-- Types

data Layout = LayoutDerived | LayoutCustom | LayoutUnknown
    deriving (Eq)

layoutStr :: Layout -> String
layoutStr LayoutDerived = "derived"
layoutStr LayoutCustom  = "custom"
layoutStr LayoutUnknown = "unknown"

-- One member of a struct, union or enum: its place in the packed value
-- as the type's Bits instance computes it (see WaveLayout), from which
-- the JSON reports fixed bit ranges and tag values where the instance
-- has them and the computing expression where it does not
data Member = Member { mb_name :: String
                     , mb_type :: Maybe CType
                     , mb_width :: Maybe Integer
                     , mb_derivedTag :: Maybe Integer  -- the tag a derived instance gives it
                     , mb_when :: Maybe BitExpr        -- 1 iff the value is this member
                     , mb_bits :: Maybe BitExpr }      -- the member's own bits

-- The description of one type, and the types its members name
describeType :: ErrorHandle -> Flags -> SymTab -> CType -> IO (Maybe JValue, [CType])
describeType errh flags symtab t =
    case analyzeType flags symtab t of
      Left _ -> return (Nothing, [])
      Right ta -> describe ta
  where
    width = maybe JNull JNum
    typeBits = case analyzeType flags symtab t of
                 Right ta -> getWidth ta
                 Left _ -> Nothing

    describe :: TypeAnalysis -> IO (Maybe JValue, [CType])
    describe ta@(Primary {}) =
        return (Just (JObj [("kind", JStr "primary"), ("width", width (getWidth ta))]), [])
    describe (Alias _ _ _ target) =
        return (Just (JObj [("kind", JStr "alias"), ("target", JStr (typeName target))]),
                [target])
    describe ta@(Struct _ _ _ _ fields _) = do
        let fws = [ (i, qualToType qt, fw) | (i, qt, fw) <- fields ]
            members = [ Member (getIdBaseString i) (Just ft) fw Nothing Nothing Nothing
                      | (i, ft, fw) <- fws ]
            queries w = [ q | (n, (i, _, Just fw)) <- zip [0 :: Int ..] fws, fw > 0
                            , let q = Query (valName n) w fw (\ b -> packed (CSelect (unpacked b) i)) ]
        members' <- layout queries members
        -- a derived instance concatenates the fields, first field highest
        let derivedRanges =
                case (typeBits, sequence [ fw | (_, _, fw) <- fws ]) of
                  (Just total, Just ws) ->
                      let his = scanl (-) total ws
                      in  Just (zip (drop 1 his) his)
                  _ -> Nothing
            derived = case derivedRanges of
                        Just rs -> and [ Just r == fixedRange m | (m, r) <- zip members' rs ]
                        Nothing -> False
        return (Just (JObj [ ("kind", JStr "struct")
                           , ("width", width typeBits)
                           , ("layout", JStr (layoutStr (layoutOf members' derived)))
                           , ("members", JArr (map (memberJson Nothing) members')) ]),
                [ ft | (_, ft, _) <- fws ])
    describe (Enum qi cons _) = do
        let members = [ Member (getIdBaseString c) Nothing Nothing (fmap conTag (tagInfo symtab qi c)) Nothing Nothing
                      | c <- cons ]
            queries w = [ Query (isName n) w 1 (isCon (CPCon c []))
                        | (n, c) <- zip [0 :: Int ..] cons ]
        members' <- layout queries members
        let table = tagTable members'
            derived = case table of
                        Just ((0, hi), sels) ->
                            Just hi == typeBits &&
                            and [ map Just sel == [mb_derivedTag m] | (m, sel) <- zip members' sels ]
                        _ -> False
        return (Just (JObj [ ("kind", JStr "enum")
                           , ("width", width typeBits)
                           , ("layout", JStr (layoutStr (layoutOf members' derived)))
                           , ("members", JArr (memberJsons "value" "values" table members')) ]), [])
    describe (TaggedUnion qi _ _ _ arms _) = do
        let members = [ Member (getIdBaseString c) (Just at) aw (fmap conTag (tagInfo symtab qi c)) Nothing Nothing
                      | (c, at, aw) <- arms ]
            queries w = concat
                [ Query (isName n) w 1 (isCon (CPCon c (payloadPat at))) :
                  [ Query (valName n) w pw (payloadOf c)
                  | Just pw <- [aw], pw > 0 ]
                | (n, (c, at, aw)) <- zip [0 :: Int ..] arms ]
        members' <- layout queries members
        -- a derived instance puts the tag above the widest payload, each
        -- payload right-aligned
        let table = tagTable members'
            payload = maximum (0 : [ pw | Member { mb_width = Just pw } <- members' ])
            derived = case table of
                        Just ((lo, hi), sels) ->
                            lo == payload && Just hi == typeBits &&
                            and [ map Just sel == [mb_derivedTag m] &&
                                  (mb_width m == Just 0 || fixedRange m == fmap (\ pw -> (0, pw)) (mb_width m))
                                | (m, sel) <- zip members' sels ]
                        _ -> False
            tag_range = case table of
                          Just ((lo, hi), _) -> [("tag", JObj [("lo", JNum lo), ("hi", JNum hi)])]
                          Nothing -> []
        return (Just (JObj $ [ ("kind", JStr "union")
                             , ("width", width typeBits)
                             , ("layout", JStr (layoutStr (layoutOf members' derived))) ] ++
                             tag_range ++
                             [ ("members", JArr (memberJsons "tag" "tags" table members')) ]),
                [ at | (_, at, _) <- arms ])
    describe ta@(Vector _ len elt _) =
        let w = getWidth ta
            n = case len of
                  TCon (TyNum v _) -> Just v
                  _                -> Nothing
            stride = case (w, n) of
                       (Just total, Just k) | k > 0 -> Just (total `div` k)
                       _ -> Nothing
        in  return (Just (JObj $ [ ("kind", JStr "vector")
                                 , ("width", width w)
                                 , ("length", width n)
                                 , ("elem", JStr (typeName elt))
                                 , ("stride", width stride) ]), [elt])
    describe _ = return (Nothing, [])

    -- the queries, in the type's source syntax
    pos = noPosition
    isName n = "is" ++ show n
    valName n = "val" ++ show n
    unpacked b = CHasType (CApply (CVar idUnpack) [b]) (CQType [] t)
    packed e = CApply (CVar idPack) [e]
    isCon pat b = Ccase pos (unpacked b)
                    [ CCaseArm pat [] (packed (CCon idTrue []))
                    , CCaseArm (CPAny pos) [] (packed (CCon idFalse [])) ]
    payloadPat at | isUnitType at = []
                  | otherwise = [CPAny pos]
    payloadOf c b = let x = mkId pos (mkFString "payload")
                    in  Ccase pos (unpacked b)
                          [ CCaseArm (CPCon c [CPVar x]) [] (packed (CVar x))
                          , CCaseArm (CPAny pos) [] (CAny pos UDontCare) ]
    isUnitType (TCon (TyCon i _ _)) = i == idPrimUnit
    isUnitType _ = False

    -- run the queries (when the type's width is known) and attach their
    -- results to the members
    layout :: (Integer -> [Query]) -> [Member] -> IO [Member]
    layout queries members =
        case typeBits of
          Nothing -> return members
          Just w -> do
              let qs = queries w
              results <- if null qs then return [Right []]
                         else reduceQueries errh flags [(typePackages t, qs)]
              case results of
                [Right rs] ->
                    let get n = lookup n rs
                    in  return [ m { mb_when = get (isName n), mb_bits = get (valName n) }
                               | (n, m) <- zip [0 :: Int ..] members ]
                _ -> return members

    layoutOf members derived
      | derived = LayoutDerived
      | any resolved members = LayoutCustom
      | otherwise = LayoutUnknown
    resolved m = isJust (mb_when m) || isJust (mb_bits m)

    -- the bits a member occupies, when they are a plain slice of the value
    fixedRange :: Member -> Maybe (Integer, Integer)
    fixedRange m = mb_bits m >>= slice
    slice (BArg w) = Just (0, w)
    slice (BExtract hi lo (BArg _)) = Just (lo, hi + 1)
    slice _ = Nothing

    -- the members' tag values, when what tells them apart is a range of
    -- bits small enough to enumerate: the range, and per member the
    -- values of those bits that select it
    tagTable :: [Member] -> Maybe ((Integer, Integer), [[Integer]])
    tagTable members = do
        whens <- mapM mb_when members
        let supp = S.unions (map bitExprSupport whens)
        if S.null supp then Nothing else do
          let lo = S.findMin supp
              hi = S.findMax supp + 1
              tw = hi - lo
          if tw > 12 then Nothing else do
            let vals = [0 .. 2 ^ tw - 1]
                selects e = [ v | v <- vals, evalBitExpr e (v `shiftL` fromInteger lo) == Just 1 ]
            return ((lo, hi), map selects whens)

    memberJsons :: String -> String -> Maybe ((Integer, Integer), [[Integer]]) -> [Member] -> [JValue]
    memberJsons one many table members =
        case table of
          Just (_, sels) -> [ memberJson (Just (one, many, sel)) m | (m, sel) <- zip members sels ]
          Nothing -> map (memberJson Nothing) members

    memberJson :: Maybe (String, String, [Integer]) -> Member -> JValue
    memberJson tags m =
        JObj $ [ ("name", JStr (mb_name m)) ] ++
               maybe [] (\ mt -> [("type", JStr (typeName mt))]) (mb_type m) ++
               maybe [] (\ w -> [("width", JNum w)]) (mb_width m) ++
               (case (tags, mb_when m) of
                  (Just (one, _, [v]), _) -> [(one, JNum v)]
                  (Just (_, many, vs), _) | length vs <= 64 -> [(many, JArr (map JNum vs))]
                  (_, Just e) -> [("when", bitExprJson e)]
                  _ -> []) ++
               (case (fixedRange m, mb_bits m) of
                  (Just (lo, hi), _) -> [("lo", JNum lo), ("hi", JNum hi)]
                  (Nothing, Just e) -> [("bits", bitExprJson e)]
                  _ -> [])

-- The packages whose definitions a type refers to
typePackages :: CType -> [Id]
typePackages = nub . go
  where
    go (TCon (TyCon i _ _)) | not (null (getIdQualString i)) = [mkId noPosition (getIdQual i)]
    go (TAp a b) = go a ++ go b
    go _ = []

-- An expression over the packed value, as JSON: an object naming the
-- operation, with the operands under "args" (concat's most significant
-- first; if's condition, then, else; case's scrutinee, default, then
-- alternating match value and result), a constant's bits as a binary
-- string, and the result width where it is not implied
bitExprJson :: BitExpr -> JValue
bitExprJson (BArg _) = JObj [("op", JStr "arg")]
bitExprJson (BConst w v) =
    JObj [("op", JStr "const"), ("bits", JStr [ if testBit v (fromInteger b) then '1' else '0'
                                              | b <- [w - 1, w - 2 .. 0] ])]
bitExprJson (BUndet w) = JObj [("op", JStr "undet"), ("width", JNum w)]
bitExprJson (BExtract hi lo e) =
    JObj [("op", JStr "extract"), ("hi", JNum hi), ("lo", JNum lo), ("args", JArr [bitExprJson e])]
bitExprJson (BConcat es) = JObj [("op", JStr "concat"), ("args", JArr (map bitExprJson es))]
bitExprJson (BIf c t f) = JObj [("op", JStr "if"), ("args", JArr (map bitExprJson [c, t, f]))]
bitExprJson (BCase s d arms) =
    JObj [("op", JStr "case"), ("args", JArr (map bitExprJson (s : d : concat [ [c, e] | (c, e) <- arms ])))]
bitExprJson (BOp op w es) =
    JObj [("op", JStr op), ("width", JNum w), ("args", JArr (map bitExprJson es))]

-- The packing information of a constructor of the given type
tagInfo :: SymTab -> Id -> Id -> Maybe ConTagInfo
tagInfo symtab ty con =
    case findCon symtab con of
      Just [ci] -> Just (ci_taginfo ci)
      Just cis  -> case [ ci | ci <- cis, qualEq ty (ci_id ci) ] of
                     [ci] -> Just (ci_taginfo ci)
                     _    -> Nothing
      Nothing   -> Nothing

-- Every type the signals name, and every type those reach, described
-- once each, keyed by name
typesJson :: ErrorHandle -> Flags -> SymTab -> [CType] -> IO JValue
typesJson errh flags symtab roots = fmap JObj (go S.empty [] roots)
  where
    go _ acc [] = return (reverse acc)
    go seen acc (t:ts)
      | S.member name seen = go seen acc ts
      | otherwise = do
          (mdesc, more) <- describeType errh flags symtab t
          let seen' = S.insert name seen
          case mdesc of
            Nothing   -> go seen' acc ts
            Just desc -> go seen' ((name, desc) : acc) (ts ++ more)
      where name = typeName t

-- ---------------

-- The JSON text of the debug information for the design under `top`,
-- whose scope path in the dump is main.top
waveDebugInfo :: ErrorHandle -> Flags -> SymTab -> HierMap -> [(String, ABinEitherModInfo)]
              -> String -> IO String
waveDebugInfo errh flags symtab hier mods top = do
    let design = Design { d_hier = hier
                        , d_mods = M.fromList mods
                        , d_hide = not (tclShowHidden flags) }
        signals = moduleSignals design ["main", "top"] [] top
        roots = nub (mapMaybe sig_type signals)
    types <- typesJson errh flags symtab roots
    return $ render $
          JObj [ ("format", JStr "bsc-wave-debug-info")
               , ("version", JNum 1)
               , ("top", JStr top)
               , ("signals", JArr (map signalJson signals))
               , ("types", types) ]
