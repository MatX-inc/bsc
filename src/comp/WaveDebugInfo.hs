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
-- reach: kind, width in bits, and for structs, unions, enums and
-- vectors the bit range of each member (ranges are half-open,
-- [lo, hi), counted from the least significant bit).  Layouts follow
-- the compiler's derived Bits instances: struct fields concatenated
-- with the first field most significant; a union's tag in the high
-- bits and each arm's payload right-aligned below it; a vector's
-- element 0 lowest.  A type whose Bits instance is hand-written can
-- lay out otherwise; when its width does not match the derived shape
-- the layout is reported as "custom" and the members carry no ranges.
module WaveDebugInfo (waveDebugInfo) where

import Data.List(sortBy, nub)
import Data.Maybe(mapMaybe)
import Data.Char(isDigit)
import qualified Data.Map as M
import qualified Data.Set as S

import Error(EMsg)
import Flags(Flags, tclShowHidden)
import Position(getPosition, getPositionFile, getPositionLine,
                getPositionColumn)
import TclUtils(isRealPosition)
import Id
import IType(IType, iToCT)
import ISyntaxUtil(itBool)
import CType(CType, Type(..), TyCon(..))
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

layoutStr :: Layout -> String
layoutStr LayoutDerived = "derived"
layoutStr LayoutCustom  = "custom"
layoutStr LayoutUnknown = "unknown"

-- The description of one type, and the types its members name
describeType :: Flags -> SymTab -> CType -> (Maybe JValue, [CType])
describeType flags symtab t =
    case analyzeType flags symtab t of
      Left _ -> (Nothing, [])
      Right ta -> describe ta
  where
    width = maybe JNull JNum

    describe :: TypeAnalysis -> (Maybe JValue, [CType])
    describe ta@(Primary {}) =
        (Just (JObj [("kind", JStr "primary"), ("width", width (getWidth ta))]), [])
    describe (Alias _ _ _ target) =
        (Just (JObj [("kind", JStr "alias"), ("target", JStr (typeName target))]),
         [target])
    describe ta@(Struct _ _ _ _ fields _) =
        let w = getWidth ta
            fws = [ (i, qualToType qt, fw) | (i, qt, fw) <- fields ]
            -- first field most significant
            ranges = case (w, sequence [ fw | (_, _, fw) <- fws ]) of
                       (Just total, Just ws) ->
                           let his = scanl (-) total ws
                           in  Just (zip (drop 1 his) his)
                       _ -> Nothing
            member ((i, ft, fw), r) =
                JObj $ [ ("name", JStr (getIdBaseString i))
                       , ("type", JStr (typeName ft))
                       , ("width", width fw) ] ++ range r
            layout = maybe LayoutUnknown (const LayoutDerived) ranges
        in  (Just (JObj [ ("kind", JStr "struct")
                        , ("width", width w)
                        , ("layout", JStr (layoutStr layout))
                        , ("members", JArr (map member (zipRanges fws ranges))) ]),
             [ ft | (_, ft, _) <- fws ])
    describe ta@(Enum qi cons _) =
        let w = getWidth ta
            member c = JObj $ [ ("name", JStr (getIdBaseString c)) ] ++
                              maybe [] (\ct -> [("value", JNum (conTag ct))])
                                       (tagInfo symtab qi c)
        in  (Just (JObj [ ("kind", JStr "enum")
                        , ("width", width w)
                        , ("members", JArr (map member cons)) ]), [])
    describe ta@(TaggedUnion qi _ _ _ arms _) =
        let w = getWidth ta
            tags = [ tagInfo symtab qi c | (c, _, _) <- arms ]
            tag_size = case mapMaybe id tags of
                         (ct:_) -> Just (tagSize ct)
                         []     -> Nothing
            payload = maximum (0 : [ aw | (_, _, Just aw) <- arms ])
            -- the derived shape: tag above the widest arm's payload
            layout = case (w, tag_size, sequence [ aw | (_, _, aw) <- arms ]) of
                       (Just total, Just ts, Just _)
                         | total == ts + payload -> LayoutDerived
                         | otherwise             -> LayoutCustom
                       _ -> LayoutUnknown
            derived = case layout of LayoutDerived -> True
                                     _             -> False
            tag_range = case (w, tag_size) of
                          (Just total, Just ts) | derived ->
                              [("tag", JObj [("lo", JNum (total - ts)), ("hi", JNum total)])]
                          _ -> []
            member ((c, at, aw), mct) =
                JObj $ [ ("name", JStr (getIdBaseString c))
                       , ("type", JStr (typeName at))
                       , ("width", width aw) ] ++
                       maybe [] (\ct -> [("tag", JNum (conTag ct))]) mct ++
                       (case aw of
                          Just n | derived -> [("lo", JNum 0), ("hi", JNum n)]
                          _ -> [])
        in  (Just (JObj $ [ ("kind", JStr "union")
                          , ("width", width w)
                          , ("layout", JStr (layoutStr layout)) ] ++
                          tag_range ++
                          [ ("members", JArr (map member (zip arms tags))) ]),
             [ at | (_, at, _) <- arms ])
    describe ta@(Vector _ len elt _) =
        let w = getWidth ta
            n = case len of
                  TCon (TyNum v _) -> Just v
                  _                -> Nothing
            stride = case (w, n) of
                       (Just total, Just k) | k > 0 -> Just (total `div` k)
                       _ -> Nothing
        in  (Just (JObj $ [ ("kind", JStr "vector")
                          , ("width", width w)
                          , ("length", width n)
                          , ("elem", JStr (typeName elt))
                          , ("stride", width stride) ]), [elt])
    describe _ = (Nothing, [])

    zipRanges fws Nothing   = [ (f, Nothing) | f <- fws ]
    zipRanges fws (Just rs) = zip fws (map Just rs)
    range Nothing = []
    range (Just (lo, hi)) = [("lo", JNum lo), ("hi", JNum hi)]

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
typesJson :: Flags -> SymTab -> [CType] -> JValue
typesJson flags symtab roots = JObj (go S.empty [] roots)
  where
    go _ acc [] = reverse acc
    go seen acc (t:ts)
      | S.member name seen = go seen acc ts
      | otherwise =
          let (mdesc, more) = describeType flags symtab t
              seen' = S.insert name seen
          in  case mdesc of
                Nothing   -> go seen' acc ts
                Just desc -> go seen' ((name, desc) : acc) (ts ++ more)
      where name = typeName t

-- ---------------

-- The JSON text of the debug information for the design under `top`,
-- whose scope path in the dump is main.top
waveDebugInfo :: Flags -> SymTab -> HierMap -> [(String, ABinEitherModInfo)]
              -> String -> Either [EMsg] String
waveDebugInfo flags symtab hier mods top =
    let design = Design { d_hier = hier
                        , d_mods = M.fromList mods
                        , d_hide = not (tclShowHidden flags) }
        signals = moduleSignals design ["main", "top"] [] top
        roots = nub (mapMaybe sig_type signals)
    in  Right $ render $
          JObj [ ("format", JStr "bsc-wave-debug-info")
               , ("version", JNum 1)
               , ("top", JStr top)
               , ("signals", JArr (map signalJson signals))
               , ("types", typesJson flags symtab roots) ]
