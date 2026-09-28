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
-- signal's path in a dump of the synthesized hierarchy (scope names
-- then the signal name, as Bluesim emits them) and "bsvpath" its path
-- in the source: the instance names down through the inlined modules,
-- ending in the entity's own name -- which is also its path in a dump
-- made with -wave-source-hierarchy, for state elements and rule fires.
-- "type" names the signal's Bluespec type, in the spelling the dump
-- records; "file", "line" and "column" locate the entity in the source.
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
module WaveDebugInfo (waveDebugInfo, phase) where

import Data.List(intersperse)
import Data.Maybe(mapMaybe, isJust)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as BL
import Control.Exception(evaluate)
import Control.Monad(when)
import Data.Time.Clock(getCurrentTime, diffUTCTime)
import System.IO(hPutStrLn, stderr)
import Data.Bits(shiftL, testBit)
import Data.Char(isDigit)
import qualified Data.Map as M
import qualified Data.Set as S

import Error(ErrorHandle)
import Flags(Flags, tclShowHidden, verbose)
import Position(getPositionFile, getPositionLine,
                getPositionColumn, noPosition)
import TclUtils(isRealPosition)
import Id
import IType(IType, iToCT)
import ISyntaxUtil(itBool)
import CSyntax
import Undefined(UndefKind(..))
import PreIds(idPack, idUnpack, idTrue, idFalse)
import FStringCompat(mkFString)
import WaveLayout
import Pred(qualToType, Qual(..))
import Assump(Assump(..))
import Scheme(Scheme(..))
import CType(getArrows, leftTyCon, isTypeUnit, TyCon(..), TISort(..), StructSubType(..))
import PVPrint(pvpString)
import SymTab(SymTab, ConInfo(..), findCon)
import ConTagInfo(ConTagInfo(..))
import TypeAnalysis(TypeAnalysis(..), analyzeType, getWidth)
import VModInfo
import ASyntax
import ABin(ABinEitherModInfo, abemi_apkg)
import ABinUtil(HierMap)
import InstScopes(InstScope(..), instScopes, scopeLocalName)

-- ---------------
-- JSON

data JValue = JObj [(String, JValue)]
            | JArr [JValue]
            | JStr String
            | JNum Integer
            | JBool Bool
            | JNull

-- The document's bytes, laid out for reading: scalar arrays on one
-- line, objects and other arrays one member per line.  A Builder, so
-- that a large design's document is emitted in one pass rather than
-- assembled as a String.
render :: JValue -> B.Builder
render v = go 0 v <> B.char7 '\n'
  where
    go :: Int -> JValue -> B.Builder
    go _ (JStr s)  = quote s
    go _ (JNum n)  = B.integerDec n
    go _ (JBool b) = B.string7 (if b then "true" else "false")
    go _ JNull     = B.string7 "null"
    go _ (JArr []) = B.string7 "[]"
    go _ (JObj []) = B.string7 "{}"
    go ind (JArr vs)
      | all scalar vs = B.char7 '[' <> joined (B.string7 ", ") (map (go ind) vs) <> B.char7 ']'
      | otherwise = B.string7 "[\n" <> lines' (ind + 2) (map (go (ind + 2)) vs) <>
                    B.char7 '\n' <> pad ind <> B.char7 ']'
    go ind (JObj kvs) =
      B.string7 "{\n" <> lines' (ind + 2) [ quote k <> B.string7 ": " <> go (ind + 2) x
                                          | (k, x) <- kvs ] <>
      B.char7 '\n' <> pad ind <> B.char7 '}'
    scalar (JArr _) = False
    scalar (JObj _) = False
    scalar _        = True
    joined sep = mconcat . intersperse sep
    lines' ind xs = joined (B.string7 ",\n") (map (pad ind <>) xs)
    pad n = B.string7 (replicate n ' ')
    -- most strings need no escaping and go out in one piece
    quote s | any needsEsc s = B.char7 '"' <> foldMap esc s <> B.char7 '"'
            | otherwise = B.char7 '"' <> B.stringUtf8 s <> B.char7 '"'
    needsEsc c = c == '"' || c == '\\' || c < ' '
    esc '"'  = B.string7 "\\\""
    esc '\\' = B.string7 "\\\\"
    esc '\n' = B.string7 "\\n"
    esc '\t' = B.string7 "\\t"
    esc c | c < ' ' = B.string7 ("\\u" ++ hex4 (fromEnum c))
          | otherwise = B.charUtf8 c
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

-- `nameOf` renders a type's name; the caller shares one rendering per
-- type across the many signals that name it
signalJson :: (CType -> String) -> Signal -> JValue
signalJson nameOf s =
    JObj $ [ ("synthpath", JArr (map JStr (sig_synth s)))
           , ("bsvpath", JArr (map JStr (sig_bsv s)))
           , ("kind", JStr (sig_kind s)) ]
           ++ sig_detail s
           ++ maybe [] (\t -> [("type", JStr (nameOf t))]) (sig_type s)
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

-- The source name an instance, state element or rule displays
localName :: Id -> String
localName = scopeLocalName

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
            scopes = instScopes (d_hide d) (apkg_name apkg) (apkg_inst_tree apkg)
            ports = concatMap (portSignals apkg scope bsv) (apkg_interface apkg)
        in  ports ++ scopeSignals d scope bsv submods prims scopes

-- The signals of the source-level scopes of a module (see InstScope):
-- its state elements and rules, then those of the inlined instances,
-- each one more level of the source path
scopeSignals :: Design -> [String] -> [String] -> M.Map String String
             -> M.Map String AVInst -> InstScope -> [Signal]
scopeSignals d scope bsv submods prims sc =
    concat [ stateSignals d scope bsv submods prims name flat | (name, flat) <- is_states sc ] ++
    concat [ ruleSignals scope bsv name rule | (name, rule) <- is_rules sc ] ++
    concat [ scopeSignals d scope (bsv ++ [scopeLocalName (is_name c)]) submods prims c
           | c <- is_children sc ]

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

-- What describing one type takes: the types its members name, the
-- packages its layout queries import, the queries, and the finishing
-- step that turns the queries' results into the description
data Prep = Prep { p_more :: [CType]
                 , p_pkgs :: [Id]
                 , p_queries :: [Query]
                 , p_finish :: [(String, BitExpr)] -> JValue }

prepareType :: Flags -> SymTab -> CType -> Maybe Prep
prepareType flags symtab t =
    case analyzeType flags symtab t of
      Left _ -> Nothing
      Right ta -> prepare ta
  where
    width = maybe JNull JNum
    typeBits = case analyzeType flags symtab t of
                 Right ta -> getWidth ta
                 Left _ -> Nothing
    plain more json = Just (Prep more [] [] (const json))
    -- the queries a type poses, once its width is known
    posed queries = maybe [] queries typeBits

    prepare :: TypeAnalysis -> Maybe Prep
    prepare ta@(Primary {}) =
        plain [] (JObj [("kind", JStr "primary"), ("width", width (getWidth ta))])
    prepare (Alias _ _ _ target) =
        plain [target] (JObj [("kind", JStr "alias"), ("target", JStr (typeName target))])
    prepare (Struct _ _ _ _ fields _) =
        let fws = [ (i, qualToType qt, fw) | (i, qt, fw) <- fields ]
            members = [ Member (getIdBaseString i) (Just ft) fw Nothing Nothing Nothing
                      | (i, ft, fw) <- fws ]
            queries w = [ q | (n, (i, _, Just fw)) <- zip [0 :: Int ..] fws, fw > 0
                            , let q = Query (valName n) w fw (\ b -> packed (CSelect (unpacked b) i)) ]
            -- a derived instance concatenates the fields, first field highest
            derivedRanges =
                case (typeBits, sequence [ fw | (_, _, fw) <- fws ]) of
                  (Just total, Just ws) ->
                      let his = scanl (-) total ws
                      in  Just (zip (drop 1 his) his)
                  _ -> Nothing
            finish results =
                let members' = attach results members
                    derived = case derivedRanges of
                                Just rs -> and [ Just r == fixedRange m | (m, r) <- zip members' rs ]
                                Nothing -> False
                in  JObj [ ("kind", JStr "struct")
                         , ("width", width typeBits)
                         , ("layout", JStr (layoutStr (layoutOf members' derived)))
                         , ("members", JArr (map (memberJson Nothing) members')) ]
        in  Just (Prep [ ft | (_, ft, _) <- fws ] (typePackages t) (posed queries) finish)
    prepare (Enum qi cons _) =
        let members = [ Member (getIdBaseString c) Nothing Nothing (fmap conTag (tagInfo symtab qi c)) Nothing Nothing
                      | c <- cons ]
            queries w = [ Query (isName n) w 1 (isCon (CPCon c (replicate (conArity qi c) (CPAny pos))))
                        | (n, c) <- zip [0 :: Int ..] cons ]
            finish results =
                let members' = attach results members
                    table = tagTable members'
                    derived = case table of
                                Just ((0, hi), sels) ->
                                    Just hi == typeBits &&
                                    and [ map Just sel == [mb_derivedTag m] | (m, sel) <- zip members' sels ]
                                _ -> False
                in  JObj [ ("kind", JStr "enum")
                         , ("width", width typeBits)
                         , ("layout", JStr (layoutStr (layoutOf members' derived)))
                         , ("members", JArr (memberJsons "value" "values" table members')) ]
        in  Just (Prep [] (typePackages t) (posed queries) finish)
    prepare (TaggedUnion qi _ _ _ arms _) =
        let members = [ Member (getIdBaseString c) (Just at) aw (fmap conTag (tagInfo symtab qi c)) Nothing Nothing
                      | (c, at, aw) <- arms ]
            -- a constructor of several anonymous fields has no one
            -- payload expression to pack, so only its tag is asked for
            queries w = concat
                [ Query (isName n) w 1 (isCon (CPCon c (replicate (conArity qi c) (CPAny pos)))) :
                  [ Query (valName n) w pw (payloadOf c)
                  | Just pw <- [aw], pw > 0, conArity qi c == 1 ]
                | (n, (c, _, aw)) <- zip [0 :: Int ..] arms ]
            finish results =
                let members' = attach results members
                    -- a derived instance puts the tag above the widest payload,
                    -- each payload right-aligned
                    table = tagTable members'
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
                in  JObj $ [ ("kind", JStr "union")
                           , ("width", width typeBits)
                           , ("layout", JStr (layoutStr (layoutOf members' derived))) ] ++
                           tag_range ++
                           [ ("members", JArr (memberJsons "tag" "tags" table members')) ]
        in  Just (Prep [ at | (_, at, _) <- arms ] (typePackages t) (posed queries) finish)
    prepare ta@(Vector _ len elt _) =
        let w = getWidth ta
            n = case len of
                  TCon (TyNum v _) -> Just v
                  _                -> Nothing
            stride = case (w, n) of
                       (Just total, Just k) | k > 0 -> Just (total `div` k)
                       _ -> Nothing
        in  plain [elt] (JObj $ [ ("kind", JStr "vector")
                                , ("width", width w)
                                , ("length", width n)
                                , ("elem", JStr (typeName elt))
                                , ("stride", width stride) ])
    prepare _ = Nothing

    -- the queries, in the type's source syntax
    pos = noPosition
    isName n = "is" ++ show n
    valName n = "val" ++ show n
    unpacked b = CHasType (CApply (CVar idUnpack) [b]) (CQType [] t)
    packed e = CApply (CVar idPack) [e]
    isCon pat b = Ccase pos (unpacked b)
                    [ CCaseArm pat [] (packed (CCon idTrue []))
                    , CCaseArm (CPAny pos) [] (packed (CCon idFalse [])) ]
    payloadOf c b = let x = mkId pos (mkFString "payload")
                    in  Ccase pos (unpacked b)
                          [ CCaseArm (CPCon c [CPVar x]) [] (packed (CVar x))
                          , CCaseArm (CPAny pos) [] (CAny pos UDontCare) ]

    -- the arguments a pattern for the constructor has to supply, as the
    -- type checker counts them from the constructor's declared type:
    -- none for a declared unit payload, one per field for anonymous
    -- fields, else one
    conArity :: Id -> Id -> Int
    conArity ty c =
        case conInfo symtab ty c of
          Just (ConInfo { ci_assump = _ :>: Forall _ (_ :=> ct) }) ->
              case fst (getArrows ct) of
                [argTy] | isTypeUnit argTy -> 0
                        | Just (TyCon _ _ (TIstruct (SDataCon _ False) fs)) <- leftTyCon argTy -> length fs
                _ -> 1
          Nothing -> 1

    -- the members with their queries' results
    attach :: [(String, BitExpr)] -> [Member] -> [Member]
    attach results members =
        [ m { mb_when = lookup (isName n) results, mb_bits = lookup (valName n) results }
        | (n, m) <- zip [0 :: Int ..] members ]

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
    -- values of those bits that select it.  A lone member is selected
    -- by an empty range at the top of the value (the derived instance's
    -- zero-width tag).
    tagTable :: [Member] -> Maybe ((Integer, Integer), [[Integer]])
    tagTable members = do
        whens <- mapM mb_when members
        let supp = S.unions (map bitExprSupport whens)
            always (BConst _ 1) = True
            always _ = False
        if S.null supp
          then case (typeBits, whens) of
                 (Just w, [e]) | always e -> Just ((w, w), [[0]])
                 _ -> Nothing
          else do
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
typePackages = distinct . go
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

-- The symbol table's entry for a constructor of the given type
conInfo :: SymTab -> Id -> Id -> Maybe ConInfo
conInfo symtab ty con =
    case findCon symtab con of
      Just [ci] -> Just ci
      Just cis  -> case [ ci | ci <- cis, qualEq ty (ci_id ci) ] of
                     [ci] -> Just ci
                     _    -> Nothing
      Nothing   -> Nothing

-- The packing information of a constructor of the given type
tagInfo :: SymTab -> Id -> Id -> Maybe ConTagInfo
tagInfo symtab ty con = fmap ci_taginfo (conInfo symtab ty con)

-- Every type the signals name, and every type those reach, described
-- once each, keyed by name.  The descriptions are prepared first, so
-- that every type's layout queries reduce together.
typesJson :: ErrorHandle -> Flags -> SymTab -> [CType] -> IO JValue
typesJson errh flags symtab roots = do
    let preps = collect S.empty [] roots
    results <- reduceQueries errh flags [ (p_pkgs p, p_queries p) | (_, p) <- preps ]
    return (JObj [ (name, p_finish p (either (const []) id r)) | ((name, p), r) <- zip preps results ])
  where
    collect _ acc [] = reverse acc
    collect seen acc (t:ts)
      | S.member name seen = collect seen acc ts
      | otherwise =
          case prepareType flags symtab t of
            Nothing -> collect seen' acc ts
            Just p -> collect seen' ((name, p) : acc) (ts ++ p_more p)
      where name = typeName t
            seen' = S.insert name seen

-- ---------------

-- The length of a builder's output, forcing it
rendered :: B.Builder -> Int
rendered = fromIntegral . BL.length . B.toLazyByteString

-- Runs a step, and under -v reports how long it took, the step's result
-- forced as far as the given measure takes it
phase :: Flags -> String -> (a -> Int) -> IO a -> IO a
phase flags name measure act
  | not (verbose flags) = act
  | otherwise = do
      t0 <- getCurrentTime
      x <- act
      _ <- evaluate (measure x)
      t1 <- getCurrentTime
      hPutStrLn stderr ("wavedebuginfo: " ++ name ++ ": " ++ show (diffUTCTime t1 t0))
      return x

-- The JSON bytes of the debug information for the design under `top`,
-- whose scope path in the dump is main.top
waveDebugInfo :: ErrorHandle -> Flags -> SymTab -> HierMap -> [(String, ABinEitherModInfo)]
              -> String -> IO B.Builder
waveDebugInfo errh flags symtab hier mods top = do
    let design = Design { d_hier = hier
                        , d_mods = M.fromList mods
                        , d_hide = not (tclShowHidden flags) }
    _ <- phase flags "decoding the modules" id $
        return (sum [ M.size (apkg_inst_tree p) + length (apkg_state_instances p) + length (apkg_interface p) + length (apkg_rules p)
                    | (_, abmi) <- mods, let p = abemi_apkg abmi ])
    let signals0 = moduleSignals design ["main", "top"] [] top
        roots = distinct (mapMaybe sig_type signals0)
        names = M.fromList [ (t, typeName t) | t <- roots ]
        nameOf t = M.findWithDefault (typeName t) t names
    signals <- phase flags "computing the signals" (\ ss -> length (concatMap (\ s -> sig_synth s ++ sig_bsv s ++ [sig_kind s]) ss)) $
        return signals0
    _ <- phase flags "rendering the signals" rendered $ return (render (JArr (map (signalJson nameOf) signals)))
    types <- phase flags "type layouts" (const 0) $ typesJson errh flags symtab roots
    when (verbose flags) $
        hPutStrLn stderr ("wavedebuginfo: " ++ show (length signals) ++ " signals, " ++
                          show (length roots) ++ " types named by them")
    phase flags "rendering" rendered $ return $ render $
          JObj [ ("format", JStr "bsc-wave-debug-info")
               , ("version", JNum 1)
               , ("top", JStr top)
               , ("signals", JArr (map (signalJson nameOf) signals))
               , ("types", types) ]
