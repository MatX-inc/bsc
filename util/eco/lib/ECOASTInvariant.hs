-- Consume native observation metadata, never RTL text. Compiler outcomes and
-- annotations are provenance only; each exported assertion is proved again
-- over the checked final Verilog AST with arbitrary binary boundary values.
module ECOASTInvariant
  ( EvidenceJSON(..), InvariantRow(..), InvariantReport(..)
  , parseEvidenceJSON, renderEvidenceJSON, consumeEvidenceJSON
  , consumeASTEvidence, renderInvariantReport, renderInvariantAssertions
  , consumeSelectedASTEvidence, composeInvariantBindings
  , invariantContracts, writeInvariantOutputs
  ) where

import Control.Monad (foldM, unless)
import Data.Char (chr, digitToInt, isDigit, isHexDigit, ord)
import Data.List (intercalate, nubBy, sortOn)
import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Numeric (showHex)
import Text.ParserCombinators.ReadP hiding (optional)
import Text.Read (readMaybe)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import ECOAST
import ECOASTProof
import ECOASTProofContext (childPinsFromInstances)
import ECO.Replay
import BinData (encode)
import GenABin ()
import PPrint (ppReadable)
import Verilog

data EvidenceJSON = JObject [(String,EvidenceJSON)] | JArray [EvidenceJSON]
                  | JString String | JNumber String | JBool Bool | JNull
  deriving (Eq,Show)
data InvariantRow = InvariantRow
  { irId :: String, irStage :: String, irSource :: EvidenceJSON
  , irPredicate :: Maybe VExpr, irResult :: Maybe ProofResult, irReason :: String
  } deriving Show
data InvariantReport = InvariantReport
  { irModule :: String, irAvailability :: String, irBindings :: [(String,String)]
  , irSources :: [(String,FileIdentity)], irRows :: [InvariantRow]
  , irDescriptors :: EvidenceJSON
  } deriving Show

-- This parser accepts only metadata JSON. Duplicate keys are rejected rather
-- than giving different readers different interpretations of the same record.
parseEvidenceJSON :: String -> Either String EvidenceJSON
parseEvidenceJSON source = case [x | (x,"")<-readP_to_S (spaces *> value <* spaces <* eof) source] of
  [x] -> Right x
  _ -> Left "Malformed or ambiguous native evidence JSON"
  where
    spaces=skipSpaces
    token p=p <* spaces
    value=token (object +++ array +++ (JString <$> string) +++ number +++
                 (stringLit "true" >> return (JBool True)) +++
                 (stringLit "false" >> return (JBool False)) +++
                 (stringLit "null" >> return JNull))
    stringLit=Text.ParserCombinators.ReadP.string
    object=do
      char '{'; spaces
      xs<-sepBy (do k<-token string; token (char ':'); x<-value; return (k,x)) (token (char ','))
      char '}'
      if length xs==S.size (S.fromList (map fst xs)) then return (JObject xs) else pfail
    array=JArray <$> between (char '[' *> spaces) (char ']') (sepBy value (token (char ',')))
    string=between (char '"') (char '"') (many character)
    character=satisfy (\c->c/='"' && c/='\\' && ord c>=32) +++ do
      char '\\'; c<-get
      case c of
        '"'->return '"'; '\\'->return '\\'; '/'->return '/'
        'b'->return '\b'; 'f'->return '\f'; 'n'->return '\n'; 'r'->return '\r'; 't'->return '\t'
        'u'->do ds<-count 4 (satisfy isHexDigit)
                let n=foldl (\a d->16*a+digitToInt d) 0 ds
                if n>=0xD800 && n<=0xDFFF then pfail else return (chr n)
        _->pfail
    number=do
      sign<-option "" (char '-' >> return "-")
      whole<-(char '0' >> return "0") +++ ((:) <$> satisfy (\c->c>='1' && c<='9') <*> munch isDigit)
      frac<-option "" (do char '.'; ds<-munch1 isDigit; return ('.':ds))
      expo<-option "" (do e<-satisfy (`elem` "eE"); s<-option "" ((:[]) <$> satisfy (`elem` "+-")); ds<-munch1 isDigit; return (e:s++ds))
      return (JNumber (sign++whole++frac++expo))

renderEvidenceJSON :: EvidenceJSON -> String
renderEvidenceJSON (JObject xs)="{"++intercalate "," [quote k++":"++renderEvidenceJSON x|(k,x)<-xs]++"}"
renderEvidenceJSON (JArray xs)="["++intercalate "," (map renderEvidenceJSON xs)++"]"
renderEvidenceJSON (JString x)=quote x
renderEvidenceJSON (JNumber x)=x
renderEvidenceJSON (JBool x)=if x then "true" else "false"
renderEvidenceJSON JNull="null"
quote :: String -> String
quote x='"':concatMap esc x++"\""
  where
    esc '"'="\\\""; esc '\\'="\\\\"
    esc c | ord c<32=let h=showHex (ord c) "" in "\\u"++replicate (4-length h) '0'++h
          | otherwise=[c]
field :: String -> EvidenceJSON -> Either String EvidenceJSON
field k (JObject xs)=maybe (Left ("Missing evidence field: "++k)) Right (lookup k xs)
field k _=Left ("Expected object for: "++k)
str :: EvidenceJSON -> Either String String
str (JString s)=Right s
str _=Left "Expected metadata string"
arr :: EvidenceJSON -> Either String [EvidenceJSON]
arr (JArray xs)=Right xs
arr _=Left "Expected metadata array"
textField k j=field k j >>= str
arrayField k j=field k j >>= arr
optional k j=case field k j of Right x->x;Left _->JNull
widthOf j=do
  t<-field "type" j; bits<-field "bits" t
  case bits of
    JNumber s | Just n<-readMaybe s,n>0 -> Right n
    _->Left "Only positive, statically sized bit vectors are supported"

-- All widths come from typed AST declarations, never names or rendered RTL.
symbols :: VProgram -> Either String (String,M.Map String Integer)
symbols (VProgram [m] _ _)=do
  rows<-mapM one (ports++decls)
  ws<-foldM insert M.empty rows
  return (getVIdString (vm_name m),ws)
  where
    ports=[(v,r) | VAInput v r<-ps]++[(v,r)|VAOutput v r<-ps]++
          [(fromMaybe v internal,r)|VAInout v internal (Just r)<-ps]
    ps=concatMap fst (vm_ports m)
    decls=concatMap declaration (concatMap flatten (vm_body m))
    declaration (VMDecl (VVDecl _ r vs))=[(v,r)|VVar v<-vs]
    declaration (VMDecl (VVDWire r (VVar v) _))=[(v,r)]
    declaration _=[]
    flatten (VMComment _ x)=flatten x
    flatten (VMRegGroup _ _ _ x)=flatten x
    flatten (VMGroup _ xs)=concatMap flatten (concat xs)
    flatten x=[x]
    integer (VEConst n)=Just n
    integer (VEWConst _ _ _ n)=Just n
    integer _=Nothing
    one (v,Nothing)=Right (getVIdString v,1)
    one (v,Just (h,l))=case (integer h,integer l) of
      (Just hi,Just 0) | hi>=0 ->Right (getVIdString v,hi+1)
      _->Left ("Unsupported native AST range for "++getVIdString v)
    insert m (n,w)=case M.lookup n m of
      Nothing->Right (M.insert n w m)
      Just old | old==w ->Right m
      _->Left ("Inconsistent native AST declaration width: "++n)
symbols _=Left "Invariant consumption requires exactly one native AST module"

lower :: M.Map String Integer -> M.Map String String -> M.Map String EvidenceJSON
      -> S.Set String -> EvidenceJSON -> Either String (Integer,VExpr)
lower ws binding defs active j=do
  w<-widthOf j
  kind<-textField "kind" j
  expr<-case kind of
    "integer"->do
      raw<-textField "value" j
      n<-maybe (Left "Invalid integer observation") Right (readMaybe raw)
      unless (n>=0 && n<2^w) (Left "Integer observation exceeds declared width")
      return (VEWConst (mkVId "") w 2 n)
    "definition"->do
      ident<-textField "id" j
      case M.lookup ident defs of
        Just d->do
          unless (ident `S.notMember` active) (Left ("Cyclic observed definition: "++ident))
          dwidth<-widthOf d
          unless (dwidth==w) (Left ("Observed definition width mismatch: "++ident))
          rhs<-field "expression" d
          (rw,re)<-lower ws binding defs (S.insert ident active) rhs
          unless (rw==w) (Left ("Observed RHS width mismatch: "++ident))
          return re
        Nothing->leaf w j
    "port"->leaf w j
    "parameter"->leaf w j
    "primitive"->do
      operation<-textField "operator" j
      args<-arrayField "arguments" j >>= mapM (lower ws binding defs active)
      primitive operation w args
    _->Left ("Unsupported observation expression kind: "++kind)
  return (w,expr)
  where
    leaf w x=do
      old<-textField "id" x
      let n=M.findWithDefault old old binding
      unless (M.lookup n ws==Just w) (Left ("Unbound or width-mismatched final AST signal: "++old++" -> "++n))
      return (VEVar (mkVId n))
    primitive op w args=case (op,args) of
      ("PrimInv",[(aw,a)]) | aw==w ->Right (VEUnOp (mkVId "") VInv a)
      ("PrimBNot",[(1,a)]) | w==1 ->Right (VEUnOp (mkVId "") VNot a)
      _->do
        (vop,boolean,nary)<-case op of
          "PrimAnd"->Right (VAnd,False,True); "PrimOr"->Right (VOr,False,True)
          "PrimXor"->Right (VXor,False,True); "PrimBAnd"->Right (VLAnd,True,True)
          "PrimBOr"->Right (VLOr,True,True); "PrimEQ"->Right (VEQ,True,False)
          "PrimULE"->Right (VULE,True,False); "PrimULT"->Right (VULT,True,False)
          _->Left ("Unsupported observed primitive: "++op)
        unless (length args>=2 && (nary || length args==2)) (Left "Unsupported primitive arity")
        let aw=fst (head args)
        unless (all ((==aw).fst) args && w==(if boolean then 1 else aw) &&
                (op `notElem` ["PrimBAnd","PrimBOr"] || aw==1)) (Left "Observed primitive width mismatch")
        return (foldl1 (\a b->VEOp (mkVId "") a vop b) (map snd args))

consumeEvidenceJSON :: FilePath -> Int -> FilePath -> VProgram -> [ChildPin]
                    -> [(String,String)] -> String -> IO InvariantReport
consumeEvidenceJSON yosys seconds directory program pins binding source=do
  let checked=do
        payload<-parseEvidenceJSON source
        unless (optional "schema" payload==JNumber "1") (Left "Unsupported evidence envelope schema")
        unless (optional "availability" payload==JString "captured") (Left "Native observation payload is unavailable")
        (modname,ws)<-symbols program
        unless (length binding==S.size (S.fromList (map fst binding))) (Left "Duplicate invariant binding source")
        snaps<-arrayField "snapshots" payload
        return (payload,modname,ws,snaps)
  (payload,modname,ws,snaps)<-either (ioError.userError) return checked
  rows<-fmap concat $ mapM (oneSnapshot modname ws) (zip [0::Int ..] snaps)
  let descriptor snap=JObject [(k,optional k snap)|k<-["module","stage","methods","representations","scheduleDeclarations"]]
  return (InvariantReport modname "captured" binding [] rows
    (JObject [("snapshots",JArray (map descriptor snaps)),("expansionAliases",optional "expansionAliases" payload)]))
  where
    oneSnapshot modname ws (index,snap)=do
      let setup=do
            unless (optional "schema" snap==JNumber "1") (Left "Unsupported observation snapshot schema")
            unless (optional "domain" snap==JString "compiler-two-state-IR") (Left "Unsupported observation domain")
            n<-textField "module" snap
            unless (n==modname) (Left "Observation module differs from final native AST")
            defs<-arrayField "definitions" snap
            named<-mapM (\d->do n<-textField "id" d;return (n,d)) defs
            unless (length named==M.size (M.fromList named)) (Left "Duplicate observed definition ID")
            stage<-textField "stage" snap
            observed<-arrayField "observedObligations" snap
            pending<-arrayField "pendingObligations" snap
            let same a b=all (\k->optional k a==optional k b) ["messageKind","operation","left","right"]
            return (stage,M.fromList named,nubBy same (observed++pending))
      case setup of
        Left why->return [InvariantRow ("snapshot-"++show index) "unbound" snap Nothing Nothing why]
        Right (stage,defs,obs)->mapM (oneObligation ws defs stage index) (zip [0::Int ..] obs)
    oneObligation ws defs stage index (n,ob)=do
      let ident="eco_observation_"++show index++"_"++show n
          predicate=do
            op<-textField "operation" ob
            a<-field "left" ob >>= lower ws (M.fromList binding) defs S.empty
            b<-field "right" ob >>= lower ws (M.fromList binding) defs S.empty
            unless (fst a==fst b) (Left "Obligation operand width mismatch")
            operator<-case op of "eq"->Right VEQ;"not-eq"->Right VNE;_->Left "Unsupported obligation operation"
            return (VEOp (mkVId "") (snd a) operator (snd b))
      case predicate of
        Left why->return (InvariantRow ident stage ob Nothing Nothing why)
        Right p->do
          result<-runASTProofWithContracts pins pins [AssertCombinational ident p]
                     yosys seconds (directory </> ident) Binary [] program program
          return (InvariantRow ident stage ob (Just p) (Just result)
                    (if proofPassed result then "independently proved over final binary AST" else "final AST proposition was not independently proved"))

consumeASTEvidence :: FilePath -> Int -> FilePath -> VerilogInfo -> [(String,String)] -> IO InvariantReport
consumeASTEvidence yosys seconds directory snapshot=
  consumeSelectedASTEvidence yosys seconds directory snapshot snapshot
    (viProgram snapshot) (aeOutput (viEnvelope snapshot))

-- Compose only explicit signal relations from accepted proof steps. The input
-- rows map a name before a step to its name afterwards. Width checks and the
-- actual proposition proof still occur against the final AST, never a receipt.
composeInvariantBindings :: VProgram -> [[(String,String)]] -> Either String [(String,String)]
composeInvariantBindings source relations=do
  (_,ws)<-symbols source
  mapM_ unique relations
  let rename n relation=M.findWithDefault n n (M.fromList relation)
  return [(n,final)|n<-M.keys ws,let final=foldl rename n relations,final/=n]
  where
    unique relation=unless (length relation==S.size (S.fromList (map fst relation)) &&
                            length relation==S.size (S.fromList (map snd relation)))
      (Left "Invariant rebinding requires bijective step relations")

-- The source capture supplies observations; the separately identified target
-- supplies assertion semantics. Equal source/target RTL or a stored equivalence
-- receipt is not assumed. Every exported predicate is reproved on `program`.
consumeSelectedASTEvidence :: FilePath -> Int -> FilePath -> VerilogInfo -> VerilogInfo
                          -> VProgram -> FileIdentity -> [(String,String)] -> IO InvariantReport
consumeSelectedASTEvidence yosys seconds directory sourceSnapshot targetContext program target binding=do
  validateVerilogInfo sourceSnapshot
  validateVerilogInfo targetContext
  exact<-verilogRenderingIsExact sourceSnapshot
  unless exact (ioError (userError "Invariant export requires final emitted RTL equal to the captured native AST rendering"))
  let capture=aeCapture (viEnvelope sourceSnapshot)
  targetBytes<-BS.readFile (fiPath target)
  actualTarget<-fileIdentity (fiPath target)
  sameTarget<-sameFileIdentity actualTarget target
  unless (sameTarget && targetBytes==TE.encodeUtf8 (T.pack (renderVerilogText targetContext program)))
    (ioError (userError "Selected invariant target bytes/identity differ from its typed AST rendering"))
  let sameInterface (VProgram [a] _ _) (VProgram [b] _ _)=
        vm_name a==vm_name b && concatMap fst (vm_ports a)==concatMap fst (vm_ports b)
      sameInterface _ _=False
  unless (sameInterface (viProgram sourceSnapshot) program && sameInterface (viProgram targetContext) program)
    (ioError (userError "Selected invariant target changes the native module interface"))
  -- Native pin metadata belongs to these exact child interfaces. A final AST
  -- cannot reuse it after changing an instance's module, parameters or formals.
  let childInterfaces (VProgram modules _ _)=sortOn (\(_,instanceName,_,_)->instanceName)
        [(getVIdString moduleName,getVIdString instanceName,
          parameters,sortOn id (map (getVIdString.fst) ports)) |
          m<-modules,item<-concatMap nested (vm_body m),
          VMInst moduleName instanceName parameters ports<-[item]]
      nested (VMComment _ item)=nested item
      nested (VMRegGroup _ _ _ item)=nested item
      nested (VMGroup _ groups)=concatMap nested (concat groups)
      nested item=[item]
  unless (childInterfaces (viProgram targetContext)==childInterfaces program)
    (ioError (userError "Selected invariant target changes the native child interface binding"))
  createDirectoryIfMissing True directory
  BS.writeFile (directory </> "target.ast") (BS.pack (encode program))
  targetAST<-fileIdentity (directory </> "target.ast")
  evidence<-readReplayEvidence (fiPath capture)
  report<-case evidence of
    Nothing->do
      (modname,_)<-either (ioError.userError) return (symbols program)
      return (InvariantReport modname "unavailable-in-capture" binding [] [] (JObject []))
    Just checked->do
      sameCapture<-sameFileIdentity (reCapture checked) capture
      unless sameCapture (ioError (userError "Evidence capture binding differs from native AST"))
      source<-readFile (fiPath (rePayload checked))
      parsed<-consumeEvidenceJSON yosys seconds directory program
                (childPinsFromInstances (viInstances targetContext)) binding source
      return parsed {irSources=[("payload",rePayload checked),("observedProducer",reObservedProducer checked)]}
  validateVerilogInfo sourceSnapshot
  validateVerilogInfo targetContext
  finalTarget<-fileIdentity (fiPath target)
  finalAST<-fileIdentity (fiPath targetAST)
  sameFinalTarget<-sameFileIdentity finalTarget target
  sameFinalAST<-sameFileIdentity finalAST targetAST
  unless (sameFinalTarget && sameFinalAST)
    (ioError (userError "Selected invariant target changed during assertion proof"))
  finalEvidence<-readReplayEvidence (fiPath capture)
  unless (fmap rePayload evidence==fmap rePayload finalEvidence) (ioError (userError "Native evidence changed during assertion proof"))
  return report {irSources=irSources report++[("sourceSnapshot",viIdentity sourceSnapshot),
    ("sourceCapture",capture),("sourceRTL",aeOutput (viEnvelope sourceSnapshot)),
    ("targetContext",viIdentity targetContext),("targetRTL",target),("targetAST",targetAST)]}

invariantContracts :: InvariantReport -> [ASTContract]
invariantContracts report=[AssertCombinational (irId row) p|row<-irRows report,
                            Just p<-[irPredicate row],Just result<-[irResult row],proofPassed result]
renderInvariantReport :: InvariantReport -> String
renderInvariantReport report=renderEvidenceJSON (JObject
  [("schema",JNumber "1"),("consumer",JString "native-Verilog-AST-independent-obligations")
  ,("module",JString (irModule report)),("availability",JString (irAvailability report))
  ,("domain",JString "binary-combinational-unconstrained-boundary")
  ,("assumptionsExported",JNumber "0"),("initializationOrReachabilityAssumed",JBool False)
  ,("assertions",JNumber (show (length (invariantContracts report))))
  ,("bindings",JArray [JObject [("observed",JString a),("final",JString b)]|(a,b)<-irBindings report])
  ,("sources",JObject [(label,JObject [("path",JString (fiPath f)),("sha256",JString (fiSHA256 f))])|(label,f)<-irSources report])
  ,("rows",JArray (map row (irRows report))),("descriptors",irDescriptors report)])
  where
    row r=JObject [("id",JString (irId r)),("stage",JString (irStage r)),("source",irSource r)
      ,("expression",maybe JNull (JString . ppReadable) (irPredicate r)),("reason",JString (irReason r))
      ,("proof",maybe JNull (either (const JNull) id.parseEvidenceJSON.renderProofResult) (irResult r))]
renderInvariantAssertions :: InvariantReport -> String
renderInvariantAssertions report=unlines
  (["// Independently proved binary combinational assertions; no assumptions.",
    "// Bind only to the module, signal map and exact RTL identity in report.json.",
    "// A four-state simulator or temporal harness needs an explicit domain policy."]++
   ["always @* begin : "++ident++"\n  assert ("++ppReadable p++");\nend"|
      AssertCombinational ident p<-invariantContracts report])
writeInvariantOutputs :: FilePath -> InvariantReport -> IO ()
writeInvariantOutputs directory report=do
  createDirectoryIfMissing True directory
  writeFile (directory </> "report.json") (renderInvariantReport report++"\n")
  writeFile (directory </> "assertions.svh") (renderInvariantAssertions report)
  writeASTContracts (directory </> "assertions.ast-contracts") (invariantContracts report)
