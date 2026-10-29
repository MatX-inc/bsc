-- Read-only observation. No Ids/FStrings are manufactured and no annotation is
-- consumed as a proof. Force serialization only after the native snapshot.
module ECO.Invariant (renderInvariantSnapshot) where

import ASyntax
import ProofObligation
import Id (getIdString, getIdPosition)
import IntLit (IntLit(..))
import VModInfo (VName(..))
import Data.Char (ord)
import Data.List (intercalate)
import qualified Data.Map.Strict as M
import Numeric (showHex)

renderInvariantSnapshot :: String -> APackage ->
                           [(ProofObligation AExpr, MsgFn, ProofResult)] -> String
renderInvariantSnapshot stage package results = object
    [("schema", "1"), ("module", json (getIdString (apkg_name package))),
     ("stage", json stage), ("domain", json "compiler-two-state-IR"),
     ("bindingStatus", json "IR identifiers; final RTL binding must be checked"),
     ("assumptionsMayConstrainReplay", "false"),
     ("definitions", array (map definition (apkg_local_defs package))),
     ("methods", array [method m | m <- apkg_interface package, isMethod m]),
     ("representations", array [object [("port", json n),
         ("sourceType", json (show t)), ("classification", json "representation-only"),
         ("rawInputValidityProved", "false")] |
             (VName n,t) <- M.toAscList (apkg_external_wire_types package)]),
     ("scheduleDeclarations", array [object [("classification",json "user-declaration-not-proof"),
         ("declaration",json (show p))] | p <- apkg_schedule_pragmas package]),
     ("pendingObligations", array [obligation ob msg Nothing |
         (ob,msg) <- apkg_proof_obligations package]),
     ("observedObligations", array [obligation ob msg (Just result) |
         (ob,msg,result) <- results])]
  where
    definition d = object [("id",json (getIdString (adef_objid d))),
        ("sourcePosition",json (show (getIdPosition (adef_objid d)))),
        ("type",atype (adef_type d)), ("expression",expr (adef_expr d)),
        ("classification",json "compiler-derived-definition")]
    isMethod AIDef {} = True
    isMethod AIAction {} = True
    isMethod AIActionValue {} = True
    isMethod _ = False
    method m = object [("id",json (getIdString (aif_name m))),
        ("sourcePosition",json (show (getIdPosition (aif_name m)))),
        ("classification",json "compiler-derived-method-descriptor"),
        ("predicate",expr (aIfacePred m)),
        ("arguments",array [object [("id",json (getIdString i)),("type",atype t)] |
            (i,t) <- aIfaceArgs m]),
        ("formalInterface",json (show (aif_fieldinfo m))),
        ("callerEnableImpliesReadyProved", "false"),
        ("argumentIrrelevanceWhenDisabledProved", "false")]
    obligation ob msg result =
        let (op,x,y) = case ob of
                ProveEq x y -> ("eq",x,y)
                ProveNotEq x y -> ("not-eq",x,y)
        in object [("messageKind",json (mf_ident msg)),
            ("classification",json (case result of
                Just Proven -> "compiler-proved"
                Just Disproven -> "disproven"
                Just Inconclusive -> "inconclusive"
                Nothing -> "pending-not-proved")),
            ("proofPass",json "AProofs/checkProofsObserved"),
            ("proofDomain",json "compiler-two-state-IR-SAT"),
            ("operation",json op), ("left",expr x), ("right",expr y),
            ("usableAsRTLAssumptionWithoutReproof", "false")]

atype :: AType -> String
atype t = object [("descriptor",json (show t)),
    ("bits",case t of ATBit width -> show width; _ -> "null")]

expr :: AExpr -> String
expr e = object ([("type",atype (ae_type e))] ++ fields e)
  where
    reference kind i = [("kind",json kind),("id",json (getIdString i))]
    fields (ASDef _ i) = reference "definition" i
    fields (ASPort _ i) = reference "port" i
    fields (ASParam _ i) = reference "parameter" i
    fields (ASInt _ _ lit) = [("kind",json "integer"),("value",json (show (ilValue lit)))]
    fields (APrim _ _ op args) = [("kind",json "primitive"),("operator",json (show op)),
                                ("arguments",array (map expr args))]
    fields (ATuple _ args) = [("kind",json "tuple"),("arguments",array (map expr args))]
    fields (ATupleSel _ value index) = [("kind",json "tuple-select"),("value",expr value),("index",show index)]
    fields (ASAny _ chosen) = [("kind",json "undetermined"),
        ("chosen",maybe "null" expr chosen), ("validInputRestriction", "false")]
    fields other = [("kind",json "opaque"),("compilerIR",json (show other)),
                    ("requiresIndependentLoweringProof", "true")]

object :: [(String,String)] -> String
object fields = "{" ++ intercalate "," [json k ++ ":" ++ v | (k,v) <- fields] ++ "}"
array :: [String] -> String
array xs = "[" ++ intercalate "," xs ++ "]"
json :: String -> String
json s = '"' : concatMap escape s ++ "\""
  where
    escape '"' = "\\\""
    escape '\\' = "\\\\"
    escape c | ord c < 32 = let h = showHex (ord c) ""
                            in "\\u" ++ replicate (4 - length h) '0' ++ h
             | otherwise = [c]
