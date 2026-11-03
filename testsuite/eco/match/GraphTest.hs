{-# OPTIONS_GHC -Wno-missing-fields #-}
module Main where

import ASyntax
import ECO.Types
import ECOMatchTypes
import ECO.Graph
import ECOMatch
import Id (mk_homeless_id)
import IntLit (ilSizedHex)
import Prim (PrimOp(..))
import VModInfo
import SchedInfo
import Control.Monad (unless)
import qualified Data.Map.Strict as M
import System.Exit (die)

ident :: String -> AId
ident = mk_homeless_id

typ :: AType
typ = ATBit 8

definition :: String -> AExpr -> ADef
definition n e = ADef (ident n) typ e []

signalInfo :: ASPSignalInfo
signalInfo = ASPSignalInfo [] [] [] [] [] []
    [(ident "RL_update",[ident "WILL_FIRE_RL_update"])]
    [ident "sum"] [] []

registerInstance :: AVInst
registerInstance = AVInst (ident "rA") typ False [] M.empty vmi [] []
  where
    writeMethod = ident "write"
    field = Method writeMethod Nothing Nothing 1 [[(VName "D_IN",[])]] []
                      (Just (VName "EN",[]))
    conflicts = MethodConflictInfo [] [] [] [] [] [(writeMethod,writeMethod)] []
    schedule = SchedInfo conflicts [] [] []
    vmi = mkVModInfo (VName "RegN") (error "unexpected clock inspection")
             (error "unexpected reset inspection") [] [field] schedule (VPathInfo [])

-- The adapter deliberately projects only the scheduled local identities.
-- Missing fields in this fixture expose any accidental attempt to serialize
-- or force the whole earlier package.
scheduled :: APackage
scheduled = APackage {apkg_local_defs = [definition "offset" (ASInt (ident "lit") typ (ilSizedHex 8 1))]}

package :: String -> Integer -> ASPackage
package source offset = ASPackage
    { aspkg_name = ident "mkExample"
    , aspkg_is_wrapped = False
    , aspkg_parameters = []
    , aspkg_outputs = [(ident "result",typ)]
    , aspkg_inputs = []
    , aspkg_inouts = []
    , aspkg_state_instances = [registerInstance]
    , aspkg_state_outputs = [(ident "rA",typ),(ident "rB",typ)]
    , aspkg_values =
        [ definition "offset" (ASInt (ident "lit") typ (ilSizedHex 8 offset))
        , definition "sameConstant" (ASInt (ident "anotherLit") typ (ilSizedHex 8 offset))
        , definition "sum" (APrim (ident "plus") typ PrimAdd
                              [ASPort typ (ident source),ASDef typ (ident "offset")])
        , definition "result" (ASDef typ (ident "sum"))
        , definition "rA$D_IN" (ASPort typ (ident source))
        , definition "rA$EN" (ASAny typ Nothing)
        , definition "WILL_FIRE_RL_update" (ASAny typ Nothing)
        , definition "foreignResult" (AFunCall typ (ident "call") "foreignFunction" True [])
        ]
    , aspkg_inout_values = []
    , aspkg_foreign_calls = []
    , aspkg_inlined_ports = []
    , aspkg_signal_info = signalInfo
    , aspkg_comment_info = ASPCommentInfo [] []
    }

check :: String -> Bool -> IO ()
check label ok = unless ok (die ("FAIL: " ++ label))

right :: Show a => Either a b -> IO b
right (Right x) = return x
right (Left e) = die (show e)

main :: IO ()
main = do
    let old = package "rA" 1
        new = package "rB" 2
        og = extractFinalGraph scheduled old old
        ng = extractFinalGraph scheduled new new
        ns = M.fromList [(enKey n,n) | n <- egNodes og]
        at k = ns M.! k
    right (validateGraph og)
    check "final typed operands are ordered references" $
        enOperands (at "def:sum:rhs") == ["wire:rA","def:offset"]
    check "exact integer width base and value" $
        enOp (at "def:offset:rhs") == "integer:(Just 8,16,1)"
    check "equal literal occurrences are not merged" $
        M.member "def:sameConstant:rhs" ns && M.member "def:offset:rhs" ns
    check "scheduled identity seed" $
        "scheduled-definition:offset" `elem` enContexts (at "def:offset")
    check "AState mux role" $
        "astate-mux-selector:sum" `elem` enContexts (at "def:sum")
    check "AState rule role" $
        "rule:RL_update:fire:0" `elem` enContexts (at "def:WILL_FIRE_RL_update")
    check "state boundaries protected" $
        not (enEligible (at "wire:rA")) && enBoundary (at "wire:rA") == Just "state-output:rA"
    check "top-level result protected" $
        not (enEligible (at "def:result")) && enBoundary (at "def:result") == Just "output:result"
    check "register data input is a protected consumer boundary" $
        not (enEligible (at "def:rA$D_IN")) &&
        enBoundary (at "def:rA$D_IN") == Just "instance-method-input:rA:D_IN"
    check "foreign expression opaque and protected" $
        enOpaque (at "def:foreignResult") && not (enEligible (at "def:foreignResult"))
    check "existing don't-care retained without completion" $
        enEligible (at "def:WILL_FIRE_RL_update") && enOp (at "def:WILL_FIRE_RL_update:rhs") == "any"
    matched <- right (matchGraphs og ng)
    check "real adapter changed register and constant correspondence" $
        any (\c -> ecOld c == "def:sum" && ecNew c == "def:sum" && ecKind c == ChangedRole)
            (emCorrespondences matched)
    check "changed register source matches its input consumer role" $
        any (\c -> ecOld c == "def:rA$D_IN" && ecNew c == "def:rA$D_IN" && ecKind c == ChangedRole)
            (emCorrespondences matched)
    check "neutral graph serializes without live handles" ((read (show og) :: EcoGraph) == og)
    putStrLn "ECO graph adapter: all tests passed"
