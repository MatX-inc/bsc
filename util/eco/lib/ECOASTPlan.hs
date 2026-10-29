-- External planning over neutral native graphs. Proposals are not proofs and
-- remain subject to the native keep checker and independent AST equivalence.
module ECOASTPlan
    ( KeepProposal(..), KeepDecision(..), proposeKeeps, makeBackendPlan ) where

import Control.Monad (foldM, unless)
import Data.Char (isHexDigit)
import Data.List (isPrefixOf, minimumBy, sort)
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Ord (comparing)
import ECO.Types
import ECO.Validate (validateGraph)

data KeepDecision = KeepDecision
    { kdOldName :: String, kdOldUses :: Int, kdStatus :: String
    , kdCandidates :: [String], kdSelected :: Maybe String
    } deriving (Eq, Read, Show)

data KeepProposal = KeepProposal
    { kpModule :: String, kpKeeps :: [(String,String,EcoKey)]
    , kpDecisions :: [KeepDecision]
    } deriving (Eq, Read, Show)

-- Hash-cons complete typed DAG nodes across both graphs. Integer identities
-- here are private structural identities, never compiler FString handles.
data Shape = Shape String String (Maybe String) [Int] deriving (Eq, Ord)
data Value = Value { valueShape :: Int, valuePure :: Bool, valueConstant :: Bool }
type Shapes = M.Map Shape Int
type Values = M.Map EcoKey Value
type Nodes = M.Map EcoKey EcoNode

closed :: EcoGraph -> Shapes -> Either String (Values,Shapes)
closed graph initial = do
    validateGraph graph
    foldM (visit S.empty) (M.empty,initial) (M.keys nodes)
  where
    nodes = M.fromList [(enKey n,n) | n <- egNodes graph]
    visit active state@(memo,_) key
        | M.member key memo = Right state
        | S.member key active = Left ("cyclic ECO graph at " ++ key)
        | otherwise = do
            let n = nodes M.! key
            (memo',table) <- foldM (visit (S.insert key active)) state (enOperands n)
            let children = [memo' M.! k | k <- enOperands n]
                pureValue = not (enOpaque n) && enOp n /= "any" && all valuePure children
                literal = any (`isPrefixOf` enOp n) ["integer:","real:","string:"]
                constant = (literal || not (null children) && all valueConstant children)
                           && enBoundary n == Nothing && enOp n /= "any"
                shape = Shape (enType n) (enOp n) (enBoundary n) (map valueShape children)
                idx = M.findWithDefault (M.size table) shape table
                wrapper = case enOperands n of
                    [child] -> enOp n == "definition" && enType n == enType (nodes M.! child)
                    _ -> False
                (content,table') = if wrapper then (valueShape (head children),table)
                                  else (idx,M.insert shape idx table)
            return (M.insert key (Value content pureValue constant) memo',table')

uses :: EcoGraph -> M.Map EcoKey Int
uses graph = M.fromListWith (+)
    [(key,1) | n <- egNodes graph, key <- enOperands n,
               maybe False ((== "definition") . enOp) (M.lookup key nodes)]
  where nodes = M.fromList [(enKey n,n) | n <- egNodes graph]

proposeKeeps :: EcoGraph -> EcoGraph -> EcoGraph -> EcoGraph -> Either String KeepProposal
proposeKeeps oldPre oldFinal newPre newFinal = do
    mapM_ validateGraph [oldPre,oldFinal,newPre,newFinal]
    unless (all ((== egModule newPre) . egModule) [oldPre,oldFinal,newFinal]) $
        Left "keep proposal module mismatch"
    (oldValues,table) <- closed oldPre M.empty
    (newValues,_) <- closed newPre table
    let old = M.fromList [(enKey n,n) | n <- egNodes oldPre]
        new = M.fromList [(enKey n,n) | n <- egNodes newPre]
        oldUses = uses oldPre; newUses = uses newPre
        count u k = M.findWithDefault 0 k u
        names g = S.fromList [s | n <- egNodes g, Just s <- [enName n]]
        oldSurvived = names oldFinal; newSurvived = names newFinal
        name n = fromMaybe "" (enName n)
        internal n = enOp n == "definition" && isJust (enName n) &&
                     enBoundary n == Nothing && not (enOpaque n) && enEligible n
        eligible nodes values n = internal n && "Bit:" `isPrefixOf` enType n &&
            valuePure (values M.! enKey n) && not (valueConstant (values M.! enKey n)) &&
            not ("field:" `isPrefixOf` enKey n) &&
            not (any ("tuple-parent:" `isPrefixOf`) (enContexts n)) &&
            case enOperands n of
                [rhs] -> enOp (nodes M.! rhs) `notElem` ["definition","boundary","any"]
                _ -> False
        represented = S.fromList [valueShape (newValues M.! enKey n) | n <- M.elems new,
                                  internal n, name n `S.member` newSurvived]
        candidates = M.fromListWith (++)
            [(valueShape (newValues M.! enKey n),[n]) | n <- M.elems new,
                eligible new newValues n, name n `S.notMember` newSurvived,
                count newUses (enKey n) > 0]
        decide n =
            let value = valueShape (oldValues M.! enKey n)
                pool = [c | c <- M.findWithDefault [] value candidates,
                            count newUses (enKey c) < count oldUses (enKey n)]
                status | value `S.member` represented = "value-still-represented"
                       | null pool = "no-equal-typed-lost-helper"
                       | otherwise = "selected"
                selected = if status == "selected"
                           then Just (minimumBy (comparing (\c -> (name c /= name n,name c))) pool)
                           else Nothing
                decision = KeepDecision (name n) (count oldUses (enKey n)) status
                                        (sort (map name pool)) (fmap name selected)
                keep c = (name c,enType c,head (enOperands c))
            in (decision,fmap keep selected)
        decisions = [decide n | n <- M.elems old, eligible old oldValues n,
                         name n `S.member` oldSurvived, count oldUses (enKey n) >= 2]
    return $ KeepProposal (egModule newPre)
        (S.toAscList (S.fromList [k | (_,Just k) <- decisions])) (map fst decisions)

makeBackendPlan :: KeepProposal -> String -> String -> Either String EcoBackendPlan
makeBackendPlan proposal captureHash graphHash = do
    unless (all (\s -> length s == 64 && all isHexDigit s) [captureHash,graphHash]) $
        Left "invalid native plan SHA256"
    return $ EcoBackendPlan 1 (kpModule proposal) captureHash graphHash (kpKeeps proposal)
