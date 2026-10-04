-- | Reconstruct the legacy backend view of rule exclusivity from the
-- schedule's saved relations and its original elaborated module.  The
-- exclusivity cache is deliberately not part of the schedule file format.
module AScheduleRelations
    ( deriveExclusiveRulesDB, methodBeforeRuleEdges ) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (foldl', sortBy)
import Data.Maybe (isJust)

import AScheduleInfo
    ( ExclusiveRulesDB(..), RuleRelationDB(..), RuleRelationInfo(..) )
import ASyntax
import ASyntaxUtil (exprFold)
import AUses (RuleUsesMap, rumToObjectMap)
import Error (internalError)
import Id (isRdyId, cmpIdByName)
import PPrint (ppReadable)
import Util (headOrErr)

-- | The package must be the original .bmod body, before ready values and
-- rule guards are rewritten or unused definitions are removed.  Its method
-- argument dependencies are the ones examined by ASchedule.
--
-- The rule-use map retains every scheduled rule, including subsequently
-- removed rules and generated mutually-exclusive assertion monitors.
-- ASchedule's final SC map only adds to its initial method-use conflicts:
-- resource, cycle and pragma edges are in the relation DB; method-argument
-- self edges are reconstructed here. Implicit method-order constraints are
-- supplied as concrete SC edges chosen by the scheduler, not compiler options.
-- Neither CF-only conflicts nor arbitrary ordering edges imply exclusivity.
deriveExclusiveRulesDB :: APackage -> RuleUsesMap -> RuleRelationDB ->
                          [(ARuleId, ARuleId)] -> ExclusiveRulesDB
deriveExclusiveRulesDB original ruleUses (RuleRelationDB disjoint relations)
                       methodOrderEdges =
    ExclusiveRulesDB (M.fromList (concatMap makeRow (M.keys ruleObjects)))
  where
    ruleObjects = rumToObjectMap ruleUses
    objectUsers = M.fromListWith S.union
        [ (obj, S.singleton rule)
        | (rule, objects) <- M.toList ruleObjects
        , obj <- S.toList objects ]

    initialAndAddedEdges =
        [ (r1, r2)
        | ((r1, r2), info) <- M.toList relations
        , any isJust [mSC info, mRes info, mCycle info, mPragma info] ]

    selfEdges = methodArgumentSelfEdges original relations
    excludedByRule = M.fromListWith S.union
        [ (r1, S.singleton r2)
        | (r1, r2) <- initialAndAddedEdges ++ selfEdges ++ methodOrderEdges ]

    makeRow r1 =
        let objects = M.findWithDefault S.empty r1 ruleObjects
            sharedUsers = S.unions
                [ M.findWithDefault S.empty obj objectUsers
                | obj <- S.toList objects ]
            exclusions = M.findWithDefault S.empty r1 excludedByRule
            candidates = sharedUsers `S.union` exclusions
            -- Preserve ASchedule's shared-object restriction.  Bluesim uses
            -- this CAN_FIRE relation to inhibit later scheduling after an
            -- earlier exclusive rule executes; a superset changes behavior.
            addPair (ds, es) r2
                | r1 /= r2 && r2 `S.member` sharedUsers &&
                  (r1, r2) `S.member` disjoint = (S.insert r2 ds, es)
                | r2 `S.member` exclusions = (ds, S.insert r2 es)
                | otherwise = (ds, es)
            (ds, es) = foldl' addPair (S.empty, S.empty) (S.toList candidates)
        in if S.null ds && S.null es then [] else [(r1, (ds, es))]

-- | Concrete SC constraints for a scheduler which chooses to order every
-- interface method before local rules. A pair (rule, method) means that the
-- method must execute first, matching ASchedule.mkEarlinessEdgesMethods.
-- The scheduler decides whether these constraints apply and persists the
-- resulting edges; loading the schedule never repeats that option decision.
-- Use the rule-use map to include generated monitors and subsequently removed
-- rules in the same rule universe that ASchedule used. Sort the serialized
-- edge list by textual names, independently of identifier interning order.
methodBeforeRuleEdges :: APackage -> RuleUsesMap -> [(ARuleId, ARuleId)]
methodBeforeRuleEdges original ruleUses =
    sortBy compareEdge [ (rule, method)
    | rule <- S.toList userRules
    , method <- S.toList interfaceRules ]
  where
    compareEdge (rule1, method1) (rule2, method2) =
        case cmpIdByName rule1 rule2 of
            EQ -> cmpIdByName method1 method2
            order -> order
    interfaceRules = S.fromList
        (concatMap interfaceRuleIds (apkg_interface original))
    userRules = M.keysSet ruleUses `S.difference` interfaceRules

-- These are precisely the names selected by ASchedule.cvtIfc; an action
-- method can have several split rules rather than its interface field name.
interfaceRuleIds :: AIFace -> [ARuleId]
interfaceRuleIds (AIAction { aif_body = rules }) = map arule_id rules
interfaceRuleIds (AIActionValue { aif_body = rules }) = map arule_id rules
interfaceRuleIds (AIDef { aif_name = name })
    | not (isRdyId name) = [name]
interfaceRuleIds _ = []

-- The dependency walk mirrors ASchedule.extractMethodArgEdges, but needs
-- only the existence of each edge, not a diagnostic conflict explanation.
-- Keep the definition map lazy: definition dependencies form its own knot.
methodArgumentSelfEdges :: APackage ->
                           M.Map (ARuleId, ARuleId) RuleRelationInfo ->
                           [(ARuleId, ARuleId)]
methodArgumentSelfEdges original relations =
    [ (name, name)
    | method <- apkg_interface original
    , let name = aif_name method
    , not (alreadyConflicts name)
    , not (S.null (argumentUses method)) ]
  where
    alreadyConflicts name =
        maybe False (isJust . mSC) (M.lookup (name, name) relations)

    definitionPorts = M.fromList
        [ (name, expressionPorts expression)
        | ADef name _ expression _ <- apkg_local_defs original ]

    expressionPorts expression = exprFold collect S.empty expression
      where
        collect (ASPort _ name) ports = S.insert name ports
        collect (ASDef _ name) ports =
            case M.lookup name definitionPorts of
                Just used -> S.union used ports
                Nothing -> internalError
                    ("AScheduleRelations: missing original definition " ++
                     ppReadable name)
        collect _ ports = ports

    actionConditionPorts rules = S.unions
        [ expressionPorts
            (headOrErr "AScheduleRelations: action without condition"
                       (aact_args action))
        | rule <- rules, action <- arule_actions rule ]

    argumentUses method@(AIActionValue { aif_value = ADef _ _ expression _,
                                         aif_body = rules }) =
        S.fromList (map fst (aIfaceArgs method)) `S.intersection`
            S.union (actionConditionPorts rules) (expressionPorts expression)
    argumentUses method@(AIAction { aif_body = rules }) =
        S.fromList (map fst (aIfaceArgs method)) `S.intersection`
            actionConditionPorts rules
    argumentUses _ = S.empty
