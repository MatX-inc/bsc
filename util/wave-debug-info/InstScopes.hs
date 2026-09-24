-- The source-level view of a module's contents, from its instance tree
module InstScopes(InstScope(..), instScopes, scopeLocalName) where

import Data.List(sortBy, isPrefixOf)
import qualified Data.Map as M

import Position(getPosition, isPreludePosition)
import Id
import CType(CType)
import InstNodes(InstNode(..), InstTree, nodeChildren, isHiddenKP, isHiddenAll)

-- A module's contents as the source arranges them: the module itself
-- and each inlined module instance (or loop body) is a scope holding,
-- under their source names, the state elements and rules instantiated
-- directly in it, and the instances nested in it.  An instance of a
-- library module whose only content is one state element (an mkDReg,
-- say) is that state element, not a scope around it.
data InstScope = InstScope {
    is_name :: Id,                -- the instance; its position is the instantiation
    is_type :: Maybe CType,       -- the instance's interface
    is_states :: [(Id, Id)],      -- source name, flattened instance name
    is_rules :: [(Id, Id)],       -- source name, rule
    is_children :: [InstScope]
  }

-- The name a scope, state or rule displays: its display name when
-- the elaborator recorded one (loop bodies), else its own
scopeLocalName :: Id -> String
scopeLocalName = getIdBaseString . addIdDisplayName

-- The scopes of the module `top`, from its instance tree; `hide` leaves
-- out the {-# hide #-}'d instances
instScopes :: Bool -> Id -> InstTree -> InstScope
instScopes hide top tree = scopeOf top Nothing (M.elems tree)
  where
    scopeOf name ty nodes =
        let (ss, rs, cs) = partitionItems (childItems (sortBy cmpNode nodes))
        in  InstScope name ty ss rs cs

    -- The items of a scope's children.  A child whose name the
    -- flattened names leave out too (an underscore-prefixed binder in
    -- library code, such as a list built by recursion) contributes its
    -- own items directly, unless their names would clash with the
    -- other children's; then it stays a scope, so that the elements
    -- of a vector of like instances keep apart.
    childItems :: [InstNode] -> [Item]
    childItems nodes =
        let alts = [ (itemsOf n, if unnamed n then Just (kept n) else Nothing) | n <- nodes ]
            counts = M.fromListWith (+) [ (itemName it, 1 :: Int) | (its, _) <- alts, it <- its ]
            clashes its = any (\ it -> M.findWithDefault 0 (itemName it) counts > 1) its
            choose (its, Just alt) | clashes its = alt
            choose (its, _) = its
        in  concatMap choose alts

    itemsOf :: InstNode -> [Item]
    itemsOf node =
        case node of
          Loc {}
            | hide && isHiddenAll node -> []
            | otherwise ->
                -- a state element or rule sits under a Loc carrying its
                -- source name (a library module's hidden wrapper in
                -- between is looked through); any other Loc is an
                -- inlined instance, unless the elaborator marked it as
                -- adding nothing to the path or its name is left out
                -- (see childItems)
                case nodeChildren hide node of
                  [StateVar { node_name = flat }] -> [ScopeState (node_name node) flat]
                  [Rule { node_name = rule }] -> [ScopeRule (node_name node) rule]
                  children
                    | node_ignore node || (hide && isHiddenKP node) || unnamed node ->
                        childItems (sortBy cmpNode children)
                    | otherwise -> kept node
          StateVar { node_name = flat } -> [ScopeState flat flat]
          Rule { node_name = rule } -> [ScopeRule rule rule]

    -- the Loc as a scope of its own
    kept node = [fold (scopeOf (node_name node) (node_type node) (nodeChildren hide node))]

    itemName (ScopeState n _) = scopeLocalName n
    itemName (ScopeRule n _) = scopeLocalName n
    itemName (ScopeChild sc) = scopeLocalName (is_name sc)

    -- a library module instance holding one state element is that
    -- state element, under the instance's name
    fold s@(InstScope { is_states = [(inner, flat)], is_rules = [], is_children = [] })
      | isPreludePosition (getPosition inner) = ScopeState (is_name s) flat
    fold s = ScopeChild s

    unnamed node@(Loc {}) = "_" `isPrefixOf` scopeLocalName (node_name node)
    unnamed _ = False

    partitionItems items =
        ( [ (n, f) | ScopeState n f <- items ]
        , [ (n, r) | ScopeRule n r <- items ]
        , [ s | ScopeChild s <- items ] )

    cmpNode a b = cmpIdByName (node_name a) (node_name b)

data Item = ScopeState Id Id | ScopeRule Id Id | ScopeChild InstScope
