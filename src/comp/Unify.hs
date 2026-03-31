{-# LANGUAGE PatternGuards #-}
module Unify(Unify(..), matchList) where
import Type
import Subst
import CType
import ErrorUtil(internalError)
import Util(fastNub)

-- For tracing
import PFPrint
import Util(traces)
import IOUtil(progArgs)

doRTrace :: Bool
doRTrace = elem "-trace-type" progArgs
rtrace :: String -> a -> a
rtrace s x = if doRTrace then traces s x else x

class Unify t where
    mgu :: [TyVar] {- bound type vars: don't substitute with other tyvars -}
        -- result: list of substitutions and required type equalities
        -> t -> t -> Maybe (Subst, [(Type, Type)])

instance Unify Type where
    -- Reflexivity: identical types always unify without substitution.
    mgu _ t1 t2
        | t1 == t2 = Just (nullSubst, [])

    -- Expand saturated type synonyms before structural matching.
    mgu bound_tyvars t1 t2
        | Just t1' <- expandSatSyn t1 = mgu bound_tyvars t1' t2
        | Just t2' <- expandSatSyn t2 = mgu bound_tyvars t1 t2'

    -- Two distinct type constructors never unify.
    -- (Equal TCons are already caught by reflexivity above.)
    mgu _ (TCon _) (TCon _) = Nothing

    -- TVar/TVar: order by generatedness and index to maintain canonical form.
    -- (u == v is already caught by reflexivity above.)
    mgu bound_tyvars tu@(TVar u) tv@(TVar v)
        -- Both bound: defer the equality regardless of kind.  Callers handle
        -- these two ways:
        --   fundep improvement: the equality is axiomatic (given by the
        --     instance), applied locally to dictionaries without being
        --     accumulated into the global substitution.
        --   unification: becomes an error — "you declared X here, we deduced
        --     Y there" — with the bound variable's position as provenance.
        -- KNum/KStr bound-variable equalities additionally go to the
        -- constraint solver as wanteds; other kinds are resolved structurally
        -- or reported as errors by the caller.
        | u `elem` bound_tyvars
        , v `elem` bound_tyvars
            = Just (nullSubst, [(tu, tv)])
        -- When one var is bound, bind the *unbound* var to point at the bound
        -- one.  This keeps the bound var out of the substitution domain, which
        -- is required by orderInstHead (MakeSymTab): okSubst checks that none
        -- of the "pivot" vars appear in the substitution domain.
        | u `elem` bound_tyvars = varBindWithEqs v tu
        | v `elem` bound_tyvars = varBindWithEqs u tv
        | isGeneratedTVar u
        , isGeneratedTVar v     =
            case compare (tv_num u) (tv_num v) of
              GT -> varBindWithEqs u tv
              LT -> varBindWithEqs v tu
              EQ -> internalError "don't substitute a variable for itself"
        | isGeneratedTVar u     = varBindWithEqs u tv
        | isGeneratedTVar v     = varBindWithEqs v tu
        | otherwise             = varBindWithEqs u tv

    -- TVar/type: unbound variables bind freely.  Bound variables defer the
    -- equality regardless of kind; occurs check still applies.
    -- See TVar/TVar comment above for how callers handle bound-var equalities.
    mgu bound_tyvars tu@(TVar u) t
        | u `notElem` bound_tyvars = varBindWithEqs u t
        | u `elem` tv t            = Nothing    -- occurs check
        | otherwise                = Just (nullSubst, [(tu, t)])
    mgu bound_tyvars t tvu@(TVar u)
        | u `notElem` bound_tyvars = varBindWithEqs u t
        | u `elem` tv t            = Nothing    -- occurs check
        | otherwise                = Just (nullSubst, [(t, tvu)])

    -- Type operator application (ATF or numeric/string op like TAdd/TStrCat):
    -- deferred as wanteds.  KNum/KStr go to the constraint solver; ATFs are
    -- converted to class predicates by tryATFClassPred in eqToPred.
    -- The TVar cases above already handle TVar/typeop uniformly, so here at
    -- least one side is a non-TVar type operator application.
    mgu _ t1 t2
        | isATFAp t1 || isTypeopAp t1 || isATFAp t2 || isTypeopAp t2
        = Just (nullSubst, [(t1, t2)])

    -- Structural: recurse on both sides.  ATF/typeop applications are already
    -- handled above, so this only fires for ordinary type constructor applications.
    mgu bound_tyvars (TAp l r) (TAp l' r') = do
        (s1, eqs1) <- mgu bound_tyvars l l'
        (s2, eqs2) <- mgu bound_tyvars (apSub s1 r) (apSub s1 r')
        Just (s2 @@ s1, fastNub (eqs1 ++ eqs2))

    mgu _ _ _ = Nothing

-- Expand a saturated type synonym one step.  Returns Nothing if the type is
-- not a saturated synonym application.
expandSatSyn :: Type -> Maybe Type
expandSatSyn t =
    let (f, as) = splitTAp t
    in case f of
        TCon (TyCon _ _ (TItype n body))
            | fromIntegral (length as) == n
            -> Just (instTypeSyn as body)
        _ -> Nothing

-- Substitute synonym parameters (encoded as TGen) with the supplied arguments.
instTypeSyn :: [Type] -> Type -> Type
instTypeSyn as (TAp l r)  = TAp (instTypeSyn as l) (instTypeSyn as r)
instTypeSyn as (TGen _ n) = as !! n
instTypeSyn _  t          = t

instance (Types t, Unify t) => Unify [t] where
    mgu bound_tyvars (x:xs) (y:ys) = do
        (s1,eqs1) <- mgu bound_tyvars x y
        (s2,eqs2) <- mgu bound_tyvars (apSub s1 xs) (apSub s1 ys)
        return (s2 @@ s1, fastNub (eqs1 ++ eqs2))
    mgu bound_tyvars []     []     = return (nullSubst, [])
    mgu bound_tyvars _      _      = Nothing

varBindWithEqs :: TyVar -> Type -> Maybe (Subst, [(Type, Type)])
varBindWithEqs u t = fmap no_eqs $ varBind u t
 where no_eqs s = (s,[])

varBind :: TyVar -> Type -> Maybe Subst
varBind u t | t == TVar u      = Just nullSubst
            | isUnSatSyn t     = Nothing
            | u `elem` tv t    = Nothing
            | kind u == kind t = Just (u +-> t)
            | otherwise        = Nothing

-- Cannot allow a variable to be bound to an unsaturated type synonym.
isUnSatSyn :: Type -> Bool
isUnSatSyn t = isUnSatSyn' t 0
isUnSatSyn' :: Type -> Integer -> Bool
isUnSatSyn' (TCon (TyCon _ _ (TItype n _))) args = n > args
isUnSatSyn' (TAp f a) args = isUnSatSyn' f (args + 1)
isUnSatSyn' _  _ = False

match :: Type -> Type -> Maybe Subst
match (TAp l r) (TAp l' r') = rtrace ("match: TAp: " ++ ppReadable (l,r)) $ do
    sl <- match l l'
    sr <- match r r'
    rtrace ("match: TAp result:  " ++ ppReadable (merge sl sr, sl, sr)) $ merge sl sr
match (TVar u1)  (TVar u2)  | u1 == u2         =
   rtrace ("match: Var, Var: " ++ ppReadable (u1, u2))  $ Just nullSubst
match (TVar u)   t          | kind u == kind t =
   rtrace ("match: Var, oth: " ++ ppReadable (u,t))     $ Just (u +-> t)
match (TCon tc1) (TCon tc2) | tc1 == tc2       =
   rtrace ("match: Con, Con: " ++ ppReadable (tc1,tc2)) $ Just nullSubst
match t1         t2                            =
   rtrace ("match: Nothing: " ++ ppReadable (t1,t2))    $ Nothing

matchList :: [Type] -> [Type] -> Maybe Subst
matchList ts ts' = mergeListWith merge (zipWith match ts ts')
