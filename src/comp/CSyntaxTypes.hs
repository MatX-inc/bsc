module CSyntaxTypes(
                    Types(..)
                    ) where

import Data.List(union, (\\), nub)
import Util(mapSnd)
import PPrint(ppReadable)
import ErrorUtil(internalError)
import Subst
import Changed
import CSyntax

-- NB the instances below are deliberately LAZY (always Changed, with
-- per-child sharing via changedOrId . apSubC): CSyntax trees are large
-- and only partially demanded by the typechecker, so eager whole-node
-- changed-detection (mapChanged/changedN) forces far more than is ever
-- consumed -- measured +40% typecheck on a register-heavy module.
-- Leaf constructors still return Unchanged, and Type-level detection
-- (Subst.hs) stays eager where nodes are small.

--import Debug.Trace


instance Types CDefn where
    apSubO s (CValueSign d) = CValueSign (apSubO s d)
    apSubO s d = d
    apSubC s (CValueSign d) = changed1 CValueSign (apSubC s d)
    apSubC s d = Unchanged
    tv (CValueSign d) = tv d
    tv d = []

instance Types CDef where
    apSubO s (CDef i qt cs) = CDef i (apSubO s qt) (apSubO s cs)
    -- This line has been here since revision 1 with the comment "XXX vs ?"
    --apSubO s (CDefT i vs qt cs) = CDefT i vs (apSubO s qt) (apSubO s cs)
    -- The "vs" are the lambda-bound type variables in this definition.
    -- Thus, substituting for or to them is an error.  (Unless the caller
    -- knows what he's doing?)
    -- Below, we remove these variables from the substitution.
    -- This fixes bug 675, but is it fixing the problem or just masking it?
    -- More thought/investigation is needed.
    apSubO s (CDefT i vs qt cs) =
        let s' = if null vs then s else trimSubstByVars vs s
        in  CDefT i vs (apSubO s' qt) (apSubO s' cs)
{-
    -- For investigating, use this code to assert an internalError or trace
    -- on bad substitutions.
        let (s',removed_vs) = removeFromSubst vs s
            r = getSubstRange s'
        in
            --if (any (\v -> elem v r) removed_vs)
            if (any (\v -> elem v r) vs)
            then internalError ("apSubO CDefT:\n" ++
                                " i = " ++ ppReadable i ++
                                " vs = " ++ ppReadable vs ++
                                " removed_vs = " ++ ppReadable removed_vs ++
                                " s' = " ++ ppReadable s')
            else
            if (length removed_vs > 0)
            then trace ("apSubO CDefT, removing from Subst:\n" ++
                        " i = " ++ ppReadable i ++
                        " vs = " ++ ppReadable vs ++
                        " removed_vs = " ++ ppReadable removed_vs ++
                        " s = " ++ ppReadable s) $
                 CDefT i vs (apSubO s' qt) (apSubO s' cs)
            else CDefT i vs (apSubO s' qt) (apSubO s' cs)
-}
    apSubC s (CDef i qt cs) = Changed (CDef i (changedOrId (apSubC s) qt) (map (changedOrId (apSubC s)) cs))
    apSubC s (CDefT i vs qt cs) =
        let s' = if null vs then s else trimSubstByVars vs s
        in  if isNullSubst s'
            then Unchanged
            else Changed (CDefT i vs (changedOrId (apSubC s') qt)
                              (map (changedOrId (apSubC s')) cs))
    tv (CDef i qt cs) = tv (qt, cs)
    tv (CDefT i vs qt cs) = (nub (tv (qt, cs))) \\ vs

instance Types CClause where
    apSubO s (CClause ps qs e) = CClause (apSubO s ps) (apSubO s qs) (apSubO s e)
    apSubC s (CClause ps qs e) =
        Changed (CClause (map (changedOrId (apSubC s)) ps) (map (changedOrId (apSubC s)) qs) (changedOrId (apSubC s) e))
    tv (CClause ps qs e) = tv (ps, qs, e)

instance Types CRule where
    apSubO s (CRule rps mi qs e) = CRule rps (apSubO s mi) (apSubO s qs) (apSubO s e)
    apSubO s (CRuleNest rps mi qs rs) = CRuleNest rps (apSubO s mi) (apSubO s qs) (apSubO s rs)
    apSubC s (CRule rps mi qs e) =
        Changed (CRule rps (fmap (changedOrId (apSubC s)) mi) (map (changedOrId (apSubC s)) qs) (changedOrId (apSubC s) e))
    apSubC s (CRuleNest rps mi qs rs) =
        Changed (CRuleNest rps (fmap (changedOrId (apSubC s)) mi) (map (changedOrId (apSubC s)) qs) (map (changedOrId (apSubC s)) rs))
    tv (CRule rps mi qs e) = tv (mi, qs, e)
    tv (CRuleNest rps mi qs rs) = tv (mi, qs, rs)

instance Types CQual where
    apSubO s (CQGen t p e) = CQGen (apSubO s t) (apSubO s p) (apSubO s e)
    apSubO s (CQFilter e) = CQFilter (apSubO s e)
    apSubC s (CQGen t p e) = Changed (CQGen (changedOrId (apSubC s) t) (changedOrId (apSubC s) p) (changedOrId (apSubC s) e))
    apSubC s (CQFilter e) = changed1 CQFilter (apSubC s e)
    tv (CQGen t p e) = tv (t, p, e)
    tv (CQFilter e) = tv e

instance Types CExpr where
    apSubO s (CLam i e) = CLam i (apSubO s e)
    apSubO s (CLamT i t e) = CLamT i (apSubO s t) (apSubO s e)
    apSubO s (Cletrec ds e) = Cletrec (apSubO s ds) (apSubO s e)
    apSubO s (Cletseq ds e) = Cletseq (apSubO s ds) (apSubO s e)
    apSubO s (CSelect e i) = CSelect (apSubO s e) i
    apSubO s (CSelectTT ti e i) = CSelectTT ti (apSubO s e) i
    apSubO s (CCon i es) = CCon i (apSubO s es)
    apSubO s (Ccase pos e as) = Ccase pos (apSubO s e) (apSubO s as)
    apSubO s (CStruct mb ti fs) = CStruct mb ti (mapSnd (apSubO s) fs)
    apSubO s (CStructUpd e fs) = CStructUpd (apSubO s e) (mapSnd (apSubO s) fs)
    apSubO s (Cwrite pos e v) = Cwrite pos (apSubO s e) (apSubO s v)
    apSubO s e@(CAny {}) = e
    apSubO s e@(CVar _) = e
    apSubO s (CApply f es) = CApply (apSubO s f) (apSubO s es)
    apSubO s (CTaskApply f es) = CTaskApply (apSubO s f) (apSubO s es)
    apSubO s (CTaskApplyT f t es) = CTaskApplyT (apSubO s f) (apSubO s t) (apSubO s es)
    apSubO s e@(CLit _) = e
    apSubO s (CBinOp e1 o e2) = CBinOp (apSubO s e1) o (apSubO s e2)
    apSubO s (CHasType e t) = CHasType (apSubO s e) (apSubO s t)
    apSubO s (Cif pos e1 e2 e3) = Cif pos (apSubO s e1) (apSubO s e2) (apSubO s e3)
    apSubO s (CSub pos e1 e2) = CSub pos (apSubO s e1) (apSubO s e2)
    apSubO s (CSub2 e1 e2 e3) = CSub2 (apSubO s e1) (apSubO s e2) (apSubO s e3)
    apSubO s (CSubUpdate pos e_vec (e_h, e_l) e_rhs) =
        CSubUpdate pos (apSubO s e_vec) (apSubO s e_h, apSubO s e_l) (apSubO s e_rhs)
    apSubO s (Cmodule pos is) = Cmodule pos (apSubO s is)
    apSubO s (Cinterface pos mi ds) = Cinterface pos mi (apSubO s ds)
    apSubO s (CmoduleVerilog m ui c r ses fs sch ps) = CmoduleVerilog (apSubO s m) ui c r (mapSnd (apSubO s) ses) fs sch ps
    apSubO s (CForeignFuncC i wty) = CForeignFuncC i (apSubO s wty)
    apSubO s (Cdo r ss) = Cdo r (apSubO s ss)
    apSubO s (Caction pos ss) = Caction pos (apSubO s ss)
    apSubO s (Crules ps rs) = Crules ps (apSubO s rs)
    apSubO s (CTApply e ts) = CTApply (apSubO s e) (apSubO s ts)
    apSubO s (CSelectT ti i) = CSelectT ti i
    apSubO s (CStructT t fs) = CStructT (apSubO s t) (mapSnd (apSubO s) fs)
    apSubO s (CCon1 ti i e) = CCon1 ti i (apSubO s e)
    apSubO s (CConT t i es) = CConT t i (apSubO s es)
    apSubO s (CLitT t l) = CLitT (apSubO s t) l
    apSubO s (CAnyT pos uk t) = CAnyT pos uk (apSubO s t)
    apSubO s (CmoduleVerilogT t m ui c r ses fs sch ps) =
        CmoduleVerilogT (apSubO s t) (apSubO s m) ui c r (mapSnd (apSubO s) ses) fs sch ps
    apSubO s (CForeignFuncCT i pty) = CForeignFuncCT i (apSubO s pty)
    apSubO s (COper os) = internalError ("CSyntaxTypes.Types(CExpr).apSub: COper " ++ ppReadable os)
    apSubO s e@(Cattributes pps) = e
    apSubO s e = internalError ("CSyntaxTypes.Types(CExpr).apSub: " ++ ppReadable e)

    apSubC s (CLam i e) = changed1 (CLam i) (apSubC s e)
    apSubC s (CLamT i t e) = Changed (CLamT i (changedOrId (apSubC s) t) (changedOrId (apSubC s) e))
    apSubC s (Cletrec ds e) = Changed (Cletrec (map (changedOrId (apSubC s)) ds) (changedOrId (apSubC s) e))
    apSubC s (Cletseq ds e) = Changed (Cletseq (map (changedOrId (apSubC s)) ds) (changedOrId (apSubC s) e))
    apSubC s (CSelect e i) = changed1 (\e' -> CSelect e' i) (apSubC s e)
    apSubC s (CSelectTT ti e i) =
        changed1 (\e' -> CSelectTT ti e' i) (apSubC s e)
    apSubC s (CCon i es) = Changed (CCon i (map (changedOrId (apSubC s)) es))
    apSubC s (Ccase pos e as) = Changed (Ccase pos (changedOrId (apSubC s) e) (map (changedOrId (apSubC s)) as))
    apSubC s (CStruct mb ti fs) = Changed (CStruct mb ti (mapSnd (changedOrId (apSubC s)) fs))
    apSubC s (CStructUpd e fs) = Changed (CStructUpd (changedOrId (apSubC s) e) (mapSnd (changedOrId (apSubC s)) fs))
    apSubC s (Cwrite pos e v) = Changed (Cwrite pos (changedOrId (apSubC s) e) (changedOrId (apSubC s) v))
    apSubC s (CAny {}) = Unchanged
    apSubC s (CVar _) = Unchanged
    apSubC s (CApply f es) = Changed (CApply (changedOrId (apSubC s) f) (map (changedOrId (apSubC s)) es))
    apSubC s (CTaskApply f es) = Changed (CTaskApply (changedOrId (apSubC s) f) (map (changedOrId (apSubC s)) es))
    apSubC s (CTaskApplyT f t es) =
        Changed (CTaskApplyT (changedOrId (apSubC s) f) (changedOrId (apSubC s) t) (map (changedOrId (apSubC s)) es))
    apSubC s (CLit _) = Unchanged
    apSubC s (CBinOp e1 o e2) = Changed (CBinOp (changedOrId (apSubC s) e1) o (changedOrId (apSubC s) e2))
    apSubC s (CHasType e t) = Changed (CHasType (changedOrId (apSubC s) e) (changedOrId (apSubC s) t))
    apSubC s (Cif pos e1 e2 e3) =
        Changed (Cif pos (changedOrId (apSubC s) e1) (changedOrId (apSubC s) e2) (changedOrId (apSubC s) e3))
    apSubC s (CSub pos e1 e2) = Changed (CSub pos (changedOrId (apSubC s) e1) (changedOrId (apSubC s) e2))
    apSubC s (CSub2 e1 e2 e3) =
        Changed (CSub2 (changedOrId (apSubC s) e1) (changedOrId (apSubC s) e2) (changedOrId (apSubC s) e3))
    apSubC s (CSubUpdate pos e_vec (e_h, e_l) e_rhs) =
        Changed (CSubUpdate pos (changedOrId (apSubC s) e_vec) ((changedOrId (apSubC s) e_h), (changedOrId (apSubC s) e_l)) (changedOrId (apSubC s) e_rhs))
    apSubC s (Cmodule pos is) = Changed (Cmodule pos (map (changedOrId (apSubC s)) is))
    apSubC s (Cinterface pos mi ds) = Changed (Cinterface pos mi (map (changedOrId (apSubC s)) ds))
    apSubC s (CmoduleVerilog m ui c r ses fs sch ps) =
        Changed (CmoduleVerilog (changedOrId (apSubC s) m) ui c r (mapSnd (changedOrId (apSubC s)) ses) fs sch ps)
    apSubC s (CForeignFuncC i wty) = changed1 (CForeignFuncC i) (apSubC s wty)
    apSubC s (Cdo r ss) = Changed (Cdo r (map (changedOrId (apSubC s)) ss))
    apSubC s (Caction pos ss) = Changed (Caction pos (map (changedOrId (apSubC s)) ss))
    apSubC s (Crules ps rs) = Changed (Crules ps (map (changedOrId (apSubC s)) rs))
    apSubC s (CTApply e ts) = Changed (CTApply (changedOrId (apSubC s) e) (map (changedOrId (apSubC s)) ts))
    apSubC s (CSelectT ti i) = Unchanged
    apSubC s (CStructT t fs) = Changed (CStructT (changedOrId (apSubC s) t) (mapSnd (changedOrId (apSubC s)) fs))
    apSubC s (CCon1 ti i e) = changed1 (CCon1 ti i) (apSubC s e)
    apSubC s (CConT t i es) = Changed (CConT t i (map (changedOrId (apSubC s)) es))
    apSubC s (CLitT t l) = changed1 (\t' -> CLitT t' l) (apSubC s t)
    apSubC s (CAnyT pos uk t) = changed1 (CAnyT pos uk) (apSubC s t)
    apSubC s (CmoduleVerilogT t m ui c r ses fs sch ps) =
        Changed (CmoduleVerilogT (changedOrId (apSubC s) t) (changedOrId (apSubC s) m) ui c r (mapSnd (changedOrId (apSubC s)) ses) fs sch ps)
    apSubC s (CForeignFuncCT i pty) =
        changed1 (CForeignFuncCT i) (apSubC s pty)
    apSubC s (COper os) = internalError ("CSyntaxTypes.Types(CExpr).apSub: COper " ++ ppReadable os)
    apSubC s e@(Cattributes pps) = Unchanged
    apSubC s e = internalError ("CSyntaxTypes.Types(CExpr).apSub: " ++ ppReadable e)
    tv (CLam i e) = tv e
    tv (CLamT i t e) = tv (t, e)
    tv (Cletrec ds e) = tv (ds, e)
    tv (Cletseq ds e) = tv (ds, e)
    tv (CSelect e i) = tv e
    tv (CSelectTT ti e i) = tv e
    tv (CCon i es) = tv es
    tv (Ccase pos e as) = tv (e, as)
    tv (CStruct _ _ fs) = tv (map snd fs)
    tv (CStructUpd e fs) = tv (e, map snd fs)
    tv (Cwrite pos e v) = tv (e,v)
    tv e@(CAny {}) = []
    tv e@(CVar _) = []
    tv (CApply f es) = tv (f, es)
    tv (CTaskApply f es) = tv (f, es)
    tv (CTaskApplyT f t es) = tv (f, t, es)
    tv e@(CLit _) = []
    tv (CBinOp e1 o e2) = tv (e1, e2)
    tv (CHasType e t) = tv (e, t)
    tv (Cif pos e1 e2 e3) = tv (e1, e2, e3)
    tv (CSub pos e1 e2) = tv (e1, e2)
    tv (CSub2 e1 e2 e3) = tv (e1, e2, e3)
    tv (CSubUpdate pos e_vec (e_h, e_l) e_rhs) = tv [e_vec, e_h, e_l, e_rhs]
    tv (Cmodule pos is) = tv is
    tv (Cinterface pos mi ds) = tv ds
    tv (CmoduleVerilog m ui c r ses fs sch ps) = tv (m, map snd ses)
    tv (CForeignFuncC i wty) = tv wty
    tv (Cdo r ss) = tv ss
    tv (Caction pos ss) = tv ss
    tv (Crules ps rs) = tv rs
    tv (CTApply e ts) = tv (e, ts)
    tv (CSelectT ti i) = []
    tv (CStructT t fs) = tv (t, map snd fs)
    tv (CCon1 ti i e) = tv e
    tv (CConT t i es) = tv es
    tv (CLitT t l) = tv t
    tv e@(CAnyT _ _ t) = tv t
    tv (CmoduleVerilogT t m ui c r ses fs sch ps) = tv (t, m, map snd ses)
    tv (CForeignFuncCT i pty) = tv pty
    tv (COper os) = internalError ("CSyntaxTypes.Types(CExpr).apSub: COper " ++ ppReadable os)
    tv e@(Cattributes pps) = []
    tv e = internalError ("CSyntaxTypes.Types(CExpr).tv: " ++ ppReadable e)

instance Types CStmt where
    apSubO s (CSBindT p name pprops t e) = CSBindT (apSubO s p) name pprops (apSubO s t) (apSubO s e)
    apSubO s (CSBind p name pprops e) = CSBind (apSubO s p) name pprops (apSubO s e)
    apSubO s (CSletrec ds) = CSletrec (apSubO s ds)
    apSubO s (CSletseq ds) = CSletseq (apSubO s ds)
    apSubO s (CSExpr name e) = CSExpr name (apSubO s e)
    apSubC s (CSBindT p name pprops t e) =
        Changed (CSBindT (changedOrId (apSubC s) p) name pprops (changedOrId (apSubC s) t) (changedOrId (apSubC s) e))
    apSubC s (CSBind p name pprops e) =
        Changed (CSBind (changedOrId (apSubC s) p) name pprops (changedOrId (apSubC s) e))
    apSubC s (CSletrec ds) = Changed (CSletrec (map (changedOrId (apSubC s)) ds))
    apSubC s (CSletseq ds) = Changed (CSletseq (map (changedOrId (apSubC s)) ds))
    apSubC s (CSExpr name e) = changed1 (CSExpr name) (apSubC s e)
    tv (CSBindT p _ _ t e) = tv (p, t, e)
    tv (CSBind p _ _ e) = tv (p, e)
    tv (CSletrec ds) = tv ds
    tv (CSletseq ds) = tv ds
    tv (CSExpr _ e) = tv e


instance Types CMStmt where
    apSubO s (CMStmt t) = CMStmt (apSubO s t)
    apSubO s (CMrules e) = CMrules (apSubO s e)
    apSubO s (CMinterface e) = CMinterface (apSubO s e)
    apSubO s (CMTupleInterface pos es) = CMTupleInterface pos (apSubO s es)
    apSubC s (CMStmt t) = changed1 CMStmt (apSubC s t)
    apSubC s (CMrules e) = changed1 CMrules (apSubC s e)
    apSubC s (CMinterface e) = changed1 CMinterface (apSubC s e)
    apSubC s (CMTupleInterface pos es) =
        Changed (CMTupleInterface pos (map (changedOrId (apSubC s)) es))
    tv (CMStmt t) = tv t
    tv (CMrules e) = tv e
    tv (CMinterface e) = tv e
    tv (CMTupleInterface pos es) = tv es

instance Types CPat where
    apSubO s (CPCon c ps) = CPCon c (apSubO s ps)
    apSubO s (CPstruct mb c fs) = CPstruct mb c (mapSnd (apSubO s) fs)
    apSubO s p@(CPVar i) = p
    apSubO s (CPAs i p) = CPAs i (apSubO s p)
    apSubO s p@(CPAny {}) = p
    apSubO s p@(CPLit l) = p
    apSubO s p@(CPMixedLit {}) = p
    apSubO s (CPCon1 ti c p) = CPCon1 ti c (apSubO s p)
    apSubO s (CPConTs ti c ts ps) = CPConTs ti c (apSubO s ts) (apSubO s ps)
    apSubO s (CPOper os) = internalError ("CSyntaxTypes.Types(CPat).apSub: CPOper " ++ ppReadable os)
    apSubC s (CPCon c ps) = Changed (CPCon c (map (changedOrId (apSubC s)) ps))
    apSubC s (CPstruct mb c fs) = Changed (CPstruct mb c (mapSnd (changedOrId (apSubC s)) fs))
    apSubC s (CPVar i) = Unchanged
    apSubC s (CPAs i p) = changed1 (CPAs i) (apSubC s p)
    apSubC s (CPAny {}) = Unchanged
    apSubC s (CPLit l) = Unchanged
    apSubC s (CPMixedLit {}) = Unchanged
    apSubC s (CPCon1 ti c p) = changed1 (CPCon1 ti c) (apSubC s p)
    apSubC s (CPConTs ti c ts ps) =
        Changed (CPConTs ti c (map (changedOrId (apSubC s)) ts) (map (changedOrId (apSubC s)) ps))
    apSubC s (CPOper os) = internalError ("CSyntaxTypes.Types(CPat).apSub: CPOper " ++ ppReadable os)
    tv (CPCon c ps) = tv ps
    tv (CPstruct _ _ fs) = tv (map snd fs)
    tv (CPVar p) = []
    tv (CPAs i p) = tv p
    tv (CPAny {}) = []
    tv (CPLit l) = []
    tv (CPMixedLit {}) = []
    tv (CPCon1 ti c p) = tv p
    tv (CPConTs ti c ts ps) = tv (ts, ps)
    tv (CPOper os) = internalError ("CSyntaxTypes.Types(CPat).tv: CPOper " ++ ppReadable os)

instance Types CDefl where
    apSubO s (CLValueSign d me) = CLValueSign (apSubO s d) (apSubO s me)
    apSubO s (CLValue i cs me) = CLValue i (apSubO s cs) (apSubO s me)
    apSubO s (CLMatch p e) = CLMatch (apSubO s p) (apSubO s e)
    apSubC s (CLValueSign d me) = Changed (CLValueSign (changedOrId (apSubC s) d) (map (changedOrId (apSubC s)) me))
    apSubC s (CLValue i cs me) = Changed (CLValue i (map (changedOrId (apSubC s)) cs) (map (changedOrId (apSubC s)) me))
    apSubC s (CLMatch p e) = Changed (CLMatch (changedOrId (apSubC s) p) (changedOrId (apSubC s) e))
    tv (CLValueSign d me) = tv (d, me)
    tv (CLValue i cs me) = tv (cs, me)
    tv (CLMatch p e) = tv (p, e)

instance Types CQType where
    apSubO s (CQType ps t) = CQType (apSubO s ps) (apSubO s t)
    apSubC s (CQType ps t) = Changed (CQType (map (changedOrId (apSubC s)) ps) (changedOrId (apSubC s) t))
    tv (CQType ps t) = tv ps `union` tv t

instance Types CPred where
    apSubO s (CPred c ts) = CPred c (apSubO s ts)
    apSubC s (CPred c ts) = Changed (CPred c (map (changedOrId (apSubC s)) ts))
    tv (CPred _ ts) = tv ts

instance (Types t) => Types (Maybe t) where
    apSubO s (Just t) = Just (apSubO s t)
    apSubO s Nothing = Nothing
    apSubC s mt = mapMaybeChanged (apSubC s) mt
    tv (Just t) = tv t
    tv Nothing = []

instance (Types a, Types b) => Types (a, b) where
    apSubO s (a, b) = (apSubO s a, apSubO s b)
    apSubC s (a, b) = changed2 (,) a b (apSubC s a) (apSubC s b)
    tv (a, b) = tv a `union` tv b

instance (Types a, Types b, Types c) => Types (a, b, c) where
    apSubO s (a, b, c) = (apSubO s a, apSubO s b, apSubO s c)
    apSubC s (a, b, c) =
        changed3 (,,) a b c (apSubC s a) (apSubC s b) (apSubC s c)
    tv (a, b, c) = tv a `union` tv b `union` tv c

instance (Types a, Types b, Types c, Types d) => Types (a, b, c, d) where
    apSubO s (a, b, c, d) = (apSubO s a, apSubO s b, apSubO s c, apSubO s d)
    apSubC s (a, b, c, d) =
        changed2 (\(a', b') (c', d') -> (a', b', c', d')) (a, b) (c, d)
                 (apSubC s (a, b)) (apSubC s (c, d))
    tv (a, b, c, d) = tv a `union` tv b `union` tv c `union` tv d

instance Types CCaseArm where
    apSubO subst arm =
        CCaseArm { cca_pattern = apSubO subst (cca_pattern arm),
                   cca_filters = apSubO subst (cca_filters arm),
                   cca_consequent = apSubO subst (cca_consequent arm) }
    apSubC s arm =
        Changed (CCaseArm { cca_pattern = (changedOrId (apSubC s) (cca_pattern arm)),
                            cca_filters = (map (changedOrId (apSubC s)) (cca_filters arm)),
                            cca_consequent = (changedOrId (apSubC s) (cca_consequent arm)) })
    tv arm = tv (cca_pattern arm) `union` tv (cca_filters arm) `union`
             tv (cca_consequent arm)
