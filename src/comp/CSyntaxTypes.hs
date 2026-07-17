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
    apSubC s (CValueSign d) = changed1 CValueSign (apSubC s d)
    apSubC s d = Unchanged
    tv (CValueSign d) = tv d
    tv d = []
    tv (CValueSign d) = tv d
    tv d = []

instance Types CDef where
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
    apSubC s (CClause ps qs e) =
        Changed (CClause (map (changedOrId (apSubC s)) ps) (map (changedOrId (apSubC s)) qs) (changedOrId (apSubC s) e))
    tv (CClause ps qs e) = tv (ps, qs, e)

instance Types CRule where
    apSubC s (CRule rps mi qs e) =
        Changed (CRule rps (fmap (changedOrId (apSubC s)) mi) (map (changedOrId (apSubC s)) qs) (changedOrId (apSubC s) e))
    apSubC s (CRuleNest rps mi qs rs) =
        Changed (CRuleNest rps (fmap (changedOrId (apSubC s)) mi) (map (changedOrId (apSubC s)) qs) (map (changedOrId (apSubC s)) rs))
    tv (CRule rps mi qs e) = tv (mi, qs, e)
    tv (CRuleNest rps mi qs rs) = tv (mi, qs, rs)

instance Types CQual where
    apSubC s (CQGen t p e) = Changed (CQGen (changedOrId (apSubC s) t) (changedOrId (apSubC s) p) (changedOrId (apSubC s) e))
    apSubC s (CQFilter e) = changed1 CQFilter (apSubC s e)
    tv (CQGen t p e) = tv (t, p, e)
    tv (CQFilter e) = tv e

instance Types CExpr where
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
    apSubC s (CLValueSign d me) = Changed (CLValueSign (changedOrId (apSubC s) d) (map (changedOrId (apSubC s)) me))
    apSubC s (CLValue i cs me) = Changed (CLValue i (map (changedOrId (apSubC s)) cs) (map (changedOrId (apSubC s)) me))
    apSubC s (CLMatch p e) = Changed (CLMatch (changedOrId (apSubC s) p) (changedOrId (apSubC s) e))
    tv (CLValueSign d me) = tv (d, me)
    tv (CLValue i cs me) = tv (cs, me)
    tv (CLMatch p e) = tv (p, e)

instance Types CQType where
    apSubC s (CQType ps t) = Changed (CQType (map (changedOrId (apSubC s)) ps) (changedOrId (apSubC s) t))
    tv (CQType ps t) = tv ps `union` tv t

instance Types CPred where
    apSubC s (CPred c ts) = Changed (CPred c (map (changedOrId (apSubC s)) ts))
    tv (CPred _ ts) = tv ts

instance (Types t) => Types (Maybe t) where
    apSubC s mt = mapMaybeChanged (apSubC s) mt
    tv (Just t) = tv t
    tv Nothing = []

instance (Types a, Types b) => Types (a, b) where
    apSubC s (a, b) = changed2 (,) a b (apSubC s a) (apSubC s b)
    tv (a, b) = tv a `union` tv b

instance (Types a, Types b, Types c) => Types (a, b, c) where
    apSubC s (a, b, c) =
        changed3 (,,) a b c (apSubC s a) (apSubC s b) (apSubC s c)
    tv (a, b, c) = tv a `union` tv b `union` tv c

instance (Types a, Types b, Types c, Types d) => Types (a, b, c, d) where
    apSubC s (a, b, c, d) =
        changed2 (\(a', b') (c', d') -> (a', b', c', d')) (a, b) (c, d)
                 (apSubC s (a, b)) (apSubC s (c, d))
    tv (a, b, c, d) = tv a `union` tv b `union` tv c `union` tv d

instance Types CCaseArm where
    apSubC s arm =
        Changed (CCaseArm { cca_pattern = (changedOrId (apSubC s) (cca_pattern arm)),
                            cca_filters = (map (changedOrId (apSubC s)) (cca_filters arm)),
                            cca_consequent = (changedOrId (apSubC s) (cca_consequent arm)) })
    tv arm = tv (cca_pattern arm) `union` tv (cca_filters arm) `union`
             tv (cca_consequent arm)
