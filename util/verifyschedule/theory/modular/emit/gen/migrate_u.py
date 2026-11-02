"""The skipped-value API (ParentU, CertU, OblU) for an emitted Em<m> (the statement piece's migrate.py)."""
import re


def migrate(s, mm):
    s = s.replace('(u : Nat → Nat → V)', '(u : St → Nat → V)')
    s = s.replace('(u 0)', '(u (.inl ()))').replace('(u 1)', '(u (.inr .go))').replace('(u 2)', '(u (.inr .back))')
    s = re.sub(r'\bu 0 (\d+)', r'u (.inl ()) \1', s)
    s = re.sub(r'\bu 1 (\d+)', r'u (.inr .go) \1', s)
    s = re.sub(r'\bu 2 (\d+)', r'u (.inr .back) \1', s)
    old_p = s[s.index('abbrev P (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Parent V where'):s.index('/-! ## Each step\'s trace')]
    new_p = old_p.replace('abbrev P (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Parent V where',
                          'abbrev F (ops : Ops V) (E : EqC V) : ParentU V where')
    new_p = new_p.replace('body := fun _ _ => runSites (u (.inl ())) (resPeek ops) (sitesF (sitePeek ops E) kPeek) 0 (u (.inl ()))',
                          'body := fun _ _ v => runSites v (resPeek ops) (sitesF (sitePeek ops E) kPeek) 0 v')
    new_p = new_p.replace('rbody := fun r => match r with', 'rbody := fun r v => match r with')
    new_p = new_p.replace('runSites (u (.inr .go)) (fun _ => some ops.z) (sitesF (siteGo ops E) kGo) 0 (u (.inr .go))',
                          'runSites v (fun _ => some ops.z) (sitesF (siteGo ops E) kGo) 0 v')
    new_p = new_p.replace('runSites (u (.inr .back)) (fun _ => some ops.z) (sitesF (siteBack ops E) kBack) 0 (u (.inr .back))',
                          'runSites v (fun _ => some ops.z) (sitesF (siteBack ops E) kBack) 0 v')
    new_p += '/-- The family at skipped values `u`: step `s` sees `u s` only. -/\nabbrev P (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Parent V := (F ops E).at u\n\n'
    s = s.replace(old_p, new_p)
    # acyc reads no program
    s = s.replace('def acyc (ops : Ops V) (E : EqC V) (u : St → Nat → V) : Acyc (P ops E u) where',
                  'def acyc (ops : Ops V) (E : EqC V) : Acyc (F ops E).base where')
    s = s.replace('dep_of_all (P ops E u)', 'dep_of_all (F ops E).base')

    # per-site "reads only always-taken sites" tables, from the existing SFReads trees
    def tree(name):
        a = s.index('theorem rb%s ' % name)
        b = s.index('theorem rb', a + 10) if s.find('theorem rb', a + 10) != -1 else s.index('theorem hOKGo')
        b = min(b, s.index('theorem hOKGo'))
        return s[a:b]
    extra = ["""/-! ## Each step's calls at every skipped value (`CertU.trU`): every site is safe -/

    /-- Site `i` with condition and argument `c` is safe: equal earlier conditions, and equal earlier
        values where taken, give an equal condition and, where taken, an equal argument. -/
    def SafeSF (site : Nat → Site I (fun j => (CI j).M) V) (i : Nat) (c : SF V) : Prop :=
      ∀ ρ1 ρ2 : Nat → V, (∀ j, j < i → (site j).cond ρ1 = (site j).cond ρ2 ∧
        ((site j).cond ρ1 = true → ρ1 (0 + j) = ρ2 (0 + j))) →
        c.1 ρ1 = c.1 ρ2 ∧ (c.1 ρ1 = true → c.2 ρ1 = c.2 ρ2)
    """]
    leaf_re = re.compile(r"\.leaf \((rdSF|fun V \(ops : Ops V\) \(E : EqC V\) => \(\(fun ρ => (.+?), (fun _ => ops\.z|fun ρ => (\S+) \(ρ (\d+)\))\) : SF V\))\)")
    for name, k in [('Go', 'kGo'), ('Back', 'kBack'), ('Peek', 'kPeek')]:
        a0 = s.index('def sf%s : Tab' % name)
        a1 = s.index('\n\n', a0)
        nk_ = int(re.search(r'abbrev %s : Nat := (\d+)' % k, s).group(1))
        leaves = leaf_re.findall(s[a0:])[:nk_]
        conds = {}
        for i, lf in enumerate(leaves):
            conds[i] = None if lf[0] == 'rdSF' else lf[1]
        leafp = []
        for i, lf in enumerate(leaves):
            if lf[0] == 'rdSF':
                leafp.append('fun _ _ _ => ⟨rfl, fun _ => rfl⟩')
                continue
            cond = lf[1]
            m = re.match(r'E\.eqc \(ρ (\d+)\) (\d+)$', cond)
            if m:
                f, ca = 'fun x => E.eqc x %s' % m.group(2), int(m.group(1))
            else:
                m = re.match(r'(\S+) \(ρ (\d+)\)$', cond)
                f, ca = m.group(1), int(m.group(2))
            assert conds[ca] is None, (name, i)
            c_part = 'congrArg (%s) ((hp %d (by decide)).2 rfl)' % (f, ca)
            if lf[2] == 'fun _ => ops.z':
                a_part = 'fun _ => rfl'
            else:
                g, cb = lf[3], int(lf[4])
                if conds[cb] is None:
                    prem = 'rfl'
                else:
                    assert conds[cb] == cond, (name, i, cb)
                    prem = 'hc'
                a_part = 'fun hc => congrArg %s ((hp %d (by decide)).2 %s)' % (g, cb, prem)
            leafp.append('fun ρ1 ρ2 hp => ⟨%s, %s⟩' % (c_part, a_part))
        # the proof tree mirrors the read-fact tree rb<name>, leaf for leaf
        t0 = s.index('theorem rb%s ' % name)
        t1 = s.index(':=', t0) + 2
        t2 = s.index('\ntheorem ', t1)
        tree_s = s[t1:t2].strip()
        it = iter(leafp)
        tree_s = re.sub(r'Tab\.one \(sfr_\w+[^⟩]*?\)(?=[⟩,])', lambda m_: 'Tab.one (%s)' % next(it), tree_s)
        extra.append("""theorem st%s (ops : Ops V) (E : EqC V) : Tab.AllR (fun i F => SafeSF (site%s ops E) i (F V ops E)) sf%s 0 %s :=%s%s
      %s
    theorem safe%s (ops : Ops V) (E : EqC V) : ∀ (i : Nat) (hi : i < (sitesF (site%s ops E) %s).length) (ρ1 ρ2 : Nat → V),
        (∀ k, k < 0 → ρ1 k = ρ2 k) →
        (∀ (j : Nat) (hj : j < i), (sitesF (site%s ops E) %s)[j].cond ρ1 = (sitesF (site%s ops E) %s)[j].cond ρ2 ∧
          ((sitesF (site%s ops E) %s)[j].cond ρ1 = true → ρ1 (0 + j) = ρ2 (0 + j))) →
        (sitesF (site%s ops E) %s)[i].cond ρ1 = (sitesF (site%s ops E) %s)[i].cond ρ2 ∧
          ((sitesF (site%s ops E) %s)[i].cond ρ1 = true → (sitesF (site%s ops E) %s)[i].arg ρ1 = (sitesF (site%s ops E) %s)[i].arg ρ2) := by
      intro i hi ρ1 ρ2 _ hp
      rw [sitesF_get]
      rw [sitesF_length] at hi
      exact Tab.get_allR _ 0 %s (st%s ops E) i (Nat.zero_le _) hi ρ1 ρ2 (fun j hj => by
        have h := hp j hj
        rwa [sitesF_get] at h)
    """ % ((name, name, name, k, '', '', tree_s, name, name, k) + (name, k) * 8 + (k, name)))
        extra.append("""theorem trU%s (ops : Ops V) (E : EqC V) (oc : OC V) (u u' : Nat → V) :
        callsOf (envHw u oc (sitesF (site%s ops E) %s) 0 u) (sitesF (site%s ops E) %s) =
          callsOf (envHw u' oc (sitesF (site%s ops E) %s) 0 u') (sitesF (site%s ops E) %s) :=
      callsOf_envHw_safe u oc u' _ 0 u u' (sitesOK_fn _ %s (hOK%s ops E)) (fun k hk => absurd hk (Nat.not_lt_zero _))
        (safe%s ops E)
    """ % (name, name, k, name, k, name, k, name, k, k, name, name))
    extra_s = '\n'.join(extra) + '\n'
    # certificate fixed before u, the family obligation
    certu = '''
    /-- **The certificate fixed before the skipped values**: one rule list, firing and hits. -/
    def certU (ops : Ops V) (E : EqC V) : CertU (F ops E) where
      rules := [.go, .back]
      fire := fire ops E
      hit _ _ _ _ := false
      cert u := cert ops E u
      rules_eq _ := rfl
      fire_eq _ := rfl
      hit_eq _ := rfl
      trU oc w s u u' _ _ := by
        rcases s with ⟨⟩ | r
        · rw [trSPeek ops E u oc w, trSPeek ops E u' oc w]; exact trUPeek ops E oc _ _
        · cases r
          · rw [trGo ops E u oc w, trGo ops E u' oc w]; exact trUGo ops E oc _ _
          · rw [trBack ops E u oc w, trBack ops E u' oc w]; exact trUBack ops E oc _ _

    theorem oblU [Inhabited V] (ops : Ops V) (E : EqC V) : OblU (F ops E) := oblU_of_cert (F ops E) (certU ops E)
    '''
    a = s.index('theorem obl (ops : Ops V) (E : EqC V) (u : St → Nat → V)')
    b = s.index('\n', s.index('\n', a) + 1)
    s = s[:a] + extra_s + s[a:b + 1] + certu + s[b + 1:]
    old_b = s[s.index('def implK (ops : Ops V)'):s.index('end\nend Em%d' % mm)]
    new_b = '''def implK (ops : Ops V) : (κ : Kind) → Module V (KI κ)
      | .reg => Reg
      | .wire => RWire ops.ofBool false false

    theorem built [Inhabited V] (ops : Ops V) (E : EqC V) :
        Built X ((F ops E).toModuleU (fun j => (implK ops j.1).toU)) :=
      Built.node (F ops E) (fun j => (implK ops j.1).toU)
        (fun ⟨κ, _⟩ => by
          cases κ
          · exact Built.leaf _ reg_realizes (creg_causal 1)
          · exact Built.leaf _ (rwire_realizes _ _ _) (rwire_causal _ _ _))
        (fun ⟨κ, _⟩ => aliased_of_none _ _ (fun p q => by cases κ <;> rfl) (fun a p => by cases κ <;> rfl))
        (oblU ops E) (acyc ops E)

    theorem top [Inhabited V] (ops : Ops V) (E : EqC V) :
        RealizesU ((F ops E).toModuleU (fun j => (implK ops j.1).toU)) ∧
        ∀ s w, ((F ops E).toModuleU (fun j => (implK ops j.1).toU)).clean s = s →
          ∃ o s' g, ((F ops E).toModuleU (fun j => (implK ops j.1).toU)).hw s w o s' g :=
      (built ops E).top

    '''
    s = s.replace(old_b, new_b)
    s = s.replace('#print axioms Em%d.obl\n' % mm, '#print axioms Em%d.obl\n#print axioms Em%d.oblU\n' % (mm, mm))
    return s

