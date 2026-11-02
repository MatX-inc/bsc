"""Negative controls for the site-value readiness check (round 7 review s2 minors 1, 2, 4).

Each synthetic design has one rule whose guard reads f1.first on its port (the stale head) and a
readiness read f2.first whose R_seq has one literal of the kind under test.  The readiness fact of
f2.first is false for the file's formula, so a correct tool must not confirm it:
  eqcase  -- R is an 'eq' literal over the conditional read f1.first (a case arm);
  eqsel   -- the same from an array selection;
  ccase   -- the same literal as a comparison (an `if`): the control the old tool already got right;
  alias   -- the guard reads f2.notEmpty, the file's literal is f2.i_notEmpty (separate ports in the file);
  hwlit   -- R's literal is another rule's WILL_FIRE (the file's R cannot read it);
and positive controls whose fact holds for the file's formula (eqok, aliasok, hwok; literals are
whole: a wire inside an `if` condition drops that whole condition, so hwok nests its two conditions).
usage: python3 -I neg_sitecond.py WORKDIR [TOOLSDIR...]   (default: this directory)
"""
import os
import subprocess
import sys

HERE = os.path.dirname(os.path.abspath(__file__))

KIDS = '''children:
  f1 : FIFO2   []
    first (value)
      copy -:
    i_notEmpty (value)
      copy -:
    notEmpty (value)
      copy -:
  f2 : FIFO2   []
    first (value)
      copy -:
    i_notEmpty (value)
      copy -:
    notEmpty (value)
      copy -:
  r : RegN   []
    read (value)
      copy -:
  o : RegN   []
    write (action)
      copy -:
'''

BA_HEAD = '''  f1 :: ABSTRACT:  FIFOF_._FIFOF_.FIFOF_143 = FIFO2
  f2 :: ABSTRACT:  FIFOF_._FIFOF_.FIFOF_143 = FIFO2
  r :: ABSTRACT:  Prelude.VReg = RegN
  o :: ABSTRACT:  Prelude.VReg = RegN
  -- AP local definitions
'''

# design -> (local definitions, the rule's guard name, reads of RL_t, extra rule RL_u?)
CASES = {
    'eqcase': ([('g', 'Bit 1', "!(f1.first == 8'd3) || f2.i_notEmpty"),
                ('sel', 'Bit 8', "case f1.first of 8'd3 -> f2.first _ -> 8'd0")],
               ['f1.first', 'f1.i_notEmpty', 'f2.first', 'f2.i_notEmpty'], False),
    'eqsel': ([('g', 'Bit 1', "!(f1.first == 1) || f2.i_notEmpty"),
               ('arr', 'Bit 16', "PrimBuildArray 8'd0 f2.first"),
               ('sel', 'Bit 8', "PrimArrayDynSelect arr f1.first")],
              ['f1.first', 'f1.i_notEmpty', 'f2.first', 'f2.i_notEmpty'], False),
    'ccase': ([('g', 'Bit 1', "!(f1.first == 8'd3) || f2.i_notEmpty"),
               ('sel', 'Bit 8', "_if_ (f1.first == 8'd3) f2.first 8'd0")],
              ['f1.first', 'f1.i_notEmpty', 'f2.first', 'f2.i_notEmpty'], False),
    'alias': ([('g', 'Bit 1', "(r.read == 8'd0) || f2.notEmpty"),
               ('sel', 'Bit 8', "_if_ (r.read == 8'd0) 8'd0 f2.first")],
              ['r.read', 'f2.first', 'f2.i_notEmpty', 'f2.notEmpty'], False),
    'hwlit': ([('g', 'Bit 1', "!(r.read == 8'd1) || f2.i_notEmpty"),
               ('WILL_FIRE_RL_u', 'Bit 1', "r.read == 8'd1"),
               ('sel', 'Bit 8', "_if_ WILL_FIRE_RL_u f2.first 8'd0")],
              ['r.read', 'f2.first', 'f2.i_notEmpty'], True),
    # positive controls: the fact holds for the file's formula, and both tools confirm it
    'eqok': ([('g', 'Bit 1', "!(f1.first == 8'd3) || f2.i_notEmpty"),
              ('sel', 'Bit 8', "_if_ f1.i_notEmpty (case f1.first of 8'd3 -> f2.first _ -> 8'd0) 8'd0")],
             ['f1.first', 'f1.i_notEmpty', 'f2.first', 'f2.i_notEmpty'], False),
    'aliasok': ([('g', 'Bit 1', "(r.read == 8'd0) || f2.i_notEmpty"),
                 ('sel', 'Bit 8', "_if_ (r.read == 8'd0) 8'd0 f2.first")],
                ['r.read', 'f2.first', 'f2.i_notEmpty', 'f2.notEmpty'], False),
    'hwok': ([('g', 'Bit 1', "(r.read == 8'd2) || f2.i_notEmpty"),
              ('WILL_FIRE_RL_u', 'Bit 1', "r.read == 8'd1"),
              ('sel', 'Bit 8', "_if_ WILL_FIRE_RL_u (_if_ (!(r.read == 8'd2)) f2.first 8'd0) 8'd0")],
             ['r.read', 'f2.first', 'f2.i_notEmpty'], True),
}


def write_design(work, name, defs, reads, extra):
    d = os.path.join(work, 'designs', name)
    os.makedirs(d, exist_ok=True)
    steps = ['  1. RL_t   [rule RL_t]', '       guard g']
    steps += ['       reads %s  (hint: used when 1\'d1)' % x for x in reads]
    steps += ['       calls o.write when 1\'d1 with sel']
    if extra:
        steps += ['  2. RL_u   [rule RL_u]', "       guard WILL_FIRE_RL_u", '       reads r.read  (hint: used when 1\'d1)']
    open(os.path.join(d, 'dump.txt'), 'w').write(
        'module %s (package Neg), from Neg.bs\ninputs:  CLK :: Clock\n\n%s\nexecution order:\n%s\n' % (
            name, KIDS, '\n'.join(steps)))
    ba = BA_HEAD + ''.join('  %s :: %s;\n  %s  = %s;\n' % (n, t, n, e) for n, t, e in defs)
    ba += '  -- AP rules\nrule RL_t "RL_t":\n when g\n  ==> { o.write sel; }\n'
    if extra:
        ba += 'rule RL_u "RL_u":\n when WILL_FIRE_RL_u\n  ==> { }\n'
    os.makedirs(os.path.join(work, 'data'), exist_ok=True)
    open(os.path.join(work, 'data', name + '.ba.txt'), 'w').write(ba)
    return d


RUN = r'''
import os, sys
sys.path.insert(0, sys.argv[1])
import sitecond
work = sys.argv[2]
for d in sys.argv[3:]:
    res = sitecond.analyse(d, work, work)
    for st, s in res['_ctx']['need']:
        if 'r' in s['need'] and s['label'] == 'f2.first':
            sv = s['rdy_sv']
            print('%-7s f2.first readiness fact confirmed: %s' % (os.path.basename(d), sv['rdy']))
'''


def main():
    work = os.path.abspath(sys.argv[1])
    tools = sys.argv[2:] or [HERE]
    os.makedirs(work, exist_ok=True)
    ds = [write_design(work, n, *CASES[n]) for n in CASES]
    for t in tools:
        print('== tools', t)
        env = dict(os.environ, SITES_DATA=os.path.join(work, 'data'))
        r = subprocess.run([sys.executable, '-I', '-c', RUN, os.path.abspath(t), work] + ds, env=env,
                           stdout=subprocess.PIPE, stderr=subprocess.STDOUT, text=True)
        print(r.stdout, end='')


if __name__ == '__main__':
    main()
