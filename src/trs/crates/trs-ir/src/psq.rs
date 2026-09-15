//! A weight-balanced priority search queue, ported from bsc's
//! `Balanced.lhs`.
//!
//! The merge needs this because bsc's topological sort pops from one,
//! and on a priority tie the node that comes out is decided by the
//! tree's shape rather than by its key.  The order between nodes the
//! schedule does not order is observable -- through unguarded
//! primitives and through task output -- so reproducing bsc's schedule
//! means reproducing this structure, not merely some valid topological
//! order.
//!
//! Keys are node ranks and priorities are in-degrees, so both are
//! `u32`; nothing here needs to be generic.  Subtrees are shared
//! rather than copied, as they are in the original, which is what
//! keeps `adjust` logarithmic.

use std::rc::Rc;

pub type Key = u32;
pub type Prio = u32;

/// A loser tree.  `left_wins` distinguishes bsc's `LLoser` from its
/// `RLoser`: which side of the match the loser recorded here came from.
#[derive(Clone)]
pub enum LTree {
    Start,
    Loser {
        left_wins: bool,
        size: u32,
        k: Key,
        p: Prio,
        l: Rc<LTree>,
        m: Key,
        r: Rc<LTree>,
    },
}

/// The queue: empty, or the winner plus the tree of everyone it beat
/// and the largest key present.
#[derive(Clone)]
pub enum Psq {
    Void,
    Winner {
        k: Key,
        p: Prio,
        t: Rc<LTree>,
        m: Key,
    },
}

fn size(t: &LTree) -> u32 {
    match t {
        LTree::Start => 0,
        LTree::Loser { size, .. } => *size,
    }
}

fn left(t: &LTree) -> &LTree {
    match t {
        LTree::Start => panic!("left of an empty loser tree"),
        LTree::Loser { l, .. } => l,
    }
}

fn right(t: &LTree) -> &LTree {
    match t {
        LTree::Start => panic!("right of an empty loser tree"),
        LTree::Loser { r, .. } => r,
    }
}

fn loser(left_wins: bool, k: Key, p: Prio, l: Rc<LTree>, m: Key, r: Rc<LTree>) -> Rc<LTree> {
    let size = 1 + size(&l) + size(&r);
    Rc::new(LTree::Loser {
        left_wins,
        size,
        k,
        p,
        l,
        m,
        r,
    })
}

fn lloser(k: Key, p: Prio, l: Rc<LTree>, m: Key, r: Rc<LTree>) -> Rc<LTree> {
    loser(true, k, p, l, m, r)
}

fn rloser(k: Key, p: Prio, l: Rc<LTree>, m: Key, r: Rc<LTree>) -> Rc<LTree> {
    loser(false, k, p, l, m, r)
}

/// Adams's balance factor.
const OMEGA: u32 = 2;

fn balance(lose_left: bool, k: Key, p: Prio, l: Rc<LTree>, m: Key, r: Rc<LTree>) -> Rc<LTree> {
    let (sl, sr) = (size(&l), size(&r));
    let mk = if lose_left { lloser } else { rloser };
    if sl + sr < 2 {
        mk(k, p, l, m, r)
    } else if sr > OMEGA * sl {
        balance_left(lose_left, k, p, l, m, r)
    } else if sl > OMEGA * sr {
        balance_right(lose_left, k, p, l, m, r)
    } else {
        mk(k, p, l, m, r)
    }
}

fn balance_left(lose_left: bool, k: Key, p: Prio, l: Rc<LTree>, m: Key, r: Rc<LTree>) -> Rc<LTree> {
    if size(left(&r)) < size(right(&r)) {
        single_left(lose_left, k, p, l, m, r)
    } else {
        double_left(lose_left, k, p, l, m, r)
    }
}

fn balance_right(
    lose_left: bool,
    k: Key,
    p: Prio,
    l: Rc<LTree>,
    m: Key,
    r: Rc<LTree>,
) -> Rc<LTree> {
    if size(right(&l)) < size(left(&l)) {
        single_right(lose_left, k, p, l, m, r)
    } else {
        double_right(lose_left, k, p, l, m, r)
    }
}

/// Destructure a loser node; the caller has already established it is
/// not `Start`.
fn parts(t: &LTree) -> (bool, Key, Prio, Rc<LTree>, Key, Rc<LTree>) {
    match t {
        LTree::Start => panic!("rotation through an empty loser tree"),
        LTree::Loser {
            left_wins,
            k,
            p,
            l,
            m,
            r,
            ..
        } => (*left_wins, *k, *p, l.clone(), *m, r.clone()),
    }
}

fn single_left(
    lose_left: bool,
    k1: Key,
    p1: Prio,
    t1: Rc<LTree>,
    m1: Key,
    r: Rc<LTree>,
) -> Rc<LTree> {
    let (r_left, k2, p2, t2, m2, t3) = parts(&r);
    if lose_left {
        if r_left {
            if p1 <= p2 {
                lloser(k1, p1, rloser(k2, p2, t1, m1, t2), m2, t3)
            } else {
                lloser(k2, p2, lloser(k1, p1, t1, m1, t2), m2, t3)
            }
        } else {
            rloser(k2, p2, lloser(k1, p1, t1, m1, t2), m2, t3)
        }
    } else if r_left {
        rloser(k1, p1, rloser(k2, p2, t1, m1, t2), m2, t3)
    } else {
        rloser(k2, p2, rloser(k1, p1, t1, m1, t2), m2, t3)
    }
}

fn single_right(
    lose_left: bool,
    k1: Key,
    p1: Prio,
    l: Rc<LTree>,
    m2: Key,
    t3: Rc<LTree>,
) -> Rc<LTree> {
    let (l_left, k2, p2, t1, m1, t2) = parts(&l);
    if lose_left {
        if l_left {
            lloser(k2, p2, t1, m1, lloser(k1, p1, t2, m2, t3))
        } else {
            lloser(k1, p1, t1, m1, lloser(k2, p2, t2, m2, t3))
        }
    } else if l_left {
        lloser(k2, p2, t1, m1, rloser(k1, p1, t2, m2, t3))
    } else if p1 <= p2 {
        rloser(k1, p1, t1, m1, lloser(k2, p2, t2, m2, t3))
    } else {
        rloser(k2, p2, t1, m1, rloser(k1, p1, t2, m2, t3))
    }
}

fn double_left(
    lose_left: bool,
    k1: Key,
    p1: Prio,
    t1: Rc<LTree>,
    m1: Key,
    r: Rc<LTree>,
) -> Rc<LTree> {
    let (r_left, k2, p2, t2, m2, t3) = parts(&r);
    single_left(
        lose_left,
        k1,
        p1,
        t1,
        m1,
        single_right(r_left, k2, p2, t2, m2, t3),
    )
}

fn double_right(
    lose_left: bool,
    k1: Key,
    p1: Prio,
    l: Rc<LTree>,
    m2: Key,
    t3: Rc<LTree>,
) -> Rc<LTree> {
    let (l_left, k2, p2, t1, m1, t2) = parts(&l);
    single_right(
        lose_left,
        k1,
        p1,
        single_left(l_left, k2, p2, t1, m1, t2),
        m2,
        t3,
    )
}

/// One match: the lower priority wins, and a tie goes to the left --
/// which, because the tree is key-ordered, is the smaller key only
/// when the two are siblings.
fn play(a: Psq, b: Psq) -> Psq {
    match (a, b) {
        (Psq::Void, t) => t,
        (t, Psq::Void) => t,
        (
            Psq::Winner { k, p, t, m },
            Psq::Winner {
                k: k2,
                p: p2,
                t: t2,
                m: m2,
            },
        ) => {
            if p <= p2 {
                Psq::Winner {
                    k,
                    p,
                    t: balance(false, k2, p2, t, m, t2),
                    m: m2,
                }
            } else {
                Psq::Winner {
                    k: k2,
                    p: p2,
                    t: balance(true, k, p, t, m, t2),
                    m: m2,
                }
            }
        }
    }
}

fn single(k: Key, p: Prio) -> Psq {
    Psq::Winner {
        k,
        p,
        t: Rc::new(LTree::Start),
        m: k,
    }
}

fn max_key(q: &Psq) -> Key {
    match q {
        Psq::Void => panic!("max key of an empty queue"),
        Psq::Winner { m, .. } => *m,
    }
}

enum Tour {
    Null,
    Single(Key, Prio),
    Play(Psq, Psq),
}

fn tour_view(q: &Psq) -> Tour {
    match q {
        Psq::Void => Tour::Null,
        Psq::Winner { k, p, t, m } => match &**t {
            LTree::Start => Tour::Single(*k, *p),
            LTree::Loser {
                left_wins,
                k: k2,
                p: p2,
                l,
                m: m2,
                r,
                ..
            } => {
                let (a, b) = if *left_wins {
                    (
                        Psq::Winner {
                            k: *k2,
                            p: *p2,
                            t: l.clone(),
                            m: *m2,
                        },
                        Psq::Winner {
                            k: *k,
                            p: *p,
                            t: r.clone(),
                            m: *m,
                        },
                    )
                } else {
                    (
                        Psq::Winner {
                            k: *k,
                            p: *p,
                            t: l.clone(),
                            m: *m2,
                        },
                        Psq::Winner {
                            k: *k2,
                            p: *p2,
                            t: r.clone(),
                            m: *m,
                        },
                    )
                };
                Tour::Play(a, b)
            }
        },
    }
}

/// Build a queue from bindings in ascending key order, folding by
/// binary subdivision so the tree comes out balanced the same way
/// bsc's does.
pub fn from_ord_list(bindings: &[(Key, Prio)]) -> Psq {
    fn rec(xs: &[(Key, Prio)]) -> Psq {
        match xs.len() {
            0 => Psq::Void,
            1 => single(xs[0].0, xs[0].1),
            n => {
                let m = n / 2;
                play(rec(&xs[..n - m]), rec(&xs[n - m..]))
            }
        }
    }
    rec(bindings)
}

fn second_best(t: &LTree, m: Key) -> Psq {
    match t {
        LTree::Start => Psq::Void,
        LTree::Loser {
            left_wins,
            k,
            p,
            l,
            m: m2,
            r,
            ..
        } => {
            if *left_wins {
                play(
                    Psq::Winner {
                        k: *k,
                        p: *p,
                        t: l.clone(),
                        m: *m2,
                    },
                    second_best(r, m),
                )
            } else {
                play(
                    second_best(l, *m2),
                    Psq::Winner {
                        k: *k,
                        p: *p,
                        t: r.clone(),
                        m,
                    },
                )
            }
        }
    }
}

/// The lowest-priority binding and the queue without it.
pub fn min_view(q: &Psq) -> Option<((Key, Prio), Psq)> {
    match q {
        Psq::Void => None,
        Psq::Winner { k, p, t, m } => Some(((*k, *p), second_best(t, *m))),
    }
}

/// Apply `f` to one key's priority.
pub fn adjust(q: &Psq, k: Key, f: impl Fn(Prio) -> Prio + Copy) -> Psq {
    match tour_view(q) {
        Tour::Null => Psq::Void,
        Tour::Single(k2, p) => single(k2, if k == k2 { f(p) } else { p }),
        Tour::Play(tl, tr) => {
            if k <= max_key(&tl) {
                play(adjust(&tl, k, f), tr)
            } else {
                play(tl, adjust(&tr, k, f))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// bsc's `ntsort` loop, over integer nodes given by their
    /// predecessor lists.  The merge runs the same loop over schedule
    /// nodes; here it stands in for it so the queue can be checked
    /// against bsc's own answers.
    fn tsort(priors: &[Vec<u32>]) -> Vec<u32> {
        let n = priors.len() as u32;
        let mut after: Vec<Vec<u32>> = vec![Vec::new(); n as usize];
        for (i, ps) in priors.iter().enumerate() {
            for p in ps {
                after[*p as usize].push(i as u32);
            }
        }
        for v in &mut after {
            v.sort_unstable_by(|a, b| b.cmp(a));
        }
        let bindings: Vec<(Key, Prio)> = (0..n)
            .map(|i| (i, priors[i as usize].len() as u32))
            .collect();
        let mut q = from_ord_list(&bindings);
        let mut out = Vec::new();
        while let Some(((i, p), rest)) = min_view(&q) {
            assert_eq!(p, 0, "these graphs are acyclic");
            out.push(i);
            q = rest;
            for t in &after[i as usize] {
                q = adjust(&q, *t, |d| d - 1);
            }
        }
        out
    }

    fn parse(spec: &str) -> Vec<Vec<u32>> {
        spec.split(';')
            .map(|s| {
                if s.is_empty() {
                    vec![]
                } else {
                    s.split(',').map(|x| x.parse().unwrap()).collect()
                }
            })
            .collect()
    }

    /// Answers taken from bsc's own `SCC.tsort` run over these graphs.
    /// The point of the port is to agree with it on nodes the graph
    /// does not order, so cases where it does something other than
    /// count upwards are what this is for -- the plain ones only show
    /// the queue has not broken something it had no business touching.
    #[test]
    fn the_queue_pops_what_bscs_does() {
        let cases: &[(&str, &[u32])] = &[
            (";;;0;1;2", &[0, 1, 2, 3, 4, 5]),
            (";;;;;0;1;2;3;4", &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9]),
            // six Sched/Exec pairs: 5 comes out before 4
            (";;;;;;0;1;2;3;4;5", &[0, 1, 2, 3, 5, 4, 6, 7, 8, 9, 10, 11]),
            // nine pairs: 11 before 10
            (
                ";;;;;;;;;0;1;2;3;4;5;6;7;8",
                &[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 11, 10, 12, 13, 14, 15, 16, 17],
            ),
            (";;;;;;;;", &[0, 1, 2, 3, 4, 5, 6, 7, 8]),
            // four scattered edges among 24 nodes, and the shape of the
            // queue reorders four separate pairs
            (
                ";;;;;1;;5;;;;3;;5;;;;;;;;;;",
                &[
                    0, 1, 2, 3, 4, 5, 6, 7, 9, 8, 11, 10, 12, 14, 13, 15, 17, 16, 18, 19, 20, 21,
                    23, 22,
                ],
            ),
            (
                ";;1;1;;0,1;0,1,2,4;1,3,5;2;1,4;2,7;2,3,8,10;3,9;5,10;;5,7,11,14;\
                 0,4,6;4,6,10,11,15,16;2,4,5,7;2,4,16;6,7,17;11,16,18,19;\
                 4,8,14,16;9,17,19,21",
                &[
                    0, 1, 2, 3, 4, 5, 6, 7, 9, 8, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21,
                    22, 23,
                ],
            ),
            (
                ";0;;;1,2;4;;0,1,4;5;2;;5,8;7;6;0;7;0,6,10,13;3,4,10;\
                 1,10,11,15,16;1,6,8,9,18;6,8,12,17;11,12,13,14;1,6,7,12;3,7,19,20",
                &[
                    0, 1, 2, 3, 4, 5, 6, 7, 9, 8, 10, 11, 12, 14, 13, 15, 16, 17, 18, 19, 20, 21,
                    22, 23,
                ],
            ),
        ];
        for (spec, want) in cases {
            let g = parse(&spec.replace(char::is_whitespace, ""));
            assert_eq!(tsort(&g), *want, "graph {spec}");
        }
    }

    /// Whatever the queue does about ties, it still has to sort.
    #[test]
    fn every_node_comes_out_after_its_predecessors() {
        // a deterministic spread of shapes, so a change in the
        // balancing shows up as more than a reordering
        let mut state = 12345u64;
        let mut next = || {
            state = state
                .wrapping_mul(6364136223846793005)
                .wrapping_add(1442695040888963407);
            state >> 33
        };
        for _ in 0..200 {
            let n = 1 + (next() % 30) as usize;
            let priors: Vec<Vec<u32>> = (0..n)
                .map(|i| (0..i as u32).filter(|_| next() % 5 == 0).collect())
                .collect();
            let order = tsort(&priors);
            assert_eq!(order.len(), n, "every node must be placed");
            let mut at = vec![usize::MAX; n];
            for (p, i) in order.iter().enumerate() {
                at[*i as usize] = p;
            }
            for (i, ps) in priors.iter().enumerate() {
                for p in ps {
                    assert!(at[*p as usize] < at[i], "{p} must precede {i}");
                }
            }
        }
    }
}
