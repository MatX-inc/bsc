#ifndef __BS_STR_H__
#define __BS_STR_H__

#include <cstddef>

#include "bluesim_types.h"

/*
 * A constant string value in a Bluesim model.
 *
 * Generated code represents a string as a pointer to an immutable
 * tStr node: either a leaf, holding a pointer to some bytes and
 * their count, or the concatenation of two subtrees.  Byte counts
 * are explicit so embedded NUL characters are preserved end to end.
 *
 * A leaf for a string literal is a constant-initialized file-scope
 * object next to the literal's character array; a string module
 * parameter is a node pointer passed in through the module's
 * constructor; and every string concatenation in a module is one
 * node member of that module, built once in its constructor.  The
 * node for a concatenation therefore never re-concatenates: uses of
 * the value just walk the finished tree.
 *
 * Every node carries the total byte count of its subtree, so
 * consumers can size buffers without walking, and the walk itself
 * (bs_str_leaf_at) descends from the root by comparing a byte
 * position against subtree counts: no recursion, no side storage.
 */
struct tStr
{
  const char* data;  /* leaf: the bytes (not NUL-terminated)     */
  tUInt32     len;   /* total byte count of this subtree         */
  const tStr* fst;   /* concatenation: first part  (leaf: NULL)  */
  const tStr* snd;   /* concatenation: second part (leaf: NULL)  */

  /* a leaf over the 'n' bytes at 'd' */
  constexpr tStr(const char* d, tUInt32 n)
    : data(d), len(n), fst(NULL), snd(NULL) {}
  /* the concatenation of 'a' and 'b' */
  tStr(const tStr* a, const tStr* b)
    : data(NULL), len(a->len + b->len), fst(a), snd(b) {}
};

/* Total byte count of a string value; an absent value is empty. */
static inline tUInt32 bs_str_len(const tStr* s)
{
  return (s == NULL) ? 0u : s->len;
}

/* The leaf holding byte 'pos' of 's', for pos < s->len.  '*off'
 * receives the position of the leaf's first byte within 's'.  The
 * returned leaf is never empty: descending by position steps over
 * zero-length leaves.
 */
static inline const tStr* bs_str_leaf_at(const tStr* s, tUInt32 pos,
                                         tUInt32* off)
{
  tUInt32 base = 0u;
  while (s->data == NULL)
  {
    if (pos - base < s->fst->len)
      s = s->fst;
    else
    {
      base += s->fst->len;
      s = s->snd;
    }
  }
  *off = base;
  return s;
}

/* Copy the bytes of 's' into 'buf', which holds at least
 * bs_str_len(s) bytes.  Returns 'buf'.
 */
static inline char* bs_str_copy(const tStr* s, char* buf)
{
  tUInt32 pos = 0u;
  tUInt32 total = bs_str_len(s);
  while (pos < total)
  {
    tUInt32 off;
    const tStr* leaf = bs_str_leaf_at(s, pos, &off);
    for (tUInt32 i = 0u; i < leaf->len; ++i)
      buf[off + i] = leaf->data[i];
    pos = off + leaf->len;
  }
  return buf;
}

/* Copy 's' into 'buf' NUL-terminated; 'buf' holds at least
 * bs_str_len(s) + 1 bytes.  Consumers that need C-string semantics
 * (file names, open modes, plusarg names) flatten through this into
 * a stack buffer at the call site.
 */
static inline const char* bs_str_flatten(const tStr* s, char* buf)
{
  bs_str_copy(s, buf);
  buf[bs_str_len(s)] = '\0';
  return buf;
}

#endif /* __BS_STR_H__ */
