#ifndef __BS_PRIM_STORAGE_H__
#define __BS_PRIM_STORAGE_H__

#include "bluesim_types.h"
#include "bluesim_introspection.h"
#include "bs_wide_data.h"

/*
 * Caller-provided storage for Bluesim primitive state.
 *
 * A model is constructed inside a single caller-provided state
 * buffer: the module objects themselves (the "shells") are
 * placement-constructed at the front, and every primitive's live
 * value storage is bound to the element sub-area whose layout the
 * code generator published in the design's introspection descriptor
 * table (see bluesim_introspection.h).  Nothing here allocates.
 *
 * tStateLayout is the cursor the generated module constructors walk
 * during construction.  Module constructor initializer lists run in
 * member declaration order, which the code generator keeps identical
 * to the descriptor table's depth-first pre-order walk, so each
 * primitive constructor claims exactly the descriptor entry that
 * describes it.
 */
struct tStateLayout
{
  unsigned char*      elems; /* element sub-area base (borrowed) */
  const tBkStateInfo* table; /* the design's state descriptor table */
  tUInt32             next;  /* index of the next element to claim */

  /* claim the next element's storage, in table order */
  void* claim() { return elems + table[next++].offset; }
};

/* Bind a primitive's published element value to its claimed storage.
 * For a narrow value the storage IS the live value: the primitive
 * holds a reference into the element area (and 'stg' goes unused).
 * For a wide value 'stg' becomes a non-owning WideData view over the
 * element area's words and the reference designates 'stg'.
 */
template<typename T>
inline T& bs_bind_elem(T& /* stg */, void* elem, unsigned int /* bits */)
{
  return *((T*) elem);
}

inline WideData& bs_bind_elem(WideData& stg, void* elem, unsigned int bits)
{
  stg.bind((unsigned int*) elem, bits);
  return stg;
}

/* Bind a secondary (non-published) primitive value to auxiliary
 * storage carved from the parent module object.  A narrow member is
 * its own storage, so nothing happens.  A wide member becomes a
 * non-owning view over '*aux', which advances by the value's word
 * count; the parent sized the array with the BS_*_AUX_WORDS macro of
 * the primitive's header.
 */
template<typename T>
inline void bs_bind_aux(T& /* v */, unsigned int** /* aux */,
                        unsigned int /* bits */)
{
}

inline void bs_bind_aux(WideData& v, unsigned int** aux, unsigned int bits)
{
  v.bind(*aux, bits);
  *aux += NUM_WORDS(bits);
}

/* Return a value by value without allocating: a narrow value is
 * simply copied; a wide value is returned as a non-owning view of
 * the member's storage (the returned prvalue is constructed directly
 * in place, so no owning deep copy is made).
 */
template<typename T>
inline T bs_value_view(const T& v, unsigned int /* bits */)
{
  return v;
}

inline WideData bs_value_view(const WideData& v, unsigned int bits)
{
  return WideData(v.data, bits);
}

/* number of aux words a wide value of 'b' bits consumes (0 if narrow) */
#define BS_AUX_WORDS(b) ((b) > 64u ? (((b) + 31u) / 32u) : 0u)

/*
 * A flat array of primitive entries (RegFile/BRAM backing store,
 * FIFO data, ...) living in the caller-provided element area at the
 * published offsets: entry n of a w-bit element occupies its storage
 * unit (1/4/8 bytes, or NUM_WORDS(w) 32-bit words when wide) at
 * offset n * unit, exactly as documented in bluesim_introspection.h.
 *
 * The array is a non-owning view: bind() attaches it to claimed
 * storage and nothing is ever freed.  ref() hands back an lvalue for
 * a narrow entry and a non-owning WideData view for a wide one; both
 * read and write the caller's storage directly (bind the result to
 * 'auto&&' when a pointer to the entry is needed).
 */
template<typename T>
class tStateArray
{
 private:
  T* arr;
  unsigned int bits;

 public:
  tStateArray() : arr(0), bits(0u) {}

  void bind(void* storage, unsigned int width)
  {
    arr = (T*) storage;
    bits = width;
  }

  /* write the undetermined-value pattern over every entry */
  void init_undet(tUInt64 entries)
  {
    for (tUInt64 n = 0llu; n < entries; ++n)
      write_undet(&arr[n], bits);
  }

  T& ref(tUInt64 n) const { return arr[n]; }
  const T& get(tUInt64 n) const { return arr[n]; }
  void put(tUInt64 n, const T& v) { arr[n] = v; }

  /* the entry's value words, for symbol peeking */
  const unsigned int* sym_value(tUInt64 n) const
  {
    return (const unsigned int*) &arr[n];
  }
};

template<>
class tStateArray<WideData>
{
 private:
  unsigned int* base;
  unsigned int  bits;
  unsigned int  words;

 public:
  tStateArray() : base(0), bits(0u), words(0u) {}

  void bind(void* storage, unsigned int width)
  {
    base = (unsigned int*) storage;
    bits = width;
    words = NUM_WORDS(width);
  }

  void init_undet(tUInt64 entries)
  {
    for (tUInt64 n = 0llu; n < entries; ++n)
    {
      WideData e = ref(n);
      write_undet(&e, bits);
    }
  }

  /* a non-owning view of entry n (reference semantics) */
  WideData ref(tUInt64 n) const
  {
    return WideData(base + n * words, bits);
  }
  WideData get(tUInt64 n) const { return ref(n); }
  void put(tUInt64 n, const WideData& v)
  {
    WideData e = ref(n);
    e = v; /* assignment to a view copies words in place */
  }

  const unsigned int* sym_value(tUInt64 n) const
  {
    return base + n * words;
  }
};

#endif /* __BS_PRIM_STORAGE_H__ */
