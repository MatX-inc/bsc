/*
 * Almost all primitive operations are defined
 * static inline in bs_prim_ops.h
 *
 * This file contains just the implementation of
 * functions which manage copies of arguments to
 * foreign functions.
 *
 * This translation unit is linked into a model only when the design
 * imports foreign (BDPI) functions -- generated code references it
 * only for their argument marshaling.  BDPI marshaling is the one
 * truly unbounded storage consumer in a Bluesim model: an imported
 * function can take arbitrarily many arguments of arbitrary width,
 * so its copies cannot be bounded at code-generation time.  It is
 * handled in two tiers:
 *
 *   - a fixed bump arena serves the copies of a foreign call batch
 *     and is reset wholesale by delete_arg_copies() at the end of
 *     the batch, so ordinary BDPI traffic makes no allocator calls
 *     at all in steady state;
 *
 *   - a batch too large for the arena falls back to the Bluesim
 *     allocator (alloc_mem), with each fallback block recorded for
 *     freeing by delete_arg_copies().
 *
 * Because this TU is the unbounded consumer, it also carries the
 * strong definitions of the Bluesim allocator's heap hooks
 * (bs_mem_heap_*, declared weak in mem_alloc.cxx): linking a design
 * with BDPI imports is what gives the model an unbounded
 * program-allocator fallback -- and the operator new[]/delete[]
 * imports in its shared object, the documented exclusion from the
 * no-allocator-imports property.  Models without BDPI imports never
 * pull in this TU and stay free of allocator imports.
 */

#include <cstring>

#include "bluesim_types.h"
#include "bs_mem_defines.h"
#include "bs_str.h"
#include "mem_alloc.h"

/* The strong definitions of the Bluesim allocator's unbounded
 * fallback hooks (see mem_alloc.cxx).
 */
extern "C" void* bs_mem_heap_alloc(unsigned int nWords)
{
  return new unsigned int[(nWords > 0u) ? nWords : 1u];
}

extern "C" void bs_mem_heap_free(void* ptr, unsigned int /* nWords */)
{
  delete[] ((unsigned int*) ptr);
}

/* Size of the bump arena serving one foreign call batch's argument
 * copies, in 32-bit words.
 */
#define BS_ARG_COPY_ARENA_WORDS 4096u

static unsigned int arg_arena[BS_ARG_COPY_ARENA_WORDS]
  __attribute__((aligned(16)));
static unsigned int arg_arena_used = 0u;

/* The record book for fallback copies: the blocks taken from
 * alloc_mem() when a batch outgrows the arena, to be freed by
 * delete_arg_copies().  The first entries are embedded static
 * storage; the book itself grows through the Bluesim allocator
 * beyond that.  All static storage here is constant-initialized:
 * loading a model runs no constructors and makes no allocator calls
 * for it.
 */
#define BS_ARG_COPY_BOOK_ENTRIES 32u

static void*        book_items_first[BS_ARG_COPY_BOOK_ENTRIES];
static unsigned int book_sizes_first[BS_ARG_COPY_BOOK_ENTRIES];

typedef struct
{
  void**        items;     // the outstanding fallback copies
  unsigned int* sizes;     // their sizes, in words
  unsigned int  count;     // number of outstanding copies
  unsigned int  capacity;  // slots available in items/sizes
} tArgCopies;

static tArgCopies arg_copies =
  { book_items_first, book_sizes_first, 0u, BS_ARG_COPY_BOOK_ENTRIES };

// Records the current return argument
static unsigned int* current_return_data = NULL;

/* number of words needed for 'n' entries of an items or sizes array */
static unsigned int record_words(unsigned int n, size_t entry_size)
{
  return (unsigned int) ((n * entry_size + sizeof(unsigned int) - 1) /
                         sizeof(unsigned int));
}

/* Record one outstanding fallback copy, growing the record book if
 * it is full.  The capacity is retained across delete_arg_copies()
 * calls, so a design's record book stops growing once it has seen
 * its largest batch of fallback copies.
 */
static void record_copy(void* ptr, unsigned int n)
{
  tArgCopies* book = &arg_copies;
  if (book->count == book->capacity)
  {
    unsigned int new_capacity = 2u * book->capacity;
    void** new_items =
      (void**) alloc_mem(record_words(new_capacity, sizeof(void*)));
    unsigned int* new_sizes =
      (unsigned int*) alloc_mem(record_words(new_capacity,
                                             sizeof(unsigned int)));
    for (unsigned int i = 0u; i < book->count; ++i)
    {
      new_items[i] = book->items[i];
      new_sizes[i] = book->sizes[i];
    }
    if (book->items != book_items_first)
      free_mem(book->items, record_words(book->capacity, sizeof(void*)));
    if (book->sizes != book_sizes_first)
      free_mem(book->sizes, record_words(book->capacity,
                                         sizeof(unsigned int)));
    book->items = new_items;
    book->sizes = new_sizes;
    book->capacity = new_capacity;
  }
  book->items[book->count] = ptr;
  book->sizes[book->count] = n;
  ++(book->count);
}

/* Allocate 'nWords' words for one argument copy: from the bump
 * arena when it fits (nothing to record -- the arena is reset
 * wholesale), from the Bluesim allocator otherwise (recorded for
 * freeing).
 */
static void* copy_alloc(unsigned int nWords)
{
  /* carve in even word counts so every copy is 8-byte aligned */
  unsigned int carve = (nWords + 1u) & ~1u;
  if (carve == 0u)
    carve = 2u;
  if (arg_arena_used + carve <= BS_ARG_COPY_ARENA_WORDS)
  {
    void* copy = (void*) (arg_arena + arg_arena_used);
    arg_arena_used += carve;
    return copy;
  }
  void* copy = alloc_mem(nWords);
  record_copy(copy, nWords);
  return copy;
}

// Copy a small argument (used with polymorphic arguments <= 8 bits)
unsigned int* copy_arg(const tUInt8* data, unsigned int /* n */)
{
  unsigned int* copy = (unsigned int*) copy_alloc(1);
  copy[0] = (unsigned int) (*data);
  return copy;
}

// Copy a word argument (used with polymorphic arguments <= 32 bits)
unsigned int* copy_arg(const tUInt32* data)
{
  unsigned int* copy = (unsigned int*) copy_alloc(1);
  copy[0] = (unsigned int) (*data);
  return copy;
}

// Copy a small argument (used with polymorphic arguments <= 64 bits)
unsigned int* copy_arg(const tUInt64* data, unsigned int /* n */)
{
  unsigned int* copy = (unsigned int*) copy_alloc(2);
  copy[0] = (unsigned int) (*data);
  copy[1] = (unsigned int) ((*data) >> 32);
  return copy;
}

// Copy an array of unsigned ints (used with wide data, polymorphic or not)
unsigned int* copy_arg(const unsigned int* data, unsigned int n)
{
  unsigned int* copy = (unsigned int*) copy_alloc(n);
  memcpy(copy, data, n * sizeof(unsigned int));
  return copy;
}

// Copy a string argument held as a string tree (see bs_str.h): the
// tree's bytes are flattened, NUL-terminated, into argument storage.
char* copy_arg(const tStr* str)
{
  unsigned int n = (bs_str_len(str) / BYTES_PER_WORD) + 1;
  char* copy = (char*) copy_alloc(n);
  bs_str_flatten(str, copy);
  return copy;
}

// Copy a string argument held in a plain character array
// (the form in which generated code stores its string literals)
char* copy_arg(const char* str)
{
  unsigned int n = (strlen(str) / BYTES_PER_WORD) + 1;
  char* copy = (char*) copy_alloc(n);
  strcpy(copy, str);
  return copy;
}

// Allocate an uninitialized temporary array
unsigned int* ignore_arg(unsigned int n)
{
  unsigned int* arg = (unsigned int*) copy_alloc(n);
  current_return_data = NULL;
  return arg;
}

unsigned int* return_arg(unsigned int n)
{
  unsigned int* arg = (unsigned int*) copy_alloc(n);
  current_return_data = arg;
  return arg;
}

// Copy data from current_return_data back to the result
tUInt8  write_return(unsigned int unused, tUInt8* data)
{
  *data = (tUInt8) (current_return_data[0] & 0xFF);
  return *data;
}

tUInt32 write_return(unsigned int unused, tUInt32* data)
{
  *data = (tUInt32) (current_return_data[0]);
  return *data;
}

tUInt64 write_return(unsigned int unused, tUInt64* data)
{
  *data = ((tUInt64) current_return_data[0]);
  *data |= ((tUInt64) current_return_data[1]) << 32;
  return *data;
}

// Delete all of the currently outstanding argument copies: reset the
// bump arena and free the recorded fallback copies
void delete_arg_copies()
{
  arg_arena_used = 0u;
  for (unsigned int i = 0u; i < arg_copies.count; ++i)
    free_mem(arg_copies.items[i], arg_copies.sizes[i]);
  arg_copies.count = 0u;
}
