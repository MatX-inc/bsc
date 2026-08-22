/*
 * Almost all primitive operations are defined
 * static inline in bs_prim_ops.h
 *
 * This file contains just the implementation of
 * functions which manage copies of arguments to
 * foreign functions.
 */

#include <cstring>

#include "bluesim_types.h"
#include "bs_mem_defines.h"
#include "bs_str.h"
#include "mem_alloc.h"

// Used internally to record argument copies which have been allocated
// but not yet released.  The record book is a pair of parallel arrays
// grown on demand through the Bluesim allocator; its static storage is
// constant-initialized, so loading a model runs no constructors and
// makes no allocator calls for it.
typedef struct
{
  void**        items;     // the outstanding copies
  unsigned int* sizes;     // their sizes, in words
  unsigned int  count;     // number of outstanding copies
  unsigned int  capacity;  // slots available in items/sizes
} tArgCopies;

static tArgCopies arg_copies_uint = { NULL, NULL, 0u, 0u };
static tArgCopies arg_copies_char = { NULL, NULL, 0u, 0u };

// Records the current return argument
static unsigned int* current_return_data = NULL;

/* number of words needed for 'n' entries of an items or sizes array */
static unsigned int record_words(unsigned int n, size_t entry_size)
{
  return (unsigned int) ((n * entry_size + sizeof(unsigned int) - 1) /
                         sizeof(unsigned int));
}

/* Record one outstanding copy, growing the record book if it is full.
 * The capacity is retained across delete_arg_copies() calls, so a
 * design's record book stops growing once it has seen its largest
 * batch of argument copies.
 */
static void record_copy(tArgCopies* book, void* ptr, unsigned int n)
{
  if (book->count == book->capacity)
  {
    unsigned int new_capacity = (book->capacity == 0u) ? 16u
                                                       : 2u * book->capacity;
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
    if (book->items != NULL)
      free_mem(book->items, record_words(book->capacity, sizeof(void*)));
    if (book->sizes != NULL)
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

// Copy a small argument (used with polymorphic arguments <= 8 bits)
unsigned int* copy_arg(const tUInt8* data, unsigned int /* n */)
{
  unsigned int* copy = (unsigned int*) alloc_mem(1);
  record_copy(&arg_copies_uint, copy, 1);
  copy[0] = (unsigned int) (*data);
  return copy;
}

// Copy a word argument (used with polymorphic arguments <= 32 bits)
unsigned int* copy_arg(const tUInt32* data)
{
  unsigned int* copy = (unsigned int*) alloc_mem(1);
  record_copy(&arg_copies_uint, copy, 1);
  copy[0] = (unsigned int) (*data);
  return copy;
}

// Copy a small argument (used with polymorphic arguments <= 64 bits)
unsigned int* copy_arg(const tUInt64* data, unsigned int /* n */)
{
  unsigned int* copy = (unsigned int*) alloc_mem(2);
  record_copy(&arg_copies_uint, copy, 2);
  copy[0] = (unsigned int) (*data);
  copy[1] = (unsigned int) ((*data) >> 32);
  return copy;
}

// Copy an array of unsigned ints (used with wide data, polymorphic or not)
unsigned int* copy_arg(const unsigned int* data, unsigned int n)
{
  unsigned int* copy = (unsigned int*) alloc_mem(n);
  record_copy(&arg_copies_uint, copy, n);
  memcpy(copy, data, n * sizeof(unsigned int));
  return copy;
}

// Copy a string argument held as a string tree (see bs_str.h): the
// tree's bytes are flattened, NUL-terminated, into argument storage.
char* copy_arg(const tStr* str)
{
  unsigned int n = (bs_str_len(str) / BYTES_PER_WORD) + 1;
  char* copy = (char*) alloc_mem(n);
  record_copy(&arg_copies_char, copy, n);
  bs_str_flatten(str, copy);
  return copy;
}

// Copy a string argument held in a plain character array
// (the form in which generated code stores its string literals)
char* copy_arg(const char* str)
{
  unsigned int n = (strlen(str) / BYTES_PER_WORD) + 1;
  char* copy = (char*) alloc_mem(n);
  record_copy(&arg_copies_char, copy, n);
  strcpy(copy, str);
  return copy;
}

// Allocate an uninitialized temporary array
unsigned int* ignore_arg(unsigned int n)
{
  unsigned int* arg = (unsigned int*) alloc_mem(n);
  record_copy(&arg_copies_uint, arg, n);
  current_return_data = NULL;
  return arg;
}

unsigned int* return_arg(unsigned int n)
{
  unsigned int* arg = (unsigned int*) alloc_mem(n);
  record_copy(&arg_copies_uint, arg, n);
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

// Delete all of the currently allocated argument copies
void delete_arg_copies()
{
  for (unsigned int i = 0u; i < arg_copies_uint.count; ++i)
    free_mem(arg_copies_uint.items[i], arg_copies_uint.sizes[i]);
  arg_copies_uint.count = 0u;

  for (unsigned int i = 0u; i < arg_copies_char.count; ++i)
    free_mem(arg_copies_char.items[i], arg_copies_char.sizes[i]);
  arg_copies_char.count = 0u;
}
