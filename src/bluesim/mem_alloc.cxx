/* The Bluesim word allocator.
 *
 * Storage comes from a fixed, statically allocated arena; freed
 * blocks are kept on size-indexed free lists and reused.  The
 * allocator therefore makes NO program-allocator calls (malloc or
 * operator new) itself, so a model shared object that references it
 * imports no allocator symbols.
 *
 * The one truly unbounded consumer is BDPI argument marshaling
 * (prim_ops.cxx).  That translation unit -- linked into a model only
 * when the design imports BDPI functions -- provides the strong
 * definitions of the bs_mem_heap_* hooks declared weak below, giving
 * such models an unbounded program-allocator fallback once the arena
 * is exhausted (and, with it, the documented operator new[]/delete[]
 * imports).  In a model without BDPI imports the hooks resolve to
 * NULL and exhausting the arena is a fatal error, reported through
 * the process-wide host operations in the manner of the other fixed
 * runtime capacities (see e.g. the event queue).
 */

#include "mem_alloc.h"
#include "bluesim_kernel_api.h"
#include "bluesim_host_ops.h"

#define MAX_CHUNK_WORDS 16

/* Size of the fixed arena, in 32-bit words.  Steady-state simulation
 * allocates nothing at all (the hostalloc testsuite directory proves
 * it); what the arena serves is the small set of on-demand runtime
 * tables (the $fopen file table) and any remaining heap-owning
 * WideData use, so this is generous.
 */
#ifndef BS_MEM_ARENA_WORDS
#define BS_MEM_ARENA_WORDS (64u * 1024u)
#endif

/* the arena, and the number of words carved from it so far */
static unsigned int arena[BS_MEM_ARENA_WORDS] __attribute__((aligned(16)));
static unsigned int arena_used = 0u;

/* superimpose this structure on freed memory to create a free list
 * structure (blocks are carved in even word counts from a 16-byte
 * aligned arena, so the pointer is properly aligned)
 */
typedef struct tMemHeader tMemHeader;
struct tMemHeader
{
  tMemHeader* next;
};

/* freed blocks of more than MAX_CHUNK_WORDS words carry their size
 * and are reused on an exact size match
 */
typedef struct tLargeHeader tLargeHeader;
struct tLargeHeader
{
  tLargeHeader* next;
  unsigned int  nWords;
};

static tMemHeader*   free_list[MAX_CHUNK_WORDS+1];
static tLargeHeader* large_free_list = NULL;

/* cumulative call counts, for the bs_mem_alloc_counters()
 * test-support accessor
 */
static unsigned long long alloc_count = 0;
static unsigned long long free_count = 0;

/* The unbounded fallback hooks.  Only the BDPI marshaling TU
 * (prim_ops.cxx) defines them; the weak references resolve to NULL
 * everywhere else.  (An undefined weak symbol is legal in the model
 * shared object: the dynamic linker resolves it to NULL.)
 */
extern "C" void* bs_mem_heap_alloc(unsigned int nWords)
  __attribute__((weak));
extern "C" void  bs_mem_heap_free(void* ptr, unsigned int nWords)
  __attribute__((weak));

/* is this pointer inside the arena? */
static bool in_arena(const void* ptr)
{
  const unsigned int* p = (const unsigned int*) ptr;
  return (p >= arena) && (p < arena + BS_MEM_ARENA_WORDS);
}

/* Report arena exhaustion in a model with no heap fallback and stop
 * the process.  Does not return.
 */
static void arena_exhausted(void) __attribute__((noreturn));
static void arena_exhausted(void)
{
  const struct bs_host_ops* ops = bk_host_ops(NULL);
  if (ops != NULL)
  {
    static const char msg[] =
      "Error: Bluesim's fixed storage arena is exhausted; only models "
      "with imported (BDPI) functions carry an unbounded fallback "
      "(see mem_alloc.cxx / BS_MEM_ARENA_WORDS)\n";
    void* ctx = bk_host_ctx(NULL);
    struct bs_host_file* err = ops->std_stream(ctx, BS_HOST_STDERR);
    ops->write(ctx, err, msg, sizeof(msg) - 1);
  }
  __builtin_trap();
}

void init_mem_allocator()
{
  /* all allocator state is constant-initialized static storage;
   * nothing to do
   */
}

void shutdown_mem_allocator()
{
  /* The storage is static, so there is nothing to release; freed
   * blocks stay on the free lists, ready for a re-initialization in
   * the same process.  Live allocations (e.g. the $fopen file
   * table, which outlives one simulation) are unaffected, exactly
   * as they were when this released only the cached free chunks.
   */
}

void* alloc_mem(unsigned int nWords)
{
  /* every block can hold a free-list header once returned */
  static const unsigned int min_words =
    (unsigned int) ((sizeof(tLargeHeader) + sizeof(unsigned int) - 1) /
                    sizeof(unsigned int));

  ++alloc_count;

  if (nWords <= MAX_CHUNK_WORDS)
  {
    if (free_list[nWords] != NULL)
    {
      tMemHeader* ret = free_list[nWords];
      free_list[nWords] = ret->next;
      return (void*) ret;
    }
  }
  else
  {
    /* reuse a freed large block of exactly this size */
    tLargeHeader** link = &large_free_list;
    while (*link != NULL)
    {
      if ((*link)->nWords == nWords)
      {
        tLargeHeader* ret = *link;
        *link = ret->next;
        return (void*) ret;
      }
      link = &((*link)->next);
    }
  }

  /* carve a fresh block from the arena, in even word counts so
   * every block is 8-byte aligned
   */
  unsigned int carve = (nWords < min_words) ? min_words : nWords;
  carve = (carve + 1u) & ~1u;
  if (arena_used + carve <= BS_MEM_ARENA_WORDS)
  {
    void* ret = (void*) (arena + arena_used);
    arena_used += carve;
    return ret;
  }

  /* the arena is exhausted: models with BDPI imports fall back to
   * the program allocator (the documented unbounded path); others
   * stop
   */
  if (bs_mem_heap_alloc != NULL)
    return bs_mem_heap_alloc(nWords);
  arena_exhausted();
}

void free_mem(void* ptr, unsigned int nWords)
{
  ++free_count;
  if (ptr == NULL)
    return;
  if (in_arena(ptr))
  {
    if (nWords <= MAX_CHUNK_WORDS)
    {
      tMemHeader* mem = (tMemHeader*) ptr;
      mem->next = free_list[nWords];
      free_list[nWords] = mem;
    }
    else
    {
      tLargeHeader* mem = (tLargeHeader*) ptr;
      mem->nWords = nWords;
      mem->next = large_free_list;
      large_free_list = mem;
    }
  }
  else if (bs_mem_heap_free != NULL)
  {
    /* a block outside the arena can only have come from the heap
     * fallback
     */
    bs_mem_heap_free(ptr, nWords);
  }
}

extern "C" void bs_mem_alloc_counters(unsigned long long* allocs,
                                      unsigned long long* frees)
{
  if (allocs)
    *allocs = alloc_count;
  if (frees)
    *frees = free_count;
}
