#include <cstdlib>
#include <algorithm>

#include "mem_alloc.h"

#define MAX_CHUNK_WORDS 16

// superimpose this structure on allocated memory to create a free list structure
typedef struct tMemHeader tMemHeader;
struct tMemHeader
{
  tMemHeader* next;
};

static bool is_active = false;
static tMemHeader* free_list[MAX_CHUNK_WORDS+1];

// cumulative call counts, for the bs_mem_alloc_counters()
// test-support accessor
static unsigned long long alloc_count = 0;
static unsigned long long free_count = 0;

void init_mem_allocator()
{
  if (!is_active)
  {
    for (unsigned int i = 0; i < MAX_CHUNK_WORDS; ++i)
      free_list[i] = NULL;
    is_active = true;
  }
}

void shutdown_mem_allocator()
{
  if (is_active)
  {
    for (unsigned int i = 0; i < MAX_CHUNK_WORDS; ++i)
    {
      tMemHeader* mem = free_list[i];
      while (mem)
      {
	tMemHeader* next = mem->next;
	delete[] mem;
	mem = next;
      }
      free_list[i] = NULL;
    }
    is_active = false;
  }
}

void* alloc_mem(unsigned int nWords)
{
  static unsigned int min_words = (sizeof(tMemHeader) + sizeof(unsigned int) - 1) / sizeof(unsigned int);
  ++alloc_count;
  if ((nWords > MAX_CHUNK_WORDS) || (free_list[nWords] == NULL))
  {
    return new unsigned int[std::max(nWords,min_words)];
  }
  else
  {
    tMemHeader* ret = free_list[nWords];
    free_list[nWords] = ret->next;
    return (void*)ret;
  }
}

void free_mem(void* ptr, unsigned int nWords)
{
  ++free_count;
  if (nWords > MAX_CHUNK_WORDS)
    delete[] ((unsigned int*)ptr);
  else if (ptr != NULL)
  {
    tMemHeader* mem = (tMemHeader*) ptr;
    mem->next = free_list[nWords];
    free_list[nWords] = mem;
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
