#ifndef __MEM_ALLOC_H__
#define __MEM_ALLOC_H__

// call before any alloc_mem/free_mem
void init_mem_allocator();

// call after all alloc_mem/free_mem
void shutdown_mem_allocator();

void* alloc_mem(unsigned int nWords);

void free_mem(void* ptr, unsigned int nWords);

// Test-support accessor: report the cumulative number of alloc_mem()
// and free_mem() calls made so far (freelist hits included).  Used by
// the testsuite to demonstrate that steady-state evaluation makes no
// allocator calls; not part of the public Bluesim API.
extern "C" void bs_mem_alloc_counters(unsigned long long* allocs,
                                      unsigned long long* frees);

#endif /* __MEM_ALLOC_H__ */
