#ifndef __BS_PRIM_MOD_REGFILE_H__
#define __BS_PRIM_MOD_REGFILE_H__

#include <string>

#include "bluesim_kernel_api.h"
#include "bs_mem_file.h"
#include "bs_str.h"
#include "bs_module.h"
#include "bs_wide_data.h"
#include "bs_range_tracker.h"
#include "bs_reset.h"

// forward declaration
template<typename AT, typename DT> class MOD_RegFile;

// Handler for binary mem files
template<typename AT, typename DT>
class BinFormatHandler : public FormatHandler
{
 public:
  BinFormatHandler(MOD_RegFile<AT,DT>* reg_file, bool check_ranges,
		   unsigned int addr_width, unsigned int data_width,
		   const AT& range_start, const AT& range_end)
    : FormatHandler(), rf(reg_file),
      addr_bits(addr_width), data_bits(data_width),
      start(range_start), end(range_end), check(check_ranges)
  {
    addr = start;
    decreasing = (start > end);
  }

  virtual ~BinFormatHandler() {}

  virtual tMemFileStatus updateAddress(const char* addr_str)
  {
    if (!parse_hex(&addr, addr_str, addr_bits))
      return MF_BAD_FORMAT;

    if (check && (addr < start || addr > end))
      return MF_OUT_OF_BOUNDS;

    return MF_ACCEPTED;
  }

  virtual tMemFileStatus setEntry(const char* data_str)
  {
    tMemFileStatus status;

    if (addr < start || addr > end)
    {
      status = MF_IGNORED;
    }
    else
    {
      DT value;
      init_val(value, data_bits);
      if (!parse_bin(&value, data_str, data_bits))
	status = MF_BAD_FORMAT;
      else
      {
	rf->METH_upd(addr, value, true);
	status = MF_ACCEPTED;
	rt.setAddr(addr);
      }
    }

    if (decreasing)
      --addr;
    else
      ++addr;

    return status;
  }

  virtual void checkRange(tSimStateHdl simHdl,
			  const char* filename, const char* memname)
  {
    rt.checkRange(simHdl, filename, memname, start, end);
  }
 private:
  MOD_RegFile<AT,DT>* rf;
  unsigned int addr_bits;
  unsigned int data_bits;
  AT start;
  AT end;
  AT addr;
  bool check;
  bool decreasing;
  RangeTracker<AT> rt;
};

// Handler for hex mem files
template<typename AT, typename DT>
class HexFormatHandler : public FormatHandler
{
 public:
  HexFormatHandler(MOD_RegFile<AT,DT>* reg_file, bool check_ranges,
		   unsigned int addr_width, unsigned int data_width,
		   const AT& range_start, const AT& range_end)
    : FormatHandler(), rf(reg_file),
      addr_bits(addr_width), data_bits(data_width),
      start(range_start), end(range_end), check(check_ranges)
  {
    addr = start;
    decreasing = (start > end);
  }

  virtual ~HexFormatHandler() {}

  virtual tMemFileStatus updateAddress(const char* addr_str)
  {
    if (!parse_hex(&addr, addr_str, addr_bits))
      return MF_BAD_FORMAT;

    if (check && (addr < start || addr > end))
      return MF_OUT_OF_BOUNDS;
    else
      return MF_ACCEPTED;
  }

  virtual tMemFileStatus setEntry(const char* data_str)
  {
    tMemFileStatus status;

    if (addr < start || addr > end)
    {
      status = MF_IGNORED;
    }
    else
    {
      DT value;
      init_val(value, data_bits);
      if (!parse_hex(&value, data_str, data_bits))
	status = MF_BAD_FORMAT;
      else
      {
	rf->METH_upd(addr, value, true);
	status = MF_ACCEPTED;
	rt.setAddr(addr);
      }
    }

    if (decreasing)
      --addr;
    else
      ++addr;

    return status;
  }

  virtual void checkRange(tSimStateHdl simHdl,
			  const char* filename, const char* memname)
  {
    rt.checkRange(simHdl, filename, memname, start, end);
  }
 private:
  MOD_RegFile<AT,DT>* rf;
  unsigned int addr_bits;
  unsigned int data_bits;
  AT start;
  AT end;
  AT addr;
  bool check;
  bool decreasing;
  RangeTracker<AT> rt;
};

template<typename AT, typename DT>
const unsigned int* index_rf_fn(void* base, tUInt64 addr);

// This is the definition of the RegFile primitive
template<typename AT, typename DT>
class MOD_RegFile : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[3];
 public:
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage();

    init_symbols();
  }
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      const std::string& memfile,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi, bool bin_format)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage();

    init_from_file(memfile, bin_format);

    init_symbols();
  }
  // as above, with the file name given as a string tree (a
  // concatenation built in the enclosing module's constructor, see
  // bs_str.h): the name is flattened into a stack buffer with
  // C-string semantics for the load (a VLA, see
  // DYNAMIC_VLA_FUNCTIONS)
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      const tStr* memfile,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi, bool bin_format)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage();

    char memfile_buf[bs_str_len(memfile) + 1];
    init_from_file(bs_str_flatten(memfile, memfile_buf), bin_format);

    init_symbols();
  }
  ~MOD_RegFile() { delete_blocks(top_level,0); }

 // shared initialization routines
 private:
  void init_storage()
  {
    last_word = hi_addr - lo_addr;

    // partition address space for sparse storage
    num_levels = (addr_bits + 9) / 10;
    if ((num_levels > 1) && (addr_bits % 10 > 0) && (addr_bits % 10 < 5))
      --num_levels;
    level_bits = new unsigned char[num_levels];
    unsigned int bits_remaining = addr_bits;
    for (unsigned int i = num_levels; i > 0; --i)
    {
      if (bits_remaining < 15)
      {
	level_bits[i-1] = bits_remaining;
	bits_remaining = 0;
      }
      else if (bits_remaining > 16)
      {
	level_bits[i-1] = 10;
	bits_remaining -= 10;
      }
      else
      {
	level_bits[i-1] = 8;
	bits_remaining -= 8;
      }
    }

    // allocate top-level storage block
    top_level = new_block(0);

    // initialize address and data storage
    init_val(upd_addr, addr_bits);
    write_undet(&upd_addr, addr_bits);
    init_val(upd_prev, data_bits);
    write_undet(&upd_prev, data_bits);
  }

  void init_symbols()
  {
    // initialize symbols
    symbol_count = 3;
    symbols = __symbols;

    range.lo = (unsigned long long) lo_addr;
    range.hi = (unsigned long long) hi_addr;
    range.base = (void*) this;
    range.fetch = index_rf_fn<AT,DT>;

    symbols[0].key = "";
    symbols[0].info = SYM_RANGE | data_bits << 4;
    symbols[0].value = (void*)(&range);

    symbols[1].key = "high_addr";
    symbols[1].info = SYM_PARAM | addr_bits << 4;
    symbols[1].value = (void*)(&hi_addr);

    symbols[2].key = "low_addr";
    symbols[2].info = SYM_PARAM | addr_bits << 4;
    symbols[2].value = (void*)(&lo_addr);
  }

  void init_from_file(const std::string& memfile, bool bin_format)
  {
    FormatHandler* reader;
    if (bin_format)
      reader = new BinFormatHandler<AT,DT>(this, true,
					   addr_bits, data_bits,
					   lo_addr, hi_addr);
    else
      reader = new HexFormatHandler<AT,DT>(this, true,
					   addr_bits, data_bits,
					   lo_addr, hi_addr);
    read_mem_file(sim_hdl, memfile.c_str(), inst_name, reader);
  }

  void* new_block(unsigned int level)
  {
    unsigned int nEntries = 1 << level_bits[level];
    if (level == (num_levels - 1))
    {
      DT* data = new DT[nEntries];
      for (unsigned int n = 0; n < nEntries; ++n)
      {
	init_val(data[n], data_bits);
	write_undet(&(data[n]), data_bits);
      }
      return (void*) data;
    }
    else
    {
      void** ptrs = new void*[nEntries];
      for (unsigned int n = 0; n < nEntries; ++n)
	ptrs[n] = NULL;
      return (void*) ptrs;
    }
  }

  void delete_blocks(void* ptr, unsigned int level)
  {
    if (level == (num_levels - 1))
    {
      DT* data = (DT*) ptr;
      delete[] data;
    }
    else
    {
      void** ptrs = (void**) ptr;
      unsigned int nEntries = 1 << level_bits[level];
      for (unsigned int n = 0; n < nEntries; ++n)
      {
	if (ptrs[n] != NULL)
	  delete_blocks(ptrs[n], level+1);
      }
      delete[] ptrs;
    }
  }

  DT* lookup_value(const AT& addr, bool alloc)
  {
    // figure out the target index and which bits of the address to use
    unsigned long long idx = addr - lo_addr;
    unsigned int shift = addr_bits;
    void* ptr = top_level;
    unsigned int level = 0;
    while (true)
    {
      shift -= level_bits[level];
      unsigned int mask = (1 << level_bits[level]) - 1;
      unsigned int n = (idx >> shift) & mask;
      if (level == (num_levels - 1))
      {
	DT* data = (DT*) ptr;
	return &(data[n]);
      }
      else
      {
	void** ptrs = (void**) ptr;
	++level;
	if (ptrs[n] == NULL)
	{
	  if (alloc) ptrs[n] = new_block(level);
	  else return NULL;
	}
	ptr = ptrs[n];
      }
    }
  }

  /* Report an out-of-bounds access through the out_of_bounds host
   * operation.  Does not return.
   */
  BS_HOST_NORETURN void oob_panic(const char* access, const AT& addr) const
  {
    BufferTarget name_buf(sim_hdl, 1024);
    write_name(&name_buf);
    bk_out_of_bounds(sim_hdl, "RegFile", name_buf.str(), access,
                     (tUInt64) addr, (tUInt64) lo_addr, (tUInt64) hi_addr);
  }

 public:
  // Note: there is RegFileWCF variant of RegFile that
  // allows upd before sub, but sub should be able to read the
  // value from the beginning of the cycle.
  const DT METH_sub(const AT& addr)
  {
    if (addr < lo_addr || addr > hi_addr)
    {
      if (any_reset_asserted(sim_hdl))
      {
	// While some reset is asserted, rule bodies execute this read
	// speculatively (before their in-reset check) with the address
	// register possibly still at its undetermined initial pattern,
	// so tolerate the access: silently return an undetermined
	// value, as the pre-panic runtime did (see bs_reset.h).
	DT v;
	init_val(v, data_bits);
	write_undet(&v, data_bits);
	return v;
      }
      oob_panic("Read address", addr);
    }
    else if ((upd_addr == addr) && bk_is_same_time(sim_hdl, upd_at))
    {
      return upd_prev;
    }
    else
    {
      DT* value_ptr = lookup_value(addr, false);
      if (value_ptr != NULL)
      {
	return *value_ptr;
      }
      else
      {
	DT v;
	init_val(v, data_bits);
	write_undet(&v, data_bits);
	return v;
      }
    }
  }
  void METH_upd(const AT& addr, const DT& val, bool immediate = false)
  {
    if (addr < lo_addr || addr > hi_addr)
    {
      if (any_reset_asserted(sim_hdl))
	return; // in-reset carve-out: drop the write silently instead
		// of panicking (lookup_value() must not run: it would
		// alias an out-of-bounds address onto a valid entry)
      oob_panic("Write address", addr);
    }
    DT* value_ptr = lookup_value(addr, true);
    if (value_ptr != NULL)
    {
      if (!immediate)
      {
	upd_at = bk_now(sim_hdl);
	upd_addr = addr;
	upd_prev = *value_ptr;
      }
      *value_ptr = val;
    }
  }

 public:
  const unsigned int* data_index(tUInt64 addr)
  {
    DT* value_ptr = lookup_value(addr, true);
    if (value_ptr != NULL)
      return symbol_value(value_ptr, data_bits);
    else
      return NULL;
  }

 // RegFile data members
 private:
  unsigned int addr_bits;
  unsigned int data_bits;
  AT lo_addr;
  AT hi_addr;
  AT last_word;
  unsigned int num_levels;
  unsigned char* level_bits;
  void* top_level;
  tTime upd_at;
  AT upd_addr;
  DT upd_prev;

  // range structure for symbolic access to RegFile data
  Range range;

 public:
  friend class BinFormatHandler<AT,DT>;
  friend class HexFormatHandler<AT,DT>;
};

// Function to index into RegFile data array
template<typename AT, typename DT>
const unsigned int* index_rf_fn(void* base, tUInt64 addr)
{
  MOD_RegFile<AT,DT>* rf = (MOD_RegFile<AT,DT>*) base;
  return rf->data_index(addr);
}

#endif /* __BS_PRIM_MOD_REGFILE_H__ */
