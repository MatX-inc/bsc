#ifndef __BS_PRIM_MOD_REGFILE_H__
#define __BS_PRIM_MOD_REGFILE_H__

#include "bluesim_kernel_api.h"
#include "bs_mem_file.h"
#include "bs_str.h"
#include "bs_module.h"
#include "bs_wide_data.h"
#include "bs_prim_storage.h"
#include "bs_range_tracker.h"
#include "bs_reset.h"

/* aux storage words a wide RegFile needs: the same-cycle read-back
 * value (upd_prev) and the undetermined-value scratch entry */
#define BS_REGFILE_AUX_WORDS(b) (2u * BS_AUX_WORDS(b))

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
    else if (!rf->preload_entry(addr, data_str, true))
    {
      status = MF_BAD_FORMAT;
    }
    else
    {
      status = MF_ACCEPTED;
      rt.setAddr(addr);
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
    else if (!rf->preload_entry(addr, data_str, false))
    {
      status = MF_BAD_FORMAT;
    }
    else
    {
      status = MF_ACCEPTED;
      rt.setAddr(addr);
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

// This is the definition of the RegFile primitive.
//
// The entries live as an eager, flat array in the caller-provided
// element storage claimed at construction (one published entry per
// address, see bluesim_introspection.h); the sparse lazily-allocated
// block tree of earlier runtimes is gone, and constructing, loading
// or accessing a RegFile makes no allocator calls.
template<typename AT, typename DT>
class MOD_RegFile : public Module
{
  // embedded symbol-table storage (bound to Module::symbols;
  // symbol tables never allocate)
 private:
  tSym __symbols[3];
 public:
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      tStateLayout* sto, unsigned int* aux,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage(sto, aux);

    init_symbols();
  }
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      tStateLayout* sto, unsigned int* aux,
	      const char* memfile,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi, bool bin_format)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage(sto, aux);

    init_from_file(memfile, bin_format);

    init_symbols();
  }
  // as above, with the file name given as a string tree (a
  // concatenation built in the enclosing module's constructor, see
  // bs_str.h): the name is flattened into a stack buffer with
  // C-string semantics for the load (a VLA, see
  // DYNAMIC_VLA_FUNCTIONS)
  MOD_RegFile(tSimStateHdl simHdl, const char* name, Module* parent,
	      tStateLayout* sto, unsigned int* aux,
	      const tStr* memfile,
	      unsigned int addr_width, unsigned int data_width,
	      const AT& lo, const AT& hi, bool bin_format)
    : Module(simHdl, name, parent), addr_bits(addr_width),
      data_bits(data_width), lo_addr(lo), hi_addr(hi),
      upd_at(~bk_now(sim_hdl))
  {
    init_storage(sto, aux);

    char memfile_buf[bs_str_len(memfile) + 1];
    init_from_file(bs_str_flatten(memfile, memfile_buf), bin_format);

    init_symbols();
  }

 // shared initialization routines
 private:
  void init_storage(tStateLayout* sto, unsigned int* aux)
  {
    n_entries = ((tUInt64)(hi_addr - lo_addr)) + 1llu;
    data.bind(sto->claim(), data_bits);
    data.init_undet(n_entries);

    // initialize address and data storage
    init_val(upd_addr, addr_bits);
    write_undet(&upd_addr, addr_bits);
    bs_bind_aux(upd_prev, &aux, data_bits);
    write_undet(&upd_prev, data_bits);
    bs_bind_aux(undet_val, &aux, data_bits);
    write_undet(&undet_val, data_bits);
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

  void init_from_file(const char* memfile, bool bin_format)
  {
    // the handlers live on the stack for the duration of the read
    if (bin_format)
    {
      BinFormatHandler<AT,DT> reader(this, true, addr_bits, data_bits,
				     lo_addr, hi_addr);
      read_mem_file(sim_hdl, memfile, inst_name, &reader);
    }
    else
    {
      HexFormatHandler<AT,DT> reader(this, true, addr_bits, data_bits,
				     lo_addr, hi_addr);
      read_mem_file(sim_hdl, memfile, inst_name, &reader);
    }
  }

  /* Report an out-of-bounds access through the out_of_bounds host
   * operation.  Does not return.
   */
  BS_HOST_NORETURN void oob_panic(const char* access, const AT& addr) const
  {
    char name_store[1024 + 1];
    BufferTarget name_buf(sim_hdl, name_store, 1024);
    write_name(&name_buf);
    bk_out_of_bounds(sim_hdl, "RegFile", name_buf.str(), access,
                     (tUInt64) addr, (tUInt64) lo_addr, (tUInt64) hi_addr);
  }

 public:
  /* parse one preloaded entry directly into its element storage
   * (used by the mem-file format handlers; the address has already
   * been checked against [lo_addr, hi_addr]) */
  bool preload_entry(const AT& addr, const char* data_str, bool bin_format)
  {
    auto&& entry = data.ref((tUInt64)(addr - lo_addr));
    if (bin_format)
      return parse_bin(&entry, data_str, data_bits);
    else
      return parse_hex(&entry, data_str, data_bits);
  }

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
	return bs_value_view(undet_val, data_bits);
      }
      oob_panic("Read address", addr);
    }
    else if ((upd_addr == addr) && bk_is_same_time(sim_hdl, upd_at))
    {
      return bs_value_view(upd_prev, data_bits);
    }
    return data.get((tUInt64)(addr - lo_addr));
  }
  void METH_upd(const AT& addr, const DT& val, bool immediate = false)
  {
    if (addr < lo_addr || addr > hi_addr)
    {
      if (any_reset_asserted(sim_hdl))
	return; // in-reset carve-out: drop the write silently instead
		// of panicking
      oob_panic("Write address", addr);
    }
    auto&& entry = data.ref((tUInt64)(addr - lo_addr));
    if (!immediate)
    {
      upd_at = bk_now(sim_hdl);
      upd_addr = addr;
      upd_prev = entry;
    }
    entry = val;
  }

 public:
  const unsigned int* data_index(tUInt64 addr)
  {
    if ((addr < (tUInt64) lo_addr) || (addr > (tUInt64) hi_addr))
      return NULL;
    return data.sym_value(addr - (tUInt64) lo_addr);
  }

 // RegFile data members
 private:
  unsigned int addr_bits;
  unsigned int data_bits;
  AT lo_addr;
  AT hi_addr;
  tUInt64 n_entries;
  tStateArray<DT> data;  // flat entries, in caller-provided storage
  tTime upd_at;
  AT upd_addr;
  DT upd_prev;           // aux-bound when wide
  DT undet_val;          // aux-bound when wide

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
