#ifndef __BS_RANGE_TRACKER_H__
#define __BS_RANGE_TRACKER_H__

#include <algorithm>

#include "bs_target.h"

template<typename AT>
struct RangeElem
{
  AT low;
  AT high;
};

template<typename AT>
bool operator< (const RangeElem<AT> & x, const RangeElem<AT> & y)
{
  if (x.low < y.low) {
    return true;
  } else if (x.low == y.low) {
    return (x.high < y.high);
  } else {
    return false;
  }
}

/* The number of distinct address ranges a RangeTracker can record.
 * The tracker exists only to warn about gaps and duplicates in
 * memory files (see checkRange), so the ranges live in a fixed array
 * inside the memory primitive instead of a heap container.  A
 * memory file so scattered that it produces more disjoint ranges
 * than this stops being tracked: checkRange then reports once that
 * the gap and duplicate checks were skipped (the file's values are
 * loaded either way; only the warnings are affected).
 */
#define BS_RANGE_TRACKER_MAX_RANGES 64u

template<typename AT>
class RangeTracker
{
 public:
  RangeTracker() : count(0), overflowed(false) {}
  ~RangeTracker() {}

 public:
  void setAddr (const AT& addr)
    {
      if (overflowed) {
	// out of fixed storage: tracking has stopped
	return;
      }
      if (count == 0) {
	// create a new entry
	ranges[count].low = addr;
	ranges[count].high = addr;
	++count;
      } else if (addr == (ranges[count-1].high + 1)) {
	ranges[count-1].high++;
      } else if (addr == (ranges[count-1].low - 1)) {
	ranges[count-1].low--;
      } else if (count < BS_RANGE_TRACKER_MAX_RANGES) {
	// start a new entry
	ranges[count].low = addr;
	ranges[count].high = addr;
	++count;
      } else {
	// no room for a new entry: stop tracking; checkRange
	// reports the skipped checks once
	overflowed = true;
      }
    }

  void checkRange(tSimStateHdl simHdl,
		  const char* filename, const char* memname,
		  const AT& start, const AT& end)
    {
      if (overflowed) {
	FileTarget dest(simHdl);
	dest.write_string("Warning: file '");
	dest.write_string(filename);
	dest.write_string("' for memory '");
	dest.write_string(memname);
	dest.write_string("' has more than ");
	dest.write_decimal((unsigned long long) BS_RANGE_TRACKER_MAX_RANGES);
	dest.write_string(" address ranges; "
			  "gap and duplicate checks were skipped.\n");
	count = 0;
	overflowed = false;
	return;
      }

      if (count != 0) {
	FileTarget dest(simHdl);
	std::sort(ranges, ranges + count);

	// one more than the last address seen
	AT next_addr = start;
	// one more than the last overlap reported
	AT next_overlap_addr = start;
	// booleans to tell if the above addresses have rolled over
	bool full = false; // set to true when next_addr passes end
	bool overlap_full = false; // when next_overlap_addr passes end

	for (unsigned int i = 0; i < count; ++i) {
	  RangeElem<AT> e = ranges[i];
	  if ((e.low < next_addr) || full) {
	    // overlap
	    AT overlap_low = e.low;
	    AT overlap_high =
	      ((e.high < next_addr) || full) ? e.high : next_addr - 1 ;
	    // only report overlap that hasn't been reported yet
	    if (!overlap_full && (overlap_high >= next_overlap_addr)) {
	      if (overlap_low < next_overlap_addr) {
		overlap_low = next_overlap_addr;
	      }
	      if (overlap_low == overlap_high) {
		report(dest, filename, memname, "duplicate values for address");
		dest.write_decimal((unsigned long long)overlap_low);
		dest.write_string(".\n");
	      } else {
		report(dest, filename, memname, "duplicate values for addresses");
		dest.write_decimal((unsigned long long)overlap_low);
		dest.write_string(" to ");
		dest.write_decimal((unsigned long long)overlap_high);
		dest.write_string(".\n");
	      }
	      next_overlap_addr = overlap_high + 1;
	      if (overlap_high == end) {
		overlap_full = true;
	      }
	    }
	  } else if (e.low > next_addr) {
	    // gap
	    if (next_addr == e.low - 1) {
	      report(dest, filename, memname, "a gap at address");
	      dest.write_decimal((unsigned long long)next_addr);
	      dest.write_string(".\n");
	    } else {
	      report(dest, filename, memname, "a gap at addresses");
	      dest.write_decimal((unsigned long long)next_addr);
	      dest.write_string(" to ");
	      dest.write_decimal((unsigned long long)(e.low - 1));
	      dest.write_string(".\n");
	    }
	  }
	  if (e.high >= next_addr) {
	    next_addr = e.high + 1;
	    if (e.high == end) {
	      full = true;
	    }
	  }
	}

	if (!full) {
	  if (next_addr == end) {
	    report(dest, filename, memname, "a gap at address");
	    dest.write_decimal((unsigned long long) next_addr);
	    dest.write_string(".\n");
	  } else {
	    report(dest, filename, memname, "a gap at addresses");
	    dest.write_decimal((unsigned long long)next_addr);
	    dest.write_string(" to ");
	    dest.write_decimal((unsigned long long)end);
	    dest.write_string(".\n");
	  }
	}

	// the ranges are no longer needed
	count = 0;
      }
    }

 private:
  // write "Warning: file '<filename>' for memory '<memname>' has <what> "
  static void report(Target& dest, const char* filename,
		     const char* memname, const char* what)
    {
      dest.write_string("Warning: file '");
      dest.write_string(filename);
      dest.write_string("' for memory '");
      dest.write_string(memname);
      dest.write_string("' has ");
      dest.write_string(what);
      dest.write_char(' ');
    }

 private:
  RangeElem<AT> ranges[BS_RANGE_TRACKER_MAX_RANGES];
  unsigned int  count;
  bool          overflowed;
};

#endif /* __BS_RANGE_TRACKER_H__ */
