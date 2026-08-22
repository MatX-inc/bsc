#ifndef __BS_RANGE_TRACKER_H__
#define __BS_RANGE_TRACKER_H__

#include <list>

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

template<typename AT>
class RangeTracker
{
 public:
  RangeTracker() {}
  ~RangeTracker() {}

 public:
  void setAddr (const AT& addr)
    {
      if (rlist.empty()) {
	// create a new entry
	RangeElem<AT> e;
	e.low = addr;
	e.high = addr;
	rlist.push_back(e);
      } else if (addr == (rlist.back().high + 1)) {
	rlist.back().high++;
      } else if (addr == (rlist.back().low - 1)) {
	rlist.back().low--;
      } else {
	// start a new entry
	RangeElem<AT> e;
	e.low = addr;
	e.high = addr;
	rlist.push_back(e);
      }
    }

  void checkRange(tSimStateHdl simHdl,
		  const char* filename, const char* memname,
		  const AT& start, const AT& end)
    {
      if (!rlist.empty()) {
	FileTarget dest(simHdl);
	rlist.sort();

	// one more than the last address seen
	AT next_addr = start;
	// one more than the last overlap reported
	AT next_overlap_addr = start;
	// booleans to tell if the above addresses have rolled over
	bool full = false; // set to true when next_addr passes end
	bool overlap_full = false; // when next_overlap_addr passes end

	typename std::list<RangeElem<AT> >::iterator i = rlist.begin();
	while (i != rlist.end()) {
	  RangeElem<AT> e = *i;
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
	  // next element
	  ++i;
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

	// the list is no longer needed
	rlist.clear();
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
  std::list<RangeElem<AT> > rlist;
};

#endif /* __BS_RANGE_TRACKER_H__ */

