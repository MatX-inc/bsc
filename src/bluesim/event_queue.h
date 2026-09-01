#ifndef __EVENT_QUEUE_H__
#define __EVENT_QUEUE_H__

#include "bluesim_types.h"
#include "priority.h"

// Forward declaration of the recursive type tEvent
typedef struct tEvent tEvent;

// A pending reset call, embedded directly in its event (see
// reset.cxx) so that scheduling a reset allocates nothing.
typedef struct
{
  tResetFn fn;
  void*    parent;
  tUInt8   rst;
} tResetRequest;

// A tEventData is a union type used to pass data to an event
// handler function.
typedef union
{
  void*         ptr;
  bool          flag;
  tClock        clk;
  unsigned int  value;
  tResetRequest reset;
} tEventData;

// A tEventFn is the type of functions which can be scheduled for
// execution at a given time.  The tEventData argument is a data
// value associated with the event.  The return value is
// an interval at which to reschedule the event, or 0 to not
// reschedule.
typedef tTime (*tEventFn)(tSimStateHdl, tEvent&);

// A tEvent combines a time and priority with a callback function and
// a void* to some data which is supplied to the callback function.
struct tEvent
{
  tTime      at;
  tPriority  priority;
  tEventFn   fn;
  tEventData data;
};

// Type of predicate function used for searching and filtering
// an event queue.
typedef bool (*tEventPredicate)(tSimStateHdl, const tEvent&);

// An EventQueue provides a simple priority queue interface.
//
// The queue has a fixed capacity, chosen by the host at
// bk_sync_init(), and its storage lives in the host-provided context
// buffer (the queue allocates nothing and NEVER grows).  Scheduling
// an event into a full queue reports the fatal condition through the
// host's event_queue_overflow operation (via
// bk_event_queue_overflow), which does not return.
class EventQueue
{
 private:
  tSimStateHdl        sim_hdl;
  tEvent*             events;   // fixed storage of 'capacity' slots,
                                // provided by the caller and borrowed
                                // for the queue's lifetime
  unsigned int        capacity;
  unsigned int        count;
  unsigned int        max_count; // high-water mark of 'count'

  bool                in_event;
  tEvent              executing_event;
  bool                removed_self;
  bool                halted;

  // queue search state
  mutable tEventPredicate last_find_pred;
  mutable unsigned int curr_find_idx;

 public:
  EventQueue(tSimStateHdl simHdl, unsigned int queue_capacity,
             tEvent* storage);
  ~EventQueue();

  // schedule an event (calls the noreturn overflow hook when full)
  void schedule(const tEvent& e);

  // execute events in sequence
  void execute(tSimStateHdl simHdl);

  // stop the current execute() loop after the current event
  void halt();

  // get the number of events in the queue
  unsigned int size() const;

  // get the most events the queue has ever held at once
  // (for validating capacity budgets in tests)
  unsigned int high_water() const;

  // find an event satisfying the predicate in the queue
  // Note: giving a NULL predicate repeats the previous
  // search to find the next match, if any.  Matches are
  // *NOT* guaranteed to be returned in order.
  const tEvent* find(tSimStateHdl simHdl, tEventPredicate pred) const;

  // remove all events satisfying a predicate
  void remove(tSimStateHdl simHdl, tEventPredicate pred);

  // remove all events
  void clear();

  // debugging utility function
  void print(tSimStateHdl simHdl) const;

 private: // heap maintenance functions
  bool isValid();
  void bubble_up(unsigned int idx);
  void bubble_down(unsigned int idx);
};

#endif /* __EVENT_QUEUE_H__ */
