#include <utility>
#include <cstdint>

#include "bs_target.h"
#include "event_queue.h"

/* we need these for the debugging routines only */
#include "priority.h"
extern "C" const char* bk_clock_name(tSimStateHdl simHdl, tClock handle);

/* the noreturn overflow report (kernel.cxx), called when an event is
 * scheduled into a full queue
 */
extern "C" void bk_event_queue_overflow(tSimStateHdl simHdl,
                                        tUInt32 capacity)
#if defined(__GNUC__)
    __attribute__((noreturn))
#endif
    ;


/* Fundamental heap operations */

#define PARENT(n) ((n-1)/2)
#define LEFT(n)   ((2*n)+1)
#define RIGHT(n)  ((2*n)+2)

/* Comparison function for ordering events */
bool operator<(const tEvent& e1, const tEvent& e2)
{
  if (e1.at < e2.at)
    return true;
  else if (e1.at > e2.at)
    return false;
  else
    return (e1.priority < e2.priority);
}

/* Move an element in the heap up until
 * the heap property is restored.
 */
void EventQueue::bubble_up(unsigned int idx)
{
  unsigned int current = idx;
  unsigned int parent = PARENT(current);

  while ((current != 0) && (events[current] < events[parent]))
  {
    std::swap<tEvent>(events[current],events[parent]);
    current = parent;
    parent = PARENT(current);
  }
}

/* Move an element in the heap down until
 * the heap property is restored.
 */
void EventQueue::bubble_down(unsigned int idx)
{
  unsigned int parent = idx;
  unsigned int left;
  unsigned int right;

  while (true)
  {
    left  = LEFT(parent);
    right = RIGHT(parent);

    // find which node is the smallest out of parent and both children
    unsigned int smallest = parent;
    if ((right < count) && (events[right] < events[smallest]))
      smallest = right;
    if ((left < count) && (events[left] < events[smallest]))
      smallest = left;

    if (smallest == parent)
    {
      // we can stop bubbling down now -- the heap property holds
      return;
    }
    else
    {
      // we must swap with the smallest child and continue the loop
      std::swap<tEvent>(events[parent],events[smallest]);
      parent = smallest;
    }
  }
}

/* Check if the heap property holds */
bool EventQueue::isValid()
{
  for (unsigned int i = 0; i < count; ++i)
  {
    unsigned int l = LEFT(i);
    unsigned int r = RIGHT(i);
    if ((l < count) && (events[l] < events[i]))
      return false;
    if ((r < count) && (events[r] < events[i]))
      return false;
  }
  return true;
}

/*
 * The event queue operations
 */

/* Construct an EventQueue with a fixed capacity over caller-provided
 * storage of 'queue_capacity' tEvent slots.  The queue allocates
 * nothing, the storage never grows (see schedule()), and the caller
 * keeps ownership of it.
 */
EventQueue::EventQueue(tSimStateHdl simHdl, unsigned int queue_capacity,
                       tEvent* storage)
  : sim_hdl(simHdl), events(storage), capacity(queue_capacity),
    count(0), max_count(0), in_event(false), halted(false),
    last_find_pred(NULL), curr_find_idx(0)
{}

/* Destroy an EventQueue.  Its storage belongs to the caller and is
 * not freed here.
 */
EventQueue::~EventQueue()
{}

/* Add an event to the queue.  The queue never grows past the
 * capacity fixed at construction: scheduling into a full queue is a
 * fatal condition reported through the host's event_queue_overflow
 * operation, which does not return.
 */
void EventQueue::schedule(const tEvent& e)
{
  if (count == capacity)
    bk_event_queue_overflow(sim_hdl, capacity); /* does not return */
  events[count] = e;
  bubble_up(count++);
  if (count > max_count)
    max_count = count;
}

/* Execute events in sequence */
void EventQueue::execute(tSimStateHdl simHdl)
{
  halted = false;
  while ((count > 0) && !halted)
  {
    // We must copy the event rather than passing a reference
    // to the first event on the queue, since the event function
    // may schedule additional events and modify the queue.
    executing_event = events[0];

    // Remove the event from the queue
    if (--count > 0)
    {
      events[0] = events[count];
      bubble_down(0);
    }

    // Execute the event fn, passing in the copy of the event struct
    removed_self = false;
    in_event = true;
    tTime t = executing_event.fn(simHdl, executing_event);
    in_event = false;

    // If the event fn returned a non-zero time and did not
    // remove itself, then reschedule the event for that number
    // of time units in the future.
    if ((t != 0llu) && !removed_self)
    {
      executing_event.at += t;
      schedule(executing_event);
    }
  }
}

/* Stop the current execute() loop after the current event */
void EventQueue::halt()
{
  halted = true;
}

/* Get the number of events in the queue */
unsigned int EventQueue::size() const
{
  return count;
}

/* Get the most events the queue has ever held at once */
unsigned int EventQueue::high_water() const
{
  return max_count;
}

/* Search the queue for a matching event.
 * When a match is found, a pointer to the event is returned.
 * When there is no match, a NULL pointer is returned.
 * The search starts from the beginning when a predicate is
 * provided, and continues from the point of the last match
 * when the predicate is NULL.
 */
const tEvent* EventQueue::find(tSimStateHdl simHdl, tEventPredicate pred) const
{
  if (pred != NULL)
  {
    // start a new search
    last_find_pred = pred;
    curr_find_idx = 0;
  }

  if (last_find_pred != NULL)
  {
    const tEvent* ptr = NULL;
    while (curr_find_idx < count)
    {
      if (last_find_pred(simHdl, events[curr_find_idx]))
	ptr = &(events[curr_find_idx]);
      ++curr_find_idx;
      if (ptr)
	return (ptr);
    }
  }

  return NULL;
}

/* remove all events satisfying a predicate */
void EventQueue::remove(tSimStateHdl simHdl, tEventPredicate pred)
{
  if (pred == NULL)
    return;

  if (in_event)
    removed_self = pred(simHdl, executing_event);

  unsigned int i = 0;
  while (i < count)
  {
    if (pred(simHdl, events[i]))
    { // remove this event
      if (--count > 0)
      {
        events[i] = events[count];
	bubble_down(i);
        bubble_up(i);
      }
    }
    else
    { // keep this one
      ++i;
    }
  }
}

/* Remove all events (the fixed storage is retained) */
void EventQueue::clear()
{
  count = 0;
}

/* helper for EventQueue::print: write a pointer like printf's %p */
static void write_pointer(Target& dest, const void* ptr)
{
  if (ptr == NULL)
  {
    dest.write_string("(nil)");
    return;
  }
  dest.write_string("0x");
  dest.write_hex((tUInt64)(uintptr_t)ptr);
}

/* Print the event queue contents (for debugging) */
void EventQueue::print(tSimStateHdl simHdl) const
{
  FileTarget dest(simHdl);
  dest.write_string("Event queue:\n");
  for (unsigned int i = 0; i < count; ++i)
  {
    dest.write_string("  ");
    write_pointer(dest, (const void*)events[i].fn);
    dest.write_char('(');
    write_pointer(dest, events[i].data.ptr);
    dest.write_string(") @ ");
    dest.write_decimal(events[i].at);
    dest.write_char(' ');
    dest.write_string(priority_group_name(priority_group(events[i].priority)));
    dest.write_char(' ');
    dest.write_string(priority_slot_name(priority_slot(events[i].priority)));
    dest.write_char(' ');
    dest.write_string(bk_clock_name(simHdl, priority_clock(events[i].priority)));
    dest.write_char('\n');
  }
}
