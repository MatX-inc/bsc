#ifndef __BS_MODEL_H__
#define __BS_MODEL_H__

#include "bluesim_types.h"

/* This is the (pure virtual) base class for Bluesim-generated designs.
 * It declares the functions that the kernel requires from a design.
 */

class Model
{
 // The functions that the kernel requires, declared as pure virtual
 // functions so that derived classes have to define them.
 public:
  virtual void create_model(tSimStateHdl simHdl, bool master) = 0;
  virtual void destroy_model() = 0;
  virtual void reset_model(bool asserted) = 0;
  virtual void get_version(char const **name, char const **build) = 0;
  virtual time_t get_creation_time() = 0;
  virtual void * get_instance() = 0;

  /* The maximum number of events this design can have live in the
   * kernel's event queue at any one time, ASSUMING NO HOST CALLS THAT
   * ENQUEUE EVENTS.  This is a static per-design constant computed by
   * the code generator from the clocks and reset primitives the
   * generated model registers (the formula is documented at the
   * computation, in SimBlocksToC).  Host calls that enqueue events
   * (bk_quit_at, bk_schedule_ui_event, host-invoked
   * bk_trigger_clock_edge, bk_enable_cycle_dumping, ...) come on top
   * of this bound; see bluesim_kernel_api.h for the per-call costs.
   */
  virtual tUInt32 get_max_event_queue_depth() = 0;

 // Require construction be of the derived classes, not this class
 protected:
  Model() { };

 // Similarly, prevent use of the copy constructor and the assignment operator
 private:
  Model(Model const & m) { };
  Model & operator= (Model const & m) { return *this; };

 // Declare the destructor as virtual, so that the derived destructor is used
 public:
  virtual ~Model() { };
};

#endif /* __BS_MODEL_H__ */
