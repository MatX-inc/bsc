#ifndef __BS_MODEL_H__
#define __BS_MODEL_H__

#include "bluesim_types.h"
#include "bluesim_introspection.h"

/* This is the (pure virtual) base class for Bluesim-generated designs.
 * It declares the functions that the kernel requires from a design.
 */

struct bs_host_ops;

class Model
{
 /* Storage recorded by new_MODEL_*(): the host operations and host
  * context the model performs I/O through, and the caller-provided
  * buffers the model constructs itself into (see the new_MODEL_*
  * contract in bluesim_kernel_api.h).  All five pointers are
  * borrowed; they must stay valid until bk_shutdown().  A sizing
  * call may leave them NULL; bk_sync_init() refuses to construct a
  * model whose required storage is unbound.
  */
 public:
  const struct bs_host_ops* model_ops;
  void* model_ctx;
  void* state_storage;   /* >= get_state_bytes(), max_align_t-aligned */
  void* input_storage;   /* >= get_input_bytes() (may be NULL if 0) */
  void* output_storage;  /* >= get_output_bytes() (may be NULL if 0) */

  void set_storage(const struct bs_host_ops* ops, void* ctx,
                   void* state, void* inputs, void* outputs)
  {
    model_ops = ops;
    model_ctx = ctx;
    state_storage = state;
    input_storage = inputs;
    output_storage = outputs;
  }

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

  /* Non-allocating introspection of the design's state elements and
   * of the top module's input and output ports (see
   * bluesim_introspection.h for the descriptor types and for the
   * documented ordering, alignment and layout rules).  These are
   * static per-design tables emitted by the code generator: the
   * walkers allocate nothing, return pointers to 'static const'
   * storage inside the generated code, and are usable before
   * create_model().  The get_*_element/get_*_port accessors return
   * NULL when the index is out of range; the get_*_bytes accessors
   * return the total byte size of the corresponding planned area.
   */
  virtual tUInt32 get_num_state_elements() = 0;
  virtual const tBkStateInfo* get_state_element(tUInt32 n) = 0;
  virtual tUInt64 get_state_bytes() = 0;
  virtual tUInt64 get_state_elements_offset() = 0;
  virtual tUInt32 get_num_input_ports() = 0;
  virtual const tBkPortInfo* get_input_port(tUInt32 n) = 0;
  virtual tUInt64 get_input_bytes() = 0;
  virtual tUInt32 get_num_output_ports() = 0;
  virtual const tBkPortInfo* get_output_port(tUInt32 n) = 0;
  virtual tUInt64 get_output_bytes() = 0;

 // Require construction be of the derived classes, not this class
 protected:
  Model()
    : model_ops(0), model_ctx(0),
      state_storage(0), input_storage(0), output_storage(0) { };

 // Similarly, prevent use of the copy constructor and the assignment operator
 private:
  Model(Model const & m) { };
  Model & operator= (Model const & m) { return *this; };

 // Declare the destructor as virtual, so that the derived destructor is used
 public:
  virtual ~Model() { };
};

#endif /* __BS_MODEL_H__ */
