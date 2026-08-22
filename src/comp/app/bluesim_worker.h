#ifndef __BLUESIM_WORKER_H__
#define __BLUESIM_WORKER_H__

/* Host-side simulation worker thread for a dynamically-loaded
 * Bluesim model.
 *
 * This code used to live in the Bluesim kernel (kernel.cxx and
 * portability.cxx), where bk_init() started a simulation thread and
 * bk_advance() / bk_is_running() / bk_sync() / bk_abort_now() /
 * bk_shutdown() controlled it.  The kernel now executes simulation
 * events on the caller's thread through its synchronous API
 * (bk_sync_init() / bk_sync_run() / ...), and this module recreates
 * the old threaded behavior on top of that API, so that bluetcl's
 * 'sim run async', 'sim stop', Ctrl-C handling, etc. work as before.
 *
 * The kernel entry points are passed in as function pointers, because
 * the model is a shared object opened with dlopen() and its entry
 * points are found with dlsym().
 */

#if __cplusplus
extern "C" {
#endif

/* Handle to the simulation state inside the loaded model
 * (the kernel's tSimStateHdl, opaque on this side).
 */
typedef void* tBkSimStateHdl;

/* Kernel API entry points looked up in the model shared object.
 * The types mirror the declarations in bluesim_kernel_api.h
 * (tStatus = int, tBool = unsigned char, tTime = unsigned long long).
 */
typedef int (*tBkSyncRunFn)(tBkSimStateHdl simHdl);            /* bk_sync_run */
typedef void (*tBkAbortNowFn)(tBkSimStateHdl simHdl);          /* bk_abort_now */
typedef void (*tBkShutdownFn)(tBkSimStateHdl simHdl);          /* bk_shutdown */
typedef unsigned long long (*tBkNowFn)(tBkSimStateHdl simHdl); /* bk_now */

/* Handle to a simulation worker */
typedef struct tBluesimWorker* tBluesimWorkerHdl;

/* Status values, matching the kernel's BK_ERROR/BK_SUCCESS */
#define BW_ERROR   (-1)
#define BW_SUCCESS   0

/* Create the worker for a simulation state handle obtained from
 * bk_sync_init(), start its simulation thread and wait for the
 * thread to block waiting for work.  The thread also installs
 * SIGINT/SIGPIPE handlers which abort the simulations of all live
 * workers in the process.
 *
 * Returns a handle to the worker, or NULL on error.
 */
tBluesimWorkerHdl bluesim_worker_create(tBkSimStateHdl simHdl,
                                        tBkSyncRunFn   sync_run,
                                        tBkAbortNowFn  abort_now,
                                        tBkShutdownFn  shutdown,
                                        tBkNowFn       now);

/* Execute simulation events on the worker thread until none remain,
 * simulation is interrupted, or a stopping condition (time limit,
 * etc.) is encountered.
 *
 * When called with an async argument of 0, it will not return until
 * the simulation has stopped.  When called with a non-zero argument
 * it will return immediately, and bluesim_worker_sync() and
 * bluesim_worker_is_running() should be used to synchronize with the
 * simulation thread.
 *
 * Returns BW_ERROR on error and BW_SUCCESS on success.
 */
int bluesim_worker_advance(tBluesimWorkerHdl wrk, unsigned char async);

/* Test if the simulation thread is still running.
 *
 * Returns 0 if the thread is not running and non-zero if
 * the thread is running.
 */
unsigned char bluesim_worker_is_running(tBluesimWorkerHdl wrk);

/* Wait for a simulation started using bluesim_worker_advance() in
 * async mode to complete.
 *
 * Returns the simulation time at which execution stopped (as
 * reported by bk_now(), so scaled by any timescale factor).
 */
unsigned long long bluesim_worker_sync(tBluesimWorkerHdl wrk);

/* Abort the simulation (from outside, Ctrl-C, SIGPIPE, etc.);
 * it stops at the end of the current simulation cycle.
 */
void bluesim_worker_abort_now(tBluesimWorkerHdl wrk);

/* End the worker thread, shut down the kernel (bk_shutdown()) and
 * free the resources controlled by the worker.  After this call the
 * worker handle is no longer valid.
 */
void bluesim_worker_shutdown(tBluesimWorkerHdl wrk);

#if __cplusplus
} /* extern "C" */
#endif

#endif /* __BLUESIM_WORKER_H__ */
