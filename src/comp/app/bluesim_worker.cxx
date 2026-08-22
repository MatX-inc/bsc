/* Host-side simulation worker thread for a dynamically-loaded
 * Bluesim model.  See bluesim_worker.h for an overview.
 *
 * The thread management, semaphores and signal handling here were
 * moved (nearly verbatim) from the Bluesim kernel's kernel.cxx and
 * portability.cxx; the difference is that instead of operating the
 * kernel's event queue directly, the simulation thread now drives it
 * through the kernel's synchronous API, received as function
 * pointers.
 */

#include <list>
#include <cstdio>
#include <cstdlib>

#include <pthread.h>
#include <signal.h>
#include <semaphore.h>
#if __APPLE__
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#endif
#include <errno.h>

#include "bluesim_worker.h"

/* portable semaphore facade (moved from the kernel's portability.cxx) */

#ifdef __APPLE__
#define USE_NAMED_SEMAPHORES 1
#else
#define USE_NAMED_SEMAPHORES 0
#endif

typedef sem_t tSemaphore;

#if USE_NAMED_SEMAPHORES

/*
 * Implementation using named semaphores.
 */

static tSemaphore* create_semaphore()
{
  // Since multiple bluesim models may be running at the same time, we need
  // to use a name that is unique to this process.
  char* name;
  asprintf(&name, "/bsim_arbitrary_sem_name_%05d", getpid());
  // Remove the semaphore if it already exists.  It should not exist, but just
  // in case it does, we want to remove it so that we can create it.
  sem_unlink(name);
  // create the semaphore
  tSemaphore* semaphore = sem_open( name
                                  , O_CREAT | O_EXCL
                                  , S_IRUSR | S_IWUSR
                                  , 0 );
  if (semaphore == SEM_FAILED)
  {
    perror("sem_open");
    semaphore = NULL;
  }
  // Unlink the semaphore to get rid of the name.  The underlying semaphore will
  // continue to exist as long as there are open handles to it, but we don't
  // wan't to keep the name around.
  sem_unlink(name);
  free(name);
  return semaphore;
}

static void release_semaphore(tSemaphore* semaphore)
{
  if (semaphore == NULL) return;
  sem_close(semaphore);
}

#else /* USE_NAMED_SEMAPHORES */

/*
 * Implementation using unnamed semaphores.
 */

static tSemaphore* create_semaphore()
{
  // allocate semaphore struct
  tSemaphore* semaphore = (tSemaphore*) malloc(sizeof(tSemaphore));
  if (semaphore == NULL)
  {
    perror("malloc");
    return NULL;
  }

  // initialize the semaphore
  if (sem_init(semaphore, 0, 0) != 0)
  {
    perror("sem_init");
    free(semaphore);
    return NULL;
  }

  return semaphore;
}

static void release_semaphore(tSemaphore* semaphore)
{
  if (semaphore == NULL) return;
  sem_destroy(semaphore);
  free(semaphore);
}

#endif /* USE_NAMED_SEMAPHORES */

/*
 * Common implementation for both named and unnamed semaphores.
 */

static void post_semaphore(tSemaphore* semaphore)
{
  if (semaphore != NULL)
    sem_post(semaphore);
}

static void trywait_on_semaphore(tSemaphore* semaphore)
{
  if (semaphore == NULL) return;

  while ((sem_trywait(semaphore) != 0) && (errno == EINTR)) {} ;
}

static void wait_on_semaphore(tSemaphore* semaphore)
{
  if (semaphore == NULL) return;

  while ((sem_wait(semaphore) != 0) && (errno == EINTR)) {};
}

/*
 * Worker state (the thread-related fields moved from the kernel's
 * tSimState, plus the kernel handle and entry points the moved code
 * used to reach directly).
 */
struct tBluesimWorker {
  // handle to the simulation state in the loaded model
  tBkSimStateHdl sim_hdl;

  // kernel API entry points looked up in the model shared object
  tBkSyncRunFn  sync_run;
  tBkAbortNowFn abort_now;
  tBkShutdownFn shutdown;
  tBkNowFn      now;

  // semaphores, etc. used for synchronization between API and sim_thread
  volatile bool sim_running;
  volatile bool sim_shutting_down;
  tSemaphore* start_semaphore; /* used to trigger simulation start */
  tSemaphore* stop_semaphore;  /* used to indicate simulation stop */
  pthread_mutex_t sim_mutex;   /* used to protect sim_running, etc. */
  pthread_t sim_thread_id;
};

/* mutex operations */

static void lock_sim_state(tBluesimWorkerHdl wrk)
{
  if (pthread_mutex_lock(&(wrk->sim_mutex)) != 0)
    perror("lock_sim_state()");
}

static void unlock_sim_state(tBluesimWorkerHdl wrk)
{
  if (pthread_mutex_unlock(&(wrk->sim_mutex)) != 0)
    perror("unlock_sim_state()");
}

/* Stop the simulation thread until told to restart */
static void pause_sim(tBluesimWorkerHdl wrk)
{
  fflush(NULL); /* flush open file buffers */

  /* The stop_semaphore is used to indicate when simulation
   * stops.  It is posted here and can be waited on in
   * wait_for_sim_stop().
   */
  post_semaphore(wrk->stop_semaphore);

  /* The start_semaphore is used to control simulation wake-up.
   * It is waited on here and posted in bluesim_worker_advance(), so
   * that we know the simulation will wake-up exactly once for
   * each call to bluesim_worker_advance().
   */
  wait_on_semaphore(wrk->start_semaphore);
}

/* Wait for the simulation thread to stop in pause_sim().
 * This is a hard-wait and should only be called when we know
 * the simulation thread is actually running (or going to run).
 */
static void wait_for_sim_stop(tBluesimWorkerHdl wrk)
{
  /* Wait for the simulation thread to stop */
  wait_on_semaphore(wrk->stop_semaphore);

  fflush(NULL); /* flush open file buffers */
}

/*
 * SIGINT handler which triggers a simulation stop via
 * bluesim_worker_abort_now()
 */

// list of sims that have registered their interest in the signal
// (use a list, so that iterators are still valid after an erase)
static std::list<tBluesimWorkerHdl> abort_watchers;

static void abort_handler(int /* unused */)
{
  std::list<tBluesimWorkerHdl>::iterator it;
  for (it = abort_watchers.begin(); it != abort_watchers.end(); it++)
    bluesim_worker_abort_now(*it);
}

static void add_abort_watcher(tBluesimWorkerHdl wrk)
{
  abort_watchers.push_back(wrk);
}

static void remove_abort_watcher(tBluesimWorkerHdl wrk)
{
  std::list<tBluesimWorkerHdl>::iterator it;
  // list iterators are valid after an erase, except for the erased iterator;
  // therefore, a for-loop is OK, but do the increment before erasing
  for (it = abort_watchers.begin(); it != abort_watchers.end(); ) {
    if ((*it) == wrk) {
      std::list<tBluesimWorkerHdl>::iterator next_it = it;
      next_it++;
      abort_watchers.erase(it);
      it = next_it;
    } else {
      it++;
    }
  }
}

/*
 * Simulation thread which handles the actual simulation event queue
 * operations.  It communicates with the rest of the API through
 * semaphore operations in pause_sim, bluesim_worker_advance and
 * wait_for_sim_stop.
 */
static void* sim_thread(void* ptr)
{
  tBluesimWorker* wrk = (tBluesimWorker*)ptr;

  if (wrk == NULL)
    return NULL;

  /* install signal handlers to shut down simulation */
  struct sigaction sa;
  sa.sa_flags = 0;
  sa.sa_handler = abort_handler;
  sigemptyset(&sa.sa_mask);
  /* SIGINT (user types Ctrl-C) */
  sigaction(SIGINT, &sa, NULL);
  /* SIGPIPE (usually stdout piped to a program that exits, eg /usr/bin/head) */
  sigaction(SIGPIPE, &sa, NULL);

  /* add this sim to the signal watch list */
  add_abort_watcher(wrk);

  while (!wrk->sim_shutting_down)
  {
    /* yield to the UI and wait for a trigger to execute */
    lock_sim_state(wrk);
    wrk->sim_running = false;
    unlock_sim_state(wrk);
    pause_sim(wrk);

    /* execute the events in the simulation queue; bk_sync_run()
     * returns when a stopping condition is encountered or the queue
     * drains (the yield events which used to pause this thread
     * inside the queue now end the bk_sync_run() call instead)
     */
    if (!wrk->sim_shutting_down)
      wrk->sync_run(wrk->sim_hdl);
  }

  /* remove this sim from the signal watch list */
  remove_abort_watcher(wrk);

  pthread_exit(NULL);
}

/*
 * Worker API routines
 */

/* Create the worker and start its simulation thread */
tBluesimWorkerHdl bluesim_worker_create(tBkSimStateHdl simHdl,
                                        tBkSyncRunFn   sync_run,
                                        tBkAbortNowFn  abort_now,
                                        tBkShutdownFn  shutdown,
                                        tBkNowFn       now)
{
  if ((simHdl == NULL) || (sync_run == NULL) || (abort_now == NULL) ||
      (shutdown == NULL) || (now == NULL))
    return NULL; // ERROR

  tBluesimWorker* wrk = new tBluesimWorker;

  wrk->sim_hdl   = simHdl;
  wrk->sync_run  = sync_run;
  wrk->abort_now = abort_now;
  wrk->shutdown  = shutdown;
  wrk->now       = now;

  /* setup simulation thread infrastructure */
  wrk->sim_shutting_down = false;
  pthread_mutex_init(&(wrk->sim_mutex), NULL);
  wrk->start_semaphore = create_semaphore();
  wrk->stop_semaphore = create_semaphore();
  if (wrk->start_semaphore == NULL || wrk->stop_semaphore == NULL)
  {
    release_semaphore(wrk->start_semaphore);
    release_semaphore(wrk->stop_semaphore);
    pthread_mutex_destroy(&(wrk->sim_mutex));
    delete wrk;
    return NULL; // ERROR
  }

  /* start the simulation thread and wait for it to block in pause_sim */
  wrk->sim_running = true;
  pthread_create(&(wrk->sim_thread_id), NULL, sim_thread, (void*)wrk);
  wait_for_sim_stop(wrk);

  return wrk;
}

/* Execute simulation events until none remain, simulation is
 * interrupted, or a stopping condition (time limit, etc.) is
 * encountered.
 */
int bluesim_worker_advance(tBluesimWorkerHdl wrk, unsigned char async)
{
  if (wrk == NULL)
    return BW_ERROR;

  /* check if the simulation is already running */
  if (bluesim_worker_is_running(wrk))
    return BW_ERROR;

  /* in case there was no bluesim_worker_sync(), we want the stop
   * semaphore to return to 0.
   */
  trywait_on_semaphore(wrk->stop_semaphore);

  /* kick off the simulation thread by posting to the start_semaphore */
  lock_sim_state(wrk);
  wrk->sim_running = true;
  unlock_sim_state(wrk);
  post_semaphore(wrk->start_semaphore);

  if (async)
    return BW_SUCCESS;  // don't wait for simulation to complete

  /* handle the synchronous case */
  wait_for_sim_stop(wrk);

  return BW_SUCCESS;
}

/* Test if the simulation thread is still running.
 *
 * Returns 0 if the thread is not running and non-zero if
 * the thread is running.
 */
unsigned char bluesim_worker_is_running(tBluesimWorkerHdl wrk)
{
  unsigned char ret = 0;
  lock_sim_state(wrk);
  if (wrk->sim_running) ret = 1;
  unlock_sim_state(wrk);
  return ret;
}

/* Wait for a simulation started using bluesim_worker_advance in
 * async mode to complete.
 *
 * Returns the simulation time at which execution stopped, as
 * reported by bk_now().
 */
unsigned long long bluesim_worker_sync(tBluesimWorkerHdl wrk)
{
  if (bluesim_worker_is_running(wrk))
    wait_for_sim_stop(wrk);

  return wrk->now(wrk->sim_hdl);
}

/* Abort simulation (from outside, Ctrl-C, SIGPIPE, etc.).
 * The kernel's bk_abort_now() records the abort and halts event
 * execution at the end of the current simulation cycle, at which
 * point the simulation thread pauses.
 */
void bluesim_worker_abort_now(tBluesimWorkerHdl wrk)
{
  if (wrk == NULL)
    return;

  wrk->abort_now(wrk->sim_hdl);
}

/* End the worker thread and shutdown the Bluesim kernel */
void bluesim_worker_shutdown(tBluesimWorkerHdl wrk)
{
  if (wrk == NULL)
    return;

  /* trigger the simulation thread to end; if it is mid-run, the
   * abort makes the kernel halt at the end of the current cycle
   */
  lock_sim_state(wrk);
  wrk->sim_shutting_down = true;
  unlock_sim_state(wrk);
  post_semaphore(wrk->start_semaphore);
  wrk->abort_now(wrk->sim_hdl);
  pthread_join(wrk->sim_thread_id, NULL);
  wrk->sim_running = false;

  /* clean up semaphores and mutexes */
  release_semaphore(wrk->start_semaphore);
  wrk->start_semaphore = NULL;
  release_semaphore(wrk->stop_semaphore);
  wrk->stop_semaphore = NULL;
  pthread_mutex_destroy(&(wrk->sim_mutex));

  /* shutdown the kernel itself */
  wrk->shutdown(wrk->sim_hdl);

  delete wrk;
}
