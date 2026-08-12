/*
 * Multithreaded STP driver for ThreadSanitizer runs.
 *
 * bsc (a GHC binary) cannot run with the TSan runtime preloaded on
 * aarch64, so this harness exercises the instrumented libstp directly:
 * an instrumented main making solver queries from several pthreads
 * (phase 1: concurrent independent solver instances, the shape GHC's
 * RTS produces when Haskell threads run FFI calls on different OS
 * threads) and from a succession of short-lived threads (phase 2: the
 * shape produced when the RTS migrates a Haskell thread between OS
 * threads across FFI calls).
 */
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "stp_c_interface.h"

#ifndef ITERS
#define ITERS 25
#endif

static void one_query(int tid, int iter)
{
    VC vc = vc_createValidityChecker();
    Type bv8 = vc_bvType(vc, 8);
    char name[48];
    snprintf(name, sizeof name, "x_%d_%d", tid, iter);
    Expr x = vc_varExpr(vc, name, bv8);
    Expr c17 = vc_bvConstExprFromInt(vc, 8, 17);
    Expr sum = vc_bvPlusExpr(vc, 8, x, c17);
    Expr eq = vc_eqExpr(vc, sum, c17);  /* x + 17 == 17: not valid */
    int r = vc_query(vc, eq);
    Expr eq2 = vc_eqExpr(vc, x, x);     /* x == x: valid */
    int r2 = vc_query(vc, eq2);
    if (r != 0 || r2 != 1) {
        fprintf(stderr,
                "stp-mt-harness: unexpected query results r=%d r2=%d "
                "(tid %d iter %d)\n", r, r2, tid, iter);
        exit(3);
    }
    vc_Destroy(vc);
}

static void *worker(void *arg)
{
    int tid = (int)(long)arg;
    for (int i = 0; i < ITERS; i++)
        one_query(tid, i);
    return 0;
}

int main(int argc, char **argv)
{
    int concurrent = (argc > 1 && argv[1][0] == 'c');

    if (!concurrent) {
        /* Serial handoff: solver work handed from thread to thread,
         * one solver instance live at a time.  This is the usage
         * shape bsc actually produces (the GHC RTS serializes the
         * Haskell thread but may run successive FFI calls on
         * different OS threads). */
        for (long t = 0; t < 8; t++) {
            pthread_t h;
            pthread_create(&h, 0, worker, (void *)(100 + t));
            pthread_join(h, 0);
        }
        printf("stp-mt-harness: serial-handoff phase completed OK\n");
        return 0;
    }

    /* Concurrent independent solver instances.  The vendored STP has
     * global state (BitVector_Boot, a global STP manager), so this is
     * expected to produce TSan reports and may abort; that is a
     * finding about thread-safety, not an infrastructure failure. */
    pthread_t th[4];
    for (long t = 0; t < 4; t++)
        pthread_create(&th[t], 0, worker, (void *)t);
    for (int t = 0; t < 4; t++)
        pthread_join(th[t], 0);
    printf("stp-mt-harness: concurrent phase completed OK\n");
    return 0;
}
