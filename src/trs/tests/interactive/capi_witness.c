/* Direct C-API witness: dlopen an interactive model .so (no bluetcl),
 * drive the bk_* lifecycle, and exercise the trs_* namespace —
 * engine queries and the on-demand oracle checkpoint.  The harness
 * prints deterministic lines the battery diffs against a literal
 * expectation (trs_* has no reference implementation to mirror).
 *
 *   cc -o capi_witness capi_witness.c -ldl
 *   TRS_CAPI_ENGINES=interp,aot ./capi_witness b3_top.so top
 */
#include <dlfcn.h>
#include <stdio.h>

int main(int argc, char **argv) {
    /* line-buffer C stdio so our lines interleave chronologically
     * with the model's (Rust) line-buffered design output */
    setvbuf(stdout, 0, _IOLBF, 0);
    if (argc < 3) {
        fprintf(stderr, "usage: %s model.so top\n", argv[0]);
        return 2;
    }
    void *h = dlopen(argv[1], RTLD_NOW);
    if (!h) {
        fprintf(stderr, "dlopen: %s\n", dlerror());
        return 2;
    }
    char sym[256];
    snprintf(sym, sizeof sym, "new_MODEL_%s", argv[2]);
    void *(*new_model)(void) = (void *(*)(void))dlsym(h, sym);
    void *(*bk_init)(void *, unsigned char) =
        (void *(*)(void *, unsigned char))dlsym(h, "bk_init");
    int (*bk_advance)(void *, unsigned char) =
        (int (*)(void *, unsigned char))dlsym(h, "bk_advance");
    unsigned char (*bk_finished)(void *) =
        (unsigned char (*)(void *))dlsym(h, "bk_finished");
    int (*bk_exit_status)(void *) = (int (*)(void *))dlsym(h, "bk_exit_status");
    void (*bk_shutdown)(void *) = (void (*)(void *))dlsym(h, "bk_shutdown");
    unsigned (*engine_count)(void *) =
        (unsigned (*)(void *))dlsym(h, "trs_engine_count");
    const char *(*engine_kind)(void *, unsigned) =
        (const char *(*)(void *, unsigned))dlsym(h, "trs_engine_kind");
    unsigned char (*oracle_check)(void *) =
        (unsigned char (*)(void *))dlsym(h, "trs_oracle_check");
    if (!new_model || !bk_init || !bk_advance || !bk_finished ||
        !bk_exit_status || !bk_shutdown || !engine_count || !engine_kind ||
        !oracle_check) {
        fprintf(stderr, "missing symbol\n");
        return 2;
    }
    void *hdl = bk_init(new_model(), 1);
    if (!hdl) {
        fprintf(stderr, "bk_init failed\n");
        return 2;
    }
    unsigned n = engine_count(hdl);
    printf("engines %u:", n);
    for (unsigned i = 0; i < n; i++) {
        const char *k = engine_kind(hdl, i);
        printf(" %s", k ? k : "?");
    }
    printf("\n");
    printf("kind-oob %s\n", engine_kind(hdl, n) ? "NONNULL" : "null");
    bk_advance(hdl, 0); /* runs to the design's $finish */
    printf("oracle %u\n", oracle_check(hdl));
    printf("finished %u status %d\n", bk_finished(hdl), bk_exit_status(hdl));
    bk_shutdown(hdl);
    printf("shutdown ok\n");
    return 0;
}
