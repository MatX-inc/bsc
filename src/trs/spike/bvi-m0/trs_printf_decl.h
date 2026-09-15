// Force-included into every verilated TU so the VL_PRINTF redirect resolves.
#ifdef __cplusplus
extern "C" int trs_vlt_printf(const char* fmt, ...);
#else
int trs_vlt_printf(const char* fmt, ...);
#endif
