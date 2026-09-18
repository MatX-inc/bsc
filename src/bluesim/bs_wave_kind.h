#ifndef __BS_WAVE_KIND_H__
#define __BS_WAVE_KIND_H__

/* What a dumped signal is.  The waveform formats record it as far as
 * they can: VCD distinguishes only reg (WAVE_STATE) from wire, FST
 * also records the port direction.
 */
typedef enum { WAVE_STATE     /* register or storage element contents */
             , WAVE_WIRE      /* a named combinational value */
             , WAVE_INTERNAL  /* a value the compiler introduced */
             , WAVE_FIRE      /* a rule's CAN_FIRE or WILL_FIRE */
             , WAVE_INPUT     /* a module's or primitive's input port */
             , WAVE_OUTPUT    /* a module's or primitive's output port */
             , WAVE_CLOCK
             , WAVE_RESET
             } tWaveKind;

#endif /* __BS_WAVE_KIND_H__ */
